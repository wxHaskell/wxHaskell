-----------------------------------------------------------------------------------------
{-|
Module      :  Process
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Process and stream wrappers.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore.Process
        (
        -- * Process
          OnReceive, OnEndProcess
        , processExecAsyncTimed, processExecAsync
        -- * Streams
        , StreamStatus(..)
        , streamBaseStatus
        -- * Blocking IO
        , inputStreamGetContents
        , inputStreamGetContentsN
        , inputStreamGetLine
        , inputStreamGetString
        , inputStreamGetChar
        , outputStreamPutString
        -- * Non-blocking IO
        , inputStreamGetLineNoWait
        , inputStreamGetStringNoWait
        , inputStreamGetCharNoWait
        , outputStreamPutStringNoWait
        ) where

import System.IO.Unsafe( unsafeInterleaveIO )
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.Events

import Foreign
import Foreign.C.String
import Foreign.C.Types

  

-- | Write a string to an output stream, potentially blocking
-- until all output has been written.
outputStreamPutString :: OutputStream a -> String -> IO ()
outputStreamPutString outputStream s
  = withCString s $ \cstr -> write cstr (length s)
  where
    write cstr n
      = do outputStreamWrite outputStream cstr n
           m <- outputStreamLastWrite outputStream
           if (m < n && m > 0 {- prevent infinite loop -} )
            then write (advancePtr cstr m) (n - m)
            else return ()
      

-- | Write a string to an output stream, returning the
-- number of bytes actually written.
outputStreamPutStringNoWait :: OutputStream a -> String -> IO Int
outputStreamPutStringNoWait outputStream s
  = withCString s $ \cstr ->
    do outputStreamWrite outputStream cstr (length s)
       outputStreamLastWrite outputStream

-- | @inputStreamGetLineNoWait stream n@ reads a line of at most @n@ characters from the
-- input stream in a non-blocking way. The function does automatic end-of-line
-- conversion. If the line ends with @\\n@, an entire line
-- has been read, otherwise, either the maximum has been reached, or no more
-- input was available.
inputStreamGetLineNoWait :: InputStream a -> Int -> IO String
inputStreamGetLineNoWait inputStream maxChars
  = readInputStream "" 0
  where
    readInputStream acc n
      = if n >= maxChars
         then return (reverse acc)
         else do mbc <- inputStreamGetCharNoWait inputStream
                 case mbc of
                  Nothing    -> return (reverse acc)
                  Just '\n'  -> return (reverse ('\n':acc))
                  Just '\r'  -> do mbc2 <- inputStreamGetCharNoWait inputStream
                                   case mbc2 of
                                     Just c2  | c2 /= '\n' -> inputStreamUngetch inputStream c2 >>
                                                              return ()
                                     _        -> return ()
                                   return (reverse ('\n':acc))
                  Just c     -> readInputStream (c:acc) (n+1)

-- | @inputStreamGetStringNoWait stream n@ reads a line of at most @n@ characters from the
-- input stream in a non-blocking way. 
inputStreamGetStringNoWait :: InputStream a -> Int -> IO String
inputStreamGetStringNoWait input maxChars
  = readInputStream "" 0
  where
    readInputStream acc n
      = if ( n >= maxChars )
         then return (reverse acc)
         else do mbc <- inputStreamGetCharNoWait input
                 case mbc of
                   Nothing -> return (reverse acc)
                   Just c  -> readInputStream (c:acc) (n+1)


-- | Read a single character from the input, returning @Nothing@ if no input
-- was available (using 'inputStreamCanRead').
inputStreamGetCharNoWait :: InputStream a -> IO (Maybe Char)
inputStreamGetCharNoWait input
  = do canRead <- inputStreamCanRead input
       if canRead
        then do c <- inputStreamGetC input
                return (Just c)
        else return Nothing


-- | @inputStreamGetLine s n@ reads a line of at most @n@ characters from the
-- input stream (potentially waiting for input). The function does automatic end-of-line
-- conversion. If the line ends with @\\n@, an entire line
-- has been read, otherwise, either the maximum has been reached, or no more
-- input was available.
inputStreamGetLine :: InputStream a -> Int -> IO String
inputStreamGetLine inputStream maxChars
  = readInputStream "" 0
  where
    readInputStream acc n
      = if n >= maxChars
         then return (reverse acc)
         else do c <- inputStreamGetChar inputStream
                 case c of
                  '\n'  -> return (reverse ('\n':acc))
                  '\r'  -> do mbc2 <- inputStreamGetCharNoWait inputStream
                              case mbc2 of
                                Just c2  | c2 /= '\n' -> inputStreamUngetch inputStream c2 >>
                                                         return ()
                                _        -> return ()
                              return (reverse ('\n':acc))
                  _     -> readInputStream (c:acc) (n+1)

-- | Read a single character from the input. (equals 'inputStreamGetC')
inputStreamGetChar :: InputStream a -> IO Char
inputStreamGetChar input
  = inputStreamGetC input


-- | The expression (@inputStreamGetString n input@) reads a string of maximally
-- @n@ characters from @input@.
inputStreamGetString :: InputStream a -> Int -> IO String
inputStreamGetString inputStream n
  = allocaBytes (n+1) $ \buffer ->
    do inputStreamRead inputStream buffer n
       nread <- inputStreamLastRead inputStream
       mapM (peekChar buffer) [0..nread-1]
  where       
    peekChar :: Ptr CChar -> Int -> IO Char
    peekChar p ofs
      = do cchar <- peekElemOff p ofs
           return (castCCharToChar cchar)



-- | Get the entire contents of an input stream. The content
-- is returned as a lazy stream (like 'hGetContents').
inputStreamGetContents :: InputStream a -> IO String
inputStreamGetContents inputStream
  = inputStreamGetContentsN inputStream 1

-- | Get the entire contents of an input stream. The content
-- is returned as a lazy stream (like 'hGetContents'). The
-- contents are returned in lazy /batches/, whose size is
-- determined by the first parameter.
inputStreamGetContentsN :: InputStream a -> Int -> IO String
inputStreamGetContentsN inputStream n
  = do status <- streamBaseGetLastError inputStream
       if (status == wxSTREAM_NO_ERROR)
        then do x  <- inputStreamGetString inputStream n
                xs <- unsafeInterleaveIO (inputStreamGetContentsN inputStream n)
                return (x ++ xs)
        else return ""

-- | Return the status of the stream
streamBaseStatus :: StreamBase a -> IO StreamStatus
streamBaseStatus stream
  = do code <- streamBaseGetLastError stream
       return (streamStatusFromInt code)

-- | Type of input receiver function.
type OnReceive     = String -> StreamStatus -> IO ()

-- | Type of end-of-process event handler. Gets the exitcode as its argument.
type OnEndProcess  = Int -> IO ()



-- | (@processExecAsyncTimer command processOutputOnEnd onEndProcess onOutput onErrorOutput parent@) starts
-- the @command@ asynchronously. The handler @onEndProcess@ is called when the process
-- terminates. @onOutput@ receives the output from @stdout@, while @onErrorOutput@ receives
-- output from @stderr@. If @processOutputOnEnd@ is 'True', the remaining output of a terminated
-- process is processed (calling @onOutput@). The call returns a triple (@send,process,pid@):
-- The @send@ function is used to send input to the @stdin@ pipe of the process. The
-- process object is returned in @process@ and the process identifier in @pid@.
--
-- Note: The method uses idle event timers to process the output channels. On
-- many platforms this is much more trustworthy and robust than the 'processExecAsync' that
-- uses threads (which can cause all kinds of portability problems).
processExecAsyncTimed :: Window a -> String -> Bool -> OnEndProcess -> OnReceive -> OnReceive
                      -> IO (String -> IO StreamStatus, Process (), Int)
processExecAsyncTimed parent cmd readInputOnEnd onEndProcess onOutput onErrOutput
  = do process    <- processCreateDefault parent idAny
       processRedirect process
       pid        <- wxcAppExecuteProcess cmd wxEXEC_ASYNC process
       if (pid == 0)
        then return (\_s -> return StreamEof, objectNull, pid)
        else do v <- varCreate (Just process)
                windowOnIdle parent (handleAnyInput v)
                unregister <- appRegisterIdle 100           -- 10 times a second
                evtHandlerOnEndProcess parent (handleTerminate v unregister)
                let send txt = handleSend v txt
                return (send, process, pid)
  where
    maxLine :: Int
    maxLine = 160

    handleSend :: Var (Maybe (Process a)) -> String -> IO StreamStatus
    handleSend v txt
      = withProcess v StreamEof $ \process ->
        do outputPipe <- processGetOutputStream process
           outputStreamPutString outputPipe txt         -- TODO: use idle events to do output non-blocking?
           streamBaseStatus outputPipe
           
    handleAnyInput :: Var (Maybe (Process a)) -> IO Bool
    handleAnyInput v
      = withProcess v False $ \process ->
        do inputPipe <- processGetInputStream process 
           available <- handleInput inputPipe onOutput  -- process some input on stdout
           errorPipe <- processGetErrorStream process           
           handleAllInput errorPipe onErrOutput         -- process all input on stderr
           return available

    handleAllInput :: InputStream a -> OnReceive -> IO ()
    handleAllInput input onOutput' 
      = do available <- handleInput input onOutput'
           if (available)
            then handleAllInput input onOutput'
            else return ()
    
    handleInput :: InputStream a -> OnReceive -> IO Bool
    handleInput input onOutput' 
      = do txt    <- inputStreamGetLineNoWait input maxLine
           status <- streamBaseStatus input
           if null txt 
            then case status of
                   StreamOk -> return False
                   _        -> do onOutput' "" status
                                  return False
            else do onOutput' txt status
                    return True

    handleTerminate :: Var (Maybe (Process a)) -> IO () -> Int -> Int -> IO ()
    handleTerminate v unregister _pid exitCode
      = do unregister
           withProcess v () $ \process -> 
            do varSet v Nothing
               if (readInputOnEnd)
                then do inputPipe <- processGetInputStream process
                        handleAllInput inputPipe onOutput  -- handle remaining input
                else return ()
               onEndProcess exitCode
               processDelete process 
               return ()

    withProcess v x f
      = do mb <- varGet v 
           case mb of
             Nothing -> return x
             Just p  -> f p



{-# DEPRECATED processExecAsync "Use processExecAsyncTimed instead (if possible)" #-}
-- | deprecated: use 'processExecAsyncTimed' instead (if possible).
-- (@processExecAsync command bufferSize onEndProcess onOutput onErrorOutput parent@) starts
-- the @command@ asynchronously. The handler @onEndProcess@ is called when the process
-- terminates. @onOutput@ receives the output from @stdout@, while @onErrorOutput@ receives
-- output from @stderr@. The @bufferSize@ determines the intermediate buffer used to
-- cache the output from those channels. The calls returns a triple (@send,process,pid@):
-- The @send@ function is used to send input to the @stdin@ pipe of the process. The
-- process object is returned in @process@ and the process identifier in @pid@.
processExecAsync :: Window a -> String -> Int -> OnEndProcess -> OnReceive -> OnReceive
                      -> IO (String -> IO (), Process (), Int)
processExecAsync parent command bufferSize onEndProcess onOutput onErrOutput
  = do process    <- processCreateDefault parent idAny
       processRedirect process
       pid        <- wxcAppExecuteProcess command wxEXEC_ASYNC process
       if (pid == 0)
        then return (\_s -> return (), objectNull, pid)
        else do inputPipe  <- processGetInputStream process
                outputPipe <- processGetOutputStream process
                errorPipe  <- processGetErrorStream process
                evtHandlerOnEndProcess parent (handleOnEndProcess pid process inputPipe outputPipe errorPipe)
                evtHandlerOnInput parent onOutput inputPipe bufferSize
                evtHandlerOnInput parent onErrOutput errorPipe bufferSize
                let send txt   = outputStreamPutString outputPipe txt
                return (send, process, pid)
  where
    handleOnEndProcess ourPid process _inputPipe _outputPipe _errorPipe pid exitcode
      | ourPid == pid  = do onEndProcess exitcode
                            processDelete process
      | otherwise      = return ()
