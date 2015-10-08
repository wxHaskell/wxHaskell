{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-----------------------------------------------------------------------------------------
{-|
Module      :  WxcObject
Copyright   :  (c) Daan Leijen 2003, 2004
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Basic object type.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore.WxcObject(
            -- * Object types
              Object, objectNull, objectIsNull, objectCast, objectIsManaged
            , objectFromPtr, objectFromManagedPtr
            , withObjectPtr
            , objectFinalize, objectNoFinalize
            -- * Managed objects
            , ManagedPtr, TManagedPtr, CManagedPtr(..)
            ) where

import System.IO.Unsafe( unsafePerformIO )
import Foreign.Ptr

{- note: for GHC 6.10.2 or higher, recommends to use "import Foreign.Concurrent"
   See http://www.haskell.org/pipermail/cvs-ghc/2009-January/047120.html -}
import Foreign.ForeignPtr hiding (newForeignPtr,addForeignPtrFinalizer)
import Foreign.Concurrent

{-----------------------------------------------------------------------------------------
    Objects
-----------------------------------------------------------------------------------------}

{- | An @Object a@ is a pointer to an object of type @a@. The @a@ parameter is used
   to encode the inheritance relation. When the type parameter is unit @()@, it denotes
   an object of exactly that class, when the parameter is a type variable @a@, it
   specifies an object that is at least an instance of that class. For example in 
   wxWidgets, we have the following class hierarchy:

   > EvtHandler
   >   |- Window
   >        |- Frame
   >        |- Control
   >            |- Button
   >            |- Radiobox

   In wxHaskell, all the creation functions will return objects of exactly that
   class and use the @()@ type:

   > frameCreate :: Window a -> ... -> IO (Frame ())
   > buttonCreate :: Window a -> ... -> IO (Button ())
   > ...

   In contrast, all the /this/ (or /self/) pointers of methods can take objects
   of any instance of that class and have a type variable, for example:

   > windowSetClientSize :: Window a -> Size -> IO ()
   > controlSetLabel     :: Control a -> String -> IO ()
   > buttonSetDefault    :: Button a -> IO ()

   This means that we can use @windowSetClientSize@ on any window, including
   buttons and frames, but we can only use @controlSetLabel@ on controls, not
   including frames.

   In wxHaskell, this works since a @Frame ()@ is actually a type synonym for
   @Window (CFrame ())@ (where @CFrame@ is an abstract data type). We can thus
   pass a value of type @Frame ()@ to anything that expects some @Window a@.
   For a button this works too, as it is a synonym for @Control (CButton ())@
   which is in turn a synonym for @Window (CControl (CButton ()))@. Note that
   we can\'t pass a frame to something that expects a value of type @Control a@.
   Of course, a @Window a@ is actually a type synonym for @EvtHandler (CWindow a)@.
   If you study the documentation in "Graphics.UI.WX.Classes" closely, you
   can discover where this chain ends :-).  

   Objects are not automatically deleted. Normally you can use a delete function
   like @windowDelete@ to delete an object. However, almost all objects in the
   wxWidgets library are automatically deleted by the library. The only objects
   that should be used with care are resources as bitmaps, fonts and brushes.
-}
data Object a   = Object  !(Ptr a)
                | Managed !(ForeignPtr (TManagedPtr a))


-- | Managed pointer (proxy) objects
type ManagedPtr a   = Ptr (CManagedPtr a)
type TManagedPtr a  = CManagedPtr a
data CManagedPtr a  = CManagedPtr


instance Eq (Object a) where
  obj1 == obj2
    = unsafePerformIO $
      withObjectPtr obj1 $ \p1 ->
      withObjectPtr obj2 $ \p2 ->
      return (p1 == p2)

instance Ord (Object a) where
  compare obj1 obj2
    = unsafePerformIO $
      withObjectPtr obj1 $ \p1 ->
      withObjectPtr obj2 $ \p2 ->
      return (compare p1 p2)

instance Show (Object a) where
  show obj
    = unsafePerformIO $
      withObjectPtr obj $ \p ->
      return (show p)

-- | A null object. Use with care.
objectNull :: Object a
objectNull
  = Object nullPtr

-- | Is this a managed object?
objectIsManaged :: Object a -> Bool
objectIsManaged obj
  = case obj of
      Managed _fp -> True
      _           -> False

-- | Test for null object.
objectIsNull :: Object a -> Bool
objectIsNull obj
  = unsafePerformIO $
    withObjectPtr obj $ \p -> return (p == nullPtr)
      

-- | Cast an object to another type. Use with care.
objectCast :: Object a -> Object b
objectCast obj
  = case obj of
      Object p   -> Object (castPtr p)
      Managed fp -> Managed (castForeignPtr fp) 


-- | Do something with the object pointer.
withObjectPtr :: Object a -> (Ptr a -> IO b) -> IO b
withObjectPtr obj f
  = case obj of
      Object p   -> f p
      Managed fp -> withForeignPtr fp $ \mp ->
                    do p <- wxManagedPtr_GetPtr mp
                       f p

-- | Finalize a managed object manually. (No effect on unmanaged objects.)
objectFinalize :: Object a -> IO ()
objectFinalize obj
  = case obj of
      Object _p  -> return ()
      Managed fp -> withForeignPtr fp $ wxManagedPtr_Finalize
                          
-- | Remove the finalizer on a managed object. (No effect on unmanaged objects.)
objectNoFinalize :: Object a -> IO ()
objectNoFinalize obj
  = case obj of
      Object  _p -> return ()
      Managed fp -> withForeignPtr fp $ wxManagedPtr_NoFinalize


-- | Create an unmanaged object.
objectFromPtr :: Ptr a -> Object a
objectFromPtr p
  = Object p

-- | Create a managed object with a given finalizer.
objectFromManagedPtr :: ManagedPtr a -> IO (Object a)
objectFromManagedPtr mp
  = do fun <- wxManagedPtrDeleteFunction
       -- wxManagedPtr_NoFinalize mp    {- turn off finalization -}
       fp <- newForeignPtr mp (fun mp)
       return (Managed fp)


wxManagedPtrDeleteFunction :: IO (ManagedPtr a -> IO ())
wxManagedPtrDeleteFunction
  = do fun <- wxManagedPtr_GetDeleteFunction
       return $ wxManagedPtr_CallbackFunction fun

{--------------------------------------------------------------------------
  Managed pointers
--------------------------------------------------------------------------}
foreign import ccall wxManagedPtr_GetPtr     :: Ptr (TManagedPtr a) -> IO (Ptr a)
foreign import ccall wxManagedPtr_Finalize   :: ManagedPtr a -> IO ()
foreign import ccall wxManagedPtr_NoFinalize :: ManagedPtr a -> IO ()
foreign import ccall wxManagedPtr_GetDeleteFunction :: IO (FunPtr (ManagedPtr a -> IO ()))
foreign import ccall "dynamic" wxManagedPtr_CallbackFunction :: FunPtr (ManagedPtr a -> IO ()) -> ManagedPtr a -> IO ()
