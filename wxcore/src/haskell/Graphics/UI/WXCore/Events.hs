{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------------------
{-|
Module      :  Events
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Dynamically set (and get) Haskell event handlers for basic wxWidgets events.
Note that one should always call 'skipCurrentEvent' when an event is not
processed in the event handler so that other eventhandlers can process the
event.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore.Events
        (
        -- Veto command for veto-able events
          Veto
        -- * Set event handlers
        -- ** Controls
        , buttonOnCommand
        , checkBoxOnCommand
        , choiceOnCommand
        , comboBoxOnCommand
        , comboBoxOnTextEnter
        , controlOnText
        , listBoxOnCommand
        , spinCtrlOnCommand
        -- , listBoxOnDClick
        , radioBoxOnCommand
        , sliderOnCommand
        , textCtrlOnTextEnter
        , listCtrlOnListEvent
        , toggleButtonOnCommand
        , treeCtrlOnTreeEvent
        , gridOnGridEvent
        , wizardOnWizEvent
        , propertyGridOnPropertyGridEvent

        -- ** Windows
        , windowOnMouse
        , windowOnKeyChar
        , windowOnKeyDown
        , windowOnKeyUp
        , windowAddOnClose
        , windowOnClose
        , windowOnDestroy
        , windowAddOnDelete
        , windowOnDelete
        , windowOnCreate
        , windowOnIdle
        , windowOnTimer
        , windowOnSize
        , windowOnFocus
        , windowOnActivate
        , windowOnPaint
        , windowOnPaintRaw
        , windowOnPaintGc
        , windowOnContextMenu
        , windowOnScroll
        , htmlWindowOnHtmlEvent

        -- ** Event handlers
        , evtHandlerOnMenuCommand
        , evtHandlerOnEndProcess
        , evtHandlerOnInput
        , evtHandlerOnInputSink
        , evtHandlerOnTaskBarIconEvent

        -- ** Raw STC export
        , EventSTC(..)
        , stcOnSTCEvent
        , stcGetOnSTCEvent

        -- ** Print events
        , EventPrint(..)
        , printOutOnPrint

        -- * Get event handlers
        -- ** Controls
        , buttonGetOnCommand
        , checkBoxGetOnCommand
        , choiceGetOnCommand
        , comboBoxGetOnCommand
        , comboBoxGetOnTextEnter
        , controlGetOnText
        , listBoxGetOnCommand
        , spinCtrlGetOnCommand
        -- , listBoxGetOnDClick
        , radioBoxGetOnCommand
        , sliderGetOnCommand
        , textCtrlGetOnTextEnter
        , listCtrlGetOnListEvent
        , toggleButtonGetOnCommand
        , treeCtrlGetOnTreeEvent
        , gridGetOnGridEvent
        , wizardGetOnWizEvent
        , propertyGridGetOnPropertyGridEvent

        -- ** Windows
        , windowGetOnMouse
        , windowGetOnKeyChar
        , windowGetOnKeyDown
        , windowGetOnKeyUp
        , windowGetOnClose
        , windowGetOnDestroy
        , windowGetOnDelete
        , windowGetOnCreate
        , windowGetOnIdle
        , windowGetOnTimer
        , windowGetOnSize
        , windowGetOnFocus
        , windowGetOnActivate
        , windowGetOnPaint
        , windowGetOnPaintRaw
        , windowGetOnPaintGc
        , windowGetOnContextMenu
        , windowGetOnScroll
        , htmlWindowGetOnHtmlEvent

        -- ** Event handlers
        , evtHandlerGetOnMenuCommand
        , evtHandlerGetOnEndProcess
        , evtHandlerGetOnInputSink
        , evtHandlerGetOnTaskBarIconEvent

        -- ** Printing
        , printOutGetOnPrint

        -- * Timers
        , windowTimerAttach
        , windowTimerCreate
        , timerOnCommand
        , timerGetOnCommand

        -- Idle events
        , appRegisterIdle

        -- * Calenders
        , EventCalendar(..)
        , calendarCtrlOnCalEvent 
        , calendarCtrlGetOnCalEvent

        -- * Types
        -- ** Streams
        , StreamStatus(..), streamStatusFromInt

        -- ** Modifiers
        , Modifiers(..)
        , showModifiers
        , noneDown, justShift, justAlt, justControl, justMeta, isNoneDown
        , isNoShiftAltControlDown

        -- ** Mouse events
        , EventMouse (..)
        , showMouse
        , mousePos, mouseModifiers

        -- ** Keyboard events
        , EventKey (..), Key(..)
        , keyKey, keyModifiers, keyPos
        , showKey, showKeyModifiers

        -- * Set event handlers
        -- ** Drop Target events
        , DragResult (..)
        , dropTargetOnData
        , dropTargetOnDrop
        , dropTargetOnEnter
        , dropTargetOnDragOver
        , dropTargetOnLeave
        
        -- ** On DragAndDropEvent
        , DragMode (..)
        , dragAndDrop
        
        -- *** Special handler for Drop File event
        , fileDropTarget
        -- *** Special handler for Drop Text event 
        , textDropTarget

        -- ** Scroll events
        , EventScroll(..), Orientation(..)
        , scrollOrientation, scrollPos

        -- ** Tree control events
        , EventTree(..)
        
        -- ** List control events
        , EventList(..), ListIndex

        -- ** Grid control events
        , EventGrid(..), Row, Column

        -- ** Html window events
        , EventHtml(..)
        
        -- * TaskBar icon events
        , EventTaskBarIcon(..)

        -- ** Wizard events
        , EventWizard(..), Direction(..)

        -- ** PropertyGrid events
        , EventPropertyGrid(..)

        -- ** AuiNotebook events
        , WindowId(..)
        , WindowSelection(..)
        , PageWindow(..)
        , EventAuiNotebook(..)
        , noWindowSelection 
        , auiNotebookOnAuiNotebookEvent
        , auiNotebookOnAuiNotebookEventEx
        , auiNotebookGetOnAuiNotebookEvent

        -- * Current event
        , propagateEvent
        , skipCurrentEvent
        , withCurrentEvent

        -- * Primitive
        , appOnInit

        -- ** Client data
        , treeCtrlSetItemClientData

        , evtHandlerWithClientData
        , evtHandlerSetClientData

        , objectWithClientData
        , objectSetClientData       

        -- ** Input sink
        , inputSinkEventLastString

        -- ** Keys
        , KeyCode
        , modifiersToAccelFlags
        , keyCodeToKey, keyToKeyCode

        -- ** Events
        , windowOnEvent, windowOnEventEx

        -- ** Generic
        , OnEvent
        , evtHandlerOnEvent
        , evtHandlerOnEventConnect

        -- ** Unsafe
        , unsafeTreeCtrlGetItemClientData
        , unsafeEvtHandlerGetClientData
        , unsafeObjectGetClientData
        , unsafeGetHandlerState
        , unsafeWindowGetHandlerState
        ) where

import Data.List( intersperse )
import System.Environment( getProgName, getArgs )
import Foreign.StablePtr
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils

import Data.Char ( chr ) -- used in stc
import Data.Maybe ( fromMaybe, fromJust )
import Control.Concurrent.MVar
import System.IO.Unsafe( unsafePerformIO )

import qualified Data.IntMap as IntMap
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.WxcClassInfo
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.Draw
import Graphics.UI.WXCore.Defines

-- | IO action to cancel events.
type Veto = IO ()

------------------------------------------------------------------------------------------
-- Controls  (COMMAND events)
------------------------------------------------------------------------------------------
-- | Set an event handler for a push button.
buttonOnCommand :: Button a -> IO () -> IO ()
buttonOnCommand button eventHandler
  = windowOnEvent button [wxEVT_COMMAND_BUTTON_CLICKED] eventHandler (\_evt -> eventHandler)


-- | Get the current button event handler on a window.
buttonGetOnCommand :: Window a -> IO (IO ())
buttonGetOnCommand button
  = unsafeWindowGetHandlerState button wxEVT_COMMAND_BUTTON_CLICKED skipCurrentEvent


-- | Set an event handler for "updated text", works for example on a 'TextCtrl' and 'ComboBox'.
controlOnText :: Control a -> IO () -> IO ()
controlOnText control eventHandler
  = windowOnEvent control [wxEVT_COMMAND_TEXT_UPDATED] eventHandler (\_evt -> eventHandler)

-- | Get the current event handler for updated text.
controlGetOnText :: Control a -> IO (IO ())
controlGetOnText control
  = unsafeWindowGetHandlerState control wxEVT_COMMAND_TEXT_UPDATED skipCurrentEvent


-- | Set an event handler for an enter command in a text control.
textCtrlOnTextEnter :: TextCtrl a -> IO () -> IO ()
textCtrlOnTextEnter textCtrl eventHandler
  = windowOnEvent textCtrl [wxEVT_COMMAND_TEXT_ENTER] eventHandler (\_evt -> eventHandler)

-- | Get the current text enter event handler.
textCtrlGetOnTextEnter :: TextCtrl a -> IO (IO ())
textCtrlGetOnTextEnter textCtrl
  = unsafeWindowGetHandlerState textCtrl wxEVT_COMMAND_TEXT_ENTER skipCurrentEvent

{-
-- | Set an event handler for when a user tries to type more than than the maximally
-- allowed text in a text control.
textCtrlOnTextMaxLen :: IO () -> TextCtrl a -> IO ()
textCtrlOnTextMaxLen eventHandler textCtrl
  = windowOnEvent textCtrl [wxEVT_COMMAND_TEXT_MAXLEN] eventHandler (\_evt -> eventHandler)

-- | Get the current maximal text event handler.
textCtrlGetOnTextMaxLen :: TextCtrl a -> IO (IO ())
textCtrlGetOnTextMaxLen textCtrl
  = unsafeWindowGetHandlerState textCtrl wxEVT_COMMAND_TEXT_MAXLEN skipCurrentEvent
-}

-- | Set an event handler for an enter command in a combo box.
comboBoxOnTextEnter :: ComboBox a -> IO () -> IO ()
comboBoxOnTextEnter comboBox eventHandler
  = windowOnEvent comboBox [wxEVT_COMMAND_TEXT_ENTER] eventHandler (\_evt -> eventHandler)

-- | Get the current text enter event handler.
comboBoxGetOnTextEnter :: ComboBox a -> IO (IO ())
comboBoxGetOnTextEnter comboBox
  = unsafeWindowGetHandlerState comboBox wxEVT_COMMAND_TEXT_ENTER skipCurrentEvent


-- | Set an event handler for when a combo box item is selected.
comboBoxOnCommand :: ComboBox a -> IO () -> IO ()
comboBoxOnCommand comboBox eventHandler
  = windowOnEvent comboBox [wxEVT_COMMAND_COMBOBOX_SELECTED] eventHandler (\_evt -> eventHandler)

-- | Get the current combo box event handler for selections
comboBoxGetOnCommand :: ComboBox a -> IO (IO ())
comboBoxGetOnCommand comboBox
  = unsafeWindowGetHandlerState comboBox wxEVT_COMMAND_COMBOBOX_SELECTED skipCurrentEvent

-- | Set an event handler for when a listbox item is (de)selected.
listBoxOnCommand :: ListBox a -> IO () -> IO ()
listBoxOnCommand listBox eventHandler
  = windowOnEvent listBox [wxEVT_COMMAND_LISTBOX_SELECTED] eventHandler (\_evt -> eventHandler)

-- | Get the current listbox event handler for selections.
listBoxGetOnCommand :: ListBox a -> IO (IO ())
listBoxGetOnCommand listBox
  = unsafeWindowGetHandlerState listBox wxEVT_COMMAND_LISTBOX_SELECTED skipCurrentEvent

{-
-- | Set an event handler for when a listbox item is double clicked. Takes the selected
-- item index as an argument.
listBoxOnDClick :: (Int -> IO ()) -> ListBox a -> IO ()
listBoxOnDClick eventHandler listBox
  = windowOnEvent listBox [wxEVT_COMMAND_LISTBOX_DCLICK] eventHandler dclickHandler
  where
    dclickHandler event
      = do index <- commandEventGetInt (objectCast event)
           eventHandler index

-- | Get the current double click listbox event handler.
listBoxGetOnDClick :: ListBox a -> IO (IO ())
listBoxGetOnDClick listBox
  = unsafeWindowGetHandlerState listBox wxEVT_COMMAND_LISTBOX_DCLICK (\index -> skipCurrentEvent)
-}

-- | Set an event handler for when a choice item is (de)selected.
choiceOnCommand :: Choice a -> IO () -> IO ()
choiceOnCommand choice eventHandler
  = windowOnEvent choice [wxEVT_COMMAND_CHOICE_SELECTED] eventHandler (\_evt -> eventHandler)

-- | Get the current choice command event handler.
choiceGetOnCommand :: Choice a -> IO (IO ())
choiceGetOnCommand choice
  = unsafeWindowGetHandlerState choice wxEVT_COMMAND_CHOICE_SELECTED skipCurrentEvent


-- | Set an event handler for when a radiobox item is selected.
radioBoxOnCommand :: RadioBox a -> IO () -> IO ()
radioBoxOnCommand radioBox eventHandler
  = windowOnEvent radioBox [wxEVT_COMMAND_RADIOBOX_SELECTED] eventHandler (\_evt -> eventHandler)

-- | Get the current radio box command handler.
radioBoxGetOnCommand :: RadioBox a -> IO (IO ())
radioBoxGetOnCommand radioBox
  = unsafeWindowGetHandlerState radioBox wxEVT_COMMAND_RADIOBOX_SELECTED skipCurrentEvent


-- | Set an event handler for when a slider item changes.
sliderOnCommand :: Slider a -> IO () -> IO ()
sliderOnCommand slider eventHandler
  = windowOnEvent slider [wxEVT_COMMAND_SLIDER_UPDATED] eventHandler (\_evt -> eventHandler)

-- | Get the current slider command event handler.
sliderGetOnCommand :: Slider a -> IO (IO ())
sliderGetOnCommand slider
  = unsafeWindowGetHandlerState slider wxEVT_COMMAND_SLIDER_UPDATED skipCurrentEvent



-- | Set an event handler for when a checkbox clicked.
checkBoxOnCommand :: CheckBox a -> (IO ()) -> IO ()
checkBoxOnCommand checkBox eventHandler
  = windowOnEvent checkBox [wxEVT_COMMAND_CHECKBOX_CLICKED] eventHandler (\_evt -> eventHandler)

-- | Get the current check box event handler.
checkBoxGetOnCommand :: CheckBox a -> IO (IO ())
checkBoxGetOnCommand checkBox
  = unsafeWindowGetHandlerState checkBox wxEVT_COMMAND_CHECKBOX_CLICKED (skipCurrentEvent)

-- | Set an event handler for when a spinCtrl clicked.
spinCtrlOnCommand :: SpinCtrl a -> (IO ()) -> IO ()
spinCtrlOnCommand spinCtrl eventHandler
  = windowOnEvent spinCtrl [wxEVT_COMMAND_SPINCTRL_UPDATED] eventHandler (\_evt -> eventHandler)

-- | Get the current check box event handler.
spinCtrlGetOnCommand :: SpinCtrl a -> IO (IO ())
spinCtrlGetOnCommand spinCtrl
  = unsafeWindowGetHandlerState spinCtrl wxEVT_COMMAND_SPINCTRL_UPDATED (skipCurrentEvent)

-- | Set an event handler for a push button.
toggleButtonOnCommand :: ToggleButton a -> IO () -> IO ()
toggleButtonOnCommand button eventHandler
  = windowOnEvent button [wxEVT_COMMAND_TOGGLEBUTTON_CLICKED] eventHandler (\_evt -> eventHandler)

-- | Get the current button event handler on a window.
toggleButtonGetOnCommand :: Window a -> IO (IO ())
toggleButtonGetOnCommand button
  = unsafeWindowGetHandlerState button wxEVT_COMMAND_TOGGLEBUTTON_CLICKED skipCurrentEvent

{-----------------------------------------------------------------------------------------
  wxStyledTextCtrl's event
-----------------------------------------------------------------------------------------}
-- | Scintilla events. * Means extra information is available (excluding position,
--   key and modifiers) but not yet implemented. ! means it's done
data EventSTC
    = STCChange             -- ^ ! wxEVT_STC_CHANGE.
    | STCStyleNeeded        -- ^ ! wxEVT_STC_STYLENEEDED.
    | STCCharAdded Char Int -- ^ ? wxEVT_STC_CHARADDED. The position seems to be broken
    | STCSavePointReached   -- ^ ! wxEVT_STC_SAVEPOINTREACHED.
    | STCSavePointLeft      -- ^ ! wxEVT_STC_SAVEPOINTLEFT.
    | STCROModifyAttempt    -- ^ ! wxEVT_STC_ROMODIFYATTEMPT.
    | STCKey                -- ^ * wxEVT_STC_KEY.
                            -- kolmodin 20050304:
                            -- is this event ever raised? not under linux.
                            -- according to davve, not under windows either
    | STCDoubleClick        -- ^ ! wxEVT_STC_DOUBLECLICK.
    | STCUpdateUI           -- ^ ! wxEVT_STC_UPDATEUI.
    | STCModified Int Int (Maybe String) Int Int Int Int Int          -- ^ ? wxEVT_STC_MODIFIED.
    | STCMacroRecord Int Int Int  -- ^ ! wxEVT_STC_MACRORECORD iMessage wParam lParam
    | STCMarginClick Bool Bool Bool Int Int -- ^ ? wxEVT_STC_MARGINCLICK.
                                            -- kolmodin 20050304:
                                            -- Add something nicer for alt, shift and ctrl?
                                            -- Perhaps a new datatype or a tuple.
    | STCNeedShown Int Int  -- ^ ! wxEVT_STC_NEEDSHOWN length position.
    | STCPainted            -- ^ ! wxEVT_STC_PAINTED. 
    | STCUserListSelection Int String -- ^ ! wxEVT_STC_USERLISTSELECTION listType text
    | STCUriDropped String  -- ^ ! wxEVT_STC_URIDROPPED
    | STCDwellStart Point -- ^ ! wxEVT_STC_DWELLSTART
    | STCDwellEnd Point   -- ^ ! wxEVT_STC_DWELLEND
    | STCStartDrag Int Int String            -- ^ ! wxEVT_STC_START_DRAG.
    | STCDragOver Point DragResult            -- ^ ! wxEVT_STC_DRAG_OVER
    | STCDoDrop String DragResult            -- ^ ! wxEVT_STC_DO_DROP
    | STCZoom               -- ^ ! wxEVT_STC_ZOOM
    | STCHotspotClick       -- ^ ! wxEVT_STC_HOTSPOT_CLICK
    | STCHotspotDClick      -- ^ ! wxEVT_STC_HOTSPOT_DCLICK
    | STCCalltipClick       -- ^ ! wxEVT_STC_CALLTIP_CLICK
    | STCAutocompSelection  -- ^ ! wxEVT_STC_AUTOCOMP_SELECTION
    | STCUnknown            -- ^ Unknown event. Should never occur.

instance Show EventSTC where
    show STCChange = "(stc event: change)"
    show STCStyleNeeded = "(stc event: style needed)"
    show (STCCharAdded c p) = "(stc event: char added: " ++ show c ++ " at position " ++ show p ++ ")"
    show STCSavePointReached = "(stc event: save point reached)"
    show STCSavePointLeft = "(stc event: save point left)"
    show STCROModifyAttempt = "(stc event: read only modify attempt)"
    show STCKey = "(stc event: key)"
    show STCDoubleClick = "(stc event: double click)"
    show STCUpdateUI = "(stc event: update ui)"
    show (STCModified p mt t len ladd line fln flp) = "(stc event: modified: position " ++ show p ++ ", modtype " ++ show mt ++ ", text " ++ show t ++ ", length " ++ show len ++ ", lines added " ++ show ladd ++ ", line " ++ show line ++ ", fln " ++ show fln ++ ", flp " ++ show flp ++ ")"
    show (STCMacroRecord m wp lp) = "(stc event: macro record, message " ++ show m ++ ", wParam " ++ show wp ++ ", lParam " ++ show lp ++ ")"
    show (STCMarginClick alt shift ctrl p m) = "(stc event: margin " ++ show m ++ " clicked, pos " ++ show p ++ ", modifiers = [" ++ (if alt then "alt, " else "") ++ (if shift then "shift, " else "") ++ (if ctrl then "control" else "") ++ "])"
    show (STCNeedShown p len) = "(stc event: need to show lines from " ++ show p ++ ", length " ++ show len ++ ")"
    show STCPainted = "(stc event: painted)"
    show (STCUserListSelection lt t) = "(stc event: user list selection, type " ++ show lt ++ ", text " ++ show t ++ ")"
    show (STCUriDropped t) = "(stc event: uri dropped: " ++ t ++ ")"
    show (STCDwellStart p) = "(stc event: dwell start, (x,y) " ++ show p ++ ")"
    show (STCDwellEnd p) = "(stc event: dwell end, (x,y) " ++ show p ++ ")"
    show (STCStartDrag lin car str) = "(stc event: start drag, line " ++ show lin ++ ", caret " ++ show car ++ ", text " ++ show str ++ ")"
    show (STCDragOver p res) = "(stc event: drag over, (x,y) " ++ show p ++ ", dragResult " ++ show res ++ ")"
    show (STCDoDrop str res) = "(stc event: do drop, text " ++ show str ++ ", dragResult " ++ show res ++ ")"
    show STCZoom = "(stc event: zoom)"
    show STCHotspotClick = "(stc event: hotspot click)"
    show STCHotspotDClick = "(stc event: hotspot double click)"
    show STCCalltipClick = "(stc event: calltip clicked)"
    show STCAutocompSelection = "(stc event: autocomp selectioned)"
    show STCUnknown = "(stc event: unknown)"

fromSTCEvent :: StyledTextEvent a -> IO EventSTC
fromSTCEvent event
  = do et <- eventGetEventType event
       case lookup et stcEvents of
         Just action -> action event
         Nothing -> return STCUnknown

stcEvents :: [(EventId, StyledTextEvent a -> IO EventSTC)]
stcEvents = [ (wxEVT_STC_CHANGE,            \_ -> return STCChange)
            , (wxEVT_STC_STYLENEEDED,       \_ -> return STCStyleNeeded)
            , (wxEVT_STC_CHARADDED,         charAdded)
            , (wxEVT_STC_SAVEPOINTREACHED,  \_ -> return STCSavePointReached)
            , (wxEVT_STC_SAVEPOINTLEFT,     \_ -> return STCSavePointLeft)
            , (wxEVT_STC_ROMODIFYATTEMPT,   \_ -> return STCROModifyAttempt)
            , (wxEVT_STC_KEY,               \_ -> return STCKey)
            , (wxEVT_STC_DOUBLECLICK,       \_ -> return STCDoubleClick)
            , (wxEVT_STC_UPDATEUI,          \_ -> return STCUpdateUI)
            , (wxEVT_STC_MODIFIED,          modified)
            , (wxEVT_STC_MACRORECORD,       macroRecord)
            , (wxEVT_STC_MARGINCLICK,       marginClick)
            , (wxEVT_STC_NEEDSHOWN,         needShown)
            , (wxEVT_STC_PAINTED,           \_ -> return STCPainted)
            , (wxEVT_STC_USERLISTSELECTION, userListSelection)
            , (wxEVT_STC_URIDROPPED,        uriDropped)
            , (wxEVT_STC_DWELLSTART,        dwellStart)
            , (wxEVT_STC_DWELLEND,          dwellEnd)
            , (wxEVT_STC_START_DRAG,        startDrag)
            , (wxEVT_STC_DRAG_OVER,         dragOver)
            , (wxEVT_STC_DO_DROP,           doDrop)
            , (wxEVT_STC_ZOOM,              \_ -> return STCZoom)
            , (wxEVT_STC_HOTSPOT_CLICK,     \_ -> return STCHotspotClick)
            , (wxEVT_STC_CALLTIP_CLICK,     \_ -> return STCCalltipClick)
            -- TODO: STCAutocompSelection event is not tested yet.
            , (wxEVT_STC_AUTOCOMP_SELECTION,    \_ -> return STCAutocompSelection)
            ]
  where
    charAdded evt = do
      c <- styledTextEventGetKey evt
      let c' | c < 0 = chr $ c + 256
             | otherwise = chr c
      p <- styledTextEventGetPosition evt
      return $ STCCharAdded c' p
    modified evt = do
      p <- styledTextEventGetPosition evt
      mt <- styledTextEventGetModificationType evt
      t <- styledTextEventGetText evt
      len <- styledTextEventGetLength evt
      ladd <- styledTextEventGetLinesAdded evt
      line <- styledTextEventGetLine evt
      fln <- styledTextEventGetFoldLevelNow evt
      flp <- styledTextEventGetFoldLevelPrev evt 
      -- TODO: t should only be returned under some modificationtype conditions
      -- or should we always return it?
      return $ STCModified p mt (Just t) len ladd line fln flp
    macroRecord evt = do
      m <- styledTextEventGetMessage evt
      wp <- styledTextEventGetWParam evt
      lp <- styledTextEventGetLParam evt
      return $ STCMacroRecord m wp lp
    marginClick evt = do
      alt <- styledTextEventGetAlt evt
      shift <- styledTextEventGetShift evt
      ctrl <- styledTextEventGetControl evt
      p <- styledTextEventGetPosition evt
      m <- styledTextEventGetMargin evt
      return $ STCMarginClick alt shift ctrl p m
    needShown evt = do
      p <- styledTextEventGetPosition evt
      len <- styledTextEventGetLength evt
      return $ STCNeedShown p len
    {-
    -- expEVT_STC_POSCHANGED is removed in wxWidgets-2.6.x.
    posChanged evt = do
      p <- styledTextEventGetPosition evt
      return $ STCPosChanged p
    -}
    userListSelection evt = do
      lt <- styledTextEventGetListType evt
      text <- styledTextEventGetText evt
      return $ STCUserListSelection lt text
    uriDropped evt = do
      t <- styledTextEventGetText evt
      return $ STCUriDropped t
    dwellStart evt = do
      x <- styledTextEventGetX evt
      y <- styledTextEventGetY evt
      return $ STCDwellStart (point x y)
    dwellEnd evt = do
      x <- styledTextEventGetX evt
      y <- styledTextEventGetY evt
      return $ STCDwellEnd (point x y)
    startDrag evt = do
      lin <- styledTextEventGetLine evt
      car <- styledTextEventGetPosition evt
      str <- styledTextEventGetDragText evt
      return $ STCStartDrag lin car str
    dragOver evt = do
      x <- styledTextEventGetX evt
      y <- styledTextEventGetY evt
      res <- styledTextEventGetDragResult evt
      return $ STCDragOver (point x y) $ toDragResult res
    doDrop evt = do
      str <- styledTextEventGetDragText evt
      res <- styledTextEventGetDragResult evt
      return $ STCDoDrop str $ toDragResult res

stcOnSTCEvent :: StyledTextCtrl a -> (EventSTC -> IO ()) -> IO ()
stcOnSTCEvent stc handler
  = do windowOnEvent stc stcEventsAll handler eventHandler
  where
    eventHandler event
      = do eventSTC <- fromSTCEvent (objectCast event)
           if isSTCUnknown eventSTC
              then return () -- what else?
              else handler eventSTC
    isSTCUnknown :: EventSTC -> Bool
    isSTCUnknown STCUnknown = True
    isSTCUnknown _ = False
    -- most of the events can probably be ignored
    stcEventsAll = map fst stcEvents

stcGetOnSTCEvent :: StyledTextCtrl a -> IO (EventSTC -> IO ())
stcGetOnSTCEvent window
  = unsafeWindowGetHandlerState window (head $ map fst stcEvents) (\_ev -> skipCurrentEvent)

{-----------------------------------------------------------------------------------------
  Printing
-----------------------------------------------------------------------------------------}
-- | Printer events.
data EventPrint  = PrintBeginDoc (IO ()) Int Int    -- ^ Print a copy: cancel, start page, end page
                 | PrintEndDoc
                 | PrintBegin                       -- ^ Begin a print job.
                 | PrintEnd
                 | PrintPrepare                     -- ^ Prepare: chance to call 'printOutSetPageLimits' for example.
                 | PrintPage (IO ()) (DC ()) Int    -- ^ Print a page: cancel, printer device context, page number.
                 | PrintUnknown Int                 -- ^ Unknown print event with event code

-- | Convert a 'PrintEvent' object to an 'EventPrint' value.
fromPrintEvent :: WXCPrintEvent a -> IO EventPrint
fromPrintEvent event
  = do tp <- eventGetEventType event
       case lookup tp printEvents of
         Just f  -> f event
         Nothing -> return (PrintUnknown tp)

-- | Print event list.
printEvents :: [(Int,WXCPrintEvent a -> IO EventPrint)]
printEvents
  = [(wxEVT_PRINT_PAGE,     \ev -> do page <- wxcPrintEventGetPage ev
                                      pout <- wxcPrintEventGetPrintout ev
                                      dc   <- printoutGetDC pout
                                      let cancel = wxcPrintEventSetContinue ev False
                                      return (PrintPage cancel dc page))
    ,(wxEVT_PRINT_BEGIN_DOC,\ev -> do page <- wxcPrintEventGetPage ev
                                      epage<- wxcPrintEventGetEndPage ev
                                      let cancel = wxcPrintEventSetContinue ev False
                                      return (PrintBeginDoc cancel page epage))
    ,(wxEVT_PRINT_PREPARE, \_ev -> return PrintPrepare)
    ,(wxEVT_PRINT_END_DOC, \_ev -> return PrintEndDoc)
    ,(wxEVT_PRINT_BEGIN,   \_ev -> return PrintBegin)
    ,(wxEVT_PRINT_END,     \_ev -> return PrintEnd)
    ]

-- | Set an event handler for printing.
printOutOnPrint :: WXCPrintout a -> (EventPrint -> IO ()) -> IO ()
printOutOnPrint printOut eventHandler
  = do evtHandler <- wxcPrintoutGetEvtHandler printOut
       evtHandlerOnEvent evtHandler idAny idAny (map fst printEvents)
                         eventHandler (\_ -> return ()) printHandler
  where
    printHandler event
      = do eventPrint <- fromPrintEvent (objectCast event)
           eventHandler eventPrint

-- | Get the current print handler
printOutGetOnPrint :: WXCPrintout a -> IO (EventPrint -> IO ())
printOutGetOnPrint printOut 
  = do evtHandler <- wxcPrintoutGetEvtHandler printOut
       unsafeGetHandlerState evtHandler idAny wxEVT_PRINT_PAGE (\_ev -> skipCurrentEvent)


{-----------------------------------------------------------------------------------------
  Scrolling
-----------------------------------------------------------------------------------------}
-- | Scroll events.
data EventScroll = ScrollTop      !Orientation !Int    -- ^ scroll to top
                 | ScrollBottom   !Orientation !Int    -- ^ scroll to bottom
                 | ScrollLineUp   !Orientation !Int    -- ^ scroll line up
                 | ScrollLineDown !Orientation !Int    -- ^ scroll line down
                 | ScrollPageUp   !Orientation !Int    -- ^ scroll page up
                 | ScrollPageDown !Orientation !Int    -- ^ scroll page down
                 | ScrollTrack    !Orientation !Int    -- ^ frequent event when user drags the thumbtrack
                 | ScrollRelease  !Orientation !Int    -- ^ thumbtrack is released
                 deriving Show

-- | The orientation of a widget.
data Orientation  = Horizontal | Vertical
                  deriving (Eq, Show)


-- | Get the orientation of a scroll event.
scrollOrientation :: EventScroll -> Orientation
scrollOrientation scroll
  = case scroll of
      ScrollTop      orient _pos   -> orient
      ScrollBottom   orient _pos   -> orient
      ScrollLineUp   orient _pos   -> orient
      ScrollLineDown orient _pos   -> orient
      ScrollPageUp   orient _pos   -> orient
      ScrollPageDown orient _pos   -> orient
      ScrollTrack    orient _pos   -> orient
      ScrollRelease  orient _pos   -> orient

-- | Get the position of the scroll bar.
scrollPos :: EventScroll -> Int
scrollPos scroll
  = case scroll of
      ScrollTop      _orient pos   -> pos
      ScrollBottom   _orient pos   -> pos
      ScrollLineUp   _orient pos   -> pos
      ScrollLineDown _orient pos   -> pos
      ScrollPageUp   _orient pos   -> pos
      ScrollPageDown _orient pos   -> pos
      ScrollTrack    _orient pos   -> pos
      ScrollRelease  _orient pos   -> pos



fromScrollEvent :: ScrollWinEvent a -> IO EventScroll
fromScrollEvent event
  = do orient <- scrollWinEventGetOrientation event
       pos    <- scrollWinEventGetPosition event
       tp     <- eventGetEventType event
       let orientation | orient == wxHORIZONTAL  = Horizontal
                       | otherwise               = Vertical
       case lookup tp scrollEvents of
         Just evt  -> return (evt orientation pos)
         Nothing   -> return (ScrollRelease orientation pos)

scrollEvents :: [(Int,Orientation -> Int -> EventScroll)]
scrollEvents
  = [(wxEVT_SCROLLWIN_TOP,        ScrollTop)
    ,(wxEVT_SCROLLWIN_BOTTOM,     ScrollBottom)
    ,(wxEVT_SCROLLWIN_LINEUP,     ScrollLineUp)
    ,(wxEVT_SCROLLWIN_LINEDOWN,   ScrollLineDown)
    ,(wxEVT_SCROLLWIN_PAGEUP,     ScrollPageUp)
    ,(wxEVT_SCROLLWIN_PAGEDOWN,   ScrollPageDown)
    ,(wxEVT_SCROLLWIN_THUMBTRACK, ScrollTrack)
    ,(wxEVT_SCROLLWIN_THUMBRELEASE, ScrollRelease)
    ]

-- | Set a scroll event handler.
windowOnScroll :: Window a -> (EventScroll -> IO ()) -> IO ()
windowOnScroll window eventHandler
  = windowOnEvent window (map fst scrollEvents) eventHandler scrollHandler
  where
    scrollHandler event
      = do eventScroll <- fromScrollEvent (objectCast event)
           eventHandler eventScroll

-- | Get the current scroll event handler of a window.
windowGetOnScroll :: Window a -> IO (EventScroll -> IO ())
windowGetOnScroll window
  = unsafeWindowGetHandlerState window wxEVT_SCROLLWIN_TOP (\_scroll -> skipCurrentEvent)

{--------------------------------------------------------------------------
  HTML event
--------------------------------------------------------------------------}
-- | HTML window events
data EventHtml  
  = HtmlCellClicked String EventMouse  Point 
      -- ^ A /cell/ is clicked. Contains the cell /id/ attribute value, the mouse event and the logical coordinates.
  | HtmlCellHover String 
      -- ^ The mouse hovers over a cell. Contains the cell /id/ attribute value.
  | HtmlLinkClicked String String String EventMouse Point 
     -- ^ A link is clicked. Contains the hyperlink, the frame target, the cell /id/ attribute value, the mouse event, and the logical coordinates.
  | HtmlSetTitle String
     -- ^ Called when a @<title>@ tag is parsed.
  | HtmlUnknown 
     -- ^ Unrecognised HTML event

instance Show EventHtml where
  show ev
    = case ev of
        HtmlCellClicked id' mouse _pnt             -> "HTML Cell " ++ show id' ++ " clicked: " ++ show mouse
        HtmlLinkClicked href _target id' _mouse _p -> "HTML Link " ++ show id' ++ " clicked: " ++ href
        HtmlCellHover   id'                        -> "HTML Cell " ++ show id' ++ " hover"
        HtmlSetTitle    title                      -> "HTML event title: " ++ title
        HtmlUnknown                                -> "HTML event unknown"

fromHtmlEvent :: WXCHtmlEvent a -> IO EventHtml
fromHtmlEvent event
  = do tp <- eventGetEventType event
       case lookup tp htmlEvents of
         Nothing      -> return HtmlUnknown 
         Just action  -> action event
  where
    htmlEvents  = [(wxEVT_HTML_CELL_MOUSE_HOVER,  htmlHover)
                  ,(wxEVT_HTML_CELL_CLICKED,      htmlClicked)
                  ,(wxEVT_HTML_LINK_CLICKED,      htmlLink)
                  ,(wxEVT_HTML_SET_TITLE,         htmlTitle)]

    htmlTitle event'
      = do title <- commandEventGetString event'
           return (HtmlSetTitle title)

    htmlHover event'
      = do id'     <- wxcHtmlEventGetHtmlCellId event'
           return (HtmlCellHover id')

    htmlClicked event'
      = do id'     <- wxcHtmlEventGetHtmlCellId event'
           mouseEv <- wxcHtmlEventGetMouseEvent event'
           mouse   <- fromMouseEvent mouseEv
           pnt     <- wxcHtmlEventGetLogicalPosition event'
           return (HtmlCellClicked id' mouse pnt)

    htmlLink event'
      = do id'     <- wxcHtmlEventGetHtmlCellId event'
           mouseEv <- wxcHtmlEventGetMouseEvent event'
           mouse   <- fromMouseEvent mouseEv
           href    <- wxcHtmlEventGetHref event'
           target  <- wxcHtmlEventGetTarget event'
           pnt     <- wxcHtmlEventGetLogicalPosition event'
           return (HtmlLinkClicked href target id' mouse pnt)
      
-- | Set a html event handler for a HTML window. The first argument determines whether
-- hover events ('HtmlCellHover') are handled or not.
htmlWindowOnHtmlEvent :: WXCHtmlWindow a -> Bool -> (EventHtml -> IO ()) -> IO ()
htmlWindowOnHtmlEvent window allowHover handler
  = windowOnEvent window htmlEvents handler eventHandler
  where
    htmlEvents
      = [wxEVT_HTML_CELL_CLICKED,wxEVT_HTML_LINK_CLICKED,wxEVT_HTML_SET_TITLE]
        ++ (if allowHover then [wxEVT_HTML_CELL_MOUSE_HOVER] else [])

    eventHandler event
      = do eventHtml <- fromHtmlEvent (objectCast event)
           handler eventHtml

-- | Get the current HTML event handler of a HTML window.
htmlWindowGetOnHtmlEvent :: WXCHtmlWindow a -> IO (EventHtml -> IO ())
htmlWindowGetOnHtmlEvent window
  = unsafeWindowGetHandlerState window wxEVT_HTML_CELL_CLICKED (\_ev -> skipCurrentEvent)

     
          

{-----------------------------------------------------------------------------------------
  Close, Destroy, Create
-----------------------------------------------------------------------------------------}
-- | Adds a close handler to the currently installed close handlers.
windowAddOnClose :: Window a -> IO () -> IO ()
windowAddOnClose window new'
  = do prev <- windowGetOnClose window
       windowOnClose window (do { new'; prev })

-- | Set an event handler that is called when the user tries to close a frame or dialog.
-- Don't forget to call the previous handler or 'frameDestroy' explicitly or otherwise the
-- frame won't be closed.
windowOnClose :: Window a -> IO () -> IO ()
windowOnClose window eventHandler
  = windowOnEvent window [wxEVT_CLOSE_WINDOW] eventHandler (\_ev -> eventHandler)

-- | Get the current close event handler.
windowGetOnClose :: Window a -> IO (IO ())
windowGetOnClose window
  = unsafeWindowGetHandlerState window wxEVT_CLOSE_WINDOW (windowDestroy window >> return ())

-- | Set an event handler that is called when the window is destroyed.
-- /Note: does not seem to work on Windows/.
windowOnDestroy :: Window a -> IO () -> IO ()
windowOnDestroy window eventHandler
  = windowOnEvent window [wxEVT_DESTROY] eventHandler (\_ev -> eventHandler)

-- | Get the current destroy event handler.
windowGetOnDestroy :: Window a -> IO (IO ())
windowGetOnDestroy window
  = unsafeWindowGetHandlerState window wxEVT_DESTROY (return ())

-- | Add a delete-event handler to the current installed delete-event handlers.
--
-- > windowAddOnDelete window new
-- >   = do prev <- windowGetOnDelete window
-- >        windowOnDelete window (do{ new; prev })

windowAddOnDelete :: Window a -> IO () -> IO ()
windowAddOnDelete window new'
  = do prev <- windowGetOnDelete window
       windowOnDelete window (do { new'; prev })

-- | Set an event handler that is called when the window is deleted.
-- Use with care as the window itself is in a deletion state.
windowOnDelete :: Window a -> IO () -> IO ()
windowOnDelete window eventHandler
  = windowOnEventEx window [wxEVT_DELETE] eventHandler onDelete (\_ev -> return ())
  where
    onDelete ownerDeleted
      | ownerDeleted  = eventHandler
      | otherwise     = return ()    -- don't run on disconnect!

-- | Get the current delete event handler.
windowGetOnDelete :: Window a -> IO (IO ())
windowGetOnDelete window
  = unsafeWindowGetHandlerState window wxEVT_DELETE (return ())


-- | Set an event handler that is called when the window is created.
windowOnCreate :: Window a -> IO () -> IO ()
windowOnCreate window eventHandler
  = windowOnEvent window [wxEVT_CREATE] eventHandler (\_ev -> eventHandler)

-- | Get the current create event handler.
windowGetOnCreate :: Window a -> IO (IO ())
windowGetOnCreate window
  = unsafeWindowGetHandlerState window wxEVT_CREATE (return ())

-- | Set an event handler that is called when the window is resized.
windowOnSize :: Window a -> IO () -> IO ()
windowOnSize window eventHandler
  = windowOnEvent window [wxEVT_SIZE] eventHandler (\_ev -> eventHandler)

-- | Get the current resize event handler.
windowGetOnSize :: Window a -> IO (IO ())
windowGetOnSize window
  = unsafeWindowGetHandlerState window wxEVT_SIZE (return ())

-- | Set an event handler that is called when the window is activated or deactivated.
-- The event parameter is 'True' when the window is activated.
windowOnActivate :: Window a -> (Bool -> IO ()) -> IO ()
windowOnActivate window eventHandler
  = windowOnEvent window [wxEVT_ACTIVATE] eventHandler activateHandler
  where
    activateHandler event
      = do active <- activateEventGetActive (objectCast event)
           eventHandler active

-- | Get the current activate event handler.
windowGetOnActivate :: Window a -> IO (Bool -> IO ())
windowGetOnActivate window
  = unsafeWindowGetHandlerState window wxEVT_ACTIVATE (\_active -> return ())

-- | Set an event handler that is called when the window gets or loses the focus.
-- The event parameter is 'True' when the window gets the focus.
windowOnFocus :: Window a -> (Bool -> IO ()) -> IO ()
windowOnFocus window eventHandler
  = do windowOnEvent window [wxEVT_SET_FOCUS] eventHandler getFocusHandler
       windowOnEvent window [wxEVT_KILL_FOCUS] eventHandler killFocusHandler
  where
    getFocusHandler _event
      = eventHandler True
    killFocusHandler _event
      = eventHandler False

-- | Get the current focus event handler.
windowGetOnFocus :: Window a -> IO (Bool -> IO ())
windowGetOnFocus window
  = unsafeWindowGetHandlerState window wxEVT_SET_FOCUS (\_getfocus -> return ())


-- | A context menu event is generated when the user right-clicks in a window
-- or presses shift-F10.
windowOnContextMenu :: Window a -> IO () -> IO ()
windowOnContextMenu window eventHandler
  = windowOnEvent window [wxEVT_CONTEXT_MENU] eventHandler (\_ev -> eventHandler)

-- | Get the current context menu event handler.
windowGetOnContextMenu :: Window a -> IO (IO ())
windowGetOnContextMenu window
  = unsafeWindowGetHandlerState window wxEVT_CONTEXT_MENU skipCurrentEvent

-- | A menu event is generated when the user selects a menu item.
-- You should install this handler on the window that owns the menubar or a popup menu.
evtHandlerOnMenuCommand :: EvtHandler a -> Id -> IO () -> IO ()
evtHandlerOnMenuCommand window id' eventHandler
  = evtHandlerOnEvent window id' id' [wxEVT_COMMAND_MENU_SELECTED] eventHandler (\_ -> return ()) (\_ev -> eventHandler)

-- | Get the current event handler for a certain menu.
evtHandlerGetOnMenuCommand :: EvtHandler a -> Id -> IO (IO ())
evtHandlerGetOnMenuCommand window id'
  = unsafeGetHandlerState window id' wxEVT_COMMAND_MENU_SELECTED skipCurrentEvent


-- | An idle event is generated in idle time. The handler should return whether more
-- idle processing is needed ('True') or otherwise the event loop goes into a passive
-- waiting state.
windowOnIdle :: Window a -> IO Bool -> IO ()
windowOnIdle window eventHandler
  = windowOnEvent window [wxEVT_IDLE] eventHandler idleHandler
  where
    idleHandler event
      = do requestMore <- eventHandler
           idleEventRequestMore (objectCast event) requestMore
           return ()

-- | Get the current context menu event handler.
windowGetOnIdle :: Window a -> IO (IO Bool)
windowGetOnIdle window
  = unsafeWindowGetHandlerState window wxEVT_IDLE (return False)


-- | A timer event is generated by an attached timer, see 'windowTimerAttach'.
-- /Broken!/ (use 'timerOnCommand' instead).
windowOnTimer :: Window a -> IO () -> IO ()
windowOnTimer window eventHandler
  = windowOnEvent window [wxEVT_TIMER] eventHandler (\_ev -> eventHandler)

-- | Get the current timer handler.
windowGetOnTimer :: Window a -> IO (IO ())
windowGetOnTimer window
  = unsafeWindowGetHandlerState window wxEVT_TIMER (return ())

{-----------------------------------------------------------------------------------------
  Paint
-----------------------------------------------------------------------------------------}
-- | Set an event handler for /raw/ paint events. Draws directly to the
-- paint device context ('PaintDC') and the 'DC' is not cleared when the handler
-- is called. The handler takes two other arguments: the view rectangle and a
-- list of /dirty/ rectangles. The rectangles contain logical coordinates and
-- are already adjusted for scrolled windows.
-- Note: you can not set both a 'windowOnPaintRaw' and 'windowOnPaint' handler!
windowOnPaintRaw :: Window a -> (PaintDC () -> Rect -> [Rect] -> IO ()) -> IO ()
windowOnPaintRaw window paintHandler
  = windowOnEvent window [wxEVT_PAINT] paintHandler onPaint 
  where
    onPaint event
      = do obj <- eventGetEventObject event
           if (obj==objectNull)
            then return ()
            else do let window' = objectCast obj
                    region <- windowGetUpdateRects window'
                    view   <- windowGetViewRect window'
                    withPaintDC window' (\paintDC ->
                     do isScrolled <- objectIsScrolledWindow window'
                        when (isScrolled) (scrolledWindowPrepareDC (objectCast window') paintDC)
                        paintHandler paintDC view region)

-- | Get the current /raw/ paint event handler. 
windowGetOnPaintRaw :: Window a -> IO (PaintDC () -> Rect -> [Rect] -> IO ())
windowGetOnPaintRaw window
  = unsafeWindowGetHandlerState window wxEVT_PAINT (\_dc _rect _region -> return ())

-- | Get the current paint event handler.
windowGetOnPaintGc :: Window a -> IO (GCDC () -> Rect -> IO ())
windowGetOnPaintGc window
  = unsafeWindowGetHandlerState window wxEVT_PAINT (\_dc _view -> return ())


-- | Set an event handler for paint events. The implementation uses an 
-- intermediate buffer for non-flickering redraws. 
-- The device context ('DC')
-- is always cleared before the paint handler is called. The paint handler
-- also gets the currently visible view area as an argument (adjusted for scrolling).
-- Note: you can not set both a 'windowOnPaintRaw' and 'windowOnPaint' handler!
windowOnPaint :: Window a -> (DC () -> Rect -> IO ()) -> IO ()
windowOnPaint window paintHandler
  | wxToolkit == WxMac  = windowOnPaintRaw window (\dc view _ -> paintHandler (downcastDC dc) view)
  | otherwise
  = do v <- varCreate objectNull
       windowOnEventEx window [wxEVT_PAINT] paintHandler (destroy v) (onPaint v)
  where
    destroy v _ownerDeleted
      = do bitmap <- varSwap v objectNull
           when (not (objectIsNull bitmap)) (bitmapDelete bitmap)

    onPaint v event
      = do obj <- eventGetEventObject event
           if (obj==objectNull)
            then return ()
            else do let window' = objectCast obj
                    view  <- windowGetViewRect window'
                    withPaintDC window (\paintDC ->
                     do isScrolled <- objectIsScrolledWindow window'
                        when (isScrolled) (scrolledWindowPrepareDC (objectCast window') paintDC)
                        -- Note: wxMSW 2.4 does not clear the properly scrolled view rectangle.
                        let clear dc  | wxToolkit == WxMSW  = dcClearRect dc view
                                      | otherwise           = dcClear dc
                        -- and repaint with buffer
                        dcBufferWithRefEx paintDC clear (Just v) view (\dc -> paintHandler dc view))

-- | Set an event handler for GCDC paint events. The implementation uses an 
-- intermediate buffer for non-flickering redraws. 
-- The device context ('GCDC')
-- is always cleared before the paint handler is called. The paint handler
-- also gets the currently visible view area as an argument (adjusted for scrolling).
-- Note: you can not set both a 'windowOnPaintRaw' and 'windowOnPaint' handler!
windowOnPaintGc :: Window a -> (GCDC () -> Rect -> IO ()) -> IO ()
windowOnPaintGc window paintHandler
  | wxToolkit == WxMac  = windowOnPaintRaw window
                          (\dc_ view _ -> do
                            dc <- gcdcCreate dc_
                            paintHandler dc view
                            gcdcDelete dc)
  | otherwise
  = do v <- varCreate objectNull
       windowOnEventEx window [wxEVT_PAINT] paintHandler (destroy v) (onPaint v)
  where
    destroy v _ownerDeleted
      = do bitmap <- varSwap v objectNull
           when (not (objectIsNull bitmap)) (bitmapDelete bitmap)

    onPaint v event
      = do obj <- eventGetEventObject event
           if (obj==objectNull)
            then return ()
            else do let window' = objectCast obj
                    view  <- windowGetViewRect window'
                    withPaintDC window (\paintDC ->
                     do isScrolled <- objectIsScrolledWindow window'
                        when (isScrolled) (scrolledWindowPrepareDC (objectCast window') paintDC)
                        -- Note: wxMSW 2.4 does not clear the properly scrolled view rectangle.
                        let clear dc  | wxToolkit == WxMSW  = dcClearRect dc view
                                      | otherwise           = dcClear dc
                        -- and repaint with buffer
                        dcBufferWithRefExGcdc paintDC clear (Just v) view (\dc -> paintHandler dc view))

-- | Get the current paint event handler.
windowGetOnPaint :: Window a -> IO (DC () -> Rect -> IO ())
windowGetOnPaint window
  = unsafeWindowGetHandlerState window wxEVT_PAINT (\_dc _view -> return ())


-- Get the logical /dirty/ rectangles as a list of 'Rect'.
windowGetUpdateRects :: Window a -> IO [Rect]
windowGetUpdateRects window
  = do region <- windowGetUpdateRegion window
       iter   <- regionIteratorCreateFromRegion region
       rects  <- getRects iter
       regionIteratorDelete iter
       p <- windowGetViewStart window
       return (map (\r -> rectMove r (vecFromPoint p)) rects)
  where
    getRects iter
      = do more <- regionIteratorHaveRects iter
           if more
            then do x <- regionIteratorGetX iter
                    y <- regionIteratorGetY iter
                    w <- regionIteratorGetWidth iter
                    h <- regionIteratorGetHeight iter
                    regionIteratorNext iter
                    rs <- getRects iter
                    return (rect (pt x y) (sz w h) : rs)
            else return []


{-----------------------------------------------------------------------------------------
  Modifiers
-----------------------------------------------------------------------------------------}
-- | Called when a process is ended with the process @pid@ and exitcode.
evtHandlerOnEndProcess :: EvtHandler a -> (Int -> Int -> IO ()) -> IO ()
evtHandlerOnEndProcess  evtHandler handler
  = evtHandlerOnEvent evtHandler (-1) (-1) [wxEVT_END_PROCESS] handler onDelete onEndProcess
  where
    onDelete _ownerDeleted
      = return ()

    onEndProcess event
      = let processEvent = objectCast event
        in  do pid  <- processEventGetPid processEvent
               code <- processEventGetExitCode processEvent
               handler pid code


-- | Retrieve the current end process handler.
evtHandlerGetOnEndProcess :: EvtHandler a -> IO (Int -> Int -> IO ())
evtHandlerGetOnEndProcess evtHandler
  = unsafeGetHandlerState evtHandler (-1) wxEVT_END_PROCESS (\_pid _code -> return ())


-- | The status of a stream (see 'StreamBase')
data StreamStatus = StreamOk          -- ^ No error.
                  | StreamEof         -- ^ No more input.
                  | StreamReadError   -- ^ Read error.
                  | StreamWriteError  -- ^ Write error.
                  deriving (Eq,Show)

-- | Convert a stream status code into 'StreamStatus'.
streamStatusFromInt :: Int -> StreamStatus
streamStatusFromInt code
  | code == wxSTREAM_NO_ERROR     = StreamOk
  | code == wxSTREAM_EOF          = StreamEof
  | code == wxSTREAM_READ_ERROR   = StreamReadError
  | code == wxSTREAM_WRITE_ERROR  = StreamWriteError
  | otherwise                     = StreamReadError


-- | Install an event handler on an input stream. The handler is called
-- whenever input is read (or when an error occurred). The third parameter
-- gives the size of the input batches. The original input stream should no longer be referenced after this call!
evtHandlerOnInput :: EvtHandler b -> (String -> StreamStatus -> IO ()) -> InputStream a -> Int -> IO ()
evtHandlerOnInput evtHandler handler stream bufferLen
  = do sink <- inputSinkCreate stream evtHandler bufferLen
       evtHandlerOnInputSink evtHandler handler sink
       inputSinkStart sink

-- | Install an event handler on a specific input sink. It is advised to
-- use the 'evtHandlerOnInput' whenever retrieval of the handler is not necessary.
evtHandlerOnInputSink :: EvtHandler b -> (String -> StreamStatus -> IO ()) -> InputSink a -> IO ()
evtHandlerOnInputSink evtHandler handler sink
  = do id' <- inputSinkGetId sink
       evtHandlerOnEvent evtHandler id' id' [wxEVT_INPUT_SINK] handler onDelete onInput
  where
    onDelete _ownerDeleted
      = return ()

    onInput event
      = let inputSinkEvent = objectCast event
        in  do input <- inputSinkEventLastString inputSinkEvent
               code  <- inputSinkEventLastError inputSinkEvent
               handler input (streamStatusFromInt code)


-- | Retrieve the current input stream handler.
evtHandlerGetOnInputSink :: EvtHandler b -> IO (String -> StreamStatus -> IO ())
evtHandlerGetOnInputSink evtHandler
  = unsafeGetHandlerState evtHandler (-1) wxEVT_INPUT_SINK (\_input _status -> return ())

-- | Read the input from an 'InputSinkEvent'.
inputSinkEventLastString :: InputSinkEvent a -> IO String
inputSinkEventLastString inputSinkEvent
  = do n <- inputSinkEventLastRead inputSinkEvent
       if (n <= 0)
        then return ""
        else do buffer <- inputSinkEventLastInput inputSinkEvent
                peekCWStringLen (buffer,n)


{-----------------------------------------------------------------------------------------
  Modifiers
-----------------------------------------------------------------------------------------}
-- | The @Modifiers@ indicate the meta keys that have been pressed ('True') or not ('False').
data Modifiers  = Modifiers
                  { altDown     :: !Bool   -- ^ alt key down
                  , shiftDown   :: !Bool   -- ^ shift key down
                  , controlDown :: !Bool   -- ^ control key down
                  , metaDown    :: !Bool   -- ^ meta key down
                  }
                  deriving (Eq)

instance Show Modifiers where
  show mods = showModifiers mods

-- | Show modifiers, for example for use in menus.
showModifiers :: Modifiers -> String
showModifiers mods
  = concat $ intersperse "+" $ filter (not.null)
    [if controlDown mods then "Ctrl" else ""
    ,if altDown mods     then "Alt" else ""
    ,if shiftDown mods   then "Shift" else ""
    ,if metaDown mods    then "Meta" else ""
    ]


-- | Construct a 'Modifiers' structure with no meta keys pressed.
noneDown :: Modifiers
noneDown = Modifiers False False False False

-- | Construct a 'Modifiers' structure with just Shift meta key pressed.
justShift   :: Modifiers
justShift   = noneDown{ shiftDown = True }

-- | Construct a 'Modifiers' structure with just Alt meta key pressed.
justAlt     :: Modifiers
justAlt     = noneDown{ altDown = True }

-- | Construct a 'Modifiers' structure with just Ctrl meta key pressed.
justControl :: Modifiers
justControl = noneDown{ controlDown = True }

-- | Construct a 'Modifiers' structure with just Meta meta key pressed.
justMeta :: Modifiers
justMeta = noneDown{ metaDown = True }

-- | Test if no meta key was pressed.
isNoneDown :: Modifiers -> Bool
isNoneDown (Modifiers shift control alt meta) = not (shift || control || alt || meta)

-- | Test if no shift, alt, or control key was pressed.
isNoShiftAltControlDown :: Modifiers -> Bool
isNoShiftAltControlDown (Modifiers shift control alt _meta) = not (shift || control || alt)

-- | Tranform modifiers into an accelerator modifiers code.
modifiersToAccelFlags :: Modifiers -> Int
modifiersToAccelFlags mod'
  = mask (altDown mod') 0x01 + mask (controlDown mod') 0x02 + mask (shiftDown mod') 0x04
  where
    mask test flag = if test then flag else 0

{-----------------------------------------------------------------------------------------
  MouseEvent
-----------------------------------------------------------------------------------------}
-- | Mouse events. The 'Point' gives the logical (unscrolled) position.
data EventMouse
  =  MouseMotion      !Point !Modifiers -- ^ Mouse was moved over the client area of the window
  |  MouseEnter       !Point !Modifiers -- ^ Mouse enters in the client area of the window
  |  MouseLeave       !Point !Modifiers -- ^ Mouse leaves the client area of the window
  |  MouseLeftDown    !Point !Modifiers -- ^ Mouse left button goes down
  |  MouseLeftUp      !Point !Modifiers -- ^ Mouse left  button goes up
  |  MouseLeftDClick  !Point !Modifiers -- ^ Mouse left button double click
  |  MouseLeftDrag    !Point !Modifiers -- ^ Mouse left button drag
  |  MouseRightDown   !Point !Modifiers -- ^ Mouse right button goes down
  |  MouseRightUp     !Point !Modifiers -- ^ Mouse right  button goes up
  |  MouseRightDClick !Point !Modifiers -- ^ Mouse right button double click
  |  MouseRightDrag   !Point !Modifiers -- ^ Mouse right button drag (unsupported on most platforms)
  |  MouseMiddleDown  !Point !Modifiers -- ^ Mouse middle button goes down
  |  MouseMiddleUp    !Point !Modifiers -- ^ Mouse middle  button goes up
  |  MouseMiddleDClick !Point !Modifiers -- ^ Mouse middle button double click
  |  MouseMiddleDrag  !Point !Modifiers -- ^ Mouse middle button drag (unsupported on most platforms)
  |  MouseWheel !Bool !Point !Modifiers -- ^ Mouse wheel rotation. (Bool is True for a downward rotation)
  deriving (Eq) -- ,Show)


instance Show EventMouse where
  show mouse  = showMouse mouse

-- | Show an 'EventMouse' in a user friendly way.
showMouse :: EventMouse -> String
showMouse mouse
  = (if (null modsText) then "" else modsText ++ "+") ++ action ++ " at " ++ show (x,y)
  where
    modsText     = show (mouseModifiers mouse)
    (Point x y)  = mousePos mouse
    action
      = case mouse of
          MouseMotion _p _m       -> "Motion"
          MouseEnter _p _m        -> "Enter"
          MouseLeave _p _m        -> "Leave"
          MouseLeftDown _p _m     -> "Left down"
          MouseLeftUp _p _m       -> "Left up"
          MouseLeftDClick _p _m   -> "Left double click"
          MouseLeftDrag _p _m     -> "Left drag"
          MouseRightDown _p _m    -> "Right down"
          MouseRightUp _p _m      -> "Right up"
          MouseRightDClick _p _m  -> "Right double click"
          MouseRightDrag _p _m    -> "Right drag"
          MouseMiddleDown _p _m   -> "Middle down"
          MouseMiddleUp _p _m     -> "Middle up"
          MouseMiddleDClick _p _m -> "Middle double click"
          MouseMiddleDrag _p _m   -> "Middle drag"
          MouseWheel down _p _m   -> "Wheel " ++ (if down then "down" else "up")


-- | Extract the position from a 'MouseEvent'.
mousePos :: EventMouse -> Point
mousePos mouseEvent
  = case mouseEvent of
      MouseMotion p _m       -> p
      MouseEnter p _m        -> p
      MouseLeave p _m        -> p
      MouseLeftDown p _m     -> p
      MouseLeftUp p _m       -> p
      MouseLeftDClick p _m   -> p
      MouseLeftDrag p _m     -> p
      MouseRightDown p _m    -> p
      MouseRightUp p _m      -> p
      MouseRightDClick p _m  -> p
      MouseRightDrag p _m    -> p
      MouseMiddleDown p _m   -> p
      MouseMiddleUp p _m     -> p
      MouseMiddleDClick p _m -> p
      MouseMiddleDrag p _m   -> p
      MouseWheel _ p _m      -> p

-- | Extract the modifiers from a 'MouseEvent'.
mouseModifiers :: EventMouse -> Modifiers
mouseModifiers mouseEvent
  = case mouseEvent of
      MouseMotion _p m       -> m
      MouseEnter _p m        -> m
      MouseLeave _p m        -> m
      MouseLeftDown _p m     -> m
      MouseLeftUp _p m       -> m
      MouseLeftDClick _p m   -> m
      MouseLeftDrag _p m     -> m
      MouseRightDown _p m    -> m
      MouseRightUp _p m      -> m
      MouseRightDClick _p m  -> m
      MouseRightDrag _p m    -> m
      MouseMiddleDown _p m   -> m
      MouseMiddleUp _p m     -> m
      MouseMiddleDClick _p m -> m
      MouseMiddleDrag _p m   -> m
      MouseWheel _ _p m      -> m

fromMouseEvent :: MouseEvent a -> IO EventMouse
fromMouseEvent event
  = do x <- mouseEventGetX event
       y <- mouseEventGetY event
       obj   <- eventGetEventObject event
       point' <- windowCalcUnscrolledPosition (objectCast obj) (Point x y)

       altDown'     <- mouseEventAltDown event
       controlDown' <- mouseEventControlDown event
       shiftDown'   <- mouseEventShiftDown event
       metaDown'    <- mouseEventMetaDown event
       let modifiers = Modifiers altDown' shiftDown' controlDown' metaDown'

       dragging    <- mouseEventDragging event
       if (dragging)
        then do leftDown <- mouseEventLeftIsDown event
                if (leftDown)
                 then return (MouseLeftDrag point' modifiers)
                 else do middleDown <- mouseEventMiddleIsDown event
                         if (middleDown)
                          then return (MouseMiddleDrag point' modifiers)
                          else do rightDown <- mouseEventRightIsDown event
                                  if (rightDown)
                                   then return (MouseRightDrag point' modifiers)
                                   else return (MouseMotion point' modifiers)
        else do tp <- eventGetEventType event
                case lookup tp mouseEventTypes of
                  Just mouse  -> return (mouse point' modifiers)
                  Nothing     -> if (tp==wxEVT_MOUSEWHEEL)
                                  then do rot   <- mouseEventGetWheelRotation event
                                          delta <- mouseEventGetWheelDelta event
                                          if (abs rot >= delta)
                                           then return (MouseWheel (rot<0) point' modifiers)
                                           else return (MouseMotion point' modifiers)
                                  else return (MouseMotion point' modifiers)

mouseEventTypes :: [(Int,Point -> Modifiers -> EventMouse)]
mouseEventTypes
  = [(wxEVT_MOTION       , MouseMotion)         -- must be the first element, see "windowOnMouse"
    ,(wxEVT_ENTER_WINDOW , MouseEnter)
    ,(wxEVT_LEAVE_WINDOW , MouseLeave)
    ,(wxEVT_LEFT_DOWN    , MouseLeftDown)
    ,(wxEVT_LEFT_UP      , MouseLeftUp)
    ,(wxEVT_LEFT_DCLICK  , MouseLeftDClick)
    ,(wxEVT_MIDDLE_DOWN  , MouseMiddleDown)
    ,(wxEVT_MIDDLE_UP    , MouseMiddleUp)
    ,(wxEVT_MIDDLE_DCLICK, MouseMiddleDClick)
    ,(wxEVT_RIGHT_DOWN   , MouseRightDown)
    ,(wxEVT_RIGHT_UP     , MouseRightUp)
    ,(wxEVT_RIGHT_DCLICK , MouseRightDClick)
    ]

-- | Set a mouse event handler for a window. The first argument determines whether
-- mouse motion events ('MouseMotion') are handled or not.
windowOnMouse :: Window a -> Bool -> (EventMouse -> IO ()) -> IO ()
windowOnMouse window allowMotion handler
  = windowOnEvent window mouseEvents handler eventHandler
  where
    mouseEvents
      = (map fst (if allowMotion then mouseEventTypes else tail (mouseEventTypes))) ++ [wxEVT_MOUSEWHEEL]

    eventHandler event
      = do eventMouse <- fromMouseEvent (objectCast event)
           handler eventMouse

-- | Get the current mouse event handler of a window.
windowGetOnMouse :: Window a -> IO (EventMouse -> IO ())
windowGetOnMouse window
  = unsafeWindowGetHandlerState window wxEVT_ENTER_WINDOW (\_ev -> skipCurrentEvent)


{-----------------------------------------------------------------------------------------
  KeyboardEvent
-----------------------------------------------------------------------------------------}
-- | Set an event handler for untranslated key presses. If 'skipCurrentEvent' is not
-- called, the corresponding 'windowOnKeyChar' eventhandler won't be called.
windowOnKeyDown :: Window a -> (EventKey -> IO ()) -> IO ()
windowOnKeyDown window handler
  = windowOnEvent window [wxEVT_KEY_DOWN] handler eventHandler
  where
    eventHandler event
      = do eventKey <- eventKeyFromEvent (objectCast event)
           handler eventKey

-- | Get the current key down handler of a window.
windowGetOnKeyDown :: Window a -> IO (EventKey -> IO ())
windowGetOnKeyDown window
  = unsafeWindowGetHandlerState window wxEVT_KEY_DOWN (\_eventKey -> skipCurrentEvent)


-- | Set an event handler for translated key presses.
windowOnKeyChar :: Window a -> (EventKey -> IO ()) -> IO ()
windowOnKeyChar window handler
  = windowOnEvent window [wxEVT_CHAR] handler eventHandler
  where
    eventHandler event
      = do eventKey <- eventKeyFromEvent (objectCast event)
           handler eventKey

-- | Get the current translated key handler of a window.
windowGetOnKeyChar :: Window a -> IO (EventKey -> IO ())
windowGetOnKeyChar window
  = unsafeWindowGetHandlerState window wxEVT_CHAR (\_eventKey -> skipCurrentEvent)


-- | Set an event handler for (untranslated) key releases.
windowOnKeyUp :: Window a -> (EventKey -> IO ()) -> IO ()
windowOnKeyUp window handler
  = windowOnEvent window [wxEVT_KEY_UP] handler eventHandler
  where
    eventHandler event
      = do eventKey <- eventKeyFromEvent (objectCast event)
           handler eventKey

-- | Get the current key release handler of a window.
windowGetOnKeyUp :: Window a -> IO (EventKey -> IO ())
windowGetOnKeyUp window
  = unsafeWindowGetHandlerState window wxEVT_KEY_UP (\_keyInfo -> skipCurrentEvent)


eventKeyFromEvent :: KeyEvent a -> IO EventKey
eventKeyFromEvent event
  = do x <- keyEventGetX event
       y <- keyEventGetY event
       obj    <- eventGetEventObject event
       point' <- if objectIsNull obj
                 then return (Point x y)
                 else windowCalcUnscrolledPosition (objectCast obj) (Point x y)

       altDown'     <- keyEventAltDown event
       controlDown' <- keyEventControlDown event
       shiftDown'   <- keyEventShiftDown event
       metaDown'    <- keyEventMetaDown event
       let modifiers = Modifiers altDown' shiftDown' controlDown' metaDown'

       keyCode <- keyEventGetKeyCode event
       let key = keyCodeToKey keyCode

       return (EventKey key modifiers point')



-- | A keyboard event contains the key, the modifiers and the focus point.
data EventKey  = EventKey !Key !Modifiers !Point
               deriving (Eq,Show)

-- | Extract the key from a keyboard event.
keyKey :: EventKey -> Key
keyKey (EventKey key _mods _pos) = key

-- | Extract the modifiers from a keyboard event.
keyModifiers :: EventKey -> Modifiers
keyModifiers (EventKey _key mods _pos) = mods

-- | Extract the position from a keyboard event.
keyPos :: EventKey -> Point
keyPos (EventKey _key _mods pos) = pos


-- | A low-level virtual key code.
type KeyCode  = Int

-- | A 'Key' represents a single key on a keyboard.
data Key
  = KeyChar  !Char        -- ^ An ascii code.
  | KeyOther !KeyCode     -- ^ An unknown virtual key.
  | KeyBack
  | KeyTab
  | KeyReturn
  | KeyEscape
  | KeySpace
  | KeyDelete
  | KeyInsert
  | KeyEnd
  | KeyHome
  | KeyLeft
  | KeyUp
  | KeyRight
  | KeyDown
  | KeyPageUp
  | KeyPageDown
  | KeyStart
  | KeyClear
  | KeyShift
  | KeyAlt
  | KeyControl
  | KeyMenu
  | KeyPause
  | KeyCapital
  | KeyHelp
  | KeySelect
  | KeyPrint
  | KeyExecute
  | KeySnapshot
  | KeyCancel
  | KeyLeftButton
  | KeyRightButton
  | KeyMiddleButton
  | KeyNum0
  | KeyNum1
  | KeyNum2
  | KeyNum3
  | KeyNum4
  | KeyNum5
  | KeyNum6
  | KeyNum7
  | KeyNum8
  | KeyNum9
  | KeyMultiply
  | KeyAdd
  | KeySeparator
  | KeySubtract
  | KeyDecimal
  | KeyDivide
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyF13
  | KeyF14
  | KeyF15
  | KeyF16
  | KeyF17
  | KeyF18
  | KeyF19
  | KeyF20
  | KeyF21
  | KeyF22
  | KeyF23
  | KeyF24
  | KeyNumLock
  | KeyScroll
{- Note: If we add "deriving (Show)" we get a strange link error in ghci:
    Loading package wxh ... linking ... Overflown relocs: 122
-}
  deriving (Eq)

{-
  | KeyNumSpace
  | KeyNumTab
  | KeyNumEnter
  | KeyNumF1
  | KeyNumF2
  | KeyNumF3
  | KeyNumF4
  | KeyNumHome
  | KeyNumLeft
  | KeyNumUp
  | KeyNumRight
  | KeyNumDown
  | KeyNumPageUp
  | KeyNumPageDown
  | KeyNumEnd
  | KeyNumBegin
  | KeyNumInsert
  | KeyNumDelete
  | KeyNumEqual
  | KeyNumMultiply
  | KeyNumAdd
  | KeyNumSeparator
  | KeyNumSubstract
  | KeyNumDecimal
  | KeyNumSubstract
-}

-- | From a key to a virtual key code.
keyToKeyCode :: Key -> KeyCode
keyToKeyCode key
  = case key of
      KeyChar c       -> fromEnum c
      KeyOther code   -> code
      KeyBack         -> wxK_BACK
      KeyTab          -> wxK_TAB
      KeyReturn       -> wxK_RETURN
      KeyEscape       -> wxK_ESCAPE
      KeySpace        -> wxK_SPACE
      KeyDelete       -> wxK_DELETE
      KeyInsert       -> wxK_INSERT
      KeyEnd          -> wxK_END
      KeyHome         -> wxK_HOME
      KeyLeft         -> wxK_LEFT
      KeyUp           -> wxK_UP
      KeyRight        -> wxK_RIGHT
      KeyDown         -> wxK_DOWN
      KeyPageUp       -> wxK_PAGEUP
      KeyPageDown     -> wxK_PAGEDOWN
      KeyStart        -> wxK_START
      KeyClear        -> wxK_CLEAR
      KeyShift        -> wxK_SHIFT
      KeyAlt          -> wxK_ALT
      KeyControl      -> wxK_CONTROL
      KeyMenu         -> wxK_MENU
      KeyPause        -> wxK_PAUSE
      KeyCapital      -> wxK_CAPITAL
      KeyHelp         -> wxK_HELP
      KeySelect       -> wxK_SELECT
      KeyPrint        -> wxK_PRINT
      KeyExecute      -> wxK_EXECUTE
      KeySnapshot     -> wxK_SNAPSHOT
      KeyCancel       -> wxK_CANCEL
      KeyLeftButton   -> wxK_LBUTTON
      KeyRightButton  -> wxK_RBUTTON
      KeyMiddleButton -> wxK_MBUTTON
      KeyNum0         -> wxK_NUMPAD0
      KeyNum1         -> wxK_NUMPAD1
      KeyNum2         -> wxK_NUMPAD2
      KeyNum3         -> wxK_NUMPAD3
      KeyNum4         -> wxK_NUMPAD4
      KeyNum5         -> wxK_NUMPAD5
      KeyNum6         -> wxK_NUMPAD6
      KeyNum7         -> wxK_NUMPAD7
      KeyNum8         -> wxK_NUMPAD8
      KeyNum9         -> wxK_NUMPAD9
      KeyMultiply     -> wxK_MULTIPLY
      KeyAdd          -> wxK_ADD
      KeySeparator    -> wxK_SEPARATOR
      KeySubtract     -> wxK_SUBTRACT
      KeyDecimal      -> wxK_DECIMAL
      KeyDivide       -> wxK_DIVIDE
      KeyF1           -> wxK_F1
      KeyF2           -> wxK_F2
      KeyF3           -> wxK_F3
      KeyF4           -> wxK_F4
      KeyF5           -> wxK_F5
      KeyF6           -> wxK_F6
      KeyF7           -> wxK_F7
      KeyF8           -> wxK_F8
      KeyF9           -> wxK_F9
      KeyF10          -> wxK_F10
      KeyF11          -> wxK_F11
      KeyF12          -> wxK_F12
      KeyF13          -> wxK_F13
      KeyF14          -> wxK_F14
      KeyF15          -> wxK_F15
      KeyF16          -> wxK_F16
      KeyF17          -> wxK_F17
      KeyF18          -> wxK_F18
      KeyF19          -> wxK_F19
      KeyF20          -> wxK_F20
      KeyF21          -> wxK_F21
      KeyF22          -> wxK_F22
      KeyF23          -> wxK_F23
      KeyF24          -> wxK_F24
      KeyNumLock      -> wxK_NUMLOCK
      KeyScroll       -> wxK_SCROLL

-- | A virtual key code to a key.
keyCodeToKey :: KeyCode -> Key
keyCodeToKey keyCode
  = if (keyCode < wxK_DELETE && keyCode > wxK_SPACE)     -- optimize for the common case
     then KeyChar (toEnum keyCode)
     else case IntMap.lookup keyCode keyCodeMap of
            Just key -> key
            Nothing  | keyCode <= 255  -> KeyChar (toEnum keyCode)
                     | otherwise       -> KeyOther keyCode

-- Use a big-endian patricia tree to efficiently map key codes to Haskell keys.
-- Since it is a static map, we could maybe use one of Knuth's optimally balanced
-- trees....
keyCodeMap :: IntMap.IntMap Key
keyCodeMap
  = IntMap.fromList
    [(wxK_BACK         , KeyBack)
    ,(wxK_TAB          , KeyTab)
    ,(wxK_RETURN       , KeyReturn)
    ,(wxK_ESCAPE       , KeyEscape)
    ,(wxK_SPACE        , KeySpace)
    ,(wxK_DELETE       , KeyDelete)
    ,(wxK_INSERT       , KeyInsert)
    ,(wxK_END          , KeyEnd)
    ,(wxK_HOME         , KeyHome)
    ,(wxK_LEFT         , KeyLeft)
    ,(wxK_UP           , KeyUp)
    ,(wxK_RIGHT        , KeyRight)
    ,(wxK_DOWN         , KeyDown)
    ,(wxK_PAGEUP       , KeyPageUp)
    ,(wxK_PAGEDOWN     , KeyPageDown)
    ,(wxK_START        , KeyStart)
    ,(wxK_CLEAR        , KeyClear)
    ,(wxK_SHIFT        , KeyShift)
    ,(wxK_ALT          , KeyAlt)
    ,(wxK_CONTROL      , KeyControl)
    ,(wxK_MENU         , KeyMenu)
    ,(wxK_PAUSE        , KeyPause)
    ,(wxK_CAPITAL      , KeyCapital)
    ,(wxK_HELP         , KeyHelp)
    ,(wxK_SELECT       , KeySelect)
    ,(wxK_PRINT        , KeyPrint)
    ,(wxK_EXECUTE      , KeyExecute)
    ,(wxK_SNAPSHOT     , KeySnapshot)
    ,(wxK_CANCEL       , KeyCancel)
    ,(wxK_LBUTTON      , KeyLeftButton)
    ,(wxK_RBUTTON      , KeyRightButton)
    ,(wxK_MBUTTON      , KeyMiddleButton)
    ,(wxK_NUMPAD0      , KeyNum0)
    ,(wxK_NUMPAD1      , KeyNum1)
    ,(wxK_NUMPAD2      , KeyNum2)
    ,(wxK_NUMPAD3      , KeyNum3)
    ,(wxK_NUMPAD4      , KeyNum4)
    ,(wxK_NUMPAD5      , KeyNum5)
    ,(wxK_NUMPAD6      , KeyNum6)
    ,(wxK_NUMPAD7      , KeyNum7)
    ,(wxK_NUMPAD8      , KeyNum8)
    ,(wxK_NUMPAD9      , KeyNum9)
    ,(wxK_MULTIPLY     , KeyMultiply)
    ,(wxK_ADD          , KeyAdd)
    ,(wxK_SEPARATOR    , KeySeparator)
    ,(wxK_SUBTRACT     , KeySubtract)
    ,(wxK_DECIMAL      , KeyDecimal)
    ,(wxK_DIVIDE       , KeyDivide)
    ,(wxK_F1           , KeyF1)
    ,(wxK_F2           , KeyF2)
    ,(wxK_F3           , KeyF3)
    ,(wxK_F4           , KeyF4)
    ,(wxK_F5           , KeyF5)
    ,(wxK_F6           , KeyF6)
    ,(wxK_F7           , KeyF7)
    ,(wxK_F8           , KeyF8)
    ,(wxK_F9           , KeyF9)
    ,(wxK_F10          , KeyF10)
    ,(wxK_F11          , KeyF11)
    ,(wxK_F12          , KeyF12)
    ,(wxK_F13          , KeyF13)
    ,(wxK_F14          , KeyF14)
    ,(wxK_F15          , KeyF15)
    ,(wxK_F16          , KeyF16)
    ,(wxK_F17          , KeyF17)
    ,(wxK_F18          , KeyF18)
    ,(wxK_F19          , KeyF19)
    ,(wxK_F20          , KeyF20)
    ,(wxK_F21          , KeyF21)
    ,(wxK_F22          , KeyF22)
    ,(wxK_F23          , KeyF23)
    ,(wxK_F24          , KeyF24)
    ,(wxK_NUMLOCK      , KeyNumLock)
    ,(wxK_SCROLL       , KeyScroll)
    -- translate with loss of information
    ,(wxK_NUMPAD_SPACE , KeySpace)
    ,(wxK_NUMPAD_TAB   , KeyTab)
    ,(wxK_NUMPAD_ENTER , KeyReturn)
    ,(wxK_NUMPAD_F1    , KeyF1)
    ,(wxK_NUMPAD_F2    , KeyF2)
    ,(wxK_NUMPAD_F3    , KeyF3)
    ,(wxK_NUMPAD_F4    , KeyF4)
    ,(wxK_NUMPAD_HOME  , KeyHome)
    ,(wxK_NUMPAD_LEFT  , KeyLeft)
    ,(wxK_NUMPAD_UP    , KeyUp)
    ,(wxK_NUMPAD_RIGHT , KeyRight)
    ,(wxK_NUMPAD_DOWN  , KeyDown)
    ,(wxK_NUMPAD_PAGEUP   , KeyPageUp)
    ,(wxK_NUMPAD_PAGEDOWN , KeyPageDown)
    ,(wxK_NUMPAD_END      , KeyEnd)
--            ,(wxK_NUMPAD_BEGIN    , KeyBegin)
    ,(wxK_NUMPAD_INSERT   , KeyInsert)
    ,(wxK_NUMPAD_DELETE   , KeyDelete)
--            ,(wxK_NUMPAD_EQUAL    , KeyEqual)
    ,(wxK_NUMPAD_MULTIPLY , KeyMultiply)
    ,(wxK_NUMPAD_ADD      , KeyAdd)
    ,(wxK_NUMPAD_SEPARATOR  , KeySeparator)
    ,(wxK_NUMPAD_SUBTRACT   , KeySubtract)
    ,(wxK_NUMPAD_DECIMAL    , KeyDecimal)
    ,(wxK_NUMPAD_DIVIDE     , KeyDivide)
    ]


instance Show Key where
  show k  = showKey k

-- | Show a key\/modifiers combination, for example for use in menus.
showKeyModifiers :: Key -> Modifiers -> String
showKeyModifiers key mods
  | null modsText = show key
  | otherwise     = modsText ++ "+" ++ show key
  where
    modsText = show mods

-- | Show a key for use in menus for example.
showKey :: Key -> String
showKey key
  = case key of
      KeyChar c       -> [c]
      KeyOther code   -> "[" ++ show code ++ "]"
      KeyBack         -> "Backspace"
      KeyTab          -> "Tab"
      KeyReturn       -> "Enter"
      KeyEscape       -> "Esc"
      KeySpace        -> "Space"
      KeyDelete       -> "Delete"
      KeyInsert       -> "Insert"
      KeyEnd          -> "End"
      KeyHome         -> "Home"
      KeyLeft         -> "Left"
      KeyUp           -> "Up"
      KeyRight        -> "Right"
      KeyDown         -> "Down"
      KeyPageUp       -> "PgUp"
      KeyPageDown     -> "PgDn"
      KeyStart        -> "Start"
      KeyClear        -> "Clear"
      KeyShift        -> "Shift"
      KeyAlt          -> "Alt"
      KeyControl      -> "Ctrl"
      KeyMenu         -> "Menu"
      KeyPause        -> "Pause"
      KeyCapital      -> "Capital"
      KeyHelp         -> "Help"
      KeySelect       -> "Select"
      KeyPrint        -> "Print"
      KeyExecute      -> "Execute"
      KeySnapshot     -> "Snapshot"
      KeyCancel       -> "Cancel"
      KeyLeftButton   -> "Left Button"
      KeyRightButton  -> "Right Button"
      KeyMiddleButton -> "Middle Button"
      KeyNum0         -> "Num 0"
      KeyNum1         -> "Num 1"
      KeyNum2         -> "Num 2"
      KeyNum3         -> "Num 3"
      KeyNum4         -> "Num 4"
      KeyNum5         -> "Num 5"
      KeyNum6         -> "Num 6"
      KeyNum7         -> "Num 7"
      KeyNum8         -> "Num 8"
      KeyNum9         -> "Num 9"
      KeyMultiply     -> "Num *"
      KeyAdd          -> "Num +"
      KeySeparator    -> "Num Separator"
      KeySubtract     -> "Num -"
      KeyDecimal      -> "Num ."
      KeyDivide       -> "Num /"
      KeyF1           -> "F1"
      KeyF2           -> "F2"
      KeyF3           -> "F3"
      KeyF4           -> "F4"
      KeyF5           -> "F5"
      KeyF6           -> "F6"
      KeyF7           -> "F7"
      KeyF8           -> "F8"
      KeyF9           -> "F9"
      KeyF10          -> "F10"
      KeyF11          -> "F11"
      KeyF12          -> "F12"
      KeyF13          -> "F13"
      KeyF14          -> "F14"
      KeyF15          -> "F15"
      KeyF16          -> "F16"
      KeyF17          -> "F17"
      KeyF18          -> "F18"
      KeyF19          -> "F19"
      KeyF20          -> "F20"
      KeyF21          -> "F21"
      KeyF22          -> "F22"
      KeyF23          -> "F23"
      KeyF24          -> "F24"
      KeyNumLock      -> "Numlock"
      KeyScroll       -> "Scroll"

{-----------------------------------------------------------------------------------------
  Drag and Drop events
-----------------------------------------------------------------------------------------}
-- | Drag results
data DragResult
    = DragError
    | DragNone
    | DragCopy
    | DragMove
    | DragLink
    | DragCancel
    | DragUnknown
   deriving (Eq,Show)

dragResults :: [(Int, DragResult)]
dragResults
    = [(wxDRAG_ERROR   ,DragError)
      ,(wxDRAG_NONE    ,DragNone)
      ,(wxDRAG_COPY    ,DragCopy)
      ,(wxDRAG_MOVE    ,DragMove)
      ,(wxDRAG_LINK    ,DragLink)
      ,(wxDRAG_CANCEL  ,DragCancel)]

fromDragResult :: DragResult -> Int
fromDragResult drag
  = case drag of
      DragError   -> wxDRAG_ERROR
      DragNone    -> wxDRAG_NONE
      DragCopy    -> wxDRAG_COPY
      DragMove    -> wxDRAG_MOVE
      DragLink    -> wxDRAG_LINK
      DragCancel  -> wxDRAG_CANCEL
      DragUnknown -> wxDRAG_ERROR

toDragResult :: Int -> DragResult
toDragResult drag
 = case lookup drag dragResults of
      Just x -> x
      Nothing -> DragError

-- | Set an event handler that is called when the drop target can be filled with data.
-- This function require to use 'dropTargetGetData' in your event handler to fill data.
dropTargetOnData :: DropTarget a -> (Point -> DragResult -> IO DragResult) -> IO ()
dropTargetOnData drop' event = do
    funPtr <- dragThreeFuncHandler event
    wxcDropTargetSetOnData (objectCast drop') (toCFunPtr funPtr)

-- | Set an event handler for an drop' command in a drop' target.
dropTargetOnDrop :: DropTarget a -> (Point -> IO Bool) -> IO ()
dropTargetOnDrop drop' event = do
    funPtr <- dragTwoFuncHandler event
    wxcDropTargetSetOnDrop (objectCast drop') (toCFunPtr funPtr)

-- | Set an event handler for an enter command in a drop' target.
dropTargetOnEnter :: DropTarget a -> (Point -> DragResult -> IO DragResult) -> IO ()
dropTargetOnEnter drop' event = do
    funPtr <- dragThreeFuncHandler event
    wxcDropTargetSetOnEnter (objectCast drop') (toCFunPtr funPtr)

-- | Set an event handler for a drag over command in a drop' target.
dropTargetOnDragOver :: DropTarget a -> (Point -> DragResult -> IO DragResult) -> IO ()
dropTargetOnDragOver drop' event = do
    funPtr <- dragThreeFuncHandler event
    wxcDropTargetSetOnDragOver (objectCast drop') (toCFunPtr funPtr)

-- | Set an event handler for a leave command in a drop' target.
dropTargetOnLeave :: DropTarget a -> (IO ()) -> IO ()
dropTargetOnLeave drop' event = do
    funPtr <- dragZeroFuncHandler event
    wxcDropTargetSetOnLeave (objectCast drop') (toCFunPtr funPtr)

dragZeroFuncHandler :: IO () -> IO (FunPtr (Ptr obj -> IO ()))
dragZeroFuncHandler event =
    dragZeroFunc $ \_obj -> do
    event

dragTwoFuncHandler :: Num a 
                   => (Point2 a -> IO Bool)
                   -> IO (FunPtr (Ptr obj -> CInt -> CInt -> IO CInt))
dragTwoFuncHandler event =
    dragTwoFunc $ \_obj x y -> do
    result   <- event (point (fromIntegral x) (fromIntegral y))
    return $ fromBool result

dragThreeFuncHandler :: Num a 
                     => (Point2 a 
                     -> DragResult 
                     -> IO DragResult)
                     -> IO (FunPtr (Ptr obj -> CInt -> CInt -> CInt -> IO CInt))
dragThreeFuncHandler event =
    dragThreeFunc $ \_obj x y pre -> do
    result <- event (point (fromIntegral x) (fromIntegral y)) (toDragResult $ fromIntegral pre)
    return $ fromIntegral $ fromDragResult result

-- | Set an event handler for a drag & drop command between drag source window and drop
-- target. You must set 'dropTarget' before use this action.
-- And If you use 'fileDropTarget' or 'textDropTarget', you need not use this.
dragAndDrop :: DropSource a -> DragMode -> (DragResult -> IO ()) -> IO ()
dragAndDrop drSrc flag event = do
    result <- dropSourceDoDragDrop drSrc (fromDragMode flag)
    case lookup result dragResults of
      Just x -> event x
      Nothing -> return ()

-- | Set an event handler that is called when text is dropped in target window.
textDropTarget :: Window a -> TextDataObject b -> (Point -> String -> IO ()) -> IO ()
textDropTarget window textData event = do
    funPtr <- dropTextHandler event
    textDrop <- wxcTextDropTargetCreate nullPtr (toCFunPtr funPtr)
    dropTargetSetDataObject textDrop textData
    windowSetDropTarget window textDrop

dropTextHandler :: Num a
                =>(Point2 a -> String -> IO ())
                -> IO (FunPtr
                        (Ptr obj -> CInt -> CInt -> Ptr CWchar -> IO ())
                      )
dropTextHandler event =
    wrapTextDropHandler $ \_obj x y cstr -> do
    str <- peekCWString cstr
    event (point (fromIntegral x) (fromIntegral y)) str

-- | Set an event handler that is called when files are dropped in target window.
fileDropTarget :: Window a -> (Point -> [String] -> IO ()) -> IO ()
fileDropTarget window event = do
    funPtr <- dropFileHandler event
    fileDrop <- wxcFileDropTargetCreate nullPtr (toCFunPtr funPtr)
    windowSetDropTarget window fileDrop

dropFileHandler :: Num a
                => (Point2 a -> [String] -> IO ())
                -> IO (FunPtr
                        (Ptr obj -> CInt -> CInt -> Ptr (Ptr CWchar) -> CInt -> IO ())
                      )
dropFileHandler event =
    wrapFileDropHandler $ \_obj x y carr size -> do
    arr <- peekArray (fromIntegral size) carr
    files <- mapM peekCWString arr
    event (point (fromIntegral x) (fromIntegral y)) files

data DragMode = CopyOnly | AllowMove | Default
              deriving (Eq,Show)
              -- deriving (Eq,Show,Read,Typeable)

fromDragMode :: DragMode -> Int
fromDragMode mode
  = case mode of
      CopyOnly  -> wxDRAG_COPYONLY
      AllowMove -> wxDRAG_ALLOWMOVE
      Default   -> wxDRAG_DEFALUTMOVE

foreign import ccall "wrapper" dragZeroFunc :: (Ptr obj -> IO ()) -> IO (FunPtr (Ptr obj -> IO ()))
foreign import ccall "wrapper" dragTwoFunc :: (Ptr obj -> CInt -> CInt -> IO CInt) -> IO (FunPtr (Ptr obj -> CInt -> CInt -> IO CInt))
foreign import ccall "wrapper" dragThreeFunc :: (Ptr obj -> CInt -> CInt -> CInt -> IO CInt) -> IO (FunPtr (Ptr obj -> CInt -> CInt -> CInt -> IO CInt))
foreign import ccall "wrapper" wrapTextDropHandler :: (Ptr obj -> CInt -> CInt -> Ptr CWchar -> IO ()) -> IO (FunPtr (Ptr obj -> CInt -> CInt -> Ptr CWchar -> IO ()))
foreign import ccall "wrapper" wrapFileDropHandler :: (Ptr obj -> CInt -> CInt -> Ptr (Ptr CWchar) -> CInt -> IO ()) -> IO (FunPtr (Ptr obj -> CInt -> CInt -> Ptr (Ptr CWchar) -> CInt -> IO ()))

{-----------------------------------------------------------------------------------------
  Grid events
-----------------------------------------------------------------------------------------}
type Column     = Int
type Row        = Int

-- | Grid events. 
data EventGrid  = GridCellMouse        !Row !Column !EventMouse
                | GridLabelMouse       !Row !Column !EventMouse
                | GridCellChange       !Row !Column !(IO ())
                | GridCellSelect       !Row !Column !(IO ())
                | GridCellDeSelect     !Row !Column !(IO ())
                | GridEditorHidden     !Row !Column !(IO ())
                | GridEditorShown      !Row !Column !(IO ())
                | GridEditorCreated    !Row !Column (IO (Control ())) 
                | GridColSize          !Column !Point !Modifiers (IO ())
                | GridRowSize          !Row !Point !Modifiers (IO ())
                | GridRangeSelect      !Row !Column !Row !Column !Rect !Modifiers !(IO ())
                | GridRangeDeSelect    !Row !Column !Row !Column !Rect !Modifiers !(IO ())
                | GridUnknown          !Row !Column !Int

fromGridEvent :: GridEvent a -> IO EventGrid
fromGridEvent gridEvent
  = do tp  <- eventGetEventType gridEvent
       row <- gridEventGetRow gridEvent
       col <- gridEventGetCol gridEvent
       case lookup tp gridEvents of
         Just make  -> make gridEvent row col 
         Nothing    -> return (GridUnknown row col tp)

gridEvents :: [(Int, GridEvent a -> Int -> Int -> IO EventGrid)]
gridEvents
  = [(wxEVT_GRID_CELL_LEFT_CLICK,    gridMouse GridCellMouse MouseLeftDown)
    ,(wxEVT_GRID_CELL_LEFT_DCLICK,   gridMouse GridCellMouse MouseLeftDClick)
    ,(wxEVT_GRID_CELL_RIGHT_CLICK,   gridMouse GridCellMouse MouseRightDown)
    ,(wxEVT_GRID_CELL_RIGHT_DCLICK,  gridMouse GridCellMouse MouseRightDClick)
    ,(wxEVT_GRID_LABEL_LEFT_CLICK,   gridMouse GridLabelMouse MouseLeftDown)
    ,(wxEVT_GRID_LABEL_LEFT_DCLICK,  gridMouse GridLabelMouse MouseLeftDClick)
    ,(wxEVT_GRID_LABEL_RIGHT_CLICK,  gridMouse GridLabelMouse MouseRightDown)
    ,(wxEVT_GRID_LABEL_RIGHT_DCLICK, gridMouse GridLabelMouse MouseRightDClick)
    ,(wxEVT_GRID_SELECT_CELL,        gridSelect)
    ,(wxEVT_GRID_EDITOR_SHOWN,       gridVeto GridEditorShown)
    ,(wxEVT_GRID_EDITOR_HIDDEN,      gridVeto GridEditorHidden)
    ]
  where
    gridMouse make makeMouse gridEvent row col
      = do pt'          <- gridEventGetPosition gridEvent
           altDown'     <- gridEventAltDown gridEvent
           controlDown' <- gridEventControlDown gridEvent
           shiftDown'   <- gridEventShiftDown gridEvent
           metaDown'    <- gridEventMetaDown gridEvent
           let modifiers = Modifiers altDown' shiftDown' controlDown' metaDown'
           return (make row col (makeMouse pt' modifiers))

    gridVeto make gridEvent row col
      = return (make row col (notifyEventVeto gridEvent))

    gridSelect gridEvent row col
      = do selecting <- gridEventSelecting gridEvent
           if selecting
            then return (GridCellSelect row col (notifyEventVeto gridEvent))
            else return (GridCellDeSelect row col (notifyEventVeto gridEvent))




-- | Set a grid event handler.
gridOnGridEvent :: Grid a -> (EventGrid -> IO ()) -> IO ()
gridOnGridEvent grid eventHandler
  = windowOnEvent grid (map fst gridEvents) eventHandler gridHandler
  where
    gridHandler event
      = do eventGrid <- fromGridEvent (objectCast event)
           eventHandler eventGrid

-- | Get the current grid event handler of a window.
gridGetOnGridEvent :: Grid a -> IO (EventGrid -> IO ())
gridGetOnGridEvent grid
  = unsafeWindowGetHandlerState grid wxEVT_GRID_CELL_CHANGED (\_event -> skipCurrentEvent)


{-----------------------------------------------------------------------------------------
  TreeCtrl events
-----------------------------------------------------------------------------------------}
-- | Tree control events
data EventTree  = TreeBeginRDrag      TreeItem  !Point  (IO ()) -- ^ Drag with right button. Call @IO@ action to continue dragging.
                | TreeBeginDrag       TreeItem  !Point  (IO ())
                | TreeEndDrag         TreeItem  !Point
                | TreeBeginLabelEdit  TreeItem  String  (IO ())     -- ^ Edit a label. Call @IO@ argument to disallow the edit.
                | TreeEndLabelEdit    TreeItem  String Bool  (IO ()) -- ^ End edit. @Bool@ is 'True' when the edit was cancelled. Call the @IO@ argument to veto the action.
                | TreeDeleteItem      TreeItem  
                | TreeItemActivated   TreeItem  
                | TreeItemCollapsed   TreeItem  
                | TreeItemCollapsing  TreeItem  (IO ())          -- ^ Call the @IO@ argument to veto.       
                | TreeItemExpanding   TreeItem  (IO ())          -- ^ Call the @IO@ argument to veto.
                | TreeItemExpanded    TreeItem  
                | TreeItemRightClick  TreeItem  
                | TreeItemMiddleClick TreeItem  
                | TreeSelChanged      TreeItem  TreeItem  
                | TreeSelChanging     TreeItem  TreeItem  (IO ()) -- ^ Call the @IO@ argument to veto.
                | TreeKeyDown         TreeItem  EventKey
                | TreeUnknown

fromTreeEvent :: TreeEvent a -> IO EventTree
fromTreeEvent treeEvent
  = do tp   <- eventGetEventType treeEvent
       item <- treeEventGetItem treeEvent
       case lookup tp treeEvents of
         Just make   -> make treeEvent item
         Nothing     -> return TreeUnknown
        
treeEvents :: [(Int,TreeEvent a -> TreeItem -> IO EventTree)]
treeEvents 
  = [(wxEVT_COMMAND_TREE_DELETE_ITEM,       fromItemEvent TreeDeleteItem)
    ,(wxEVT_COMMAND_TREE_ITEM_ACTIVATED,    fromItemEvent TreeItemActivated)
    ,(wxEVT_COMMAND_TREE_ITEM_COLLAPSED,    fromItemEvent TreeItemCollapsed)
    ,(wxEVT_COMMAND_TREE_ITEM_EXPANDED,     fromItemEvent TreeItemExpanded)
    ,(wxEVT_COMMAND_TREE_ITEM_RIGHT_CLICK,  fromItemEvent TreeItemRightClick)
    ,(wxEVT_COMMAND_TREE_ITEM_MIDDLE_CLICK, fromItemEvent TreeItemMiddleClick)
    ,(wxEVT_COMMAND_TREE_ITEM_COLLAPSING,   withVeto (fromItemEvent TreeItemCollapsing))
    ,(wxEVT_COMMAND_TREE_ITEM_EXPANDING,    withVeto (fromItemEvent TreeItemExpanding))
    ,(wxEVT_COMMAND_TREE_KEY_DOWN,          fromKeyDownEvent )
    ,(wxEVT_COMMAND_TREE_BEGIN_LABEL_EDIT,  fromBeginLabelEditEvent )
    ,(wxEVT_COMMAND_TREE_END_LABEL_EDIT,    fromEndLabelEditEvent )
    ,(wxEVT_COMMAND_TREE_BEGIN_DRAG,        withAllow (fromDragEvent TreeBeginDrag))
    ,(wxEVT_COMMAND_TREE_BEGIN_RDRAG,       withAllow (fromDragEvent TreeBeginRDrag))
    ,(wxEVT_COMMAND_TREE_END_DRAG,          fromDragEvent TreeEndDrag)
    ,(wxEVT_COMMAND_TREE_SEL_CHANGED,       fromChangeEvent TreeSelChanged)
    ,(wxEVT_COMMAND_TREE_SEL_CHANGING,      withVeto (fromChangeEvent TreeSelChanging))
    ]
  where
    fromKeyDownEvent treeEvent item
      = do keyEvent <- treeEventGetKeyEvent treeEvent
           eventKey <- eventKeyFromEvent keyEvent
           return (TreeKeyDown item eventKey)

    fromBeginLabelEditEvent treeEvent item
      = do lab <- treeEventGetLabel treeEvent
           return (TreeBeginLabelEdit item lab (notifyEventVeto treeEvent))
                  
    fromEndLabelEditEvent treeEvent item
      = do lab <- treeEventGetLabel treeEvent
           can <- treeEventIsEditCancelled treeEvent
           return (TreeEndLabelEdit item lab can (notifyEventVeto treeEvent))

    fromDragEvent make treeEvent item
      = do pt' <- treeEventGetPoint treeEvent
           return (make item pt')

    fromChangeEvent make treeEvent item
      = do olditem <- treeEventGetOldItem treeEvent
           return (make item olditem)
    
    withAllow make treeEvent item
      = do f <- make treeEvent item
           return (f (treeEventAllow treeEvent))

    withVeto make treeEvent item
      = do f <- make treeEvent item
           return (f (notifyEventVeto treeEvent))

    fromItemEvent make _treeEvent item
      = return (make item)



-- | Set a tree event handler.
treeCtrlOnTreeEvent :: TreeCtrl a -> (EventTree -> IO ()) -> IO ()
treeCtrlOnTreeEvent treeCtrl eventHandler
  = windowOnEvent treeCtrl (map fst treeEvents) eventHandler treeHandler
  where
    treeHandler event
      = do eventTree <- fromTreeEvent (objectCast event)
           eventHandler eventTree

-- | Get the current tree event handler of a window.
treeCtrlGetOnTreeEvent :: TreeCtrl a -> IO (EventTree -> IO ())
treeCtrlGetOnTreeEvent treeCtrl
  = unsafeWindowGetHandlerState treeCtrl wxEVT_COMMAND_TREE_ITEM_ACTIVATED (\_event -> skipCurrentEvent)


{-----------------------------------------------------------------------------------------
  ListCtrl events
-----------------------------------------------------------------------------------------}
-- | Type synonym for documentation purposes.
type ListIndex  = Int

-- | List control events.
data EventList  = ListBeginDrag       !ListIndex !Point (IO ()) -- ^ Drag with left mouse button. Call @IO@ argument to veto this action.
                | ListBeginRDrag      !ListIndex !Point (IO ()) -- ^ Drag with right mouse button. @IO@ argument to veto this action.
                | ListBeginLabelEdit  !ListIndex (IO ())        -- ^ Edit label. Call @IO@ argument to veto this action.
                | ListEndLabelEdit    !ListIndex !Bool (IO ())  -- ^ End editing label. @Bool@ argument is 'True' when cancelled. Call @IO@ argument to veto this action.
                | ListDeleteItem      !ListIndex
                | ListDeleteAllItems
                | ListItemSelected    !ListIndex 
                | ListItemDeselected  !ListIndex 
                | ListItemActivated   !ListIndex        -- ^ Activate (ENTER or double click)  
                | ListItemFocused     !ListIndex 
                | ListItemMiddleClick !ListIndex 
                | ListItemRightClick  !ListIndex   
                | ListInsertItem      !ListIndex   
                | ListColClick        !Int              -- ^ Column has been clicked. (-1 when clicked in control header outside any column)
                | ListColRightClick   !Int                
                | ListColBeginDrag    !Int (IO ())      -- ^ Column is dragged. Index is of the column left of the divider that is being dragged. Call @IO@ argument to veto this action.
                | ListColDragging     !Int
                | ListColEndDrag      !Int (IO ())      -- ^ Column has been dragged. Call @IO@ argument to veto this action.
                | ListKeyDown         !Key              
                | ListCacheHint       !Int !Int         -- ^ (Inclusive) range of list items that are advised to be cached.
                | ListUnknown


fromListEvent :: ListEvent a -> IO EventList
fromListEvent listEvent
  = do tp <- eventGetEventType listEvent
       case lookup tp listEvents of
         Just f  -> f listEvent 
         Nothing -> return ListUnknown

listEvents :: [(Int, ListEvent a -> IO EventList)]
listEvents
  = [(wxEVT_COMMAND_LIST_BEGIN_LABEL_EDIT,  withVeto $ withItem ListBeginLabelEdit)
    ,(wxEVT_COMMAND_LIST_DELETE_ITEM,       withItem ListDeleteItem)
    ,(wxEVT_COMMAND_LIST_INSERT_ITEM,       withItem ListInsertItem)
    ,(wxEVT_COMMAND_LIST_ITEM_ACTIVATED,    withItem ListItemActivated)
    ,(wxEVT_COMMAND_LIST_ITEM_DESELECTED,   withItem ListItemDeselected)
    ,(wxEVT_COMMAND_LIST_ITEM_FOCUSED,      withItem ListItemFocused)
    ,(wxEVT_COMMAND_LIST_ITEM_MIDDLE_CLICK ,withItem ListItemMiddleClick)
    ,(wxEVT_COMMAND_LIST_ITEM_RIGHT_CLICK,  withItem ListItemRightClick)
    ,(wxEVT_COMMAND_LIST_ITEM_SELECTED,     withItem ListItemSelected)
    ,(wxEVT_COMMAND_LIST_END_LABEL_EDIT,    withVeto $ withCancel $ withItem ListEndLabelEdit )
    ,(wxEVT_COMMAND_LIST_BEGIN_RDRAG,       withVeto $ withPoint $ withItem ListBeginRDrag)
    ,(wxEVT_COMMAND_LIST_BEGIN_DRAG,        withVeto $ withPoint $ withItem ListBeginDrag)
    ,(wxEVT_COMMAND_LIST_COL_CLICK,         withColumn ListColClick)
    ,(wxEVT_COMMAND_LIST_COL_BEGIN_DRAG,    withVeto $ withColumn ListColBeginDrag)
    ,(wxEVT_COMMAND_LIST_COL_DRAGGING,      withColumn ListColDragging)
    ,(wxEVT_COMMAND_LIST_COL_END_DRAG,      withVeto $ withColumn ListColEndDrag)
    ,(wxEVT_COMMAND_LIST_COL_RIGHT_CLICK,   withColumn ListColRightClick)
    ,(wxEVT_COMMAND_LIST_CACHE_HINT,        withCache  ListCacheHint )
    ,(wxEVT_COMMAND_LIST_KEY_DOWN,          withKeyCode ListKeyDown )
    ,(wxEVT_COMMAND_LIST_DELETE_ALL_ITEMS,  \_event -> return ListDeleteAllItems )
    ]
  where
    withPoint make listEvent
      = do f   <- make listEvent
           pt'  <- listEventGetPoint listEvent
           return (f pt')

    withCancel make listEvent
      = do f   <- make listEvent
           can <- listEventCancelled listEvent
           return (f can)

    withVeto :: (ListEvent a -> IO (IO () -> EventList)) -> ListEvent a -> IO EventList
    withVeto make listEvent
      = do f <- make listEvent
           return (f (notifyEventVeto listEvent))

    withKeyCode make listEvent
      = do code <- listEventGetCode listEvent
           return (make (keyCodeToKey code))

    withCache make listEvent
      = do lo <- listEventGetCacheFrom listEvent
           hi <- listEventGetCacheTo listEvent
           return (make lo hi)
                
    withColumn make listEvent
      = do col <- listEventGetColumn listEvent
           return (make col)

    withItem :: (ListIndex -> b) -> ListEvent a -> IO b
    withItem make listEvent
      = do item <- listEventGetIndex listEvent
           return (make item)

         

-- | Set a list event handler.
listCtrlOnListEvent :: ListCtrl a -> (EventList -> IO ()) -> IO ()
listCtrlOnListEvent listCtrl eventHandler
  = windowOnEvent listCtrl (map fst listEvents) eventHandler listHandler
  where
    listHandler event
      = do eventList <- fromListEvent (objectCast event)
           eventHandler eventList

-- | Get the current list event handler of a window.
listCtrlGetOnListEvent :: ListCtrl a -> IO (EventList -> IO ())
listCtrlGetOnListEvent listCtrl
  = unsafeWindowGetHandlerState listCtrl wxEVT_COMMAND_LIST_ITEM_ACTIVATED (\_event -> skipCurrentEvent)


------------------------------------------------------------------------------------------
-- TaskBarIcon Events
------------------------------------------------------------------------------------------
data EventTaskBarIcon = TaskBarIconMove
                      | TaskBarIconLeftDown
                      | TaskBarIconLeftUp
                      | TaskBarIconRightDown
                      | TaskBarIconRightUp
                      | TaskBarIconLeftDClick
                      | TaskBarIconRightDClick
                      | TaskBarIconUnknown
                      deriving (Show, Eq)

fromTaskBarIconEvent :: Event a -> IO EventTaskBarIcon
fromTaskBarIconEvent event
  = do tp     <- eventGetEventType event
       case lookup tp taskBarIconEvents of
         Just evt  -> return evt
         Nothing   -> return TaskBarIconUnknown

taskBarIconEvents :: [(Int,EventTaskBarIcon)]
taskBarIconEvents
  = [(wxEVT_TASKBAR_MOVE,         TaskBarIconMove)
    ,(wxEVT_TASKBAR_LEFT_DOWN,    TaskBarIconLeftDown)
    ,(wxEVT_TASKBAR_LEFT_UP,      TaskBarIconLeftUp)
    ,(wxEVT_TASKBAR_RIGHT_DOWN,   TaskBarIconRightDown)
    ,(wxEVT_TASKBAR_RIGHT_UP,     TaskBarIconRightUp)
    ,(wxEVT_TASKBAR_LEFT_DCLICK,  TaskBarIconLeftDClick)
    ,(wxEVT_TASKBAR_RIGHT_DCLICK, TaskBarIconRightDClick)
    ]

-- | Set a taskbar icon event handler.
evtHandlerOnTaskBarIconEvent :: TaskBarIcon a -> (EventTaskBarIcon -> IO ()) -> IO ()
evtHandlerOnTaskBarIconEvent taskbar eventHandler
  = evtHandlerOnEvent taskbar idAny idAny (map fst taskBarIconEvents) eventHandler
       -- finalize taskBarIcon's resource on Windows.
       (\_ -> if wxToolkit == WxMSW
              then (taskBarIconRemoveIcon taskbar
                   -- But taskBarIconDelete doesn't work well in this part. I don't know why.
                   -- >> taskBarIconDelete taskbar
                   >> return ())
              else (return ()))
       scrollHandler
  where
    scrollHandler event
      = do eventTaskBar <- fromTaskBarIconEvent event
           eventHandler eventTaskBar

-- | Get the current event handler for a taskbar icon.
evtHandlerGetOnTaskBarIconEvent :: EvtHandler a -> Id -> EventTaskBarIcon -> IO (IO ())
evtHandlerGetOnTaskBarIconEvent window id' evt
  = unsafeGetHandlerState window id'
      (fromMaybe wxEVT_TASKBAR_MOVE
          $ lookup evt $ uncurry (flip zip) . unzip $ taskBarIconEvents)
      skipCurrentEvent

{-----------------------------------------------------------------------------------------
  Wizard events
-----------------------------------------------------------------------------------------}
data Direction = Backward | Forward

data EventWizard
    = WizardPageChanged     Direction
    | WizardPageChanging    Direction Veto
--    | WizardBeforePageChanged Veto   -- Missing from ClassesMZ
    | WizardPageShown
    | WizardCancel          Veto
    | WizardHelp
    | WizardFinished
    | WizardUnknown

fromWizardEvent :: WizardEvent a -> IO EventWizard
fromWizardEvent wizEvent
    = do tp <- eventGetEventType wizEvent
         case lookup tp wizEvents of
           Just f  -> f wizEvent
           Nothing -> return WizardUnknown

wizEvents :: [(Int, WizardEvent a -> IO EventWizard)]
wizEvents
    = [(wxEVT_WIZARD_PAGE_CHANGED     ,withDir (withPage WizardPageChanged))
      ,(wxEVT_WIZARD_PAGE_CHANGING    ,withVeto $ withDir (withPage WizardPageChanging))
--      ,(wxEVT_WIZARD_BEFORE_PAGE_CHANGED, withVeto WizardBeforePageChanged) -- missing from ClassesMZ
      ,(wxEVT_WIZARD_PAGE_SHOWN       ,withPage WizardPageShown)
      ,(wxEVT_WIZARD_CANCEL           ,withVeto (withPage WizardCancel))
      ,(wxEVT_WIZARD_HELP             ,withPage  WizardHelp)
      ,(wxEVT_WIZARD_FINISHED         ,withPage WizardFinished)]
    where -- page getter is missing from ClassesMZ, omitting page for the time being
          withPage :: c -> WizardEvent a -> IO c
          withPage = const . return
          withDir :: (WizardEvent a -> IO (Direction -> c)) -> WizardEvent a -> IO c
          withDir make wizEvent
            = do
              dir <- wizardEventGetDirection wizEvent
              f <- make wizEvent
              return $ f (if dir /= 0 then Forward else Backward)
          withVeto :: (WizardEvent a -> IO (Veto -> c)) -> WizardEvent a -> IO c 
          withVeto make wizEvent
            = do
              f <- make wizEvent
              return $ f (notifyEventVeto wizEvent)

-- | Set a calendar event handler.
wizardOnWizEvent :: Wizard a -> (EventWizard -> IO ()) -> IO ()
wizardOnWizEvent wiz eventHandler
  = windowOnEvent wiz (map fst wizEvents) eventHandler wizHandler
  where
    wizHandler event
      = do eventWizard <- fromWizardEvent (objectCast event)
           eventHandler eventWizard

-- | Get the current calendar event handler of a window.
wizardGetOnWizEvent :: Wizard a -> IO (EventWizard -> IO ())
wizardGetOnWizEvent wiz
  -- not sure about the wxEVT_WIZARD_PAGE_CHANGED
  = unsafeWindowGetHandlerState wiz wxEVT_WIZARD_PAGE_CHANGED (\_event -> skipCurrentEvent)


{-----------------------------------------------------------------------------------------
  PropertyGrid events
-----------------------------------------------------------------------------------------}
-- | PropertyGrid control events.
data EventPropertyGrid  
                = PropertyGridHighlighted (Maybe (PGProperty ()))
                | PropertyGridChanged (PGProperty ())
                | PropertyGridUnknown

fromPropertyGridEvent :: PropertyGridEvent a -> IO EventPropertyGrid
fromPropertyGridEvent propertyGridEvent
  = do tp <- eventGetEventType propertyGridEvent
       case lookup tp propertyGridEvents of
         Just f  -> f propertyGridEvent 
         Nothing -> return PropertyGridUnknown

propertyGridEvents :: [(Int, PropertyGridEvent a -> IO EventPropertyGrid)]
propertyGridEvents
  = [(wxEVT_PG_HIGHLIGHTED, withPGProperty PropertyGridHighlighted),
     (wxEVT_PG_CHANGED, withPGProperty (PropertyGridChanged . fromJust))
    ]
  where
    withPGProperty :: (Maybe((PGProperty ())) -> b) -> PropertyGridEvent a -> IO b
    withPGProperty make propertyGridEvent = do
        hasProp <- propertyGridEventHasProperty propertyGridEvent
        if not hasProp then return (make Nothing) else do
            prop <- propertyGridEventGetProperty propertyGridEvent
            return (make (Just prop))

-- | Set a PropertyGrid event handler.
propertyGridOnPropertyGridEvent :: PropertyGrid a -> (EventPropertyGrid -> IO ()) -> IO ()
propertyGridOnPropertyGridEvent propertyGrid eventHandler
  = windowOnEvent propertyGrid (map fst propertyGridEvents) eventHandler listHandler
  where
    listHandler event
      = do eventPropertyGrid <- fromPropertyGridEvent (objectCast event)
           eventHandler eventPropertyGrid

-- | Get the current PropertyGrid event handler of a window.
propertyGridGetOnPropertyGridEvent :: PropertyGrid a -> IO (EventPropertyGrid -> IO ())
propertyGridGetOnPropertyGridEvent propertyGrid
  -- I'm not sure what expEVT_PG_HIGHLIGHTED needs to be here for, just followed pattern with `listCtrlGetOnListEvent'
  = unsafeWindowGetHandlerState propertyGrid wxEVT_PG_HIGHLIGHTED (\_event -> skipCurrentEvent)

{-----------------------------------------------------------------------------------------
  AuiNotebook events
-----------------------------------------------------------------------------------------}

-- | Represents a page in the AuiNotebook for a

newtype WindowId = WindowId Int deriving (Eq,Show)
data WindowSelection = WindowSelection Int (Maybe PageWindow) deriving (Show, Eq)
data PageWindow = PageWindow { winId :: WindowId, win :: (Window ()) } deriving (Show, Eq)

noWindowSelection :: WindowSelection
noWindowSelection = WindowSelection wxNOT_FOUND Nothing

-- | AuiNotebook events.
data EventAuiNotebook = AuiNotebookAllowDnd { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookBeginDrag  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookBgDclick  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookButton  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookDragDone  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookDragMotion  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookEndDrag  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookPageChanged  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookPageChanging  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookPageClose  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookPageClosed  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookTabMiddleDown  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookTabMiddleUp  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookTabRightDown  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookTabRightUp  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookUnknown
                      | AuiTabCtrlPageChanging  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiTabCtrlUnknown
                      deriving (Show, Eq)


auiTabCtrlEvents :: AuiNotebook a -> [(Int, AuiNotebookEvent b -> IO EventAuiNotebook)]
auiTabCtrlEvents nb
  = [(wxEVT_AUINOTEBOOK_PAGE_CHANGING, auiWithSelection nb AuiTabCtrlPageChanging)]

auiNotebookEvents :: AuiNotebook a -> [(Int, AuiNotebookEvent b -> IO EventAuiNotebook)]
auiNotebookEvents nb
  = [(wxEVT_AUINOTEBOOK_ALLOW_DND, auiWithSelection nb AuiNotebookAllowDnd)
    ,(wxEVT_AUINOTEBOOK_BEGIN_DRAG, auiWithSelection nb AuiNotebookBeginDrag)
    ,(wxEVT_AUINOTEBOOK_BG_DCLICK, auiWithSelection nb AuiNotebookBgDclick)
    ,(wxEVT_AUINOTEBOOK_BUTTON, auiWithSelection nb AuiNotebookButton)
    ,(wxEVT_AUINOTEBOOK_DRAG_DONE,  auiWithSelection nb AuiNotebookDragDone)
    ,(wxEVT_AUINOTEBOOK_DRAG_MOTION, auiWithSelection nb AuiNotebookDragMotion)
    ,(wxEVT_AUINOTEBOOK_END_DRAG, auiWithSelection nb AuiNotebookEndDrag)
    ,(wxEVT_AUINOTEBOOK_PAGE_CHANGED, auiWithSelection nb AuiNotebookPageChanged)
    ,(wxEVT_AUINOTEBOOK_PAGE_CHANGING, auiWithSelection nb AuiNotebookPageChanging)
    ,(wxEVT_AUINOTEBOOK_PAGE_CLOSE, auiWithSelection nb AuiNotebookPageClose)
    ,(wxEVT_AUINOTEBOOK_PAGE_CLOSED, auiWithSelection nb AuiNotebookPageClosed)
    ,(wxEVT_AUINOTEBOOK_TAB_MIDDLE_DOWN,auiWithSelection nb AuiNotebookTabMiddleDown)
    ,(wxEVT_AUINOTEBOOK_TAB_MIDDLE_UP, auiWithSelection nb AuiNotebookTabMiddleUp)
    ,(wxEVT_AUINOTEBOOK_TAB_RIGHT_DOWN, auiWithSelection nb AuiNotebookTabRightDown)
    ,(wxEVT_AUINOTEBOOK_TAB_RIGHT_UP,  auiWithSelection nb AuiNotebookTabRightUp)]
           

auiWithSelection :: AuiNotebook a1
                 -> (WindowSelection -> WindowSelection -> r)
                 -> BookCtrlEvent a
                 -> IO r
auiWithSelection nb' eventAN auiNEvent = do 
    selection    <- bookCtrlEventGetSelection auiNEvent
    oldSelection <- bookCtrlEventGetOldSelection auiNEvent
    eventObj     <- eventGetEventObject auiNEvent
    winSel       <- fromSelId nb' eventObj selection auiNEvent
    winOldSel    <- fromSelId nb' eventObj oldSelection auiNEvent
    return $ eventAN winSel winOldSel
  where 
      fromSelId nb'' _eventObj selId _ev = do
         pageCount <- auiNotebookGetPageCount nb''
         if selId < pageCount && selId /= wxNOT_FOUND then do
             pg <-  auiNotebookGetPage nb'' selId
             id' <-  windowGetId pg
             return $ WindowSelection selId $ Just $ PageWindow (WindowId id')  pg
           else return noWindowSelection

fromAuiNotebookEvent :: Object a -> String -> AuiNotebookEvent q -> IO EventAuiNotebook
fromAuiNotebookEvent eventObj cName anEvent 
    = do eventType <-  eventGetEventType anEvent
         case cName of
              "wxAuiNotebook" -> 
                 lookupEvent eventType (auiNotebookEvents $ objectCast eventObj) AuiNotebookUnknown
              "wxAuiTabCtrl" -> 
               do par <- windowGetParent $ objectCast eventObj
                  let t =  (auiTabCtrlEvents . objectCast) par
                  lookupEvent eventType t AuiTabCtrlUnknown
              _ -> error $ "Graphics.UI.WXCore.Events.fromAuiNotebookEvent: Unexpected cName: " ++ cName

              --lookup an event in the given evtTable, if not found use defaul
      where lookupEvent eventType evtTable defaul = case lookup eventType evtTable of
                                    Just f  -> f anEvent
                                    Nothing -> return defaul

-- | accept an event and return the wxWidgets class name of the event's eventObject
objectClassName :: WxObject a ->  IO String
objectClassName obj = do cInfo <-  objectGetClassInfo obj
                         classInfoGetClassNameEx cInfo

-- | use when you want to handle just wxAuiNotebook
auiNotebookOnAuiNotebookEvent ::  String -> EventId ->  AuiNotebook a -> (EventAuiNotebook -> IO ()) -> IO ()
auiNotebookOnAuiNotebookEvent _s eventId notebook eventHandler
  = windowOnEvent notebook [eventId] handler (const handler)
       where handler = withCurrentEvent (\event -> do
               eventObj <-  eventGetEventObject (objectCast event)
               cName <-   objectClassName eventObj
               case cName of
                      "wxAuiNotebook" -> 
                       do ean <- fromAuiNotebookEvent eventObj cName (objectCast event)
                          eventHandler ean
                      _              -> skipCurrentEvent
               )


-- | use when you want to handle both wxAuiNotebook and wxAuiTabCtrl
auiNotebookOnAuiNotebookEventEx ::  String -> EventId ->  AuiNotebook a -> (EventAuiNotebook -> IO ()) -> IO ()
auiNotebookOnAuiNotebookEventEx _s eventId notebook eventHandler
  = windowOnEvent notebook [eventId] handler (const handler)
       where handler = withCurrentEvent (\event -> do
               eventObj <-  eventGetEventObject (objectCast event)
               cName <-   objectClassName eventObj
               ean <- fromAuiNotebookEvent eventObj cName (objectCast event)
               eventHandler ean
               )

auiNotebookGetOnAuiNotebookEvent ::  EventId -> AuiNotebook a -> IO (EventAuiNotebook -> IO ())
auiNotebookGetOnAuiNotebookEvent eventId notebook
  = unsafeWindowGetHandlerState notebook eventId (const skipCurrentEvent)

------------------------------------------------------------------------------------------
-- TimerEx is handled specially.
------------------------------------------------------------------------------------------
-- | Create a new 'Timer' that is attached to a window. It is automatically deleted when
-- its owner is deleted (using 'windowAddOnDelete'). The owning window will receive
-- timer events ('windowOnTimer'). /Broken!/ (use 'windowTimerCreate'\/'timerOnCommand' instead.)
windowTimerAttach :: Window a -> IO (Timer ())
windowTimerAttach w
  = do t <- timerCreate w idAny
       windowAddOnDelete w (timerDelete t)
       return t


-- | Create a new 'TimerEx' timer. It is automatically deleted when its owner is deleted
-- (using 'windowAddOnDelete'). React to timer events using 'timerOnCommand'.
windowTimerCreate :: Window a -> IO (TimerEx ())
windowTimerCreate w
  = do t <- timerExCreate
       windowAddOnDelete w (timerDelete t)
       return t

-- | Set an event handler that is called on a timer tick. This works for 'TimerEx'
-- objects.
timerOnCommand :: TimerEx a -> IO () -> IO ()
timerOnCommand timer io
  = do closure <- createClosure io (\_ownerDeleted -> return ()) (\_ev -> io)
       timerExConnect timer closure

-- | Get the current timer event handler.
timerGetOnCommand :: TimerEx a -> IO (IO ())
timerGetOnCommand timer
  = do closure <- timerExGetClosure timer
       unsafeClosureGetState closure (return ())

{--------------------------------------------------------------------------
  The global idle timer
  Currently only used by the process code but can potentially be used to
  enable haskell threads to run in idle time
--------------------------------------------------------------------------}
{-# NOINLINE appIdleIntervals #-}
appIdleIntervals :: Var [Int]
appIdleIntervals 
  = unsafePerformIO (varCreate [])

-- | @appRegisterIdle interval handler@ registers a global idle event 
-- handler that is at least called every @interval@ milliseconds (and
-- possible more). Returns a method that can be used to unregister this
-- handler (so that it doesn't take any resources anymore). Multiple
-- calls to this method chains the different idle event handlers.
appRegisterIdle :: Int -> IO (IO ())
appRegisterIdle interval 
  = do _ <- varUpdate appIdleIntervals (interval:)
       appUpdateIdleInterval 
       return (appUnregisterIdle interval)

-- Update the idle interval to the minimal one.
appUpdateIdleInterval :: IO ()
appUpdateIdleInterval
  = do ivals <- varGet appIdleIntervals
       let ival = if null ivals then 0 else minimum ivals   -- zero is off.
       appival <- wxcAppGetIdleInterval 
       if (ival < appival)
        then wxcAppSetIdleInterval ival
        else return ()

-- Unregister an idle handler       
appUnregisterIdle :: Int -> IO ()            
appUnregisterIdle ival
  = do _ <- varUpdate appIdleIntervals (remove ival)
       appUpdateIdleInterval
  where
    remove _ival' []      = []
    remove ival'  (i:is)  | ival' == i  = is
                          | otherwise   = i : remove ival' is


{-----------------------------------------------------------------------------------------
  Calender events
-----------------------------------------------------------------------------------------}
data EventCalendar
    = CalendarDayChanged (DateTime ())
    | CalendarDoubleClicked (DateTime ())
    | CalendarMonthChanged (DateTime ())
    | CalendarSelectionChanged (DateTime ())
    | CalendarWeekdayClicked Int
    | CalendarYearChanged (DateTime ())
    | CalendarUnknown
 
fromCalendarEvent :: CalendarEvent a -> IO EventCalendar
fromCalendarEvent calEvent
    = do tp <- eventGetEventType calEvent
         case lookup tp calEvents of
           Just f  -> f calEvent
           Nothing -> return CalendarUnknown
 
calEvents :: [(Int, CalendarEvent a -> IO EventCalendar)]
calEvents
    = [(wxEVT_CALENDAR_DAY_CHANGED    ,withDate CalendarDayChanged)
      ,(wxEVT_CALENDAR_DOUBLECLICKED  ,withDate CalendarDoubleClicked)
      ,(wxEVT_CALENDAR_MONTH_CHANGED  ,withDate CalendarMonthChanged)
      ,(wxEVT_CALENDAR_SEL_CHANGED    ,withDate CalendarSelectionChanged)
      ,(wxEVT_CALENDAR_WEEKDAY_CLICKED,withWeekday CalendarWeekdayClicked)
      ,(wxEVT_CALENDAR_YEAR_CHANGED   ,withDate CalendarYearChanged)]
    where withDate event calEvent
              = do date <- dateTimeCreate
                   withObjectPtr date $ calendarEventGetDate calEvent
                   return (event date)
          withWeekday event calEvent
              = fmap event $ calendarEventGetWeekDay calEvent
 
-- | Set a calendar event handler.
calendarCtrlOnCalEvent :: CalendarCtrl a -> (EventCalendar -> IO ()) -> IO ()
calendarCtrlOnCalEvent calCtrl eventHandler
  = windowOnEvent calCtrl (map fst calEvents) eventHandler calHandler
  where
    calHandler event
      = do eventCalendar <- fromCalendarEvent (objectCast event)
           eventHandler eventCalendar
 
-- | Get the current calendar event handler of a window.
calendarCtrlGetOnCalEvent :: CalendarCtrl a -> IO (EventCalendar -> IO ())
calendarCtrlGetOnCalEvent calCtrl
  = unsafeWindowGetHandlerState calCtrl wxEVT_CALENDAR_SEL_CHANGED (\_event -> skipCurrentEvent)


------------------------------------------------------------------------------------------
-- Application startup
------------------------------------------------------------------------------------------
-- | Installs an init handler and starts the event loop.
-- Note: the closure is deleted when initialization is complete, and than the Haskell init function
-- is started.
appOnInit :: IO () -> IO ()
appOnInit initHandler
  = do closure  <- createClosure (return () :: IO ()) onDelete (\_ev -> return ())   -- run initHandler on destroy !
       progName <- getProgName
       args     <- getArgs
       argv     <- mapM newCWString (progName:args)
       let argc = length argv
       withArray (argv ++ [nullPtr]) $ \cargv -> wxcAppInitializeC closure argc cargv
       mapM_ free argv
  where
    onDelete _ownerDeleted
      = initHandler
           


------------------------------------------------------------------------------------------
-- Attaching Haskell data to arbitrary objects.
------------------------------------------------------------------------------------------
-- | Use attached Haskell data locally. This makes it type-safe.
objectWithClientData :: WxObject a -> b -> ((b -> IO ()) -> IO b -> IO c) -> IO c
objectWithClientData object initx fun
  = do let setter x = objectSetClientData object (return ()) x
           getter   = do mb <- unsafeObjectGetClientData object
                         case mb of
                           Nothing -> return initx
                           Just x  -> return x
       setter initx
       fun setter getter

-- | Attach Haskell value to an arbitrary object. The 'IO' action is executed
-- when the object is deleted. Note: 'evtHandlerSetClientData' is preferred when possible.
objectSetClientData :: WxObject a -> IO () -> b -> IO ()
objectSetClientData object onDelete x
  = do closure <- createClosure x (const onDelete) (const (return ()))
       objectSetClientClosure object closure
       return ()

-- | Retrieve an attached Haskell value.
unsafeObjectGetClientData :: WxObject a -> IO (Maybe b)
unsafeObjectGetClientData object
  = do closure <- objectGetClientClosure object 
       unsafeClosureGetData closure
                
-- | Use attached Haskell data locally in a type-safe way.
evtHandlerWithClientData :: EvtHandler a -> b -> ((b -> IO ()) -> IO b -> IO c) -> IO c
evtHandlerWithClientData evtHandler initx fun
  = do let setter x = evtHandlerSetClientData evtHandler (return ()) x
           getter   = do mb <- unsafeEvtHandlerGetClientData evtHandler
                         case mb of
                           Nothing -> return initx
                           Just x  -> return x
       setter initx
       fun setter getter

-- | Attach a Haskell value to an object derived from 'EvtHandler'. The 'IO' action
-- executed when the object is deleted.
evtHandlerSetClientData :: EvtHandler a -> IO () -> b -> IO ()
evtHandlerSetClientData evtHandler onDelete x
  = do closure <- createClosure x (const onDelete) (const (return ()))
       evtHandlerSetClientClosure evtHandler closure
       return ()

-- | Retrieve an attached Haskell value, previously attached with 'evtHandlerSetClientData'.
unsafeEvtHandlerGetClientData :: EvtHandler a -> IO (Maybe b)
unsafeEvtHandlerGetClientData evtHandler
  = do closure <- evtHandlerGetClientClosure evtHandler
       unsafeClosureGetData closure



-- | Attach a Haskell value to tree item data. The 'IO' action
-- executed when the object is deleted.
treeCtrlSetItemClientData :: TreeCtrl a -> TreeItem -> IO () -> b -> IO ()
treeCtrlSetItemClientData treeCtrl item onDelete x
  = do closure <- createClosure x (const onDelete) (const (return ()))
       treeCtrlSetItemClientClosure treeCtrl item closure
       return ()

-- | Retrieve an attached Haskell value to a tree item, previously attached with 'treeCtrlSetItemClientData'.
unsafeTreeCtrlGetItemClientData :: TreeCtrl a -> TreeItem  -> IO (Maybe b)
unsafeTreeCtrlGetItemClientData treeCtrl item
  = do closure <- treeCtrlGetItemClientClosure treeCtrl item
       unsafeClosureGetData closure


------------------------------------------------------------------------------------------
-- Generic window connection
------------------------------------------------------------------------------------------
-- | Set a generic event handler on a certain window.
windowOnEvent :: Window a -> [EventId] -> handler -> (Event () -> IO ()) -> IO ()
windowOnEvent window eventIds state eventHandler
  = windowOnEventEx window eventIds state (\_ownerDelete -> return ()) eventHandler

-- | Set a generic event handler on a certain window. Takes also a computation
-- that is run when the event handler is destroyed -- the argument is 'True' if the
-- owner is deleted, and 'False' if the event handler is disconnected for example.
windowOnEventEx :: Window a -> [EventId] -> handler -> (Bool -> IO ()) -> (Event () -> IO ()) -> IO ()
windowOnEventEx window eventIds state destroy eventHandler
  = do let id' = idAny   -- id' <- windowGetId window
       evtHandlerOnEvent window id' id' eventIds state destroy eventHandler

-- | Retrieve the event handler state for a certain event on a window.
unsafeWindowGetHandlerState :: Window a -> EventId -> b -> IO b
unsafeWindowGetHandlerState window eventId def
  = do id' <- windowGetId window
       unsafeGetHandlerState window id' eventId def

------------------------------------------------------------------------------------------
-- The current event
------------------------------------------------------------------------------------------
{-# NOINLINE currentEvent #-}
currentEvent :: MVar (Event ())
currentEvent
  = unsafePerformIO (newMVar objectNull)

-- | Get the current event handler (can be 'objectNull').
getCurrentEvent :: IO (Event ())
getCurrentEvent
  = readMVar currentEvent

-- | Do something with the current event /if/ we are calling from an event handler.
withCurrentEvent :: (Event () -> IO ()) -> IO ()
withCurrentEvent f
  = do ev <- getCurrentEvent
       if (ev /= objectNull)
        then f ev
        else return ()

-- | Pass the event on the next /wxWidgets/ event handler, either on this window or its parent.
-- Always call this method when you do not process the event. /Note:/ The use of
-- 'propagateEvent' is encouraged as it is a much better name than 'skipCurrentEvent'. This
-- function name is just for better compatibility with wxWidgets :-)
skipCurrentEvent :: IO ()
skipCurrentEvent
  = withCurrentEvent (\event -> eventSkip event)

-- | Pass the event on the next /wxWidgets/ event handler, either on this window or its parent.
-- Always call this method when you do not process the event. (This function just call 'skipCurrentEvent').
propagateEvent :: IO ()
propagateEvent
  = skipCurrentEvent


------------------------------------------------------------------------------------------
-- Generic event connection
------------------------------------------------------------------------------------------
-- | Retrieves the state associated with a certain event handler. If
-- no event handler is defined for this kind of event or 'Id', the
-- default value is returned.
unsafeGetHandlerState :: EvtHandler a -> Id -> EventId -> b -> IO b
unsafeGetHandlerState object id' eventId def
  = do closure <- evtHandlerGetClosure object id' eventId
       unsafeClosureGetState closure def

-- | Type synonym to make the type signatures shorter for the documentation :-)
type OnEvent = (Bool -> IO ()) -> (Event () -> IO ()) -> IO ()

-- | Sets a generic event handler, just as 'evtHandlerOnEventConnect' but first
-- disconnects any event handlers for the same kind of events.
evtHandlerOnEvent :: EvtHandler a -> Id -> Id -> [EventId] -> handler -> OnEvent
evtHandlerOnEvent object firstId lastId eventIds state destroy eventHandler
  = do evtHandlerOnEventDisconnect object firstId lastId eventIds
       evtHandlerOnEventConnect object firstId lastId eventIds state destroy eventHandler


-- Hack: using a global variable to determine whether we are disconnecting an event
-- or not. This is used as a parameter to the 'destroy' procedure of an event. This
-- enables us to re-install a 'windowOnDelete' handler for example without executing
-- the deletion code.
{-# NOINLINE disconnecting #-}
disconnecting :: Var Bool
disconnecting
  = unsafePerformIO (varCreate False)

-- | Disconnect a certain event handler.
evtHandlerOnEventDisconnect :: EvtHandler a -> Id -> Id -> [EventId] -> IO ()
evtHandlerOnEventDisconnect object firstId lastId eventIds
  = do prev <- varSwap disconnecting True
       mapM_ disconnectEventId eventIds
       varSet disconnecting prev
  where
    disconnectEventId eventId
      = evtHandlerDisconnect object firstId lastId eventId 0 {- actually: void* -}

-- | Sets a generic event handler on an 'EvtHandler' object. The call
-- (@evtHandlerOnEventConnect firstId lastId eventIds state destroy handler object@) sets an event
-- handler @handler@ on @object@. The eventhandler gets called whenever an event
-- happens that is in the list @eventIds@ on an object with an 'Id' between @firstId@
-- and @lastId@ (use -1 for any object). The @state@ is any kind of Haskell data
-- that is attached to this handler. It can be retrieved via 'unsafeGetHandlerState'.
-- Normally, the @state@ is the event handler itself. This allows the current event
-- handler to be retrieved via calls to 'buttonGetOnCommand' for example. The @destroy@
-- action is called when the event handler is destroyed. Its argument is 'True' when the
-- owner is deleted, and 'False' if the event handler is just disconnected.
evtHandlerOnEventConnect :: EvtHandler a -> Id -> Id -> [EventId] -> state -> OnEvent
evtHandlerOnEventConnect object firstId lastId eventIds state destroy eventHandler
  = do closure <- createClosure state destroy eventHandler
       withObjectPtr closure $ \pclosure ->
        mapM_ (connectEventId pclosure) eventIds
  where
    connectEventId pclosure eventId
      = evtHandlerConnect object firstId lastId eventId pclosure



-- Use a data wrapper for the closure state: seem to circumvent bugs when wrapping
-- things like Int or overloaded stuff.
data Wrap a  = Wrap a


unsafeClosureGetState :: Closure () -> a -> IO a
unsafeClosureGetState closure def
  = do mb <- unsafeClosureGetData closure
       case mb of
         Nothing -> return def
         Just x  -> return x

unsafeClosureGetData :: Closure () -> IO (Maybe a)
unsafeClosureGetData closure
  = if (objectIsNull closure)
     then return Nothing
     else do ptr <- closureGetData closure
             if (ptrIsNull ptr)
              then return Nothing
              else do (Wrap x) <- deRefStablePtr (castPtrToStablePtr ptr)
                      return (Just x)


-- | Create a closure with a certain Haskell state, a function that is called
-- when the closure is destroyed, and a function that is called when an event
-- happens. The destroy function takes a boolean that is 'True' when the parent
-- is deleted (and 'False' when the closure is just disconnected). The event
-- handlers gets the 'Event' as its argument.
createClosure :: state -> (Bool -> IO ()) -> (Event () -> IO ()) -> IO (Closure ())
createClosure st destroy handler
  = do funptr  <- wrapEventHandler eventHandlerWrapper
       stptr   <- newStablePtr (Wrap st)
       closureCreate funptr (castStablePtrToPtr stptr)
  where
    eventHandlerWrapper :: Ptr fun -> Ptr () -> Ptr (TEvent ()) -> IO ()
    eventHandlerWrapper funptr stptr eventptr
      = do let event = objectFromPtr eventptr
           prev <- swapMVar currentEvent event
           if (objectIsNull event)
            then do isDisconnecting <- varGet disconnecting
                    destroy (not isDisconnecting)
                    when (stptr/=ptrNull)
                      (freeStablePtr (castPtrToStablePtr stptr))
                    when (funptr/=ptrNull)
                      (freeHaskellFunPtr (castPtrToFunPtr funptr))
            else handler event
           _ <- swapMVar currentEvent prev
           return ()



foreign import ccall "wrapper" wrapEventHandler :: (Ptr fun -> Ptr st -> Ptr (TEvent ()) -> IO ()) -> IO (FunPtr (Ptr fun -> Ptr st -> Ptr (TEvent ()) -> IO ()))
