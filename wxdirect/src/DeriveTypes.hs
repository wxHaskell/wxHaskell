-----------------------------------------------------------------------------------------
{-| Module      :  DeriveTypes
    Copyright   :  (c) Daan Leijen 2003
    License     :  BSD-style

    Maintainer  :  wxhaskell-devel@lists.sourceforge.net
    Stability   :  provisional
    Portability :  portable

    Module that derives more specific types from a C-header signature.
    Also removes duplicates and ignored definitions.
-}
-----------------------------------------------------------------------------------------
module DeriveTypes ( deriveTypes, deriveTypesAll
                   , Name(..), Method(..), ClassName, MethodName, PropertyName
                   , classifyName
                   ) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Char( toUpper, isUpper )
import Data.List( isPrefixOf )

import Types
import Classes( isClassName )

{-----------------------------------------------------------------------------------------
  The whole type derivation can be tuned with these tables
-----------------------------------------------------------------------------------------}
-- | Map properties names (@GetInvokingWindow@) to object types (@Window@).
objectProperties :: Map.Map String Type
objectProperties
  = Map.fromList $ map (\(nm,objname) -> (nm,Object objname))
    [("InvokingWindow"  , "wxWindow")
    ,("EventHandler"    , "wxEvtHandler")
    ,("NextHandler"     , "wxEvtHandler")
    ,("PreviousHandler" , "wxEvtHandler")
    ,("Parent"          , "wxWindow")     -- a bit weak :-(
    ,("TopWindow"       , "wxWindow")
    ,("ClientObject"    , "wxClientData")
    ,("ToolClientData"  , "wxClientData")
    ,("TextBackground"  , "wxColour")
    ,("TextForeground"  , "wxColour")
    ,("ForegroundColour", "wxColour")
    ,("BackgroundColour", "wxColour")
    ,("CustomColour"    , "wxColour")
    ,("BorderColour"    , "wxColour")
    ,("TextColour"      , "wxColour")
    ,("SystemColour"    , "wxColour")
    ,("Stipple"         , "wxBitmap")
    ,("BitmapLabel"     , "wxBitmap")
    ,("BitmapFocus"     , "wxBitmap")
    ,("BitmapSelected"  , "wxBitmap")
    ,("BitmapDisabled"  , "wxBitmap")
    ,("WeekDayInSameWeek", "wxDateTime")
    ,("NextWeekDay"     , "wxDateTime")
    ,("PrevWeekDay"     , "wxDateTime")
    ,("WeekDay"         , "wxDateTime")
    ,("LastWeekDay"     , "wxDateTime")
    ,("Week"            , "wxDateTime")
    ,("LastMonthDay"    , "wxDateTime")
    ,("SystemFont"      , "wxFont")
    ,("App"             , "wxApp")
    ,("EventObject"     , "wxObject")
    ,("Constraints"     , "wxLayoutConstraints")
    ,("UpdateRegion"    , "wxRegion")
    ,("SubBitmap"       , "wxBitmap")
    ,("Background"      , "wxBrush")
    -- App
    ,("TopWindow"       , "wxWindow")
    ,("LogTarget"       , "wxLog")
    ]

-- | Property names that correspond to a boolean
booleanProperties :: Set.Set String
booleanProperties
  = Set.fromList
    ["EvtHandlerEnabled"
    ,"ToolEnabled"
    ,"Skipped"
    ,"Enabled"
    ,"Checked"
    ]

-- | Methods that have an object result.
objectMethods :: Map.Map String Type
objectMethods
  = Map.fromList $ map (\(nm,objname) -> (nm,Object objname))
    [ ("FindFocus", "wxWindow")
    , ("FindWindow", "wxWindow")
    , ("FindClass", "wxClassInfo")
    -- App
    , ("FindWindowByName", "wxWindow")
    , ("FindWindowByLabel", "wxWindow")
    ]


-- | Methods that have a boolean result.
booleanMethods :: Set.Set String
booleanMethods
  = Set.fromList
    ["Dragging"
    ,"Entering"
    ,"Leaving"
    ,"LeftDClick"
    ,"LeftDown"
    ,"LeftIsDown"
    ,"LeftUp"
    ,"RightDClick"
    ,"RightDown"
    ,"RightIsDown"
    ,"RightUp"
    ,"MiddleDClick"
    ,"MiddleDown"
    ,"MiddleIsDown"
    ,"MiddleUp"
    ,"ButtonDown"
    ,"ButtonIsDown"
    ,"ButtonUp"
    ,"ButtonDClick"
    ,"Moving"

    ,"MetaDown"
    ,"ControlDown"
    ,"AltDown"
    ,"ShiftDown"

    ,"MoreRequested"
    ,"Destroy"
    ,"DestroyChildren"
    ,"Close"
    ,"Disable"
    ,"Hide"
    ,"Show"
    ,"Validate"
    ]

-- | Argument names that correspond to booleans.
booleans :: Set.Set String
booleans
  = Set.fromList
    ["_force","force"
    ,"_enable","enable","enb"
    ,"check"
    ,"modal"
    ,"eraseBackground"
    ,"x_scrolling"
    ,"y_scrolling"
    ,"useMask"
    ,"doIt"
    ,"needMore"         -- idleEventRequestMore
    ,"deleteHandler"    -- popEventHandler
    ,"autoLayout"       -- setAutoLayout
    ,"refresh"          -- setScrollbar
    ]


-- | Argument names that correspond to strings.
strings :: Set.Set String
strings
  = Set.fromList
    [ "_name", "name"
    , "strText"
    , "str"
    , "text"
    , "_txt"
    , "msg", "_msg", "cap", "_cap"
    , "string"
    , "path"
    , "_lbl"              -- HsApp
    , "shelp", "lhelp"    -- Toolbar
    , "section","keyword","viewer"           -- HelpController
    , "file", "rootpath"
    , "_dir", "_fle", "_wcd", "message", "wildCard"      -- FileDialog
    ]


-- | Argument name pairs that correspond to Points.
points :: Set.Set (String,String)
points
  = Set.fromList
    [ ("x","y")
    , ("x1","y1")
    , ("x2","y2")
    , ("xc","yc")
    , ("xoffset","yoffset")
    , ("xpos","ypos")
    , ("x_pos","y_pos")
    , ("x_unit","y_unit")
    , ("_x","_y")
    , ("xsrc","ysrc")
    , ("posx","posy")
    , ("_lft","_top")       -- FileDialog
    ]

-- | Argument name pairs that correspond to Sizes.
sizes :: Set.Set (String,String)
sizes
  = Set.fromList
    [ ("w","h")
    , ("_w","_h")
    , ("width","height")
    , ("_width","_height")
    ]

-- | Argument name pairs that correspond to Vectors.
vectors :: Set.Set (String,String)
vectors
  = Set.fromList
    [ ("dx","dy")
    , ("_dx","_dy")
    ]

-- | Argument name quadruples that correspond to Rectangles.
rectangles :: Set.Set (String,String,String,String)
rectangles
  = Set.fromList
    [ ("x","y","w","h")
    , ("_x","_y","_w","_h")
    , ("x","y","width","height")
    , ("xdest","ydest","width","height")
    , ("_lft","_top","_wdt","_hgt")
    ]

-- | Argument names that correspond to a certain object.
objects :: Map.Map String String
objects
  = Map.fromList
    [ ("submenu",  "wxMenu")
    , ("handler",  "wxEvtHandler")
    , ("dc",       "wxDC")
    , ("_prc",     "wxProcess")
    , ("ctrl",     "wxControl")
    , ("win",      "wxWindow")
    , ("_wnd",     "wxWindow")
    , ("_prt",     "wxWindow")
    , ("parent",   "wxWindow")
    , ("_par",     "wxWindow")
    , ("_prt",     "wxWindow")
    , ("child",    "wxWindow")
    , ("_lst",     "wxList")
    , ("sibling",  "wxWindow")
    , ("otherW",   "wxWindow")
    , ("otherWin", "wxWindow")
    , ("nb",       "wxNotebook")
    , ("cmap",     "wxPalette")
    , ("theFont",  "wxFont")
    , ("stipple",  "wxBitmap")
    , ("_bmp",     "wxBitmap")
    , ("bmp",      "wxBitmap")
    , ("bmp1",     "wxBitmap")
    , ("bmp2",     "wxBitmap")
    , ("t1",       "wxDateTime")
    , ("t2",       "wxDateTime")
    , ("dt",       "wxDateTime")
    , ("col",      "wxColour")
    , ("_itm",     "wxMenuItem")
    , ("cfg",      "wxConfigBase")    -- htmlHelpController
    , ("config",   "wxConfigBase")    -- htmlHelpController
    ]

-- | Argument name  pairs that correspond to a certain (haskell) function pointer.
functions :: Map.Map String String
functions
  = Map.fromList
    [ ("_fun_CEvent", "Ptr fun -> Ptr state -> Event evt -> IO ()")
    ]


referenceObjects :: Map.Map String String
referenceObjects
  = Map.fromList
    [ ("AddTime",      "wxDateTime")
    , ("SubtractTime", "wxDateTime")
    , ("AddDate",      "wxDateTime")
    , ("SubtractDate", "wxDateTime")
    , ("LoadBitmap",   "wxBitmap")
    , ("LoadIcon",     "wxIcon")
    ]

-- | Definitions that should be ignored.
ignore :: [Decl -> Maybe String]
ignore
  = [
  -- gizmos: eljgizmos
     prefix "expEVT_DYNAMIC_SASH"       "gizmos"
    ,prefix "wxRemotelyScrolled"        "gizmos"
    ,prefix "wxTreeCompanionWindow"     "gizmos"
    ,prefix "wxThinSplitterWindow"      "gizmos"
    ,prefix "wxSplitterScrolledWindow"  "gizmos"
    ,prefix "wxMultiCell"               "gizmos"
    ,prefix "wxLED"                     "gizmos"
    ,prefix "wxEditableListBox"         "gizmos"
    ,prefix "wxDynamicSash"             "gizmos"
  -- frame layout: eljfl
    ,equals "expEVT_USER_FIRST"         "frame layout"
    ,prefix "cb"                        "frame layout"
    ,prefix "wxFrameLayout"             "frame layout"
    ,prefix "wxToolLayout"              "frame layout"
    ,prefix "wxToolWindow"              "frame layout"
    ,prefix "wxNewBitmapButton"         "frame layout"
    ,prefix "wxDynamicToolBar"          "frame layout"
    ,prefix "wxDynToolInfo"             "frame layout"
  -- plot window: eljplot
    ,prefix "expEVT_PLOT"               "plot"
    ,prefix "wxPlot"                    "plot"
    ,prefix "ELJPlot"                   "plot"
  -- joystick: eljjoystick
    ,prefix "expEVT_JOY"                "joystick"
    ,prefix "wxJoystick"                "joystick"
  -- command processor: eljcommand
    ,prefix "wxCommandProcessor"        "command proc"
    ,prefix "ELJCommand"                "command proc"
  -- message parameters: eljmime / wrapper.h
    ,prefix "wxMessageParameters"        "message param"
  -- non-portable
    --,equals "expEVT_COMMAND_TOGGLEBUTTON_CLICKED" "toggle button"
    --,prefix "wxToggleButton_"            "toggle button"
    ,prefix "wxDialUpEvent_"             "dialup events"
    ,prefix "wxDialUpManager_"           "dialup manager"
    ,prefix "wxCriticalSection_"         "threads"
    ,prefix "wxMutex_"                   "threads"
    ,prefix "wxCondition_"               "threads"
    ,prefix "wxMutexGui_"                "threads"
  -- misc.
    ,equals "wxDateTime_IsGregorianDate" "gregorian date"
    ,equals "wxIconBundle_Assign"        "icon bundle assign"
    ,prefix "ELJConnection"              "elj connection"
    ,prefix "ELJServer"                  "elj server"
    ,prefix "ELJClient"                  "elj client"
  -- basic types
    ,prefix "wxColour"                   "colour"
    ,prefix "wxPoint"                    "point"
    ,prefix "wxTreeItemId"               "tree item id"
    ,classprefix "wxSize"                "size"
    ,classprefix "wxString"              "string"
    ]
  where
    classprefix s msg decl  | (s == declName decl) = Just msg
                            | isPrefixOf (s++"_") (declName decl) = Just msg
                            | otherwise = Nothing

    prefix s msg  decl  | isPrefixOf s (declName decl) = Just msg
                        | otherwise = Nothing

    equals s msg decl   | (s == declName decl) = Just msg
                        | otherwise = Nothing

{-----------------------------------------------------------------------------------------
Derive types
-----------------------------------------------------------------------------------------}
deriveTypes :: Bool -> [Decl] -> [Decl]
deriveTypes showIgnore decls
  = map (deriveBetterTypes . deriveOutTypes) (removeDupsAndUndefined shouldIgnore showIgnore decls)

deriveTypesAll :: Bool -> [Decl] -> [Decl]
deriveTypesAll showIgnore decls
  = map (deriveBetterTypes . deriveOutTypes) (removeDupsAndUndefined (const Nothing) showIgnore decls)

{-----------------------------------------------------------------------------------------
  Ignore certain decls
-----------------------------------------------------------------------------------------}
shouldIgnore :: Decl -> Maybe String
shouldIgnore decl
  = walk ignore
  where
    walk []       = Nothing
    walk (f:fs)   = case f decl of
                      Nothing  -> walk fs
                      Just msg -> Just msg

{-----------------------------------------------------------------------------------------
   Remove duplicates and undefined stuff
-----------------------------------------------------------------------------------------}
removeDupsAndUndefined :: (Decl -> Maybe String) -> Bool -> [Decl] -> [Decl]
removeDupsAndUndefined shouldIgnore' showIgnore decls
  = filterDupsAndUndefined Set.empty decls
  where
    filterDupsAndUndefined _   [] = []
    filterDupsAndUndefined set (decl:decls')
      = case shouldIgnore' decl of
          Just msg -> (if showIgnore then traceIgnore msg decl else id) $ filterDupsAndUndefined set decls'
          _other   | Set.member (declName decl) set  -> traceIgnore "duplicate" decl $ filterDupsAndUndefined set decls'
                   | otherwise                       -> decl : filterDupsAndUndefined (Set.insert (declName decl) set) decls'

{-----------------------------------------------------------------------------------------
   Derive "Out" types
-----------------------------------------------------------------------------------------}
deriveOutTypes :: Decl -> Decl
deriveOutTypes decl
  = case (declRet decl,reverse (declArgs decl)) of
      -- string
      (StringLen,Arg _name (StringOut ctp) :args)
          -> decl{ declRet = String ctp, declArgs = reverse args }
      -- bytestring
      (ByteStringLen,Arg _name (ByteStringOut ctp) :args)
          -> decl{ declRet = ByteString ctp, declArgs = reverse args }
      -- int array
      (ArrayLen,Arg _name (ArrayIntOut ctp) :args)
          -> decl{ declRet = ArrayInt ctp, declArgs = reverse args }
      -- intptr array
      (ArrayLen,Arg _name (ArrayIntPtrOut ctp) :args)
          -> decl{ declRet = ArrayIntPtr ctp, declArgs = reverse args }
      -- string array
      (ArrayLen,Arg _name (ArrayStringOut ctp) :args)
          -> decl{ declRet = ArrayString ctp, declArgs = reverse args }
      -- object array
      (ArrayLen,Arg _name (ArrayObjectOut cname ctp) :args)
          -> decl{ declRet = ArrayObject cname ctp, declArgs = reverse args }
      -- unknown array
      (ArrayLen,args)
          -> decl{ declRet = Int CInt, declArgs = reverse args }
      -- point
      (Void,Arg _name (PointOut ctp   ):args)
          -> decl{ declRet = Point ctp   , declArgs = reverse args }
      -- size
      (Void,Arg _name (SizeOut ctp   ):args)
          -> decl{ declRet = Size ctp   , declArgs = reverse args }
      -- vector
      (Void,Arg _name (VectorOut ctp   ):args)
          -> decl{ declRet = Vector ctp   , declArgs = reverse args }
      -- rect
      (Void,Arg _name (RectOut ctp   ):args)
          -> decl{ declRet = Rect ctp   , declArgs = reverse args }
      -- rect -- just for treectrl::getBoundingRect and listctrl::GetItemRect
      (Int CInt,Arg _name (RectOut ctp   ):args)
          -> decl{ declRet = Rect ctp   , declArgs = reverse args }
      -- reference
      (Void,Arg _ obj@(RefObject _):args)
          -> decl{ declRet = obj, declArgs = reverse args }
      -- other
      _   -> decl

{-----------------------------------------------------------------------------------------
   Derive extended types
-----------------------------------------------------------------------------------------}
deriveBetterTypes :: Decl -> Decl
deriveBetterTypes decl
  = deriveReturnProperties
  $ deriveThis
  $ deriveExtReturn
  $ deriveExtTypes
  $ deriveStringReturn
  $ deriveSimpleTypes
  $ deriveId
  $ decl


-- Extended types: int x, int y  => Point
deriveExtTypes :: Decl -> Decl
deriveExtTypes decl
  = decl{ declArgs = deriveExtArgs (declArgs decl) }
  where
    -- rectangle
    deriveExtArgs (Arg x (Int ctp): Arg y (Int _): Arg w (Int _): Arg h (Int _): args)
      | Set.member (concat x,concat y,concat w,concat h) rectangles
      = Arg (concat [x,y,w,h]) (Rect ctp): deriveExtArgs args
    -- point
    deriveExtArgs (Arg x (Int ctp): Arg y (Int _): args)
      | Set.member (concat x,concat y) points
      = Arg (x++y) (Point ctp): deriveExtArgs args
    -- vector
    deriveExtArgs (Arg dx (Int ctp): Arg dy (Int _): args)
      | Set.member (concat dx,concat dy) vectors
      = Arg (dx++dy) (Vector ctp): deriveExtArgs args
    -- size
    deriveExtArgs (Arg w (Int ctp): Arg h (Int _): args)
      | Set.member (concat w,concat h) sizes
      = Arg (w++h) (Size ctp): deriveExtArgs args
    -- other
    deriveExtArgs (arg:args)
      = arg:deriveExtArgs args
    deriveExtArgs []
      = []

-- Derive string return: "int fun(..., void* _buf)"
deriveStringReturn :: Decl -> Decl
deriveStringReturn decl
  = case (classifyName (declName decl),declRet decl,reverse (declArgs decl)) of
      -- string
      (_,Int _,Arg ["_buf"] (Ptr Void):args)
          -> decl{ declRet = String CVoid, declArgs = reverse args }
      (_,Int _,Arg ["_buf"] (Ptr Char):args)
          -> decl{ declRet = String CChar, declArgs = reverse args }
      _other
          -> decl

-- Extended return types. Like string: "int fun(..., void* _buf)"
deriveExtReturn :: Decl -> Decl
deriveExtReturn decl
  = case (classifyName (declName decl),declRet decl,reverse (declArgs decl)) of
      -- string
      (_,Int _,Arg ["_buf"] (Ptr Void):args)
          -> decl{ declRet = String CVoid, declArgs = reverse args }
      (_,Int _,Arg ["_buf"] (Ptr Char):args)
          -> decl{ declRet = String CChar, declArgs = reverse args }
      -- point
      (_,Void,Arg y (Ptr (Int ctp)):Arg x (Ptr (Int _)):args)
          | Set.member (concat x,concat y) points
          -> decl{ declRet = Point ctp, declArgs = reverse args }
      (_,Void,Arg y (Ptr Void):Arg x (Ptr Void):args)
          | Set.member (concat x,concat y) points
          -> decl{ declRet = Point CVoid, declArgs = reverse args }
      -- rect
      (_,Void,Arg h (Ptr (Int ctp)):Arg w (Ptr (Int _)):Arg y (Ptr (Int _)):Arg x (Ptr (Int _)):args)
          | Set.member (concat x,concat y,concat w,concat h) rectangles
          -> decl{ declRet = Rect ctp, declArgs = reverse args }
      (_,Void,Arg h (Ptr Void):Arg w (Ptr Void):Arg y (Ptr Void):Arg x (Ptr Void):args)
          | Set.member (concat x,concat y,concat w,concat h) rectangles
          -> decl{ declRet = Rect CVoid, declArgs = reverse args }
      -- size
      (_,Void,Arg y (Ptr (Int ctp)):Arg x (Ptr (Int _)):args)
          | Set.member (concat x,concat y) sizes
          -> decl{ declRet = Size ctp, declArgs = reverse args }
      (_,Void,Arg y (Ptr Void):Arg x (Ptr Void):args)
          | Set.member (concat x,concat y) sizes
          -> decl{ declRet = Size CVoid, declArgs = reverse args }
      -- Vector
      (_,Void,Arg y (Ptr (Int ctp)):Arg x (Ptr (Int _)):args)
          | Set.member (concat x,concat y) vectors
          -> decl{ declRet = Vector ctp, declArgs = reverse args }
      (_,Void,Arg y (Ptr Void):Arg x (Ptr Void):args)
          | Set.member (concat x,concat y) vectors
          -> decl{ declRet = Vector CVoid, declArgs = reverse args }

      -- Null pointer
      (Name name,Ptr Void,[])
        | isPrefixOf "Null_" name && isClassName cname
        -> decl{ declRet = Object cname }
        where
          cname = "wx" ++ drop 5 name

      -- Is/Has
      (Method _cname (Normal mname),Int _ctp,_)
          | (isPrefixOf "Is" mname || isPrefixOf "Has" mname || isPrefixOf "Can" mname
            || mname=="Ok" || isPrefixOf "Contains" mname)
          -> decl{ declRet = Bool }

      (Method cname (Normal mname),Ptr Void,_)
          | isPrefixOf "Create" mname && isClassName createName   -- frameCreateStatusBar
          -> decl{ declRet = Object createName}
          | isPrefixOf "CreateFrom" mname                           -- bitmapCreateFromMetafile
          -> decl{ declRet = Object cname }
          | mname == "CreateLoad" || mname == "CreateDefault"
            || mname == "CreateEmpty" || mname == "CreateSized"      -- bitmapCreateLoad
            || mname == "FromRaw" || mname == "FromXPM"              -- iconFromRaw
          -> decl{ declRet = Object cname }
          where
            createName  = drop (length "Create") mname

      -- Boolean methods
      (Method _cname (Normal mname),Int _ctp,_)
          | Set.member mname booleanMethods
          -> decl{ declRet = Bool }

      -- Object methods
      (Method _cname (Normal mname),Ptr Void,_)
          -> case Map.lookup mname objectMethods of
               Just tp  -> decl{ declRet = tp }
               Nothing  -> decl
      -- other
      _   -> decl


-- returned properties: assumes that deriveThis has already been done
deriveReturnProperties :: Decl -> Decl
deriveReturnProperties decl
  = case (classifyName (declName decl),declRet decl,reverse (declArgs decl)) of
      -- Get via reference
      (Method _cname (Get propname),Void,Arg _ (Ptr Void):args)
          | isClassName ("wx" ++ propname)
          -> -- trace ("ref: " ++ propname ++ ": " ++ declName decl) $
             decl{ declRet = RefObject ("wx" ++ propname), declArgs = reverse args }

      (Method cname (Get _propname),Void,[Arg _ (Object objname),argself@(Arg _ (Object selfname))])
          | selfname == cname
          -> -- trace ("ref: " ++ objname ++ ": " ++ declName decl) $
             decl{ declRet = RefObject objname, declArgs = [argself] }

      -- bit adventurous: deals with things like "bitmapGetSubBitmap"
      (Method _cname (Get propname),Void,(Arg _ (Object _objname):args))
          | Map.member  propname objectProperties
          -> case Map.lookup propname objectProperties of
               Just (Object name) -> -- trace ("ref: " ++ name ++ ": " ++ declName decl) $
                                     decl{ declRet = RefObject name, declArgs = reverse args }
               Just _             -> error "deriveReturnProperties: unexpected objectProperty (1)"
               Nothing            -> traceError ("illegal reference object") decl $ decl

      (Method cname (Get propname),Void,Arg ["_ref"] (Ptr Void):args)
          -> case Map.lookup propname objectProperties of
               Just (Object name) -> -- trace ("ref: " ++ name ++ ": " ++ declName decl) $
                                     decl{ declRet = RefObject name, declArgs = reverse args }
               Just _             -> error "deriveReturnProperties: unexpected objectProperty (2)"
               Nothing  | cname == "wxListEvent" && propname == "Item"
                        -> -- trace ("ref: ListItem: " ++ declName decl) $
                           decl{ declRet = RefObject "wxListItem", declArgs = reverse args }
                        | cname == "wxTreeEvent" && propname == "Item"
                        -> -- trace ("ref: TreeItemId: " ++ declName decl) $
                          decl{ declRet = RefObject "wxTreeItemId", declArgs = reverse args }
                        | cname == "wxTreeEvent" && propname == "OldItem"
                        -> -- trace ("ref: TreeItemId: " ++ declName decl) $
                           decl{ declRet = RefObject "wxTreeItemId", declArgs = reverse args }
                        | otherwise
                        -> traceWarning ("unknown reference object") decl $
                           decl{ declRet = RefObject "Ptr", declArgs = reverse args }

      (Method cname (Normal mname),Void,Arg ["_ref"] (Ptr Void):args)
          -> case Map.lookup mname referenceObjects of
               Just name -> -- trace ("ref: " ++ name ++ ": " ++ declName decl) $
                            decl{ declRet = RefObject name, declArgs = reverse args }
               Nothing   | cname == "wxIconBundle" && mname == "Assign"
                         -> decl{ declRet = RefObject "wxIconBundle", declArgs = reverse args }
                         | otherwise
                         -> traceWarning ("unknown reference object") decl $
                            decl{ declRet = RefObject "Ptr", declArgs = reverse args }

      -- Get
      (Method _cname (Get propname),Ptr Void,_)
          | isClassName ("wx" ++ propname)
          -> decl{ declRet = Object ("wx" ++ propname) }
          | otherwise
          -> case Map.lookup propname objectProperties of
                Just tp -> decl{ declRet = tp }
                Nothing -> decl
      (Method _cname (Get propname),Int _,_)
          | Set.member propname booleanProperties
          -> decl{ declRet = Bool }


      -- Set
      (Method _cname (Set propname),_,Arg argname (Ptr Void):args)
          | isClassName ("wx" ++ propname)
          -> decl{ declArgs = reverse (Arg argname (Object ("wx"++propname)) : args)}
          | otherwise
          -> case Map.lookup propname objectProperties of
               Just tp -> decl{ declArgs = reverse (Arg argname tp : args)}
               Nothing -> decl
      (Method _cname (Set propname),_,Arg argname (Int _):args)
          | Set.member propname booleanProperties
          -> decl{ declArgs = reverse (Arg argname Bool : args)}

      -- other
      _   -> decl



-- Simple types: char* => String
deriveSimpleTypes :: Decl -> Decl
deriveSimpleTypes decl
  = decl{ declArgs = deriveArgs (declArgs decl) }
  where
    deriveArgs args
      = map (\arg -> arg{ argType = deriveArg arg }) args

    deriveArg arg
      = case argType arg of
          Ptr Char  -> String CChar
          Int _ctp  | Set.member (argName arg) booleans  -> Bool
                    | isPrefixOf "is" (argName arg)      -> Bool
          Ptr Void  -> case Map.lookup (argName arg) objects of
                         Just name  -> Object name
                         _ -> case Map.lookup (argName arg) functions of
                                Just tp  -> Fun tp
                                _   | Set.member (argName arg) strings -> String CVoid
                                    | isClassName cargName             -> Object cargName
                                    | otherwise  -> Ptr Void
                                    where
                                      cargName  = case argName arg of
                                                    (c:cs) -> "wx" ++ (toUpper c : cs)
                                                    other  -> other
          tp        -> tp

-- Check for "this" pointer
-- derive for "wxObject_Create :: ... -> IO (Ptr ())"
deriveThis :: Decl -> Decl
deriveThis decl
  = case classifyName (declName decl) of
      Create cname    | declRet decl == Ptr Void         -- bitmapCreate
                      -> decl{ declRet = Object cname }
      Method cname _m | not (null args) && (argType (head args) == Ptr Void) && not (elem cname ["ELJApp"])
                      -> decl{ declArgs = (head args){ argType = Object cname} : tail args }
      _               -> decl
  where
    args = declArgs decl


-- derive event ids: int expEVT_XXX() and expXXX_XXX();
deriveId :: Decl -> Decl
deriveId decl@Decl{ declRet = Int _, declArgs = [] }
  | isPrefixOf "expEVT_" (declName decl)
  = decl{ declRet = EventId }
deriveId decl@Decl{ declRet = Int _, declArgs = [] }
  | isPrefixOf "exp" (declName decl)
  = decl{ declRet = Id }
deriveId decl
  = decl

{-----------------------------------------------------------------------------------------
   Names
-----------------------------------------------------------------------------------------}
data Name = Name   String
          | Create { className :: ClassName }
          | Method { className :: ClassName, method :: Method }
          deriving Show

data Method
          = Normal { methodName :: MethodName }
          | Set    { propName :: PropertyName }
          | Get    { propName :: PropertyName }
          deriving Show

type ClassName = String
type MethodName = String
type PropertyName = String

classifyName :: String -> Name
classifyName s
  = case s of
      ('w' : 'x' : _)     -> getClassName s
      ('c' : 'b' : _)     -> getClassName s
      (c : _) | isUpper c -> getClassName s
      _                   -> Name s
  where
    getClassName name
      = case (span (/='_') name) of
         (cname,method')   | isClassName cname && not (null method')
              -> if (method' == "_Create")
                  then Create cname
                 else if (isPrefixOf "_Get" method')
                  then Method cname (Get (drop 4 method'))
                 else if (isPrefixOf "_Set" method')
                  then Method cname (Set (drop 4 method'))
                  else Method cname (Normal (drop 1 method'))
         (_,_)-> Name s
