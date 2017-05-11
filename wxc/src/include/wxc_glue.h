#ifndef WXC_GLUE_H
#define WXC_GLUE_H

/* $Id: wxc_glue.h,v 1.23 2005/02/25 11:14:58 dleijen Exp $ */

/* wx/version.h must be included for preprocessing by wxdirect */
#include "wx/version.h"

/* Null */
TClass(wxAcceleratorTable) Null_AcceleratorTable(  );
TClass(wxBitmap) Null_Bitmap(  );
TClass(wxBrush) Null_Brush(  );
TClass(wxColour) Null_Colour(  );
TClass(wxCursor) Null_Cursor(  );
TClass(wxFont) Null_Font(  );
TClass(wxIcon) Null_Icon(  );
TClass(wxPalette) Null_Palette(  );
TClass(wxPen) Null_Pen(  );

/* Events */
int expEVT_AUITOOLBAR_TOOL_DROPDOWN();
int expEVT_AUITOOLBAR_OVERFLOW_CLICK();
int expEVT_AUITOOLBAR_RIGHT_CLICK();
int expEVT_AUITOOLBAR_MIDDLE_CLICK();
int expEVT_AUITOOLBAR_BEGIN_DRAG();
int expEVT_AUINOTEBOOK_PAGE_CLOSE();
int expEVT_AUINOTEBOOK_PAGE_CHANGED();
int expEVT_AUINOTEBOOK_PAGE_CHANGING();
int expEVT_AUINOTEBOOK_PAGE_CLOSED();
int expEVT_AUINOTEBOOK_BUTTON();
int expEVT_AUINOTEBOOK_BEGIN_DRAG();
int expEVT_AUINOTEBOOK_END_DRAG();
int expEVT_AUINOTEBOOK_DRAG_MOTION();
int expEVT_AUINOTEBOOK_ALLOW_DND();
int expEVT_AUINOTEBOOK_TAB_MIDDLE_DOWN();
int expEVT_AUINOTEBOOK_TAB_MIDDLE_UP();
int expEVT_AUINOTEBOOK_TAB_RIGHT_DOWN();
int expEVT_AUINOTEBOOK_TAB_RIGHT_UP();
int expEVT_AUINOTEBOOK_DRAG_DONE();
int expEVT_AUINOTEBOOK_BG_DCLICK();
int expEVT_AUI_PANE_BUTTON();
int expEVT_AUI_PANE_CLOSE();
int expEVT_AUI_PANE_MAXIMIZE();
int expEVT_AUI_PANE_RESTORE();
int expEVT_AUI_RENDER();
int expEVT_AUI_FIND_MANAGER();
int expEVT_CALENDAR_SEL_CHANGED();
int expEVT_CALENDAR_PAGE_CHANGED();
int expEVT_CALENDAR_DOUBLECLICKED();
int expEVT_CALENDAR_WEEKDAY_CLICKED();
int expEVT_CALENDAR_WEEK_CLICKED();
int expEVT_CALENDAR_DAY_CHANGED();
int expEVT_CALENDAR_MONTH_CHANGED();
int expEVT_CALENDAR_YEAR_CHANGED();
int expEVT_COMMAND_CHOICEBOOK_PAGE_CHANGED();
int expEVT_COMMAND_CHOICEBOOK_PAGE_CHANGING();
int expEVT_CLIPBOARD_CHANGED();
int expEVT_COMMAND_COLOURPICKER_CHANGED();
int expEVT_COMMAND_COLLPANE_CHANGED();
int expEVT_COMMAND_DATAVIEW_SELECTION_CHANGED();
int expEVT_COMMAND_DATAVIEW_ITEM_ACTIVATED();
int expEVT_COMMAND_DATAVIEW_ITEM_COLLAPSED();
int expEVT_COMMAND_DATAVIEW_ITEM_EXPANDED();
int expEVT_COMMAND_DATAVIEW_ITEM_COLLAPSING();
int expEVT_COMMAND_DATAVIEW_ITEM_EXPANDING();
int expEVT_COMMAND_DATAVIEW_ITEM_START_EDITING();
int expEVT_COMMAND_DATAVIEW_ITEM_EDITING_STARTED();
int expEVT_COMMAND_DATAVIEW_ITEM_EDITING_DONE();
int expEVT_COMMAND_DATAVIEW_ITEM_VALUE_CHANGED();
int expEVT_COMMAND_DATAVIEW_ITEM_CONTEXT_MENU();
int expEVT_COMMAND_DATAVIEW_COLUMN_HEADER_CLICK();
int expEVT_COMMAND_DATAVIEW_COLUMN_HEADER_RIGHT_CLICK();
int expEVT_COMMAND_DATAVIEW_COLUMN_SORTED();
int expEVT_COMMAND_DATAVIEW_COLUMN_REORDERED();
int expEVT_COMMAND_DATAVIEW_CACHE_HINT();
int expEVT_COMMAND_DATAVIEW_ITEM_BEGIN_DRAG();
int expEVT_COMMAND_DATAVIEW_ITEM_DROP_POSSIBLE();
int expEVT_COMMAND_DATAVIEW_ITEM_DROP();
int expEVT_DATE_CHANGED();
int expEVT_WINDOW_MODAL_DIALOG_CLOSED();
int expEVT_COMMAND_BUTTON_CLICKED();
int expEVT_COMMAND_CHECKBOX_CLICKED();
int expEVT_COMMAND_CHOICE_SELECTED();
int expEVT_COMMAND_LISTBOX_SELECTED();
int expEVT_COMMAND_LISTBOX_DOUBLECLICKED();
int expEVT_COMMAND_CHECKLISTBOX_TOGGLED();
int expEVT_COMMAND_MENU_SELECTED();
int expEVT_COMMAND_SLIDER_UPDATED();
int expEVT_COMMAND_RADIOBOX_SELECTED();
int expEVT_COMMAND_RADIOBUTTON_SELECTED();
int expEVT_COMMAND_VLBOX_SELECTED();
int expEVT_COMMAND_COMBOBOX_SELECTED();
int expEVT_COMMAND_TOOL_RCLICKED();
int expEVT_COMMAND_TOOL_DROPDOWN_CLICKED();
int expEVT_COMMAND_TOOL_ENTER();
int expEVT_COMMAND_COMBOBOX_DROPDOWN();
int expEVT_COMMAND_COMBOBOX_CLOSEUP();
int expEVT_COMMAND_THREAD();
int expEVT_LEFT_DOWN();
int expEVT_LEFT_UP();
int expEVT_MIDDLE_DOWN();
int expEVT_MIDDLE_UP();
int expEVT_RIGHT_DOWN();
int expEVT_RIGHT_UP();
int expEVT_MOTION();
int expEVT_ENTER_WINDOW();
int expEVT_LEAVE_WINDOW();
int expEVT_LEFT_DCLICK();
int expEVT_MIDDLE_DCLICK();
int expEVT_RIGHT_DCLICK();
int expEVT_SET_FOCUS();
int expEVT_KILL_FOCUS();
int expEVT_CHILD_FOCUS();
int expEVT_MOUSEWHEEL();
int expEVT_AUX1_DOWN();
int expEVT_AUX1_UP();
int expEVT_AUX1_DCLICK();
int expEVT_AUX2_DOWN();
int expEVT_AUX2_UP();
int expEVT_AUX2_DCLICK();
int expEVT_CHAR();
int expEVT_CHAR_HOOK();
int expEVT_NAVIGATION_KEY();
int expEVT_KEY_DOWN();
int expEVT_KEY_UP();
int expEVT_HOTKEY();
int expEVT_SET_CURSOR();
int expEVT_SCROLL_TOP();
int expEVT_SCROLL_BOTTOM();
int expEVT_SCROLL_LINEUP();
int expEVT_SCROLL_LINEDOWN();
int expEVT_SCROLL_PAGEUP();
int expEVT_SCROLL_PAGEDOWN();
int expEVT_SCROLL_THUMBTRACK();
int expEVT_SCROLL_THUMBRELEASE();
int expEVT_SCROLL_CHANGED();
int expEVT_SPIN_UP();
int expEVT_SPIN_DOWN();
int expEVT_SPIN();
int expEVT_SCROLLWIN_TOP();
int expEVT_SCROLLWIN_BOTTOM();
int expEVT_SCROLLWIN_LINEUP();
int expEVT_SCROLLWIN_LINEDOWN();
int expEVT_SCROLLWIN_PAGEUP();
int expEVT_SCROLLWIN_PAGEDOWN();
int expEVT_SCROLLWIN_THUMBTRACK();
int expEVT_SCROLLWIN_THUMBRELEASE();
int expEVT_SIZE();
int expEVT_MOVE();
int expEVT_CLOSE_WINDOW();
int expEVT_END_SESSION();
int expEVT_QUERY_END_SESSION();
int expEVT_ACTIVATE_APP();
int expEVT_ACTIVATE();
int expEVT_CREATE();
int expEVT_DESTROY();
int expEVT_SHOW();
int expEVT_ICONIZE();
int expEVT_MAXIMIZE();
int expEVT_MOUSE_CAPTURE_CHANGED();
int expEVT_MOUSE_CAPTURE_LOST();
int expEVT_PAINT();
int expEVT_ERASE_BACKGROUND();
int expEVT_NC_PAINT();
int expEVT_MENU_OPEN();
int expEVT_MENU_CLOSE();
int expEVT_MENU_HIGHLIGHT();
int expEVT_CONTEXT_MENU();
int expEVT_SYS_COLOUR_CHANGED();
int expEVT_DISPLAY_CHANGED();
int expEVT_QUERY_NEW_PALETTE();
int expEVT_PALETTE_CHANGED();
int expEVT_JOY_BUTTON_DOWN();
int expEVT_JOY_BUTTON_UP();
int expEVT_JOY_MOVE();
int expEVT_JOY_ZMOVE();
int expEVT_DROP_FILES();
int expEVT_INIT_DIALOG();
int expEVT_IDLE();
int expEVT_UPDATE_UI();
int expEVT_SIZING();
int expEVT_MOVING();
int expEVT_MOVE_START();
int expEVT_MOVE_END();
int expEVT_HIBERNATE();
int expEVT_COMMAND_TEXT_COPY();
int expEVT_COMMAND_TEXT_CUT();
int expEVT_COMMAND_TEXT_PASTE();
int expEVT_COMMAND_LEFT_CLICK();
int expEVT_COMMAND_LEFT_DCLICK();
int expEVT_COMMAND_RIGHT_CLICK();
int expEVT_COMMAND_RIGHT_DCLICK();
int expEVT_COMMAND_SET_FOCUS();
int expEVT_COMMAND_KILL_FOCUS();
int expEVT_COMMAND_ENTER();
int expEVT_HELP();
int expEVT_DETAILED_HELP();
int expEVT_COMMAND_TOOL_CLICKED();

/* eljfindrepldlg.h */
int expEVT_COMMAND_FIND();
int expEVT_COMMAND_FIND_NEXT();
int expEVT_COMMAND_FIND_REPLACE();
int expEVT_COMMAND_FIND_REPLACE_ALL();
int expEVT_COMMAND_FIND_CLOSE();

int expEVT_FILECTRL_SELECTIONCHANGED();
int expEVT_FILECTRL_FILEACTIVATED();
int expEVT_FILECTRL_FOLDERCHANGED();
int expEVT_FILECTRL_FILTERCHANGED();
int expEVT_COMMAND_FILEPICKER_CHANGED();
int expEVT_COMMAND_DIRPICKER_CHANGED();
int expEVT_COMMAND_FONTPICKER_CHANGED();
int expEVT_FSWATCHER();
int expEVT_GRID_CELL_LEFT_CLICK();
int expEVT_GRID_CELL_RIGHT_CLICK();
int expEVT_GRID_CELL_LEFT_DCLICK();
int expEVT_GRID_CELL_RIGHT_DCLICK();
int expEVT_GRID_LABEL_LEFT_CLICK();
int expEVT_GRID_LABEL_RIGHT_CLICK();
int expEVT_GRID_LABEL_LEFT_DCLICK();
int expEVT_GRID_LABEL_RIGHT_DCLICK();
int expEVT_GRID_ROW_SIZE();
int expEVT_GRID_COL_SIZE();
int expEVT_GRID_RANGE_SELECT();
int expEVT_GRID_CELL_CHANGING();
int expEVT_GRID_CELL_CHANGED();
int expEVT_GRID_SELECT_CELL();
int expEVT_GRID_EDITOR_SHOWN();
int expEVT_GRID_EDITOR_HIDDEN();
int expEVT_GRID_EDITOR_CREATED();
int expEVT_GRID_CELL_BEGIN_DRAG();
int expEVT_GRID_COL_MOVE();
int expEVT_GRID_COL_SORT();
int expEVT_QUERY_LAYOUT_INFO();
int expEVT_CALCULATE_LAYOUT();
int expEVT_SASH_DRAGGED();
int expEVT_COMMAND_HEADER_CLICK();
int expEVT_COMMAND_HEADER_RIGHT_CLICK();
int expEVT_COMMAND_HEADER_MIDDLE_CLICK();
int expEVT_COMMAND_HEADER_DCLICK();
int expEVT_COMMAND_HEADER_RIGHT_DCLICK();
int expEVT_COMMAND_HEADER_MIDDLE_DCLICK();
int expEVT_COMMAND_HEADER_SEPARATOR_DCLICK();
int expEVT_COMMAND_HEADER_BEGIN_RESIZE();
int expEVT_COMMAND_HEADER_RESIZING();
int expEVT_COMMAND_HEADER_END_RESIZE();
int expEVT_COMMAND_HEADER_BEGIN_REORDER();
int expEVT_COMMAND_HEADER_END_REORDER();
int expEVT_COMMAND_HEADER_DRAGGING_CANCELLED();
int expEVT_COMMAND_HTML_CELL_CLICKED();
int expEVT_COMMAND_HTML_CELL_HOVER();
int expEVT_COMMAND_HTML_LINK_CLICKED();
int expEVT_COMMAND_HYPERLINK();
int expEVT_COMMAND_LIST_BEGIN_DRAG();
int expEVT_COMMAND_LIST_BEGIN_RDRAG();
int expEVT_COMMAND_LIST_BEGIN_LABEL_EDIT();
int expEVT_COMMAND_LIST_END_LABEL_EDIT();
int expEVT_COMMAND_LIST_DELETE_ITEM();
int expEVT_COMMAND_LIST_DELETE_ALL_ITEMS();
int expEVT_COMMAND_LIST_ITEM_SELECTED();
int expEVT_COMMAND_LIST_ITEM_DESELECTED();
int expEVT_COMMAND_LIST_KEY_DOWN();
int expEVT_COMMAND_LIST_INSERT_ITEM();
int expEVT_COMMAND_LIST_COL_CLICK();
int expEVT_COMMAND_LIST_ITEM_RIGHT_CLICK();
int expEVT_COMMAND_LIST_ITEM_MIDDLE_CLICK();
int expEVT_COMMAND_LIST_ITEM_ACTIVATED();
int expEVT_COMMAND_LIST_CACHE_HINT();
int expEVT_COMMAND_LIST_COL_RIGHT_CLICK();
int expEVT_COMMAND_LIST_COL_BEGIN_DRAG();
int expEVT_COMMAND_LIST_COL_DRAGGING();
int expEVT_COMMAND_LIST_COL_END_DRAG();
int expEVT_COMMAND_LIST_ITEM_FOCUSED();
int expEVT_COMMAND_LISTBOOK_PAGE_CHANGED();
int expEVT_COMMAND_LISTBOOK_PAGE_CHANGING();

int expEVT_COMMAND_NOTEBOOK_PAGE_CHANGED();
int expEVT_COMMAND_NOTEBOOK_PAGE_CHANGING();
int expEVT_POWER_SUSPENDING();
int expEVT_POWER_SUSPENDED();
int expEVT_POWER_SUSPEND_CANCEL();
int expEVT_POWER_RESUME();
int expEVT_END_PROCESS();
int expEVT_PG_SELECTED();
int expEVT_PG_CHANGING();
int expEVT_PG_CHANGED();
int expEVT_PG_HIGHLIGHTED();
int expEVT_PG_RIGHT_CLICK();
int expEVT_PG_PAGE_CHANGED();
int expEVT_PG_ITEM_COLLAPSED();
int expEVT_PG_ITEM_EXPANDED();
int expEVT_PG_DOUBLE_CLICK();
int expEVT_COMMAND_RIBBONBAR_PAGE_CHANGED();
int expEVT_COMMAND_RIBBONBAR_PAGE_CHANGING();
int expEVT_COMMAND_RIBBONBAR_TAB_MIDDLE_DOWN();
int expEVT_COMMAND_RIBBONBAR_TAB_MIDDLE_UP();
int expEVT_COMMAND_RIBBONBAR_TAB_RIGHT_DOWN();
int expEVT_COMMAND_RIBBONBAR_TAB_RIGHT_UP();
int expEVT_COMMAND_RIBBONBUTTON_CLICKED();
int expEVT_COMMAND_RIBBONBUTTON_DROPDOWN_CLICKED();
int expEVT_COMMAND_RIBBONGALLERY_HOVER_CHANGED();
int expEVT_COMMAND_RIBBONGALLERY_SELECTED();
int expEVT_COMMAND_RIBBONTOOL_CLICKED();
int expEVT_COMMAND_RIBBONTOOL_DROPDOWN_CLICKED();
int expEVT_COMMAND_RICHTEXT_LEFT_CLICK();
int expEVT_COMMAND_RICHTEXT_RIGHT_CLICK();
int expEVT_COMMAND_RICHTEXT_MIDDLE_CLICK();
int expEVT_COMMAND_RICHTEXT_LEFT_DCLICK();
int expEVT_COMMAND_RICHTEXT_RETURN();
int expEVT_COMMAND_RICHTEXT_CHARACTER();
int expEVT_COMMAND_RICHTEXT_DELETE();
int expEVT_COMMAND_RICHTEXT_STYLESHEET_CHANGING();
int expEVT_COMMAND_RICHTEXT_STYLESHEET_CHANGED();
int expEVT_COMMAND_RICHTEXT_STYLESHEET_REPLACING();
int expEVT_COMMAND_RICHTEXT_STYLESHEET_REPLACED();
int expEVT_COMMAND_RICHTEXT_CONTENT_INSERTED();
int expEVT_COMMAND_RICHTEXT_CONTENT_DELETED();
int expEVT_COMMAND_RICHTEXT_STYLE_CHANGED();
int expEVT_COMMAND_RICHTEXT_SELECTION_CHANGED();
int expEVT_COMMAND_RICHTEXT_BUFFER_RESET();
int expEVT_SOCKET();
int expEVT_COMMAND_SPINCTRL_UPDATED();
int expEVT_COMMAND_SPINCTRLDOUBLE_UPDATED();
int expEVT_COMMAND_SPLITTER_SASH_POS_CHANGED();
int expEVT_COMMAND_SPLITTER_SASH_POS_CHANGING();
int expEVT_COMMAND_SPLITTER_DOUBLECLICKED();
int expEVT_COMMAND_SPLITTER_UNSPLIT();
int expEVT_COMMAND_SEARCHCTRL_CANCEL_BTN();
int expEVT_COMMAND_SEARCHCTRL_SEARCH_BTN();

/* taskbaricon.cpp */
int expEVT_TASKBAR_MOVE();
int expEVT_TASKBAR_LEFT_DOWN();
int expEVT_TASKBAR_LEFT_UP();
int expEVT_TASKBAR_RIGHT_DOWN();
int expEVT_TASKBAR_RIGHT_UP();
int expEVT_TASKBAR_LEFT_DCLICK();
int expEVT_TASKBAR_RIGHT_DCLICK();
int expEVT_TASKBAR_BALLOON_TIMEOUT();
int expEVT_TASKBAR_BALLOON_CLICK();

int expEVT_COMMAND_TEXT_UPDATED();
int expEVT_COMMAND_TEXT_ENTER();
int expEVT_COMMAND_TEXT_URL();
int expEVT_COMMAND_TEXT_MAXLEN();
int expEVT_COMMAND_TOGGLEBUTTON_CLICKED();
int expEVT_TIMER();
int expEVT_COMMAND_TOOLBOOK_PAGE_CHANGED();
int expEVT_COMMAND_TOOLBOOK_PAGE_CHANGING();
int expEVT_COMMAND_TREE_BEGIN_DRAG();
int expEVT_COMMAND_TREE_BEGIN_RDRAG();
int expEVT_COMMAND_TREE_BEGIN_LABEL_EDIT();
int expEVT_COMMAND_TREE_END_LABEL_EDIT();
int expEVT_COMMAND_TREE_DELETE_ITEM();
int expEVT_COMMAND_TREE_GET_INFO();
int expEVT_COMMAND_TREE_SET_INFO();
int expEVT_COMMAND_TREE_ITEM_EXPANDED();
int expEVT_COMMAND_TREE_ITEM_EXPANDING();
int expEVT_COMMAND_TREE_ITEM_COLLAPSED();
int expEVT_COMMAND_TREE_ITEM_COLLAPSING();
int expEVT_COMMAND_TREE_SEL_CHANGED();
int expEVT_COMMAND_TREE_SEL_CHANGING();
int expEVT_COMMAND_TREE_KEY_DOWN();
int expEVT_COMMAND_TREE_ITEM_ACTIVATED();
int expEVT_COMMAND_TREE_ITEM_RIGHT_CLICK();
int expEVT_COMMAND_TREE_ITEM_MIDDLE_CLICK();
int expEVT_COMMAND_TREE_END_DRAG();
int expEVT_COMMAND_TREE_STATE_IMAGE_CLICK();
int expEVT_COMMAND_TREE_ITEM_GETTOOLTIP();
int expEVT_COMMAND_TREE_ITEM_MENU();
int expEVT_COMMAND_TREEBOOK_PAGE_CHANGED();
int expEVT_COMMAND_TREEBOOK_PAGE_CHANGING();
int expEVT_COMMAND_TREEBOOK_NODE_COLLAPSED();
int expEVT_COMMAND_TREEBOOK_NODE_EXPANDED();
int expEVT_WIZARD_PAGE_CHANGED();
int expEVT_WIZARD_PAGE_CHANGING();
int expEVT_WIZARD_CANCEL();
int expEVT_WIZARD_HELP();
int expEVT_WIZARD_FINISHED();
int expEVT_WIZARD_PAGE_SHOWN();
int expEVT_DELETE();
int expEVT_HTML_CELL_CLICKED();
int expEVT_HTML_CELL_MOUSE_HOVER();
int expEVT_HTML_LINK_CLICKED();
int expEVT_HTML_SET_TITLE();
int expEVT_INPUT_SINK();
int expEVT_SORT();

/* Keys */
int        expK_BACK(  );
int        expK_TAB(  );
int        expK_RETURN(  );
int        expK_ESCAPE(  );
int        expK_SPACE(  );
int        expK_DELETE(  );
int        expK_START(  );
int        expK_LBUTTON(  );
int        expK_RBUTTON(  );
int        expK_CANCEL(  );
int        expK_MBUTTON(  );
int        expK_CLEAR(  );
int        expK_SHIFT(  );
int        expK_ALT(  );
int        expK_CONTROL(  );
int        expK_MENU(  );
int        expK_PAUSE(  );
int        expK_CAPITAL(  );
int        expK_END(  );
int        expK_HOME(  );
int        expK_LEFT(  );
int        expK_UP(  );
int        expK_RIGHT(  );
int        expK_DOWN(  );
int        expK_SELECT(  );
int        expK_PRINT(  );
int        expK_EXECUTE(  );
int        expK_SNAPSHOT(  );
int        expK_INSERT(  );
int        expK_HELP(  );
int        expK_NUMPAD0(  );
int        expK_NUMPAD1(  );
int        expK_NUMPAD2(  );
int        expK_NUMPAD3(  );
int        expK_NUMPAD4(  );
int        expK_NUMPAD5(  );
int        expK_NUMPAD6(  );
int        expK_NUMPAD7(  );
int        expK_NUMPAD8(  );
int        expK_NUMPAD9(  );
int        expK_MULTIPLY(  );
int        expK_ADD(  );
int        expK_SEPARATOR(  );
int        expK_SUBTRACT(  );
int        expK_DECIMAL(  );
int        expK_DIVIDE(  );
int        expK_F1(  );
int        expK_F2(  );
int        expK_F3(  );
int        expK_F4(  );
int        expK_F5(  );
int        expK_F6(  );
int        expK_F7(  );
int        expK_F8(  );
int        expK_F9(  );
int        expK_F10(  );
int        expK_F11(  );
int        expK_F12(  );
int        expK_F13(  );
int        expK_F14(  );
int        expK_F15(  );
int        expK_F16(  );
int        expK_F17(  );
int        expK_F18(  );
int        expK_F19(  );
int        expK_F20(  );
int        expK_F21(  );
int        expK_F22(  );
int        expK_F23(  );
int        expK_F24(  );
int        expK_NUMLOCK(  );
int        expK_SCROLL(  );
int        expK_PAGEUP(  );
int        expK_PAGEDOWN(  );
int        expK_NUMPAD_SPACE(  );
int        expK_NUMPAD_TAB(  );
int        expK_NUMPAD_ENTER(  );
int        expK_NUMPAD_F1(  );
int        expK_NUMPAD_F2(  );
int        expK_NUMPAD_F3(  );
int        expK_NUMPAD_F4(  );
int        expK_NUMPAD_HOME(  );
int        expK_NUMPAD_LEFT(  );
int        expK_NUMPAD_UP(  );
int        expK_NUMPAD_RIGHT(  );
int        expK_NUMPAD_DOWN(  );
int        expK_NUMPAD_PAGEUP(  );
int        expK_NUMPAD_PAGEDOWN(  );
int        expK_NUMPAD_END(  );
int        expK_NUMPAD_BEGIN(  );
int        expK_NUMPAD_INSERT(  );
int        expK_NUMPAD_DELETE(  );
int        expK_NUMPAD_EQUAL(  );
int        expK_NUMPAD_MULTIPLY(  );
int        expK_NUMPAD_ADD(  );
int        expK_NUMPAD_SEPARATOR(  );
int        expK_NUMPAD_SUBTRACT(  );
int        expK_NUMPAD_DECIMAL(  );
int        expK_NUMPAD_DIVIDE(  );


/* Misc. */
int        ELJSysErrorCode(  );
void*      ELJSysErrorMsg( int nErrCode );
void       LogErrorMsg( TClass(wxString) _msg );
void       LogFatalErrorMsg( TClass(wxString) _msg );
void       LogMessageMsg( TClass(wxString) _msg );
void       LogWarningMsg( TClass(wxString) _msg );
TBool      Quantize( TClass(wxImage) src, TClass(wxImage) dest, int desiredNoColours, void* eightBitData, int flags );
TBool      QuantizePalette( TClass(wxImage) src, TClass(wxImage) dest, void* pPalette, int desiredNoColours, void* eightBitData, int flags );
void       wxCFree( void* _ptr );
TClass(ELJLocale) wxGetELJLocale(  );
void*      wxGetELJTranslation( TStringVoid sz );
void       wxMutexGui_Enter(  );
void       wxMutexGui_Leave(  );

/* ELJApp */
TClassDefExtend(ELJApp,wxApp)
void       ELJApp_Bell(  );
TClass(ELJLog) ELJApp_CreateLogTarget(  );
void       ELJApp_Dispatch(  );
TClass(wxSize) ELJApp_DisplaySize(  );
void       ELJApp_EnableTooltips( TBool _enable );
void       ELJApp_EnableTopLevelWindows( int _enb );
int        ELJApp_ExecuteProcess( TClass(wxString) _cmd, int _snc, TClass(wxProcess) _prc );
void       ELJApp_Exit(  );
void       ELJApp_ExitMainLoop(  );
void*      ELJApp_FindWindowById( int _id, TClass(wxWindow) _prt );
TClass(wxWindow) ELJApp_FindWindowByLabel( TClass(wxString) _lbl, TClass(wxWindow) _prt );
TClass(wxWindow) ELJApp_FindWindowByName( TClass(wxString) _lbl, TClass(wxWindow) _prt );
TClass(wxApp) ELJApp_GetApp(  );
TClass(wxString) ELJApp_GetAppName( );
TClass(wxString) ELJApp_GetClassName( );
int        ELJApp_GetExitOnFrameDelete( );
TClass(wxString) ELJApp_GetOsDescription( );
int        ELJApp_GetOsVersion( void* _maj, void* _min );
TClass(wxWindow) ELJApp_GetTopWindow(  );
int        ELJApp_GetUseBestVisual(  );
TClass(wxString) ELJApp_GetUserHome( void* _usr );
TClass(wxString) ELJApp_GetUserId( );
TClass(wxString) ELJApp_GetUserName( );
TClass(wxString) ELJApp_GetVendorName( );
/* int        ELJApp_GetWantDebugOutput(  ); */
void       ELJApp_InitAllImageHandlers(  );
TBool      ELJApp_Initialized(  );
int        ELJApp_MainLoop(  );
TClass(wxPoint) ELJApp_MousePosition( );
int        ELJApp_Pending(  );
int        ELJApp_SafeYield( TClass(wxWindow) _win );
/* int        ELJApp_SendIdleEvents(  ); */
/* int        ELJApp_SendIdleEventsToWindow( TClass(wxWindow) win ); */
void       ELJApp_SetAppName( TClass(wxString) name );
void       ELJApp_SetClassName( TClass(wxString) name );
void       ELJApp_SetExitOnFrameDelete( int flag );
void       ELJApp_SetPrintMode( int mode );
void       ELJApp_SetTooltipDelay( int _ms );
void       ELJApp_SetTopWindow( TClass(wxWindow) _wnd );
void       ELJApp_SetUseBestVisual( int flag );
void       ELJApp_SetVendorName( TClass(wxString) name );
void       ELJApp_Sleep( int _scs );
void       ELJApp_MilliSleep( int _mscs );
int        ELJApp_Yield(  );
TBoolInt   ELJApp_IsTerminating(  );


/* ELJArtProv */
TClassDefExtend(ELJArtProv,wxArtProvider)
TClass(ELJArtProv) ELJArtProv_Create( void* _obj, void* _clb );
void       ELJArtProv_Release( TSelf(ELJArtProv) _obj );

/* ELJClient */
TClassDefExtend(ELJClient,wxClient)
TClass(ELJClient) ELJClient_Create( void* _eobj, void* _cnct );
void       ELJClient_Delete( TSelf(ELJClient) _obj );
void       ELJClient_MakeConnection( TSelf(ELJClient) _obj, TClass(wxString) host, TClass(wxServer) server, TClass(wxString) topic );

/* ELJCommand */
TClassDefExtend(ELJCommand,wxCommand)
TBool      ELJCommand_CanUndo( TSelf(ELJCommand) _obj );
TClass(ELJCommand) ELJCommand_Create( int _und, TClass(wxString) _nme, void* _obj, void* _clb );
void       ELJCommand_Delete( TSelf(ELJCommand) _obj );
TClass(wxString) ELJCommand_GetName( TSelf(ELJCommand) _obj );

/* ELJConnection */
TClassDefExtend(ELJConnection,wxConnection)
int        ELJConnection_Advise( TSelf(ELJConnection) _obj, TClass(wxString) item, void* data, int size, int format );
void       ELJConnection_Compress( TSelf(ELJConnection) _obj, int on );
TClass(ELJConnection) ELJConnection_Create( void* _obj, void* buffer, int size );
TClass(ELJConnection) ELJConnection_CreateDefault( TSelf(ELJConnection) _obj );
void       ELJConnection_Delete( TSelf(ELJConnection) _obj );
TBool      ELJConnection_Disconnect( TSelf(ELJConnection) _obj );
TBool      ELJConnection_Execute( TSelf(ELJConnection) _obj, TClass(wxString) data, int size, int format );
TBool      ELJConnection_Poke( TSelf(ELJConnection) _obj, TClass(wxString) item, void* data, int size, int format );
void*      ELJConnection_Request( TSelf(ELJConnection) _obj, TClass(wxString) item, TClass(wxSize) size, int format );
void       ELJConnection_SetOnAdvise( TSelf(ELJConnection) _obj, void* _fnc );
void       ELJConnection_SetOnDisconnect( TSelf(ELJConnection) _obj, void* _fnc );
void       ELJConnection_SetOnExecute( TSelf(ELJConnection) _obj, void* _fnc );
void       ELJConnection_SetOnPoke( TSelf(ELJConnection) _obj, void* _fnc );
void       ELJConnection_SetOnRequest( TSelf(ELJConnection) _obj, void* _fnc );
void       ELJConnection_SetOnStartAdvise( TSelf(ELJConnection) _obj, void* _fnc );
void       ELJConnection_SetOnStopAdvise( TSelf(ELJConnection) _obj, void* _fnc );
TBool      ELJConnection_StartAdvise( TSelf(ELJConnection) _obj, TClass(wxString) item );
TBool      ELJConnection_StopAdvise( TSelf(ELJConnection) _obj, TClass(wxString) item );

/* ELJDragDataObject */
TClassDef(ELJDragDataObject)
TClass(ELJDragDataObject) ELJDragDataObject_Create( void* _obj, TClass(wxString) _fmt, void* _func1, void* _func2, void* _func3 );
void       ELJDragDataObject_Delete( TSelf(ELJDragDataObject) _obj );

/* ELJDropTarget */
TClassDefExtend(ELJDropTarget,wxDropTarget)
TClass(ELJDropTarget) ELJDropTarget_Create( void* _obj );
void       ELJDropTarget_Delete( TSelf(ELJDropTarget) _obj );
void       ELJDropTarget_SetOnData( TSelf(ELJDropTarget) _obj, void* _func );
void       ELJDropTarget_SetOnDragOver( TSelf(ELJDropTarget) _obj, void* _func );
void       ELJDropTarget_SetOnDrop( TSelf(ELJDropTarget) _obj, void* _func );
void       ELJDropTarget_SetOnEnter( TSelf(ELJDropTarget) _obj, void* _func );
void       ELJDropTarget_SetOnLeave( TSelf(ELJDropTarget) _obj, void* _func );

/* ELJFileDropTarget */
TClassDefExtend(ELJFileDropTarget,wxFileDropTarget)
TClass(ELJFileDropTarget) ELJFileDropTarget_Create( void* _obj, void* _func );
void       ELJFileDropTarget_Delete( TSelf(ELJFileDropTarget) _obj );
void       ELJFileDropTarget_SetOnData( TSelf(ELJFileDropTarget) _obj, void* _func );
void       ELJFileDropTarget_SetOnDragOver( TSelf(ELJFileDropTarget) _obj, void* _func );
void       ELJFileDropTarget_SetOnDrop( TSelf(ELJFileDropTarget) _obj, void* _func );
void       ELJFileDropTarget_SetOnEnter( TSelf(ELJFileDropTarget) _obj, void* _func );
void       ELJFileDropTarget_SetOnLeave( TSelf(ELJFileDropTarget) _obj, void* _func );

/* ELJGridTable */
TClassDefExtend(ELJGridTable,wxGridTableBase)
TClass(ELJGridTable) ELJGridTable_Create( void* _obj, void* _EifGetNumberRows, void* _EifGetNumberCols, void* _EifGetValue, void* _EifSetValue, void* _EifIsEmptyCell, void* _EifClear, void* _EifInsertRows, void* _EifAppendRows, void* _EifDeleteRows, void* _EifInsertCols, void* _EifAppendCols, void* _EifDeleteCols, void* _EifSetRowLabelValue, void* _EifSetColLabelValue, void* _EifGetRowLabelValue, void* _EifGetColLabelValue );
void       ELJGridTable_Delete( TSelf(ELJGridTable) _obj );
TClass(wxView) ELJGridTable_GetView( TSelf(ELJGridTable) _obj );
void*      ELJGridTable_SendTableMessage( TSelf(ELJGridTable) _obj, int id, int val1, int val2 );

/* ELJLocale */
TClassDefExtend(ELJLocale,wxLocale)

/* ELJLog */
TClassDefExtend(ELJLog,wxLog)
void       ELJLog_AddTraceMask( TSelf(ELJLog) _obj, TStringVoid str );
TClass(ELJLog) ELJLog_Create( void* _obj, void* _fnc );
void       ELJLog_Delete( TSelf(ELJLog) _obj );
void       ELJLog_DontCreateOnDemand( TSelf(ELJLog) _obj );
int        ELJLog_EnableLogging( TSelf(ELJLog) _obj, TBool doIt );
void       ELJLog_Flush( TSelf(ELJLog) _obj );
void       ELJLog_FlushActive( TSelf(ELJLog) _obj );
void*      ELJLog_GetActiveTarget(  );
void*      ELJLog_GetTimestamp( TSelf(ELJLog) _obj );
#if !wxCHECK_VERSION(3, 1, 0)
int        ELJLog_GetTraceMask( TSelf(ELJLog) _obj );
#endif
int        ELJLog_GetVerbose( TSelf(ELJLog) _obj );
TBool      ELJLog_HasPendingMessages( TSelf(ELJLog) _obj );
TBool      ELJLog_IsAllowedTraceMask( TSelf(ELJLog) _obj, TClass(wxMask) mask );
TBool      ELJLog_IsEnabled( TSelf(ELJLog) _obj );
void       ELJLog_OnLog( TSelf(ELJLog) _obj, int level, void* szString, int t );
void       ELJLog_RemoveTraceMask( TSelf(ELJLog) _obj, TStringVoid str );
void       ELJLog_Resume( TSelf(ELJLog) _obj );
void*      ELJLog_SetActiveTarget( TSelf(ELJLog) pLogger );
void       ELJLog_SetTimestamp( TSelf(ELJLog) _obj, void* ts );
// Deprecated:
//void       ELJLog_SetTraceMask( TSelf(ELJLog) _obj, int ulMask );
void       ELJLog_SetVerbose( TSelf(ELJLog) _obj, int bVerbose );
void       ELJLog_Suspend( TSelf(ELJLog) _obj );

/* ELJMessageParameters */
TClassDef(ELJMessageParameters)
TClass(ELJMessageParameters) wxMessageParameters_Create( TStringVoid _file, TStringVoid _type, void* _object, void* _func );
void       wxMessageParameters_Delete( TSelf(ELJMessageParameters) _obj );

/* ELJPlotCurve */
TClassDefExtend(ELJPlotCurve,wxPlotCurve)
TClass(ELJPlotCurve) ELJPlotCurve_Create( void* _obj, void* _str, void* _end, void* _y, int offsetY, double startY, double endY );
void       ELJPlotCurve_Delete( TSelf(ELJPlotCurve) _obj );
double     ELJPlotCurve_GetEndY( TSelf(ELJPlotCurve) _obj );
int        ELJPlotCurve_GetOffsetY( TSelf(ELJPlotCurve) _obj );
double     ELJPlotCurve_GetStartY( TSelf(ELJPlotCurve) _obj );
void       ELJPlotCurve_SetEndY( TSelf(ELJPlotCurve) _obj, double endY );
void       ELJPlotCurve_SetOffsetY( TSelf(ELJPlotCurve) _obj, int offsetY );
void       ELJPlotCurve_SetPenNormal( TSelf(ELJPlotCurve) _obj, TClass(wxPen) pen );
void       ELJPlotCurve_SetPenSelected( TSelf(ELJPlotCurve) _obj, TClass(wxPen) pen );
void       ELJPlotCurve_SetStartY( TSelf(ELJPlotCurve) _obj, double startY );

/* ELJPreviewControlBar */
TClassDefExtend(ELJPreviewControlBar,wxPreviewControlBar)
TClass(ELJPreviewControlBar) ELJPreviewControlBar_Create( void* preview, int buttons, TClass(wxWindow) parent, void* title, TRect(x,y,w,h), int style );

/* ELJPreviewFrame */
TClassDefExtend(ELJPreviewFrame,wxPreviewFrame)
TClass(ELJPreviewFrame) ELJPreviewFrame_Create( void* _obj, void* _init, void* _create_canvas, void* _create_toolbar, void* preview, TClass(wxWindow) parent, void* title, TRect(x,y,w,h), int style );
void*      ELJPreviewFrame_GetControlBar( TSelf(ELJPreviewFrame) _obj );
TClass(wxPreviewCanvas) ELJPreviewFrame_GetPreviewCanvas( TSelf(ELJPreviewFrame) _obj );
TClass(wxPrintPreview) ELJPreviewFrame_GetPrintPreview( TSelf(ELJPreviewFrame) _obj );
void       ELJPreviewFrame_Initialize( TSelf(ELJPreviewFrame) _obj );
void       ELJPreviewFrame_SetControlBar( TSelf(ELJPreviewFrame) _obj, void* obj );
void       ELJPreviewFrame_SetPreviewCanvas( TSelf(ELJPreviewFrame) _obj, TClass(wxPreviewCanvas) obj );
void       ELJPreviewFrame_SetPrintPreview( TSelf(ELJPreviewFrame) _obj, TClass(wxPrintPreview) obj );

/* ELJServer */
TClassDefExtend(ELJServer,wxServer)
TClass(ELJServer) ELJServer_Create( void* _eobj, void* _cnct );
void       ELJServer_Delete( TSelf(ELJServer) _obj );
int        ELJServer_Initialize( TSelf(ELJServer) _obj, TClass(wxString) name );

/* ELJTextDropTarget */
TClassDefExtend(ELJTextDropTarget,wxTextDropTarget)
TClass(ELJTextDropTarget) ELJTextDropTarget_Create( void* _obj, void* _func );
void       ELJTextDropTarget_Delete( TSelf(ELJTextDropTarget) _obj );
void       ELJTextDropTarget_SetOnData( TSelf(ELJTextDropTarget) _obj, void* _func );
void       ELJTextDropTarget_SetOnDragOver( TSelf(ELJTextDropTarget) _obj, void* _func );
void       ELJTextDropTarget_SetOnDrop( TSelf(ELJTextDropTarget) _obj, void* _func );
void       ELJTextDropTarget_SetOnEnter( TSelf(ELJTextDropTarget) _obj, void* _func );
void       ELJTextDropTarget_SetOnLeave( TSelf(ELJTextDropTarget) _obj, void* _func );

/* ELJTextValidator */
TClassDefExtend(ELJTextValidator,wxTextValidator)
TClass(ELJTextValidator) ELJTextValidator_Create( void* _obj, void* _fnc, TStringVoid _txt, int _stl );

/* cbAntiflickerPlugin */
TClassDefExtend(cbAntiflickerPlugin,cbPluginBase)
TClass(cbAntiflickerPlugin) cbAntiflickerPlugin_Create( void* pPanel, int paneMask );
TClass(cbAntiflickerPlugin) cbAntiflickerPlugin_CreateDefault(  );
void       cbAntiflickerPlugin_Delete( TSelf(cbAntiflickerPlugin) _obj );

/* cbBarDragPlugin */
TClassDefExtend(cbBarDragPlugin,cbPluginBase)
TClass(cbBarDragPlugin) cbBarDragPlugin_Create( void* pPanel, int paneMask );
TClass(cbBarDragPlugin) cbBarDragPlugin_CreateDefault(  );
void       cbBarDragPlugin_Delete( TSelf(cbBarDragPlugin) _obj );

/* cbBarHintsPlugin */
TClassDefExtend(cbBarHintsPlugin,cbPluginBase)
TClass(cbBarHintsPlugin) cbBarHintsPlugin_Create( void* pPanel, int paneMask );
TClass(cbBarHintsPlugin) cbBarHintsPlugin_CreateDefault(  );
void       cbBarHintsPlugin_Delete( TSelf(cbBarHintsPlugin) _obj );
void       cbBarHintsPlugin_SetGrooveCount( TSelf(cbBarHintsPlugin) _obj, int nGrooves );

/* cbBarInfo */
TClassDefExtend(cbBarInfo,wxObject)
TClass(cbBarInfo) cbBarInfo_Create(  );
void       cbBarInfo_Delete( TSelf(cbBarInfo) _obj );
TBool      cbBarInfo_IsExpanded( TSelf(cbBarInfo) _obj );
TBool      cbBarInfo_IsFixed( TSelf(cbBarInfo) _obj );

/* cbBarSpy */
TClassDefExtend(cbBarSpy,wxEvtHandler)
TClass(cbBarSpy) cbBarSpy_Create( void* pPanel );
TClass(cbBarSpy) cbBarSpy_CreateDefault(  );
void       cbBarSpy_Delete( TSelf(cbBarSpy) _obj );
int        cbBarSpy_ProcessEvent( TSelf(cbBarSpy) _obj, TClass(wxEvent) event );
void       cbBarSpy_SetBarWindow( TSelf(cbBarSpy) _obj, void* pWnd );

/* cbCloseBox */
TClassDefExtend(cbCloseBox,cbMiniButton)
TClass(cbCloseBox) cbCloseBox_Create(  );

/* cbCollapseBox */
TClassDefExtend(cbCollapseBox,cbMiniButton)
TClass(cbCollapseBox) cbCollapseBox_Create(  );

/* cbCommonPaneProperties */
TClassDefExtend(cbCommonPaneProperties,wxObject)
void       cbCommonPaneProperties_Assign( TSelf(cbCommonPaneProperties) _obj, void* _other );
int        cbCommonPaneProperties_BarCollapseIconsOn( TSelf(cbCommonPaneProperties) _obj );
int        cbCommonPaneProperties_BarDragHintsOn( TSelf(cbCommonPaneProperties) _obj );
int        cbCommonPaneProperties_BarFloatingOn( TSelf(cbCommonPaneProperties) _obj );
int        cbCommonPaneProperties_ColProportionsOn( TSelf(cbCommonPaneProperties) _obj );
TClass(cbCommonPaneProperties) cbCommonPaneProperties_CreateDefault(  );
void       cbCommonPaneProperties_Delete( TSelf(cbCommonPaneProperties) _obj );
int        cbCommonPaneProperties_ExactDockPredictionOn( TSelf(cbCommonPaneProperties) _obj );
void       cbCommonPaneProperties_MinCBarDim( TSelf(cbCommonPaneProperties) _obj, TSizeOutVoid(_w,_h) );
int        cbCommonPaneProperties_NonDestructFrictionOn( TSelf(cbCommonPaneProperties) _obj );
int        cbCommonPaneProperties_OutOfPaneDragOn( TSelf(cbCommonPaneProperties) _obj );
int        cbCommonPaneProperties_RealTimeUpdatesOn( TSelf(cbCommonPaneProperties) _obj );
int        cbCommonPaneProperties_ResizeHandleSize( TSelf(cbCommonPaneProperties) _obj );
int        cbCommonPaneProperties_RowProportionsOn( TSelf(cbCommonPaneProperties) _obj );
void       cbCommonPaneProperties_SetBarCollapseIconsOn( TSelf(cbCommonPaneProperties) _obj, int _val );
void       cbCommonPaneProperties_SetBarDragHintsOn( TSelf(cbCommonPaneProperties) _obj, int _val );
void       cbCommonPaneProperties_SetBarFloatingOn( TSelf(cbCommonPaneProperties) _obj, int _val );
void       cbCommonPaneProperties_SetColProportionsOn( TSelf(cbCommonPaneProperties) _obj, int _val );
void       cbCommonPaneProperties_SetExactDockPredictionOn( TSelf(cbCommonPaneProperties) _obj, int _val );
void       cbCommonPaneProperties_SetMinCBarDim( TSelf(cbCommonPaneProperties) _obj, TSize(_w,_h) );
void       cbCommonPaneProperties_SetNonDestructFrictionOn( TSelf(cbCommonPaneProperties) _obj, int _val );
void       cbCommonPaneProperties_SetOutOfPaneDragOn( TSelf(cbCommonPaneProperties) _obj, int _val );
void       cbCommonPaneProperties_SetRealTimeUpdatesOn( TSelf(cbCommonPaneProperties) _obj, int _val );
void       cbCommonPaneProperties_SetResizeHandleSize( TSelf(cbCommonPaneProperties) _obj, int _val );
void       cbCommonPaneProperties_SetRowProportionsOn( TSelf(cbCommonPaneProperties) _obj, int _val );
void       cbCommonPaneProperties_SetShow3DPaneBorderOn( TSelf(cbCommonPaneProperties) _obj, int _val );
int        cbCommonPaneProperties_Show3DPaneBorderOn( TSelf(cbCommonPaneProperties) _obj );

/* cbCustomizeBarEvent */
TClassDefExtend(cbCustomizeBarEvent,cbPluginEvent)
void*      cbCustomizeBarEvent_Bar( TSelf(cbCustomizeBarEvent) _obj );
void       cbCustomizeBarEvent_ClickPos( TSelf(cbCustomizeBarEvent) _obj, TPointOutVoid(_x,_y) );

/* cbCustomizeLayoutEvent */
TClassDefExtend(cbCustomizeLayoutEvent,cbPluginEvent)
void       cbCustomizeLayoutEvent_ClickPos( TSelf(cbCustomizeLayoutEvent) _obj, TPointOutVoid(_x,_y) );

/* cbDimHandlerBase */
TClassDefExtend(cbDimHandlerBase,wxObject)

/* cbDimInfo */
TClassDefExtend(cbDimInfo,wxObject)
void       cbDimInfo_Assign( TSelf(cbDimInfo) _obj, void* other );
TClass(cbDimInfo) cbDimInfo_Create( TPoint(x,y), TBool isFixed, int gap, void* pDimHandler );
TClass(cbDimInfo) cbDimInfo_CreateDefault(  );
void*      cbDimInfo_CreateWithHandler( TSelf(cbDimInfo) pDimHandler, TBool isFixed );
void*      cbDimInfo_CreateWithInfo( int dh_x, int dh_y, int dv_x, int dv_y, int f_x, int f_y, TBool isFixed, int horizGap, int vertGap, void* pDimHandler );
void       cbDimInfo_Delete( TSelf(cbDimInfo) _obj );
void*      cbDimInfo_GetDimHandler( TSelf(cbDimInfo) _obj );

/* cbDockBox */
TClassDefExtend(cbDockBox,cbMiniButton)
TClass(cbDockBox) cbDockBox_Create(  );

/* cbDockPane */
TClassDefExtend(cbDockPane,wxObject)
int        cbDockPane_BarPresent( TSelf(cbDockPane) _obj, void* pBar );
TClass(cbDockPane) cbDockPane_Create( int alignment, void* pPanel );
TClass(cbDockPane) cbDockPane_CreateDefault(  );
void       cbDockPane_Delete( TSelf(cbDockPane) _obj );
int        cbDockPane_GetAlignment( TSelf(cbDockPane) _obj );
void*      cbDockPane_GetBarInfoByWindow( TSelf(cbDockPane) _obj, void* pBarWnd );
void       cbDockPane_GetBarResizeRange( TSelf(cbDockPane) _obj, void* pBar, void* from, void* till, int forLeftHandle );
int        cbDockPane_GetDockingState( TSelf(cbDockPane) _obj );
void*      cbDockPane_GetFirstRow( TSelf(cbDockPane) _obj );
int        cbDockPane_GetPaneHeight( TSelf(cbDockPane) _obj );
void       cbDockPane_GetRealRect( TSelf(cbDockPane) _obj, TRectOutVoid(_x,_y,_w,_h) );
int        cbDockPane_GetRowList( TSelf(cbDockPane) _obj, void* _ref );
void       cbDockPane_GetRowResizeRange( TSelf(cbDockPane) _obj, void* pRow, void* from, void* till, int forUpperHandle );
int        cbDockPane_HitTestPaneItems( TSelf(cbDockPane) _obj, TPoint(x,y), void* ppRow, void* ppBar );
void       cbDockPane_InsertBarByCoord( TSelf(cbDockPane) _obj, void* pBar, TRect(x,y,w,h) );
void       cbDockPane_InsertBarByInfo( TSelf(cbDockPane) _obj, void* pBarInfo );
void       cbDockPane_InsertBarToRow( TSelf(cbDockPane) _obj, void* pBar, void* pIntoRow );
void       cbDockPane_InsertRow( TSelf(cbDockPane) _obj, void* pRow, void* pBeforeRow );
TBool      cbDockPane_IsHorizontal( TSelf(cbDockPane) _obj );
int        cbDockPane_MatchesMask( TSelf(cbDockPane) _obj, int paneMask );
void       cbDockPane_RemoveBar( TSelf(cbDockPane) _obj, void* pBar );
void       cbDockPane_RemoveRow( TSelf(cbDockPane) _obj, void* pRow );
void       cbDockPane_SetBoundsInParent( TSelf(cbDockPane) _obj, TRect(x,y,w,h));
void       cbDockPane_SetMargins( TSelf(cbDockPane) _obj, int top, int bottom, int left, int right );
void       cbDockPane_SetPaneWidth( TSelf(cbDockPane) _obj, int width );

/* cbDrawBarDecorEvent */
TClassDefExtend(cbDrawBarDecorEvent,cbPluginEvent)
void*      cbDrawBarDecorEvent_Bar( TSelf(cbDrawBarDecorEvent) _obj );
void       cbDrawBarDecorEvent_BoundsInParent( TSelf(cbDrawBarDecorEvent) _obj, TRectOutVoid(_x,_y,_w,_h) );
void*      cbDrawBarDecorEvent_Dc( TSelf(cbDrawBarDecorEvent) _obj );

/* cbDrawBarHandlesEvent */
TClassDefExtend(cbDrawBarHandlesEvent,cbPluginEvent)
void*      cbDrawBarHandlesEvent_Bar( TSelf(cbDrawBarHandlesEvent) _obj );
void*      cbDrawBarHandlesEvent_Dc( TSelf(cbDrawBarHandlesEvent) _obj );

/* cbDrawHintRectEvent */
TClassDefExtend(cbDrawHintRectEvent,cbPluginEvent)
int        cbDrawHintRectEvent_EraseRect( TSelf(cbDrawHintRectEvent) _obj );
TBool      cbDrawHintRectEvent_IsInClient( TSelf(cbDrawHintRectEvent) _obj );
int        cbDrawHintRectEvent_LastTime( TSelf(cbDrawHintRectEvent) _obj );
void       cbDrawHintRectEvent_Rect( TSelf(cbDrawHintRectEvent) _obj, TRectOutVoid(_x,_y,_w,_h) );

/* cbDrawPaneBkGroundEvent */
TClassDefExtend(cbDrawPaneBkGroundEvent,cbPluginEvent)
void*      cbDrawPaneBkGroundEvent_Dc( TSelf(cbDrawPaneBkGroundEvent) _obj );

/* cbDrawPaneDecorEvent */
TClassDefExtend(cbDrawPaneDecorEvent,cbPluginEvent)
void*      cbDrawPaneDecorEvent_Dc( TSelf(cbDrawPaneDecorEvent) _obj );

/* cbDrawRowBkGroundEvent */
TClassDefExtend(cbDrawRowBkGroundEvent,cbPluginEvent)
void*      cbDrawRowBkGroundEvent_Dc( TSelf(cbDrawRowBkGroundEvent) _obj );
void*      cbDrawRowBkGroundEvent_Row( TSelf(cbDrawRowBkGroundEvent) _obj );

/* cbDrawRowDecorEvent */
TClassDefExtend(cbDrawRowDecorEvent,cbPluginEvent)
void*      cbDrawRowDecorEvent_Dc( TSelf(cbDrawRowDecorEvent) _obj );
void*      cbDrawRowDecorEvent_Row( TSelf(cbDrawRowDecorEvent) _obj );

/* cbDrawRowHandlesEvent */
TClassDefExtend(cbDrawRowHandlesEvent,cbPluginEvent)
void*      cbDrawRowHandlesEvent_Dc( TSelf(cbDrawRowHandlesEvent) _obj );
void*      cbDrawRowHandlesEvent_Row( TSelf(cbDrawRowHandlesEvent) _obj );

/* cbDynToolBarDimHandler */
TClassDefExtend(cbDynToolBarDimHandler,cbDimHandlerBase)
TClass(cbDynToolBarDimHandler) cbDynToolBarDimHandler_Create(  );
void       cbDynToolBarDimHandler_Delete( TSelf(cbDynToolBarDimHandler) _obj );

/* cbFinishDrawInAreaEvent */
TClassDefExtend(cbFinishDrawInAreaEvent,cbPluginEvent)
void       cbFinishDrawInAreaEvent_Area( TSelf(cbFinishDrawInAreaEvent) _obj, TRectOutVoid(_x,_y,_w,_h) );

/* cbFloatedBarWindow */
TClassDefExtend(cbFloatedBarWindow,wxToolWindow)
TClass(cbFloatedBarWindow) cbFloatedBarWindow_Create( void* _obj );
void*      cbFloatedBarWindow_GetBar( TSelf(cbFloatedBarWindow) _obj );
void       cbFloatedBarWindow_PositionFloatedWnd( TSelf(cbFloatedBarWindow) _obj, TRect(_x,_y,_w,_h) );
void       cbFloatedBarWindow_SetBar( TSelf(cbFloatedBarWindow) _obj, void* _bar );
void       cbFloatedBarWindow_SetLayout( TSelf(cbFloatedBarWindow) _obj, void* _layout );

/* cbGCUpdatesMgr */
TClassDefExtend(cbGCUpdatesMgr,cbSimpleUpdatesMgr)
TClass(cbGCUpdatesMgr) cbGCUpdatesMgr_Create( void* pPanel );
TClass(cbGCUpdatesMgr) cbGCUpdatesMgr_CreateDefault(  );
void       cbGCUpdatesMgr_Delete( TSelf(cbGCUpdatesMgr) _obj );
void       cbGCUpdatesMgr_UpdateNow( TSelf(cbGCUpdatesMgr) _obj );

/* cbHintAnimationPlugin */
TClassDefExtend(cbHintAnimationPlugin,cbPluginBase)
TClass(cbHintAnimationPlugin) cbHintAnimationPlugin_Create( void* pPanel, int paneMask );
TClass(cbHintAnimationPlugin) cbHintAnimationPlugin_CreateDefault(  );
void       cbHintAnimationPlugin_Delete( TSelf(cbHintAnimationPlugin) _obj );

/* cbInsertBarEvent */
TClassDefExtend(cbInsertBarEvent,cbPluginEvent)
void*      cbInsertBarEvent_Bar( TSelf(cbInsertBarEvent) _obj );
void*      cbInsertBarEvent_Row( TSelf(cbInsertBarEvent) _obj );

/* cbLayoutRowEvent */
TClassDefExtend(cbLayoutRowEvent,cbPluginEvent)
void*      cbLayoutRowEvent_Row( TSelf(cbLayoutRowEvent) _obj );

/* cbLeftDClickEvent */
TClassDefExtend(cbLeftDClickEvent,cbPluginEvent)
void       cbLeftDClickEvent_Pos( TSelf(cbLeftDClickEvent) _obj, TPointOutVoid(_x,_y) );

/* cbLeftDownEvent */
TClassDefExtend(cbLeftDownEvent,cbPluginEvent)
void       cbLeftDownEvent_Pos( TSelf(cbLeftDownEvent) _obj, TPointOutVoid(_x,_y) );

/* cbLeftUpEvent */
TClassDefExtend(cbLeftUpEvent,cbPluginEvent)
void       cbLeftUpEvent_Pos( TSelf(cbLeftUpEvent) _obj, TPointOutVoid(_x,_y) );

/* cbMiniButton */
TClassDefExtend(cbMiniButton,wxObject)
TClass(cbMiniButton) cbMiniButton_Create(  );
void       cbMiniButton_Delete( TSelf(cbMiniButton) _obj );
void       cbMiniButton_Dim( TSelf(cbMiniButton) _obj, TSizeOutVoid(_w,_h) );
int        cbMiniButton_DragStarted( TSelf(cbMiniButton) _obj );
void       cbMiniButton_Enable( TSelf(cbMiniButton) _obj, TBool enable );
int        cbMiniButton_Enabled( TSelf(cbMiniButton) _obj );
int        cbMiniButton_HitTest( TSelf(cbMiniButton) _obj, TPoint(x,y) );
TBool      cbMiniButton_IsPressed( TSelf(cbMiniButton) _obj );
void*      cbMiniButton_Layout( TSelf(cbMiniButton) _obj );
void*      cbMiniButton_Pane( TSelf(cbMiniButton) _obj );
void*      cbMiniButton_Plugin( TSelf(cbMiniButton) _obj );
void       cbMiniButton_Pos( TSelf(cbMiniButton) _obj, TPointOutVoid(_x,_y) );
int        cbMiniButton_Pressed( TSelf(cbMiniButton) _obj );
void       cbMiniButton_Refresh( TSelf(cbMiniButton) _obj );
void       cbMiniButton_Reset( TSelf(cbMiniButton) _obj );
void       cbMiniButton_SetPos( TSelf(cbMiniButton) _obj, TPoint(x,y) );
int        cbMiniButton_Visible( TSelf(cbMiniButton) _obj );
int        cbMiniButton_WasClicked( TSelf(cbMiniButton) _obj );
void*      cbMiniButton_Wnd( TSelf(cbMiniButton) _obj );

/* cbMotionEvent */
TClassDefExtend(cbMotionEvent,cbPluginEvent)
void       cbMotionEvent_Pos( TSelf(cbMotionEvent) _obj, TPointOutVoid(_x,_y) );

/* cbPaneDrawPlugin */
TClassDefExtend(cbPaneDrawPlugin,cbPluginBase)
TClass(cbPaneDrawPlugin) cbPaneDrawPlugin_Create( void* pPanel, int paneMask );
TClass(cbPaneDrawPlugin) cbPaneDrawPlugin_CreateDefault(  );
void       cbPaneDrawPlugin_Delete( TSelf(cbPaneDrawPlugin) _obj );

/* cbPluginBase */
TClassDefExtend(cbPluginBase,wxEvtHandler)
void       cbPluginBase_Delete( TSelf(cbPluginBase) _obj );
int        cbPluginBase_GetPaneMask( TSelf(cbPluginBase) _obj );
TBool      cbPluginBase_IsReady( TSelf(cbPluginBase) _obj );
void*      cbPluginBase_Plugin( int _swt );
int        cbPluginBase_ProcessEvent( TSelf(cbPluginBase) _obj, TClass(wxEvent) event );

/* cbPluginEvent */
TClassDefExtend(cbPluginEvent,wxEvent)
void*      cbPluginEvent_Pane( TSelf(cbPluginEvent) _obj );

/* cbRemoveBarEvent */
TClassDefExtend(cbRemoveBarEvent,cbPluginEvent)
void*      cbRemoveBarEvent_Bar( TSelf(cbRemoveBarEvent) _obj );

/* cbResizeBarEvent */
TClassDefExtend(cbResizeBarEvent,cbPluginEvent)
void*      cbResizeBarEvent_Bar( TSelf(cbResizeBarEvent) _obj );
void*      cbResizeBarEvent_Row( TSelf(cbResizeBarEvent) _obj );

/* cbResizeRowEvent */
TClassDefExtend(cbResizeRowEvent,cbPluginEvent)
int        cbResizeRowEvent_ForUpperHandle( TSelf(cbResizeRowEvent) _obj );
int        cbResizeRowEvent_HandleOfs( TSelf(cbResizeRowEvent) _obj );
void*      cbResizeRowEvent_Row( TSelf(cbResizeRowEvent) _obj );

/* cbRightDownEvent */
TClassDefExtend(cbRightDownEvent,cbPluginEvent)
void       cbRightDownEvent_Pos( TSelf(cbRightDownEvent) _obj, TPointOutVoid(_x,_y) );

/* cbRightUpEvent */
TClassDefExtend(cbRightUpEvent,cbPluginEvent)
void       cbRightUpEvent_Pos( TSelf(cbRightUpEvent) _obj, TPointOutVoid(_x,_y) );

/* cbRowDragPlugin */
TClassDefExtend(cbRowDragPlugin,cbPluginBase)
TClass(cbRowDragPlugin) cbRowDragPlugin_Create( void* pPanel, int paneMask );
TClass(cbRowDragPlugin) cbRowDragPlugin_CreateDefault(  );
void       cbRowDragPlugin_Delete( TSelf(cbRowDragPlugin) _obj );

/* cbRowInfo */
TClassDefExtend(cbRowInfo,wxObject)
TClass(cbRowInfo) cbRowInfo_Create(  );
void       cbRowInfo_Delete( TSelf(cbRowInfo) _obj );
void*      cbRowInfo_GetFirstBar( TSelf(cbRowInfo) _obj );

/* cbRowLayoutPlugin */
TClassDefExtend(cbRowLayoutPlugin,cbPluginBase)
TClass(cbRowLayoutPlugin) cbRowLayoutPlugin_Create( void* pPanel, int paneMask );
TClass(cbRowLayoutPlugin) cbRowLayoutPlugin_CreateDefault(  );
void       cbRowLayoutPlugin_Delete( TSelf(cbRowLayoutPlugin) _obj );

/* cbSimpleCustomizationPlugin */
TClassDefExtend(cbSimpleCustomizationPlugin,cbPluginBase)
TClass(cbSimpleCustomizationPlugin) cbSimpleCustomizationPlugin_Create( void* pPanel, int paneMask );
TClass(cbSimpleCustomizationPlugin) cbSimpleCustomizationPlugin_CreateDefault(  );
void       cbSimpleCustomizationPlugin_Delete( TSelf(cbSimpleCustomizationPlugin) _obj );

/* cbSimpleUpdatesMgr */
TClassDefExtend(cbSimpleUpdatesMgr,cbUpdatesManagerBase)

/* cbSizeBarWndEvent */
TClassDefExtend(cbSizeBarWndEvent,cbPluginEvent)
void*      cbSizeBarWndEvent_Bar( TSelf(cbSizeBarWndEvent) _obj );
void       cbSizeBarWndEvent_BoundsInParent( TSelf(cbSizeBarWndEvent) _obj, TRectOutVoid(_x,_y,_w,_h) );

/* cbStartBarDraggingEvent */
TClassDefExtend(cbStartBarDraggingEvent,cbPluginEvent)
void*      cbStartBarDraggingEvent_Bar( TSelf(cbStartBarDraggingEvent) _obj );
void       cbStartBarDraggingEvent_Pos( TSelf(cbStartBarDraggingEvent) _obj, TPointOutVoid(_x,_y) );

/* cbStartDrawInAreaEvent */
TClassDefExtend(cbStartDrawInAreaEvent,cbPluginEvent)
void       cbStartDrawInAreaEvent_Area( TSelf(cbStartDrawInAreaEvent) _obj, TRectOutVoid(_x,_y,_w,_h) );

/* cbUpdatesManagerBase */
TClassDefExtend(cbUpdatesManagerBase,wxObject)

/* wxAcceleratorEntry */
TClassDef(wxAcceleratorEntry)
TClass(wxAcceleratorEntry) wxAcceleratorEntry_Create( int flags, int keyCode, int cmd );
void       wxAcceleratorEntry_Delete( TSelf(wxAcceleratorEntry) _obj );
int        wxAcceleratorEntry_GetCommand( TSelf(wxAcceleratorEntry) _obj );
int        wxAcceleratorEntry_GetFlags( TSelf(wxAcceleratorEntry) _obj );
int        wxAcceleratorEntry_GetKeyCode( TSelf(wxAcceleratorEntry) _obj );
void       wxAcceleratorEntry_Set( TSelf(wxAcceleratorEntry) _obj, int flags, int keyCode, int cmd );

/* wxAcceleratorTable */
TClassDef(wxAcceleratorTable)
TClass(wxAcceleratorTable) wxAcceleratorTable_Create( int n, void* entries );
void       wxAcceleratorTable_Delete( TSelf(wxAcceleratorTable) _obj );

/* wxActivateEvent */
TClassDefExtend(wxActivateEvent,wxEvent)
void       wxActivateEvent_CopyObject( TSelf(wxActivateEvent) _obj, void* obj );
TBool      wxActivateEvent_GetActive( TSelf(wxActivateEvent) _obj );

/* wxApp */
TClassDefExtend(wxApp,wxEvtHandler)

/* wxArray */
TClassDef(wxArray)

/* wxArrayString */
TClassDefExtend(wxArrayString,wxArray)

/* wxArtProvider */
TClassDefExtend(wxArtProvider,wxObject)
TBool      PopProvider(  );
void       PushProvider( TClass(wxArtProvider) provider );
TBool      RemoveProvider( TClass(wxArtProvider) provider );

/* wxAuiDefaultTabArt */
TClassDefExtend(wxAuiDefaultTabArt, wxAuiTabArt)
TClass(wxAuiDefaultTabArt)    wxAuiDefaultTabArt_Create();
TClass(wxAuiTabArt) 	wxAuiDefaultTabArt_Clone (TSelf(wxAuiDefaultTabArt) _obj );
void 	wxAuiDefaultTabArt_SetFlags (TSelf(wxAuiDefaultTabArt) _obj, int _flags );
void 	wxAuiDefaultTabArt_SetSizingInfo (TSelf(wxAuiDefaultTabArt) _obj, TSize(_width,_height), size_t tabCount );
void 	wxAuiDefaultTabArt_SetNormalFont (TSelf(wxAuiDefaultTabArt) _obj, TClass(wxFont) _font );
void 	wxAuiDefaultTabArt_SetSelectedFont (TSelf(wxAuiDefaultTabArt) _obj, TClass(wxFont) _font );
void 	wxAuiDefaultTabArt_SetMeasuringFont (TSelf(wxAuiDefaultTabArt) _obj, TClass(wxFont) _font );
void 	wxAuiDefaultTabArt_SetColour (TSelf(wxAuiDefaultTabArt) _obj, TClass(wxColour) _colour );
void 	wxAuiDefaultTabArt_SetActiveColour (TSelf(wxAuiDefaultTabArt) _obj, TClass(wxColour) _colour );
void 	wxAuiDefaultTabArt_DrawBackground (TSelf(wxAuiDefaultTabArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _rect );
void 	wxAuiDefaultTabArt_DrawTab (TSelf(wxAuiDefaultTabArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxAuiNotebookPage) _pane, TClass(wxRect) _inRect, int closeButtonState, TClass(wxRect) _outTabRect, TClass(wxRect) _outButtonRect, int *xExtent );
void 	wxAuiDefaultTabArt_DrawButton (TSelf(wxAuiDefaultTabArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _inRect, int bitmapId, int buttonState, int orientation, TClass(wxRect) _outRect );
int 	wxAuiDefaultTabArt_GetIndentSize (TSelf(wxAuiDefaultTabArt) _obj );
TClass(wxSize) 	wxAuiDefaultTabArt_GetTabSize (TSelf(wxAuiDefaultTabArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxString) _caption, TClass(wxBitmap) _bitmap, TBool active, int closeButtonState, int *xExtent );
int 	wxAuiDefaultTabArt_ShowDropDown (TSelf(wxAuiDefaultTabArt) _obj, TClass(wxWindow) _wnd, TClass(wxAuiNotebookPageArray) _items, int activeIdx );
int 	wxAuiDefaultTabArt_GetBestTabCtrlSize (TSelf(wxAuiDefaultTabArt) _obj, TClass(wxWindow) _wnd, TClass(wxAuiNotebookPageArray) _pages, TSize(_width,_height));

/* wxAuiToolBarEvent */
TClassDefExtend(wxAuiToolBarEvent, wxNotifyEvent)
TBool 	wxAuiToolBarEvent_IsDropDownClicked (TSelf(wxAuiToolBarEvent) _obj );
TClass(wxPoint) 	wxAuiToolBarEvent_GetClickPoint (TSelf(wxAuiToolBarEvent) _obj );
TClass(wxRect) 	wxAuiToolBarEvent_GetItemRect (TSelf(wxAuiToolBarEvent) _obj );
int 	wxAuiToolBarEvent_GetToolId (TSelf(wxAuiToolBarEvent) _obj );

/* wxAuiToolBarItem */
TClassDef(wxAuiToolBarItem)
TClass(wxAuiToolBarItem)  wxAuiToolBarItem_CreateDefault();
TClass(wxAuiToolBarItem)  wxAuiToolBarItem_Create(TClass(wxAuiToolBarItem) _c );
TClass(wxAuiToolBarItem) wxAuiToolBarItem_Copy (TSelf(wxAuiToolBarItem) _obj, TClass(wxAuiToolBarItem) _c);
void 	wxAuiToolBarItem_Assign (TSelf(wxAuiToolBarItem) _obj, TClass(wxAuiToolBarItem) _c );
void 	wxAuiToolBarItem_SetWindow (TSelf(wxAuiToolBarItem) _obj, TClass(wxWindow) _w );
TClass(wxWindow) 	wxAuiToolBarItem_GetWindow (TSelf(wxAuiToolBarItem) _obj );
void 	wxAuiToolBarItem_SetId (TSelf(wxAuiToolBarItem) _obj, int new_id );
int 	wxAuiToolBarItem_GetId (TSelf(wxAuiToolBarItem) _obj );
void 	wxAuiToolBarItem_SetKind (TSelf(wxAuiToolBarItem) _obj, int new_kind );
int 	wxAuiToolBarItem_GetKind (TSelf(wxAuiToolBarItem) _obj );
void 	wxAuiToolBarItem_SetState (TSelf(wxAuiToolBarItem) _obj, int new_state );
int 	wxAuiToolBarItem_GetState (TSelf(wxAuiToolBarItem) _obj );
void 	wxAuiToolBarItem_SetSizerItem (TSelf(wxAuiToolBarItem) _obj, TClass(wxSizerItem) _s );
TClass(wxSizerItem) 	wxAuiToolBarItem_GetSizerItem (TSelf(wxAuiToolBarItem) _obj );
void 	wxAuiToolBarItem_SetLabel (TSelf(wxAuiToolBarItem) _obj, TClass(wxString) _s );
TClass(wxString) 	wxAuiToolBarItem_GetLabel (TSelf(wxAuiToolBarItem) _obj );
void 	wxAuiToolBarItem_SetBitmap (TSelf(wxAuiToolBarItem) _obj, TClass(wxBitmap) _bmp );
void 	wxAuiToolBarItem_GetBitmap (TSelf(wxAuiToolBarItem) _obj, TClassRef(wxBitmap) _ref );
void 	wxAuiToolBarItem_SetDisabledBitmap (TSelf(wxAuiToolBarItem) _obj, TClass(wxBitmap) _bmp );
void 	wxAuiToolBarItem_GetDisabledBitmap (TSelf(wxAuiToolBarItem) _obj, TClassRef(wxBitmap) _ref );
void 	wxAuiToolBarItem_SetHoverBitmap (TSelf(wxAuiToolBarItem) _obj, TClass(wxBitmap) _bmp );
void 	wxAuiToolBarItem_GetHoverBitmap (TSelf(wxAuiToolBarItem) _obj, TClassRef(wxBitmap) _ref );
void 	wxAuiToolBarItem_SetShortHelp (TSelf(wxAuiToolBarItem) _obj, TClass(wxString) _s );
TClass(wxString) 	wxAuiToolBarItem_GetShortHelp (TSelf(wxAuiToolBarItem) _obj );
void 	wxAuiToolBarItem_SetLongHelp (TSelf(wxAuiToolBarItem) _obj, TClass(wxString) _s );
TClass(wxString) 	wxAuiToolBarItem_GetLongHelp (TSelf(wxAuiToolBarItem) _obj );
void 	wxAuiToolBarItem_SetMinSize (TSelf(wxAuiToolBarItem) _obj, TSize(_width,_height));
TClass(wxSize) 	wxAuiToolBarItem_GetMinSize (TSelf(wxAuiToolBarItem) _obj );
void 	wxAuiToolBarItem_SetSpacerPixels (TSelf(wxAuiToolBarItem) _obj, int s );
int 	wxAuiToolBarItem_GetSpacerPixels (TSelf(wxAuiToolBarItem) _obj );
void 	wxAuiToolBarItem_SetProportion (TSelf(wxAuiToolBarItem) _obj, int p );
int 	wxAuiToolBarItem_GetProportion (TSelf(wxAuiToolBarItem) _obj );
void 	wxAuiToolBarItem_SetActive (TSelf(wxAuiToolBarItem) _obj, TBool b );
TBool 	wxAuiToolBarItem_IsActive (TSelf(wxAuiToolBarItem) _obj );
void 	wxAuiToolBarItem_SetHasDropDown (TSelf(wxAuiToolBarItem) _obj, TBool b );
TBool 	wxAuiToolBarItem_HasDropDown (TSelf(wxAuiToolBarItem) _obj );
void 	wxAuiToolBarItem_SetSticky (TSelf(wxAuiToolBarItem) _obj, TBool b );
TBool 	wxAuiToolBarItem_IsSticky (TSelf(wxAuiToolBarItem) _obj );
void 	wxAuiToolBarItem_SetUserData (TSelf(wxAuiToolBarItem) _obj, long l );
long 	wxAuiToolBarItem_GetUserData (TSelf(wxAuiToolBarItem) _obj );
void 	wxAuiToolBarItem_SetAlignment (TSelf(wxAuiToolBarItem) _obj, int l );
int 	wxAuiToolBarItem_GetAlignment (TSelf(wxAuiToolBarItem) _obj );

/* wxAuiToolBarArt */
TClassDef(wxAuiToolBarArt)
TClass(wxAuiToolBarArt) 	 wxAuiToolBarArt_Clone (TSelf(wxAuiToolBarArt) _obj );
void 	 wxAuiToolBarArt_SetFlags (TSelf(wxAuiToolBarArt) _obj, int _flags );
int 	 wxAuiToolBarArt_GetFlags (TSelf(wxAuiToolBarArt) _obj );
void 	 wxAuiToolBarArt_SetFont (TSelf(wxAuiToolBarArt) _obj, TClass(wxFont) _font );
TClass(wxFont) 	 wxAuiToolBarArt_GetFont (TSelf(wxAuiToolBarArt) _obj );
void 	 wxAuiToolBarArt_SetTextOrientation (TSelf(wxAuiToolBarArt) _obj, int orientation );
int 	 wxAuiToolBarArt_GetTextOrientation (TSelf(wxAuiToolBarArt) _obj );
void 	 wxAuiToolBarArt_DrawBackground (TSelf(wxAuiToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _rect );
void 	 wxAuiToolBarArt_DrawPlainBackground (TSelf(wxAuiToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _rect );
void 	 wxAuiToolBarArt_DrawLabel (TSelf(wxAuiToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxAuiToolBarItem) _item, TClass(wxRect) _rect );
void 	 wxAuiToolBarArt_DrawButton (TSelf(wxAuiToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxAuiToolBarItem) _item, TClass(wxRect) _rect );
void 	 wxAuiToolBarArt_DrawDropDownButton (TSelf(wxAuiToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxAuiToolBarItem) _item, TClass(wxRect) _rect );
void 	 wxAuiToolBarArt_DrawControlLabel (TSelf(wxAuiToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxAuiToolBarItem) _item, TClass(wxRect) _rect );
void 	 wxAuiToolBarArt_DrawSeparator (TSelf(wxAuiToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _rect );
void 	 wxAuiToolBarArt_DrawGripper (TSelf(wxAuiToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _rect );
void 	 wxAuiToolBarArt_DrawOverflowButton (TSelf(wxAuiToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _rect, int state );
TClass(wxSize) 	 wxAuiToolBarArt_GetLabelSize (TSelf(wxAuiToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxAuiToolBarItem) _item );
TClass(wxSize) 	 wxAuiToolBarArt_GetToolSize (TSelf(wxAuiToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxAuiToolBarItem) _item );
int 	 wxAuiToolBarArt_GetElementSize (TSelf(wxAuiToolBarArt) _obj, int element_id );
void 	 wxAuiToolBarArt_SetElementSize (TSelf(wxAuiToolBarArt) _obj, int element_id, int size );
int 	 wxAuiToolBarArt_ShowDropDown (TSelf(wxAuiToolBarArt) _obj, TClass(wxWindow) _wnd, TClass(wxAuiToolBarItemArray) _items );

/* wxAuiDefaultToolBarArt */
TClassDefExtend(wxAuiDefaultToolBarArt, wxAuiToolBarArt)
TClass(wxAuiDefaultToolBarArt)  wxAuiDefaultToolBarArt_Create();
TClass(wxAuiToolBarArt) 	 wxAuiDefaultToolBarArt_Clone (TSelf(wxAuiDefaultToolBarArt) _obj );
void 	 wxAuiDefaultToolBarArt_SetFlags (TSelf(wxAuiDefaultToolBarArt) _obj, int _flags );
int 	 wxAuiDefaultToolBarArt_GetFlags (TSelf(wxAuiDefaultToolBarArt) _obj );
void 	 wxAuiDefaultToolBarArt_SetFont (TSelf(wxAuiDefaultToolBarArt) _obj, TClass(wxFont) _font );
TClass(wxFont) 	 wxAuiDefaultToolBarArt_GetFont (TSelf(wxAuiDefaultToolBarArt) _obj );
void 	 wxAuiDefaultToolBarArt_SetTextOrientation (TSelf(wxAuiDefaultToolBarArt) _obj, int orientation );
int 	 wxAuiDefaultToolBarArt_GetTextOrientation (TSelf(wxAuiDefaultToolBarArt) _obj );
void 	 wxAuiDefaultToolBarArt_DrawBackground (TSelf(wxAuiDefaultToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _rect );
void 	 wxAuiDefaultToolBarArt_DrawPlainBackground (TSelf(wxAuiDefaultToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _rect );
void 	 wxAuiDefaultToolBarArt_DrawLabel (TSelf(wxAuiDefaultToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxAuiToolBarItem) _item, TClass(wxRect) _rect );
void 	 wxAuiDefaultToolBarArt_DrawButton (TSelf(wxAuiDefaultToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxAuiToolBarItem) _item, TClass(wxRect) _rect );
void 	 wxAuiDefaultToolBarArt_DrawDropDownButton (TSelf(wxAuiDefaultToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxAuiToolBarItem) _item, TClass(wxRect) _rect );
void 	 wxAuiDefaultToolBarArt_DrawControlLabel (TSelf(wxAuiDefaultToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxAuiToolBarItem) _item, TClass(wxRect) _rect );
void 	 wxAuiDefaultToolBarArt_DrawSeparator (TSelf(wxAuiDefaultToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _rect );
void 	 wxAuiDefaultToolBarArt_DrawGripper (TSelf(wxAuiDefaultToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _rect );
void 	 wxAuiDefaultToolBarArt_DrawOverflowButton (TSelf(wxAuiDefaultToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _rect, int state );
TClass(wxSize) 	 wxAuiDefaultToolBarArt_GetLabelSize (TSelf(wxAuiDefaultToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxAuiToolBarItem) _item );
TClass(wxSize) 	 wxAuiDefaultToolBarArt_GetToolSize (TSelf(wxAuiDefaultToolBarArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxAuiToolBarItem) _item );
int 	 wxAuiDefaultToolBarArt_GetElementSize (TSelf(wxAuiDefaultToolBarArt) _obj, int element );
void 	 wxAuiDefaultToolBarArt_SetElementSize (TSelf(wxAuiDefaultToolBarArt) _obj, int element_id, int size );
int 	 wxAuiDefaultToolBarArt_ShowDropDown (TSelf(wxAuiDefaultToolBarArt) _obj, TClass(wxWindow) _wnd, TClass(wxAuiToolBarItemArray) _items );

/* wxAuiToolBar */
TClassDefExtend(wxAuiToolBar, wxControl)
TClass(wxAuiToolBar)  wxAuiToolBar_CreateDefault();
TClass(wxAuiToolBar)  wxAuiToolBar_Create(TClass(wxWindow) _parent, int id, TPoint(x,y), TSize(_width,_height), long style );
TBool 	 wxAuiToolBar_CreateFromDefault (TSelf(wxAuiToolBar) _obj, TClass(wxWindow) _parent, int id, TPoint(x,y), TSize(_width,_height), long style);
void     wxAuiToolBar_Delete(TSelf(wxAuiToolBar) _obj );
void 	 wxAuiToolBar_SetWindowStyleFlag (TSelf(wxAuiToolBar) _obj, long style );
long 	 wxAuiToolBar_GetWindowStyleFlag (TSelf(wxAuiToolBar) _obj );
void 	 wxAuiToolBar_SetArtProvider (TSelf(wxAuiToolBar) _obj, TClass(wxAuiToolBarArt) _art );
TClass(wxAuiToolBarArt) 	 wxAuiToolBar_GetArtProvider (TSelf(wxAuiToolBar) _obj );
TBool 	 wxAuiToolBar_SetFont (TSelf(wxAuiToolBar) _obj, TClass(wxFont) _font );
TClass(wxAuiToolBarItem) 	 wxAuiToolBar_AddToolByLabel (TSelf(wxAuiToolBar) _obj, int tool_id, TClass(wxString) _label, TClass(wxBitmap) _bitmap, TClass(wxString) _short_help_string, int kind );
TClass(wxAuiToolBarItem) 	 wxAuiToolBar_AddTool (TSelf(wxAuiToolBar) _obj, int tool_id, TClass(wxString) _label, TClass(wxBitmap) _bitmap, TClass(wxBitmap) _disabled_bitmap, int kind, TClass(wxString) _short_help_string, TClass(wxString) _long_help_string, TClass(wxObject) _client_data );
TClass(wxAuiToolBarItem) 	 wxAuiToolBar_AddToolByBitmap (TSelf(wxAuiToolBar) _obj, int tool_id, TClass(wxBitmap) _bitmap, TClass(wxBitmap) _disabled_bitmap, TBool toggle, TClass(wxObject) _client_data, TClass(wxString) _short_help_string, TClass(wxString) _long_help_string );
TClass(wxAuiToolBarItem) 	 wxAuiToolBar_AddLabel (TSelf(wxAuiToolBar) _obj, int tool_id, TClass(wxString) _label, int width );
TClass(wxAuiToolBarItem) 	 wxAuiToolBar_AddControl (TSelf(wxAuiToolBar) _obj, TClass(wxControl) _control, TClass(wxString) _label );
TClass(wxAuiToolBarItem) 	 wxAuiToolBar_AddSeparator (TSelf(wxAuiToolBar) _obj );
TClass(wxAuiToolBarItem) 	 wxAuiToolBar_AddSpacer (TSelf(wxAuiToolBar) _obj, int pixels );
TClass(wxAuiToolBarItem) 	 wxAuiToolBar_AddStretchSpacer (TSelf(wxAuiToolBar) _obj, int proportion );
TBool 	 wxAuiToolBar_Realize (TSelf(wxAuiToolBar) _obj );
TClass(wxControl) 	 wxAuiToolBar_FindControl (TSelf(wxAuiToolBar) _obj, int window_id );
TClass(wxAuiToolBarItem) 	 wxAuiToolBar_FindToolByPosition (TSelf(wxAuiToolBar) _obj, int x, int y );
TClass(wxAuiToolBarItem) 	 wxAuiToolBar_FindToolByIndex (TSelf(wxAuiToolBar) _obj, int idx );
TClass(wxAuiToolBarItem) 	 wxAuiToolBar_FindTool (TSelf(wxAuiToolBar) _obj, int tool_id );
void 	 wxAuiToolBar_ClearTools (TSelf(wxAuiToolBar) _obj );
void 	 wxAuiToolBar_Clear (TSelf(wxAuiToolBar) _obj );
TBool 	 wxAuiToolBar_DeleteTool (TSelf(wxAuiToolBar) _obj, int tool_id );
TBool 	 wxAuiToolBar_DeleteByIndex (TSelf(wxAuiToolBar) _obj, int tool_id );
size_t 	 wxAuiToolBar_GetToolCount (TSelf(wxAuiToolBar) _obj );
int 	 wxAuiToolBar_GetToolPos (TSelf(wxAuiToolBar) _obj, int tool_id );
int 	 wxAuiToolBar_GetToolIndex (TSelf(wxAuiToolBar) _obj, int tool_id );
TBool 	 wxAuiToolBar_GetToolFits (TSelf(wxAuiToolBar) _obj, int tool_id );
TClass(wxRect) 	 wxAuiToolBar_GetToolRect (TSelf(wxAuiToolBar) _obj, int tool_id );
TBool 	 wxAuiToolBar_GetToolFitsByIndex (TSelf(wxAuiToolBar) _obj, int tool_id );
TBool 	 wxAuiToolBar_GetToolBarFits (TSelf(wxAuiToolBar) _obj );
void 	 wxAuiToolBar_SetMargins (TSelf(wxAuiToolBar) _obj, TSize(_width,_height));
void 	 wxAuiToolBar_SetMarginsXY (TSelf(wxAuiToolBar) _obj, int x, int y );
void 	 wxAuiToolBar_SetMarginsDetailed (TSelf(wxAuiToolBar) _obj, int left, int right, int top, int bottom );
void 	 wxAuiToolBar_SetToolBitmapSize (TSelf(wxAuiToolBar) _obj, TSize(_width,_height));
TClass(wxSize) 	 wxAuiToolBar_GetToolBitmapSize (TSelf(wxAuiToolBar) _obj );
TBool 	 wxAuiToolBar_GetOverflowVisible (TSelf(wxAuiToolBar) _obj );
void 	 wxAuiToolBar_SetOverflowVisible (TSelf(wxAuiToolBar) _obj, TBool visible );
TBool 	 wxAuiToolBar_GetGripperVisible (TSelf(wxAuiToolBar) _obj );
void 	 wxAuiToolBar_SetGripperVisible (TSelf(wxAuiToolBar) _obj, TBool visible );
void 	 wxAuiToolBar_ToggleTool (TSelf(wxAuiToolBar) _obj, int tool_id, TBool state );
TBool 	 wxAuiToolBar_GetToolToggled (TSelf(wxAuiToolBar) _obj, int tool_id );
void 	 wxAuiToolBar_EnableTool (TSelf(wxAuiToolBar) _obj, int tool_id, TBool state );
TBool 	 wxAuiToolBar_GetToolEnabled (TSelf(wxAuiToolBar) _obj, int tool_id );
void 	 wxAuiToolBar_SetToolDropDown (TSelf(wxAuiToolBar) _obj, int tool_id, TBool dropdown );
TBool 	 wxAuiToolBar_GetToolDropDown (TSelf(wxAuiToolBar) _obj, int tool_id );
void 	 wxAuiToolBar_SetToolBorderPadding (TSelf(wxAuiToolBar) _obj, int padding );
int 	 wxAuiToolBar_GetToolBorderPadding (TSelf(wxAuiToolBar) _obj );
void 	 wxAuiToolBar_SetToolTextOrientation (TSelf(wxAuiToolBar) _obj, int orientation );
int 	 wxAuiToolBar_GetToolTextOrientation (TSelf(wxAuiToolBar) _obj );
void 	 wxAuiToolBar_SetToolPacking (TSelf(wxAuiToolBar) _obj, int packing );
int 	 wxAuiToolBar_GetToolPacking (TSelf(wxAuiToolBar) _obj );
void 	 wxAuiToolBar_SetToolProportion (TSelf(wxAuiToolBar) _obj, int tool_id, int proportion );
int 	 wxAuiToolBar_GetToolProportion (TSelf(wxAuiToolBar) _obj, int tool_id );
void 	 wxAuiToolBar_SetToolSeparation (TSelf(wxAuiToolBar) _obj, int separation );
int 	 wxAuiToolBar_GetToolSeparation (TSelf(wxAuiToolBar) _obj );
void 	 wxAuiToolBar_SetToolSticky (TSelf(wxAuiToolBar) _obj, int tool_id, TBool sticky );
TBool 	 wxAuiToolBar_GetToolSticky (TSelf(wxAuiToolBar) _obj, int tool_id );
TClass(wxString) 	 wxAuiToolBar_GetToolLabel (TSelf(wxAuiToolBar) _obj, int tool_id );
void 	 wxAuiToolBar_SetToolLabel (TSelf(wxAuiToolBar) _obj, int tool_id, TClass(wxString) _label );
void 	 wxAuiToolBar_GetToolBitmap (TSelf(wxAuiToolBar) _obj, int tool_id, TClassRef(wxBitmap) _bmp );
void 	 wxAuiToolBar_SetToolBitmap (TSelf(wxAuiToolBar) _obj, int tool_id, TClass(wxBitmap) _bitmap );
TClass(wxString) 	 wxAuiToolBar_GetToolShortHelp (TSelf(wxAuiToolBar) _obj, int tool_id );
void 	 wxAuiToolBar_SetToolShortHelp (TSelf(wxAuiToolBar) _obj, int tool_id, TClass(wxString) _help_string );
TClass(wxString) 	 wxAuiToolBar_GetToolLongHelp (TSelf(wxAuiToolBar) _obj, int tool_id );
void 	 wxAuiToolBar_SetToolLongHelp (TSelf(wxAuiToolBar) _obj, int tool_id, TClass(wxString) _help_string );
void 	 wxAuiToolBar_SetCustomOverflowItems (TSelf(wxAuiToolBar) _obj, TClass(wxAuiToolBarItemArray) _prepend, TClass(wxAuiToolBarItemArray) _append );
TClass(wxSize) 	 wxAuiToolBar_GetHintSize (TSelf(wxAuiToolBar) _obj, int dock_direction);
TBool 	 wxAuiToolBar_IsPaneValid (TSelf(wxAuiToolBar) _obj, TClass(wxAuiPaneInfo) _pane );

/* wxAuiNotebook */
TClassDefExtend(wxAuiNotebook, wxBookCtrlBase)
TClass(wxAuiNotebook)   wxAuiNotebook_CreateDefault();
TClass(wxAuiNotebook)   wxAuiNotebook_Create(TClass(wxWindow) _parent, int id, TPoint(x,y), TSize(_width,_height), long style );
TBool 	 wxAuiNotebook_CreateFromDefault (TSelf(wxAuiNotebook) _obj, TClass(wxWindow) _parent, int id, TPoint(x,y), TSize(_width,_height), long style);
TBool 	 wxAuiNotebook_AddPageWithBitmap (TSelf(wxAuiNotebook) _obj, TClass(wxWindow) _page, TClass(wxString) _caption, TBool select, TClass(wxBitmap) _bitmap);
TBool 	 wxAuiNotebook_AddPage (TSelf(wxAuiNotebook) _obj, TClass(wxWindow) _page, TClass(wxString) _text, TBool select, int imageId);
void 	 wxAuiNotebook_AdvanceSelection (TSelf(wxAuiNotebook) _obj, TBool forward);
int 	 wxAuiNotebook_ChangeSelection (TSelf(wxAuiNotebook) _obj, size_t n);
TBool 	 wxAuiNotebook_DeleteAllPages (TSelf(wxAuiNotebook) _obj );
TBool 	 wxAuiNotebook_DeletePage (TSelf(wxAuiNotebook) _obj, size_t page );
TClass(wxAuiTabArt) 	 wxAuiNotebook_GetArtProvider (TSelf(wxAuiNotebook) _obj );
TClass(wxWindow) 	 wxAuiNotebook_GetCurrentPage (TSelf(wxAuiNotebook) _obj );
int 	 wxAuiNotebook_GetHeightForPageHeight (TSelf(wxAuiNotebook) _obj, int pageHeight );
TClass(wxWindow) 	 wxAuiNotebook_GetPage (TSelf(wxAuiNotebook) _obj, size_t page_idx );
void                 wxAuiNotebook_GetPageBitmap (TSelf(wxAuiNotebook) _obj, size_t page, TClassRef(wxBitmap) bmp );
size_t 	 wxAuiNotebook_GetPageCount (TSelf(wxAuiNotebook) _obj );
int 	 wxAuiNotebook_GetPageIndex (TSelf(wxAuiNotebook) _obj, TClass(wxWindow) _page_wnd );
TClass(wxString) 	 wxAuiNotebook_GetPageText (TSelf(wxAuiNotebook) _obj, size_t page );
TClass(wxString) 	 wxAuiNotebook_GetPageToolTip (TSelf(wxAuiNotebook) _obj, size_t pageIdx );
int 	 wxAuiNotebook_GetSelection (TSelf(wxAuiNotebook) _obj );
int 	 wxAuiNotebook_GetTabCtrlHeight (TSelf(wxAuiNotebook) _obj );
TBool 	 wxAuiNotebook_InsertPageWithBitmap (TSelf(wxAuiNotebook) _obj, size_t page_idx, TClass(wxWindow) _page, TClass(wxString) _caption, TBool select, TClass(wxBitmap) _bitmap );
TBool 	 wxAuiNotebook_InsertPage (TSelf(wxAuiNotebook) _obj, size_t index, TClass(wxWindow) _page, TClass(wxString) _text, TBool select, int imageId );
TBool 	 wxAuiNotebook_RemovePage (TSelf(wxAuiNotebook) _obj, size_t page );
void 	 wxAuiNotebook_SetArtProvider (TSelf(wxAuiNotebook) _obj, TClass(wxAuiTabArt) _art );
TBool 	 wxAuiNotebook_SetFont (TSelf(wxAuiNotebook) _obj, TClass(wxFont) _font );
void 	 wxAuiNotebook_SetMeasuringFont (TSelf(wxAuiNotebook) _obj, TClass(wxFont) _font );
void 	 wxAuiNotebook_SetNormalFont (TSelf(wxAuiNotebook) _obj, TClass(wxFont) _font );
TBool 	 wxAuiNotebook_SetPageBitmap (TSelf(wxAuiNotebook) _obj, size_t page, TClass(wxBitmap) _bitmap );
TBool 	 wxAuiNotebook_SetPageImage (TSelf(wxAuiNotebook) _obj, size_t n, int imageId );
TBool 	 wxAuiNotebook_SetPageText (TSelf(wxAuiNotebook) _obj, size_t page, TClass(wxString) _text );
TBool 	 wxAuiNotebook_SetPageToolTip (TSelf(wxAuiNotebook) _obj, size_t page, TClass(wxString) _text );
void 	 wxAuiNotebook_SetSelectedFont (TSelf(wxAuiNotebook) _obj, TClass(wxFont) _font );
size_t 	 wxAuiNotebook_SetSelection (TSelf(wxAuiNotebook) _obj, size_t new_page );
void 	 wxAuiNotebook_SetTabCtrlHeight (TSelf(wxAuiNotebook) _obj, int height );
TBool 	 wxAuiNotebook_ShowWindowMenu (TSelf(wxAuiNotebook) _obj );
void 	 wxAuiNotebook_SetUniformBitmapSize (TSelf(wxAuiNotebook) _obj, TSize(_width,_height));
void 	 wxAuiNotebook_Split (TSelf(wxAuiNotebook) _obj, size_t page, int direction );

/* wxAuiTabContainerButton */
TClassDef(wxAuiTabContainerButton)
int 	wxAuiTabContainerButton_Id(TSelf(wxAuiTabContainerButton) _obj );
int 	wxAuiTabContainerButton_CurState(TSelf(wxAuiTabContainerButton) _obj );
int 	wxAuiTabContainerButton_Location(TSelf(wxAuiTabContainerButton) _obj );
void 	wxAuiTabContainerButton_Bitmap(TSelf(wxAuiTabContainerButton) _obj, TClassRef(wxBitmap) _bmp );
void 	wxAuiTabContainerButton_DisBitmap(TSelf(wxAuiTabContainerButton) _obj, TClassRef(wxBitmap) _bmp );
TClass(wxRect) 	wxAuiTabContainerButton_Rect(TSelf(wxAuiTabContainerButton) _obj );


/* wxAuiTabContainer */
TClassDef(wxAuiTabContainer)
TClass(wxAuiTabContainer)  wxAuiTabContainer_Create();
void 	 wxAuiTabContainer_SetArtProvider (TSelf(wxAuiTabContainer) _obj, TClass(wxAuiTabArt) _art );
TClass(wxAuiTabArt) 	 wxAuiTabContainer_GetArtProvider (TSelf(wxAuiTabContainer) _obj );
void 	 wxAuiTabContainer_SetFlags (TSelf(wxAuiTabContainer) _obj, int _flags );
int 	 wxAuiTabContainer_GetFlags (TSelf(wxAuiTabContainer) _obj );
TBool 	 wxAuiTabContainer_AddPage (TSelf(wxAuiTabContainer) _obj, TClass(wxWindow) _page, TClass(wxAuiNotebookPage) _info );
TBool 	 wxAuiTabContainer_InsertPage (TSelf(wxAuiTabContainer) _obj, TClass(wxWindow) _page, TClass(wxAuiNotebookPage) _info, size_t idx );
TBool 	 wxAuiTabContainer_MovePage (TSelf(wxAuiTabContainer) _obj, TClass(wxWindow) _page, size_t newIdx );
TBool 	 wxAuiTabContainer_RemovePage (TSelf(wxAuiTabContainer) _obj, TClass(wxWindow) _page );
TBool 	 wxAuiTabContainer_SetActivePageByWindow (TSelf(wxAuiTabContainer) _obj, TClass(wxWindow) _page );
TBool 	 wxAuiTabContainer_SetActivePage (TSelf(wxAuiTabContainer) _obj, size_t page );
void 	 wxAuiTabContainer_SetNoneActive (TSelf(wxAuiTabContainer) _obj );
int 	 wxAuiTabContainer_GetActivePage (TSelf(wxAuiTabContainer) _obj );
//TBool 	 wxAuiTabContainer_TabHitTest (TSelf(wxAuiTabContainer) _obj, int x, int y, TClassRef(wxWindow) _hit );
//TBool 	 wxAuiTabContainer_ButtonHitTest (TSelf(wxAuiTabContainer) _obj, int x, int y, TClassRef(wxAuiTabContainerButton) _hit );
TClass(wxWindow) 	 wxAuiTabContainer_GetWindowFromIdx (TSelf(wxAuiTabContainer) _obj, size_t idx );
int 	 wxAuiTabContainer_GetIdxFromWindow (TSelf(wxAuiTabContainer) _obj, TClass(wxWindow) _page );
size_t 	 wxAuiTabContainer_GetPageCount (TSelf(wxAuiTabContainer) _obj );
TClass(wxAuiNotebookPage) 	 wxAuiTabContainer_GetPage (TSelf(wxAuiTabContainer) _obj, size_t idx );
TClass(wxAuiNotebookPageArray) 	 wxAuiTabContainer_GetPages (TSelf(wxAuiTabContainer) _obj);
void 	 wxAuiTabContainer_SetNormalFont (TSelf(wxAuiTabContainer) _obj, TClass(wxFont) _normalFont );
void 	 wxAuiTabContainer_SetSelectedFont (TSelf(wxAuiTabContainer) _obj, TClass(wxFont) _selectedFont );
void 	 wxAuiTabContainer_SetMeasuringFont (TSelf(wxAuiTabContainer) _obj, TClass(wxFont) _measuringFont );
void 	 wxAuiTabContainer_SetColour (TSelf(wxAuiTabContainer) _obj, TClass(wxColour) _colour );
void 	 wxAuiTabContainer_SetActiveColour (TSelf(wxAuiTabContainer) _obj, TClass(wxColour) _colour );
void 	 wxAuiTabContainer_DoShowHide (TSelf(wxAuiTabContainer) _obj );
void 	 wxAuiTabContainer_SetRect (TSelf(wxAuiTabContainer) _obj, TClass(wxRect) _rect );
void 	 wxAuiTabContainer_RemoveButton (TSelf(wxAuiTabContainer) _obj, int id );
void 	 wxAuiTabContainer_AddButton (TSelf(wxAuiTabContainer) _obj, int id, int location, TClass(wxBitmap) _normalBitmap, TClass(wxBitmap) _disabledBitmap );
size_t 	 wxAuiTabContainer_GetTabOffset (TSelf(wxAuiTabContainer) _obj );
void 	 wxAuiTabContainer_SetTabOffset (TSelf(wxAuiTabContainer) _obj, size_t offset );
TBool 	 wxAuiTabContainer_IsTabVisible (TSelf(wxAuiTabContainer) _obj, int tabPage, int tabOffset, TClass(wxDC) _dc, TClass(wxWindow) _wnd );
void 	 wxAuiTabContainer_MakeTabVisible (TSelf(wxAuiTabContainer) _obj, int tabPage, TClass(wxWindow) _win );

/* wxAuiTabCtrl */
TClassDefExtend(wxAuiTabCtrl, wxControl)
void 	 wxAuiTabCtrl_SetArtProvider (TSelf(wxAuiTabCtrl) _obj, TClass(wxAuiTabArt) _art );
TClass(wxAuiTabArt) 	 wxAuiTabCtrl_GetArtProvider (TSelf(wxAuiTabCtrl) _obj );
void 	 wxAuiTabCtrl_SetFlags (TSelf(wxAuiTabCtrl) _obj, int _flags );
int 	 wxAuiTabCtrl_GetFlags (TSelf(wxAuiTabCtrl) _obj );
TBool 	 wxAuiTabCtrl_AddPage (TSelf(wxAuiTabCtrl) _obj, TClass(wxWindow) _page, TClass(wxAuiNotebookPage) _info );
TBool 	 wxAuiTabCtrl_InsertPage (TSelf(wxAuiTabCtrl) _obj, TClass(wxWindow) _page, TClass(wxAuiNotebookPage) _info, size_t idx );
TBool 	 wxAuiTabCtrl_MovePage (TSelf(wxAuiTabCtrl) _obj, TClass(wxWindow) _page, size_t newIdx );
TBool 	 wxAuiTabCtrl_RemovePage (TSelf(wxAuiTabCtrl) _obj, TClass(wxWindow) _page );
TBool 	 wxAuiTabCtrl_SetActivePageByWindow (TSelf(wxAuiTabCtrl) _obj, TClass(wxWindow) _page );
TBool 	 wxAuiTabCtrl_SetActivePage (TSelf(wxAuiTabCtrl) _obj, size_t page );
void 	 wxAuiTabCtrl_SetNoneActive (TSelf(wxAuiTabCtrl) _obj );
int 	 wxAuiTabCtrl_GetActivePage (TSelf(wxAuiTabCtrl) _obj );
//TBool 	 wxAuiTabCtrl_TabHitTest (TSelf(wxAuiTabCtrl) _obj, int x, int y, TClassRef(wxWindow) _hit );
//TBool 	 wxAuiTabCtrl_ButtonHitTest (TSelf(wxAuiTabCtrl) _obj, int x, int y, TClassRef(wxAuiTabCtrlButton) _hit );
TClass(wxWindow) 	 wxAuiTabCtrl_GetWindowFromIdx (TSelf(wxAuiTabCtrl) _obj, size_t idx );
int 	 wxAuiTabCtrl_GetIdxFromWindow (TSelf(wxAuiTabCtrl) _obj, TClass(wxWindow) _page );
size_t 	 wxAuiTabCtrl_GetPageCount (TSelf(wxAuiTabCtrl) _obj );
TClass(wxAuiNotebookPage) 	 wxAuiTabCtrl_GetPage (TSelf(wxAuiTabCtrl) _obj, size_t idx );
TClass(wxAuiNotebookPageArray) 	 wxAuiTabCtrl_GetPages (TSelf(wxAuiTabCtrl) _obj);
void 	 wxAuiTabCtrl_SetNormalFont (TSelf(wxAuiTabCtrl) _obj, TClass(wxFont) _normalFont );
void 	 wxAuiTabCtrl_SetSelectedFont (TSelf(wxAuiTabCtrl) _obj, TClass(wxFont) _selectedFont );
void 	 wxAuiTabCtrl_SetMeasuringFont (TSelf(wxAuiTabCtrl) _obj, TClass(wxFont) _measuringFont );
void 	 wxAuiTabCtrl_SetColour (TSelf(wxAuiTabCtrl) _obj, TClass(wxColour) _colour );
void 	 wxAuiTabCtrl_SetActiveColour (TSelf(wxAuiTabCtrl) _obj, TClass(wxColour) _colour );
void 	 wxAuiTabCtrl_DoShowHide (TSelf(wxAuiTabCtrl) _obj );
void 	 wxAuiTabCtrl_SetRect (TSelf(wxAuiTabCtrl) _obj, TClass(wxRect) _rect );
void 	 wxAuiTabCtrl_RemoveButton (TSelf(wxAuiTabCtrl) _obj, int id );
void 	 wxAuiTabCtrl_AddButton (TSelf(wxAuiTabCtrl) _obj, int id, int location, TClass(wxBitmap) _normalBitmap, TClass(wxBitmap) _disabledBitmap );
size_t 	 wxAuiTabCtrl_GetTabOffset (TSelf(wxAuiTabCtrl) _obj );
void 	 wxAuiTabCtrl_SetTabOffset (TSelf(wxAuiTabCtrl) _obj, size_t offset );
TBool 	 wxAuiTabCtrl_IsTabVisible (TSelf(wxAuiTabCtrl) _obj, int tabPage, int tabOffset, TClass(wxDC) _dc, TClass(wxWindow) _wnd );
void 	 wxAuiTabCtrl_MakeTabVisible (TSelf(wxAuiTabCtrl) _obj, int tabPage, TClass(wxWindow) _win );


/* wxAuiTabArt */
TClassDef(wxAuiTabArt)
TClass(wxAuiTabArt) 	 wxAuiTabArt_Clone (TSelf(wxAuiTabArt) _obj );
void 	 wxAuiTabArt_DrawBackground (TSelf(wxAuiTabArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _rect );
void 	 wxAuiTabArt_DrawButton (TSelf(wxAuiTabArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _in_rect, int bitmap_id, int button_state, int orientation, TClass(wxRect) _out_rect );
void 	 wxAuiTabArt_DrawTab (TSelf(wxAuiTabArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxAuiNotebookPage) _page, TClass(wxRect) _rect, int close_button_state, TClass(wxRect) _out_tab_rect, TClass(wxRect) _out_button_rect, int *x_extent );
int 	 wxAuiTabArt_GetBestTabCtrlSize (TSelf(wxAuiTabArt) _obj, TClass(wxWindow) _wnd, TClass(wxAuiNotebookPageArray) _pages, TSize(_width,_height));
int 	 wxAuiTabArt_GetIndentSize (TSelf(wxAuiTabArt) _obj );
TClass(wxSize) 	 wxAuiTabArt_GetTabSize (TSelf(wxAuiTabArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxString) _caption, TClass(wxBitmap) _bitmap, TBool active, int close_button_state, int *x_extent );
void 	 wxAuiTabArt_SetFlags (TSelf(wxAuiTabArt) _obj, int _flags );
void 	 wxAuiTabArt_SetMeasuringFont (TSelf(wxAuiTabArt) _obj, TClass(wxFont) _font );
void 	 wxAuiTabArt_SetNormalFont (TSelf(wxAuiTabArt) _obj, TClass(wxFont) _font );
void 	 wxAuiTabArt_SetSelectedFont (TSelf(wxAuiTabArt) _obj, TClass(wxFont) _font );
void 	 wxAuiTabArt_SetColour (TSelf(wxAuiTabArt) _obj, TClass(wxColour) _colour );
void 	 wxAuiTabArt_SetActiveColour (TSelf(wxAuiTabArt) _obj, TClass(wxColour) _colour );
void 	 wxAuiTabArt_SetSizingInfo (TSelf(wxAuiTabArt) _obj, TSize(_width,_height), size_t tab_count );

/* wxAuiSimpleTabArt */
TClassDefExtend(wxAuiSimpleTabArt, wxAuiTabArt)
TClass(wxAuiSimpleTabArt)  wxAuiSimpleTabArt_Create();
TClass(wxAuiTabArt) 	 wxAuiSimpleTabArt_Clone (TSelf(wxAuiSimpleTabArt) _obj );
void 	 wxAuiSimpleTabArt_SetFlags (TSelf(wxAuiSimpleTabArt) _obj, int _flags );
void 	 wxAuiSimpleTabArt_SetSizingInfo (TSelf(wxAuiSimpleTabArt) _obj, TSize(_width,_height), size_t tabCount );
void 	 wxAuiSimpleTabArt_SetNormalFont (TSelf(wxAuiSimpleTabArt) _obj, TClass(wxFont) _font );
void 	 wxAuiSimpleTabArt_SetSelectedFont (TSelf(wxAuiSimpleTabArt) _obj, TClass(wxFont) _font );
void 	 wxAuiSimpleTabArt_SetMeasuringFont (TSelf(wxAuiSimpleTabArt) _obj, TClass(wxFont) _font );
void 	 wxAuiSimpleTabArt_SetColour (TSelf(wxAuiSimpleTabArt) _obj, TClass(wxColour) _colour );
void 	 wxAuiSimpleTabArt_SetActiveColour (TSelf(wxAuiSimpleTabArt) _obj, TClass(wxColour) _colour );
void 	 wxAuiSimpleTabArt_DrawBackground (TSelf(wxAuiSimpleTabArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _rect );
void 	 wxAuiSimpleTabArt_DrawTab (TSelf(wxAuiSimpleTabArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxAuiNotebookPage) _pane, TClass(wxRect) _inRect, int closeButtonState, TClass(wxRect) _outTabRect, TClass(wxRect) _outButtonRect, int *xExtent );
void 	 wxAuiSimpleTabArt_DrawButton (TSelf(wxAuiSimpleTabArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxRect) _inRect, int bitmapId, int buttonState, int orientation, TClass(wxRect) _outRect );
int 	 wxAuiSimpleTabArt_GetIndentSize (TSelf(wxAuiSimpleTabArt) _obj );
TClass(wxSize) 	 wxAuiSimpleTabArt_GetTabSize (TSelf(wxAuiSimpleTabArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _wnd, TClass(wxString) _caption, TClass(wxBitmap) _bitmap, TBool active, int closeButtonState, int *xExtent );
int 	 wxAuiSimpleTabArt_ShowDropDown (TSelf(wxAuiSimpleTabArt) _obj, TClass(wxWindow) _wnd, TClass(wxAuiNotebookPageArray) _items, int activeIdx );
int 	 wxAuiSimpleTabArt_GetBestTabCtrlSize (TSelf(wxAuiSimpleTabArt) _obj, TClass(wxWindow) _wnd, TClass(wxAuiNotebookPageArray) _pages, TSize(_width,_height));


/* wxAuiManager - public member */
TClassDefExtend(wxAuiManager,wxEvtHandler)
TClass(wxAuiManager) wxAuiManager_Create(TClass(wxWindow) _managed_wnd, int _flags);
void       wxAuiManager_Delete( TSelf(wxAuiManager) _obj );
TBool      wxAuiManager_DetachPane(TSelf(wxAuiManager) _obj, TClass(wxWindow) _window);
TClass(wxAuiPaneInfoArray)  wxAuiManager_GetAllPanes(TSelf(wxAuiManager) _obj);
TClass(wxAuiDockArt) wxAuiManager_GetArtProvider(TSelf(wxAuiManager) _obj);
void       wxAuiManager_GetDockSizeConstraint(TSelf(wxAuiManager) _obj, double* _widthpct, double* _heightpct);
int    wxAuiManager_GetFlags(TSelf(wxAuiManager) _obj);
TClass(wxWindow) wxAuiManager_GetManagedWindow(TSelf(wxAuiManager) _obj);
void       wxAuiManager_HideHint(TSelf(wxAuiManager) _obj);
TBool      wxAuiManager_InsertPane(TSelf(wxAuiManager) _obj, TClass(wxWindow) _window, TClass(wxAuiPaneInfo) _insert_location, int _insert_level);
void       wxAuiManager_LoadPaneInfo(TSelf(wxAuiManager) _obj, TClass(wxString) _pane_part, TClass(wxAuiPaneInfo) _pane);
TBool      wxAuiManager_LoadPerspective(TSelf(wxAuiManager) _obj, TClass(wxString) _perspective, TBool update);
TClass(wxString) wxAuiManager_SavePaneInfo(TSelf(wxAuiManager) _obj, TClass(wxAuiPaneInfo) _pane);
TClass(wxString) wxAuiManager_SavePerspective(TSelf(wxAuiManager) _obj);
void       wxAuiManager_SetArtProvider(TSelf(wxAuiManager) _obj, TClass(wxAuiDockArt) _art_provider);
void       wxAuiManager_SetDockSizeConstraint(TSelf(wxAuiManager) _obj, double widthpct, double heightpct);
void       wxAuiManager_SetFlags(TSelf(wxAuiManager) _obj, int flags);
void       wxAuiManager_SetManagedWindow(TSelf(wxAuiManager) _obj, TClass(wxWindow) _managed_wnd);
void       wxAuiManager_ShowHint(TSelf(wxAuiManager) _obj, TClass(wxRect) _rect);
void       wxAuiManager_UnInit(TSelf(wxAuiManager) _obj);
void       wxAuiManager_Update(TSelf(wxAuiManager) _obj);
TBool     wxAuiManager_AddPane(TSelf(wxAuiManager) _obj, TClass(wxWindow) _window, int _direction, TClass(wxString) _caption);
TBool     wxAuiManager_AddPaneByPaneInfo(TSelf(wxAuiManager) _obj, TClass(wxWindow) _window, TClass(wxAuiPaneInfo) _pane_info);
TBool     wxAuiManager_AddPaneByPaneInfoAndDropPosition(TSelf(wxAuiManager) _obj, TClass(wxWindow) _window, TClass(wxAuiPaneInfo) _pane_info, TPoint(x,y));
TClass(wxAuiPaneInfo)      wxAuiManager_GetPaneByWindow(TSelf(wxAuiManager) _obj, TClass(wxWindow) _window);
TClass(wxAuiPaneInfo)      wxAuiManager_GetPaneByName(TSelf(wxAuiManager) _obj, TClass(wxString) _name);
TClass(wxAuiManager)  wxAuiManager_GetManager(TClass(wxWindow) _window);

/* wxAuiDockArt */
TClassDef(wxAuiDockArt)
void wxAuiDockArt_DrawBackground(TSelf(wxAuiDockArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _window, int orientation, TClass(wxRect) _rect);
void wxAuiDockArt_DrawBorder(TSelf(wxAuiDockArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _window, TClass(wxRect) _rect, TClass(wxAuiPaneInfo) _pane);
void wxAuiDockArt_DrawCaption(TSelf(wxAuiDockArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _window, TClass(wxString) _text, TClass(wxRect) _rect, TClass(wxAuiPaneInfo) _pane);
void wxAuiDockArt_DrawGripper(TSelf(wxAuiDockArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _window, TClass(wxRect) _rect, TClass(wxAuiPaneInfo) _pane);
void wxAuiDockArt_DrawPaneButton(TSelf(wxAuiDockArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _window, int button, int button_state, TClass(wxRect) _rect, TClass(wxAuiPaneInfo) _pane);
void wxAuiDockArt_DrawSash(TSelf(wxAuiDockArt) _obj, TClass(wxDC) _dc, TClass(wxWindow) _window, int orientation, TClass(wxRect) _rect);
TClass(wxColour) wxAuiDockArt_GetColour(TSelf(wxAuiDockArt) _obj, int id);
TClass(wxFont) wxAuiDockArt_GetFont(TSelf(wxAuiDockArt) _obj, int id);
int wxAuiDockArt_GetMetric(TSelf(wxAuiDockArt) _obj, int id);
void wxAuiDockArt_SetColour(TSelf(wxAuiDockArt) _obj, int id, TClass(wxColour) _colour);
void wxAuiDockArt_SetFont(TSelf(wxAuiDockArt) _obj, int id, TClass(wxFont) _font);
void wxAuiDockArt_SetMetric(TSelf(wxAuiDockArt) _obj, int id, int new_val);

/* wxAuiPaneInfo */
TClassDef(wxAuiPaneInfo)
TClass(wxAuiPaneInfo)    wxAuiPaneInfo_CreateDefault();
TClass(wxAuiPaneInfo)    wxAuiPaneInfo_Create(TClass(wxAuiPaneInfo) _c );
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Bottom (TSelf(wxAuiPaneInfo) _obj);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_BottomDockable (TSelf(wxAuiPaneInfo) _obj, TBool b);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Caption (TSelf(wxAuiPaneInfo) _obj, TClass(wxString) _c);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_CaptionVisible (TSelf(wxAuiPaneInfo) _obj, TBool visible);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_CloseButton (TSelf(wxAuiPaneInfo) _obj, TBool visible);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_DefaultPane (TSelf(wxAuiPaneInfo) _obj);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_DestroyOnClose (TSelf(wxAuiPaneInfo) _obj, TBool b);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Direction (TSelf(wxAuiPaneInfo) _obj, int direction);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Dock (TSelf(wxAuiPaneInfo) _obj);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_DockFixed (TSelf(wxAuiPaneInfo) _obj, TBool b);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Dockable (TSelf(wxAuiPaneInfo) _obj, TBool b);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Fixed (TSelf(wxAuiPaneInfo) _obj);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Float (TSelf(wxAuiPaneInfo) _obj);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Floatable (TSelf(wxAuiPaneInfo) _obj, TBool b);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Gripper (TSelf(wxAuiPaneInfo) _obj, TBool visible);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_GripperTop (TSelf(wxAuiPaneInfo) _obj, TBool attop);
TBool 	 wxAuiPaneInfo_HasBorder (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_HasCaption (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_HasCloseButton (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_HasFlag (TSelf(wxAuiPaneInfo) _obj, int flag );
TBool 	 wxAuiPaneInfo_HasGripper (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_HasGripperTop (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_HasMaximizeButton (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_HasMinimizeButton (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_HasPinButton (TSelf(wxAuiPaneInfo) _obj );
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Hide (TSelf(wxAuiPaneInfo) _obj);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Icon (TSelf(wxAuiPaneInfo) _obj, TClass(wxBitmap) _b);
TBool 	 wxAuiPaneInfo_IsBottomDockable (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_IsDockable (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_IsDocked (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_IsFixed (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_IsFloatable (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_IsFloating (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_IsLeftDockable (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_IsMovable (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_IsOk (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_IsResizable (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_IsRightDockable (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_IsShown (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_IsToolbar (TSelf(wxAuiPaneInfo) _obj );
TBool 	 wxAuiPaneInfo_IsTopDockable (TSelf(wxAuiPaneInfo) _obj );
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Layer (TSelf(wxAuiPaneInfo) _obj, int layer);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Left (TSelf(wxAuiPaneInfo) _obj);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_LeftDockable (TSelf(wxAuiPaneInfo) _obj, TBool b);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_MaximizeButton (TSelf(wxAuiPaneInfo) _obj, TBool visible);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_MinimizeButton (TSelf(wxAuiPaneInfo) _obj, TBool visible);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Movable (TSelf(wxAuiPaneInfo) _obj, TBool b);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Name (TSelf(wxAuiPaneInfo) _obj, TClass(wxString) _n);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_PaneBorder (TSelf(wxAuiPaneInfo) _obj, TBool visible);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_PinButton (TSelf(wxAuiPaneInfo) _obj, TBool visible);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Position (TSelf(wxAuiPaneInfo) _obj, int pos);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Resizable (TSelf(wxAuiPaneInfo) _obj, TBool resizable);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Right (TSelf(wxAuiPaneInfo) _obj);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_RightDockable (TSelf(wxAuiPaneInfo) _obj, TBool b);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Row (TSelf(wxAuiPaneInfo) _obj, int row);
void 	 wxAuiPaneInfo_SafeSet (TSelf(wxAuiPaneInfo) _obj, TClass(wxAuiPaneInfo) source );
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_SetFlag (TSelf(wxAuiPaneInfo) _obj, int flag, TBool option_state);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Show (TSelf(wxAuiPaneInfo) _obj, TBool show);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_ToolbarPane (TSelf(wxAuiPaneInfo) _obj);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Top (TSelf(wxAuiPaneInfo) _obj);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_TopDockable (TSelf(wxAuiPaneInfo) _obj, TBool b);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Window (TSelf(wxAuiPaneInfo) _obj, TClass(wxWindow) _w);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Copy (TSelf(wxAuiPaneInfo) _obj, TClass(wxAuiPaneInfo) _c);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_BestSize (TSelf(wxAuiPaneInfo) _obj, TSize(_width,_height));
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_BestSizeXY (TSelf(wxAuiPaneInfo) _obj, int x, int y);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Centre (TSelf(wxAuiPaneInfo) _obj);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_Center (TSelf(wxAuiPaneInfo) _obj);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_CentrePane (TSelf(wxAuiPaneInfo) _obj);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_CenterPane (TSelf(wxAuiPaneInfo) _obj);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_FloatingPosition (TSelf(wxAuiPaneInfo) _obj, TPoint(x,y));
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_FloatingPositionXY (TSelf(wxAuiPaneInfo) _obj, int x, int y);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_FloatingSize (TSelf(wxAuiPaneInfo) _obj, TSize(_width,_height));
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_FloatingSizeXY (TSelf(wxAuiPaneInfo) _obj, int x, int y);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_MaxSize (TSelf(wxAuiPaneInfo) _obj, TSize(_width,_height));
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_MaxSizeXY (TSelf(wxAuiPaneInfo) _obj, int x, int y);
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_MinSize (TSelf(wxAuiPaneInfo) _obj, TSize(_width,_height));
TClass(wxAuiPaneInfo) 	 wxAuiPaneInfo_MinSizeXY (TSelf(wxAuiPaneInfo) _obj, int x, int y);

/* wxAuiManagerEvent */
TClassDefExtend(wxAuiManagerEvent, wxEvtHandler)
TClass(wxAuiManagerEvent)  wxAuiManagerEvent_Create(int type );
TBool 	 wxAuiManagerEvent_CanVeto (TSelf(wxAuiManagerEvent) _obj );
int 	 wxAuiManagerEvent_GetButton (TSelf(wxAuiManagerEvent) _obj );
TClass(wxDC) 	 wxAuiManagerEvent_GetDC (TSelf(wxAuiManagerEvent) _obj );
TBool 	 wxAuiManagerEvent_GetVeto (TSelf(wxAuiManagerEvent) _obj );
TClass(wxAuiManager) 	 wxAuiManagerEvent_GetManager (TSelf(wxAuiManagerEvent) _obj );
TClass(wxAuiPaneInfo) 	 wxAuiManagerEvent_GetPane (TSelf(wxAuiManagerEvent) _obj );
void 	 wxAuiManagerEvent_SetButton (TSelf(wxAuiManagerEvent) _obj, int button );
void 	 wxAuiManagerEvent_SetCanVeto (TSelf(wxAuiManagerEvent) _obj, TBool can_veto );
void 	 wxAuiManagerEvent_SetDC (TSelf(wxAuiManagerEvent) _obj, TClass(wxDC) _pdc);
void 	 wxAuiManagerEvent_SetManager (TSelf(wxAuiManagerEvent) _obj, TClass(wxAuiManager) _manager );
void 	 wxAuiManagerEvent_SetPane (TSelf(wxAuiManagerEvent) _obj, TClass(wxAuiPaneInfo) _pane );
void 	 wxAuiManagerEvent_Veto (TSelf(wxAuiManagerEvent) _obj, TBool veto );


/* wxAuiNotebookEvent */
TClassDefExtend(wxAuiNotebookEvent, wxBookCtrlEvent)
TClass(wxAuiNotebookEvent)  wxAuiNotebookEvent_Create(int command_type, int win_id);
TClass(wxAuiNotebook) wxAuiNotebookEvent_GetDragSource(TSelf(wxAuiNotebookEvent) _obj);
/* void SetDragSource(wxAuiNotebook* s) { m_dragSource = s; } */


/* wxBookCtrlEvent */
TClassDefExtend(wxBookCtrlEvent, wxNotifyEvent)
TClass(wxBookCtrlEvent)  wxBookCtrlEvent_Create(int commandType, int winid, int nSel, int nOldSel);
int     wxBookCtrlEvent_GetSelection(TSelf(wxBookCtrlEvent) _obj);
int     wxBookCtrlEvent_GetOldSelection(TSelf(wxBookCtrlEvent) _obj);
/* void    wxBookCtrlEvent_SetSelection(int nSel); */
/* void    wxBookCtrlEvent_SetOldSelection(int nOldSel); */


/* wxBookCtrlBase */
TClassDefExtend(wxBookCtrlBase, wxControl)
TBool 	wxBookCtrlBase_CreateFromDefault (TSelf(wxBookCtrlBase) _obj, TClass(wxWindow) _parent, int winid, TPoint(x,y), TSize(_width,_height), long style, TClass(wxString) _name );
void 	wxBookCtrlBase_SetPageSize (TSelf(wxBookCtrlBase) _obj, TSize(_width,_height));
int 	wxBookCtrlBase_HitTest (TSelf(wxBookCtrlBase) _obj, TPoint(x,y), long *flags );
int 	wxBookCtrlBase_GetPageImage (TSelf(wxBookCtrlBase) _obj, size_t nPage );
TBool 	wxBookCtrlBase_SetPageImage (TSelf(wxBookCtrlBase) _obj, size_t page, int image );
TClass(wxString) 	wxBookCtrlBase_GetPageText (TSelf(wxBookCtrlBase) _obj, size_t nPage );
TBool 	wxBookCtrlBase_SetPageText (TSelf(wxBookCtrlBase) _obj, size_t page, TClass(wxString) _text );
int 	wxBookCtrlBase_GetSelection (TSelf(wxBookCtrlBase) _obj );
TClass(wxWindow) 	wxBookCtrlBase_GetCurrentPage (TSelf(wxBookCtrlBase) _obj );
int 	wxBookCtrlBase_SetSelection (TSelf(wxBookCtrlBase) _obj, size_t page );
void 	wxBookCtrlBase_AdvanceSelection (TSelf(wxBookCtrlBase) _obj, TBool forward );
int 	wxBookCtrlBase_ChangeSelection (TSelf(wxBookCtrlBase) _obj, size_t page );
int 	wxBookCtrlBase_FindPage (TSelf(wxBookCtrlBase) _obj, TClass(wxWindow) _page );
TBool 	wxBookCtrlBase_AddPage (TSelf(wxBookCtrlBase) _obj, TClass(wxWindow) _page, TClass(wxString) _text, TBool select, int imageId );
TBool 	wxBookCtrlBase_DeleteAllPages (TSelf(wxBookCtrlBase) _obj );
TBool 	wxBookCtrlBase_DeletePage (TSelf(wxBookCtrlBase) _obj, size_t page );
TBool 	wxBookCtrlBase_InsertPage (TSelf(wxBookCtrlBase) _obj, size_t index, TClass(wxWindow) _page, TClass(wxString) _text, TBool select, int imageId );
TBool 	wxBookCtrlBase_RemovePage (TSelf(wxBookCtrlBase) _obj, size_t page );
size_t 	wxBookCtrlBase_GetPageCount (TSelf(wxBookCtrlBase) _obj );
TClass(wxWindow) 	wxBookCtrlBase_GetPage (TSelf(wxBookCtrlBase) _obj, size_t page );
void 	wxBookCtrlBase_AssignImageList (TSelf(wxBookCtrlBase) _obj, TClass(wxImageList) imageList );
void 	wxBookCtrlBase_SetImageList (TSelf(wxBookCtrlBase) _obj, TClass(wxImageList) imageList );
TClass(wxImageList) wxBookCtrlBase_GetImageList (TSelf(wxBookCtrlBase) _obj );

TClassDef(wxAuiNotebookPage)
TClass(wxWindow) wxAuiNotebookPage_Window(TSelf(wxAuiNotebookPage) _obj);
TClass(wxString) wxAuiNotebookPage_Caption(TSelf(wxAuiNotebookPage) _obj);
TClass(wxString) wxAuiNotebookPage_Tooltip(TSelf(wxAuiNotebookPage) _obj);
TClass(wxBitmap) wxAuiNotebookPage_Bitmap(TSelf(wxAuiNotebookPage) _obj);
TClass(wxRect) wxAuiNotebookPage_Rect(TSelf(wxAuiNotebookPage) _obj);
TBool wxAuiNotebookPage_Active(TSelf(wxAuiNotebookPage) _obj);

/** wxAuiNotebookPageArray **/
/** see wxWidgets dynarray.h for additional array functions **/
TClassDef(wxAuiNotebookPageArray)
TClass(wxAuiNotebookPageArray) wxAuiNotebookPageArray_Create();
void       wxAuiNotebookPageArray_Delete(TSelf(wxAuiNotebookPageArray) _obj);
int        wxAuiNotebookPageArray_GetCount(TSelf(wxAuiNotebookPageArray) _obj);
TClass(wxAuiNotebookPage)       wxAuiNotebookPageArray_Item(TSelf(wxAuiNotebookPageArray) _obj, int _idx);

/** wxAuiToolBarItemArray **/
/** see wxWidgets dynarray.h for additional array functions **/
TClassDef(wxAuiToolBarItemArray)
TClass(wxAuiToolBarItemArray) wxAuiToolBarItemArray_Create();
void       wxAuiToolBarItemArray_Delete(TSelf(wxAuiToolBarItemArray) _obj);
int        wxAuiToolBarItemArray_GetCount(TSelf(wxAuiToolBarItemArray) _obj);
TClass(wxAuiToolBarItem)       wxAuiToolBarItemArray_Item(TSelf(wxAuiToolBarItemArray) _obj, int _idx);

/** wxAuiPaneInfoArray **/
/** see wxWidgets dynarray.h for additional array functions **/
TClassDef(wxAuiPaneInfoArray)
TClass(wxAuiPaneInfoArray) wxAuiPaneInfoArray_Create();
void       wxAuiPaneInfoArray_Delete(TSelf(wxAuiPaneInfoArray) _obj);
int        wxAuiPaneInfoArray_GetCount(TSelf(wxAuiPaneInfoArray) _obj);
TClass(wxAuiPaneInfo)       wxAuiPaneInfoArray_Item(TSelf(wxAuiPaneInfoArray) _obj, int _idx);


/* wxAutoBufferedPaintDC */
TClassDefExtend(wxAutoBufferedPaintDC,wxDC)
TClass(wxAutoBufferedPaintDC) wxAutoBufferedPaintDC_Create( TClass(wxWindow) window );
void       wxAutoBufferedPaintDC_Delete( TSelf(wxAutoBufferedPaintDC) self );

/* wxAutomationObject */
TClassDefExtend(wxAutomationObject,wxObject)

/* wxBitmap */
TClassDefExtend(wxBitmap,wxGDIObject)
void       wxBitmap_AddHandler( TClass(wxEvtHandler) handler );
void       wxBitmap_CleanUpHandlers(  );
TClass(wxBitmap) wxBitmap_Create( void* _data, int _type, TSize(_width,_height), int _depth );
TClass(wxBitmap) wxBitmap_CreateDefault(  );
TClass(wxBitmap) wxBitmap_CreateEmpty( TSize(_width,_height), int _depth );
TClass(wxBitmap) wxBitmap_CreateFromXPM( TSelf(wxBitmap) data );
TClass(wxBitmap) wxBitmap_CreateLoad( TClass(wxString) name, int type );
void       wxBitmap_Delete( TSelf(wxBitmap) _obj );
void*      wxBitmap_FindHandlerByExtension( TSelf(wxBitmap) extension, int type );
void*      wxBitmap_FindHandlerByName( TClass(wxString) name );
void*      wxBitmap_FindHandlerByType( int type );
int        wxBitmap_GetDepth( TSelf(wxBitmap) _obj );
int        wxBitmap_GetHeight( TSelf(wxBitmap) _obj );
TClass(wxMask) wxBitmap_GetMask( TSelf(wxBitmap) _obj );
void       wxBitmap_GetSubBitmap( TSelf(wxBitmap) _obj, TRect(x,y,w,h), TClassRef(wxBitmap) _ref );
int        wxBitmap_GetWidth( TSelf(wxBitmap) _obj );
void       wxBitmap_InitStandardHandlers(  );
void       wxBitmap_InsertHandler( TClass(wxEvtHandler) handler );
int        wxBitmap_LoadFile( TSelf(wxBitmap) _obj, TClass(wxString) name, int type );
TBool      wxBitmap_IsOk( TSelf(wxBitmap) _obj );
TBool      wxBitmap_RemoveHandler( TClass(wxString) name );
int        wxBitmap_SaveFile( TSelf(wxBitmap) _obj, TClass(wxString) name, int type, TClass(wxPalette) cmap );
void       wxBitmap_SetDepth( TSelf(wxBitmap) _obj, int d );
void       wxBitmap_SetHeight( TSelf(wxBitmap) _obj, int h );
void       wxBitmap_SetMask( TSelf(wxBitmap) _obj, TClass(wxMask) mask );
void       wxBitmap_SetWidth( TSelf(wxBitmap) _obj, int w );

/* wxBitmapButton */
TClassDefExtend(wxBitmapButton,wxButton)
TClass(wxBitmapButton) wxBitmapButton_Create( TClass(wxWindow) _prt, int _id, TClass(wxBitmap) _bmp, TRect(_lft,_top,_wdt,_hgt), int _stl );
void       wxBitmapButton_GetBitmapDisabled( TSelf(wxBitmapButton) _obj, TClassRef(wxBitmap) _ref );
void       wxBitmapButton_GetBitmapFocus( TSelf(wxBitmapButton) _obj, TClassRef(wxBitmap) _ref );
void       wxBitmapButton_GetBitmapLabel( TSelf(wxBitmapButton) _obj, TClassRef(wxBitmap) _ref );
void       wxBitmapButton_GetBitmapSelected( TSelf(wxBitmapButton) _obj, TClassRef(wxBitmap) _ref );
int        wxBitmapButton_GetMarginX( TSelf(wxBitmapButton) _obj );
int        wxBitmapButton_GetMarginY( TSelf(wxBitmapButton) _obj );
void       wxBitmapButton_SetBitmapDisabled( TSelf(wxBitmapButton) _obj, TClass(wxBitmap) disabled );
void       wxBitmapButton_SetBitmapFocus( TSelf(wxBitmapButton) _obj, TClass(wxBitmap) focus );
void       wxBitmapButton_SetBitmapLabel( TSelf(wxBitmapButton) _obj, TClass(wxBitmap) bitmap );
void       wxBitmapButton_SetBitmapSelected( TSelf(wxBitmapButton) _obj, TClass(wxBitmap) sel );
void       wxBitmapButton_SetMargins( TSelf(wxBitmapButton) _obj, TPoint(x,y) );

/* wxBitmapToggleButton */
TClassDefExtend(wxBitmapToggleButton,wxToggleButton)
TClass(wxBitmapToggleButton) wxBitmapToggleButton_Create( TClass(wxWindow) parent, int id, TClass(wxBitmap) _bmp, TRect(x,y,w,h), int style );
TBool      wxBitmapToggleButton_Enable( TSelf(wxBitmapToggleButton) _obj, TBool enable );
TBool      wxBitmapToggleButton_GetValue( TSelf(wxBitmapToggleButton) _obj );
void       wxBitmapToggleButton_SetValue( TSelf(wxBitmapToggleButton) _obj, TBool state );
void       wxBitmapToggleButton_SetBitmapLabel( TSelf(wxBitmapToggleButton) _obj, TClass(wxBitmap) _bmp );

/* wxBitmapDataObject */
TClassDefExtend(wxBitmapDataObject,wxDataObjectSimple)
TClass(wxBitmapDataObject) BitmapDataObject_Create( TClass(wxBitmap) _bmp );
TClass(wxBitmapDataObject) BitmapDataObject_CreateEmpty(  );
void       BitmapDataObject_Delete( TSelf(wxBitmapDataObject) _obj );
void       BitmapDataObject_GetBitmap( TSelf(wxBitmapDataObject) _obj, TClassRef(wxBitmap) _bmp );
void       BitmapDataObject_SetBitmap( TSelf(wxBitmapDataObject) _obj, TClass(wxBitmap) _bmp );

/* wxBitmapHandler */
TClassDefExtend(wxBitmapHandler,wxObject)

/* wxBoxSizer */
TClassDefExtend(wxBoxSizer,wxSizer)
TClass(wxSize) wxBoxSizer_CalcMin( TSelf(wxBoxSizer) _obj );
TClass(wxBoxSizer) wxBoxSizer_Create( int orient );
int        wxBoxSizer_GetOrientation( TSelf(wxBoxSizer) _obj );
void       wxBoxSizer_RecalcSizes( TSelf(wxBoxSizer) _obj );

/* wxBrush */
TClassDefExtend(wxBrush,wxGDIObject)
void       wxBrush_Assign( TSelf(wxBrush) _obj, TClass(wxBrush) brush );
TClass(wxBrush) wxBrush_CreateDefault(  );
TClass(wxBrush) wxBrush_CreateFromBitmap( TClass(wxBitmap) bitmap );
TClass(wxBrush) wxBrush_CreateFromColour( TClass(wxColour) col, int style );
TClass(wxBrush) wxBrush_CreateFromStock( int id );
void       wxBrush_Delete( TSelf(wxBrush) _obj );
void       wxBrush_GetColour( TSelf(wxBrush) _obj, TClassRef(wxColour) _ref );
void       wxBrush_GetStipple( TSelf(wxBrush) _obj, TClassRef(wxBitmap) _ref );
int        wxBrush_GetStyle( TSelf(wxBrush) _obj );
TBool      wxBrush_IsEqual( TSelf(wxBrush) _obj, TClass(wxBrush) brush );
TBool      wxBrush_IsOk( TSelf(wxBrush) _obj );
void       wxBrush_SetColour( TSelf(wxBrush) _obj, TClass(wxColour) col );
void       wxBrush_SetColourSingle( TSelf(wxBrush) _obj, TChar r, TChar g, TChar b );
void       wxBrush_SetStipple( TSelf(wxBrush) _obj, TClass(wxBitmap) stipple );
void       wxBrush_SetStyle( TSelf(wxBrush) _obj, int style );

/* wxBrushList */
TClassDefExtend(wxBrushList,wxList)

/* wxBufferedDC */
TClassDefExtend(wxBufferedDC,wxDC)
TClass(wxBufferedDC) wxBufferedDC_CreateByDCAndSize( TClass(wxDC) dc, TSize(width, hight), int style );
TClass(wxBufferedDC) wxBufferedDC_CreateByDCAndBitmap( TClass(wxDC) dc, TClass(wxBitmap) bitmap, int style );
void       wxBufferedDC_Delete( TSelf(wxBufferedDC) self );

/* wxBufferedPaintDC */
TClassDefExtend(wxBufferedPaintDC,wxDC)
TClass(wxBufferedPaintDC) wxBufferedPaintDC_Create( TClass(wxWindow) window, int style );
TClass(wxBufferedPaintDC) wxBufferedPaintDC_CreateWithBitmap( TClass(wxWindow) window, TClass(wxBitmap) bitmap, int style );
void       wxBufferedPaintDC_Delete( TSelf(wxBufferedPaintDC) self );

/* wxBufferedInputStream */
TClassDefExtend(wxBufferedInputStream,wxFilterInputStream)

/* wxBufferedOutputStream */
TClassDefExtend(wxBufferedOutputStream,wxFilterOutputStream)

/* wxBusyCursor */
TClassDef(wxBusyCursor)
TClass(wxBusyCursor) wxBusyCursor_Create(  );
void*      wxBusyCursor_CreateWithCursor( TSelf(wxBusyCursor) _cur );
void       wxBusyCursor_Delete( TSelf(wxBusyCursor) _obj );

/* wxBusyInfo */
TClassDef(wxBusyInfo)
TClass(wxBusyInfo) wxBusyInfo_Create( TClass(wxString) _txt );
void       wxBusyInfo_Delete( TSelf(wxBusyInfo) _obj );

/* wxButton */
TClassDefExtend(wxButton,wxControl)
TClass(wxButton) wxButton_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TRect(_lft,_top,_wdt,_hgt), int _stl );
int        wxButton_SetBackgroundColour( TSelf(wxButton) _obj, TClass(wxColour) colour );
void       wxButton_SetDefault( TSelf(wxButton) _obj );

/* wxCSConv */
TClassDefExtend(wxCSConv,wxMBConv)

/* wxCalculateLayoutEvent */
TClassDefExtend(wxCalculateLayoutEvent,wxEvent)
TClass(wxCalculateLayoutEvent) wxCalculateLayoutEvent_Create( int id );
int        wxCalculateLayoutEvent_GetFlags( TSelf(wxCalculateLayoutEvent) _obj );
TClass(wxRect) wxCalculateLayoutEvent_GetRect( TSelf(wxCalculateLayoutEvent) _obj );
void       wxCalculateLayoutEvent_SetFlags( TSelf(wxCalculateLayoutEvent) _obj, int flags );
void       wxCalculateLayoutEvent_SetRect( TSelf(wxCalculateLayoutEvent) _obj, TRect(x,y,w,h) );

/* wxCalendarCtrl */
TClassDefExtend(wxCalendarCtrl,wxControl)
TClass(wxCalendarCtrl) wxCalendarCtrl_Create( TClass(wxWindow) _prt, int _id, TClass(wxDateTime) _dat, TRect(_lft,_top,_wdt,_hgt), int _stl );
void       wxCalendarCtrl_EnableHolidayDisplay( TSelf(wxCalendarCtrl) _obj, int display );
void       wxCalendarCtrl_EnableMonthChange( TSelf(wxCalendarCtrl) _obj, TBool enable );
void*      wxCalendarCtrl_GetAttr( TSelf(wxCalendarCtrl) _obj, int day );
void       wxCalendarCtrl_GetDate( TSelf(wxCalendarCtrl) _obj, void* date );
void       wxCalendarCtrl_GetHeaderColourBg( TSelf(wxCalendarCtrl) _obj, TClassRef(wxColour) _ref );
void       wxCalendarCtrl_GetHeaderColourFg( TSelf(wxCalendarCtrl) _obj, TClassRef(wxColour) _ref );
void       wxCalendarCtrl_GetHighlightColourBg( TSelf(wxCalendarCtrl) _obj, TClassRef(wxColour) _ref );
void       wxCalendarCtrl_GetHighlightColourFg( TSelf(wxCalendarCtrl) _obj, TClassRef(wxColour) _ref );
void       wxCalendarCtrl_GetHolidayColourBg( TSelf(wxCalendarCtrl) _obj, TClassRef(wxColour) _ref );
void       wxCalendarCtrl_GetHolidayColourFg( TSelf(wxCalendarCtrl) _obj, TClassRef(wxColour) _ref );
int        wxCalendarCtrl_HitTest( TSelf(wxCalendarCtrl) _obj, TPoint(x,y), void* date, void* wd );
void       wxCalendarCtrl_ResetAttr( TSelf(wxCalendarCtrl) _obj, int day );
void       wxCalendarCtrl_SetAttr( TSelf(wxCalendarCtrl) _obj, int day, void* attr );
void       wxCalendarCtrl_SetDate( TSelf(wxCalendarCtrl) _obj, void* date );
void       wxCalendarCtrl_SetHeaderColours( TSelf(wxCalendarCtrl) _obj, void* colFg, void* colBg );
void       wxCalendarCtrl_SetHighlightColours( TSelf(wxCalendarCtrl) _obj, void* colFg, void* colBg );
void       wxCalendarCtrl_SetHoliday( TSelf(wxCalendarCtrl) _obj, int day );
void       wxCalendarCtrl_SetHolidayColours( TSelf(wxCalendarCtrl) _obj, void* colFg, void* colBg );

/* wxCalendarDateAttr */
TClassDef(wxCalendarDateAttr)
TClass(wxCalendarDateAttr) wxCalendarDateAttr_Create( void* _ctxt, void* _cbck, void* _cbrd, void* _fnt, int _brd );
TClass(wxCalendarDateAttr) wxCalendarDateAttr_CreateDefault(  );
void       wxCalendarDateAttr_Delete( TSelf(wxCalendarDateAttr) _obj );
void       wxCalendarDateAttr_GetBackgroundColour( TSelf(wxCalendarDateAttr) _obj, TClassRef(wxColour) _ref );
int        wxCalendarDateAttr_GetBorder( TSelf(wxCalendarDateAttr) _obj );
void       wxCalendarDateAttr_GetBorderColour( TSelf(wxCalendarDateAttr) _obj, TClassRef(wxColour) _ref );
void       wxCalendarDateAttr_GetFont( TSelf(wxCalendarDateAttr) _obj, TClassRef(wxFont) _ref );
void       wxCalendarDateAttr_GetTextColour( TSelf(wxCalendarDateAttr) _obj, TClassRef(wxColour) _ref );
TBool      wxCalendarDateAttr_HasBackgroundColour( TSelf(wxCalendarDateAttr) _obj );
TBool      wxCalendarDateAttr_HasBorder( TSelf(wxCalendarDateAttr) _obj );
TBool      wxCalendarDateAttr_HasBorderColour( TSelf(wxCalendarDateAttr) _obj );
TBool      wxCalendarDateAttr_HasFont( TSelf(wxCalendarDateAttr) _obj );
TBool      wxCalendarDateAttr_HasTextColour( TSelf(wxCalendarDateAttr) _obj );
TBool      wxCalendarDateAttr_IsHoliday( TSelf(wxCalendarDateAttr) _obj );
void       wxCalendarDateAttr_SetBackgroundColour( TSelf(wxCalendarDateAttr) _obj, TClass(wxColour) col );
void       wxCalendarDateAttr_SetBorder( TSelf(wxCalendarDateAttr) _obj, int border );
void       wxCalendarDateAttr_SetBorderColour( TSelf(wxCalendarDateAttr) _obj, TClass(wxColour) col );
void       wxCalendarDateAttr_SetFont( TSelf(wxCalendarDateAttr) _obj, TClass(wxFont) font );
void       wxCalendarDateAttr_SetHoliday( TSelf(wxCalendarDateAttr) _obj, int holiday );
void       wxCalendarDateAttr_SetTextColour( TSelf(wxCalendarDateAttr) _obj, TClass(wxColour) col );

/* wxCalendarEvent */
TClassDefExtend(wxCalendarEvent,wxCommandEvent)
void       wxCalendarEvent_GetDate( TSelf(wxCalendarEvent) _obj, void* _dte );
int        wxCalendarEvent_GetWeekDay( TSelf(wxCalendarEvent) _obj );

/* wxCaret */
TClassDef(wxCaret)
TClass(wxCaret) wxCaret_Create( TClass(wxWindow) _wnd, int _wth, int _hgt );
int        wxCaret_GetBlinkTime(  );
TClass(wxPoint) wxCaret_GetPosition( TSelf(wxCaret) _obj );
TClass(wxSize) wxCaret_GetSize( TSelf(wxCaret) _obj );
TClass(wxWindow) wxCaret_GetWindow( TSelf(wxCaret) _obj );
void       wxCaret_Hide( TSelf(wxCaret) _obj );
TBool      wxCaret_IsOk( TSelf(wxCaret) _obj );
TBool      wxCaret_IsVisible( TSelf(wxCaret) _obj );
void       wxCaret_Move( TSelf(wxCaret) _obj, TPoint(x,y) );
void       wxCaret_SetBlinkTime( int milliseconds );
void       wxCaret_SetSize( TSelf(wxCaret) _obj, TSize(width,height) );
void       wxCaret_Show( TSelf(wxCaret) _obj );

/* wxCheckBox */
TClassDefExtend(wxCheckBox,wxControl)
TClass(wxCheckBox) wxCheckBox_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TRect(_lft,_top,_wdt,_hgt), int _stl );
void       wxCheckBox_Delete( TSelf(wxCheckBox) _obj );
TBool      wxCheckBox_GetValue( TSelf(wxCheckBox) _obj );
void       wxCheckBox_SetValue( TSelf(wxCheckBox) _obj, TBoolInt value );

/* wxCheckListBox */
TClassDefExtend(wxCheckListBox,wxListBox)
void       wxCheckListBox_Check( TSelf(wxCheckListBox) _obj, int item, TBool check );
TClass(wxCheckListBox) wxCheckListBox_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), TArrayString(n,str), int _stl );
void       wxCheckListBox_Delete( TSelf(wxCheckListBox) _obj );
TBool      wxCheckListBox_IsChecked( TSelf(wxCheckListBox) _obj, int item );

/* wxChoice */
TClassDefExtend(wxChoice,wxControl)
void       wxChoice_Append( TSelf(wxChoice) _obj, TClass(wxString) item );
void       wxChoice_Clear( TSelf(wxChoice) _obj );
TClass(wxChoice) wxChoice_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), TArrayString(n,str), int _stl );
void       wxChoice_Delete( TSelf(wxChoice) _obj, int n );
int        wxChoice_FindString( TSelf(wxChoice) _obj, TClass(wxString) s );
int        wxChoice_GetCount( TSelf(wxChoice) _obj );
int        wxChoice_GetSelection( TSelf(wxChoice) _obj );
TClass(wxString) wxChoice_GetString( TSelf(wxChoice) _obj, int n );
void       wxChoice_SetSelection( TSelf(wxChoice) _obj, int n );
void       wxChoice_SetString( TSelf(wxChoice) _obj, int n, TClass(wxString) s );

/* wxClassInfo */
TClassDef(wxClassInfo)
void*      wxClassInfo_CreateClassByName( TSelf(wxClassInfo) _inf );
void*      wxClassInfo_GetClassName( TSelf(wxClassInfo) _inf );
TBool      wxClassInfo_IsKindOf( TSelf(wxClassInfo) _obj, TClass(wxString) _name );

/* wxClient */
TClassDefExtend(wxClient,wxClientBase)

/* wxClientBase */
TClassDefExtend(wxClientBase,wxObject)

/* wxClientDC */
TClassDefExtend(wxClientDC,wxWindowDC)
TClass(wxClientDC) wxClientDC_Create( TClass(wxWindow) win );
void       wxClientDC_Delete( TSelf(wxClientDC) _obj );

/* wxClientData */
TClassDef(wxClientData)

/* wxClientDataContainer */
TClassDef(wxClientDataContainer)

/* wxClipboard */
TClassDefExtend(wxClipboard,wxObject)
TBool      wxClipboard_AddData( TSelf(wxClipboard) _obj, TClass(wxDataObject) data );
void       wxClipboard_Clear( TSelf(wxClipboard) _obj );
void       wxClipboard_Close( TSelf(wxClipboard) _obj );
TClass(wxClipboard) wxClipboard_Create(  );
TBool      wxClipboard_Flush( TSelf(wxClipboard) _obj );
TBool      wxClipboard_GetData( TSelf(wxClipboard) _obj, TClass(wxDataObject) data );
TBool      wxClipboard_IsOpened( TSelf(wxClipboard) _obj );
TBool      wxClipboard_IsSupported( TSelf(wxClipboard) _obj, TClass(wxDataFormat) format );
TBool      wxClipboard_Open( TSelf(wxClipboard) _obj );
TBool      wxClipboard_SetData( TSelf(wxClipboard) _obj, TClass(wxDataObject) data );
void       wxClipboard_UsePrimarySelection( TSelf(wxClipboard) _obj, TBool primary );

/* wxCloseEvent */
TClassDefExtend(wxCloseEvent,wxEvent)
TBool      wxCloseEvent_CanVeto( TSelf(wxCloseEvent) _obj );
void       wxCloseEvent_CopyObject( TSelf(wxCloseEvent) _obj, TClass(wxObject) obj );
TBool      wxCloseEvent_GetLoggingOff( TSelf(wxCloseEvent) _obj );
TBool      wxCloseEvent_GetVeto( TSelf(wxCloseEvent) _obj );
void       wxCloseEvent_SetCanVeto( TSelf(wxCloseEvent) _obj, TBool canVeto );
void       wxCloseEvent_SetLoggingOff( TSelf(wxCloseEvent) _obj, TBool logOff );
void       wxCloseEvent_Veto( TSelf(wxCloseEvent) _obj, TBool veto );

/* wxClosure */
TClassDefExtend(wxClosure,wxObject)

/* wxColour */
TClassDefExtend(wxColour,wxObject)
TUInt8     wxColour_Alpha( TSelf(wxColour) _obj );
void       wxColour_Assign( TSelf(wxColour) _obj, void* other );
TUInt8     wxColour_Blue( TSelf(wxColour) _obj );
void       wxColour_Copy( TSelf(wxColour) _obj, void* _other );
TClass(wxColour) wxColour_CreateByName( TClass(wxString) _name );
TClass(wxColour) wxColour_CreateEmpty(  );
TClass(wxColour) wxColour_CreateFromStock( int id );
TClass(wxColour) wxColour_CreateRGB( TUInt8 _red, TUInt8 _green, TUInt8 _blue, TUInt8 _alpha );
void       wxColour_Delete( TSelf(wxColour) _obj );
TUInt8     wxColour_Green( TSelf(wxColour) _obj );
TBool      wxColour_IsOk( TSelf(wxColour) _obj );
TUInt8     wxColour_Red( TSelf(wxColour) _obj );
void       wxColour_Set( TSelf(wxColour) _obj, TUInt8 _red, TUInt8 _green, TUInt8 _blue, TUInt8 _alpha );
void       wxColour_SetByName( TSelf(wxColour) _obj, TClass(wxString) _name );
TBool      wxColour_ValidName( TStringVoid _name );

/* wxColourData */
TClassDefExtend(wxColourData,wxObject)
TClass(wxColourData) wxColourData_Create(  );
void       wxColourData_Delete( TSelf(wxColourData) _obj );
TBool      wxColourData_GetChooseFull( TSelf(wxColourData) _obj );
void       wxColourData_GetColour( TSelf(wxColourData) _obj, TClassRef(wxColour) _ref );
void       wxColourData_GetCustomColour( TSelf(wxColourData) _obj, int i, TClassRef(wxColour) _ref );
void       wxColourData_SetChooseFull( TSelf(wxColourData) _obj, TBool flag );
void       wxColourData_SetColour( TSelf(wxColourData) _obj, TClass(wxColour) colour );
void       wxColourData_SetCustomColour( TSelf(wxColourData) _obj, int i, TClass(wxColour) colour );

/* wxColourDatabase */
TClassDefExtend(wxColourDatabase,wxList)

/* wxColourDialog */
TClassDefExtend(wxColourDialog,wxDialog)
TClass(wxColourDialog) wxColourDialog_Create( TClass(wxWindow) _prt, TClass(wxColourData) col );
void       wxColourDialog_GetColourData( TSelf(wxColourDialog) _obj, TClassRef(wxColourData) _ref );

/* wxComboBox */
TClassDefExtend(wxComboBox,wxChoice)
void       wxComboBox_Append( TSelf(wxComboBox) _obj, TClass(wxString) item );
void       wxComboBox_AppendData( TSelf(wxComboBox) _obj, TClass(wxString) item, void* d );
void       wxComboBox_Clear( TSelf(wxComboBox) _obj );
void       wxComboBox_Copy( TSelf(wxComboBox) _obj );
TClass(wxComboBox) wxComboBox_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TRect(_lft,_top,_wdt,_hgt), TArrayString(n,str), int _stl );
void       wxComboBox_Cut( TSelf(wxComboBox) _obj );
void       wxComboBox_Delete( TSelf(wxComboBox) _obj, int n );
int        wxComboBox_FindString( TSelf(wxComboBox) _obj, TClass(wxString) s );
TClass(wxClientData) wxComboBox_GetClientData( TSelf(wxComboBox) _obj, int n );
int        wxComboBox_GetCount( TSelf(wxComboBox) _obj );
int        wxComboBox_GetInsertionPoint( TSelf(wxComboBox) _obj );
int        wxComboBox_GetLastPosition( TSelf(wxComboBox) _obj );
int        wxComboBox_GetSelection( TSelf(wxComboBox) _obj );
TClass(wxString) wxComboBox_GetString( TSelf(wxComboBox) _obj, int n );
TClass(wxString) wxComboBox_GetStringSelection( TSelf(wxComboBox) _obj );
TClass(wxString) wxComboBox_GetValue( TSelf(wxComboBox) _obj );
void       wxComboBox_SetValue( TSelf(wxComboBox) _obj, TClass(wxString) value);
void       wxComboBox_Paste( TSelf(wxComboBox) _obj );
void       wxComboBox_Remove( TSelf(wxComboBox) _obj, int from, int to );
void       wxComboBox_Replace( TSelf(wxComboBox) _obj, int from, int to, TClass(wxString) value );
void       wxComboBox_SetClientData( TSelf(wxComboBox) _obj, int n, TClass(wxClientData) clientData );
void       wxComboBox_SetEditable( TSelf(wxComboBox) _obj, TBool editable );
void       wxComboBox_SetInsertionPoint( TSelf(wxComboBox) _obj, int pos );
void       wxComboBox_SetInsertionPointEnd( TSelf(wxComboBox) _obj );
void       wxComboBox_SetSelection( TSelf(wxComboBox) _obj, int n );
void       wxComboBox_SetTextSelection( TSelf(wxComboBox) _obj, int from, int to );

/* wxCommand */
TClassDefExtend(wxCommand,wxObject)

/* wxCommandEvent */
TClassDefExtend(wxCommandEvent,wxEvent)
void       wxCommandEvent_CopyObject( TSelf(wxCommandEvent) _obj, void* object_dest );
TClass(wxCommandEvent) wxCommandEvent_Create( int _typ, int _id );
void       wxCommandEvent_Delete( TSelf(wxCommandEvent) _obj );
TClass(wxClientData) wxCommandEvent_GetClientData( TSelf(wxCommandEvent) _obj );
TClass(wxClientData) wxCommandEvent_GetClientObject( TSelf(wxCommandEvent) _obj );
long       wxCommandEvent_GetExtraLong( TSelf(wxCommandEvent) _obj );
long       wxCommandEvent_GetInt( TSelf(wxCommandEvent) _obj );
int        wxCommandEvent_GetSelection( TSelf(wxCommandEvent) _obj );
TClass(wxString) wxCommandEvent_GetString( TSelf(wxCommandEvent) _obj );
TBool      wxCommandEvent_IsChecked( TSelf(wxCommandEvent) _obj );
TBool      wxCommandEvent_IsSelection( TSelf(wxCommandEvent) _obj );
void       wxCommandEvent_SetClientData( TSelf(wxCommandEvent) _obj, TClass(wxClientData) clientData );
void       wxCommandEvent_SetClientObject( TSelf(wxCommandEvent) _obj, TClass(wxClientData) clientObject );
void       wxCommandEvent_SetExtraLong( TSelf(wxCommandEvent) _obj, long extraLong );
void       wxCommandEvent_SetInt( TSelf(wxCommandEvent) _obj, int i );
void       wxCommandEvent_SetString( TSelf(wxCommandEvent) _obj, TClass(wxString) s );

/* wxCommandLineParser */
TClassDef(wxCommandLineParser)

/* wxCommandProcessor */
TClassDefExtend(wxCommandProcessor,wxObject)
TBool      wxCommandProcessor_CanRedo( TSelf(wxCommandProcessor) _obj );
TBool      wxCommandProcessor_CanUndo( TSelf(wxCommandProcessor) _obj );
void       wxCommandProcessor_ClearCommands( TSelf(wxCommandProcessor) _obj );
void       wxCommandProcessor_Delete( TSelf(wxCommandProcessor) _obj );
int        wxCommandProcessor_GetCommands( TSelf(wxCommandProcessor) _obj, void* _ref );
void*      wxCommandProcessor_GetEditMenu( TSelf(wxCommandProcessor) _obj );
int        wxCommandProcessor_GetMaxCommands( TSelf(wxCommandProcessor) _obj );
void       wxCommandProcessor_Initialize( TSelf(wxCommandProcessor) _obj );
int        wxCommandProcessor_Redo( TSelf(wxCommandProcessor) _obj );
void       wxCommandProcessor_SetEditMenu( TSelf(wxCommandProcessor) _obj, TClass(wxMenu) menu );
void       wxCommandProcessor_SetMenuStrings( TSelf(wxCommandProcessor) _obj );
int        wxCommandProcessor_Submit( TSelf(wxCommandProcessor) _obj, TClass(wxCommand) command, int storeIt );
int        wxCommandProcessor_Undo( TSelf(wxCommandProcessor) _obj );
void*      wxCommandProcessor_wxCommandProcessor( int maxCommands );

/* wxCondition */
TClassDef(wxCondition)
void       wxCondition_Broadcast( TSelf(wxCondition) _obj );
TClass(wxCondition) wxCondition_Create( void* _mut );
void       wxCondition_Delete( TSelf(wxCondition) _obj );
void       wxCondition_Signal( TSelf(wxCondition) _obj );
void       wxCondition_Wait( TSelf(wxCondition) _obj );
int        wxCondition_WaitFor( TSelf(wxCondition) _obj, int sec, int nsec );

/* wxConfigBase */
TClassDef(wxConfigBase)
TClass(wxConfigBase) wxConfigBase_Create(  );
void       wxConfigBase_Delete( TSelf(wxConfigBase) _obj );
TBool      wxConfigBase_DeleteAll( TSelf(wxConfigBase) _obj );
TBool      wxConfigBase_DeleteEntry( TSelf(wxConfigBase) _obj, TClass(wxString) key, TBool bDeleteGroupIfEmpty );
TBool      wxConfigBase_DeleteGroup( TSelf(wxConfigBase) _obj, TClass(wxString) key );
TBool      wxConfigBase_Exists( TSelf(wxConfigBase) _obj, TClass(wxString) strName );
TClass(wxString) wxConfigBase_ExpandEnvVars( TSelf(wxConfigBase) _obj, TClass(wxString) str );
TBool      wxConfigBase_Flush( TSelf(wxConfigBase) _obj, TBool bCurrentOnly );
TClass(wxString) wxConfigBase_GetAppName( TSelf(wxConfigBase) _obj );
int        wxConfigBase_GetEntryType( TSelf(wxConfigBase) _obj, TClass(wxString) name );
TClass(wxString) wxConfigBase_GetFirstEntry( TSelf(wxConfigBase) _obj, void* lIndex );
TClass(wxString) wxConfigBase_GetFirstGroup( TSelf(wxConfigBase) _obj, void* lIndex );
TClass(wxString) wxConfigBase_GetNextEntry( TSelf(wxConfigBase) _obj, void* lIndex);
TClass(wxString) wxConfigBase_GetNextGroup( TSelf(wxConfigBase) _obj, void* lIndex);
int        wxConfigBase_GetNumberOfEntries( TSelf(wxConfigBase) _obj, TBool bRecursive );
int        wxConfigBase_GetNumberOfGroups( TSelf(wxConfigBase) _obj, TBool bRecursive );
TClass(wxString) wxConfigBase_GetPath( TSelf(wxConfigBase) _obj );
int        wxConfigBase_GetStyle( TSelf(wxConfigBase) _obj );
TClass(wxString) wxConfigBase_GetVendorName( TSelf(wxConfigBase) _obj );
TBool      wxConfigBase_HasEntry( TSelf(wxConfigBase) _obj, TClass(wxString) strName );
TBool      wxConfigBase_HasGroup( TSelf(wxConfigBase) _obj, TClass(wxString) strName );
TBool      wxConfigBase_IsExpandingEnvVars( TSelf(wxConfigBase) _obj );
TBool      wxConfigBase_IsRecordingDefaults( TSelf(wxConfigBase) _obj );
TBool      wxConfigBase_ReadBool( TSelf(wxConfigBase) _obj, TClass(wxString) key, TBool defVal );
double     wxConfigBase_ReadDouble( TSelf(wxConfigBase) _obj, TClass(wxString) key, double defVal );
int        wxConfigBase_ReadInteger( TSelf(wxConfigBase) _obj, TClass(wxString) key, int defVal );
TClass(wxString) wxConfigBase_ReadString( TSelf(wxConfigBase) _obj, TClass(wxString) key, TClass(wxString) defVal );
TBool      wxConfigBase_RenameEntry( TSelf(wxConfigBase) _obj, TClass(wxString) oldName, TClass(wxString) newName );
TBool      wxConfigBase_RenameGroup( TSelf(wxConfigBase) _obj, TClass(wxString) oldName, TClass(wxString) newName );
void       wxConfigBase_SetAppName( TSelf(wxConfigBase) _obj, TClass(wxString) appName );
void       wxConfigBase_SetExpandEnvVars( TSelf(wxConfigBase) _obj, TBool bDoIt );
void       wxConfigBase_SetPath( TSelf(wxConfigBase) _obj, TClass(wxString) strPath );
void       wxConfigBase_SetRecordDefaults( TSelf(wxConfigBase) _obj, TBool bDoIt );
void       wxConfigBase_SetStyle( TSelf(wxConfigBase) _obj, int style );
void       wxConfigBase_SetVendorName( TSelf(wxConfigBase) _obj, TClass(wxString) vendorName );
TBool      wxConfigBase_WriteBool( TSelf(wxConfigBase) _obj, TClass(wxString) key, TBool value );
TBool      wxConfigBase_WriteDouble( TSelf(wxConfigBase) _obj, TClass(wxString) key, double value );
TBool      wxConfigBase_WriteInteger( TSelf(wxConfigBase) _obj, TClass(wxString) key, int value );
TBool      wxConfigBase_WriteLong( TSelf(wxConfigBase) _obj, TClass(wxString) key, long value );
TBool      wxConfigBase_WriteString( TSelf(wxConfigBase) _obj, TClass(wxString) key, TClass(wxString) value );

/* wxConnection */
TClassDefExtend(wxConnection,wxConnectionBase)

/* wxConnectionBase */
TClassDefExtend(wxConnectionBase,wxObject)

/* wxContextHelp */
TClassDefExtend(wxContextHelp,wxObject)
TBool      wxContextHelp_BeginContextHelp( TSelf(wxContextHelp) _obj, TClass(wxWindow) win );
TClass(wxContextHelp) wxContextHelp_Create( TClass(wxWindow) win, TBool beginHelp );
void       wxContextHelp_Delete( TSelf(wxContextHelp) _obj );
TBool      wxContextHelp_EndContextHelp( TSelf(wxContextHelp) _obj );

/* wxContextHelpButton */
TClassDefExtend(wxContextHelpButton,wxBitmapButton)
TClass(wxContextHelpButton) wxContextHelpButton_Create( TClass(wxWindow) parent, int id, TRect(x,y,w,h), long style );

/* wxControl */
TClassDefExtend(wxControl,wxWindow)
void       wxControl_Command( TSelf(wxControl) _obj, TClass(wxEvent) event );
TClass(wxString) wxControl_GetLabel( TSelf(wxControl) _obj );
void       wxControl_SetLabel( TSelf(wxControl) _obj, TClass(wxString) text );

/* wxCountingOutputStream */
TClassDefExtend(wxCountingOutputStream,wxOutputStream)

/* wxCriticalSection */
TClassDef(wxCriticalSection)
TClass(wxCriticalSection) wxCriticalSection_Create(  );
void       wxCriticalSection_Delete( TSelf(wxCriticalSection) _obj );
void       wxCriticalSection_Enter( TSelf(wxCriticalSection) _obj );
void       wxCriticalSection_Leave( TSelf(wxCriticalSection) _obj );

/* wxCriticalSectionLocker */
TClassDef(wxCriticalSectionLocker)

/* wxCursor */
TClassDefExtend(wxCursor,wxBitmap)
TClass(wxCursor)  Cursor_CreateFromStock( int _id );
TClass(wxCursor)  Cursor_CreateFromImage( TClass(wxImage) image );
TClass(wxCursor)  Cursor_CreateLoad( TClass(wxString) name, long type, TSize(width,height) );

/* wxCustomDataObject */
TClassDefExtend(wxCustomDataObject,wxDataObjectSimple)

/* wxDC */
TClassDefExtend(wxDC,wxObject)
TBool      wxDC_Blit( TSelf(wxDC) _obj, TRect(xdest,ydest,width,height), TClass(wxDC) source, TPoint(xsrc,ysrc), int rop, TBool useMask );
void       wxDC_CalcBoundingBox( TSelf(wxDC) _obj, TPoint(x,y) );
TBool      wxDC_CanDrawBitmap( TSelf(wxDC) _obj );
TBool      wxDC_CanGetTextExtent( TSelf(wxDC) _obj );
void       wxDC_Clear( TSelf(wxDC) _obj );
void       wxDC_ComputeScaleAndOrigin( TSelf(wxDC) obj );
void       wxDC_CrossHair( TSelf(wxDC) _obj, TPoint(x,y) );
void       wxDC_Delete( TSelf(wxDC) _obj );
void       wxDC_DestroyClippingRegion( TSelf(wxDC) _obj );
int        wxDC_DeviceToLogicalX( TSelf(wxDC) _obj, int x );
int        wxDC_DeviceToLogicalXRel( TSelf(wxDC) _obj, int x );
int        wxDC_DeviceToLogicalY( TSelf(wxDC) _obj, int y );
int        wxDC_DeviceToLogicalYRel( TSelf(wxDC) _obj, int y );
void       wxDC_DrawArc( TSelf(wxDC) _obj, TPoint(x1,y1), TPoint(x2,y2), TPoint(xc,yc) );
void       wxDC_DrawBitmap( TSelf(wxDC) _obj, TClass(wxBitmap) bmp, TPoint(x,y), TBool useMask );
void       wxDC_DrawCheckMark( TSelf(wxDC) _obj, TRect(x,y,width,height) );
void       wxDC_DrawCircle( TSelf(wxDC) _obj, TPoint(x,y), int radius );
void       wxDC_DrawEllipse( TSelf(wxDC) _obj, TRect(x,y,width,height) );
void       wxDC_DrawEllipticArc( TSelf(wxDC) _obj, TRect(x,y,w,h), double sa, double ea );
void       wxDC_DrawIcon( TSelf(wxDC) _obj, TClass(wxIcon) icon, TPoint(x,y) );
void       wxDC_DrawLabel( TSelf(wxDC) _obj, TClass(wxString) str, TRect(x,y,w,h), int align, int indexAccel);
TClass(wxRect) wxDC_DrawLabelBitmap( TSelf(wxDC) _obj, TClass(wxString) str, TClass(wxBitmap) bmp, TRect(x,y,w,h), int align, int indexAccel );
void       wxDC_DrawLine( TSelf(wxDC) _obj, TPoint(x1,y1), TPoint(x2,y2) );
void       wxDC_DrawLines( TSelf(wxDC) _obj, int n, void* x, void* y, TPoint(xoffset,yoffset) );
void       wxDC_DrawPoint( TSelf(wxDC) _obj, TPoint(x,y) );
void       wxDC_DrawPolygon( TSelf(wxDC) _obj, int n, void* x, void* y, TPoint(xoffset,yoffset), int fillStyle );
void       wxDC_DrawPolyPolygon( TSelf(wxDC) _obj, int n, void *count, void *x, void *y, TPoint(xoffset,yoffset), int fillStyle);
void       wxDC_DrawRectangle( TSelf(wxDC) _obj, TRect(x,y,width,height) );
void       wxDC_DrawRotatedText( TSelf(wxDC) _obj, TClass(wxString) text, TPoint(x,y), double angle );
void       wxDC_DrawRoundedRectangle( TSelf(wxDC) _obj, TRect(x,y,width,height), double radius );
void       wxDC_DrawText( TSelf(wxDC) _obj, TClass(wxString) text, TPoint(x,y) );
void       wxDC_EndDoc( TSelf(wxDC) _obj );
void       wxDC_EndPage( TSelf(wxDC) _obj );
void       wxDC_FloodFill( TSelf(wxDC) _obj, TPoint(x,y), TClass(wxColour) col, int style );
void       wxDC_GetBackground( TSelf(wxDC) _obj, TClassRef(wxBrush) _ref );
int        wxDC_GetBackgroundMode( TSelf(wxDC) _obj );
void       wxDC_GetBrush( TSelf(wxDC) _obj, TClassRef(wxBrush) _ref );
int        wxDC_GetCharHeight( TSelf(wxDC) _obj );
int        wxDC_GetCharWidth( TSelf(wxDC) _obj );
void       wxDC_GetClippingBox( TSelf(wxDC) _obj, TRectOutVoid(_x,_y,_w,_h) );
int        wxDC_GetDepth( TSelf(wxDC) _obj );
void       wxDC_GetDeviceOrigin( TSelf(wxDC) _obj, TPointOutVoid(_x,_y) );
void       wxDC_GetFont( TSelf(wxDC) _obj, TClassRef(wxFont) _ref );
int        wxDC_GetLogicalFunction( TSelf(wxDC) _obj );
void       wxDC_GetLogicalOrigin( TSelf(wxDC) _obj, TPointOutVoid(_x,_y) );
void       wxDC_GetLogicalScale( TSelf(wxDC) _obj, TSizeOutDouble(_x,_y) );
int        wxDC_GetMapMode( TSelf(wxDC) _obj );
TClass(wxSize) wxDC_GetPPI( TSelf(wxDC) _obj );
void       wxDC_GetPen( TSelf(wxDC) _obj, TClassRef(wxPen) _ref );
TBool      wxDC_GetPixel( TSelf(wxDC) _obj, TPoint(x,y), TClass(wxColour) col );
TClass(wxSize) wxDC_GetSize( TSelf(wxDC) _obj );
TClass(wxSize) wxDC_GetSizeMM( TSelf(wxDC) _obj );
void       wxDC_GetTextBackground( TSelf(wxDC) _obj, TClassRef(wxColour) _ref );
void       wxDC_GetTextExtent( TSelf(wxDC) self, TClass(wxString) string, void* w, void* h, void* descent, void* externalLeading, TClass(wxFont) theFont );
void       wxDC_GetMultiLineTextExtent( TSelf(wxDC) self, TClass(wxString) string, void* w, void* h, void* heightLine, TClass(wxFont) theFont );
void       wxDC_GetTextForeground( TSelf(wxDC) _obj, TClassRef(wxColour) _ref );
void       wxDC_GetUserScale( TSelf(wxDC) _obj, TSizeOutDouble(x, y) );
int        wxDC_LogicalToDeviceX( TSelf(wxDC) _obj, int x );
int        wxDC_LogicalToDeviceXRel( TSelf(wxDC) _obj, int x );
int        wxDC_LogicalToDeviceY( TSelf(wxDC) _obj, int y );
int        wxDC_LogicalToDeviceYRel( TSelf(wxDC) _obj, int y );
int        wxDC_MaxX( TSelf(wxDC) _obj );
int        wxDC_MaxY( TSelf(wxDC) _obj );
int        wxDC_MinX( TSelf(wxDC) _obj );
int        wxDC_MinY( TSelf(wxDC) _obj );
TBool      wxDC_IsOk( TSelf(wxDC) _obj );
void       wxDC_ResetBoundingBox( TSelf(wxDC) _obj );
void       wxDC_SetAxisOrientation( TSelf(wxDC) _obj, TBool xLeftRight, TBool yBottomUp );
void       wxDC_SetBackground( TSelf(wxDC) _obj, TClass(wxBrush) brush );
void       wxDC_SetBackgroundMode( TSelf(wxDC) _obj, int mode );
void       wxDC_SetBrush( TSelf(wxDC) _obj, TClass(wxBrush) brush );
void       wxDC_SetClippingRegion( TSelf(wxDC) _obj, TRect(x,y,width,height) );
void       wxDC_SetClippingRegionFromRegion( TSelf(wxDC) _obj, TClass(wxRegion) region );
#if wxCHECK_VERSION(2,9,5)
void       wxDC_SetDeviceClippingRegion( TSelf(wxDC) _obj, TClass(wxRegion) region );
#endif
void       wxDC_SetDeviceOrigin( TSelf(wxDC) _obj, TPoint(x,y) );
void       wxDC_SetFont( TSelf(wxDC) _obj, TClass(wxFont) font );
void       wxDC_SetLogicalFunction( TSelf(wxDC) _obj, int function );
void       wxDC_SetLogicalOrigin( TSelf(wxDC) _obj, TPoint(x,y) );
void       wxDC_SetLogicalScale( TSelf(wxDC) _obj, double x, double y );
void       wxDC_SetMapMode( TSelf(wxDC) _obj, int mode );
void       wxDC_SetPalette( TSelf(wxDC) _obj, TClass(wxPalette) palette );
void       wxDC_SetPen( TSelf(wxDC) _obj, TClass(wxPen) pen );
void       wxDC_SetTextBackground( TSelf(wxDC) _obj, TClass(wxColour) colour );
void       wxDC_SetTextForeground( TSelf(wxDC) _obj, TClass(wxColour) colour );
void       wxDC_SetUserScale( TSelf(wxDC) _obj, double x, double y );
TBool      wxDC_StartDoc( TSelf(wxDC) _obj, TClass(wxString) msg );
void       wxDC_StartPage( TSelf(wxDC) _obj );

/* wxDCClipper */
TClassDef(wxDCClipper)

/* wxDDEClient */
TClassDefExtend(wxDDEClient,wxClientBase)

/* wxDDEConnection */
TClassDefExtend(wxDDEConnection,wxConnectionBase)

/* wxDDEServer */
TClassDefExtend(wxDDEServer,wxServerBase)

/* wxDataFormat */
TClassDef(wxDataFormat)
TClass(wxDataFormat) wxDataFormat_CreateFromId( TClass(wxString) name );
TClass(wxDataFormat) wxDataFormat_CreateFromType( int typ );
void       wxDataFormat_Delete( TSelf(wxDataFormat) _obj );
TClass(wxString) wxDataFormat_GetId( TSelf(wxDataFormat) _obj );
int        wxDataFormat_GetType( TSelf(wxDataFormat) _obj );
TBool      wxDataFormat_IsEqual( TSelf(wxDataFormat) _obj, void* other );
void       wxDataFormat_SetId( TSelf(wxDataFormat) _obj, void* id );
void       wxDataFormat_SetType( TSelf(wxDataFormat) _obj, int typ );

/* wxDataInputStream */
TClassDef(wxDataInputStream)

/* wxDataObject */
TClassDef(wxDataObject)

/* wxDataObjectComposite */
TClassDefExtend(wxDataObjectComposite,wxDataObject)
void       wxDataObjectComposite_Add( TSelf(wxDataObjectComposite) _obj, void* _dat, int _preferred );
TClass(wxDataObjectComposite) wxDataObjectComposite_Create(  );
void       wxDataObjectComposite_Delete( TSelf(wxDataObjectComposite) _obj );

/* wxDataObjectSimple */
TClassDefExtend(wxDataObjectSimple,wxDataObject)

/* wxDataOutputStream */
TClassDef(wxDataOutputStream)

/* wxDatabase */
TClassDefExtend(wxDatabase,wxObject)

/* wxDateTime */
TClassDef(wxDateTime)
void       wxDateTime_AddDate( TSelf(wxDateTime) _obj, void* diff, TClassRef(wxDateTime) _ref );
void       wxDateTime_AddDateValues( TSelf(wxDateTime) _obj, int _yrs, int _mnt, int _wek, int _day );
void       wxDateTime_AddTime( TSelf(wxDateTime) _obj, void* diff, TClassRef(wxDateTime) _ref );
void       wxDateTime_AddTimeValues( TSelf(wxDateTime) _obj, int _hrs, int _min, int _sec, int _mls );
int        wxDateTime_ConvertYearToBC( int year );
TClass(wxDateTime) wxDateTime_Create( );
TClass(wxString) wxDateTime_Format( TSelf(wxDateTime) _obj, void* format, int tz );
TClass(wxString) wxDateTime_FormatDate( TSelf(wxDateTime) _obj );
TClass(wxString) wxDateTime_FormatISODate( TSelf(wxDateTime) _obj );
TClass(wxString) wxDateTime_FormatISOTime( TSelf(wxDateTime) _obj );
TClass(wxString) wxDateTime_FormatTime( TSelf(wxDateTime) _obj );
TClass(wxString) wxDateTime_GetAmString( );
void       wxDateTime_GetBeginDST( int year, int country, TClass(wxDateTime) dt );
int        wxDateTime_GetCentury( int year );
int        wxDateTime_GetCountry(  );
int        wxDateTime_GetCurrentMonth( int cal );
int        wxDateTime_GetCurrentYear( int cal );
int        wxDateTime_GetDay( TSelf(wxDateTime) _obj, int tz );
int        wxDateTime_GetDayOfYear( TSelf(wxDateTime) _obj, int tz );
void       wxDateTime_GetEndDST( int year, int country, TClass(wxDateTime) dt );
int        wxDateTime_GetHour( TSelf(wxDateTime) _obj, int tz );
void       wxDateTime_GetLastMonthDay( TSelf(wxDateTime) _obj, int month, int year, TClassRef(wxDateTime) _ref );
void       wxDateTime_GetLastWeekDay( TSelf(wxDateTime) _obj, int weekday, int month, int year, TClassRef(wxDateTime) _ref );
int        wxDateTime_GetMillisecond( TSelf(wxDateTime) _obj, int tz );
int        wxDateTime_GetMinute( TSelf(wxDateTime) _obj, int tz );
int        wxDateTime_GetMonth( TSelf(wxDateTime) _obj, int tz );
TClass(wxString) wxDateTime_GetMonthName( int month, int flags );
void       wxDateTime_GetNextWeekDay( TSelf(wxDateTime) _obj, int weekday, TClassRef(wxDateTime) _ref );
int        wxDateTime_GetNumberOfDays( int year, int cal );
int        wxDateTime_GetNumberOfDaysMonth( int month, int year, int cal );
TClass(wxString) wxDateTime_GetPmString( );
void       wxDateTime_GetPrevWeekDay( TSelf(wxDateTime) _obj, int weekday, TClassRef(wxDateTime) _ref );
int        wxDateTime_GetSecond( TSelf(wxDateTime) _obj, int tz );
time_t     wxDateTime_GetTicks( TSelf(wxDateTime) _obj );
int        wxDateTime_GetTimeNow(  );
void       wxDateTime_GetValue( TSelf(wxDateTime) _obj, void* hi_long, void* lo_long );
void       wxDateTime_GetWeekDay( TSelf(wxDateTime) _obj, int weekday, int n, int month, int year, TClassRef(wxDateTime) _ref );
void       wxDateTime_GetWeekDayInSameWeek( TSelf(wxDateTime) _obj, int weekday, TClassRef(wxDateTime) _ref );
TClass(wxString) wxDateTime_GetWeekDayName( int weekday, int flags );
int        wxDateTime_GetWeekDayTZ( TSelf(wxDateTime) _obj, int tz );
int        wxDateTime_GetWeekOfMonth( TSelf(wxDateTime) _obj, int flags, int tz );
int        wxDateTime_GetWeekOfYear( TSelf(wxDateTime) _obj, int flags, int tz );
int        wxDateTime_GetYear( TSelf(wxDateTime) _obj, int tz );
TBool      wxDateTime_IsBetween( TSelf(wxDateTime) _obj, TClass(wxDateTime) t1, TClass(wxDateTime) t2 );
TBool      wxDateTime_IsDST( TSelf(wxDateTime) _obj, int country );
TBool      wxDateTime_IsDSTApplicable( int year, int country );
TBool      wxDateTime_IsEarlierThan( TSelf(wxDateTime) _obj, void* datetime );
TBool      wxDateTime_IsEqualTo( TSelf(wxDateTime) _obj, void* datetime );
TBool      wxDateTime_IsEqualUpTo( TSelf(wxDateTime) _obj, TClass(wxDateTime) dt, void* ts );
TBool      wxDateTime_IsGregorianDate( TSelf(wxDateTime) _obj, int country );
TBool      wxDateTime_IsLaterThan( TSelf(wxDateTime) _obj, void* datetime );
TBool      wxDateTime_IsLeapYear( int year, int cal );
TBool      wxDateTime_IsSameDate( TSelf(wxDateTime) _obj, TClass(wxDateTime) dt );
TBool      wxDateTime_IsSameTime( TSelf(wxDateTime) _obj, TClass(wxDateTime) dt );
TBool      wxDateTime_IsStrictlyBetween( TSelf(wxDateTime) _obj, TClass(wxDateTime) t1, TClass(wxDateTime) t2 );
TBool      wxDateTime_IsValid( TSelf(wxDateTime) _obj );
TBool      wxDateTime_IsWestEuropeanCountry( int country );
TBool      wxDateTime_IsWorkDay( TSelf(wxDateTime) _obj, int country );
void       wxDateTime_MakeGMT( TSelf(wxDateTime) _obj, int noDST );
void       wxDateTime_MakeTimezone( TSelf(wxDateTime) _obj, int tz, int noDST );
void       wxDateTime_Now( TSelf(wxDateTime) dt );
void*      wxDateTime_ParseDate( TSelf(wxDateTime) _obj, void* date );
void*      wxDateTime_ParseDateTime( TSelf(wxDateTime) _obj, void* datetime );
void*      wxDateTime_ParseFormat( TSelf(wxDateTime) _obj, void* date, void* format, void* dateDef );
void*      wxDateTime_ParseRfc822Date( TSelf(wxDateTime) _obj, void* date );
void*      wxDateTime_ParseTime( TSelf(wxDateTime) _obj, TClass(wxTime) time );
void       wxDateTime_ResetTime( TSelf(wxDateTime) _obj );
void       wxDateTime_Set( TSelf(wxDateTime) _obj, int day, int month, int year, int hour, int minute, int second, int millisec );
void       wxDateTime_SetCountry( int country );
void       wxDateTime_SetDay( TSelf(wxDateTime) _obj, int day );
void       wxDateTime_SetHour( TSelf(wxDateTime) _obj, int hour );
void       wxDateTime_SetMillisecond( TSelf(wxDateTime) _obj, int millisecond );
void       wxDateTime_SetMinute( TSelf(wxDateTime) _obj, int minute );
void       wxDateTime_SetMonth( TSelf(wxDateTime) _obj, int month );
void       wxDateTime_SetSecond( TSelf(wxDateTime) _obj, int second );
void       wxDateTime_SetTime( TSelf(wxDateTime) _obj, int hour, int minute, int second, int millisec );
void       wxDateTime_SetToCurrent( TSelf(wxDateTime) _obj );
void       wxDateTime_SetToLastMonthDay( TSelf(wxDateTime) _obj, int month, int year );
TBool      wxDateTime_SetToLastWeekDay( TSelf(wxDateTime) _obj, int weekday, int month, int year );
void       wxDateTime_SetToNextWeekDay( TSelf(wxDateTime) _obj, int weekday );
void       wxDateTime_SetToPrevWeekDay( TSelf(wxDateTime) _obj, int weekday );
TBool      wxDateTime_SetToWeekDay( TSelf(wxDateTime) _obj, int weekday, int n, int month, int year );
void       wxDateTime_SetToWeekDayInSameWeek( TSelf(wxDateTime) _obj, int weekday );
void       wxDateTime_SetYear( TSelf(wxDateTime) _obj, int year );
void       wxDateTime_SubtractDate( TSelf(wxDateTime) _obj, void* diff, TClassRef(wxDateTime) _ref );
void       wxDateTime_SubtractTime( TSelf(wxDateTime) _obj, void* diff, TClassRef(wxDateTime) _ref );
void       wxDateTime_ToGMT( TSelf(wxDateTime) _obj, int noDST );
void       wxDateTime_ToTimezone( TSelf(wxDateTime) _obj, int tz, int noDST );
void       wxDateTime_Today( TSelf(wxDateTime) dt );
void       wxDateTime_UNow( TSelf(wxDateTime) dt );
void*      wxDateTime_wxDateTime( int hi_long, int lo_long );

/* wxDb */
TClassDef(wxDb)

/* wxDbColDef */
TClassDef(wxDbColDef)

/* wxDbColFor */
TClassDef(wxDbColFor)

/* wxDbColInf */
TClassDef(wxDbColInf)

/* wxDbConnectInf */
TClassDef(wxDbConnectInf)

/* wxDbInf */
TClassDef(wxDbInf)

/* wxDbSqlTypeInfo */
TClassDef(wxDbSqlTypeInfo)

/* wxDbTable */
TClassDef(wxDbTable)

/* wxDbTableInfo */
TClassDef(wxDbTableInfo)

/* wxDebugContext */
TClassDef(wxDebugContext)

/* wxDialUpEvent */
TClassDefExtend(wxDialUpEvent,wxEvent)
TBool      wxDialUpEvent_IsConnectedEvent( TSelf(wxDialUpEvent) _obj );
TBool      wxDialUpEvent_IsOwnEvent( TSelf(wxDialUpEvent) _obj );

/* wxDialUpManager */
TClassDef(wxDialUpManager)
TBool      wxDialUpManager_CancelDialing( TSelf(wxDialUpManager) _obj );
TClass(wxDialUpManager) wxDialUpManager_Create(  );
void       wxDialUpManager_Delete( TSelf(wxDialUpManager) _obj );
TBool      wxDialUpManager_Dial( TSelf(wxDialUpManager) _obj, TClass(wxString) nameOfISP, TClass(wxString) username, TClass(wxString) password, TBool async );
void       wxDialUpManager_DisableAutoCheckOnlineStatus( TSelf(wxDialUpManager) _obj );
TBool      wxDialUpManager_EnableAutoCheckOnlineStatus( TSelf(wxDialUpManager) _obj, int nSeconds );
int        wxDialUpManager_GetISPNames( TSelf(wxDialUpManager) _obj, TClass(wxList) _lst );
TBool      wxDialUpManager_HangUp( TSelf(wxDialUpManager) _obj );
TBool      wxDialUpManager_IsAlwaysOnline( TSelf(wxDialUpManager) _obj );
TBool      wxDialUpManager_IsDialing( TSelf(wxDialUpManager) _obj );
TBool      wxDialUpManager_IsOk( TSelf(wxDialUpManager) _obj );
TBool      wxDialUpManager_IsOnline( TSelf(wxDialUpManager) _obj );
void       wxDialUpManager_SetConnectCommand( TSelf(wxDialUpManager) _obj, TClass(wxString) commandDial, TClass(wxString) commandHangup );
void       wxDialUpManager_SetOnlineStatus( TSelf(wxDialUpManager) _obj, TBool isOnline );
void       wxDialUpManager_SetWellKnownHost( TSelf(wxDialUpManager) _obj, TClass(wxString) hostname, int portno );

/* wxDialog */
TClassDefExtend(wxDialog,wxTopLevelWindow)
TClass(wxDialog) wxDialog_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TRect(_lft,_top,_wdt,_hgt), int _stl );
void       wxDialog_EndModal( TSelf(wxDialog) _obj, int retCode );
int        wxDialog_GetReturnCode( TSelf(wxDialog) _obj );
TBool      wxDialog_IsModal( TSelf(wxDialog) _obj );
void       wxDialog_SetReturnCode( TSelf(wxDialog) _obj, int returnCode );
int        wxDialog_ShowModal( TSelf(wxDialog) _obj );

/* wxDirDialog */
TClassDefExtend(wxDirDialog,wxDialog)
TClass(wxDirDialog) wxDirDialog_Create( TClass(wxWindow) _prt, TClass(wxString) _msg, TClass(wxString) _dir, TPoint(_lft,_top), int _stl );
TClass(wxString) wxDirDialog_GetMessage( TSelf(wxDirDialog) _obj );
TClass(wxString) wxDirDialog_GetPath( TSelf(wxDirDialog) _obj );
int        wxDirDialog_GetStyle( TSelf(wxDirDialog) _obj );
void       wxDirDialog_SetMessage( TSelf(wxDirDialog) _obj, TClass(wxString) msg );
void       wxDirDialog_SetPath( TSelf(wxDirDialog) _obj, TClass(wxString) pth );
void       wxDirDialog_SetStyle( TSelf(wxDirDialog) _obj, int style );

/* wxDirTraverser */
TClassDef(wxDirTraverser)

/* wxDllLoader */
TClassDef(wxDllLoader)
/*
void*      wxDllLoader_GetSymbol( int _handle, TStringVoid _name );
int        wxDllLoader_LoadLibrary( TStringVoid _name, void* _success );
void       wxDllLoader_UnloadLibrary( int _handle );
*/

/* wxDocChildFrame */
TClassDefExtend(wxDocChildFrame,wxFrame)

/* wxDocMDIChildFrame */
TClassDefExtend(wxDocMDIChildFrame,wxMDIChildFrame)

/* wxDocMDIParentFrame */
TClassDefExtend(wxDocMDIParentFrame,wxMDIParentFrame)

/* wxDocManager */
TClassDefExtend(wxDocManager,wxEvtHandler)

/* wxDocParentFrame */
TClassDefExtend(wxDocParentFrame,wxFrame)

/* wxDocTemplate */
TClassDefExtend(wxDocTemplate,wxObject)

/* wxDocument */
TClassDefExtend(wxDocument,wxEvtHandler)

/* wxDragImage */
TClassDefExtend(wxDragImage,wxObject)

/* wxDrawControl */
TClassDefExtend(wxDrawControl,wxControl)
TClass(wxDrawControl) wxDrawControl_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );

/* wxDrawWindow */
TClassDefExtend(wxDrawWindow,wxWindow)
TClass(wxDrawWindow) wxDrawWindow_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );

/* wxDropFilesEvent */
TClassDefExtend(wxDropFilesEvent,wxEvent)

/* wxDropSource */
TClassDef(wxDropSource)
TClass(wxDropSource) DropSource_Create( TClass(wxDataObject) data, TClass(wxWindow) win, void* copy, void* move, void* none );
void       DropSource_Delete( TSelf(wxDropSource) _obj );
int        DropSource_DoDragDrop( TSelf(wxDropSource) _obj, int _move );

/* wxDropTarget */
TClassDef(wxDropTarget)
void       wxDropTarget_GetData( TSelf(wxDropTarget) _obj );
void       wxDropTarget_SetDataObject( TSelf(wxDropTarget) _obj, TClass(wxDataObject) _dat );

/* wxDynToolInfo */
TClassDefExtend(wxDynToolInfo,wxToolLayoutItem)
int        wxDynToolInfo_Index( TSelf(wxDynToolInfo) _obj );
void       wxDynToolInfo_RealSize( TSelf(wxDynToolInfo) _obj, TSizeOutVoid(_w,_h) );
void*      wxDynToolInfo_pToolWnd( TSelf(wxDynToolInfo) _obj );

/* wxDynamicLibrary */
TClassDef(wxDynamicLibrary)

/* wxDynamicSashWindow */
TClassDefExtend(wxDynamicSashWindow,wxWindow)
TClass(wxDynamicSashWindow) wxDynamicSashWindow_Create( TClass(wxWindow) parent, int id, TRect(x,y,w,h), int style );
void       wxDynamicSashWindow_Delete( TSelf(wxDynamicSashWindow) _obj );
void*      wxDynamicSashWindow_GetHScrollBar( TSelf(wxDynamicSashWindow) _obj, TClass(wxWindow) child );
void*      wxDynamicSashWindow_GetVScrollBar( TSelf(wxDynamicSashWindow) _obj, TClass(wxWindow) child );

/* wxDynamicToolBar */
TClassDefExtend(wxDynamicToolBar,wxToolBarBase)
void       wxDynamicToolBar_AddSeparator( TSelf(wxDynamicToolBar) _obj, void* pSepartorWnd );
void       wxDynamicToolBar_AddTool( TSelf(wxDynamicToolBar) _obj, int toolIndex, void* pToolWindow, TSize(w,h) );
void*      wxDynamicToolBar_AddToolBitmap( TSelf(wxDynamicToolBar) _obj, int toolIndex, TClass(wxBitmap) bitmap, void* pushedBitmap, int toggle, TPoint(x,y), TClass(wxClientData) clientData, void* helpString1, void* helpString2 );
void       wxDynamicToolBar_AddToolImage( TSelf(wxDynamicToolBar) _obj, int toolIndex, void* imageFileName, int imageFileType, void* labelText, int alignTextRight, TBool isFlat );
void       wxDynamicToolBar_AddToolLabel( TSelf(wxDynamicToolBar) _obj, int toolIndex, void* labelBmp, void* labelText, int alignTextRight, TBool isFlat );
TClass(wxDynamicToolBar) wxDynamicToolBar_Create( TClass(wxWindow) parent, int id, TRect(x,y,w,h), int style, int orientation, int RowsOrColumns );
TClass(wxDynamicToolBar) wxDynamicToolBar_CreateDefault(  );
void*      wxDynamicToolBar_CreateDefaultLayout( TSelf(wxDynamicToolBar) _obj );
int        wxDynamicToolBar_CreateParams( TSelf(wxDynamicToolBar) _obj, TClass(wxWindow) parent, int id, TRect(x,y,w,h), int style, int orientation, int RowsOrColumns );
void*      wxDynamicToolBar_CreateTool( TSelf(wxDynamicToolBar) _obj, int id, void* label, void* bmpNormal, void* bmpDisabled, int kind, TClass(wxClientData) clientData, void* shortHelp, void* longHelp );
void*      wxDynamicToolBar_CreateToolControl( TSelf(wxDynamicToolBar) _obj, TClass(wxControl) control );
void       wxDynamicToolBar_Delete( TSelf(wxDynamicToolBar) _obj );
int        wxDynamicToolBar_DoDeleteTool( TSelf(wxDynamicToolBar) _obj, int pos, void* tool );
void       wxDynamicToolBar_DoEnableTool( TSelf(wxDynamicToolBar) _obj, void* tool, TBool enable );
int        wxDynamicToolBar_DoInsertTool( TSelf(wxDynamicToolBar) _obj, int pos, void* tool );
void       wxDynamicToolBar_DoSetToggle( TSelf(wxDynamicToolBar) _obj, void* tool, int toggle );
void       wxDynamicToolBar_DoToggleTool( TSelf(wxDynamicToolBar) _obj, void* tool, int toggle );
void       wxDynamicToolBar_DrawSeparator( TSelf(wxDynamicToolBar) _obj, void* info, TClass(wxDC) dc );
void       wxDynamicToolBar_EnableTool( TSelf(wxDynamicToolBar) _obj, int toolIndex, TBool enable );
void*      wxDynamicToolBar_FindToolForPosition( TSelf(wxDynamicToolBar) _obj, TPoint(x,y) );
void       wxDynamicToolBar_GetPreferredDim( TSelf(wxDynamicToolBar) _obj, int gw, int gh, void* pw, void* ph );
void*      wxDynamicToolBar_GetToolInfo( TSelf(wxDynamicToolBar) _obj, int toolIndex );
int        wxDynamicToolBar_Layout( TSelf(wxDynamicToolBar) _obj );
void       wxDynamicToolBar_RemoveTool( TSelf(wxDynamicToolBar) _obj, int toolIndex );
void       wxDynamicToolBar_SetLayout( TSelf(wxDynamicToolBar) _obj, void* pLayout );

/* wxEditableListBox */
TClassDefExtend(wxEditableListBox,wxPanel)
TClass(wxEditableListBox) wxEditableListBox_Create( TClass(wxWindow) parent, int id, TStringVoid label, TRect(x,y,w,h), int style );
void*      wxEditableListBox_GetDelButton( TSelf(wxEditableListBox) _obj );
void*      wxEditableListBox_GetDownButton( TSelf(wxEditableListBox) _obj );
void*      wxEditableListBox_GetEditButton( TSelf(wxEditableListBox) _obj );
TClass(wxListCtrl) wxEditableListBox_GetListCtrl( TSelf(wxEditableListBox) _obj );
void*      wxEditableListBox_GetNewButton( TSelf(wxEditableListBox) _obj );
TArrayLen  wxEditableListBox_GetStrings( TSelf(wxEditableListBox) _obj, TArrayStringOutVoid _ref );
void*      wxEditableListBox_GetUpButton( TSelf(wxEditableListBox) _obj );
void       wxEditableListBox_SetStrings( TSelf(wxEditableListBox) _obj, void* strings, int _n );

/* wxEncodingConverter */
TClassDefExtend(wxEncodingConverter,wxObject)
void       wxEncodingConverter_Convert( TSelf(wxEncodingConverter) _obj, void* input, void* output );
TClass(wxEncodingConverter) wxEncodingConverter_Create(  );
void       wxEncodingConverter_Delete( TSelf(wxEncodingConverter) _obj );
int        wxEncodingConverter_GetAllEquivalents( TSelf(wxEncodingConverter) _obj, int enc, TClass(wxList) _lst );
int        wxEncodingConverter_GetPlatformEquivalents( TSelf(wxEncodingConverter) _obj, int enc, int platform, TClass(wxList) _lst );
int        wxEncodingConverter_Init( TSelf(wxEncodingConverter) _obj, int input_enc, int output_enc, int method );

/* wxEraseEvent */
TClassDefExtend(wxEraseEvent,wxEvent)
void       wxEraseEvent_CopyObject( TSelf(wxEraseEvent) _obj, void* obj );
TClass(wxDC) wxEraseEvent_GetDC( TSelf(wxEraseEvent) _obj );

/* wxEvent */
TClassDefExtend(wxEvent,wxObject)
void       wxEvent_CopyObject( TSelf(wxEvent) _obj, void* object_dest );
TClass(wxObject) wxEvent_GetEventObject( TSelf(wxEvent) _obj );
int        wxEvent_GetEventType( TSelf(wxEvent) _obj );
int        wxEvent_GetId( TSelf(wxEvent) _obj );
TBool      wxEvent_GetSkipped( TSelf(wxEvent) _obj );
int        wxEvent_GetTimestamp( TSelf(wxEvent) _obj );
TBool      wxEvent_IsCommandEvent( TSelf(wxEvent) _obj );
int        wxEvent_NewEventType(  );
void       wxEvent_SetEventObject( TSelf(wxEvent) _obj, TClass(wxObject) obj );
void       wxEvent_SetEventType( TSelf(wxEvent) _obj, int typ );
void       wxEvent_SetId( TSelf(wxEvent) _obj, int Id );
void       wxEvent_SetTimestamp( TSelf(wxEvent) _obj, int ts );
void       wxEvent_Skip( TSelf(wxEvent) _obj );

/* wxEvtHandler */
TClassDefExtend(wxEvtHandler,wxObject)
void       wxEvtHandler_AddPendingEvent( TSelf(wxEvtHandler) _obj, TClass(wxEvent) event );
int        wxEvtHandler_Connect( TSelf(wxEvtHandler) _obj, int first, int last, int type, void* data );
TClass(wxEvtHandler) wxEvtHandler_Create(  );
void       wxEvtHandler_Delete( TSelf(wxEvtHandler) _obj );
int        wxEvtHandler_Disconnect( TSelf(wxEvtHandler) _obj, int first, int last, int type, int id );
TBool      wxEvtHandler_GetEvtHandlerEnabled( TSelf(wxEvtHandler) _obj );
TClass(wxEvtHandler) wxEvtHandler_GetNextHandler( TSelf(wxEvtHandler) _obj );
TClass(wxEvtHandler) wxEvtHandler_GetPreviousHandler( TSelf(wxEvtHandler) _obj );
TBool      wxEvtHandler_ProcessEvent( TSelf(wxEvtHandler) _obj, TClass(wxEvent) event );
void       wxEvtHandler_ProcessPendingEvents( TSelf(wxEvtHandler) _obj );
void       wxEvtHandler_SetEvtHandlerEnabled( TSelf(wxEvtHandler) _obj, TBool enabled );
void       wxEvtHandler_SetNextHandler( TSelf(wxEvtHandler) _obj, TClass(wxEvtHandler) handler );
void       wxEvtHandler_SetPreviousHandler( TSelf(wxEvtHandler) _obj, TClass(wxEvtHandler) handler );

/* wxExpr */
TClassDef(wxExpr)

/* wxExprDatabase */
TClassDefExtend(wxExprDatabase,wxList)

/* wxFFile */
TClassDef(wxFFile)

/* wxFFileInputStream */
TClassDefExtend(wxFFileInputStream,wxInputStream)

/* wxFFileOutputStream */
TClassDefExtend(wxFFileOutputStream,wxOutputStream)

/* wxFSFile */
TClassDefExtend(wxFSFile,wxObject)

/* wxFTP */
TClassDefExtend(wxFTP,wxProtocol)

/* wxFileDataObject */
TClassDefExtend(wxFileDataObject,wxDataObjectSimple)
void       FileDataObject_AddFile( TSelf(wxFileDataObject) _obj, TClass(wxString) _fle );
TClass(wxFileDataObject) FileDataObject_Create( TArrayString(_cnt, _lst) );
void       FileDataObject_Delete( TSelf(wxFileDataObject) _obj );
TArrayLen        FileDataObject_GetFilenames( TSelf(wxFileDataObject) _obj, TArrayStringOutVoid _lst );

/* wxFileDialog */
TClassDefExtend(wxFileDialog,wxDialog)
TClass(wxFileDialog) wxFileDialog_Create( TClass(wxWindow) _prt, TClass(wxString) _msg, TClass(wxString) _dir, TClass(wxString) _fle, TClass(wxString) _wcd, TPoint(_lft,_top), int _stl );
TClass(wxString) wxFileDialog_GetDirectory( TSelf(wxFileDialog) _obj );
TClass(wxString) wxFileDialog_GetFilename( TSelf(wxFileDialog) _obj );
TArrayLen  wxFileDialog_GetFilenames( TSelf(wxFileDialog) _obj, TArrayStringOutVoid paths );
int        wxFileDialog_GetFilterIndex( TSelf(wxFileDialog) _obj );
TClass(wxString) wxFileDialog_GetMessage( TSelf(wxFileDialog) _obj );
TClass(wxString) wxFileDialog_GetPath( TSelf(wxFileDialog) _obj );
TArrayLen  wxFileDialog_GetPaths( TSelf(wxFileDialog) _obj, TArrayStringOutVoid paths );
int        wxFileDialog_GetStyle( TSelf(wxFileDialog) _obj );
TClass(wxString) wxFileDialog_GetWildcard( TSelf(wxFileDialog) _obj );
void       wxFileDialog_SetDirectory( TSelf(wxFileDialog) _obj, TClass(wxString) dir );
void       wxFileDialog_SetFilename( TSelf(wxFileDialog) _obj, TClass(wxString) name );
void       wxFileDialog_SetFilterIndex( TSelf(wxFileDialog) _obj, int filterIndex );
void       wxFileDialog_SetMessage( TSelf(wxFileDialog) _obj, TClass(wxString) message );
void       wxFileDialog_SetPath( TSelf(wxFileDialog) _obj, TClass(wxString) path );
void       wxFileDialog_SetStyle( TSelf(wxFileDialog) _obj, int style );
void       wxFileDialog_SetWildcard( TSelf(wxFileDialog) _obj, TClass(wxString) wildCard );

/* wxFileDropTarget */
TClassDefExtend(wxFileDropTarget,wxDropTarget)

/* wxFileHistory */
TClassDefExtend(wxFileHistory,wxObject)
void       wxFileHistory_AddFileToHistory( TSelf(wxFileHistory) _obj, TClass(wxString) file );
void       wxFileHistory_AddFilesToMenu( TSelf(wxFileHistory) _obj, TClass(wxMenu) menu );
TClass(wxFileHistory) wxFileHistory_Create( int maxFiles );
void       wxFileHistory_Delete( TSelf(wxFileHistory) _obj );
int        wxFileHistory_GetCount( TSelf(wxFileHistory) _obj );
TClass(wxString) wxFileHistory_GetHistoryFile( TSelf(wxFileHistory) _obj, int i );
int        wxFileHistory_GetMaxFiles( TSelf(wxFileHistory) _obj );
TArrayLen  wxFileHistory_GetMenus( TSelf(wxFileHistory) _obj, TArrayObjectOutVoid(wxMenu) _ref );
void       wxFileHistory_Load( TSelf(wxFileHistory) _obj, TClass(wxConfigBase) config );
void       wxFileHistory_RemoveFileFromHistory( TSelf(wxFileHistory) _obj, int i );
void       wxFileHistory_RemoveMenu( TSelf(wxFileHistory) _obj, TClass(wxMenu) menu );
void       wxFileHistory_Save( TSelf(wxFileHistory) _obj, TClass(wxConfigBase) config );
void       wxFileHistory_UseMenu( TSelf(wxFileHistory) _obj, TClass(wxMenu) menu );

/* wxFileInputStream */
TClassDefExtend(wxFileInputStream,wxInputStream)

/* wxFileName */
TClassDef(wxFileName)

/* wxFileOutputStream */
TClassDefExtend(wxFileOutputStream,wxOutputStream)

/* wxFileSystem */
TClassDefExtend(wxFileSystem,wxObject)

/* wxFileSystemHandler */
TClassDefExtend(wxFileSystemHandler,wxObject)

/* wxFileType */
TClassDef(wxFileType)
void       wxFileType_Delete( TSelf(wxFileType) _obj );
TClass(wxString) wxFileType_ExpandCommand( TSelf(wxFileType) _obj, void* _cmd, void* _params );
TClass(wxString) wxFileType_GetDescription( TSelf(wxFileType) _obj );
int        wxFileType_GetExtensions( TSelf(wxFileType) _obj, TClass(wxList) _lst );
int        wxFileType_GetIcon( TSelf(wxFileType) _obj, TClass(wxIcon) icon );
TClass(wxString) wxFileType_GetMimeType( TSelf(wxFileType) _obj );
int        wxFileType_GetMimeTypes( TSelf(wxFileType) _obj, TClass(wxList) _lst );
int        wxFileType_GetOpenCommand( TSelf(wxFileType) _obj, void* _buf, void* _params );
int        wxFileType_GetPrintCommand( TSelf(wxFileType) _obj, void* _buf, void* _params );

/* wxFilterInputStream */
TClassDefExtend(wxFilterInputStream,wxInputStream)

/* wxFilterOutputStream */
TClassDefExtend(wxFilterOutputStream,wxOutputStream)

/* wxFindDialogEvent */
TClassDefExtend(wxFindDialogEvent,wxCommandEvent)
int        wxFindDialogEvent_GetFindString( TSelf(wxFindDialogEvent) _obj, void* _ref );
int        wxFindDialogEvent_GetFlags( TSelf(wxFindDialogEvent) _obj );
int        wxFindDialogEvent_GetReplaceString( TSelf(wxFindDialogEvent) _obj, void* _ref );

/* wxFindReplaceData */
TClassDefExtend(wxFindReplaceData,wxObject)
TClass(wxFindReplaceData) wxFindReplaceData_Create( int flags );
TClass(wxFindReplaceData) wxFindReplaceData_CreateDefault(  );
void       wxFindReplaceData_Delete( TSelf(wxFindReplaceData) _obj );
TClass(wxString) wxFindReplaceData_GetFindString( TSelf(wxFindReplaceData) _obj );
int        wxFindReplaceData_GetFlags( TSelf(wxFindReplaceData) _obj );
TClass(wxString) wxFindReplaceData_GetReplaceString( TSelf(wxFindReplaceData) _obj );
void       wxFindReplaceData_SetFindString( TSelf(wxFindReplaceData) _obj, TClass(wxString) str );
void       wxFindReplaceData_SetFlags( TSelf(wxFindReplaceData) _obj, int flags );
void       wxFindReplaceData_SetReplaceString( TSelf(wxFindReplaceData) _obj, TClass(wxString) str );

/* wxFindReplaceDialog */
TClassDefExtend(wxFindReplaceDialog,wxDialog)
TClass(wxFindReplaceDialog) wxFindReplaceDialog_Create( TClass(wxWindow) parent, TClass(wxFindReplaceData) data, TClass(wxString) title, int style );
TClass(wxFindReplaceData)   wxFindReplaceDialog_GetData( TSelf(wxFindReplaceDialog) _obj );
void       wxFindReplaceDialog_SetData( TSelf(wxFindReplaceDialog) _obj, TClass(wxFindReplaceData) data );

/* wxFlexGridSizer */
TClassDefExtend(wxFlexGridSizer,wxGridSizer)
void       wxFlexGridSizer_AddGrowableCol( TSelf(wxFlexGridSizer) _obj, size_t idx );
void       wxFlexGridSizer_AddGrowableRow( TSelf(wxFlexGridSizer) _obj, size_t idx );
TClass(wxSize) wxFlexGridSizer_CalcMin( TSelf(wxFlexGridSizer) _obj );
TClass(wxFlexGridSizer) wxFlexGridSizer_Create( int rows, int cols, int vgap, int hgap );
void       wxFlexGridSizer_RecalcSizes( TSelf(wxFlexGridSizer) _obj );
void       wxFlexGridSizer_RemoveGrowableCol( TSelf(wxFlexGridSizer) _obj, size_t idx );
void       wxFlexGridSizer_RemoveGrowableRow( TSelf(wxFlexGridSizer) _obj, size_t idx );

/* wxFocusEvent */
TClassDefExtend(wxFocusEvent,wxEvent)

/* wxFont */
TClassDefExtend(wxFont,wxGDIObject)
TClass(wxFont) wxFont_Create( int pointSize, int family, int style, int weight, TBool underlined, TClass(wxString) face, int enc );
TClass(wxFont) wxFont_CreateFromStock( int id );
TClass(wxFont) wxFont_CreateDefault(  );
void       wxFont_Delete( TSelf(wxFont) _obj );
int        wxFont_GetDefaultEncoding( TSelf(wxFont) _obj );
int        wxFont_GetEncoding( TSelf(wxFont) _obj );
TClass(wxString) wxFont_GetFaceName( TSelf(wxFont) _obj );
int        wxFont_GetFamily( TSelf(wxFont) _obj );
TClass(wxString) wxFont_GetFamilyString( TSelf(wxFont) _obj );
int        wxFont_GetPointSize( TSelf(wxFont) _obj );
int        wxFont_GetStyle( TSelf(wxFont) _obj );
TClass(wxString) wxFont_GetStyleString( TSelf(wxFont) _obj );
int        wxFont_GetUnderlined( TSelf(wxFont) _obj );
int        wxFont_GetWeight( TSelf(wxFont) _obj );
TClass(wxString) wxFont_GetWeightString( TSelf(wxFont) _obj );
TBool      wxFont_IsOk( TSelf(wxFont) _obj );
void       wxFont_SetDefaultEncoding( TSelf(wxFont) _obj, int encoding );
void       wxFont_SetEncoding( TSelf(wxFont) _obj, int encoding );
void       wxFont_SetFaceName( TSelf(wxFont) _obj, TClass(wxString) faceName );
void       wxFont_SetFamily( TSelf(wxFont) _obj, int family );
void       wxFont_SetPointSize( TSelf(wxFont) _obj, int pointSize );
void       wxFont_SetStyle( TSelf(wxFont) _obj, int style );
void       wxFont_SetUnderlined( TSelf(wxFont) _obj, int underlined );
void       wxFont_SetWeight( TSelf(wxFont) _obj, int weight );

/* wxFontData */
TClassDefExtend(wxFontData,wxObject)
TClass(wxFontData) wxFontData_Create(  );
void       wxFontData_Delete( TSelf(wxFontData) _obj );
void       wxFontData_EnableEffects( TSelf(wxFontData) _obj, TBool flag );
TBool      wxFontData_GetAllowSymbols( TSelf(wxFontData) _obj );
void       wxFontData_GetChosenFont( TSelf(wxFontData) _obj, TClassRef(wxFont) ref );
void       wxFontData_GetColour( TSelf(wxFontData) _obj, TClassRef(wxColour) _ref );
TBool      wxFontData_GetEnableEffects( TSelf(wxFontData) _obj );
int        wxFontData_GetEncoding( TSelf(wxFontData) _obj );
void       wxFontData_GetInitialFont( TSelf(wxFontData) _obj, TClassRef(wxFont) ref );
int        wxFontData_GetShowHelp( TSelf(wxFontData) _obj );
void       wxFontData_SetAllowSymbols( TSelf(wxFontData) _obj, TBool flag );
void       wxFontData_SetChosenFont( TSelf(wxFontData) _obj, TClass(wxFont) font );
void       wxFontData_SetColour( TSelf(wxFontData) _obj, TClass(wxColour) colour );
void       wxFontData_SetEncoding( TSelf(wxFontData) _obj, int encoding );
void       wxFontData_SetInitialFont( TSelf(wxFontData) _obj, TClass(wxFont) font );
void       wxFontData_SetRange( TSelf(wxFontData) _obj, int minRange, int maxRange );
void       wxFontData_SetShowHelp( TSelf(wxFontData) _obj, TBool flag );

/* wxFontDialog */
TClassDefExtend(wxFontDialog,wxDialog)
TClass(wxFontDialog) wxFontDialog_Create( TClass(wxWindow) _prt, TClass(wxFontData) fnt );
void       wxFontDialog_GetFontData( TSelf(wxFontDialog) _obj, TClassRef(wxFontData) _ref );

/* wxFontEnumerator */
TClassDef(wxFontEnumerator)
TClass(wxFontEnumerator) wxFontEnumerator_Create( void* _obj, void* _fnc );
void       wxFontEnumerator_Delete( TSelf(wxFontEnumerator) _obj );
TBool      wxFontEnumerator_EnumerateEncodings( TSelf(wxFontEnumerator) _obj, TClass(wxString) facename );
TBool      wxFontEnumerator_EnumerateFacenames( TSelf(wxFontEnumerator) _obj, int encoding, int fixedWidthOnly );

/* wxFontList */
TClassDefExtend(wxFontList,wxList)

/* wxFontMapper */
TClassDef(wxFontMapper)
TClass(wxFontMapper) wxFontMapper_Create(  );
TBool wxFontMapper_GetAltForEncoding( TSelf(wxFontMapper) _obj, int encoding, void* alt_encoding, TClass(wxString) _buf );
TBool wxFontMapper_IsEncodingAvailable( TSelf(wxFontMapper) _obj, int encoding, TClass(wxString) _buf );

/* wxFrame */
TClassDefExtend(wxFrame,wxTopLevelWindow)
TClass(wxFrame) wxFrame_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TRect(_lft,_top,_wdt,_hgt), int _stl );
TClass(wxStatusBar) wxFrame_CreateStatusBar( TSelf(wxFrame) _obj, int number, int style );
TClass(wxToolBar)   wxFrame_CreateToolBar( TSelf(wxFrame) _obj, long style );
int        wxFrame_GetClientAreaOrigin_left( TSelf(wxFrame) _obj );
int        wxFrame_GetClientAreaOrigin_top( TSelf(wxFrame) _obj );
TClass(wxMenuBar) wxFrame_GetMenuBar( TSelf(wxFrame) _obj );
TClass(wxStatusBar) wxFrame_GetStatusBar( TSelf(wxFrame) _obj );
TClass(wxToolBar) wxFrame_GetToolBar( TSelf(wxFrame) _obj );
void       wxFrame_Restore( TSelf(wxFrame) _obj );
void       wxFrame_SetMenuBar( TSelf(wxFrame) _obj, TClass(wxMenuBar) menubar );
void       wxFrame_SetStatusBar( TSelf(wxFrame) _obj, TClass(wxStatusBar) statBar );
void       wxFrame_SetStatusText( TSelf(wxFrame) _obj, TClass(wxString) _txt, int _number );
void       wxFrame_SetStatusWidths( TSelf(wxFrame) _obj, int _n, void* _widths_field );
void       wxFrame_SetToolBar( TSelf(wxFrame) _obj, TClass(wxToolBar) _toolbar );

/* wxFrameLayout */
TClassDefExtend(wxFrameLayout,wxEvtHandler)
void       wxFrameLayout_Activate( TSelf(wxFrameLayout) _obj );
void       wxFrameLayout_AddBar( TSelf(wxFrameLayout) _obj, void* pBarWnd, void* dimInfo, int alignment, int rowNo, int columnPos, TStringVoid name, int spyEvents, int state );
void       wxFrameLayout_AddPlugin( TSelf(wxFrameLayout) _obj, void* pPlInfo, int paneMask );
void       wxFrameLayout_AddPluginBefore( TSelf(wxFrameLayout) _obj, void* pNextPlInfo, void* pPlInfo, int paneMask );
void       wxFrameLayout_ApplyBarProperties( TSelf(wxFrameLayout) _obj, void* pBar );
void       wxFrameLayout_CaptureEventsForPane( TSelf(wxFrameLayout) _obj, void* toPane );
void       wxFrameLayout_CaptureEventsForPlugin( TSelf(wxFrameLayout) _obj, void* pPlugin );
TClass(wxFrameLayout) wxFrameLayout_Create( void* pParentFrame, void* pFrameClient, int activateNow );
void       wxFrameLayout_Deactivate( TSelf(wxFrameLayout) _obj );
void       wxFrameLayout_Delete( TSelf(wxFrameLayout) _obj );
void       wxFrameLayout_DestroyBarWindows( TSelf(wxFrameLayout) _obj );
void       wxFrameLayout_EnableFloating( TSelf(wxFrameLayout) _obj, TBool enable );
void*      wxFrameLayout_FindBarByName( TSelf(wxFrameLayout) _obj, TStringVoid name );
void*      wxFrameLayout_FindBarByWindow( TSelf(wxFrameLayout) _obj, void* pWnd );
void*      wxFrameLayout_FindPlugin( TSelf(wxFrameLayout) _obj, void* pPlInfo );
void       wxFrameLayout_FirePluginEvent( TSelf(wxFrameLayout) _obj, TClass(wxEvent) event );
int        wxFrameLayout_GetBars( TSelf(wxFrameLayout) _obj, void* _ref );
int        wxFrameLayout_GetClientHeight( TSelf(wxFrameLayout) _obj );
void       wxFrameLayout_GetClientRect( TSelf(wxFrameLayout) _obj, TRectOutVoid(_x,_y,_w,_h) );
int        wxFrameLayout_GetClientWidth( TSelf(wxFrameLayout) _obj );
void*      wxFrameLayout_GetFrameClient( TSelf(wxFrameLayout) _obj );
void*      wxFrameLayout_GetPane( TSelf(wxFrameLayout) _obj, int alignment );
void       wxFrameLayout_GetPaneProperties( TSelf(wxFrameLayout) _obj, void* props, int alignment );
void*      wxFrameLayout_GetParentFrame( TSelf(wxFrameLayout) _obj );
void*      wxFrameLayout_GetTopPlugin( TSelf(wxFrameLayout) _obj );
void*      wxFrameLayout_GetUpdatesManager( TSelf(wxFrameLayout) _obj );
TBool      wxFrameLayout_HasTopPlugin( TSelf(wxFrameLayout) _obj );
void       wxFrameLayout_HideBarWindows( TSelf(wxFrameLayout) _obj );
void       wxFrameLayout_InverseVisibility( TSelf(wxFrameLayout) _obj, void* pBar );
void       wxFrameLayout_OnLButtonDown( TSelf(wxFrameLayout) _obj, TClass(wxEvent) event );
void       wxFrameLayout_OnLButtonUp( TSelf(wxFrameLayout) _obj, TClass(wxEvent) event );
void       wxFrameLayout_OnLDblClick( TSelf(wxFrameLayout) _obj, TClass(wxEvent) event );
void       wxFrameLayout_OnMouseMove( TSelf(wxFrameLayout) _obj, TClass(wxEvent) event );
void       wxFrameLayout_OnRButtonDown( TSelf(wxFrameLayout) _obj, TClass(wxEvent) event );
void       wxFrameLayout_OnRButtonUp( TSelf(wxFrameLayout) _obj, TClass(wxEvent) event );
void       wxFrameLayout_OnSize( TSelf(wxFrameLayout) _obj, TClass(wxEvent) event );
void       wxFrameLayout_PopAllPlugins( TSelf(wxFrameLayout) _obj );
void       wxFrameLayout_PopPlugin( TSelf(wxFrameLayout) _obj );
void       wxFrameLayout_PushDefaultPlugins( TSelf(wxFrameLayout) _obj );
void       wxFrameLayout_PushPlugin( TSelf(wxFrameLayout) _obj, void* pPugin );
void       wxFrameLayout_RecalcLayout( TSelf(wxFrameLayout) _obj, int repositionBarsNow );
int        wxFrameLayout_RedockBar( TSelf(wxFrameLayout) _obj, void* pBar, TRect(x,y,w,h), void* pToPane, int updateNow );
void       wxFrameLayout_RefreshNow( TSelf(wxFrameLayout) _obj, int recalcLayout );
void       wxFrameLayout_ReleaseEventsFromPane( TSelf(wxFrameLayout) _obj, void* fromPane );
void       wxFrameLayout_ReleaseEventsFromPlugin( TSelf(wxFrameLayout) _obj, void* pPlugin );
void       wxFrameLayout_RemoveBar( TSelf(wxFrameLayout) _obj, void* pBar );
void       wxFrameLayout_RemovePlugin( TSelf(wxFrameLayout) _obj, void* pPlInfo );
void       wxFrameLayout_SetBarState( TSelf(wxFrameLayout) _obj, void* pBar, int newStatem, int updateNow );
void       wxFrameLayout_SetFrameClient( TSelf(wxFrameLayout) _obj, void* pFrameClient );
void       wxFrameLayout_SetMargins( TSelf(wxFrameLayout) _obj, int top, int bottom, int left, int right, int paneMask );
void       wxFrameLayout_SetPaneBackground( TSelf(wxFrameLayout) _obj, TClass(wxColour) colour );
void       wxFrameLayout_SetPaneProperties( TSelf(wxFrameLayout) _obj, void* props, int paneMask );
void       wxFrameLayout_SetTopPlugin( TSelf(wxFrameLayout) _obj, void* pPlugin );
void       wxFrameLayout_SetUpdatesManager( TSelf(wxFrameLayout) _obj, void* pUMgr );

/* wxGDIObject */
TClassDefExtend(wxGDIObject,wxObject)

/* wxGLCanvas */
TClassDefExtend(wxGLCanvas,wxScrolledWindow)

/* wxGauge */
TClassDefExtend(wxGauge,wxControl)
TClass(wxGauge) wxGauge_Create( TClass(wxWindow) _prt, int _id, int _rng, TRect(_lft,_top,_wdt,_hgt), int _stl );
int        wxGauge_GetBezelFace( TSelf(wxGauge) _obj );
int        wxGauge_GetRange( TSelf(wxGauge) _obj );
int        wxGauge_GetShadowWidth( TSelf(wxGauge) _obj );
int        wxGauge_GetValue( TSelf(wxGauge) _obj );
void       wxGauge_SetBezelFace( TSelf(wxGauge) _obj, int w );
void       wxGauge_SetRange( TSelf(wxGauge) _obj, int r );
void       wxGauge_SetShadowWidth( TSelf(wxGauge) _obj, int w );
void       wxGauge_SetValue( TSelf(wxGauge) _obj, int pos );

/* wxGenericDirCtrl */
TClassDefExtend(wxGenericDirCtrl,wxControl)

/* wxGenericValidator */
TClassDefExtend(wxGenericValidator,wxValidator)

/* wxGrid */
TClassDefExtend(wxGrid,wxScrolledWindow)
TBool      wxGrid_AppendCols( TSelf(wxGrid) _obj, int numCols, TBool updateLabels );
TBool      wxGrid_AppendRows( TSelf(wxGrid) _obj, int numRows, TBool updateLabels );
void       wxGrid_AutoSize( TSelf(wxGrid) _obj );
void       wxGrid_AutoSizeColumn( TSelf(wxGrid) _obj, int col, TBoolInt setAsMin );
void       wxGrid_AutoSizeColumns( TSelf(wxGrid) _obj, TBoolInt setAsMin );
void       wxGrid_AutoSizeRow( TSelf(wxGrid) _obj, int row, TBoolInt setAsMin );
void       wxGrid_AutoSizeRows( TSelf(wxGrid) _obj, TBoolInt setAsMin );
void       wxGrid_BeginBatch( TSelf(wxGrid) _obj );
TClass(wxRect) wxGrid_BlockToDeviceRect( TSelf(wxGrid) _obj, int top, int left, int bottom, int right );
TBool      wxGrid_CanDragColSize( TSelf(wxGrid) _obj, int col );
TBool      wxGrid_CanDragGridSize( TSelf(wxGrid) _obj );
TBool      wxGrid_CanDragRowSize( TSelf(wxGrid) _obj, int row );
TBool      wxGrid_CanEnableCellControl( TSelf(wxGrid) _obj );
TClass(wxRect) wxGrid_CellToRect( TSelf(wxGrid) _obj, int row, int col );
void       wxGrid_ClearGrid( TSelf(wxGrid) _obj );
void       wxGrid_ClearSelection( TSelf(wxGrid) _obj );
TClass(wxGrid) wxGrid_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );
void       wxGrid_CreateGrid( TSelf(wxGrid) _obj, int rows, int cols, int selmode );
TBool      wxGrid_DeleteCols( TSelf(wxGrid) _obj, int pos, int numCols, TBool updateLabels );
TBool      wxGrid_DeleteRows( TSelf(wxGrid) _obj, int pos, int numRows, TBool updateLabels );
void       wxGrid_DisableCellEditControl( TSelf(wxGrid) _obj );
void       wxGrid_DisableDragColSize( TSelf(wxGrid) _obj );
void       wxGrid_DisableDragGridSize( TSelf(wxGrid) _obj );
void       wxGrid_DisableDragRowSize( TSelf(wxGrid) _obj );
void       wxGrid_DrawAllGridLines( TSelf(wxGrid) _obj, TClass(wxDC) dc, TClass(wxRegion) reg );
void       wxGrid_DrawCell( TSelf(wxGrid) _obj, TClass(wxDC) dc, int _row, int _col );
void       wxGrid_DrawCellBorder( TSelf(wxGrid) _obj, TClass(wxDC) dc, int _row, int _col );
void       wxGrid_DrawCellHighlight( TSelf(wxGrid) _obj, TClass(wxDC) dc, TClass(wxGridCellAttr) attr );
void       wxGrid_DrawColLabel( TSelf(wxGrid) _obj, TClass(wxDC) dc, int col );
void       wxGrid_DrawColLabels( TSelf(wxGrid) _obj, TClass(wxDC) dc );
void       wxGrid_DrawGridSpace( TSelf(wxGrid) _obj, TClass(wxDC) dc );
void       wxGrid_DrawRowLabel( TSelf(wxGrid) _obj, TClass(wxDC) dc, int row );
void       wxGrid_DrawRowLabels( TSelf(wxGrid) _obj, TClass(wxDC) dc );
void       wxGrid_DrawTextRectangle( TSelf(wxGrid) _obj, TClass(wxDC) dc, TClass(wxString) txt, TRect(x,y,w,h), int horizontalAlignment, int verticalAlignment );
void       wxGrid_EnableCellEditControl( TSelf(wxGrid) _obj, TBool enable );
void       wxGrid_EnableDragColSize( TSelf(wxGrid) _obj, TBool enable );
void       wxGrid_EnableDragGridSize( TSelf(wxGrid) _obj, TBool enable );
void       wxGrid_EnableDragRowSize( TSelf(wxGrid) _obj, TBool enable );
void       wxGrid_EnableEditing( TSelf(wxGrid) _obj, TBoolInt edit );
void       wxGrid_EnableGridLines( TSelf(wxGrid) _obj, TBool enable );
void       wxGrid_EndBatch( TSelf(wxGrid) _obj );
int        wxGrid_GetBatchCount( TSelf(wxGrid) _obj );
void       wxGrid_GetCellAlignment( TSelf(wxGrid) _obj, int row, int col, TSizeOut(horiz, vert) );
void       wxGrid_GetCellBackgroundColour( TSelf(wxGrid) _obj, int row, int col, TClass(wxColour) colour );
TClass(wxGridCellEditor) wxGrid_GetCellEditor( TSelf(wxGrid) _obj, int row, int col );
void       wxGrid_GetCellFont( TSelf(wxGrid) _obj, int row, int col, TClass(wxFont) font );
void       wxGrid_GetCellHighlightColour( TSelf(wxGrid) _obj, TClassRef(wxColour) _ref );
TClass(wxGridCellRenderer) wxGrid_GetCellRenderer( TSelf(wxGrid) _obj, int row, int col );
void       wxGrid_GetCellTextColour( TSelf(wxGrid) _obj, int row, int col, TClass(wxColour) colour );
TClass(wxString) wxGrid_GetCellValue( TSelf(wxGrid) _obj, int row, int col );
void       wxGrid_GetColLabelAlignment( TSelf(wxGrid) _obj, TSizeOut(horiz, vert)  );
int        wxGrid_GetColLabelSize( TSelf(wxGrid) _obj );
TClass(wxString) wxGrid_GetColLabelValue( TSelf(wxGrid) _obj, int col );
int        wxGrid_GetColSize( TSelf(wxGrid) _obj, int col );
void       wxGrid_GetDefaultCellAlignment( TSelf(wxGrid) _obj, TSizeOut(horiz, vert)  );
void       wxGrid_GetDefaultCellBackgroundColour( TSelf(wxGrid) _obj, TClassRef(wxColour) _ref );
void       wxGrid_GetDefaultCellFont( TSelf(wxGrid) _obj, TClassRef(wxFont) _ref );
void       wxGrid_GetDefaultCellTextColour( TSelf(wxGrid) _obj, TClassRef(wxColour) _ref );
int        wxGrid_GetDefaultColLabelSize( TSelf(wxGrid) _obj );
int        wxGrid_GetDefaultColSize( TSelf(wxGrid) _obj );
TClass(wxGridCellEditor) wxGrid_GetDefaultEditor( TSelf(wxGrid) _obj );
TClass(wxGridCellEditor) wxGrid_GetDefaultEditorForCell( TSelf(wxGrid) _obj, int row, int col );
TClass(wxGridCellEditor) wxGrid_GetDefaultEditorForType( TSelf(wxGrid) _obj, TClass(wxString) typeName );
TClass(wxGridCellRenderer) wxGrid_GetDefaultRenderer( TSelf(wxGrid) _obj );
TClass(wxGridCellRenderer) wxGrid_GetDefaultRendererForCell( TSelf(wxGrid) _obj, int row, int col );
TClass(wxGridCellRenderer) wxGrid_GetDefaultRendererForType( TSelf(wxGrid) _obj, TClass(wxString) typeName );
int        wxGrid_GetDefaultRowLabelSize( TSelf(wxGrid) _obj );
int        wxGrid_GetDefaultRowSize( TSelf(wxGrid) _obj );
int        wxGrid_GetGridCursorCol( TSelf(wxGrid) _obj );
int        wxGrid_GetGridCursorRow( TSelf(wxGrid) _obj );
void       wxGrid_GetGridLineColour( TSelf(wxGrid) _obj, TClassRef(wxColour) _ref );
void       wxGrid_GetLabelBackgroundColour( TSelf(wxGrid) _obj, TClassRef(wxColour) _ref );
void       wxGrid_GetLabelFont( TSelf(wxGrid) _obj, TClassRef(wxFont) _ref );
void       wxGrid_GetLabelTextColour( TSelf(wxGrid) _obj, TClassRef(wxColour) _ref );
int        wxGrid_GetNumberCols( TSelf(wxGrid) _obj );
int        wxGrid_GetNumberRows( TSelf(wxGrid) _obj );
void       wxGrid_GetRowLabelAlignment( TSelf(wxGrid) _obj, TSizeOut(horiz,vert) );
int        wxGrid_GetRowLabelSize( TSelf(wxGrid) _obj );
TClass(wxString) wxGrid_GetRowLabelValue( TSelf(wxGrid) _obj, int row );
int        wxGrid_GetRowSize( TSelf(wxGrid) _obj, int row );
void       wxGrid_GetSelectionBackground( TSelf(wxGrid) _obj, TClassRef(wxColour) _ref );
void       wxGrid_GetSelectionForeground( TSelf(wxGrid) _obj, TClassRef(wxColour) _ref );
TClass(wxGridTableBase) wxGrid_GetTable( TSelf(wxGrid) _obj );
void       wxGrid_GetTextBoxSize( TSelf(wxGrid) _obj, TClass(wxDC) dc, TArrayString(count,lines), TSizeOutVoid(_w,_h) );
int        wxGrid_GridLinesEnabled( TSelf(wxGrid) _obj );
void       wxGrid_HideCellEditControl( TSelf(wxGrid) _obj );
TBool      wxGrid_InsertCols( TSelf(wxGrid) _obj, int pos, int numCols, TBool updateLabels );
TBool      wxGrid_InsertRows( TSelf(wxGrid) _obj, int pos, int numRows, TBool updateLabels );
TBool      wxGrid_IsCellEditControlEnabled( TSelf(wxGrid) _obj );
TBool      wxGrid_IsCellEditControlShown( TSelf(wxGrid) _obj );
TBool      wxGrid_IsCurrentCellReadOnly( TSelf(wxGrid) _obj );
TBool      wxGrid_IsEditable( TSelf(wxGrid) _obj );
TBool      wxGrid_IsInSelection( TSelf(wxGrid) _obj, int row, int col );
TBool      wxGrid_IsReadOnly( TSelf(wxGrid) _obj, int row, int col );
TBool      wxGrid_IsSelection( TSelf(wxGrid) _obj );
TBool      wxGrid_IsVisible( TSelf(wxGrid) _obj, int row, int col, TBool wholeCellVisible );
void       wxGrid_MakeCellVisible( TSelf(wxGrid) _obj, int row, int col );
TBool      wxGrid_MoveCursorDown( TSelf(wxGrid) _obj, TBool expandSelection );
TBool      wxGrid_MoveCursorDownBlock( TSelf(wxGrid) _obj, TBool expandSelection );
TBool      wxGrid_MoveCursorLeft( TSelf(wxGrid) _obj, TBool expandSelection );
TBool      wxGrid_MoveCursorLeftBlock( TSelf(wxGrid) _obj, TBool expandSelection );
TBool      wxGrid_MoveCursorRight( TSelf(wxGrid) _obj, TBool expandSelection );
TBool      wxGrid_MoveCursorRightBlock( TSelf(wxGrid) _obj, TBool expandSelection );
TBool      wxGrid_MoveCursorUp( TSelf(wxGrid) _obj, TBool expandSelection );
TBool      wxGrid_MoveCursorUpBlock( TSelf(wxGrid) _obj, TBool expandSelection );
TBool      wxGrid_MovePageDown( TSelf(wxGrid) _obj );
TBool      wxGrid_MovePageUp( TSelf(wxGrid) _obj );
TBool      wxGrid_ProcessTableMessage( TSelf(wxGrid) _obj, TClass(wxEvent) evt );
void       wxGrid_RegisterDataType( TSelf(wxGrid) _obj, TClass(wxString) typeName, TClass(wxGridCellRenderer) renderer, TClass(wxGridCellEditor) editor );
void       wxGrid_SaveEditControlValue( TSelf(wxGrid) _obj );
void       wxGrid_SelectAll( TSelf(wxGrid) _obj );
void       wxGrid_SelectBlock( TSelf(wxGrid) _obj, int topRow, int leftCol, int bottomRow, int rightCol, TBoolInt addToSelected );
void       wxGrid_SelectCol( TSelf(wxGrid) _obj, int col, TBoolInt addToSelected );
void       wxGrid_SelectRow( TSelf(wxGrid) _obj, int row, TBoolInt addToSelected );
void       wxGrid_SetCellAlignment( TSelf(wxGrid) _obj, int row, int col, int horiz, int vert );
void       wxGrid_SetCellBackgroundColour( TSelf(wxGrid) _obj, int row, int col, TClass(wxColour) colour );
void       wxGrid_SetCellEditor( TSelf(wxGrid) _obj, int row, int col, TClass(wxGridCellEditor) editor );
void       wxGrid_SetCellFont( TSelf(wxGrid) _obj, int row, int col, TClass(wxFont) font );
void       wxGrid_SetCellHighlightColour( TSelf(wxGrid) _obj, TClass(wxColour) col );
void       wxGrid_SetCellRenderer( TSelf(wxGrid) _obj, int row, int col, TClass(wxGridCellRenderer) renderer );
void       wxGrid_SetCellTextColour( TSelf(wxGrid) _obj, int row, int col, TClass(wxColour) colour );
void       wxGrid_SetCellValue( TSelf(wxGrid) _obj, int row, int col, TClass(wxString) s );
void       wxGrid_SetColAttr( TSelf(wxGrid) _obj, int col, TClass(wxGridCellAttr) attr );
void       wxGrid_SetColFormatBool( TSelf(wxGrid) _obj, int col );
void       wxGrid_SetColFormatCustom( TSelf(wxGrid) _obj, int col, TClass(wxString) typeName );
void       wxGrid_SetColFormatFloat( TSelf(wxGrid) _obj, int col, int width, int precision );
void       wxGrid_SetColFormatNumber( TSelf(wxGrid) _obj, int col );
void       wxGrid_SetColLabelAlignment( TSelf(wxGrid) _obj, int horiz, int vert );
void       wxGrid_SetColLabelSize( TSelf(wxGrid) _obj, int height );
void       wxGrid_SetColLabelValue( TSelf(wxGrid) _obj, int col, TClass(wxString) label );
void       wxGrid_SetColMinimalWidth( TSelf(wxGrid) _obj, int col, int width );
void       wxGrid_SetColSize( TSelf(wxGrid) _obj, int col, int width );
void       wxGrid_SetDefaultCellAlignment( TSelf(wxGrid) _obj, int horiz, int vert );
void       wxGrid_SetDefaultCellBackgroundColour( TSelf(wxGrid) _obj, TClass(wxColour) colour );
void       wxGrid_SetDefaultCellFont( TSelf(wxGrid) _obj, TClass(wxFont) font );
void       wxGrid_SetDefaultCellTextColour( TSelf(wxGrid) _obj, TClass(wxColour) colour );
void       wxGrid_SetDefaultColSize( TSelf(wxGrid) _obj, int width, TBoolInt resizeExistingCols );
void       wxGrid_SetDefaultEditor( TSelf(wxGrid) _obj, TClass(wxGridCellEditor) editor );
void       wxGrid_SetDefaultRenderer( TSelf(wxGrid) _obj, TClass(wxGridCellRenderer) renderer );
void       wxGrid_SetDefaultRowSize( TSelf(wxGrid) _obj, int height, TBoolInt resizeExistingRows );
void       wxGrid_SetGridCursor( TSelf(wxGrid) _obj, int row, int col );
void       wxGrid_SetGridLineColour( TSelf(wxGrid) _obj, TClass(wxColour) col );
void       wxGrid_SetLabelBackgroundColour( TSelf(wxGrid) _obj, TClass(wxColour) colour );
void       wxGrid_SetLabelFont( TSelf(wxGrid) _obj, TClass(wxFont) font );
void       wxGrid_SetLabelTextColour( TSelf(wxGrid) _obj, TClass(wxColour) colour );
void       wxGrid_SetMargins( TSelf(wxGrid) _obj, int extraWidth, int extraHeight );
void       wxGrid_SetReadOnly( TSelf(wxGrid) _obj, int row, int col, TBool isReadOnly );
void       wxGrid_SetRowAttr( TSelf(wxGrid) _obj, int row, TClass(wxGridCellAttr) attr );
void       wxGrid_SetRowLabelAlignment( TSelf(wxGrid) _obj, int horiz, int vert );
void       wxGrid_SetRowLabelSize( TSelf(wxGrid) _obj, int width );
void       wxGrid_SetRowLabelValue( TSelf(wxGrid) _obj, int row, TClass(wxString) label );
void       wxGrid_SetRowMinimalHeight( TSelf(wxGrid) _obj, int row, int width );
void       wxGrid_SetRowSize( TSelf(wxGrid) _obj, int row, int height );
void       wxGrid_SetSelectionBackground( TSelf(wxGrid) _obj, TClass(wxColour) c );
void       wxGrid_SetSelectionForeground( TSelf(wxGrid) _obj, TClass(wxColour) c );
void       wxGrid_SetSelectionMode( TSelf(wxGrid) _obj, int selmode );
TBool     wxGrid_SetTable( TSelf(wxGrid) _obj, TClass(wxGridTableBase) table, TBool takeOwnership, int selmode );
void       wxGrid_ShowCellEditControl( TSelf(wxGrid) _obj );
int        wxGrid_StringToLines( TSelf(wxGrid) _obj, TClass(wxString) value, void* lines );
int        wxGrid_XToCol( TSelf(wxGrid) _obj, int x );
int        wxGrid_XToEdgeOfCol( TSelf(wxGrid) _obj, int x );
void       wxGrid_XYToCell( TSelf(wxGrid) _obj, TPoint(x,y), TPointOut(row,col) );
int        wxGrid_YToEdgeOfRow( TSelf(wxGrid) _obj, int y );
int        wxGrid_YToRow( TSelf(wxGrid) _obj, int y );
void       wxGrid_GetSelectedCells(TSelf(wxGrid) _obj, TClassRef(wxGridCellCoordsArray) _arr);
void       wxGrid_GetSelectionBlockTopLeft(TSelf(wxGrid) _obj, TClassRef(wxGridCellCoordsArray) _arr);
void       wxGrid_GetSelectionBlockBottomRight(TSelf(wxGrid) _obj, TClassRef(wxGridCellCoordsArray) _arr);
TArrayLen  wxGrid_GetSelectedRows(TSelf(wxGrid) _obj, TArrayIntOutVoid _arr);
TArrayLen  wxGrid_GetSelectedCols(TSelf(wxGrid) _obj, TArrayIntOutVoid _arr);
void       wxGrid_GetCellSize(TSelf(wxGrid) _obj, int row, int col, TSizeOut(srow,scol));
void       wxGrid_SetCellSize(TSelf(wxGrid) _obj, int row, int col, TSize(srow,scol));

/* wxGridCellAttr */
TClassDef(wxGridCellAttr)
TClass(wxGridCellAttr)    wxGridCellAttr_Ctor(  );
void       wxGridCellAttr_DecRef( TSelf(wxGridCellAttr) _obj );
void       wxGridCellAttr_GetAlignment( TSelf(wxGridCellAttr) _obj, TSizeOut(hAlign, vAlign) );
void       wxGridCellAttr_GetBackgroundColour( TSelf(wxGridCellAttr) _obj, TClassRef(wxColour) _ref );
TClass(wxGridCellEditor) wxGridCellAttr_GetEditor( TSelf(wxGridCellAttr) _obj, TClass(wxGrid) grid, int row, int col );
void       wxGridCellAttr_GetFont( TSelf(wxGridCellAttr) _obj, TClassRef(wxFont) _ref );
TClass(wxGridCellRenderer)  wxGridCellAttr_GetRenderer( TSelf(wxGridCellAttr) _obj, TClass(wxGrid) grid, int row, int col );
void       wxGridCellAttr_GetTextColour( TSelf(wxGridCellAttr) _obj, TClassRef(wxColour) _ref );
TBool      wxGridCellAttr_HasAlignment( TSelf(wxGridCellAttr) _obj );
TBool      wxGridCellAttr_HasBackgroundColour( TSelf(wxGridCellAttr) _obj );
TBool      wxGridCellAttr_HasEditor( TSelf(wxGridCellAttr) _obj );
TBool      wxGridCellAttr_HasFont( TSelf(wxGridCellAttr) _obj );
TBool      wxGridCellAttr_HasRenderer( TSelf(wxGridCellAttr) _obj );
TBool      wxGridCellAttr_HasTextColour( TSelf(wxGridCellAttr) _obj );
void       wxGridCellAttr_IncRef( TSelf(wxGridCellAttr) _obj );
TBool      wxGridCellAttr_IsReadOnly( TSelf(wxGridCellAttr) _obj );
void       wxGridCellAttr_SetAlignment( TSelf(wxGridCellAttr) _obj, int hAlign, int vAlign );
void       wxGridCellAttr_SetBackgroundColour( TSelf(wxGridCellAttr) _obj, TClass(wxColour) colBack );
void       wxGridCellAttr_SetDefAttr( TSelf(wxGridCellAttr) _obj, TClass(wxGridCellAttr) defAttr );
void       wxGridCellAttr_SetEditor( TSelf(wxGridCellAttr) _obj, TClass(wxGridCellEditor) editor );
void       wxGridCellAttr_SetFont( TSelf(wxGridCellAttr) _obj, TClass(wxFont) font );
void       wxGridCellAttr_SetReadOnly( TSelf(wxGridCellAttr) _obj, TBool isReadOnly );
void       wxGridCellAttr_SetRenderer( TSelf(wxGridCellAttr) _obj, TClass(wxGridCellRenderer) renderer );
void       wxGridCellAttr_SetTextColour( TSelf(wxGridCellAttr) _obj, TClass(wxColour) colText );

/* wxGridCellBoolEditor */
TClassDefExtend(wxGridCellBoolEditor,wxGridCellEditor)
TClass(wxGridCellBoolEditor)   wxGridCellBoolEditor_Ctor(  );

/* wxGridCellBoolRenderer */
TClassDefExtend(wxGridCellBoolRenderer,wxGridCellRenderer)

/* wxGridCellChoiceEditor */
TClassDefExtend(wxGridCellChoiceEditor,wxGridCellEditor)
TClass(wxGridCellChoiceEditor) wxGridCellChoiceEditor_Ctor( TArrayString(count,choices), TBoolInt allowOthers );

/* wxGridCellCoordsArray */
TClassDef(wxGridCellCoordsArray)
TClass(wxGridCellCoordsArray) wxGridCellCoordsArray_Create();
void       wxGridCellCoordsArray_Delete(TSelf(wxGridCellCoordsArray) _obj);
int        wxGridCellCoordsArray_GetCount(TSelf(wxGridCellCoordsArray) _obj);
void       wxGridCellCoordsArray_Item(TSelf(wxGridCellCoordsArray) _obj, int _idx, TPointOut(_c,_r));

/* wxGridCellEditor */
TClassDefExtend(wxGridCellEditor,wxGridCellWorker)
void       wxGridCellEditor_BeginEdit( TSelf(wxGridCellEditor) _obj, int row, int col, TClass(wxGrid) grid );
void       wxGridCellEditor_Create( TSelf(wxGridCellEditor) _obj, TClass(wxWindow) parent, int id, TClass(wxEvtHandler) evtHandler );
void       wxGridCellEditor_Destroy( TSelf(wxGridCellEditor) _obj );
int        wxGridCellEditor_EndEdit( TSelf(wxGridCellEditor) _obj, int row, int col, TClass(wxGrid) grid, TClass(wxString) oldStr, TClass(wxString) newStr );
TClass(wxControl) wxGridCellEditor_GetControl( TSelf(wxGridCellEditor) _obj );
void       wxGridCellEditor_HandleReturn( TSelf(wxGridCellEditor) _obj, TClass(wxEvent) event );
TBool      wxGridCellEditor_IsAcceptedKey( TSelf(wxGridCellEditor) _obj, TClass(wxEvent) event );
TBool      wxGridCellEditor_IsCreated( TSelf(wxGridCellEditor) _obj );

#if (wxVERSION_NUMBER >= 2905)
void       wxGridCellEditor_PaintBackground( TSelf(wxGridCellEditor) _obj, TClass(wxDC) dc, TRect(x,y,w,h), TClass(wxGridCellAttr) attr );
#else
void       wxGridCellEditor_PaintBackground( TSelf(wxGridCellEditor) _obj, TRect(x,y,w,h), TClass(wxGridCellAttr) attr );
#endif

void       wxGridCellEditor_Reset( TSelf(wxGridCellEditor) _obj );
void       wxGridCellEditor_SetControl( TSelf(wxGridCellEditor) _obj, TClass(wxControl) control );
void       wxGridCellEditor_SetParameters( TSelf(wxGridCellEditor) _obj, TClass(wxString) params );
void       wxGridCellEditor_SetSize( TSelf(wxGridCellEditor) _obj, TRect(x,y,w,h) );
void       wxGridCellEditor_Show( TSelf(wxGridCellEditor) _obj, TBoolInt show, TClass(wxGridCellAttr) attr );
void       wxGridCellEditor_StartingClick( TSelf(wxGridCellEditor) _obj );
void       wxGridCellEditor_StartingKey( TSelf(wxGridCellEditor) _obj, TClass(wxEvent) event );

/* wxGridCellFloatEditor */
TClassDefExtend(wxGridCellFloatEditor,wxGridCellTextEditor)
TClass(wxGridCellFloatEditor) wxGridCellFloatEditor_Ctor( int width, int precision );

/* wxGridCellFloatRenderer */
TClassDefExtend(wxGridCellFloatRenderer,wxGridCellStringRenderer)

/* wxGridCellNumberEditor */
TClassDefExtend(wxGridCellNumberEditor,wxGridCellTextEditor)
TClass(wxGridCellNumberEditor)  wxGridCellNumberEditor_Ctor( int min, int max );

/* wxGridCellNumberRenderer */
TClassDefExtend(wxGridCellNumberRenderer,wxGridCellStringRenderer)
TClass(wxGridCellNumberRenderer)  wxGridCellNumberRenderer_Ctor();

/* wxGridCellAutoWrapStringRenderer */
TClassDefExtend(wxGridCellAutoWrapStringRenderer,wxGridCellStringRenderer)
TClass(wxGridCellAutoWrapStringRenderer)  wxGridCellAutoWrapStringRenderer_Ctor();

/* wxGridCellRenderer */
TClassDefExtend(wxGridCellRenderer,wxGridCellWorker)

/* wxGridCellStringRenderer */
TClassDefExtend(wxGridCellStringRenderer,wxGridCellRenderer)

/* wxGridCellTextEditor */
TClassDefExtend(wxGridCellTextEditor,wxGridCellEditor)
TClass(wxGridCellTextEditor) wxGridCellTextEditor_Ctor(  );

/* wxGridCellWorker */
TClassDef(wxGridCellWorker)

/* wxGridEditorCreatedEvent */
TClassDefExtend(wxGridEditorCreatedEvent,wxCommandEvent)
int        wxGridEditorCreatedEvent_GetCol (TSelf(wxGridEditorCreatedEvent) _obj);
TClass(wxControl) wxGridEditorCreatedEvent_GetControl (TSelf(wxGridEditorCreatedEvent) _obj);
int        wxGridEditorCreatedEvent_GetRow (TSelf(wxGridEditorCreatedEvent) _obj);
void       wxGridEditorCreatedEvent_SetCol (TSelf(wxGridEditorCreatedEvent) _obj, int col);
void       wxGridEditorCreatedEvent_SetControl (TSelf(wxGridEditorCreatedEvent) _obj, TClass(wxControl) ctrl);
void       wxGridEditorCreatedEvent_SetRow (TSelf(wxGridEditorCreatedEvent) _obj, int row);

/* wxGridEvent */
TClassDefExtend(wxGridEvent,wxNotifyEvent)
TBool      wxGridEvent_AltDown (TSelf(wxGridEvent) _obj);
TBool      wxGridEvent_ControlDown (TSelf(wxGridEvent) _obj);
int        wxGridEvent_GetCol (TSelf(wxGridEvent) _obj);
TClass(wxPoint) wxGridEvent_GetPosition (TSelf(wxGridEvent) _obj);
int        wxGridEvent_GetRow (TSelf(wxGridEvent) _obj);
TBool      wxGridEvent_MetaDown (TSelf(wxGridEvent) _obj);
TBool      wxGridEvent_Selecting (TSelf(wxGridEvent) _obj);
TBool      wxGridEvent_ShiftDown (TSelf(wxGridEvent) _obj);

/* wxGridRangeSelectEvent */
TClassDefExtend(wxGridRangeSelectEvent,wxNotifyEvent)
void       wxGridRangeSelectEvent_GetTopLeftCoords (TSelf(wxGridRangeSelectEvent) _obj, TPointOutVoid(col,row));
void       wxGridRangeSelectEvent_GetBottomRightCoords (TSelf(wxGridRangeSelectEvent) _obj, TPointOutVoid(col,row));
int        wxGridRangeSelectEvent_GetTopRow (TSelf(wxGridRangeSelectEvent) _obj);
int        wxGridRangeSelectEvent_GetBottomRow (TSelf(wxGridRangeSelectEvent) _obj);
int        wxGridRangeSelectEvent_GetLeftCol (TSelf(wxGridRangeSelectEvent) _obj);
int        wxGridRangeSelectEvent_GetRightCol (TSelf(wxGridRangeSelectEvent) _obj);
TBool      wxGridRangeSelectEvent_Selecting (TSelf(wxGridRangeSelectEvent) _obj);
TBool      wxGridRangeSelectEvent_ControlDown (TSelf(wxGridRangeSelectEvent) _obj);
TBool      wxGridRangeSelectEvent_MetaDown (TSelf(wxGridRangeSelectEvent) _obj);
TBool      wxGridRangeSelectEvent_ShiftDown (TSelf(wxGridRangeSelectEvent) _obj);
TBool      wxGridRangeSelectEvent_AltDown (TSelf(wxGridRangeSelectEvent) _obj);

/* wxGridSizeEvent */
TClassDefExtend(wxGridSizeEvent,wxNotifyEvent)
int        wxGridSizeEvent_GetRowOrCol (TSelf(wxGridSizeEvent) _obj);
TClass(wxPoint) wxGridSizeEvent_GetPosition (TSelf(wxGridSizeEvent) _obj);
TBool      wxGridSizeEvent_ControlDown (TSelf(wxGridSizeEvent) _obj);
TBool      wxGridSizeEvent_MetaDown (TSelf(wxGridSizeEvent) _obj);
TBool      wxGridSizeEvent_ShiftDown (TSelf(wxGridSizeEvent) _obj);
TBool      wxGridSizeEvent_AltDown (TSelf(wxGridSizeEvent) _obj);


/* wxGridSizer */
TClassDefExtend(wxGridSizer,wxSizer)
TClass(wxSize) wxGridSizer_CalcMin( TSelf(wxGridSizer) _obj );
TClass(wxGridSizer) wxGridSizer_Create( int rows, int cols, int vgap, int hgap );
int        wxGridSizer_GetCols( TSelf(wxGridSizer) _obj );
int        wxGridSizer_GetHGap( TSelf(wxGridSizer) _obj );
int        wxGridSizer_GetRows( TSelf(wxGridSizer) _obj );
int        wxGridSizer_GetVGap( TSelf(wxGridSizer) _obj );
void       wxGridSizer_RecalcSizes( TSelf(wxGridSizer) _obj );
void       wxGridSizer_SetCols( TSelf(wxGridSizer) _obj, int cols );
void       wxGridSizer_SetHGap( TSelf(wxGridSizer) _obj, int gap );
void       wxGridSizer_SetRows( TSelf(wxGridSizer) _obj, int rows );
void       wxGridSizer_SetVGap( TSelf(wxGridSizer) _obj, int gap );

/* wxGridTableBase */
TClassDefExtend(wxGridTableBase,wxObject)

/* wxHTTP */
TClassDefExtend(wxHTTP,wxProtocol)

/* wxHashMap */
TClassDef(wxHashMap)

/* wxHelpController */
TClassDefExtend(wxHelpController,wxHelpControllerBase)

/* wxHelpControllerBase */
TClassDefExtend(wxHelpControllerBase,wxObject)

/* wxHelpControllerHelpProvider */
TClassDefExtend(wxHelpControllerHelpProvider,wxSimpleHelpProvider)
TClass(wxHelpControllerHelpProvider) wxHelpControllerHelpProvider_Create( TClass(wxHelpControllerBase) ctr );
TClass(wxHelpControllerBase) wxHelpControllerHelpProvider_GetHelpController( TSelf(wxHelpControllerHelpProvider) _obj );
void       wxHelpControllerHelpProvider_SetHelpController( TSelf(wxHelpControllerHelpProvider) _obj, TClass(wxHelpController) hc );

/* wxHelpEvent */
TClassDefExtend(wxHelpEvent,wxCommandEvent)
TClass(wxString) wxHelpEvent_GetLink( TSelf(wxHelpEvent) _obj );
TClass(wxPoint) wxHelpEvent_GetPosition( TSelf(wxHelpEvent) _obj );
TClass(wxString) wxHelpEvent_GetTarget( TSelf(wxHelpEvent) _obj );
void       wxHelpEvent_SetLink( TSelf(wxHelpEvent) _obj, TClass(wxString) link );
void       wxHelpEvent_SetPosition( TSelf(wxHelpEvent) _obj, TPoint(x,y) );
void       wxHelpEvent_SetTarget( TSelf(wxHelpEvent) _obj, TClass(wxString) target );

/* wxHelpProvider */
TClassDef(wxHelpProvider)
void       wxHelpProvider_AddHelp( TSelf(wxHelpProvider) _obj, TClass(wxWindow) window, TClass(wxString) text );
void       wxHelpProvider_AddHelpById( TSelf(wxHelpProvider) _obj, int id, TClass(wxString) text );
void       wxHelpProvider_Delete( TSelf(wxHelpProvider) _obj );
TSelf(wxHelpProvider) wxHelpProvider_Get(  );
TClass(wxString) wxHelpProvider_GetHelp( TSelf(wxHelpProvider) _obj, TClass(wxWindow) window );
void       wxHelpProvider_RemoveHelp( TSelf(wxHelpProvider) _obj, TClass(wxWindow) window );
TSelf(wxHelpProvider) wxHelpProvider_Set( TSelf(wxHelpProvider) helpProvider );
TBool      wxHelpProvider_ShowHelp( TSelf(wxHelpProvider) _obj, TClass(wxWindow) window );

/* wxHtmlCell */
TClassDefExtend(wxHtmlCell,wxObject)

/* wxHtmlColourCell */
TClassDefExtend(wxHtmlColourCell,wxHtmlCell)

/* wxHtmlContainerCell */
TClassDefExtend(wxHtmlContainerCell,wxHtmlCell)

/* wxHtmlDCRenderer */
TClassDefExtend(wxHtmlDCRenderer,wxObject)

/* wxHtmlEasyPrinting */
TClassDefExtend(wxHtmlEasyPrinting,wxObject)

/* wxHtmlFilter */
TClassDefExtend(wxHtmlFilter,wxObject)

/* wxHtmlHelpController */
TClassDefExtend(wxHtmlHelpController,wxHelpControllerBase)
TBool      wxHtmlHelpController_AddBook( TSelf(wxHtmlHelpController) _obj, void* book, int show_wait_msg );
TClass(wxHtmlHelpController) wxHtmlHelpController_Create( int _style );
void       wxHtmlHelpController_Delete( TSelf(wxHtmlHelpController) _obj );
int        wxHtmlHelpController_Display( TSelf(wxHtmlHelpController) _obj, void* x );
TBool      wxHtmlHelpController_DisplayBlock( TSelf(wxHtmlHelpController) _obj, int blockNo );
int        wxHtmlHelpController_DisplayContents( TSelf(wxHtmlHelpController) _obj );
int        wxHtmlHelpController_DisplayIndex( TSelf(wxHtmlHelpController) _obj );
int        wxHtmlHelpController_DisplayNumber( TSelf(wxHtmlHelpController) _obj, int id );
TBool      wxHtmlHelpController_DisplaySection( TSelf(wxHtmlHelpController) _obj, TClass(wxString) section );
TBool      wxHtmlHelpController_DisplaySectionNumber( TSelf(wxHtmlHelpController) _obj, int sectionNo );
TClass(wxFrame) wxHtmlHelpController_GetFrame( TSelf(wxHtmlHelpController) _obj );
void*      wxHtmlHelpController_GetFrameParameters( TSelf(wxHtmlHelpController) _obj, void* title, int* width, int* height, int* pos_x, int* pos_y, int* newFrameEachTime );
TBool      wxHtmlHelpController_Initialize( TSelf(wxHtmlHelpController) _obj, TClass(wxString) file );
TBool      wxHtmlHelpController_KeywordSearch( TSelf(wxHtmlHelpController) _obj, TClass(wxString) keyword );
TBool      wxHtmlHelpController_LoadFile( TSelf(wxHtmlHelpController) _obj, TClass(wxString) file );
TBool      wxHtmlHelpController_Quit( TSelf(wxHtmlHelpController) _obj );
void       wxHtmlHelpController_ReadCustomization( TSelf(wxHtmlHelpController) _obj, TClass(wxConfigBase) cfg, TClass(wxString) path );
void       wxHtmlHelpController_SetFrameParameters( TSelf(wxHtmlHelpController) _obj, void* title, TSize(width,height), int pos_x, int pos_y, TBool newFrameEachTime );
void       wxHtmlHelpController_SetTempDir( TSelf(wxHtmlHelpController) _obj, TClass(wxString) path );
void       wxHtmlHelpController_SetTitleFormat( TSelf(wxHtmlHelpController) _obj, void* format );
void       wxHtmlHelpController_SetViewer( TSelf(wxHtmlHelpController) _obj, TClass(wxString) viewer, int flags );
void       wxHtmlHelpController_UseConfig( TSelf(wxHtmlHelpController) _obj, TClass(wxConfigBase) config, TClass(wxString) rootpath );
void       wxHtmlHelpController_WriteCustomization( TSelf(wxHtmlHelpController) _obj, TClass(wxConfigBase) cfg, TClass(wxString) path );

/* wxHtmlHelpData */
TClassDefExtend(wxHtmlHelpData,wxObject)

/* wxHtmlHelpFrame */
TClassDefExtend(wxHtmlHelpFrame,wxFrame)

/* wxHtmlLinkInfo */
TClassDefExtend(wxHtmlLinkInfo,wxObject)

/* wxHtmlParser */
TClassDefExtend(wxHtmlParser,wxObject)

/* wxHtmlPrintout */
TClassDefExtend(wxHtmlPrintout,wxPrintout)

/* wxHtmlTag */
TClassDefExtend(wxHtmlTag,wxObject)

/* wxHtmlTagHandler */
TClassDefExtend(wxHtmlTagHandler,wxObject)

/* wxHtmlTagsModule */
TClassDefExtend(wxHtmlTagsModule,wxModule)

/* wxHtmlWidgetCell */
TClassDefExtend(wxHtmlWidgetCell,wxHtmlCell)

/* wxHtmlWinParser */
TClassDefExtend(wxHtmlWinParser,wxHtmlParser)

/* wxHtmlWinTagHandler */
TClassDefExtend(wxHtmlWinTagHandler,wxHtmlTagHandler)

/* wxHtmlWindow */
TClassDefExtend(wxHtmlWindow,wxScrolledWindow)

/* wxIPV4address */
TClassDefExtend(wxIPV4address,wxSockAddress)

/* wxIcon */
TClassDefExtend(wxIcon,wxBitmap)
void       wxIcon_Assign( TSelf(wxIcon) _obj, void* other );
void       wxIcon_CopyFromBitmap( TSelf(wxIcon) _obj, TClass(wxBitmap) bmp );
TClass(wxIcon) wxIcon_CreateDefault(  );
TClass(wxIcon) wxIcon_CreateLoad( TClass(wxString) name, long type, TSize(width,height) );
void       wxIcon_Delete( TSelf(wxIcon) _obj );
TClass(wxIcon) wxIcon_FromRaw( TSelf(wxIcon) data, TSize(width,height) );
TClass(wxIcon) wxIcon_FromXPM( TSelf(wxIcon) data );
int        wxIcon_GetDepth( TSelf(wxIcon) _obj );
int        wxIcon_GetHeight( TSelf(wxIcon) _obj );
int        wxIcon_GetWidth( TSelf(wxIcon) _obj );
TBool      wxIcon_IsEqual( TSelf(wxIcon) _obj, TSelf(wxIcon) other );
int        wxIcon_Load( TSelf(wxIcon) _obj, TClass(wxString) name, long type, TSize(width,height) );
TBool      wxIcon_IsOk( TSelf(wxIcon) _obj );
void       wxIcon_SetDepth( TSelf(wxIcon) _obj, int depth );
void       wxIcon_SetHeight( TSelf(wxIcon) _obj, int height );
void       wxIcon_SetWidth( TSelf(wxIcon) _obj, int width );

/* wxIconBundle */
TClassDef(wxIconBundle)
void       wxIconBundle_AddIcon( TSelf(wxIconBundle) _obj, TClass(wxIcon) icon );
void       wxIconBundle_AddIconFromFile( TSelf(wxIconBundle) _obj, TClass(wxString) file, int type );
void       wxIconBundle_Assign( TSelf(wxIconBundle) _obj, TClassRef(wxIconBundle) _ref );
TClass(wxIconBundle) wxIconBundle_CreateDefault(  );
TClass(wxIconBundle) wxIconBundle_CreateFromFile( TClass(wxString) file, int type );
TClass(wxIconBundle) wxIconBundle_CreateFromIcon( TClass(wxIcon) icon );
void       wxIconBundle_Delete( TSelf(wxIconBundle) _obj );
void       wxIconBundle_GetIcon( TSelf(wxIconBundle) _obj, TSize(w,h), TClassRef(wxIcon) _ref );

/* wxIconizeEvent */
TClassDefExtend(wxIconizeEvent,wxEvent)

/* wxIdleEvent */
TClassDefExtend(wxIdleEvent,wxEvent)
void       wxIdleEvent_CopyObject( TSelf(wxIdleEvent) _obj, TClass(wxObject) object_dest );
TBool      wxIdleEvent_MoreRequested( TSelf(wxIdleEvent) _obj );
void       wxIdleEvent_RequestMore( TSelf(wxIdleEvent) _obj, TBool needMore );

/* wxImage */
TClassDefExtend(wxImage,wxObject)
TBool      wxImage_CanRead( TClass(wxString) name );
void       wxImage_ConvertToBitmap( TSelf(wxImage) _obj, TClassRef(wxBitmap) bitmap );
TByteStringLen wxImage_ConvertToByteString( TSelf(wxImage) _obj, int type, TByteStringOut data  );
TByteStringLen wxImage_ConvertToLazyByteString( TSelf(wxImage) _obj, int type, TByteStringLazyOut data );
int        wxImage_CountColours( TSelf(wxImage) _obj, int stopafter );
void       wxImage_Copy( TSelf(wxImage) _obj, TClassRef(wxImage) image );
TClass(wxImage) wxImage_CreateDefault(  );
TClass(wxImage) wxImage_CreateFromBitmap( TClass(wxBitmap) bitmap );
// TClass(wxImage) wxImage_CreateFromByteString( TByteString(data,length), int type );
TClass(wxImage) wxImage_CreateFromByteString( TSize(width, height), TByteString(data, length) );
// TClass(wxImage) wxImage_CreateFromLazyByteString( TByteStringLazy(data,length), int type );
TClass(wxImage) wxImage_CreateFromLazyByteString( TSize(width, height), TByteStringLazy(data, length) );
TClass(wxImage) wxImage_CreateFromData( TSize(width,height), void* data );
TClass(wxImage) wxImage_CreateFromFile( TClass(wxString) name );
TClass(wxImage) wxImage_CreateSized( TSize(width,height) );
void       wxImage_Destroy( TSelf(wxImage) _obj );
TChar      wxImage_GetBlue( TSelf(wxImage) _obj, TPoint(x,y) );
void*      wxImage_GetData( TSelf(wxImage) _obj );
TChar      wxImage_GetGreen( TSelf(wxImage) _obj, TPoint(x,y) );
int        wxImage_GetHeight( TSelf(wxImage) _obj );
TChar      wxImage_GetMaskBlue( TSelf(wxImage) _obj );
TChar      wxImage_GetMaskGreen( TSelf(wxImage) _obj );
TChar      wxImage_GetMaskRed( TSelf(wxImage) _obj );
TChar      wxImage_GetRed( TSelf(wxImage) _obj, TPoint(x,y) );
void       wxImage_GetSubImage( TSelf(wxImage) _obj, TRect(x,y,w,h), TClassRef(wxImage) image );
int        wxImage_GetType( TSelf(wxImage) _obj );
void       wxImage_SetType( TSelf(wxImage) _obj ,int type);
int        wxImage_GetWidth( TSelf(wxImage) _obj );
TBool      wxImage_HasMask( TSelf(wxImage) _obj );
TClass(wxString) wxImage_GetOption( TSelf(wxImage) _obj, TClass(wxString) name );
TBool      wxImage_GetOptionInt( TSelf(wxImage) _obj, TClass(wxString) name );
TBool      wxImage_HasOption( TSelf(wxImage) _obj, TClass(wxString) name );
void       wxImage_Initialize( TSelf(wxImage) _obj, TSize(width,height) );
void       wxImage_InitializeFromData( TSelf(wxImage) _obj, TSize(width,height), void* data );
TBool      wxImage_LoadFile( TSelf(wxImage) _obj, TClass(wxString) name, int type );
TBool      wxImage_LoadStream( TSelf(wxImage) _obj, TClass(wxInputStream) name, int type, int index );
void       wxImage_Mirror( TSelf(wxImage) _obj, TBoolInt horizontally, TClassRef(wxImage) image );
TBool      wxImage_IsOk( TSelf(wxImage) _obj );
void       wxImage_Paste( TSelf(wxImage) _obj, TClass(wxImage) image, TPoint(x,y) );
void       wxImage_Replace( TSelf(wxImage) _obj, TColorRGB(r1,g1,b1), TColorRGB(r2,g2,b2) );
void       wxImage_Rescale( TSelf(wxImage) _obj, TSize(width,height) );
void       wxImage_RescaleEx( TSelf(wxImage) _obj, TSize(width,height), int quality);
void       wxImage_Rotate( TSelf(wxImage) _obj, double angle, TPoint(c_x,c_y), TBoolInt interpolating, void* offset_after_rotation, TClassRef(wxImage) image );
void       wxImage_Rotate90( TSelf(wxImage) _obj, TBoolInt clockwise, TClassRef(wxImage) image );
TBool      wxImage_SaveFile( TSelf(wxImage) _obj, TClass(wxString) name, int type );
TBool      wxImage_SaveStream( TSelf(wxImage) _obj, TClass(wxOutputStream) stream, int type );
void       wxImage_ScaleEx( TSelf(wxImage) _obj, TSize(width,height),int quality, TClassRef(wxImage) image );
void       wxImage_Scale( TSelf(wxImage) _obj, TSize(width,height), TClassRef(wxImage) image );
void       wxImage_SetData( TSelf(wxImage) _obj, void* data );
void       wxImage_SetDataAndSize( TSelf(wxImage) _obj, void* data, TSize(new_width,new_height) );
void       wxImage_SetMask( TSelf(wxImage) _obj, int mask );
void       wxImage_SetMaskColour( TSelf(wxImage) _obj, TColorRGB(r,g,b) );
void       wxImage_SetOption( TSelf(wxImage) _obj, TClass(wxString) name, TClass(wxString) value );
void       wxImage_SetOptionInt( TSelf(wxImage) _obj, TClass(wxString) name, int value );
void       wxImage_SetRGB( TSelf(wxImage) _obj, TPoint(x,y), TColorRGB(r,g,b) );

/* wxImageHandler */
TClassDefExtend(wxImageHandler,wxObject)

/* wxImageList */
TClassDefExtend(wxImageList,wxObject)
int        wxImageList_AddBitmap( TSelf(wxImageList) _obj, TClass(wxBitmap) bitmap, TClass(wxBitmap) mask );
int        wxImageList_AddIcon( TSelf(wxImageList) _obj, TClass(wxIcon) icon );
int        wxImageList_AddMasked( TSelf(wxImageList) _obj, TClass(wxBitmap) bitmap, TClass(wxColour) maskColour );
TClass(wxImageList) wxImageList_Create( TSize(width,height), TBoolInt mask, int initialCount );
void       wxImageList_Delete( TSelf(wxImageList) _obj );
TBool      wxImageList_Draw( TSelf(wxImageList) _obj, int index, TClass(wxDC) dc, TPoint(x,y), int flags, TBool solidBackground );
int        wxImageList_GetImageCount( TSelf(wxImageList) _obj );
void       wxImageList_GetSize( TSelf(wxImageList) _obj, int index, TSizeOut(width,height) );
TBool      wxImageList_Remove( TSelf(wxImageList) _obj, int index );
TBool      wxImageList_RemoveAll( TSelf(wxImageList) _obj );
TBool      wxImageList_Replace( TSelf(wxImageList) _obj, int index, TClass(wxBitmap) bitmap, TClass(wxBitmap) mask );
TBool      wxImageList_ReplaceIcon( TSelf(wxImageList) _obj, int index, TClass(wxIcon) icon );

/* wxIndividualLayoutConstraint */
TClassDefExtend(wxIndividualLayoutConstraint,wxObject)
void       wxIndividualLayoutConstraint_Above( TSelf(wxIndividualLayoutConstraint) _obj, TClass(wxWindow) sibling, int marg );
void       wxIndividualLayoutConstraint_Absolute( TSelf(wxIndividualLayoutConstraint) _obj, int val );
void       wxIndividualLayoutConstraint_AsIs( TSelf(wxIndividualLayoutConstraint) _obj );
void       wxIndividualLayoutConstraint_Below( TSelf(wxIndividualLayoutConstraint) _obj, TClass(wxWindow) sibling, int marg );
TBool      wxIndividualLayoutConstraint_GetDone( TSelf(wxIndividualLayoutConstraint) _obj );
int        wxIndividualLayoutConstraint_GetEdge( TSelf(wxIndividualLayoutConstraint) _obj, int which, void* thisWin, void* other );
int        wxIndividualLayoutConstraint_GetMargin( TSelf(wxIndividualLayoutConstraint) _obj );
int        wxIndividualLayoutConstraint_GetMyEdge( TSelf(wxIndividualLayoutConstraint) _obj );
int        wxIndividualLayoutConstraint_GetOtherEdge( TSelf(wxIndividualLayoutConstraint) _obj );
void*      wxIndividualLayoutConstraint_GetOtherWindow( TSelf(wxIndividualLayoutConstraint) _obj );
int        wxIndividualLayoutConstraint_GetPercent( TSelf(wxIndividualLayoutConstraint) _obj );
int        wxIndividualLayoutConstraint_GetRelationship( TSelf(wxIndividualLayoutConstraint) _obj );
int        wxIndividualLayoutConstraint_GetValue( TSelf(wxIndividualLayoutConstraint) _obj );
void       wxIndividualLayoutConstraint_LeftOf( TSelf(wxIndividualLayoutConstraint) _obj, TClass(wxWindow) sibling, int marg );
void       wxIndividualLayoutConstraint_PercentOf( TSelf(wxIndividualLayoutConstraint) _obj, TClass(wxWindow) otherW, int wh, int per );
TBool      wxIndividualLayoutConstraint_ResetIfWin( TSelf(wxIndividualLayoutConstraint) _obj, TClass(wxWindow) otherW );
void       wxIndividualLayoutConstraint_RightOf( TSelf(wxIndividualLayoutConstraint) _obj, TClass(wxWindow) sibling, int marg );
void       wxIndividualLayoutConstraint_SameAs( TSelf(wxIndividualLayoutConstraint) _obj, TClass(wxWindow) otherW, int edge, int marg );
TBool      wxIndividualLayoutConstraint_SatisfyConstraint( TSelf(wxIndividualLayoutConstraint) _obj, void* constraints, TClass(wxWindow) win );
void       wxIndividualLayoutConstraint_Set( TSelf(wxIndividualLayoutConstraint) _obj, int rel, TClass(wxWindow) otherW, int otherE, int val, int marg );
void       wxIndividualLayoutConstraint_SetDone( TSelf(wxIndividualLayoutConstraint) _obj, TBool d );
void       wxIndividualLayoutConstraint_SetEdge( TSelf(wxIndividualLayoutConstraint) _obj, int which );
void       wxIndividualLayoutConstraint_SetMargin( TSelf(wxIndividualLayoutConstraint) _obj, int m );
void       wxIndividualLayoutConstraint_SetRelationship( TSelf(wxIndividualLayoutConstraint) _obj, int r );
void       wxIndividualLayoutConstraint_SetValue( TSelf(wxIndividualLayoutConstraint) _obj, int v );
void       wxIndividualLayoutConstraint_Unconstrained( TSelf(wxIndividualLayoutConstraint) _obj );

/* wxInitDialogEvent */
TClassDefExtend(wxInitDialogEvent,wxEvent)

/* wxInputStream */
TClassDefExtend(wxInputStream,wxStreamBase)
void       wxInputStream_Delete( TSelf(wxInputStream) _obj );
TBool      wxInputStream_Eof( TSelf(wxInputStream) _obj );
TChar      wxInputStream_GetC( TSelf(wxInputStream) _obj );
int        wxInputStream_LastRead( TSelf(wxInputStream) _obj );
TChar      wxInputStream_Peek( TSelf(wxInputStream) _obj );
void       wxInputStream_Read( TSelf(wxInputStream) _obj, void* buffer, int size );
int        wxInputStream_SeekI( TSelf(wxInputStream) _obj, int pos, int mode );
int        wxInputStream_Tell( TSelf(wxInputStream) _obj );
int        wxInputStream_UngetBuffer( TSelf(wxInputStream) _obj, void* buffer, int size );
int        wxInputStream_Ungetch( TSelf(wxInputStream) _obj, TChar c );

/* wxJoystick */
TClassDefExtend(wxJoystick,wxObject)
TClass(wxJoystick) wxJoystick_Create( int joystick );
void       wxJoystick_Delete( TSelf(wxJoystick) _obj );
int        wxJoystick_GetButtonState( TSelf(wxJoystick) _obj );
int        wxJoystick_GetManufacturerId( TSelf(wxJoystick) _obj );
int        wxJoystick_GetMaxAxes( TSelf(wxJoystick) _obj );
int        wxJoystick_GetMaxButtons( TSelf(wxJoystick) _obj );
int        wxJoystick_GetMovementThreshold( TSelf(wxJoystick) _obj );
int        wxJoystick_GetNumberAxes( TSelf(wxJoystick) _obj );
int        wxJoystick_GetNumberButtons( TSelf(wxJoystick) _obj );
int        wxJoystick_GetNumberJoysticks( TSelf(wxJoystick) _obj );
int        wxJoystick_GetPOVCTSPosition( TSelf(wxJoystick) _obj );
int        wxJoystick_GetPOVPosition( TSelf(wxJoystick) _obj );
int        wxJoystick_GetPollingMax( TSelf(wxJoystick) _obj );
int        wxJoystick_GetPollingMin( TSelf(wxJoystick) _obj );
TClass(wxPoint) wxJoystick_GetPosition( TSelf(wxJoystick) _obj );
int        wxJoystick_GetProductId( TSelf(wxJoystick) _obj );
TClass(wxString) wxJoystick_GetProductName( TSelf(wxJoystick) _obj );
int        wxJoystick_GetRudderMax( TSelf(wxJoystick) _obj );
int        wxJoystick_GetRudderMin( TSelf(wxJoystick) _obj );
int        wxJoystick_GetRudderPosition( TSelf(wxJoystick) _obj );
int        wxJoystick_GetUMax( TSelf(wxJoystick) _obj );
int        wxJoystick_GetUMin( TSelf(wxJoystick) _obj );
int        wxJoystick_GetUPosition( TSelf(wxJoystick) _obj );
int        wxJoystick_GetVMax( TSelf(wxJoystick) _obj );
int        wxJoystick_GetVMin( TSelf(wxJoystick) _obj );
int        wxJoystick_GetVPosition( TSelf(wxJoystick) _obj );
int        wxJoystick_GetXMax( TSelf(wxJoystick) _obj );
int        wxJoystick_GetXMin( TSelf(wxJoystick) _obj );
int        wxJoystick_GetYMax( TSelf(wxJoystick) _obj );
int        wxJoystick_GetYMin( TSelf(wxJoystick) _obj );
int        wxJoystick_GetZMax( TSelf(wxJoystick) _obj );
int        wxJoystick_GetZMin( TSelf(wxJoystick) _obj );
int        wxJoystick_GetZPosition( TSelf(wxJoystick) _obj );
TBool      wxJoystick_HasPOV( TSelf(wxJoystick) _obj );
TBool      wxJoystick_HasPOV4Dir( TSelf(wxJoystick) _obj );
TBool      wxJoystick_HasPOVCTS( TSelf(wxJoystick) _obj );
TBool      wxJoystick_HasRudder( TSelf(wxJoystick) _obj );
TBool      wxJoystick_HasU( TSelf(wxJoystick) _obj );
TBool      wxJoystick_HasV( TSelf(wxJoystick) _obj );
TBool      wxJoystick_HasZ( TSelf(wxJoystick) _obj );
TBool      wxJoystick_IsOk( TSelf(wxJoystick) _obj );
int        wxJoystick_ReleaseCapture( TSelf(wxJoystick) _obj );
int        wxJoystick_SetCapture( TSelf(wxJoystick) _obj, TClass(wxWindow) win, int pollingFreq );
void       wxJoystick_SetMovementThreshold( TSelf(wxJoystick) _obj, int threshold );

/* wxJoystickEvent */
TClassDefExtend(wxJoystickEvent,wxEvent)
TBool      wxJoystickEvent_ButtonDown( TSelf(wxJoystickEvent) _obj, int but );
TBool      wxJoystickEvent_ButtonIsDown( TSelf(wxJoystickEvent) _obj, int but );
TBool      wxJoystickEvent_ButtonUp( TSelf(wxJoystickEvent) _obj, int but );
void       wxJoystickEvent_CopyObject( TSelf(wxJoystickEvent) _obj, void* obj );
int        wxJoystickEvent_GetButtonChange( TSelf(wxJoystickEvent) _obj );
int        wxJoystickEvent_GetButtonState( TSelf(wxJoystickEvent) _obj );
int        wxJoystickEvent_GetJoystick( TSelf(wxJoystickEvent) _obj );
TClass(wxPoint) wxJoystickEvent_GetPosition( TSelf(wxJoystickEvent) _obj );
int        wxJoystickEvent_GetZPosition( TSelf(wxJoystickEvent) _obj );
TBool      wxJoystickEvent_IsButton( TSelf(wxJoystickEvent) _obj );
TBool      wxJoystickEvent_IsMove( TSelf(wxJoystickEvent) _obj );
TBool      wxJoystickEvent_IsZMove( TSelf(wxJoystickEvent) _obj );
void       wxJoystickEvent_SetButtonChange( TSelf(wxJoystickEvent) _obj, int change );
void       wxJoystickEvent_SetButtonState( TSelf(wxJoystickEvent) _obj, int state );
void       wxJoystickEvent_SetJoystick( TSelf(wxJoystickEvent) _obj, int stick );
void       wxJoystickEvent_SetPosition( TSelf(wxJoystickEvent) _obj, TPoint(x,y) );
void       wxJoystickEvent_SetZPosition( TSelf(wxJoystickEvent) _obj, int zPos );

/* wxKeyEvent */
TClassDefExtend(wxKeyEvent,wxEvent)
TBool      wxKeyEvent_AltDown( TSelf(wxKeyEvent) _obj );
TBool      wxKeyEvent_ControlDown( TSelf(wxKeyEvent) _obj );
void       wxKeyEvent_CopyObject( TSelf(wxKeyEvent) _obj, void* obj );
int        wxKeyEvent_GetKeyCode( TSelf(wxKeyEvent) _obj );
TClass(wxPoint) wxKeyEvent_GetPosition( TSelf(wxKeyEvent) _obj );
int        wxKeyEvent_GetX( TSelf(wxKeyEvent) _obj );
int        wxKeyEvent_GetY( TSelf(wxKeyEvent) _obj );
int        wxKeyEvent_GetModifiers( TSelf(wxKeyEvent) _obj );
TBool      wxKeyEvent_HasModifiers( TSelf(wxKeyEvent) _obj );
TBool      wxKeyEvent_MetaDown( TSelf(wxKeyEvent) _obj );
void       wxKeyEvent_SetKeyCode( TSelf(wxKeyEvent) _obj, int code );
TBool      wxKeyEvent_ShiftDown( TSelf(wxKeyEvent) _obj );

/* wxLEDNumberCtrl */
TClassDefExtend(wxLEDNumberCtrl,wxControl)
TClass(wxLEDNumberCtrl) wxLEDNumberCtrl_Create( TClass(wxWindow) parent, int id, TRect(x,y,w,h), int style );
int        wxLEDNumberCtrl_GetAlignment( TSelf(wxLEDNumberCtrl) _obj );
int        wxLEDNumberCtrl_GetDrawFaded( TSelf(wxLEDNumberCtrl) _obj );
int        wxLEDNumberCtrl_GetValue( TSelf(wxLEDNumberCtrl) _obj, void* _ref );
void       wxLEDNumberCtrl_SetAlignment( TSelf(wxLEDNumberCtrl) _obj, int Alignment, int Redraw );
void       wxLEDNumberCtrl_SetDrawFaded( TSelf(wxLEDNumberCtrl) _obj, int DrawFaded, int Redraw );
void       wxLEDNumberCtrl_SetValue( TSelf(wxLEDNumberCtrl) _obj, void* Value, int Redraw );

/* wxLayoutAlgorithm */
TClassDefExtend(wxLayoutAlgorithm,wxObject)
TClass(wxLayoutAlgorithm) wxLayoutAlgorithm_Create(  );
void       wxLayoutAlgorithm_Delete( TSelf(wxLayoutAlgorithm) _obj );
TBool      wxLayoutAlgorithm_LayoutFrame( TSelf(wxLayoutAlgorithm) _obj, TClass(wxFrame) frame, void* mainWindow );
TBool      wxLayoutAlgorithm_LayoutMDIFrame( TSelf(wxLayoutAlgorithm) _obj, TClass(wxFrame) frame, TRect(x,y,w,h), int use );
TBool      wxLayoutAlgorithm_LayoutWindow( TSelf(wxLayoutAlgorithm) _obj, TClass(wxFrame) frame, void* mainWindow );

/* wxLayoutConstraints */
TClassDefExtend(wxLayoutConstraints,wxObject)
TClass(wxLayoutConstraints) wxLayoutConstraints_Create(  );
void*      wxLayoutConstraints_bottom( TSelf(wxLayoutConstraints) _obj );
void*      wxLayoutConstraints_centreX( TSelf(wxLayoutConstraints) _obj );
void*      wxLayoutConstraints_centreY( TSelf(wxLayoutConstraints) _obj );
void*      wxLayoutConstraints_height( TSelf(wxLayoutConstraints) _obj );
void*      wxLayoutConstraints_left( TSelf(wxLayoutConstraints) _obj );
void*      wxLayoutConstraints_right( TSelf(wxLayoutConstraints) _obj );
void*      wxLayoutConstraints_top( TSelf(wxLayoutConstraints) _obj );
void*      wxLayoutConstraints_width( TSelf(wxLayoutConstraints) _obj );

/* wxList */
TClassDefExtend(wxList,wxObject)

/* wxListBox */
TClassDefExtend(wxListBox,wxControl)
void       wxListBox_Append( TSelf(wxListBox) _obj, TClass(wxString) item );
void       wxListBox_AppendData( TSelf(wxListBox) _obj, TClass(wxString) item, void* data );
void       wxListBox_Clear( TSelf(wxListBox) _obj );
TClass(wxListBox) wxListBox_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), TArrayString(n,str), int _stl );
void       wxListBox_Delete( TSelf(wxListBox) _obj, int n );
int        wxListBox_FindString( TSelf(wxListBox) _obj, TClass(wxString) s );
TClass(wxClientData) wxListBox_GetClientData( TSelf(wxListBox) _obj, int n );
int        wxListBox_GetCount( TSelf(wxListBox) _obj );
int        wxListBox_GetSelection( TSelf(wxListBox) _obj );
int        wxListBox_GetSelections( TSelf(wxListBox) _obj, int* aSelections, int allocated );
TClass(wxString) wxListBox_GetString( TSelf(wxListBox) _obj, int n );
void       wxListBox_InsertItems( TSelf(wxListBox) _obj, void* items, int pos, int count );
TBool      wxListBox_IsSelected( TSelf(wxListBox) _obj, int n );
void       wxListBox_SetClientData( TSelf(wxListBox) _obj, int n, TClass(wxClientData) clientData );
void       wxListBox_SetFirstItem( TSelf(wxListBox) _obj, int n );
void       wxListBox_SetSelection( TSelf(wxListBox) _obj, int n, TBoolInt select );
void       wxListBox_SetString( TSelf(wxListBox) _obj, int n, TClass(wxString) s );
void       wxListBox_SetStringSelection( TSelf(wxListBox) _obj, TClass(wxString) str, TBool sel );

/* wxListCtrl */
TClassDefExtend(wxListCtrl,wxControl)
TBool      wxListCtrl_Arrange( TSelf(wxListCtrl) _obj, int flag );
void       wxListCtrl_ClearAll( TSelf(wxListCtrl) _obj );
TClass(wxListCtrl) wxListCtrl_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );
TBool      wxListCtrl_DeleteAllColumns( TSelf(wxListCtrl) _obj );
TBool      wxListCtrl_DeleteAllItems( TSelf(wxListCtrl) _obj );
TBool      wxListCtrl_DeleteColumn( TSelf(wxListCtrl) _obj, int col );
TBool      wxListCtrl_DeleteItem( TSelf(wxListCtrl) _obj, int item );
void       wxListCtrl_EditLabel( TSelf(wxListCtrl) _obj, int item );
TBool      wxListCtrl_EndEditLabel( TSelf(wxListCtrl) _obj, int cancel );
TBool      wxListCtrl_EnsureVisible( TSelf(wxListCtrl) _obj, int item );
int        wxListCtrl_FindItem( TSelf(wxListCtrl) _obj, int start, TClass(wxString) str, TBool partial );
int        wxListCtrl_FindItemByData( TSelf(wxListCtrl) _obj, int start, int data );
int        wxListCtrl_FindItemByPosition( TSelf(wxListCtrl) _obj, int start, TPoint(x,y), int direction );
TBool      wxListCtrl_GetColumn( TSelf(wxListCtrl) _obj, int col, TClass(wxListItem) item );
int        wxListCtrl_GetColumnCount( TSelf(wxListCtrl) _obj );
int        wxListCtrl_GetColumnWidth( TSelf(wxListCtrl) _obj, int col );
int        wxListCtrl_GetCountPerPage( TSelf(wxListCtrl) _obj );
TClass(wxTextCtrl)  wxListCtrl_GetEditControl( TSelf(wxListCtrl) _obj );
TClass(wxImageList) wxListCtrl_GetImageList( TSelf(wxListCtrl) _obj, int which );
TBool      wxListCtrl_GetItem( TSelf(wxListCtrl) _obj, TClass(wxListItem) info );
int        wxListCtrl_GetItemCount( TSelf(wxListCtrl) _obj );
int        wxListCtrl_GetItemData( TSelf(wxListCtrl) _obj, int item );
TClass(wxFont) wxListCtrl_GetItemFont( TSelf(wxListCtrl) _obj, long item);
TClass(wxPoint) wxListCtrl_GetItemPosition( TSelf(wxListCtrl) _obj, int item );
TClass(wxRect) wxListCtrl_GetItemRect( TSelf(wxListCtrl) _obj, int item, int code );
TClass(wxSize) wxListCtrl_GetItemSpacing( TSelf(wxListCtrl) _obj, TBool isSmall );
int        wxListCtrl_GetItemState( TSelf(wxListCtrl) _obj, int item, int stateMask );
TClass(wxString) wxListCtrl_GetItemText( TSelf(wxListCtrl) _obj, int item );
int        wxListCtrl_GetNextItem( TSelf(wxListCtrl) _obj, int item, int geometry, int state );
int        wxListCtrl_GetSelectedItemCount( TSelf(wxListCtrl) _obj );
void       wxListCtrl_GetTextColour( TSelf(wxListCtrl) _obj, TClassRef(wxColour) _ref );
int        wxListCtrl_GetTopItem( TSelf(wxListCtrl) _obj );
int        wxListCtrl_HitTest( TSelf(wxListCtrl) _obj, TPoint(x,y), void* flags );
int        wxListCtrl_InsertColumn( TSelf(wxListCtrl) _obj, int col, TClass(wxString) heading, int format, int width );
int        wxListCtrl_InsertColumnFromInfo( TSelf(wxListCtrl) _obj, int col, TClass(wxListItem) info );
int        wxListCtrl_InsertItem( TSelf(wxListCtrl) _obj, TClass(wxListItem) info );
int        wxListCtrl_InsertItemWithData( TSelf(wxListCtrl) _obj, int index, TClass(wxString) label );
int        wxListCtrl_InsertItemWithImage( TSelf(wxListCtrl) _obj, int index, int imageIndex );
int        wxListCtrl_InsertItemWithLabel( TSelf(wxListCtrl) _obj, int index, TClass(wxString) label, int imageIndex );
TBool      wxListCtrl_IsVirtual( TSelf(wxListCtrl) _obj );
void       wxListCtrl_RefreshItem( TSelf(wxListCtrl) _obj, long item );
TBool      wxListCtrl_ScrollList( TSelf(wxListCtrl) _obj, TVector(dx,dy) );
void       wxListCtrl_SetBackgroundColour( TSelf(wxListCtrl) _obj, TClass(wxColour) col );
TBool      wxListCtrl_SetColumn( TSelf(wxListCtrl) _obj, int col, TClass(wxListItem) item );
TBool      wxListCtrl_SetColumnWidth( TSelf(wxListCtrl) _obj, int col, int width );
int        wxListCtrl_SetForegroundColour( TSelf(wxListCtrl) _obj, TClass(wxColour) col );
void       wxListCtrl_SetImageList( TSelf(wxListCtrl) _obj, TClass(wxImageList) imageList, int which );
TBool      wxListCtrl_SetItem( TSelf(wxListCtrl) _obj, int index, int col, TClass(wxString) label, int imageId );
TBool      wxListCtrl_SetItemData( TSelf(wxListCtrl) _obj, int item, int data );
TBool      wxListCtrl_SetItemFromInfo( TSelf(wxListCtrl) _obj, TClass(wxListItem) info );
TBool      wxListCtrl_SetItemImage( TSelf(wxListCtrl) _obj, int item, int image, int selImage );
TBool      wxListCtrl_SetItemPosition( TSelf(wxListCtrl) _obj, int item, TPoint(x,y) );
TBool      wxListCtrl_SetItemState( TSelf(wxListCtrl) _obj, int item, int state, int stateMask );
void       wxListCtrl_SetItemText( TSelf(wxListCtrl) _obj, int item, TClass(wxString) str );
void       wxListCtrl_SetSingleStyle( TSelf(wxListCtrl) _obj, int style, TBool add );
void       wxListCtrl_SetTextColour( TSelf(wxListCtrl) _obj, TClass(wxColour) col );
void       wxListCtrl_SetWindowStyleFlag( TSelf(wxListCtrl) _obj, int style );
TBool      wxListCtrl_SortItems( TSelf(wxListCtrl) _obj, void* fn, void* eif_obj );
void       wxListCtrl_UpdateStyle( TSelf(wxListCtrl) _obj );

/* wxListEvent */
TClassDefExtend(wxListEvent,wxNotifyEvent)
TBool      wxListEvent_Cancelled( TSelf(wxListEvent) _obj );
int        wxListEvent_GetCode( TSelf(wxListEvent) _obj );
int        wxListEvent_GetColumn( TSelf(wxListEvent) _obj );
int        wxListEvent_GetData( TSelf(wxListEvent) _obj );
int        wxListEvent_GetImage( TSelf(wxListEvent) _obj );
int        wxListEvent_GetIndex( TSelf(wxListEvent) _obj );
void       wxListEvent_GetItem( TSelf(wxListEvent) _obj, TClassRef(wxListItem) _ref );
TClass(wxString) wxListEvent_GetLabel( TSelf(wxListEvent) _obj );
int        wxListEvent_GetMask( TSelf(wxListEvent) _obj );
/*
int        wxListEvent_GetOldIndex( TSelf(wxListEvent) _obj );
int        wxListEvent_GetOldItem( TSelf(wxListEvent) _obj );
*/
TClass(wxPoint) wxListEvent_GetPoint( TSelf(wxListEvent) _obj );
TClass(wxString) wxListEvent_GetText( TSelf(wxListEvent) _obj );

/* wxListItem */
TClassDefExtend(wxListItem,wxObject)
void       wxListItem_Clear( TSelf(wxListItem) _obj );
void       wxListItem_ClearAttributes( TSelf(wxListItem) _obj );
TClass(wxListItem) wxListItem_Create(  );
void       wxListItem_Delete( TSelf(wxListItem) _obj );
int        wxListItem_GetAlign( TSelf(wxListItem) _obj );
void*      wxListItem_GetAttributes( TSelf(wxListItem) _obj );
void       wxListItem_GetBackgroundColour( TSelf(wxListItem) _obj, TClassRef(wxColour) _ref );
int        wxListItem_GetColumn( TSelf(wxListItem) _obj );
int        wxListItem_GetData( TSelf(wxListItem) _obj );
void       wxListItem_GetFont( TSelf(wxListItem) _obj, TClassRef(wxFont) _ref );
int        wxListItem_GetId( TSelf(wxListItem) _obj );
int        wxListItem_GetImage( TSelf(wxListItem) _obj );
int        wxListItem_GetMask( TSelf(wxListItem) _obj );
int        wxListItem_GetState( TSelf(wxListItem) _obj );
TClass(wxString) wxListItem_GetText( TSelf(wxListItem) _obj );
void       wxListItem_GetTextColour( TSelf(wxListItem) _obj, TClassRef(wxColour) _ref );
int        wxListItem_GetWidth( TSelf(wxListItem) _obj );
TBool      wxListItem_HasAttributes( TSelf(wxListItem) _obj );
void       wxListItem_SetAlign( TSelf(wxListItem) _obj, int align );
void       wxListItem_SetBackgroundColour( TSelf(wxListItem) _obj, TClass(wxColour) colBack );
void       wxListItem_SetColumn( TSelf(wxListItem) _obj, int col );
void       wxListItem_SetData( TSelf(wxListItem) _obj, int data );
void       wxListItem_SetDataPointer( TSelf(wxListItem) _obj, void* data );
void       wxListItem_SetFont( TSelf(wxListItem) _obj, TClass(wxFont) font );
void       wxListItem_SetId( TSelf(wxListItem) _obj, int id );
void       wxListItem_SetImage( TSelf(wxListItem) _obj, int image );
void       wxListItem_SetMask( TSelf(wxListItem) _obj, int mask );
void       wxListItem_SetState( TSelf(wxListItem) _obj, int state );
void       wxListItem_SetStateMask( TSelf(wxListItem) _obj, int stateMask );
void       wxListItem_SetText( TSelf(wxListItem) _obj, TClass(wxString) text );
void       wxListItem_SetTextColour( TSelf(wxListItem) _obj, TClass(wxColour) colText );
void       wxListItem_SetWidth( TSelf(wxListItem) _obj, int width );

/* wxLocale */
TClassDef(wxLocale)
int        wxLocale_AddCatalog( TSelf(wxLocale) _obj, void* szDomain );
void       wxLocale_AddCatalogLookupPathPrefix( TSelf(wxLocale) _obj, void* prefix );
TClass(wxLocale) wxLocale_Create( int _name, int _flags );
void       wxLocale_Delete( TSelf(wxLocale) _obj );
TClass(wxLocale) wxLocale_GetLocale( TSelf(wxLocale) _obj );
TClass(wxString) wxLocale_GetName( TSelf(wxLocale) _obj );
TString    wxLocale_GetString( TSelf(wxLocale) _obj, void* szOrigString, void* szDomain );
TBool      wxLocale_IsLoaded( TSelf(wxLocale) _obj, void* szDomain );
TBool      wxLocale_IsOk( TSelf(wxLocale) _obj );

/* wxLog */
TClassDef(wxLog)

/* wxLogChain */
TClassDefExtend(wxLogChain,wxLog)
TClass(wxLogChain) wxLogChain_Create( TClass(wxLog) logger );
void       wxLogChain_Delete( TSelf(wxLogChain) _obj );
TClass(wxLog) wxLogChain_GetOldLog( TSelf(wxLogChain) _obj );
TBool      wxLogChain_IsPassingMessages( TSelf(wxLogChain) _obj );
void       wxLogChain_PassMessages( TSelf(wxLogChain) _obj, TBool bDoPass );
void       wxLogChain_SetLog( TSelf(wxLogChain) _obj, TClass(wxLog) logger );

/* wxLogGUI */
TClassDefExtend(wxLogGUI,wxLog)

/* wxLogNull */
TClassDefExtend(wxLogNull,wxLog)

/* wxLogPassThrough */
TClassDefExtend(wxLogPassThrough,wxLogChain)

/* wxLogStderr */
TClassDefExtend(wxLogStderr,wxLog)

/* wxLogStream */
TClassDefExtend(wxLogStream,wxLog)

/* wxLogTextCtrl */
TClassDefExtend(wxLogTextCtrl,wxLog)

/* wxLogWindow */
TClassDefExtend(wxLogWindow,wxLogPassThrough)

/* wxLongLong */
TClassDef(wxLongLong)

/* wxMBConv */
TClassDef(wxMBConv)

/* wxMBConvFile */
TClassDefExtend(wxMBConvFile,wxMBConv)

/* wxMBConvUTF7 */
TClassDefExtend(wxMBConvUTF7,wxMBConv)

/* wxMBConvUTF8 */
TClassDefExtend(wxMBConvUTF8,wxMBConv)

/* wxMDIChildFrame */
TClassDefExtend(wxMDIChildFrame,wxFrame)
void       wxMDIChildFrame_Activate( TSelf(wxMDIChildFrame) _obj );
TClass(wxMDIChildFrame) wxMDIChildFrame_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TRect(_lft,_top,_wdt,_hgt), int _stl );

/* wxMDIClientWindow */
TClassDefExtend(wxMDIClientWindow,wxWindow)

/* wxMDIParentFrame */
TClassDefExtend(wxMDIParentFrame,wxFrame)
void       wxMDIParentFrame_ActivateNext( TSelf(wxMDIParentFrame) _obj );
void       wxMDIParentFrame_ActivatePrevious( TSelf(wxMDIParentFrame) _obj );
void       wxMDIParentFrame_ArrangeIcons( TSelf(wxMDIParentFrame) _obj );
void       wxMDIParentFrame_Cascade( TSelf(wxMDIParentFrame) _obj );
TClass(wxMDIParentFrame)  wxMDIParentFrame_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TRect(_lft,_top,_wdt,_hgt), int _stl );
TClass(wxMDIChildFrame)   wxMDIParentFrame_GetActiveChild( TSelf(wxMDIParentFrame) _obj );
TClass(wxMDIClientWindow) wxMDIParentFrame_GetClientWindow( TSelf(wxMDIParentFrame) _obj );
TClass(wxMenu)            wxMDIParentFrame_GetWindowMenu( TSelf(wxMDIParentFrame) _obj );
TClass(wxMDIClientWindow) wxMDIParentFrame_OnCreateClient( TSelf(wxMDIParentFrame) _obj );
void       wxMDIParentFrame_SetWindowMenu( TSelf(wxMDIParentFrame) _obj, TClass(wxMenu) menu );
void       wxMDIParentFrame_Tile( TSelf(wxMDIParentFrame) _obj );

/* wxMask */
TClassDefExtend(wxMask,wxObject)
TClass(wxMask) wxMask_Create( TClass(wxBitmap) bitmap );
void*      wxMask_CreateColoured( TClass(wxBitmap) bitmap, TClass(wxColour) colour );

/* wxMaximizeEvent */
TClassDefExtend(wxMaximizeEvent,wxEvent)

/* wxMemoryDC */
TClassDefExtend(wxMemoryDC,wxDC)
TClass(wxMemoryDC) wxMemoryDC_Create(  );
TClass(wxMemoryDC) wxMemoryDC_CreateCompatible( TClass(wxDC) dc );
TClass(wxMemoryDC) wxMemoryDC_CreateWithBitmap( TClass(wxBitmap) bitmap );
void       wxMemoryDC_Delete( TSelf(wxMemoryDC) _obj );
void       wxMemoryDC_SelectObject( TSelf(wxMemoryDC) _obj, TClass(wxBitmap) bitmap );

/* wxMemoryFSHandler */
TClassDefExtend(wxMemoryFSHandler,wxFileSystemHandler)

/* wxMemoryInputStream */
TClassDefExtend(wxMemoryInputStream,wxInputStream)

/* wxMemoryOutputStream */
TClassDefExtend(wxMemoryOutputStream,wxOutputStream)

/* wxMenu */
TClassDefExtend(wxMenu,wxEvtHandler)
void       wxMenu_Append( TSelf(wxMenu) _obj, int id, TClass(wxString) text, TClass(wxString) help, TBool isCheckable );
void       wxMenu_AppendItem( TSelf(wxMenu) _obj, TClass(wxMenuItem) _itm );
void       wxMenu_AppendSeparator( TSelf(wxMenu) _obj );
void       wxMenu_AppendSub( TSelf(wxMenu) _obj, int id, TClass(wxString) text, TClass(wxMenu) submenu, TClass(wxString) help );
void       wxMenu_Break( TSelf(wxMenu) _obj );
void       wxMenu_Check( TSelf(wxMenu) _obj, int id, TBool check );
TClass(wxMenu) wxMenu_Create( TClass(wxString) title, long style );
void       wxMenu_DeleteById( TSelf(wxMenu) _obj, int id );
void       wxMenu_DeleteByItem( TSelf(wxMenu) _obj, TClass(wxMenuItem) _itm );
void       wxMenu_DeletePointer( TSelf(wxMenu) _obj );
void       wxMenu_DestroyById( TSelf(wxMenu) _obj, int id );
void       wxMenu_DestroyByItem( TSelf(wxMenu) _obj, TClass(wxMenuItem) _itm );
void       wxMenu_Enable( TSelf(wxMenu) _obj, int id, TBool enable );
TClass(wxMenuItem)  wxMenu_FindItem( TSelf(wxMenu) _obj, int id);
int        wxMenu_FindItemByLabel( TSelf(wxMenu) _obj, TClass(wxString) itemString );
TClass(wxClientData) wxMenu_GetClientData( TSelf(wxMenu) _obj );
TClass(wxString) wxMenu_GetHelpString( TSelf(wxMenu) _obj, int id );
TClass(wxWindow) wxMenu_GetInvokingWindow( TSelf(wxMenu) _obj );
// TClass(wxString) wxMenu_GetLabel( TSelf(wxMenu) _obj, int id );
TClass(wxString) wxMenu_GetLabelText( TSelf(wxMenu) _obj, int id );
size_t     wxMenu_GetMenuItemCount( TSelf(wxMenu) _obj );
int        wxMenu_GetMenuItems( TSelf(wxMenu) _obj, TClass(wxList) _lst );
TClass(wxMenu) wxMenu_GetParent( TSelf(wxMenu) _obj );
int        wxMenu_GetStyle( TSelf(wxMenu) _obj );
TClass(wxString) wxMenu_GetTitle( TSelf(wxMenu) _obj );
void       wxMenu_Insert( TSelf(wxMenu) _obj, size_t pos, int id, TClass(wxString) text, TClass(wxString) help, TBool isCheckable );
void       wxMenu_InsertItem( TSelf(wxMenu) _obj, size_t pos, TClass(wxMenuItem) _itm );
void       wxMenu_InsertSub( TSelf(wxMenu) _obj, size_t pos, int id, TClass(wxString) text, TClass(wxMenu) submenu, TClass(wxString) help );
TBool      wxMenu_IsAttached( TSelf(wxMenu) _obj );
TBool      wxMenu_IsChecked( TSelf(wxMenu) _obj, int id );
TBool      wxMenu_IsEnabled( TSelf(wxMenu) _obj, int id );
void       wxMenu_Prepend( TSelf(wxMenu) _obj, int id, TClass(wxString) text, TClass(wxString) help, TBool isCheckable );
void       wxMenu_PrependItem( TSelf(wxMenu) _obj, TClass(wxMenuItem) _itm );
void       wxMenu_PrependSub( TSelf(wxMenu) _obj, int id, TClass(wxString) text, TClass(wxMenu) submenu, TClass(wxString) help );
void       wxMenu_RemoveById( TSelf(wxMenu) _obj, int id, TClass(wxMenuItem) _itm );
void       wxMenu_RemoveByItem( TSelf(wxMenu) _obj, void* item );
void       wxMenu_SetClientData( TSelf(wxMenu) _obj, TClass(wxClientData) clientData );
void       wxMenu_SetEventHandler( TSelf(wxMenu) _obj, TClass(wxEvtHandler) handler );
void       wxMenu_SetHelpString( TSelf(wxMenu) _obj, int id, TClass(wxString) helpString );
void       wxMenu_SetInvokingWindow( TSelf(wxMenu) _obj, TClass(wxWindow) win );
void       wxMenu_SetLabel( TSelf(wxMenu) _obj, int id, TClass(wxString) label );
void       wxMenu_SetParent( TSelf(wxMenu) _obj, TClass(wxWindow) parent );
void       wxMenu_SetTitle( TSelf(wxMenu) _obj, TClass(wxString) title );
void       wxMenu_UpdateUI( TSelf(wxMenu) _obj, void* source );

/* wxMenuBar */
TClassDefExtend(wxMenuBar,wxEvtHandler)
int        wxMenuBar_Append( TSelf(wxMenuBar) _obj, TClass(wxMenu) menu, TClass(wxString) title );
void       wxMenuBar_Check( TSelf(wxMenuBar) _obj, int id, TBool check );
TClass(wxMenuBar) wxMenuBar_Create( int _style );
void       wxMenuBar_DeletePointer( TSelf(wxMenuBar) _obj );
int        wxMenuBar_Enable( TSelf(wxMenuBar) _obj, TBool enable );
void       wxMenuBar_EnableItem( TSelf(wxMenuBar) _obj, int id, TBool enable );
void       wxMenuBar_EnableTop( TSelf(wxMenuBar) _obj, int pos, TBool enable );
TClass(wxMenuItem) wxMenuBar_FindItem( TSelf(wxMenuBar) _obj, int id);
int        wxMenuBar_FindMenu( TSelf(wxMenuBar) _obj, TClass(wxString) title );
int        wxMenuBar_FindMenuItem( TSelf(wxMenuBar) _obj, TClass(wxString) menuString, TClass(wxString) itemString );
TClass(wxString) wxMenuBar_GetHelpString( TSelf(wxMenuBar) _obj, int id );
TClass(wxString) wxMenuBar_GetLabel( TSelf(wxMenuBar) _obj, int id );
// TClass(wxString) wxMenuBar_GetLabelTop( TSelf(wxMenuBar) _obj, int pos );
TClass(wxString) wxMenuBar_GetMenuLabel( TSelf(wxMenuBar) _obj, int pos );
TClass(wxMenu) wxMenuBar_GetMenu( TSelf(wxMenuBar) _obj, int pos );
int        wxMenuBar_GetMenuCount( TSelf(wxMenuBar) _obj );
int        wxMenuBar_Insert( TSelf(wxMenuBar) _obj, int pos, TClass(wxMenu) menu, TClass(wxString) title );
TBool      wxMenuBar_IsChecked( TSelf(wxMenuBar) _obj, int id );
TBool      wxMenuBar_IsEnabled( TSelf(wxMenuBar) _obj, int id );
TClass(wxMenu) wxMenuBar_Remove( TSelf(wxMenuBar) _obj, int pos );
TClass(wxMenu) wxMenuBar_Replace( TSelf(wxMenuBar) _obj, int pos, TClass(wxMenu) menu, TClass(wxString) title );
void       wxMenuBar_SetHelpString( TSelf(wxMenuBar) _obj, int id, TClass(wxString) helpString );
void       wxMenuBar_SetItemLabel( TSelf(wxMenuBar) _obj, int id, TClass(wxString) label );
void       wxMenuBar_SetLabel( TSelf(wxMenuBar) _obj, TClass(wxString) s );
// void       wxMenuBar_SetLabelTop( TSelf(wxMenuBar) _obj, int pos, TClass(wxString) label );
void       wxMenuBar_SetMenuLabel( TSelf(wxMenuBar) _obj, int pos, TClass(wxString) label );

/* wxMenuEvent */
TClassDefExtend(wxMenuEvent,wxEvent)
void       wxMenuEvent_CopyObject( TSelf(wxMenuEvent) _obj, void* obj );
int        wxMenuEvent_GetMenuId( TSelf(wxMenuEvent) _obj );

/* wxMenuItem */
TClassDefExtend(wxMenuItem,wxObject)
void       wxMenuItem_Check( TSelf(wxMenuItem) _obj, TBool check );
TClass(wxMenuItem) wxMenuItem_Create(  );
void       wxMenuItem_Delete( TSelf(wxMenuItem) _obj );
void       wxMenuItem_Enable( TSelf(wxMenuItem) _obj, TBool enable );
TClass(wxString) wxMenuItem_GetHelp( TSelf(wxMenuItem) _obj );
int        wxMenuItem_GetId( TSelf(wxMenuItem) _obj );
// TClass(wxString) wxMenuItem_GetLabel( TSelf(wxMenuItem) _obj );
TClass(wxString) wxMenuItem_GetItemLabelText( TSelf(wxMenuItem) _obj );
// TClass(wxString) wxMenuItem_GetLabelFromText( TStringVoid text );
TClass(wxString) wxMenuItem_GetLabelText( TStringVoid text );
TClass(wxMenu) wxMenuItem_GetMenu( TSelf(wxMenuItem) _obj );
TClass(wxMenu) wxMenuItem_GetSubMenu( TSelf(wxMenuItem) _obj );
// TClass(wxString) wxMenuItem_GetText( TSelf(wxMenuItem) _obj );
TClass(wxString) wxMenuItem_GetItemLabel( TSelf(wxMenuItem) _obj );
TBool      wxMenuItem_IsCheckable( TSelf(wxMenuItem) _obj );
TBool      wxMenuItem_IsChecked( TSelf(wxMenuItem) _obj );
TBool      wxMenuItem_IsEnabled( TSelf(wxMenuItem) _obj );
TBool      wxMenuItem_IsSeparator( TSelf(wxMenuItem) _obj );
TBool      wxMenuItem_IsSubMenu( TSelf(wxMenuItem) _obj );
void       wxMenuItem_SetCheckable( TSelf(wxMenuItem) _obj, TBool checkable );
void       wxMenuItem_SetHelp( TSelf(wxMenuItem) _obj, TClass(wxString) str );
void       wxMenuItem_SetId( TSelf(wxMenuItem) _obj, int id );
void       wxMenuItem_SetSubMenu( TSelf(wxMenuItem) _obj, TClass(wxMenu) menu );
// void       wxMenuItem_SetText( TSelf(wxMenuItem) _obj, TClass(wxString) str );
void       wxMenuItem_SetItemLabel( TSelf(wxMenuItem) _obj, TClass(wxString) str );

/* wxMessageDialog */
TClassDefExtend(wxMessageDialog,wxDialog)
TClass(wxMessageDialog) wxMessageDialog_Create( TClass(wxWindow) _prt, TClass(wxString) _msg, TClass(wxString) _cap, int _stl );
void       wxMessageDialog_Delete( TSelf(wxMessageDialog) _obj );
int        wxMessageDialog_ShowModal( TSelf(wxMessageDialog) _obj );

/* wxMetafile */
TClassDefExtend(wxMetafile,wxObject)
TClass(wxMetafile) wxMetafile_Create( TClass(wxString) _file );
void       wxMetafile_Delete( TSelf(wxMetafile) _obj );
TBool      wxMetafile_IsOk( TSelf(wxMetafile) _obj );
TBool      wxMetafile_Play( TSelf(wxMetafile) _obj, TClass(wxDC) _dc );
TBool      wxMetafile_SetClipboard( TSelf(wxMetafile) _obj, TSize(width,height) );

/* wxMetafileDC */
TClassDefExtend(wxMetafileDC,wxDC)
void*      wxMetafileDC_Close( TSelf(wxMetafileDC) _obj );
TClass(wxMetafileDC) wxMetafileDC_Create( TClass(wxString) _file );
void       wxMetafileDC_Delete( TSelf(wxMetafileDC) _obj );

/* wxMimeTypesManager */
TClassDef(wxMimeTypesManager)
void       wxMimeTypesManager_AddFallbacks( TSelf(wxMimeTypesManager) _obj, void* _types );
TClass(wxMimeTypesManager) wxMimeTypesManager_Create(  );
int        wxMimeTypesManager_EnumAllFileTypes( TSelf(wxMimeTypesManager) _obj, TClass(wxList) _lst );
TClass(wxFileType) wxMimeTypesManager_GetFileTypeFromExtension( TSelf(wxMimeTypesManager) _obj, TClass(wxString) _ext );
TClass(wxFileType) wxMimeTypesManager_GetFileTypeFromMimeType( TSelf(wxMimeTypesManager) _obj, TClass(wxString) _name );
TBool      wxMimeTypesManager_IsOfType( TSelf(wxMimeTypesManager) _obj, TClass(wxString) _type, TClass(wxString) _wildcard );

/* wxMiniFrame */
TClassDefExtend(wxMiniFrame,wxFrame)
TClass(wxMiniFrame) wxMiniFrame_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TRect(_lft,_top,_wdt,_hgt), int _stl );

/* wxMirrorDC */
TClassDefExtend(wxMirrorDC,wxDC)
TClass(wxMirrorDC) wxMirrorDC_Create( TClass(wxDC) dc );
void       wxMirrorDC_Delete( TSelf(wxMemoryDC) _obj );

/* wxModule */
TClassDefExtend(wxModule,wxObject)

/* wxMouseCaptureChangedEvent */
TClassDefExtend(wxMouseCaptureChangedEvent,wxEvent)

/* wxMouseEvent */
TClassDefExtend(wxMouseEvent,wxEvent)
TBool      wxMouseEvent_AltDown( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_Button( TSelf(wxMouseEvent) _obj, int but );
TBool      wxMouseEvent_ButtonDClick( TSelf(wxMouseEvent) _obj, int but );
TBool      wxMouseEvent_ButtonDown( TSelf(wxMouseEvent) _obj, int but );
TBool      wxMouseEvent_ButtonIsDown( TSelf(wxMouseEvent) _obj, int but );
TBool      wxMouseEvent_ButtonUp( TSelf(wxMouseEvent) _obj, int but );
TBool      wxMouseEvent_ControlDown( TSelf(wxMouseEvent) _obj );
void       wxMouseEvent_CopyObject( TSelf(wxMouseEvent) _obj, void* object_dest );
TBool      wxMouseEvent_Dragging( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_Entering( TSelf(wxMouseEvent) _obj );
TClass(wxPoint) wxMouseEvent_GetLogicalPosition( TSelf(wxMouseEvent) _obj, TClass(wxDC) dc );
TClass(wxPoint) wxMouseEvent_GetPosition( TSelf(wxMouseEvent) _obj );
int        wxMouseEvent_GetX( TSelf(wxMouseEvent) _obj );
int        wxMouseEvent_GetY( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_IsButton( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_Leaving( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_LeftDClick( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_LeftDown( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_LeftIsDown( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_LeftUp( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_MetaDown( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_MiddleDClick( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_MiddleDown( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_MiddleIsDown( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_MiddleUp( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_Moving( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_RightDClick( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_RightDown( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_RightIsDown( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_RightUp( TSelf(wxMouseEvent) _obj );
TBool      wxMouseEvent_ShiftDown( TSelf(wxMouseEvent) _obj );

/* wxMoveEvent */
TClassDefExtend(wxMoveEvent,wxEvent)
void       wxMoveEvent_CopyObject( TSelf(wxMoveEvent) _obj, void* obj );
TClass(wxPoint) wxMoveEvent_GetPosition( TSelf(wxMoveEvent) _obj );

/* wxMultiCellCanvas */
TClassDefExtend(wxMultiCellCanvas,wxFlexGridSizer)
void       wxMultiCellCanvas_Add( TSelf(wxMultiCellCanvas) _obj, TClass(wxWindow) win, int row, int col );
void       wxMultiCellCanvas_CalculateConstraints( TSelf(wxMultiCellCanvas) _obj );
TClass(wxMultiCellCanvas) wxMultiCellCanvas_Create( TClass(wxWindow) parent, int numRows, int numCols );
int        wxMultiCellCanvas_MaxCols( TSelf(wxMultiCellCanvas) _obj );
int        wxMultiCellCanvas_MaxRows( TSelf(wxMultiCellCanvas) _obj );
void       wxMultiCellCanvas_SetMinCellSize( TSelf(wxMultiCellCanvas) _obj, TSize(w,h) );

/* wxMultiCellItemHandle */
TClassDefExtend(wxMultiCellItemHandle,wxObject)
TClass(wxMultiCellItemHandle) wxMultiCellItemHandle_Create( int row, int column, int height, int width, int sx, int sy, int style, int wx, int wy, int align );
void*      wxMultiCellItemHandle_CreateWithSize( TSelf(wxMultiCellItemHandle) _obj, int row, int column, int sx, int sy, int style, int wx, int wy, int align );
void*      wxMultiCellItemHandle_CreateWithStyle( TSelf(wxMultiCellItemHandle) _obj, int row, int column, int style, int wx, int wy, int align );
int        wxMultiCellItemHandle_GetAlignment( TSelf(wxMultiCellItemHandle) _obj );
int        wxMultiCellItemHandle_GetColumn( TSelf(wxMultiCellItemHandle) _obj );
int        wxMultiCellItemHandle_GetHeight( TSelf(wxMultiCellItemHandle) _obj );
void       wxMultiCellItemHandle_GetLocalSize( TSelf(wxMultiCellItemHandle) _obj, TSizeOutVoid(_w,_h) );
int        wxMultiCellItemHandle_GetRow( TSelf(wxMultiCellItemHandle) _obj );
int        wxMultiCellItemHandle_GetStyle( TSelf(wxMultiCellItemHandle) _obj );
void       wxMultiCellItemHandle_GetWeight( TSelf(wxMultiCellItemHandle) _obj, TSizeOutVoid(_w,_h) );
int        wxMultiCellItemHandle_GetWidth( TSelf(wxMultiCellItemHandle) _obj );

/* wxMultiCellSizer */
TClassDefExtend(wxMultiCellSizer,wxSizer)
void       wxMultiCellSizer_CalcMin( TSelf(wxMultiCellSizer) _obj, TSizeOutVoid(_w,_h) );
TClass(wxMultiCellSizer) wxMultiCellSizer_Create( int rows, int cols );
void       wxMultiCellSizer_Delete( TSelf(wxMultiCellSizer) _obj );
int        wxMultiCellSizer_EnableGridLines( TSelf(wxMultiCellSizer) _obj, TClass(wxWindow) win );
void       wxMultiCellSizer_RecalcSizes( TSelf(wxMultiCellSizer) _obj );
int        wxMultiCellSizer_SetColumnWidth( TSelf(wxMultiCellSizer) _obj, int column, int colSize, int expandable );
int        wxMultiCellSizer_SetDefaultCellSize( TSelf(wxMultiCellSizer) _obj, TSize(w,h) );
int        wxMultiCellSizer_SetGridPen( TSelf(wxMultiCellSizer) _obj, TClass(wxPen) pen );
int        wxMultiCellSizer_SetRowHeight( TSelf(wxMultiCellSizer) _obj, int row, int rowSize, int expandable );

/* wxMutex */
TClassDef(wxMutex)
TClass(wxMutex) wxMutex_Create(  );
void       wxMutex_Delete( TSelf(wxMutex) _obj );
TBool      wxMutex_IsLocked( TSelf(wxMutex) _obj );
int        wxMutex_Lock( TSelf(wxMutex) _obj );
int        wxMutex_TryLock( TSelf(wxMutex) _obj );
int        wxMutex_Unlock( TSelf(wxMutex) _obj );

/* wxMutexLocker */
TClassDef(wxMutexLocker)

/* wxNavigationKeyEvent */
TClassDefExtend(wxNavigationKeyEvent,wxEvent)
void*      wxNavigationKeyEvent_GetCurrentFocus( TSelf(wxNavigationKeyEvent) _obj );
TBool      wxNavigationKeyEvent_GetDirection( TSelf(wxNavigationKeyEvent) _obj );
TBool      wxNavigationKeyEvent_IsWindowChange( TSelf(wxNavigationKeyEvent) _obj );
void       wxNavigationKeyEvent_SetCurrentFocus( TSelf(wxNavigationKeyEvent) _obj, TClass(wxWindow) win );
void       wxNavigationKeyEvent_SetDirection( TSelf(wxNavigationKeyEvent) _obj, TBool bForward );
/* void       wxNavigationKeyEvent_SetPropagate( TSelf(wxNavigationKeyEvent) _obj, int bDoIt );*/
void       wxNavigationKeyEvent_SetWindowChange( TSelf(wxNavigationKeyEvent) _obj, TBool bIs );
int        wxNavigationKeyEvent_ShouldPropagate( TSelf(wxNavigationKeyEvent) _obj );

/* wxNewBitmapButton */
TClassDefExtend(wxNewBitmapButton,wxPanel)
TClass(wxNewBitmapButton) wxNewBitmapButton_Create( void* labelBitmap, void* labelText, int alignText, TBool isFlat, int firedEventType, int marginX, int marginY, int textToLabelGap, TBool isSticky );
TClass(wxNewBitmapButton) wxNewBitmapButton_CreateFromFile( TSelf(wxNewBitmapButton) bitmapFileName, int bitmapFileType, void* labelText, int alignText, TBool isFlat, int firedEventType, int marginX, int marginY, int textToLabelGap, TBool isSticky );
void       wxNewBitmapButton_Delete( TSelf(wxNewBitmapButton) _obj );
void       wxNewBitmapButton_DrawDecorations( TSelf(wxNewBitmapButton) _obj, TClass(wxDC) dc );
void       wxNewBitmapButton_DrawLabel( TSelf(wxNewBitmapButton) _obj, TClass(wxDC) dc );
int        wxNewBitmapButton_Enable( TSelf(wxNewBitmapButton) _obj, TBool enable );
void       wxNewBitmapButton_Realize( TSelf(wxNewBitmapButton) _obj, TClass(wxWindow) _prt, int _id, TRect(_x,_y,_w,_h) );
void       wxNewBitmapButton_RenderAllLabelImages( TSelf(wxNewBitmapButton) _obj );
void       wxNewBitmapButton_RenderLabelImage( TSelf(wxNewBitmapButton) _obj, void* destBmp, void* srcBmp, TBool isEnabled, TBool isPressed );
void       wxNewBitmapButton_RenderLabelImages( TSelf(wxNewBitmapButton) _obj );
void       wxNewBitmapButton_Reshape( TSelf(wxNewBitmapButton) _obj );
void       wxNewBitmapButton_SetAlignments( TSelf(wxNewBitmapButton) _obj, int alignText, int marginX, int marginY, int textToLabelGap );
void       wxNewBitmapButton_SetLabel( TSelf(wxNewBitmapButton) _obj, void* labelBitmap, void* labelText );

/* wxNodeBase */
TClassDef(wxNodeBase)

/* wxNotebook */
TClassDefExtend(wxNotebook,wxControl)
TBool      wxNotebook_AddPage( TSelf(wxNotebook) _obj, TClass(wxWindow) pPage, TClass(wxString) strText, TBool bSelect, int imageId );
void       wxNotebook_AdvanceSelection( TSelf(wxNotebook) _obj, TBool bForward );
TClass(wxNotebook) wxNotebook_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );
TBool      wxNotebook_DeleteAllPages( TSelf(wxNotebook) _obj );
TBool      wxNotebook_DeletePage( TSelf(wxNotebook) _obj, int nPage );
TClass(wxImageList) wxNotebook_GetImageList( TSelf(wxNotebook) _obj );
TClass(wxWindow)    wxNotebook_GetPage( TSelf(wxNotebook) _obj, int nPage );
int        wxNotebook_GetPageCount( TSelf(wxNotebook) _obj );
int        wxNotebook_GetPageImage( TSelf(wxNotebook) _obj, int nPage );
TClass(wxString) wxNotebook_GetPageText( TSelf(wxNotebook) _obj, int nPage );
int        wxNotebook_GetRowCount( TSelf(wxNotebook) _obj );
int        wxNotebook_GetSelection( TSelf(wxNotebook) _obj );
int        wxNotebook_HitTest( TSelf(wxNotebook) _obj, TPoint(x,y), long* flags );
TBool      wxNotebook_InsertPage( TSelf(wxNotebook) _obj, int nPage, TClass(wxWindow) pPage, TClass(wxString) strText, TBool bSelect, int imageId );
TBool      wxNotebook_RemovePage( TSelf(wxNotebook) _obj, int nPage );
void       wxNotebook_SetImageList( TSelf(wxNotebook) _obj, TClass(wxImageList) imageList );
void       wxNotebook_SetPadding( TSelf(wxNotebook) _obj, TSize(_w,_h) );
TBool      wxNotebook_SetPageImage( TSelf(wxNotebook) _obj, int nPage, int nImage );
void       wxNotebook_SetPageSize( TSelf(wxNotebook) _obj, TSize(_w,_h) );
TBool      wxNotebook_SetPageText( TSelf(wxNotebook) _obj, int nPage, TClass(wxString) strText );
int        wxNotebook_SetSelection( TSelf(wxNotebook) _obj, int nPage );

int        expNB_TOP(  );
int        expNB_BOTTOM(  );
int        expNB_LEFT(  );
int        expNB_RIGHT(  );

int        expBK_HITTEST_NOWHERE(  );
int        expBK_HITTEST_ONICON(  );
int        expBK_HITTEST_ONLABEL(  );
int        expBK_HITTEST_ONITEM(  );
int        expBK_HITTEST_ONPAGE(  );

/* wxNotebookEvent */
TClassDefExtend(wxNotebookEvent,wxNotifyEvent)

/* wxNotebookSizer */
/* Class removed from wxWidgets >= 2.8 */

/* wxNotifyEvent */
TClassDefExtend(wxNotifyEvent,wxCommandEvent)
void       wxNotifyEvent_Allow( TSelf(wxNotifyEvent) _obj );
void       wxNotifyEvent_CopyObject( TSelf(wxNotifyEvent) _obj, void* object_dest );
TBool      wxNotifyEvent_IsAllowed( TSelf(wxNotifyEvent) _obj );
void       wxNotifyEvent_Veto( TSelf(wxNotifyEvent) _obj );

/* wxObject */
TClassDef(wxObject)

/* wxObjectRefData */
TClassDef(wxObjectRefData)

/* wxOutputStream */
TClassDefExtend(wxOutputStream,wxStreamBase)
void       wxOutputStream_Delete( TSelf(wxOutputStream) _obj );
int        wxOutputStream_LastWrite( TSelf(wxOutputStream) _obj );
void       wxOutputStream_PutC( TSelf(wxOutputStream) _obj, TChar c );
int        wxOutputStream_Seek( TSelf(wxOutputStream) _obj, int pos, int mode );
void       wxOutputStream_Sync( TSelf(wxOutputStream) _obj );
int        wxOutputStream_Tell( TSelf(wxOutputStream) _obj );
void       wxOutputStream_Write( TSelf(wxOutputStream) _obj, void* buffer, int size );

/* wxPageSetupDialog */
TClassDefExtend(wxPageSetupDialog,wxDialog)
TClass(wxPageSetupDialog) wxPageSetupDialog_Create( TClass(wxWindow) parent, TClass(wxPageSetupDialogData) data );
void       wxPageSetupDialog_GetPageSetupData( TSelf(wxPageSetupDialog) _obj, TClassRef(wxPageSetupDialogData) _ref );

/* wxPageSetupDialogData */
TClassDefExtend(wxPageSetupDialogData,wxObject)
void       wxPageSetupDialogData_Assign( TSelf(wxPageSetupDialogData) _obj, TClassRef(wxPageSetupDialogData) data );
void       wxPageSetupDialogData_AssignData( TSelf(wxPageSetupDialogData) _obj, TClass(wxPrintData) printData );
void       wxPageSetupDialogData_CalculateIdFromPaperSize( TSelf(wxPageSetupDialogData) _obj );
void       wxPageSetupDialogData_CalculatePaperSizeFromId( TSelf(wxPageSetupDialogData) _obj );
TClass(wxPageSetupDialogData) wxPageSetupDialogData_Create(  );
TClass(wxPageSetupDialogData) wxPageSetupDialogData_CreateFromData( TClass(wxPrintData) printData );
void       wxPageSetupDialogData_Delete( TSelf(wxPageSetupDialogData) _obj );
void       wxPageSetupDialogData_EnableHelp( TSelf(wxPageSetupDialogData) _obj, TBool flag );
void       wxPageSetupDialogData_EnableMargins( TSelf(wxPageSetupDialogData) _obj, TBool flag );
void       wxPageSetupDialogData_EnableOrientation( TSelf(wxPageSetupDialogData) _obj, TBool flag );
void       wxPageSetupDialogData_EnablePaper( TSelf(wxPageSetupDialogData) _obj, TBool flag );
void       wxPageSetupDialogData_EnablePrinter( TSelf(wxPageSetupDialogData) _obj, TBool flag );
TBool      wxPageSetupDialogData_GetDefaultInfo( TSelf(wxPageSetupDialogData) _obj );
TBool      wxPageSetupDialogData_GetDefaultMinMargins( TSelf(wxPageSetupDialogData) _obj );
TBool      wxPageSetupDialogData_GetEnableHelp( TSelf(wxPageSetupDialogData) _obj );
TBool      wxPageSetupDialogData_GetEnableMargins( TSelf(wxPageSetupDialogData) _obj );
TBool      wxPageSetupDialogData_GetEnableOrientation( TSelf(wxPageSetupDialogData) _obj );
TBool      wxPageSetupDialogData_GetEnablePaper( TSelf(wxPageSetupDialogData) _obj );
TBool      wxPageSetupDialogData_GetEnablePrinter( TSelf(wxPageSetupDialogData) _obj );
TClass(wxPoint) wxPageSetupDialogData_GetMarginBottomRight( TSelf(wxPageSetupDialogData) _obj );
TClass(wxPoint) wxPageSetupDialogData_GetMarginTopLeft( TSelf(wxPageSetupDialogData) _obj );
TClass(wxPoint) wxPageSetupDialogData_GetMinMarginBottomRight( TSelf(wxPageSetupDialogData) _obj );
TClass(wxPoint) wxPageSetupDialogData_GetMinMarginTopLeft( TSelf(wxPageSetupDialogData) _obj );
int        wxPageSetupDialogData_GetPaperId( TSelf(wxPageSetupDialogData) _obj );
TClass(wxSize) wxPageSetupDialogData_GetPaperSize( TSelf(wxPageSetupDialogData) _obj );
void       wxPageSetupDialogData_GetPrintData( TSelf(wxPageSetupDialogData) _obj, TClassRef(wxPrintData) _ref );
void       wxPageSetupDialogData_SetDefaultInfo( TSelf(wxPageSetupDialogData) _obj, TBool flag );
void       wxPageSetupDialogData_SetDefaultMinMargins( TSelf(wxPageSetupDialogData) _obj, int flag );
void       wxPageSetupDialogData_SetMarginBottomRight( TSelf(wxPageSetupDialogData) _obj, TPoint(x,y) );
void       wxPageSetupDialogData_SetMarginTopLeft( TSelf(wxPageSetupDialogData) _obj, TPoint(x,y) );
void       wxPageSetupDialogData_SetMinMarginBottomRight( TSelf(wxPageSetupDialogData) _obj, TPoint(x,y) );
void       wxPageSetupDialogData_SetMinMarginTopLeft( TSelf(wxPageSetupDialogData) _obj, TPoint(x,y) );
void       wxPageSetupDialogData_SetPaperId( TSelf(wxPageSetupDialogData) _obj, void* id );
void       wxPageSetupDialogData_SetPaperSize( TSelf(wxPageSetupDialogData) _obj, TSize(w,h) );
void       wxPageSetupDialogData_SetPaperSizeId( TSelf(wxPageSetupDialogData) _obj, int id );
void       wxPageSetupDialogData_SetPrintData( TSelf(wxPageSetupDialogData) _obj, TClass(wxPrintData) printData );

/* wxPaintDC */
TClassDefExtend(wxPaintDC,wxWindowDC)
TClass(wxPaintDC) wxPaintDC_Create( TClass(wxWindow) win );
void       wxPaintDC_Delete( TSelf(wxPaintDC) _obj );

/* wxPaintEvent */
TClassDefExtend(wxPaintEvent,wxEvent)

/* wxPalette */
TClassDefExtend(wxPalette,wxGDIObject)
void       wxPalette_Assign( TSelf(wxPalette) _obj, TClass(wxPalette) palette );
TClass(wxPalette) wxPalette_CreateDefault(  );
TClass(wxPalette) wxPalette_CreateRGB( int n, void* red, void* green, void* blue );
void       wxPalette_Delete( TSelf(wxPalette) _obj );
int        wxPalette_GetPixel( TSelf(wxPalette) _obj, TColorRGB(red,green,blue) );
TBool      wxPalette_GetRGB( TSelf(wxPalette) _obj, int pixel, void* red, void* green, void* blue );
TBool      wxPalette_IsEqual( TSelf(wxPalette) _obj, TClass(wxPalette) palette );
TBool      wxPalette_IsOk( TSelf(wxPalette) _obj );

/* wxPaletteChangedEvent */
TClassDefExtend(wxPaletteChangedEvent,wxEvent)
void       wxPaletteChangedEvent_CopyObject( TSelf(wxPaletteChangedEvent) _obj, void* obj );
void*      wxPaletteChangedEvent_GetChangedWindow( TSelf(wxPaletteChangedEvent) _obj );
void       wxPaletteChangedEvent_SetChangedWindow( TSelf(wxPaletteChangedEvent) _obj, TClass(wxWindow) win );

/* wxPanel */
TClassDefExtend(wxPanel,wxWindow)
TClass(wxPanel)  wxPanel_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );
void       wxPanel_InitDialog( TSelf(wxPanel) _obj );
void       wxPanel_SetFocus( TSelf(wxPanel) _obj);

/* wxPathList */
TClassDefExtend(wxPathList,wxList)

/* wxPen */
TClassDefExtend(wxPen,wxGDIObject)
void       wxPen_Assign( TSelf(wxPen) _obj, TClass(wxPen) pen );
TClass(wxPen) wxPen_CreateDefault(  );
TClass(wxPen) wxPen_CreateFromBitmap( TClass(wxBitmap) stipple, int width );
TClass(wxPen) wxPen_CreateFromColour( TClass(wxColour) col, int width, int style );
TClass(wxPen) wxPen_CreateFromStock( int id );
void       wxPen_Delete( TSelf(wxPen) _obj );
int        wxPen_GetCap( TSelf(wxPen) _obj );
void       wxPen_GetColour( TSelf(wxPen) _obj, TClassRef(wxColour) _ref );
int        wxPen_GetDashes( TSelf(wxPen) _obj, void* ptr );
int        wxPen_GetJoin( TSelf(wxPen) _obj );
void       wxPen_GetStipple( TSelf(wxPen) _obj, TClassRef(wxBitmap) _ref );
int        wxPen_GetStyle( TSelf(wxPen) _obj );
int        wxPen_GetWidth( TSelf(wxPen) _obj );
TBool      wxPen_IsEqual( TSelf(wxPen) _obj, TClass(wxPen) pen );
TBool      wxPen_IsOk( TSelf(wxPen) _obj );
void       wxPen_SetCap( TSelf(wxPen) _obj, int cap );
void       wxPen_SetColour( TSelf(wxPen) _obj, TClass(wxColour) col );
void       wxPen_SetColourSingle( TSelf(wxPen) _obj, TChar r, TChar g, TChar b );
void       wxPen_SetDashes( TSelf(wxPen) _obj, int nb_dashes, void* dash );
void       wxPen_SetJoin( TSelf(wxPen) _obj, int join );
void       wxPen_SetStipple( TSelf(wxPen) _obj, TClass(wxBitmap) stipple );
void       wxPen_SetStyle( TSelf(wxPen) _obj, int style );
void       wxPen_SetWidth( TSelf(wxPen) _obj, int width );

/* wxPenList */
TClassDefExtend(wxPenList,wxList)

/* wxPlotCurve */
TClassDefExtend(wxPlotCurve,wxObject)

/* wxPlotEvent */
TClassDefExtend(wxPlotEvent,wxNotifyEvent)
void*      wxPlotEvent_GetCurve( TSelf(wxPlotEvent) _obj );
int        wxPlotEvent_GetPosition( TSelf(wxPlotEvent) _obj );
double     wxPlotEvent_GetZoom( TSelf(wxPlotEvent) _obj );
void       wxPlotEvent_SetPosition( TSelf(wxPlotEvent) _obj, int pos );
void       wxPlotEvent_SetZoom( TSelf(wxPlotEvent) _obj, double zoom );

/* wxPlotOnOffCurve */
TClassDefExtend(wxPlotOnOffCurve,wxObject)
void       wxPlotOnOffCurve_Add( TSelf(wxPlotOnOffCurve) _obj, int on, int off, TClass(wxClientData) clientData );
TClass(wxPlotOnOffCurve) wxPlotOnOffCurve_Create( int offsetY );
void       wxPlotOnOffCurve_Delete( TSelf(wxPlotOnOffCurve) _obj );
void       wxPlotOnOffCurve_DrawOffLine( TSelf(wxPlotOnOffCurve) _obj, TClass(wxDC) dc, int y, int start, int end );
void       wxPlotOnOffCurve_DrawOnLine( TSelf(wxPlotOnOffCurve) _obj, TClass(wxDC) dc, int y, int start, int end, TClass(wxClientData) clientData );
void*      wxPlotOnOffCurve_GetAt( TSelf(wxPlotOnOffCurve) _obj, int index );
TClass(wxClientData) wxPlotOnOffCurve_GetClientData( TSelf(wxPlotOnOffCurve) _obj, int index );
int        wxPlotOnOffCurve_GetCount( TSelf(wxPlotOnOffCurve) _obj );
int        wxPlotOnOffCurve_GetEndX( TSelf(wxPlotOnOffCurve) _obj );
int        wxPlotOnOffCurve_GetOff( TSelf(wxPlotOnOffCurve) _obj, int index );
int        wxPlotOnOffCurve_GetOffsetY( TSelf(wxPlotOnOffCurve) _obj );
int        wxPlotOnOffCurve_GetOn( TSelf(wxPlotOnOffCurve) _obj, int index );
int        wxPlotOnOffCurve_GetStartX( TSelf(wxPlotOnOffCurve) _obj );
void       wxPlotOnOffCurve_SetOffsetY( TSelf(wxPlotOnOffCurve) _obj, int offsetY );

/* wxPlotWindow */
TClassDefExtend(wxPlotWindow,wxScrolledWindow)
void       wxPlotWindow_Add( TSelf(wxPlotWindow) _obj, TClass(wxPlotCurve) curve );
void       wxPlotWindow_AddOnOff( TSelf(wxPlotWindow) _obj, TClass(wxPlotCurve) curve );
TClass(wxPlotWindow) wxPlotWindow_Create( TClass(wxWindow) parent, int id, TRect(x,y,w,h), int flags );
void       wxPlotWindow_Delete( TSelf(wxPlotWindow) _obj, TClass(wxPlotCurve) curve );
void       wxPlotWindow_DeleteOnOff( TSelf(wxPlotWindow) _obj, TClass(wxPlotOnOffCurve) curve );
void       wxPlotWindow_Enlarge( TSelf(wxPlotWindow) _obj, TClass(wxPlotCurve) curve, double factor );
TClass(wxPlotCurve) wxPlotWindow_GetAt( TSelf(wxPlotWindow) _obj, int n );
int        wxPlotWindow_GetCount( TSelf(wxPlotWindow) _obj );
TClass(wxPlotCurve) wxPlotWindow_GetCurrent( TSelf(wxPlotWindow) _obj );
int        wxPlotWindow_GetEnlargeAroundWindowCentre( TSelf(wxPlotWindow) _obj );
TClass(wxPlotOnOffCurve)      wxPlotWindow_GetOnOffCurveAt( TSelf(wxPlotWindow) _obj, int n );
int        wxPlotWindow_GetOnOffCurveCount( TSelf(wxPlotWindow) _obj );
int        wxPlotWindow_GetScrollOnThumbRelease( TSelf(wxPlotWindow) _obj );
double     wxPlotWindow_GetUnitsPerValue( TSelf(wxPlotWindow) _obj );
double     wxPlotWindow_GetZoom( TSelf(wxPlotWindow) _obj );
void       wxPlotWindow_Move( TSelf(wxPlotWindow) _obj, TClass(wxPlotCurve) curve, int pixels_up );
void       wxPlotWindow_RedrawEverything( TSelf(wxPlotWindow) _obj );
void       wxPlotWindow_RedrawXAxis( TSelf(wxPlotWindow) _obj );
void       wxPlotWindow_RedrawYAxis( TSelf(wxPlotWindow) _obj );
void       wxPlotWindow_ResetScrollbar( TSelf(wxPlotWindow) _obj );
void       wxPlotWindow_SetCurrent( TSelf(wxPlotWindow) _obj, TClass(wxPlotCurve) current );
void       wxPlotWindow_SetEnlargeAroundWindowCentre( TSelf(wxPlotWindow) _obj, int enlargeAroundWindowCentre );
void       wxPlotWindow_SetScrollOnThumbRelease( TSelf(wxPlotWindow) _obj, int scrollOnThumbRelease );
void       wxPlotWindow_SetUnitsPerValue( TSelf(wxPlotWindow) _obj, double upv );
void       wxPlotWindow_SetZoom( TSelf(wxPlotWindow) _obj, double zoom );

/* wxPoint */
TClassDef(wxPoint)
TClass(wxPoint) wxPoint_Create( TPoint(xx,yy) );
void       wxPoint_Destroy( TSelf(wxPoint) _obj );
int        wxPoint_GetX( TSelf(wxPoint) _obj );
int        wxPoint_GetY( TSelf(wxPoint) _obj );
void       wxPoint_SetX( TSelf(wxPoint) _obj, int w );
void       wxPoint_SetY( TSelf(wxPoint) _obj, int h );

/* wxPopupTransientWindow */
TClassDefExtend(wxPopupTransientWindow,wxPopupWindow)

/* wxPopupWindow */
TClassDefExtend(wxPopupWindow,wxWindow)

/* wxPostScriptDC */
TClassDefExtend(wxPostScriptDC,wxDC)
TClass(wxPostScriptDC) wxPostScriptDC_Create( TClass(wxPrintData) data );
void       wxPostScriptDC_Delete( TSelf(wxPostScriptDC) self );
void       wxPostScriptDC_SetResolution( TSelf(wxPostScriptDC) self, int ppi );
int        wxPostScriptDC_GetResolution( TSelf(wxPostScriptDC) self );

/* wxPreviewCanvas */
TClassDefExtend(wxPreviewCanvas,wxScrolledWindow)
TClass(wxPreviewCanvas) wxPreviewCanvas_Create( TClass(wxPrintPreview) preview, TClass(wxWindow) parent, TRect(x,y,w,h), int style );

/* wxPreviewControlBar */
TClassDefExtend(wxPreviewControlBar,wxPanel)

/* wxPreviewFrame */
TClassDefExtend(wxPreviewFrame,wxFrame)

/* wxPrintData */
TClassDefExtend(wxPrintData,wxObject)
void       wxPrintData_Assign( TSelf(wxPrintData) _obj, TClass(wxPrintData) data );
TClass(wxPrintData) wxPrintData_Create(  );
void       wxPrintData_Delete( TSelf(wxPrintData) _obj );
TBool      wxPrintData_GetCollate( TSelf(wxPrintData) _obj );
TBool      wxPrintData_GetColour( TSelf(wxPrintData) _obj );
int        wxPrintData_GetDuplex( TSelf(wxPrintData) _obj );
TClass(wxString) wxPrintData_GetFilename( TSelf(wxPrintData) _obj );
TClass(wxString) wxPrintData_GetFontMetricPath( TSelf(wxPrintData) _obj );
int        wxPrintData_GetNoCopies( TSelf(wxPrintData) _obj );
int        wxPrintData_GetOrientation( TSelf(wxPrintData) _obj );
int        wxPrintData_GetPaperId( TSelf(wxPrintData) _obj );
TClass(wxSize) wxPrintData_GetPaperSize( TSelf(wxPrintData) _obj );
TClass(wxString) wxPrintData_GetPreviewCommand( TSelf(wxPrintData) _obj );
int        wxPrintData_GetPrintMode( TSelf(wxPrintData) _obj );
TClass(wxString) wxPrintData_GetPrinterCommand( TSelf(wxPrintData) _obj );
TClass(wxString) wxPrintData_GetPrinterName( TSelf(wxPrintData) _obj );
TClass(wxString) wxPrintData_GetPrinterOptions( TSelf(wxPrintData) _obj );
double     wxPrintData_GetPrinterScaleX( TSelf(wxPrintData) _obj );
double     wxPrintData_GetPrinterScaleY( TSelf(wxPrintData) _obj );
int        wxPrintData_GetPrinterTranslateX( TSelf(wxPrintData) _obj );
int        wxPrintData_GetPrinterTranslateY( TSelf(wxPrintData) _obj );
int        wxPrintData_GetQuality( TSelf(wxPrintData) _obj );
void       wxPrintData_SetCollate( TSelf(wxPrintData) _obj, TBoolInt flag );
void       wxPrintData_SetColour( TSelf(wxPrintData) _obj, TBoolInt colour );
void       wxPrintData_SetDuplex( TSelf(wxPrintData) _obj, int duplex );
void       wxPrintData_SetFilename( TSelf(wxPrintData) _obj, TClass(wxString) filename );
void       wxPrintData_SetFontMetricPath( TSelf(wxPrintData) _obj, TClass(wxString) path );
void       wxPrintData_SetNoCopies( TSelf(wxPrintData) _obj, int v );
// void       wxPrintData_SetOrientation( TSelf(wxPrintData) _obj, wxPrintOrientation orient );
void       wxPrintData_SetOrientation( TSelf(wxPrintData) _obj, int orient );
void       wxPrintData_SetPaperId( TSelf(wxPrintData) _obj, int sizeId );
void       wxPrintData_SetPaperSize( TSelf(wxPrintData) _obj, TSize(w,h) );
void       wxPrintData_SetPreviewCommand( TSelf(wxPrintData) _obj, TClass(wxCommand) command );
void       wxPrintData_SetPrintMode( TSelf(wxPrintData) _obj, int printMode );
void       wxPrintData_SetPrinterCommand( TSelf(wxPrintData) _obj, TClass(wxCommand) command );
void       wxPrintData_SetPrinterName( TSelf(wxPrintData) _obj, TClass(wxString) name );
void       wxPrintData_SetPrinterOptions( TSelf(wxPrintData) _obj, TClass(wxString) options );
void       wxPrintData_SetPrinterScaleX( TSelf(wxPrintData) _obj, double x );
void       wxPrintData_SetPrinterScaleY( TSelf(wxPrintData) _obj, double y );
void       wxPrintData_SetPrinterScaling( TSelf(wxPrintData) _obj, double x, double y );
void       wxPrintData_SetPrinterTranslateX( TSelf(wxPrintData) _obj, int x );
void       wxPrintData_SetPrinterTranslateY( TSelf(wxPrintData) _obj, int y );
void       wxPrintData_SetPrinterTranslation( TSelf(wxPrintData) _obj, TPoint(x,y) );
void       wxPrintData_SetQuality( TSelf(wxPrintData) _obj, int quality );

/* wxPostScriptPrintNativeData */
TClassDefExtend(wxPostScriptPrintNativeData,wxObject)
TClass(wxPostScriptPrintNativeData) wxPostScriptPrintNativeData_Create(  );
void       wxPostScriptPrintNativeData_Delete( TSelf(wxPostScriptPrintNativeData) _obj );

/* wxPrintDialog */
TClassDefExtend(wxPrintDialog,wxDialog)
TClass(wxPrintDialog) wxPrintDialog_Create( TClass(wxWindow) parent, TClass(wxPrintDialogData) data );
TClass(wxDC)         wxPrintDialog_GetPrintDC( TSelf(wxPrintDialog) _obj );
void       wxPrintDialog_GetPrintData( TSelf(wxPrintDialog) _obj, TClassRef(wxPrintData) _ref );
TClass(wxPrintDialogData) wxPrintDialog_GetPrintDialogData( TSelf(wxPrintDialog) _obj );

/* wxPrintDialogData */
TClassDefExtend(wxPrintDialogData,wxObject)
void       wxPrintDialogData_Assign( TSelf(wxPrintDialogData) _obj, TClass(wxPrintDialogData) data );
void       wxPrintDialogData_AssignData( TSelf(wxPrintDialogData) _obj, TClass(wxPrintData) data );
TClass(wxPrintDialogData) wxPrintDialogData_CreateDefault(  );
TClass(wxPrintDialogData) wxPrintDialogData_CreateFromData( TClass(wxPrintData) printData );
void       wxPrintDialogData_Delete( TSelf(wxPrintDialogData) _obj );
void       wxPrintDialogData_EnableHelp( TSelf(wxPrintDialogData) _obj, TBool flag );
void       wxPrintDialogData_EnablePageNumbers( TSelf(wxPrintDialogData) _obj, TBool flag );
void       wxPrintDialogData_EnablePrintToFile( TSelf(wxPrintDialogData) _obj, TBool flag );
void       wxPrintDialogData_EnableSelection( TSelf(wxPrintDialogData) _obj, TBool flag );
int        wxPrintDialogData_GetAllPages( TSelf(wxPrintDialogData) _obj );
TBool      wxPrintDialogData_GetCollate( TSelf(wxPrintDialogData) _obj );
TBool      wxPrintDialogData_GetEnableHelp( TSelf(wxPrintDialogData) _obj );
TBool      wxPrintDialogData_GetEnablePageNumbers( TSelf(wxPrintDialogData) _obj );
TBool      wxPrintDialogData_GetEnablePrintToFile( TSelf(wxPrintDialogData) _obj );
TBool      wxPrintDialogData_GetEnableSelection( TSelf(wxPrintDialogData) _obj );
int        wxPrintDialogData_GetFromPage( TSelf(wxPrintDialogData) _obj );
int        wxPrintDialogData_GetMaxPage( TSelf(wxPrintDialogData) _obj );
int        wxPrintDialogData_GetMinPage( TSelf(wxPrintDialogData) _obj );
int        wxPrintDialogData_GetNoCopies( TSelf(wxPrintDialogData) _obj );
void       wxPrintDialogData_GetPrintData( TSelf(wxPrintDialogData) _obj, TClassRef(wxPrintData) _ref );
TBool      wxPrintDialogData_GetPrintToFile( TSelf(wxPrintDialogData) _obj );
TBool      wxPrintDialogData_GetSelection( TSelf(wxPrintDialogData) _obj );
int        wxPrintDialogData_GetToPage( TSelf(wxPrintDialogData) _obj );
void       wxPrintDialogData_SetAllPages( TSelf(wxPrintDialogData) _obj, TBool flag );
void       wxPrintDialogData_SetCollate( TSelf(wxPrintDialogData) _obj, TBool flag );
void       wxPrintDialogData_SetFromPage( TSelf(wxPrintDialogData) _obj, int v );
void       wxPrintDialogData_SetMaxPage( TSelf(wxPrintDialogData) _obj, int v );
void       wxPrintDialogData_SetMinPage( TSelf(wxPrintDialogData) _obj, int v );
void       wxPrintDialogData_SetNoCopies( TSelf(wxPrintDialogData) _obj, int v );
void       wxPrintDialogData_SetPrintData( TSelf(wxPrintDialogData) _obj, TClass(wxPrintData) printData );
void       wxPrintDialogData_SetPrintToFile( TSelf(wxPrintDialogData) _obj, TBool flag );
void       wxPrintDialogData_SetSelection( TSelf(wxPrintDialogData) _obj, TBool flag );
void       wxPrintDialogData_SetToPage( TSelf(wxPrintDialogData) _obj, int v );

/* wxPrintPreview */
TClassDefExtend(wxPrintPreview,wxObject)
TClass(wxPrintPreview) wxPrintPreview_CreateFromData( TClass(wxPrintout) printout, TClass(wxPrintout) printoutForPrinting, TClass(wxPrintData) data );
TClass(wxPrintPreview) wxPrintPreview_CreateFromDialogData( TClass(wxPrintout) printout, TClass(wxPrintout) printoutForPrinting, TClass(wxPrintDialogData) data );
void       wxPrintPreview_Delete( TSelf(wxPrintPreview) _obj );
void       wxPrintPreview_DetermineScaling( TSelf(wxPrintPreview) _obj );
TBool      wxPrintPreview_DrawBlankPage( TSelf(wxPrintPreview) _obj, TClass(wxPreviewCanvas) canvas, TClass(wxDC) dc );
TClass(wxPreviewCanvas)  wxPrintPreview_GetCanvas( TSelf(wxPrintPreview) _obj );
int        wxPrintPreview_GetCurrentPage( TSelf(wxPrintPreview) _obj );
TClass(wxFrame) wxPrintPreview_GetFrame( TSelf(wxPrintPreview) _obj );
int        wxPrintPreview_GetMaxPage( TSelf(wxPrintPreview) _obj );
int        wxPrintPreview_GetMinPage( TSelf(wxPrintPreview) _obj );
void       wxPrintPreview_GetPrintDialogData( TSelf(wxPrintPreview) _obj, TClassRef(wxPrintDialogData) _ref );
TClass(wxPrintout) wxPrintPreview_GetPrintout( TSelf(wxPrintPreview) _obj );
TClass(wxPrintout) wxPrintPreview_GetPrintoutForPrinting( TSelf(wxPrintPreview) _obj );
int        wxPrintPreview_GetZoom( TSelf(wxPrintPreview) _obj );
TBool      wxPrintPreview_IsOk( TSelf(wxPrintPreview) _obj );
TBool      wxPrintPreview_PaintPage( TSelf(wxPrintPreview) _obj, TClass(wxPrintPreview) canvas, TClass(wxDC) dc );
TBool      wxPrintPreview_Print( TSelf(wxPrintPreview) _obj, TBool interactive );
TBool      wxPrintPreview_RenderPage( TSelf(wxPrintPreview) _obj, int pageNum );
void       wxPrintPreview_SetCanvas( TSelf(wxPrintPreview) _obj, TClass(wxPreviewCanvas) canvas );
TBool      wxPrintPreview_SetCurrentPage( TSelf(wxPrintPreview) _obj, int pageNum );
void       wxPrintPreview_SetFrame( TSelf(wxPrintPreview) _obj, TClass(wxFrame) frame );
void       wxPrintPreview_SetOk( TSelf(wxPrintPreview) _obj, TBool ok );
void       wxPrintPreview_SetPrintout( TSelf(wxPrintPreview) _obj, TClass(wxPrintout) printout );
void       wxPrintPreview_SetZoom( TSelf(wxPrintPreview) _obj, int percent );

/* wxPrinter */
TClassDefExtend(wxPrinter,wxObject)
TClass(wxPrinter) wxPrinter_Create( TClass(wxPrintDialogData) data );
TClass(wxWindow)  wxPrinter_CreateAbortWindow( TSelf(wxPrinter) _obj, TClass(wxWindow) parent, TClass(wxPrintout) printout );
void       wxPrinter_Delete( TSelf(wxPrinter) _obj );
TBool      wxPrinter_GetAbort( TSelf(wxPrinter) _obj );
int        wxPrinter_GetLastError( TSelf(wxPrinter) _obj );
void       wxPrinter_GetPrintDialogData( TSelf(wxPrinter) _obj, TClassRef(wxPrintDialogData) _ref );
TBool      wxPrinter_Print( TSelf(wxPrinter) _obj, TClass(wxWindow) parent, TClass(wxPrintout) printout, TBool prompt );
TClass(wxDC)  wxPrinter_PrintDialog( TSelf(wxPrinter) _obj, TClass(wxWindow) parent );
void       wxPrinter_ReportError( TSelf(wxPrinter) _obj, TClass(wxWindow) parent, TClass(wxPrintout) printout, TClass(wxString) message );
TBool      wxPrinter_Setup( TSelf(wxPrinter) _obj, TClass(wxWindow) parent );

/* wxPrinterDC */
TClassDefExtend(wxPrinterDC,wxDC)
TClass(wxPrinterDC) wxPrinterDC_Create( TClass(wxPrintData) data );
void       wxPrinterDC_Delete( TSelf(wxPrinterDC) self );
TClass(wxRect) wxPrinterDC_GetPaperRect( TSelf(wxPrinterDC) self );

/* wxPrintout */
TClassDefExtend(wxPrintout,wxObject)

/* wxPrivateDropTarget */
TClassDefExtend(wxPrivateDropTarget,wxDropTarget)

/* wxProcess */
TClassDefExtend(wxProcess,wxEvtHandler)
void       wxProcess_CloseOutput( TSelf(wxProcess) _obj );
TClass(wxProcess) wxProcess_CreateDefault( TClass(wxWindow) _prt, int _id );
TClass(wxProcess) wxProcess_CreateRedirect( TClass(wxWindow) _prt, TBool _rdr );
void       wxProcess_Delete( TSelf(wxProcess) _obj );
void       wxProcess_Detach( TSelf(wxProcess) _obj );
TClass(wxInputStream) wxProcess_GetErrorStream( TSelf(wxProcess) _obj );
TClass(wxInputStream) wxProcess_GetInputStream( TSelf(wxProcess) _obj );
TClass(wxOutputStream) wxProcess_GetOutputStream( TSelf(wxProcess) _obj );
TBool      wxProcess_IsRedirected( TSelf(wxProcess) _obj );
void       wxProcess_Redirect( TSelf(wxProcess) _obj );

/* wxProcessEvent */
TClassDefExtend(wxProcessEvent,wxEvent)
int        wxProcessEvent_GetExitCode( TSelf(wxProcessEvent) _obj );
int        wxProcessEvent_GetPid( TSelf(wxProcessEvent) _obj );

/* wxProgressDialog */
TClassDefExtend(wxProgressDialog,wxFrame)

/* wxProtocol */
TClassDefExtend(wxProtocol,wxSocketClient)

/* wxQuantize */
TClassDefExtend(wxQuantize,wxObject)

/* wxQueryCol */
TClassDefExtend(wxQueryCol,wxObject)

/* wxQueryField */
TClassDefExtend(wxQueryField,wxObject)

/* wxQueryLayoutInfoEvent */
TClassDefExtend(wxQueryLayoutInfoEvent,wxEvent)
TClass(wxQueryLayoutInfoEvent) wxQueryLayoutInfoEvent_Create( int id );
int        wxQueryLayoutInfoEvent_GetAlignment( TSelf(wxQueryLayoutInfoEvent) _obj );
int        wxQueryLayoutInfoEvent_GetFlags( TSelf(wxQueryLayoutInfoEvent) _obj );
int        wxQueryLayoutInfoEvent_GetOrientation( TSelf(wxQueryLayoutInfoEvent) _obj );
int        wxQueryLayoutInfoEvent_GetRequestedLength( TSelf(wxQueryLayoutInfoEvent) _obj );
TClass(wxSize) wxQueryLayoutInfoEvent_GetSize( TSelf(wxQueryLayoutInfoEvent) _obj );
void       wxQueryLayoutInfoEvent_SetAlignment( TSelf(wxQueryLayoutInfoEvent) _obj, int align );
void       wxQueryLayoutInfoEvent_SetFlags( TSelf(wxQueryLayoutInfoEvent) _obj, int flags );
void       wxQueryLayoutInfoEvent_SetOrientation( TSelf(wxQueryLayoutInfoEvent) _obj, int orient );
void       wxQueryLayoutInfoEvent_SetRequestedLength( TSelf(wxQueryLayoutInfoEvent) _obj, int length );
void       wxQueryLayoutInfoEvent_SetSize( TSelf(wxQueryLayoutInfoEvent) _obj, TSize(w,h) );

/* wxQueryNewPaletteEvent */
TClassDefExtend(wxQueryNewPaletteEvent,wxEvent)
void       wxQueryNewPaletteEvent_CopyObject( TSelf(wxQueryNewPaletteEvent) _obj, TClass(wxObject) obj );
TBool      wxQueryNewPaletteEvent_GetPaletteRealized( TSelf(wxQueryNewPaletteEvent) _obj );
void       wxQueryNewPaletteEvent_SetPaletteRealized( TSelf(wxQueryNewPaletteEvent) _obj, TBool realized );

/* wxRadioBox */
TClassDefExtend(wxRadioBox,wxControl)
TClass(wxRadioBox) wxRadioBox_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TRect(_lft,_top,_wdt,_hgt), TArrayString(n, _str), int _dim, int _stl );
void       wxRadioBox_EnableItem( TSelf(wxRadioBox) _obj, int item, TBool enable );
int        wxRadioBox_FindString( TSelf(wxRadioBox) _obj, TClass(wxString) s );
TClass(wxString) wxRadioBox_GetItemLabel( TSelf(wxRadioBox) _obj, int item );
int        wxRadioBox_GetNumberOfRowsOrCols( TSelf(wxRadioBox) _obj );
int        wxRadioBox_GetSelection( TSelf(wxRadioBox) _obj );
TClass(wxString) wxRadioBox_GetStringSelection( TSelf(wxRadioBox) _obj );
int        wxRadioBox_Number( TSelf(wxRadioBox) _obj );
void       wxRadioBox_SetItemBitmap( TSelf(wxRadioBox) _obj, int item, TClass(wxBitmap) bitmap );
void       wxRadioBox_SetItemLabel( TSelf(wxRadioBox) _obj, int item, TClass(wxString) label );
void       wxRadioBox_SetNumberOfRowsOrCols( TSelf(wxRadioBox) _obj, int n );
void       wxRadioBox_SetSelection( TSelf(wxRadioBox) _obj, int _n );
void       wxRadioBox_SetStringSelection( TSelf(wxRadioBox) _obj, TClass(wxString) s );
void       wxRadioBox_ShowItem( TSelf(wxRadioBox) _obj, int item, TBool show );

/* wxRadioButton */
TClassDefExtend(wxRadioButton,wxControl)
TClass(wxRadioButton) wxRadioButton_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TRect(_lft,_top,_wdt,_hgt), int _stl );
TBool      wxRadioButton_GetValue( TSelf(wxRadioButton) _obj );
void       wxRadioButton_SetValue( TSelf(wxRadioButton) _obj, TBool value );

/* wxRealPoint */
TClassDef(wxRealPoint)

/* wxRecordSet */
TClassDefExtend(wxRecordSet,wxObject)

/* wxRect */
TClassDef(wxRect)

/* wxRegEx */
TClassDef(wxRegEx)

/* wxRegion */
TClassDefExtend(wxRegion,wxGDIObject)
void       wxRegion_Assign( TSelf(wxRegion) _obj, TClass(wxRegion) region );
void       wxRegion_Clear( TSelf(wxRegion) _obj );
TBool      wxRegion_ContainsPoint( TSelf(wxRegion) _obj, TPoint(x,y) );
TBool      wxRegion_ContainsRect( TSelf(wxRegion) _obj, TRect(x,y,width,height) );
TClass(wxRegion) wxRegion_CreateDefault(  );
TClass(wxRegion) wxRegion_CreateFromRect( TRect(x,y,w,h) );
void       wxRegion_Delete( TSelf(wxRegion) _obj );
TBool      wxRegion_IsEmpty( TSelf(wxRegion) _obj );
void       wxRegion_GetBox( TSelf(wxRegion) _obj, TRectOutVoid(_x,_y,_w,_h) );
TBool      wxRegion_IntersectRect( TSelf(wxRegion) _obj, TRect(x,y,width,height) );
TBool      wxRegion_IntersectRegion( TSelf(wxRegion) _obj, TClass(wxRegion) region );
TBool      wxRegion_SubtractRect( TSelf(wxRegion) _obj, TRect(x,y,width,height) );
TBool      wxRegion_SubtractRegion( TSelf(wxRegion) _obj, TClass(wxRegion) region );
TBool      wxRegion_UnionRect( TSelf(wxRegion) _obj, TRect(x,y,width,height) );
TBool      wxRegion_UnionRegion( TSelf(wxRegion) _obj, TClass(wxRegion) region );
TBool      wxRegion_XorRect( TSelf(wxRegion) _obj, TRect(x,y,width,height) );
TBool      wxRegion_XorRegion( TSelf(wxRegion) _obj, TClass(wxRegion) region );

/* wxRegionIterator */
TClassDefExtend(wxRegionIterator,wxObject)
TClass(wxRegionIterator) wxRegionIterator_Create(  );
TClass(wxRegionIterator) wxRegionIterator_CreateFromRegion( TClass(wxRegion) region );
void       wxRegionIterator_Delete( TSelf(wxRegionIterator) _obj );
int        wxRegionIterator_GetHeight( TSelf(wxRegionIterator) _obj );
int        wxRegionIterator_GetWidth( TSelf(wxRegionIterator) _obj );
int        wxRegionIterator_GetX( TSelf(wxRegionIterator) _obj );
int        wxRegionIterator_GetY( TSelf(wxRegionIterator) _obj );
TBool      wxRegionIterator_HaveRects( TSelf(wxRegionIterator) _obj );
void       wxRegionIterator_Next( TSelf(wxRegionIterator) _obj );
void       wxRegionIterator_Reset( TSelf(wxRegionIterator) _obj );
void       wxRegionIterator_ResetToRegion( TSelf(wxRegionIterator) _obj, TClass(wxRegion) region );

/* wxRemotelyScrolledTreeCtrl */
TClassDefExtend(wxRemotelyScrolledTreeCtrl,wxTreeCtrl)
void       wxRemotelyScrolledTreeCtrl_AdjustRemoteScrollbars( TSelf(wxRemotelyScrolledTreeCtrl) _obj );
void       wxRemotelyScrolledTreeCtrl_CalcTreeSize( TSelf(wxRemotelyScrolledTreeCtrl) _obj, TRectOutVoid(_x,_y,_w,_h) );
void       wxRemotelyScrolledTreeCtrl_CalcTreeSizeItem( TSelf(wxRemotelyScrolledTreeCtrl) _obj, void* id, TRectOutVoid(_x,_y,_w,_h) );
TClass(wxRemotelyScrolledTreeCtrl) wxRemotelyScrolledTreeCtrl_Create( void* _obj, void* _cmp, TClass(wxWindow) parent, int id, TRect(x,y,w,h), int style );
void       wxRemotelyScrolledTreeCtrl_Delete( TSelf(wxRemotelyScrolledTreeCtrl) _obj );
void*      wxRemotelyScrolledTreeCtrl_GetCompanionWindow( TSelf(wxRemotelyScrolledTreeCtrl) _obj );
int        wxRemotelyScrolledTreeCtrl_GetScrollPos( TSelf(wxRemotelyScrolledTreeCtrl) _obj, int orient );
TClass(wxScrolledWindow) wxRemotelyScrolledTreeCtrl_GetScrolledWindow( TSelf(wxRemotelyScrolledTreeCtrl) _obj );
void       wxRemotelyScrolledTreeCtrl_GetViewStart( TSelf(wxRemotelyScrolledTreeCtrl) _obj, TPointOutVoid(_x,_y) );
void       wxRemotelyScrolledTreeCtrl_HideVScrollbar( TSelf(wxRemotelyScrolledTreeCtrl) _obj );
void       wxRemotelyScrolledTreeCtrl_PrepareDC( TSelf(wxRemotelyScrolledTreeCtrl) _obj, TClass(wxDC) dc );
void       wxRemotelyScrolledTreeCtrl_ScrollToLine( TSelf(wxRemotelyScrolledTreeCtrl) _obj, int posHoriz, int posVert );
void       wxRemotelyScrolledTreeCtrl_SetCompanionWindow( TSelf(wxRemotelyScrolledTreeCtrl) _obj, void* companion );
void       wxRemotelyScrolledTreeCtrl_SetScrollbars( TSelf(wxRemotelyScrolledTreeCtrl) _obj, int pixelsPerUnitX, int pixelsPerUnitY, int noUnitsX, int noUnitsY, int xPos, int yPos, int noRefresh );

/* wxSVGFileDC */
TClassDefExtend(wxSVGFileDC,wxDC)
TClass(wxSVGFileDC) wxSVGFileDC_Create( TClass(wxString) fileName );
TClass(wxSVGFileDC) wxSVGFileDC_CreateWithSize( TClass(wxString) fileName, TSize(w,h) );
TClass(wxSVGFileDC) wxSVGFileDC_CreateWithSizeAndResolution( TClass(wxString) fileName, TSize(w,h), float a_dpi );
void       wxSVGFileDC_Delete( TSelf(wxSVGFileDC) obj );

/* wxSashEvent */
TClassDefExtend(wxSashEvent,wxEvent)
TClass(wxSashEvent) wxSashEvent_Create( int id, int edge );
TClass(wxRect) wxSashEvent_GetDragRect( TSelf(wxSashEvent) _obj );
int        wxSashEvent_GetDragStatus( TSelf(wxSashEvent) _obj );
int        wxSashEvent_GetEdge( TSelf(wxSashEvent) _obj );
void       wxSashEvent_SetDragRect( TSelf(wxSashEvent) _obj, TRect(x,y,w,h) );
void       wxSashEvent_SetDragStatus( TSelf(wxSashEvent) _obj, int status );
void       wxSashEvent_SetEdge( TSelf(wxSashEvent) _obj, int edge );

/* wxSashLayoutWindow */
TClassDefExtend(wxSashLayoutWindow,wxSashWindow)
TClass(wxSashLayoutWindow) wxSashLayoutWindow_Create( TClass(wxWindow) _par, int _id, TRect(_x,_y,_w,_h), int _stl );
int        wxSashLayoutWindow_GetAlignment( TSelf(wxSashLayoutWindow) _obj );
int        wxSashLayoutWindow_GetOrientation( TSelf(wxSashLayoutWindow) _obj );
void       wxSashLayoutWindow_SetAlignment( TSelf(wxSashLayoutWindow) _obj, int align );
void       wxSashLayoutWindow_SetDefaultSize( TSelf(wxSashLayoutWindow) _obj, TSize(w,h) );
void       wxSashLayoutWindow_SetOrientation( TSelf(wxSashLayoutWindow) _obj, int orient );

/* wxSashWindow */
TClassDefExtend(wxSashWindow,wxWindow)
TClass(wxSashWindow) wxSashWindow_Create( TClass(wxWindow) _par, int _id, TRect(_x,_y,_w,_h), int _stl );
int        wxSashWindow_GetDefaultBorderSize( TSelf(wxSashWindow) _obj );
int        wxSashWindow_GetEdgeMargin( TSelf(wxSashWindow) _obj, int edge );
int        wxSashWindow_GetExtraBorderSize( TSelf(wxSashWindow) _obj );
int        wxSashWindow_GetMaximumSizeX( TSelf(wxSashWindow) _obj );
int        wxSashWindow_GetMaximumSizeY( TSelf(wxSashWindow) _obj );
int        wxSashWindow_GetMinimumSizeX( TSelf(wxSashWindow) _obj );
int        wxSashWindow_GetMinimumSizeY( TSelf(wxSashWindow) _obj );
TBool      wxSashWindow_GetSashVisible( TSelf(wxSashWindow) _obj, int edge );
TBool      wxSashWindow_HasBorder( TSelf(wxSashWindow) _obj, int edge );
void       wxSashWindow_SetDefaultBorderSize( TSelf(wxSashWindow) _obj, int width );
void       wxSashWindow_SetExtraBorderSize( TSelf(wxSashWindow) _obj, int width );
void       wxSashWindow_SetMaximumSizeX( TSelf(wxSashWindow) _obj, int max );
void       wxSashWindow_SetMaximumSizeY( TSelf(wxSashWindow) _obj, int max );
void       wxSashWindow_SetMinimumSizeX( TSelf(wxSashWindow) _obj, int min );
void       wxSashWindow_SetMinimumSizeY( TSelf(wxSashWindow) _obj, int min );
void       wxSashWindow_SetSashBorder( TSelf(wxSashWindow) _obj, int edge, TBool border );
void       wxSashWindow_SetSashVisible( TSelf(wxSashWindow) _obj, int edge, TBool sash );

/* wxScopedArray */
TClassDef(wxScopedArray)

/* wxScopedPtr */
TClassDef(wxScopedPtr)

/* wxScreenDC */
TClassDefExtend(wxScreenDC,wxDC)
TClass(wxScreenDC) wxScreenDC_Create(  );
void       wxScreenDC_Delete( TSelf(wxScreenDC) _obj );
TBool      wxScreenDC_EndDrawingOnTop( TSelf(wxScreenDC) _obj );
TBool      wxScreenDC_StartDrawingOnTop( TSelf(wxScreenDC) _obj, TRect(x,y,w,h) );
TBool      wxScreenDC_StartDrawingOnTopOfWin( TSelf(wxScreenDC) _obj, TClass(wxWindow) win );

/* wxScrollBar */
TClassDefExtend(wxScrollBar,wxControl)
TClass(wxScrollBar) wxScrollBar_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );
int        wxScrollBar_GetPageSize( TSelf(wxScrollBar) _obj );
int        wxScrollBar_GetRange( TSelf(wxScrollBar) _obj );
int        wxScrollBar_GetThumbPosition( TSelf(wxScrollBar) _obj );
int        wxScrollBar_GetThumbSize( TSelf(wxScrollBar) _obj );
void       wxScrollBar_SetScrollbar( TSelf(wxScrollBar) _obj, int position, int thumbSize, int range, int pageSize, TBool refresh );
void       wxScrollBar_SetThumbPosition( TSelf(wxScrollBar) _obj, int viewStart );

/* wxScrollEvent */
TClassDefExtend(wxScrollEvent,wxEvent)
int        wxScrollEvent_GetOrientation( TSelf(wxScrollEvent) _obj );
int        wxScrollEvent_GetPosition( TSelf(wxScrollEvent) _obj );

/* wxScrollWinEvent */
TClassDefExtend(wxScrollWinEvent,wxEvent)
int        wxScrollWinEvent_GetOrientation( TSelf(wxScrollWinEvent) _obj );
int        wxScrollWinEvent_GetPosition( TSelf(wxScrollWinEvent) _obj );
void       wxScrollWinEvent_SetOrientation( TSelf(wxScrollWinEvent) _obj, int orient );
void       wxScrollWinEvent_SetPosition( TSelf(wxScrollWinEvent) _obj, int pos );

/* wxScrolledWindow */
TClassDefExtend(wxScrolledWindow,wxPanel)
void       wxScrolledWindow_AdjustScrollbars( TSelf(wxScrolledWindow) _obj );
void       wxScrolledWindow_CalcScrolledPosition( TSelf(wxScrolledWindow) _obj, TPoint(x,y), TPointOutVoid(xx,yy) );
void       wxScrolledWindow_CalcUnscrolledPosition( TSelf(wxScrolledWindow) _obj, TPoint(x,y), TPointOutVoid(xx,yy) );
TClass(wxScrolledWindow) wxScrolledWindow_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );
void       wxScrolledWindow_EnableScrolling( TSelf(wxScrolledWindow) _obj, TBool x_scrolling, TBool y_scrolling );
double     wxScrolledWindow_GetScaleX( TSelf(wxScrolledWindow) _obj );
double     wxScrolledWindow_GetScaleY( TSelf(wxScrolledWindow) _obj );
int        wxScrolledWindow_GetScrollPageSize( TSelf(wxScrolledWindow) _obj, int orient );
void       wxScrolledWindow_GetScrollPixelsPerUnit( TSelf(wxScrolledWindow) _obj, TPointOutVoid(_x,_y) );
TClass(wxWindow) wxScrolledWindow_GetTargetWindow( TSelf(wxScrolledWindow) _obj );
void       wxScrolledWindow_GetViewStart( TSelf(wxScrolledWindow) _obj, TPointOutVoid(_x,_y) );
void       wxScrolledWindow_GetVirtualSize( TSelf(wxScrolledWindow) _obj, TSizeOutVoid(_x,_y) );
void       wxScrolledWindow_OnDraw( TSelf(wxScrolledWindow) _obj, TClass(wxDC) dc );
void       wxScrolledWindow_PrepareDC( TSelf(wxScrolledWindow) _obj, TClass(wxDC) dc );
void       wxScrolledWindow_Scroll( TSelf(wxScrolledWindow) _obj, TPoint(x_pos,y_pos) );
void       wxScrolledWindow_SetScale( TSelf(wxScrolledWindow) _obj, double xs, double ys );
void       wxScrolledWindow_SetScrollPageSize( TSelf(wxScrolledWindow) _obj, int orient, int pageSize );
void       wxScrolledWindow_SetScrollbars( TSelf(wxScrolledWindow) _obj, int pixelsPerUnitX, int pixelsPerUnitY, int noUnitsX, int noUnitsY, int xPos, int yPos, TBool noRefresh );
void       wxScrolledWindow_ShowScrollbars( TSelf(wxScrolledWindow) _obj, int showh, int showv );
void       wxScrolledWindow_SetTargetWindow( TSelf(wxScrolledWindow) _obj, TClass(wxWindow) target );
void       wxScrolledWindow_ViewStart( TSelf(wxScrolledWindow) _obj, TPointOutVoid(_x,_y) );

/* wxSemaphore */
TClassDef(wxSemaphore)

/* wxServer */
TClassDefExtend(wxServer,wxServerBase)

/* wxServerBase */
TClassDefExtend(wxServerBase,wxObject)

/* wxSetCursorEvent */
TClassDefExtend(wxSetCursorEvent,wxEvent)
TClass(wxCursor) wxSetCursorEvent_GetCursor( TSelf(wxSetCursorEvent) _obj );
int        wxSetCursorEvent_GetX( TSelf(wxSetCursorEvent) _obj );
int        wxSetCursorEvent_GetY( TSelf(wxSetCursorEvent) _obj );
TBool      wxSetCursorEvent_HasCursor( TSelf(wxSetCursorEvent) _obj );
void       wxSetCursorEvent_SetCursor( TSelf(wxSetCursorEvent) _obj, TClass(wxCursor) cursor );

/* wxShowEvent */
TClassDefExtend(wxShowEvent,wxEvent)
void       wxShowEvent_CopyObject( TSelf(wxShowEvent) _obj, TClass(wxObject) obj );
TBool      wxShowEvent_IsShown( TSelf(wxShowEvent) _obj );
void       wxShowEvent_SetShow( TSelf(wxShowEvent) _obj, TBool show );

/* wxSimpleHelpProvider */
TClassDefExtend(wxSimpleHelpProvider,wxHelpProvider)
TClass(wxSimpleHelpProvider) wxSimpleHelpProvider_Create(  );

/* wxSingleChoiceDialog */
TClassDefExtend(wxSingleChoiceDialog,wxDialog)

/* wxSingleInstanceChecker */
TClassDef(wxSingleInstanceChecker)
TBool      wxSingleInstanceChecker_Create( void* _obj, TClass(wxString) name, TClass(wxString) path );
TClass(wxSingleInstanceChecker) wxSingleInstanceChecker_CreateDefault(  );
void       wxSingleInstanceChecker_Delete( TSelf(wxSingleInstanceChecker) _obj );
TBool      wxSingleInstanceChecker_IsAnotherRunning( TSelf(wxSingleInstanceChecker) _obj );

/* wxSize */
TClassDef(wxSize)
TClass(wxSize) wxSize_Create( TSize(w,h) );
void       wxSize_Delete( TSelf(wxSize) _obj );
int        wxSize_GetHeight( TSelf(wxSize) _obj );
int        wxSize_GetWidth( TSelf(wxSize) _obj );
void       wxSize_SetHeight( TSelf(wxSize) _obj, int h );
void       wxSize_SetWidth( TSelf(wxSize) _obj, int w );

/* wxSizeEvent */
TClassDefExtend(wxSizeEvent,wxEvent)
void       wxSizeEvent_CopyObject( TSelf(wxSizeEvent) _obj, void* obj );
TClass(wxSize) wxSizeEvent_GetSize( TSelf(wxSizeEvent) _obj );

/* wxSizer */
TClassDefExtend(wxSizer,wxObject)
void       wxSizer_Add( TSelf(wxSizer) _obj, TSize(width,height), int option, int flag, int border, void* userData );
void       wxSizer_AddSizer( TSelf(wxSizer) _obj, TClass(wxSizer) sizer, int option, int flag, int border, void* userData );
void       wxSizer_AddWindow( TSelf(wxSizer) _obj, TClass(wxWindow) window, int option, int flag, int border, void* userData );
TClass(wxSize) wxSizer_CalcMin( TSelf(wxSizer) _obj );
void       wxSizer_Fit( TSelf(wxSizer) _obj, TClass(wxWindow) window );
int        wxSizer_GetChildren( TSelf(wxSizer) _obj, void* _res, int _cnt );
TClass(wxSize) wxSizer_GetMinSize( TSelf(wxSizer) _obj );

TClass(wxPoint) wxSizer_GetPosition( TSelf(wxSizer) _obj );

TClass(wxSize) wxSizer_GetSize( TSelf(wxSizer) _obj );
void       wxSizer_Insert( TSelf(wxSizer) _obj, int before, TSize(width,height), int option, int flag, int border, void* userData );
void       wxSizer_InsertSizer( TSelf(wxSizer) _obj, int before, TClass(wxSizer) sizer, int option, int flag, int border, void* userData );
void       wxSizer_InsertWindow( TSelf(wxSizer) _obj, int before, TClass(wxWindow) window, int option, int flag, int border, void* userData );
void       wxSizer_Layout( TSelf(wxSizer) _obj );
void       wxSizer_Prepend( TSelf(wxSizer) _obj, TSize(width,height), int option, int flag, int border, void* userData );
void       wxSizer_PrependSizer( TSelf(wxSizer) _obj, TClass(wxSizer) sizer, int option, int flag, int border, void* userData );
void       wxSizer_PrependWindow( TSelf(wxSizer) _obj, TClass(wxWindow) window, int option, int flag, int border, void* userData );
void       wxSizer_RecalcSizes( TSelf(wxSizer) _obj );
void       wxSizer_SetDimension( TSelf(wxSizer) _obj, TRect(x,y,width,height) );
void       wxSizer_SetItemMinSize( TSelf(wxSizer) _obj, int pos, TSize(width,height) );
void       wxSizer_SetItemMinSizeSizer( TSelf(wxSizer) _obj, TClass(wxSizer) sizer, TSize(width,height) );
void       wxSizer_SetItemMinSizeWindow( TSelf(wxSizer) _obj, TClass(wxWindow) window, TSize(width,height) );
void       wxSizer_SetMinSize( TSelf(wxSizer) _obj, TSize(width,height) );
void       wxSizer_SetSizeHints( TSelf(wxSizer) _obj, TClass(wxWindow) window );
void       wxSizer_AddSpacer( TSelf(wxSizer) _obj, int size );
void       wxSizer_AddStretchSpacer( TSelf(wxSizer) _obj, int size );
void       wxSizer_Clear( TSelf(wxSizer) _obj, TBool delete_windows );
TBool      wxSizer_DetachWindow( TSelf(wxSizer) _obj, TClass(wxWindow) window );
TBool      wxSizer_DetachSizer( TSelf(wxSizer) _obj, TClass(wxSizer) sizer );
TBool      wxSizer_Detach( TSelf(wxSizer) _obj, int index );
void       wxSizer_FitInside( TSelf(wxSizer) _obj, TClass(wxWindow) window );
TClass(wxWindow)    wxSizer_GetContainingWindow( TSelf(wxSizer) _obj );
TClass(wxSizerItem) wxSizer_GetItemWindow( TSelf(wxSizer) _obj, TClass(wxWindow) window, TBool recursive );
TClass(wxSizerItem) wxSizer_GetItemSizer( TSelf(wxSizer) _obj, TClass(wxSizer) window, TBool recursive );
TClass(wxSizerItem) wxSizer_GetItem( TSelf(wxSizer) _obj, int index );
TBool      wxSizer_HideWindow( TSelf(wxWindow) _obj, TClass(wxWindow) window );
TBool      wxSizer_HideSizer( TSelf(wxWindow) _obj, TClass(wxSizer) sizer );
TBool      wxSizer_Hide( TSelf(wxWindow) _obj, int index );
TClass(wxSizerItem) wxSizer_InsertSpacer( TSelf(wxSizer) _obj, int index, int size );
TClass(wxSizerItem) wxSizer_InsertStretchSpacer( TSelf(wxSizer) _obj, int index, int prop );
TBool      wxSizer_IsShownWindow( TSelf(wxSizer) _obj, TClass(wxWindow) *window );
TBool      wxSizer_IsShownSizer( TSelf(wxSizer) _obj, TClass(wxSizer) *sizer );
TBool      wxSizer_IsShown( TSelf(wxSizer) _obj, int index );
TClass(wxSizerItem) wxSizer_PrependSpacer( TSelf(wxSizer) _obj, int size );
TClass(wxSizerItem) wxSizer_PrependStretchSpacer( TSelf(wxSizer) _obj, int prop );
TBool      wxSizer_ReplaceWindow( TSelf(wxSizer) _obj, TClass(wxWindow) oldwin, TClass(wxWindow) newwin, TBool recursive );
TBool      wxSizer_ReplaceSizer( TSelf(wxSizer) _obj, TClass(wxSizer) oldsz, TClass(wxSizer) newsz, TBool recursive );
TBool      wxSizer_Replace( TSelf(wxSizer) _obj, int oldindex, TClass(wxSizerItem) newitem );
// The following is replaced with wxSizer_FitInside
// void       wxSizer_SetVirtualSizeHints( TSelf(wxSizer) _obj, TClass(wxWindow) window );
TBool      wxSizer_ShowWindow( TSelf(wxSizer) _obj, TClass(wxWindow) window, TBool show, TBool recursive );
TBool      wxSizer_ShowSizer( TSelf(wxSizer) _obj, TClass(wxSizer) sizer, TBool show, TBool recursive );
TBool      wxSizer_Show( TSelf(wxSizer) _obj, TClass(wxSizer) sizer, int index, TBool show );
/* wxSizerItem */
TClassDefExtend(wxSizerItem,wxObject)
TClass(wxSize) wxSizerItem_CalcMin( TSelf(wxSizerItem) _obj );
TClass(wxSizerItem) wxSizerItem_Create( TSize(width,height), int option, int flag, int border, void* userData );
void*      wxSizerItem_CreateInSizer( TClass(wxSizer) sizer, int option, int flag, int border, void* userData );
void*      wxSizerItem_CreateInWindow( TClass(wxWindow) window, int option, int flag, int border, void* userData );
int        wxSizerItem_GetBorder( TSelf(wxSizerItem) _obj );
int        wxSizerItem_GetFlag( TSelf(wxSizerItem) _obj );
TClass(wxSize) wxSizerItem_GetMinSize( TSelf(wxSizerItem) _obj );
TClass(wxPoint) wxSizerItem_GetPosition( TSelf(wxSizerItem) _obj );
float      wxSizerItem_GetRatio( TSelf(wxSizerItem) _obj );
TClass(wxSize) wxSizerItem_GetSize( TSelf(wxSizerItem) _obj );
TClass(wxSizer) wxSizerItem_GetSizer( TSelf(wxSizerItem) _obj );
void*      wxSizerItem_GetUserData( TSelf(wxSizerItem) _obj );
TClass(wxWindow) wxSizerItem_GetWindow( TSelf(wxSizerItem) _obj );
TBool      wxSizerItem_IsSizer( TSelf(wxSizerItem) _obj );
TBool      wxSizerItem_IsSpacer( TSelf(wxSizerItem) _obj );
TBool      wxSizerItem_IsWindow( TSelf(wxSizerItem) _obj );
void       wxSizerItem_SetBorder( TSelf(wxSizerItem) _obj, int border );
void       wxSizerItem_SetDimension( TSelf(wxSizerItem) _obj, TRect(_x,_y,_w,_h) );
void       wxSizerItem_SetFlag( TSelf(wxSizerItem) _obj, int flag );
void       wxSizerItem_SetFloatRatio( TSelf(wxSizerItem) _obj, float ratio );
void       wxSizerItem_SetInitSize( TSelf(wxSizerItem) _obj, TPoint(x,y) );
void       wxSizerItem_SetRatio( TSelf(wxSizerItem) _obj, TSize(width,height) );
// void       wxSizerItem_SetSizer( TSelf(wxSizerItem) _obj, TClass(wxSizer) sizer );
void       wxSizerItem_AssignSizer( TSelf(wxSizerItem) _obj, TClass(wxSizer) sizer );
// void       wxSizerItem_SetWindow( TSelf(wxSizerItem) _obj, TClass(wxWindow) window );
void       wxSizerItem_AssignWindow( TSelf(wxSizerItem) _obj, TClass(wxWindow) window );
void       wxSizerItem_Delete( TSelf(wxSizerItem) _obj );
void       wxSizerItem_DeleteWindows( TSelf(wxSizerItem) _obj );
void       wxSizerItem_DetachSizer( TSelf(wxSizerItem) _obj );
int        wxSizerItem_GetProportion( TSelf(wxSizerItem) _obj );
TClass(wxRect) wxSizerItem_GetRect( TSelf(wxSizerItem) _obj );
TClass(wxSize) wxSizerItem_GetSpacer( TSelf(wxSizerItem) _obj );
int        wxSizerItem_IsShown( TSelf(wxSizerItem) _obj );
void       wxSizerItem_SetProportion( TSelf(wxSizerItem) _obj, int proportion );
// void       wxSizerItem_SetSpacer( TSelf(wxSizerItem) _obj, TSize(width,height) );
void       wxSizerItem_AssignSpacer( TSelf(wxSizerItem) _obj, TSize(width,height) );
void       wxSizerItem_Show( TSelf(wxSizerItem) _obj, int show );


/* wxSlider */
TClassDefExtend(wxSlider,wxControl)
void       wxSlider_ClearSel( TSelf(wxSlider) _obj );
void       wxSlider_ClearTicks( TSelf(wxSlider) _obj );
TClass(wxSlider) wxSlider_Create( TClass(wxWindow) _prt, int _id, int _init, int _min, int _max, TRect(_lft,_top,_wdt,_hgt), long _stl );
int        wxSlider_GetLineSize( TSelf(wxSlider) _obj );
int        wxSlider_GetMax( TSelf(wxSlider) _obj );
int        wxSlider_GetMin( TSelf(wxSlider) _obj );
int        wxSlider_GetPageSize( TSelf(wxSlider) _obj );
int        wxSlider_GetSelEnd( TSelf(wxSlider) _obj );
int        wxSlider_GetSelStart( TSelf(wxSlider) _obj );
int        wxSlider_GetThumbLength( TSelf(wxSlider) _obj );
int        wxSlider_GetTickFreq( TSelf(wxSlider) _obj );
int        wxSlider_GetValue( TSelf(wxSlider) _obj );
void       wxSlider_SetLineSize( TSelf(wxSlider) _obj, int lineSize );
void       wxSlider_SetPageSize( TSelf(wxSlider) _obj, int pageSize );
void       wxSlider_SetRange( TSelf(wxSlider) _obj, int minValue, int maxValue );
void       wxSlider_SetSelection( TSelf(wxSlider) _obj, int minPos, int maxPos );
void       wxSlider_SetThumbLength( TSelf(wxSlider) _obj, int len );
void       wxSlider_SetTick( TSelf(wxSlider) _obj, int tickPos );
// Obsolete
// void       wxSlider_SetTickFreq( TSelf(wxSlider) _obj, int n, int pos );
void       wxSlider_SetValue( TSelf(wxSlider) _obj, int value );

/* wxSockAddress */
TClassDefExtend(wxSockAddress,wxObject)

/* wxSocketBase */
TClassDefExtend(wxSocketBase,wxObject)

/* wxSocketClient */
TClassDefExtend(wxSocketClient,wxSocketBase)

/* wxSocketEvent */
TClassDefExtend(wxSocketEvent,wxEvent)

/* wxSocketInputStream */
TClassDefExtend(wxSocketInputStream,wxInputStream)

/* wxSocketOutputStream */
TClassDefExtend(wxSocketOutputStream,wxOutputStream)

/* wxSocketServer */
TClassDefExtend(wxSocketServer,wxSocketBase)

/* wxSpinButton */
TClassDefExtend(wxSpinButton,wxControl)
TClass(wxSpinButton) wxSpinButton_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), long _stl );
int        wxSpinButton_GetMax( TSelf(wxSpinButton) _obj );
int        wxSpinButton_GetMin( TSelf(wxSpinButton) _obj );
int        wxSpinButton_GetValue( TSelf(wxSpinButton) _obj );
void       wxSpinButton_SetRange( TSelf(wxSpinButton) _obj, int minVal, int maxVal );
void       wxSpinButton_SetValue( TSelf(wxSpinButton) _obj, int val );

/* wxSpinCtrl */
TClassDefExtend(wxSpinCtrl,wxControl)
TClass(wxSpinCtrl) wxSpinCtrl_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TRect(_lft,_top,_wdt,_hgt), long _stl, int _min, int _max, int _init );
int        wxSpinCtrl_GetMax( TSelf(wxSpinCtrl) _obj );
int        wxSpinCtrl_GetMin( TSelf(wxSpinCtrl) _obj );
int        wxSpinCtrl_GetValue( TSelf(wxSpinCtrl) _obj );
void       wxSpinCtrl_SetRange( TSelf(wxSpinCtrl) _obj, int min_val, int max_val );
void       wxSpinCtrl_SetValue( TSelf(wxSpinCtrl) _obj, int val );

/* wxSpinEvent */
TClassDefExtend(wxSpinEvent,wxNotifyEvent)
int        wxSpinEvent_GetPosition( TSelf(wxSpinEvent) _obj );
void       wxSpinEvent_SetPosition( TSelf(wxSpinEvent) _obj, int pos );

/* wxSplashScreen */
TClassDefExtend(wxSplashScreen,wxFrame)
TClass(wxSplashScreen) wxSplashScreen_Create( TClass(wxBitmap) _bmp, long _sstl, int _ms, TClass(wxWindow) parent, int id, TRect(_lft,_top,_wdt,_hgt), long _stl);

long       wxSplashScreen_GetSplashStyle( TSelf(wxSplashScreen) _obj );
int        wxSplashScreen_GetTimeout( TSelf(wxSplashScreen) _obj );

/* wxSplitterEvent */
TClassDefExtend(wxSplitterEvent,wxNotifyEvent)

/* wxSplitterScrolledWindow */
TClassDefExtend(wxSplitterScrolledWindow,wxScrolledWindow)
TClass(wxSplitterScrolledWindow) wxSplitterScrolledWindow_Create( TClass(wxWindow) parent, int id, TRect(x,y,w,h), int style );

/* wxSplitterWindow */
TClassDefExtend(wxSplitterWindow,wxWindow)
TClass(wxSplitterWindow) wxSplitterWindow_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );
int        wxSplitterWindow_GetBorderSize( TSelf(wxSplitterWindow) _obj );
int        wxSplitterWindow_GetMinimumPaneSize( TSelf(wxSplitterWindow) _obj );
int        wxSplitterWindow_GetSashPosition( TSelf(wxSplitterWindow) _obj );
int        wxSplitterWindow_GetSashSize( TSelf(wxSplitterWindow) _obj );
int        wxSplitterWindow_GetSplitMode( TSelf(wxSplitterWindow) _obj );
TClass(wxWindow) wxSplitterWindow_GetWindow1( TSelf(wxSplitterWindow) _obj );
TClass(wxWindow) wxSplitterWindow_GetWindow2( TSelf(wxSplitterWindow) _obj );
void       wxSplitterWindow_Initialize( TSelf(wxSplitterWindow) _obj, TClass(wxWindow) window );
TBool      wxSplitterWindow_IsSplit( TSelf(wxSplitterWindow) _obj );
TBool      wxSplitterWindow_ReplaceWindow( TSelf(wxSplitterWindow) _obj, TClass(wxWindow) winOld, TClass(wxWindow) winNew );
void       wxSplitterWindow_SetBorderSize( TSelf(wxSplitterWindow) _obj, int width );
void       wxSplitterWindow_SetMinimumPaneSize( TSelf(wxSplitterWindow) _obj, int min );
void       wxSplitterWindow_SetSashPosition( TSelf(wxSplitterWindow) _obj, int position, TBool redraw );
// Obsolete
// void       wxSplitterWindow_SetSashSize( TSelf(wxSplitterWindow) _obj, int width );
void       wxSplitterWindow_SetSplitMode( TSelf(wxSplitterWindow) _obj, int mode );
TBool      wxSplitterWindow_SplitHorizontally( TSelf(wxSplitterWindow) _obj, TClass(wxWindow) window1, TClass(wxWindow) window2, int sashPosition );
TBool      wxSplitterWindow_SplitVertically( TSelf(wxSplitterWindow) _obj, TClass(wxWindow) window1, TClass(wxWindow) window2, int sashPosition );
TBool      wxSplitterWindow_Unsplit( TSelf(wxSplitterWindow) _obj, TClass(wxWindow) toRemove );
double     wxSplitterWindow_GetSashGravity( TSelf(wxSplitterWindow) _obj );
void       wxSplitterWindow_SetSashGravity( TSelf(wxSplitterWindow) _obj, double gravity );

/* wxStaticBitmap */
TClassDefExtend(wxStaticBitmap,wxControl)
TClass(wxStaticBitmap) wxStaticBitmap_Create( TClass(wxWindow) _prt, int _id, TClass(wxBitmap) bitmap, TRect(_lft,_top,_wdt,_hgt), int _stl );
void       wxStaticBitmap_Delete( TSelf(wxStaticBitmap) _obj );
void       wxStaticBitmap_GetBitmap( TSelf(wxStaticBitmap) _obj, TClassRef(wxBitmap) _ref );
void       wxStaticBitmap_GetIcon( TSelf(wxStaticBitmap) _obj, TClassRef(wxIcon) _ref );
void       wxStaticBitmap_SetBitmap( TSelf(wxStaticBitmap) _obj, TClass(wxBitmap) bitmap );
void       wxStaticBitmap_SetIcon( TSelf(wxStaticBitmap) _obj, TClass(wxIcon) icon );

/* wxStaticBox */
TClassDefExtend(wxStaticBox,wxControl)
TClass(wxStaticBox) wxStaticBox_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TRect(_lft,_top,_wdt,_hgt), int _stl );

/* wxStaticBoxSizer */
TClassDefExtend(wxStaticBoxSizer,wxBoxSizer)
TClass(wxSize) wxStaticBoxSizer_CalcMin( TSelf(wxStaticBoxSizer) _obj );
TClass(wxStaticBoxSizer) wxStaticBoxSizer_Create( TClass(wxStaticBox) box, int orient );
TClass(wxStaticBox) wxStaticBoxSizer_GetStaticBox( TSelf(wxStaticBoxSizer) _obj );
void       wxStaticBoxSizer_RecalcSizes( TSelf(wxStaticBoxSizer) _obj );

/* wxStaticLine */
TClassDefExtend(wxStaticLine,wxControl)
TClass(wxStaticLine) wxStaticLine_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );
int        wxStaticLine_GetDefaultSize( TSelf(wxStaticLine) _obj );
TBool      wxStaticLine_IsVertical( TSelf(wxStaticLine) _obj );

/* wxStaticText */
TClassDefExtend(wxStaticText,wxControl)
TClass(wxStaticText) wxStaticText_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TRect(_lft,_top,_wdt,_hgt), int _stl );

/* wxStatusBar */
TClassDefExtend(wxStatusBar,wxWindow)
TClass(wxStatusBar) wxStatusBar_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );
int        wxStatusBar_GetBorderX( TSelf(wxStatusBar) _obj );
int        wxStatusBar_GetBorderY( TSelf(wxStatusBar) _obj );
int        wxStatusBar_GetFieldsCount( TSelf(wxStatusBar) _obj );
TClass(wxString) wxStatusBar_GetStatusText( TSelf(wxStatusBar) _obj, int number );
void       wxStatusBar_SetFieldsCount( TSelf(wxStatusBar) _obj, int number, int* widths );
void       wxStatusBar_SetMinHeight( TSelf(wxStatusBar) _obj, int height );
void       wxStatusBar_SetStatusText( TSelf(wxStatusBar) _obj, TClass(wxString) text, int number );
void       wxStatusBar_SetStatusWidths( TSelf(wxStatusBar) _obj, int n, int* widths );

/* wxStopWatch */
TClassDef(wxStopWatch)
TClass(wxStopWatch) wxStopWatch_Create();
void      wxStopWatch_Delete(TSelf(wxStopWatch) _obj);
void      wxStopWatch_Start(TSelf(wxStopWatch) _obj, int msec);
void      wxStopWatch_Pause(TSelf(wxStopWatch) _obj);
void      wxStopWatch_Resume(TSelf(wxStopWatch) _obj);
int       wxStopWatch_Time(TSelf(wxStopWatch) _obj);


/* wxStreamBase */
TClassDef(wxStreamBase)
int        wxStreamBase_GetLastError( TSelf(wxStreamBase) _obj );
int        wxStreamBase_GetSize( TSelf(wxStreamBase) _obj );
TBool      wxStreamBase_IsOk( TSelf(wxStreamBase) _obj );

/* wxStreamBuffer */
TClassDef(wxStreamBuffer)

/* wxStreamToTextRedirector */
TClassDef(wxStreamToTextRedirector)

/* wxString */
TClassDef(wxString)

/* wxStringBuffer */
TClassDef(wxStringBuffer)

/* wxStringClientData */
TClassDefExtend(wxStringClientData,wxClientData)

/* wxStringList */
TClassDefExtend(wxStringList,wxList)

/* wxStringTokenizer */
TClassDefExtend(wxStringTokenizer,wxObject)

/* wxSysColourChangedEvent */
TClassDefExtend(wxSysColourChangedEvent,wxEvent)

/* wxSystemOptions */
TClassDefExtend(wxSystemOptions,wxObject)

/* wxSystemSettings */
TClassDefExtend(wxSystemSettings,wxObject)
void       wxSystemSettings_GetColour( int index, TClassRef(wxColour) _ref );
void       wxSystemSettings_GetFont( int index, TClassRef(wxFont) _ref );
int        wxSystemSettings_GetMetric( int index );
int        wxSystemSettings_GetScreenType( );

/* wxTabCtrl */
TClassDefExtend(wxTabCtrl,wxControl)

/* wxTabEvent */
TClassDefExtend(wxTabEvent,wxCommandEvent)

/* wxTablesInUse */
TClassDefExtend(wxTablesInUse,wxObject)

/* wxTaskBarIcon */
TClassDefExtend(wxTaskBarIcon,wxEvtHandler)
TClass(wxTaskBarIcon) wxTaskBarIcon_Create();
void       wxTaskBarIcon_Delete( TSelf(wxTaskBarIcon) _obj );
/* TClass(wxMenu)  wxTaskBarIcon_CreatePopupMenu( TSelf(wxTaskBarIcon) _obj ); */
TBool      wxTaskBarIcon_IsIconInstalled( TSelf(wxTaskBarIcon) _obj );
TBool      wxTaskBarIcon_IsOk( TSelf(wxTaskBarIcon) _obj );
TBool      wxTaskBarIcon_PopupMenu( TSelf(wxTaskBarIcon) _obj, TClass(wxMenu) menu );
TBool      wxTaskBarIcon_RemoveIcon( TSelf(wxTaskBarIcon) _obj );
TBool      wxTaskBarIcon_SetIcon( TSelf(wxTaskBarIcon) _obj, TClass(wxIcon) icon, TClass(wxString) text );

/* wxTempFile */
TClassDef(wxTempFile)

/* wxTextAttr */
TClassDef(wxTextAttr)

/* wxTextCtrl */
TClassDefExtend(wxTextCtrl,wxControl)
void       wxTextCtrl_AppendText( TSelf(wxTextCtrl) _obj, TClass(wxString) text );
TBool      wxTextCtrl_CanCopy( TSelf(wxTextCtrl) _obj );
TBool      wxTextCtrl_CanCut( TSelf(wxTextCtrl) _obj );
TBool      wxTextCtrl_CanPaste( TSelf(wxTextCtrl) _obj );
TBool      wxTextCtrl_CanRedo( TSelf(wxTextCtrl) _obj );
TBool      wxTextCtrl_CanUndo( TSelf(wxTextCtrl) _obj );
void       wxTextCtrl_ChangeValue( TSelf(wxTextCtrl) _obj, TClass(wxString) text );
void       wxTextCtrl_Clear( TSelf(wxTextCtrl) _obj );
void       wxTextCtrl_Copy( TSelf(wxTextCtrl) _obj );
TClass(wxTextCtrl) wxTextCtrl_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TRect(_lft,_top,_wdt,_hgt), long _stl );
void       wxTextCtrl_Cut( TSelf(wxTextCtrl) _obj );
void       wxTextCtrl_DiscardEdits( TSelf(wxTextCtrl) _obj );
long       wxTextCtrl_GetInsertionPoint( TSelf(wxTextCtrl) _obj );
long       wxTextCtrl_GetLastPosition( TSelf(wxTextCtrl) _obj );
int        wxTextCtrl_GetLineLength( TSelf(wxTextCtrl) _obj, long lineNo );
TClass(wxString) wxTextCtrl_GetLineText( TSelf(wxTextCtrl) _obj, long lineNo );
int        wxTextCtrl_GetNumberOfLines( TSelf(wxTextCtrl) _obj );
void       wxTextCtrl_GetSelection( TSelf(wxTextCtrl) _obj, void* from, void* to );
TClass(wxString) wxTextCtrl_GetValue( TSelf(wxTextCtrl) _obj );
TBool      wxTextCtrl_IsEditable( TSelf(wxTextCtrl) _obj );
TBool      wxTextCtrl_IsModified( TSelf(wxTextCtrl) _obj );
TBool      wxTextCtrl_LoadFile( TSelf(wxTextCtrl) _obj, TClass(wxString) file );
void       wxTextCtrl_Paste( TSelf(wxTextCtrl) _obj );
int        wxTextCtrl_PositionToXY( TSelf(wxTextCtrl) _obj, long pos, long* x, long* y );
void       wxTextCtrl_Redo( TSelf(wxTextCtrl) _obj );
void       wxTextCtrl_Remove( TSelf(wxTextCtrl) _obj, long from, long to );
void       wxTextCtrl_Replace( TSelf(wxTextCtrl) _obj, long from, long to, TClass(wxString) value );
TBool      wxTextCtrl_SaveFile( TSelf(wxTextCtrl) _obj, TClass(wxString) file );
void       wxTextCtrl_SetEditable( TSelf(wxTextCtrl) _obj, TBool editable );
void       wxTextCtrl_SetInsertionPoint( TSelf(wxTextCtrl) _obj, long pos );
void       wxTextCtrl_SetInsertionPointEnd( TSelf(wxTextCtrl) _obj );
void       wxTextCtrl_SetSelection( TSelf(wxTextCtrl) _obj, long from, long to );
void       wxTextCtrl_SetValue( TSelf(wxTextCtrl) _obj, TClass(wxString) value );
void       wxTextCtrl_ShowPosition( TSelf(wxTextCtrl) _obj, long pos );
void       wxTextCtrl_Undo( TSelf(wxTextCtrl) _obj );
void       wxTextCtrl_WriteText( TSelf(wxTextCtrl) _obj, TClass(wxString) text );
long       wxTextCtrl_XYToPosition( TSelf(wxTextCtrl) _obj, TPointLong(x,y) );

/* wxTextDataObject */
TClassDefExtend(wxTextDataObject,wxDataObjectSimple)
TClass(TextDataObject) TextDataObject_Create( TClass(wxString) _txt );
void       TextDataObject_Delete( TSelf(TextDataObject) _obj );
size_t TextDataObject_GetTextLength( TSelf(TextDataObject) _obj );
TClass(wxString) TextDataObject_GetText( TSelf(TextDataObject) _obj );
void       TextDataObject_SetText( TSelf(TextDataObject) _obj, TClass(wxString) text );

/* wxTextDropTarget */
TClassDefExtend(wxTextDropTarget,wxDropTarget)

/* wxTextEntryDialog */
TClassDefExtend(wxTextEntryDialog,wxDialog)

/* wxTextFile */
TClassDef(wxTextFile)

/* wxTextInputStream */
TClassDef(wxTextInputStream)

/* wxTextOutputStream */
TClassDef(wxTextOutputStream)

/* wxTextValidator */
TClassDefExtend(wxTextValidator,wxValidator)
TClass(wxTextValidator) wxTextValidator_Create( int style, void* val );
TArrayLen  wxTextValidator_GetExcludes( TSelf(wxTextValidator) _obj, TArrayStringOutVoid _ref );
TArrayLen  wxTextValidator_GetIncludes( TSelf(wxTextValidator) _obj, TArrayStringOutVoid _ref );
void       wxTextValidator_SetExcludes( TSelf(wxTextValidator) _obj, TStringVoid list, int count );
void       wxTextValidator_SetIncludes( TSelf(wxTextValidator) _obj, TStringVoid list, int count );
TClass(wxValidator) wxTextValidator_Clone( TSelf(wxTextValidator) _obj );
TBool      wxTextValidator_TransferToWindow( TSelf(wxTextValidator) _obj );
TBool      wxTextValidator_TransferFromWindow( TSelf(wxTextValidator) _obj );
int        wxTextValidator_GetStyle( TSelf(wxTextValidator) _obj );
void       wxTextValidator_OnChar( TSelf(wxTextValidator) _obj, TClass(wxEvent) event );
void       wxTextValidator_SetStyle( TSelf(wxTextValidator) _obj, int style );

/* wxThinSplitterWindow */
TClassDefExtend(wxThinSplitterWindow,wxSplitterWindow)
TClass(wxThinSplitterWindow) wxThinSplitterWindow_Create( TClass(wxWindow) parent, int id, TRect(x,y,w,h), int style );
void       wxThinSplitterWindow_DrawSash( TSelf(wxThinSplitterWindow) _obj, TClass(wxDC) dc );
int        wxThinSplitterWindow_SashHitTest( TSelf(wxThinSplitterWindow) _obj, TPoint(x,y), int tolerance );
void       wxThinSplitterWindow_SizeWindows( TSelf(wxThinSplitterWindow) _obj );

/* wxThread */
TClassDef(wxThread)

/* wxTime */
TClassDefExtend(wxTime,wxObject)

/* wxTimeSpan */
TClassDef(wxTimeSpan)

/* wxTimer */
TClassDefExtend(wxTimer,wxObject)
TClass(wxTimer) wxTimer_Create( TClass(wxWindow) _prt, int _id );
void       wxTimer_Delete( TSelf(wxTimer) _obj );
int        wxTimer_GetInterval( TSelf(wxTimer) _obj );
TBool      wxTimer_IsOneShot( TSelf(wxTimer) _obj );
TBool      wxTimer_IsRuning( TSelf(wxTimer) _obj );
TBool      wxTimer_Start( TSelf(wxTimer) _obj, int _int, TBool _one );
void       wxTimer_Stop( TSelf(wxTimer) _obj );

/* wxTimerBase */
TClassDefExtend(wxTimerBase,wxObject)

/* wxTimerEvent */
TClassDefExtend(wxTimerEvent,wxEvent)
int        wxTimerEvent_GetInterval( TSelf(wxTimerEvent) _obj );

/* wxTimerEx */
TClassDefExtend(wxTimerEx,wxTimer)

/* wxTimerRunner */
TClassDef(wxTimerRunner)

/* wxTipProvider */
TClassDef(wxTipProvider)

/* wxTipWindow */
TClassDefExtend(wxTipWindow,wxPopupTransientWindow)
void       wxTipWindow_Close( TSelf(wxTipWindow) _obj );
TClass(wxTipWindow) wxTipWindow_Create( TClass(wxWindow) parent, TClass(wxString) text, int maxLength );
void       wxTipWindow_SetBoundingRect( TSelf(wxTipWindow) _obj, TRect(x,y,w,h) );
void       wxTipWindow_SetTipWindowPtr( TSelf(wxTipWindow) _obj, void* windowPtr );

/* wxToggleButton */
TClassDefExtend(wxToggleButton,wxControl)
TClass(wxToggleButton) wxToggleButton_Create( TClass(wxWindow) parent, int id, TClass(wxString) label, TRect(x,y,w,h), int style );
TBool      wxToggleButton_Enable( TSelf(wxToggleButton) _obj, TBool enable );
TBool      wxToggleButton_GetValue( TSelf(wxToggleButton) _obj );
void       wxToggleButton_SetLabel( TSelf(wxToggleButton) _obj, TClass(wxString) label );
void       wxToggleButton_SetValue( TSelf(wxToggleButton) _obj, TBool state );

/* wxToolBar */
TClassDefExtend(wxToolBar,wxToolBarBase)
TBool      wxToolBar_AddControl( TSelf(wxToolBar) _obj, TClass(wxControl) ctrl );
void       wxToolBar_AddSeparator( TSelf(wxToolBar) _obj );
// void       wxToolBar_AddTool( TSelf(wxToolBar) _obj, int id, TClass(wxBitmap) bmp, TClass(wxString) shelp, TClass(wxString) lhelp );
void       wxToolBar_AddTool( TSelf(wxToolBar) _obj, int toolid, TClass(wxString) label, TClass(wxBitmap) bitmap, TClass(wxBitmap) bmpDisabled, int kind, TClass(wxString) shortHelp, TClass(wxString) longHelp, TClass(wxObject) data);
// Obsolete
// void    wxToolBar_AddToolEx( TSelf(wxToolBar) _obj, int id, TClass(wxBitmap) bmp1, TClass(wxBitmap) bmp2, TBool isToggle, TPoint(x,y), TClass(wxObject) data, TClass(wxString) shelp, TClass(wxString) lhelp );
TClass(wxToolBar) wxToolBar_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );
void       wxToolBar_Delete( TSelf(wxToolBar) _obj );
TBool      wxToolBar_DeleteTool( TSelf(wxToolBar) _obj, int id );
TBool      wxToolBar_DeleteToolByPos( TSelf(wxToolBar) _obj, int pos );
void       wxToolBar_EnableTool( TSelf(wxToolBar) _obj, int id, TBool enable );
TClass(wxPoint) wxToolBar_GetMargins( TSelf(wxToolBar) _obj );
TClass(wxSize) wxToolBar_GetToolBitmapSize( TSelf(wxToolBar) _obj );
TClass(wxObject) wxToolBar_GetToolClientData( TSelf(wxToolBar) _obj, int id );
TBool      wxToolBar_GetToolEnabled( TSelf(wxToolBar) _obj, int id );
TClass(wxString) wxToolBar_GetToolLongHelp( TSelf(wxToolBar) _obj, int id );
int        wxToolBar_GetToolPacking( TSelf(wxToolBar) _obj );
TClass(wxString) wxToolBar_GetToolShortHelp( TSelf(wxToolBar) _obj, int id );
TClass(wxSize)  wxToolBar_GetToolSize( TSelf(wxToolBar) _obj );
TBool      wxToolBar_GetToolState( TSelf(wxToolBar) _obj, int id );
void       wxToolBar_InsertControl( TSelf(wxToolBar) _obj, int pos, TClass(wxControl) ctrl );
void       wxToolBar_InsertSeparator( TSelf(wxToolBar) _obj, int pos );
// Obsolete
// void       wxToolBar_InsertTool( TSelf(wxToolBar) _obj, int pos, int id, TClass(wxBitmap) bmp1, TClass(wxBitmap) bmp2, TBool isToggle, TClass(wxObject) data, TClass(wxString) shelp, TClass(wxString) lhelp );
TBool      wxToolBar_Realize( TSelf(wxToolBar) _obj );
void       wxToolBar_RemoveTool( TSelf(wxToolBar) _obj, int id );
void       wxToolBar_SetMargins( TSelf(wxToolBar) _obj, TPoint(x,y) );
void       wxToolBar_SetToolBitmapSize( TSelf(wxToolBar) _obj, TSize(x,y) );
void       wxToolBar_SetToolClientData( TSelf(wxToolBar) _obj, int id, TClass(wxObject) data );
void       wxToolBar_SetToolLongHelp( TSelf(wxToolBar) _obj, int id, TClass(wxString) str );
void       wxToolBar_SetToolPacking( TSelf(wxToolBar) _obj, int packing );
void       wxToolBar_SetToolSeparation( TSelf(wxToolBar) _obj, int separation );
void       wxToolBar_SetToolShortHelp( TSelf(wxToolBar) _obj, int id, TClass(wxString) str );
void       wxToolBar_ToggleTool( TSelf(wxToolBar) _obj, int id, TBool toggle );

/* wxToolBarBase */
TClassDefExtend(wxToolBarBase,wxControl)

/* wxToolLayoutItem */
TClassDefExtend(wxToolLayoutItem,wxObject)
TBool      wxToolLayoutItem_IsSeparator( TSelf(wxToolLayoutItem) _obj );
void       wxToolLayoutItem_Rect( TSelf(wxToolLayoutItem) _obj, TRectOutVoid(_x,_y,_w,_h) );

/* wxToolTip */
TClassDefExtend(wxToolTip,wxObject)

/* wxToolWindow */
TClassDefExtend(wxToolWindow,wxFrame)
void       wxToolWindow_AddMiniButton( TSelf(wxToolWindow) _obj, void* _btn );
TClass(wxToolWindow) wxToolWindow_Create( void* _obj, void* _btn, void* _ttl );
TClass(wxClient) wxToolWindow_GetClient( TSelf(wxToolWindow) _obj );
void       wxToolWindow_SetClient( TSelf(wxToolWindow) _obj, TClass(wxWindow) _wnd );
void       wxToolWindow_SetTitleFont( TSelf(wxToolWindow) _obj, void* _fnt );

/* wxTopLevelWindow */
TClassDefExtend(wxTopLevelWindow,wxWindow)
TBool      wxTopLevelWindow_EnableCloseButton( TSelf(wxTopLevelWindow) _obj, TBool enable );
TClass(wxButton) wxTopLevelWindow_GetDefaultButton( TSelf(wxTopLevelWindow) _obj );
TClass(wxWindow) wxTopLevelWindow_GetDefaultItem( TSelf(wxTopLevelWindow) _obj );
TClass(wxIcon) wxTopLevelWindow_GetIcon( TSelf(wxTopLevelWindow) _obj );
TClass(wxString) wxTopLevelWindow_GetTitle( TSelf(wxTopLevelWindow) _obj );
TBool      wxTopLevelWindow_Iconize( TSelf(wxTopLevelWindow) _obj, TBool iconize );
TBool      wxTopLevelWindow_IsActive( TSelf(wxTopLevelWindow) _obj );
TBool      wxTopLevelWindow_IsIconized( TSelf(wxTopLevelWindow) _obj );
TBool      wxTopLevelWindow_IsMaximized( TSelf(wxTopLevelWindow) _obj );
void       wxTopLevelWindow_Maximize( TSelf(wxTopLevelWindow) _obj, TBool maximize );
void       wxTopLevelWindow_RequestUserAttention( TSelf(wxTopLevelWindow) _obj, int flags );
void       wxTopLevelWindow_SetDefaultButton( TSelf(wxTopLevelWindow) _obj, TClass(wxButton) pBut );
void       wxTopLevelWindow_SetDefaultItem( TSelf(wxTopLevelWindow) _obj, TClass(wxWindow) pBut );
void       wxTopLevelWindow_SetIcon( TSelf(wxTopLevelWindow) _obj, TClass(wxIcon) pIcon );
void       wxTopLevelWindow_SetIcons( TSelf(wxTopLevelWindow) _obj, void* _icons );
void       wxTopLevelWindow_SetMaxSize( TSelf(wxTopLevelWindow) _obj, TSize(w,h) );
void       wxTopLevelWindow_SetMinSize( TSelf(wxTopLevelWindow) _obj, TSize(w,h) );
void       wxTopLevelWindow_SetTitle( TSelf(wxTopLevelWindow) _obj, TClass(wxString) pString );

/* wxTreeCompanionWindow */
TClassDefExtend(wxTreeCompanionWindow,wxWindow)
TClass(wxTreeCompanionWindow) wxTreeCompanionWindow_Create( TClass(wxWindow) parent, int id, TRect(x,y,w,h), int style );
void       wxTreeCompanionWindow_DrawItem( TSelf(wxTreeCompanionWindow) _obj, TClass(wxDC) dc, void* id, TRect(x,y,w,h));
TClass(wxTreeCtrl) wxTreeCompanionWindow_GetTreeCtrl( TSelf(wxTreeCompanionWindow) _obj );
void       wxTreeCompanionWindow_SetTreeCtrl( TSelf(wxTreeCompanionWindow) _obj, TClass(wxTreeCtrl) treeCtrl );

/* wxTreeCtrl */
TClassDefExtend(wxTreeCtrl,wxControl)
void       wxTreeCtrl_AddRoot( TSelf(wxTreeCtrl) _obj, TClass(wxString) text, int image, int selectedImage, TClass(wxTreeItemData) data, TClassRef(wxTreeItemId) _item );
void       wxTreeCtrl_AppendItem( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) parent, TClass(wxString) text, int image, int selectedImage, TClass(wxTreeItemData)  data, TClassRef(wxTreeItemId) _item );
void       wxTreeCtrl_Collapse( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
void       wxTreeCtrl_CollapseAndReset( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
TClass(wxTreeCtrl) wxTreeCtrl_Create( void* _obj, void* _cmp, TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );
void       wxTreeCtrl_Delete( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
void       wxTreeCtrl_DeleteAllItems( TSelf(wxTreeCtrl) _obj );
void       wxTreeCtrl_DeleteChildren( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
void       wxTreeCtrl_EditLabel( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
void       wxTreeCtrl_EndEditLabel( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TBool discardChanges );
void       wxTreeCtrl_EnsureVisible( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
void       wxTreeCtrl_Expand( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
TClass(wxRect) wxTreeCtrl_GetBoundingRect( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TBool textOnly );
int        wxTreeCtrl_GetChildrenCount( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TBool recursively );
int        wxTreeCtrl_GetCount( TSelf(wxTreeCtrl) _obj );
TClass(wxTextCtrl) wxTreeCtrl_GetEditControl( TSelf(wxTreeCtrl) _obj );
void       wxTreeCtrl_GetFirstChild( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, int* cookie, TClassRef(wxTreeItemId) _item );
void       wxTreeCtrl_GetFirstVisibleItem( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TClassRef(wxTreeItemId) _item );
TClass(wxImageList) wxTreeCtrl_GetImageList( TSelf(wxTreeCtrl) _obj );
int        wxTreeCtrl_GetIndent( TSelf(wxTreeCtrl) _obj );
void*      wxTreeCtrl_GetItemData( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
int        wxTreeCtrl_GetItemImage( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, int which );
TClass(wxString) wxTreeCtrl_GetItemText( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
void       wxTreeCtrl_GetLastChild( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TClassRef(wxTreeItemId) _item );
void       wxTreeCtrl_GetNextChild( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, int* cookie, TClassRef(wxTreeItemId) _item );
void       wxTreeCtrl_GetNextSibling( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TClassRef(wxTreeItemId) _item );
void       wxTreeCtrl_GetNextVisible( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TClassRef(wxTreeItemId) _item );
void       wxTreeCtrl_GetParent( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TClassRef(wxTreeItemId) _item );
void       wxTreeCtrl_GetPrevSibling( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TClassRef(wxTreeItemId) _item );
void       wxTreeCtrl_GetPrevVisible( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TClassRef(wxTreeItemId) _item );
void       wxTreeCtrl_GetRootItem( TSelf(wxTreeCtrl) _obj, TClassRef(wxTreeItemId) _item );
void       wxTreeCtrl_GetSelection( TSelf(wxTreeCtrl) _obj, TClassRef(wxTreeItemId) _item );
TArrayLen  wxTreeCtrl_GetSelections( TSelf(wxTreeCtrl) _obj, TArrayIntPtrOutVoid selections );
int        wxTreeCtrl_GetSpacing( TSelf(wxTreeCtrl) _obj );
TClass(wxImageList)  wxTreeCtrl_GetStateImageList( TSelf(wxTreeCtrl) _obj );
void       wxTreeCtrl_HitTest( TSelf(wxTreeCtrl) _obj, TPoint(_x,_y), int* flags, TClassRef(wxTreeItemId) _item );
void       wxTreeCtrl_InsertItem( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) parent, TClass(wxTreeItemId) idPrevious, TClass(wxString) text, int image, int selectedImage, void* data, TClassRef(wxTreeItemId) _item );
void       wxTreeCtrl_InsertItemByIndex( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) parent, int index, TClass(wxString) text, int image, int selectedImage, void* data, TClassRef(wxTreeItemId) _item );
TBool      wxTreeCtrl_IsBold( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
TBool      wxTreeCtrl_IsExpanded( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
TBool      wxTreeCtrl_IsSelected( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
TBool      wxTreeCtrl_IsVisible( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
int        wxTreeCtrl_ItemHasChildren( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
int        wxTreeCtrl_OnCompareItems( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item1, TClass(wxTreeItemId) item2 );
void       wxTreeCtrl_PrependItem( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) parent, TClass(wxString) text, int image, int selectedImage, void* data, TClassRef(wxTreeItemId) _item );
void       wxTreeCtrl_ScrollTo( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
void       wxTreeCtrl_SelectItem( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
void       wxTreeCtrl_SetImageList( TSelf(wxTreeCtrl) _obj, TClass(wxImageList) imageList );
void       wxTreeCtrl_SetIndent( TSelf(wxTreeCtrl) _obj, int indent );
void       wxTreeCtrl_SetItemBackgroundColour( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TClass(wxColour) col );
void       wxTreeCtrl_SetItemBold( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TBool bold );
void       wxTreeCtrl_SetItemData( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, void* data );
void       wxTreeCtrl_SetItemDropHighlight( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TBool highlight );
void       wxTreeCtrl_SetItemFont( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TClass(wxFont) font );
void       wxTreeCtrl_SetItemHasChildren( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TBool hasChildren );
void       wxTreeCtrl_SetItemImage( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, int image, int which );
void       wxTreeCtrl_SetItemText( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TClass(wxString) text );
void       wxTreeCtrl_SetItemTextColour( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TClass(wxColour) col );
void       wxTreeCtrl_SetSpacing( TSelf(wxTreeCtrl) _obj, int spacing );
void       wxTreeCtrl_SetStateImageList( TSelf(wxTreeCtrl) _obj, TClass(wxImageList) imageList );
void       wxTreeCtrl_SortChildren( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
void       wxTreeCtrl_Toggle( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
void       wxTreeCtrl_Unselect( TSelf(wxTreeCtrl) _obj );
void       wxTreeCtrl_UnselectAll( TSelf(wxTreeCtrl) _obj );

/* wxTreeEvent */
TClassDefExtend(wxTreeEvent,wxNotifyEvent)
int        wxTreeEvent_GetCode( TSelf(wxTreeEvent) _obj );
void       wxTreeEvent_GetItem( TSelf(wxTreeEvent) _obj, TClassRef(wxTreeItemId) _ref );
TClass(wxString) wxTreeEvent_GetLabel( TSelf(wxTreeEvent) _obj );
void       wxTreeEvent_GetOldItem( TSelf(wxTreeEvent) _obj, TClassRef(wxTreeItemId) _ref );
TClass(wxPoint) wxTreeEvent_GetPoint( TSelf(wxTreeEvent) _obj );

/* wxTreeItemData */
TClassDefExtend(wxTreeItemData,wxClientData)

/* wxTreeItemId */
TClassDef(wxTreeItemId)
TClass(wxTreeItemId) wxTreeItemId_Create(  );
void       wxTreeItemId_Delete( TSelf(wxTreeItemId) _obj );
TBool      wxTreeItemId_IsOk( TSelf(wxTreeItemId) _obj );

/* wxTreeLayout */
TClassDefExtend(wxTreeLayout,wxObject)

/* wxTreeLayoutStored */
TClassDefExtend(wxTreeLayoutStored,wxTreeLayout)

/* wxURL */
TClassDefExtend(wxURL,wxObject)

/* wxUpdateUIEvent */
TClassDefExtend(wxUpdateUIEvent,wxEvent)
void       wxUpdateUIEvent_Check( TSelf(wxUpdateUIEvent) _obj, TBool check );
void       wxUpdateUIEvent_CopyObject( TSelf(wxUpdateUIEvent) _obj, TClass(wxObject) obj );
void       wxUpdateUIEvent_Enable( TSelf(wxUpdateUIEvent) _obj, TBool enable );
TBool      wxUpdateUIEvent_GetChecked( TSelf(wxUpdateUIEvent) _obj );
TBool      wxUpdateUIEvent_GetEnabled( TSelf(wxUpdateUIEvent) _obj );
TBool      wxUpdateUIEvent_GetSetChecked( TSelf(wxUpdateUIEvent) _obj );
TBool      wxUpdateUIEvent_GetSetEnabled( TSelf(wxUpdateUIEvent) _obj );
TBool      wxUpdateUIEvent_GetSetText( TSelf(wxUpdateUIEvent) _obj );
TClass(wxString) wxUpdateUIEvent_GetText( TSelf(wxUpdateUIEvent) _obj );
void       wxUpdateUIEvent_SetText( TSelf(wxUpdateUIEvent) _obj, TClass(wxString) text );

/* wxValidator */
TClassDefExtend(wxValidator,wxEvtHandler)
TClass(wxValidator) wxValidator_Create(  );
void       wxValidator_Delete( TSelf(wxValidator) _obj );
TClass(wxWindow) wxValidator_GetWindow( TSelf(wxValidator) _obj );
// Obsolete
// void       wxValidator_SetBellOnError( TBool doIt );
void       wxValidator_SuppressBellOnError( TBool doIt );
void       wxValidator_SetWindow( TSelf(wxValidator) _obj, TClass(wxWindow) win );
TBool      wxValidator_TransferFromWindow( TSelf(wxValidator) _obj );
TBool      wxValidator_TransferToWindow( TSelf(wxValidator) _obj );
TBool      wxValidator_Validate( TSelf(wxValidator) _obj, TClass(wxWindow) parent );

/* wxVariant */
TClassDefExtend(wxVariant,wxObject)

/* wxVariantData */
TClassDefExtend(wxVariantData,wxObject)

/* wxView */
TClassDefExtend(wxView,wxEvtHandler)

/* wxSound */
TClassDefExtend(wxSound,wxEvtHandler)

/* wxWindow */
TClassDefExtend(wxWindow,wxEvtHandler)
void       wxWindow_AddChild( TSelf(wxWindow) _obj, TClass(wxWindow) child );
void       wxWindow_AddConstraintReference( TSelf(wxWindow) _obj, TClass(wxWindow) otherWin );
void       wxWindow_CaptureMouse( TSelf(wxWindow) _obj );
void       wxWindow_Center( TSelf(wxWindow) _obj, int direction );
void       wxWindow_CenterOnParent( TSelf(wxWindow) _obj, int dir );
void       wxWindow_ClearBackground( TSelf(wxWindow) _obj );
TClass(wxPoint) wxWindow_ClientToScreen( TSelf(wxWindow) _obj, TPoint(x,y) );
TBool      wxWindow_Close( TSelf(wxWindow) _obj, TBool _force );
TClass(wxPoint) wxWindow_ConvertDialogToPixels( TSelf(wxWindow) _obj );
TClass(wxPoint) wxWindow_ConvertPixelsToDialog( TSelf(wxWindow) _obj );
TClass(wxWindow) wxWindow_Create( TClass(wxWindow) _prt, int _id, TRect(_x,_y,_w,_h), int _stl );
void       wxWindow_DeleteRelatedConstraints( TSelf(wxWindow) _obj );
TBool      wxWindow_Destroy( TSelf(wxWindow) _obj );
TBool      wxWindow_DestroyChildren( TSelf(wxWindow) _obj );
TBool      wxWindow_Disable( TSelf(wxWindow) _obj );
int        wxWindow_DoPhase( TSelf(wxWindow) _obj, int phase );
TBool      wxWindow_Enable( TSelf(wxWindow) _obj );
TClass(wxWindow) wxWindow_FindFocus( TSelf(wxWindow) _obj );
TClass(wxWindow) wxWindow_FindWindow( TSelf(wxWindow) _obj, TClass(wxString) name );
void       wxWindow_Fit( TSelf(wxWindow) _obj );
void       wxWindow_FitInside( TSelf(wxWindow) _obj );
void       wxWindow_Freeze( TSelf(wxWindow) _obj );
TClass(wxSize) wxWindow_GetEffectiveMinSize( TSelf(wxWindow) _obj );
int        wxWindow_GetAutoLayout( TSelf(wxWindow) _obj );
void       wxWindow_GetBackgroundColour( TSelf(wxWindow) _obj, TClassRef(wxColour) _ref );
TClass(wxSize) wxWindow_GetBestSize( TSelf(wxWindow) _obj );
TClass(wxCaret) wxWindow_GetCaret( TSelf(wxWindow) _obj );
int        wxWindow_GetCharHeight( TSelf(wxWindow) _obj );
int        wxWindow_GetCharWidth( TSelf(wxWindow) _obj );
int        wxWindow_GetChildren( TSelf(wxWindow) _obj, void* _res, int _cnt );
TClass(wxClientData) wxWindow_GetClientData( TSelf(wxWindow) _obj );
TClass(wxSize) wxWindow_GetClientSize( TSelf(wxWindow) _obj );
void       wxWindow_GetClientSizeConstraint( TSelf(wxWindow) _obj, TSizeOut(_w,_h) );
TClass(wxLayoutConstraints) wxWindow_GetConstraints( TSelf(wxWindow) _obj );
void*      wxWindow_GetConstraintsInvolvedIn( TSelf(wxWindow) _obj );
TClass(wxCursor) wxWindow_GetCursor( TSelf(wxWindow) _obj );
TClass(wxDropTarget) wxWindow_GetDropTarget( TSelf(wxWindow) _obj );
TClass(wxEvtHandler) wxWindow_GetEventHandler( TSelf(wxWindow) _obj );
void       wxWindow_GetFont( TSelf(wxWindow) _obj, TClassRef(wxFont) _ref );
void       wxWindow_GetForegroundColour( TSelf(wxWindow) _obj, TClassRef(wxColour) _ref );
void*      wxWindow_GetHandle( TSelf(wxWindow) _obj );
int        wxWindow_GetId( TSelf(wxWindow) _obj );
TClass(wxString) wxWindow_GetLabel( TSelf(wxWindow) _obj );
int        wxWindow_GetLabelEmpty( TSelf(wxWindow) _obj );
int        wxWindow_GetMaxHeight( TSelf(wxWindow) _obj );
int        wxWindow_GetMaxWidth( TSelf(wxWindow) _obj );
int        wxWindow_GetMinHeight( TSelf(wxWindow) _obj );
int        wxWindow_GetMinWidth( TSelf(wxWindow) _obj );
TClass(wxString) wxWindow_GetName( TSelf(wxWindow) _obj );
TClass(wxWindow) wxWindow_GetParent( TSelf(wxWindow) _obj );
TClass(wxPoint) wxWindow_GetPosition( TSelf(wxWindow) _obj );
void       wxWindow_GetPositionConstraint( TSelf(wxWindow) _obj, TPointOut(_x,_y) );
TClass(wxRect)  wxWindow_GetRect( TSelf(wxWindow) _obj );
int        wxWindow_GetScrollPos( TSelf(wxWindow) _obj, int orient );
int        wxWindow_GetScrollRange( TSelf(wxWindow) _obj, int orient );
int        wxWindow_GetScrollThumb( TSelf(wxWindow) _obj, int orient );
TClass(wxSize) wxWindow_GetSize( TSelf(wxWindow) _obj );
void       wxWindow_GetSizeConstraint( TSelf(wxWindow) _obj, TSizeOut(_w,_h) );
TClass(wxSizer) wxWindow_GetSizer( TSelf(wxWindow) _obj );
void       wxWindow_GetTextExtent( TSelf(wxWindow) _obj, TClass(wxString) string, int* x, int* y, int* descent, int* externalLeading, TClass(wxFont) theFont );
TClass(wxString) wxWindow_GetToolTip( TSelf(wxWindow) _obj );
TClass(wxRegion) wxWindow_GetUpdateRegion( TSelf(wxWindow) _obj );
TClass(wxValidator) wxWindow_GetValidator( TSelf(wxWindow) _obj );
TClass(wxSize) wxWindow_GetVirtualSize( TSelf(wxWindow) _obj );
int        wxWindow_GetWindowStyleFlag( TSelf(wxWindow) _obj );
TBool      wxWindow_HasFlag( TSelf(wxWindow) _obj, int flag );
TBool      wxWindow_HasFocus( TSelf(wxWindow) _obj );
TBool      wxWindow_Hide( TSelf(wxWindow) _obj );
void       wxWindow_InitDialog( TSelf(wxWindow) _obj );
TBool      wxWindow_IsBeingDeleted( TSelf(wxWindow) _obj );
TBool      wxWindow_IsEnabled( TSelf(wxWindow) _obj );
TBool      wxWindow_IsExposed( TSelf(wxWindow) _obj, TRect(x,y,w,h) );
TBool      wxWindow_IsShown( TSelf(wxWindow) _obj );
TBool      wxWindow_IsTopLevel( TSelf(wxWindow) _obj );
int        wxWindow_Layout( TSelf(wxWindow) _obj );
int        wxWindow_LayoutPhase1( TSelf(wxWindow) _obj, int* noChanges );
int        wxWindow_LayoutPhase2( TSelf(wxWindow) _obj, int* noChanges );
void       wxWindow_Lower( TSelf(wxWindow) _obj );
// Obsolete
// void       wxWindow_MakeModal( TSelf(wxWindow) _obj, TBool modal );
void       wxWindow_Move( TSelf(wxWindow) _obj, TPoint(x,y) );
void       wxWindow_MoveConstraint( TSelf(wxWindow) _obj, TPoint(x,y) );
void*      wxWindow_PopEventHandler( TSelf(wxWindow) _obj, TBool deleteHandler );
int        wxWindow_PopupMenu( TSelf(wxWindow) _obj, TClass(wxMenu) menu, TPoint(x,y) );
void       wxWindow_PrepareDC( TSelf(wxWindow) _obj, TClass(wxDC) dc );
void       wxWindow_PushEventHandler( TSelf(wxWindow) _obj, TClass(wxEvtHandler) handler );
void       wxWindow_Raise( TSelf(wxWindow) _obj );
void       wxWindow_Refresh( TSelf(wxWindow) _obj, TBool eraseBackground );
void       wxWindow_RefreshRect( TSelf(wxWindow) _obj, TBool eraseBackground, TRect(x,y,w,h) );
void       wxWindow_ReleaseMouse( TSelf(wxWindow) _obj );
void       wxWindow_RemoveChild( TSelf(wxWindow) _obj, TClass(wxWindow) child );
void       wxWindow_RemoveConstraintReference( TSelf(wxWindow) _obj, TClass(wxWindow) otherWin );
int        wxWindow_Reparent( TSelf(wxWindow) _obj, TClass(wxWindow) _par );
void       wxWindow_ResetConstraints( TSelf(wxWindow) _obj );
TClass(wxPoint) wxWindow_ScreenToClient( TSelf(wxWindow) _obj, TPoint(x,y) );
void       wxWindow_ScrollWindow( TSelf(wxWindow) _obj, TVector(dx,dy) );
void       wxWindow_ScrollWindowRect( TSelf(wxWindow) _obj, TVector(dx,dy), TRect(x,y,w,h) );
void       wxWindow_SetAcceleratorTable( TSelf(wxWindow) _obj, TClass(wxAcceleratorTable) accel );
void       wxWindow_SetAutoLayout( TSelf(wxWindow) _obj, TBool autoLayout );
int        wxWindow_SetBackgroundColour( TSelf(wxWindow) _obj, TClass(wxColour) colour );
void       wxWindow_SetCaret( TSelf(wxWindow) _obj, TClass(wxCaret) caret );
void       wxWindow_SetClientData( TSelf(wxWindow) _obj, TClass(wxClientData) data );
void       wxWindow_SetClientObject( TSelf(wxWindow) _obj, TClass(wxClientData) data );
void       wxWindow_SetClientSize( TSelf(wxWindow) _obj, TSize(width,height) );
void       wxWindow_SetConstraintSizes( TSelf(wxWindow) _obj, int recurse );
void       wxWindow_SetConstraints( TSelf(wxWindow) _obj, TClass(wxLayoutConstraints) constraints );
int        wxWindow_SetCursor( TSelf(wxWindow) _obj, TClass(wxCursor) cursor );
void       wxWindow_SetDropTarget( TSelf(wxWindow) _obj, TClass(wxDropTarget) dropTarget );
void       wxWindow_SetExtraStyle( TSelf(wxWindow) _obj, long exStyle );
void       wxWindow_SetFocus( TSelf(wxWindow) _obj );
int        wxWindow_SetFont( TSelf(wxWindow) _obj, TClass(wxFont) font );
int        wxWindow_SetForegroundColour( TSelf(wxWindow) _obj, TClass(wxColour) colour );
void       wxWindow_SetId( TSelf(wxWindow) _obj, int _id );
void       wxWindow_SetLabel( TSelf(wxWindow) _obj, TClass(wxString) _title );
void       wxWindow_SetName( TSelf(wxWindow) _obj, TClass(wxString) _name );
void       wxWindow_SetScrollPos( TSelf(wxWindow) _obj, int orient, int pos, TBool refresh );
void       wxWindow_SetScrollbar( TSelf(wxWindow) _obj, int orient, int pos, int thumbVisible, int range, TBool refresh );
void       wxWindow_SetSize( TSelf(wxWindow) _obj, TRect(x,y,width,height), int sizeFlags );
void       wxWindow_SetSizeConstraint( TSelf(wxWindow) _obj, TRect(x,y,w,h) );
void       wxWindow_SetSizeHints( TSelf(wxWindow) _obj, int minW, int minH, int maxW, int maxH, int incW, int incH );
void       wxWindow_SetSizer( TSelf(wxWindow) _obj, TClass(wxSizer) sizer );
void       wxWindow_SetToolTip( TSelf(wxWindow) _obj, TClass(wxString) tip );
void       wxWindow_SetValidator( TSelf(wxWindow) _obj, TClass(wxValidator) validator );
void       wxWindow_SetWindowStyleFlag( TSelf(wxWindow) _obj, long style );
TBool      wxWindow_Show( TSelf(wxWindow) _obj );
void       wxWindow_Thaw( TSelf(wxWindow) _obj );
TBool      wxWindow_TransferDataFromWindow( TSelf(wxWindow) _obj );
TBool      wxWindow_TransferDataToWindow( TSelf(wxWindow) _obj );
void       wxWindow_UnsetConstraints( TSelf(wxWindow) _obj, void* c );
void       wxWindow_UpdateWindowUI( TSelf(wxWindow) _obj );
TBool      wxWindow_Validate( TSelf(wxWindow) _obj );
void       wxWindow_SetVirtualSize( TSelf(wxWindow) _obj, TSize(w,h) );
void       wxWindow_WarpPointer( TSelf(wxWindow) _obj, TPoint(x,y) );

/* wxWindowCreateEvent */
TClassDefExtend(wxWindowCreateEvent,wxCommandEvent)
TClass(wxWindow) wxWindowCreateEvent_GetWindow( TSelf(wxWindowCreateEvent) _obj );

/* wxWindowDC */
TClassDefExtend(wxWindowDC,wxDC)
TClass(wxWindowDC) wxWindowDC_Create( TClass(wxWindow) win );
void       wxWindowDC_Delete( TSelf(wxWindowDC) _obj );

/* wxWindowDestroyEvent */
TClassDefExtend(wxWindowDestroyEvent,wxCommandEvent)
TClass(wxWindow) wxWindowDestroyEvent_GetWindow( TSelf(wxWindowDestroyEvent) _obj );

/* wxWindowDisabler */
TClassDef(wxWindowDisabler)

/* wxWizard */
TClassDefExtend(wxWizard,wxDialog)
void       wxWizard_Chain( TClass(wxWizardPageSimple) f, TClass(wxWizardPageSimple) s );
TClass(wxWizard) wxWizard_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TClass(wxBitmap) _bmp, TRect(_lft,_top,_wdt,_hgt) );
TClass(wxWizardPage) wxWizard_GetCurrentPage( TSelf(wxWizard) _obj );
TClass(wxSize) wxWizard_GetPageSize( TSelf(wxWizard) _obj );
int        wxWizard_RunWizard( TSelf(wxWizard) _obj, TClass(wxWizardPage) firstPage );
void       wxWizard_SetPageSize( TSelf(wxWizard) _obj, TSize(w,h) );

/* wxWizardEvent */
TClassDefExtend(wxWizardEvent,wxNotifyEvent)
int        wxWizardEvent_GetDirection( TSelf(wxWizardEvent) _obj );

/* wxWizardPage */
TClassDefExtend(wxWizardPage,wxPanel)

/* wxWizardPageSimple */
TClassDefExtend(wxWizardPageSimple,wxWizardPage)
TClass(wxWizardPageSimple) wxWizardPageSimple_Create( TClass(wxWizard) _prt );
void       wxWizardPageSimple_GetBitmap( TSelf(wxWizardPageSimple) _obj, TClassRef(wxBitmap) _ref );
TClass(wxWizardPageSimple) wxWizardPageSimple_GetNext( TSelf(wxWizardPageSimple) _obj );
TClass(wxWizardPageSimple) wxWizardPageSimple_GetPrev( TSelf(wxWizardPageSimple) _obj );
void       wxWizardPageSimple_SetNext( TSelf(wxWizardPageSimple) _obj, TClass(wxWizardPageSimple) next );
void       wxWizardPageSimple_SetPrev( TSelf(wxWizardPageSimple) _obj, TClass(wxWizardPageSimple) prev );

/* wxXmlResource */
TClassDefExtend(wxXmlResource,wxObject)
void       wxXmlResource_AddHandler( TSelf(wxXmlResource) _obj, TClass(wxEvtHandler) handler );
void       wxXmlResource_AddSubclassFactory( TSelf(wxXmlResource) _obj, void* factory );
int        wxXmlResource_AttachUnknownControl( TSelf(wxXmlResource) _obj, TClass(wxControl) control, TClass(wxWindow) parent );
void       wxXmlResource_ClearHandlers( TSelf(wxXmlResource) _obj );
int        wxXmlResource_CompareVersion( TSelf(wxXmlResource) _obj, int major, int minor, int release, int revision );
TClass(wxXmlResource) wxXmlResource_Create( int flags );
TClass(wxXmlResource) wxXmlResource_CreateFromFile( TClass(wxString) filemask, int flags );
void       wxXmlResource_Delete( TSelf(wxXmlResource) _obj );
TClass(wxXmlResource) wxXmlResource_Get(  );
TClass(wxString) wxXmlResource_GetDomain( TSelf(wxXmlResource) _obj );
int        wxXmlResource_GetFlags( TSelf(wxXmlResource) _obj );
long       wxXmlResource_GetVersion( TSelf(wxXmlResource) _obj );
int        wxXmlResource_GetXRCID( TSelf(wxXmlResource) _obj, TClass(wxString) str_id );
void       wxXmlResource_InitAllHandlers( TSelf(wxXmlResource) _obj );
void       wxXmlResource_InsertHandler( TSelf(wxXmlResource) _obj, TClass(wxEvtHandler) handler );
TBool      wxXmlResource_Load( TSelf(wxXmlResource) _obj, TClass(wxString) filemask );
void       wxXmlResource_LoadBitmap( TSelf(wxXmlResource) _obj, TClass(wxString) name, TClassRef(wxBitmap) _ref );
TClass(wxDialog) wxXmlResource_LoadDialog( TSelf(wxXmlResource) _obj, TClass(wxWindow) parent, TClass(wxString) name );
TClass(wxFrame) wxXmlResource_LoadFrame( TSelf(wxXmlResource) _obj, TClass(wxWindow) parent, TClass(wxString) name );
void       wxXmlResource_LoadIcon( TSelf(wxXmlResource) _obj, TClass(wxString) name, TClassRef(wxIcon) _ref );
TClass(wxMenu) wxXmlResource_LoadMenu( TSelf(wxXmlResource) _obj, TClass(wxString) name );
TClass(wxMenuBar) wxXmlResource_LoadMenuBar( TSelf(wxXmlResource) _obj, TClass(wxWindow) parent, TClass(wxString) name );
TClass(wxPanel) wxXmlResource_LoadPanel( TSelf(wxXmlResource) _obj, TClass(wxWindow) parent, TClass(wxString) name );
TClass(wxToolBar) wxXmlResource_LoadToolBar( TSelf(wxXmlResource) _obj, TClass(wxWindow) parent, TClass(wxString) name );
TClass(wxSizer) wxXmlResource_GetSizer( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxBoxSizer) wxXmlResource_GetBoxSizer( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxStaticBoxSizer) wxXmlResource_GetStaticBoxSizer( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxGridSizer) wxXmlResource_GetGridSizer( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxFlexGridSizer) wxXmlResource_GetFlexGridSizer( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxBitmapButton) wxXmlResource_GetBitmapButton( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxButton) wxXmlResource_GetButton( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxCalendarCtrl) wxXmlResource_GetCalendarCtrl( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxCheckBox) wxXmlResource_GetCheckBox( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxCheckListBox) wxXmlResource_GetCheckListBox( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxChoice) wxXmlResource_GetChoice( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxComboBox) wxXmlResource_GetComboBox( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxGauge) wxXmlResource_GetGauge( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxGrid) wxXmlResource_GetGrid( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxHtmlWindow) wxXmlResource_GetHtmlWindow( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxListBox) wxXmlResource_GetListBox( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxListCtrl) wxXmlResource_GetListCtrl( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxMDIChildFrame) wxXmlResource_GetMDIChildFrame( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxMDIParentFrame) wxXmlResource_GetMDIParentFrame( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxMenu) wxXmlResource_GetMenu( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxMenuBar) wxXmlResource_GetMenuBar( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxMenuItem) wxXmlResource_GetMenuItem( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxNotebook) wxXmlResource_GetNotebook( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxPanel) wxXmlResource_GetPanel( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxRadioButton) wxXmlResource_GetRadioButton( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxRadioBox) wxXmlResource_GetRadioBox( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxScrollBar) wxXmlResource_GetScrollBar( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxScrolledWindow) wxXmlResource_GetScrolledWindow( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxSlider) wxXmlResource_GetSlider( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxSpinButton) wxXmlResource_GetSpinButton( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxSpinCtrl) wxXmlResource_GetSpinCtrl( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxSplitterWindow) wxXmlResource_GetSplitterWindow( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxStaticBitmap) wxXmlResource_GetStaticBitmap( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxStaticBox) wxXmlResource_GetStaticBox( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxStaticLine) wxXmlResource_GetStaticLine( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxStaticText) wxXmlResource_GetStaticText( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxTextCtrl) wxXmlResource_GetTextCtrl( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TClass(wxTreeCtrl) wxXmlResource_GetTreeCtrl( TSelf(wxWindow) _obj, TClass(wxString) str_id );
TBool      wxXmlResource_Unload( TSelf(wxXmlResource) _obj, TClass(wxString) filemask );
TClass(wxXmlResource) wxXmlResource_Set( TSelf(wxXmlResource) _obj, TSelf(wxXmlResource) res );
void       wxXmlResource_SetDomain( TSelf(wxXmlResource) _obj, TClass(wxString) domain );
void       wxXmlResource_SetFlags( TSelf(wxXmlResource) _obj, int flags );

/* wxXmlResourceHandler */
TClassDefExtend(wxXmlResourceHandler,wxObject)

/* wxZipInputStream */
TClassDefExtend(wxZipInputStream,wxInputStream)

/* wxZlibInputStream */
TClassDefExtend(wxZlibInputStream,wxFilterInputStream)

/* wxZlibOutputStream */
TClassDefExtend(wxZlibOutputStream,wxFilterOutputStream)

/* wxPropertyGrid */
TClassDefExtend(wxPropertyGrid,wxControl)
TClass(wxPGProperty)    wxPropertyGrid_Append( TSelf(wxPropertyGrid) _obj, TClass(wxPGProperty) prop );
TClass(wxPropertyGrid)  wxPropertyGrid_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );
TBool                   wxPropertyGrid_DisableProperty( TSelf(wxPropertyGrid) _obj, TClass(wxString) propName );

/* wxPropertyGridEvent */
TClassDefExtend(wxPropertyGridEvent,wxNotifyEvent)
TBool                   wxPropertyGridEvent_HasProperty( TSelf(wxPropertyGridEvent) _obj );
TClass(wxPGProperty)    wxPropertyGridEvent_GetProperty( TSelf(wxPropertyGridEvent) _obj );

/* wxPGProperty */
TClassDefExtend(wxPGProperty,wxObject)
TClass(wxString) wxPGProperty_GetLabel( TSelf(wxPGProperty) _obj );
TClass(wxString) wxPGProperty_GetName( TSelf(wxPGProperty) _obj );
TClass(wxString) wxPGProperty_GetValueAsString( TSelf(wxPGProperty) _obj );
TClass(wxString) wxPGProperty_GetValueType( TSelf(wxPGProperty) _obj );
void wxPGProperty_SetHelpString( TSelf(wxPGProperty) _obj, TClass(wxString) helpString );

/* wxStringProperty */
TClassDefExtend(wxStringProperty,wxPGProperty)
TClass(wxStringProperty) wxStringProperty_Create( TClass(wxString) label, TClass(wxString) name, TClass(wxString) value );

/* wxIntProperty */
TClassDefExtend(wxIntProperty,wxPGProperty)
TClass(wxIntProperty) wxIntProperty_Create( TClass(wxString) label, TClass(wxString) name, int value );

/* wxBoolProperty */
TClassDefExtend(wxBoolProperty,wxPGProperty)
TClass(wxBoolProperty) wxBoolProperty_Create( TClass(wxString) label, TClass(wxString) name, TBool value );

/* wxFloatProperty */
TClassDefExtend(wxFloatProperty,wxPGProperty)
TClass(wxFloatProperty) wxFloatProperty_Create( TClass(wxString) label, TClass(wxString) name, float value );

/* wxDateProperty */
TClassDefExtend(wxDateProperty,wxPGProperty)
TClass(wxDateProperty) wxDateProperty_Create( TClass(wxString) label, TClass(wxString) name, TClass(wxDateTime) value );

/* wxFileProperty */
TClassDefExtend(wxFileProperty,wxPGProperty)
TClass(wxFileProperty) wxFileProperty_Create( TClass(wxString) label, TClass(wxString) name, TClass(wxString) value);

/* wxPropertyCategory */
TClassDefExtend(wxPropertyCategory,wxPGProperty)
TClass(wxPropertyCategory) wxPropertyCategory_Create( TClass(wxString) label);

#endif /* WXC_GLUE_H */

