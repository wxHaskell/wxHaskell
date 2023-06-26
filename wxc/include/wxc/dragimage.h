/*-----------------------------------------------------------------------------
  DragImage
-----------------------------------------------------------------------------*/
TClassDefExtend(wxGenericDragImage,wxDragImage);

TClass(wxDragImage)  wxDragImage_Create( TClass(wxBitmap) image, int x, int y );
TClass(wxDragImage)  wxDragIcon( TClass(wxIcon) icon, int x, int y );
TClass(wxDragImage)  wxDragString( TClass(wxString) test, int x, int y );
TClass(wxDragImage)  wxDragTreeItem( TClass(wxTreeCtrl) treeCtrl, TClass(wxTreeItemId) id );
TClass(wxDragImage)  wxDragListItem( TClass(wxListCtrl) treeCtrl, long id );
TClass(wxGenericDragImage)  wxGenericDragImage_Create( TClass(wxCursor) cursor );
TClass(wxGenericDragImage)  wxGenericDragIcon( TClass(wxIcon) icon );
TClass(wxGenericDragImage)  wxGenericDragString( TClass(wxString) test );
TClass(wxGenericDragImage)  wxGenericDragTreeItem( TClass(wxTreeCtrl) treeCtrl, TClass(wxTreeItemId) id );
TClass(wxGenericDragImage)  wxGenericDragListItem( TClass(wxListCtrl) treeCtrl, long id );
void  wxDragImage_Delete(TSelf(wxDragImage) self);
TBool  wxDragImage_BeginDragFullScreen(TSelf(wxDragImage) self, int x_pos, int y_pos, TClass(wxWindow) window, TBool fullScreen, TClass(wxRect) rect);
TBool  wxDragImage_BeginDrag(TSelf(wxDragImage) self, int x, int y, TClass(wxWindow) window, TClass(wxWindow) boundingWindow );
TBool  wxGenericDragImage_DoDrawImage(TSelf(wxGenericDragImage) self, TClass(wxDC) dc, int x, int y );
void  wxDragImage_EndDrag(TSelf(wxDragImage) self );
TClass(wxRect) wxGenericDragImage_GetImageRect(TSelf(wxGenericDragImage) self, int x_pos, int y_pos );
TBool  wxDragImage_Hide(TSelf(wxDragImage) self );
TBool  wxDragImage_Move(TSelf(wxDragImage) self, int x, int y );
TBool  wxDragImage_Show(TSelf(wxDragImage) self );
TBool  wxGenericDragImage_UpdateBackingFromWindow(TSelf(wxGenericDragImage) self, TClass(wxDC) windowDC, TClass(wxMemoryDC) destDC, int x, int y, int w, int h, int xdest, int ydest, int width, int height );
