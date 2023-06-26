void wxStyledTextCtrl_AddText(TSelf(wxStyledTextCtrl) _obj, TClass(wxString) text);

void wxStyledTextCtrl_AddStyledText(TSelf(wxStyledTextCtrl) _obj, TClass(wxMemoryBuffer) data);

void wxStyledTextCtrl_InsertText(TSelf(wxStyledTextCtrl) _obj, int pos, TClass(wxString) text);

void wxStyledTextCtrl_ClearAll(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_ClearDocumentStyle(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_GetLength(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_GetCharAt(TSelf(wxStyledTextCtrl) _obj, int pos);

int wxStyledTextCtrl_GetCurrentPos(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_GetAnchor(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_GetStyleAt(TSelf(wxStyledTextCtrl) _obj, int pos);

void wxStyledTextCtrl_Redo(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetUndoCollection(TSelf(wxStyledTextCtrl) _obj, TBool collectUndo);

void wxStyledTextCtrl_SelectAll(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetSavePoint(TSelf(wxStyledTextCtrl) _obj);

TBool wxStyledTextCtrl_CanRedo(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_MarkerLineFromHandle(TSelf(wxStyledTextCtrl) _obj, int handle);

void wxStyledTextCtrl_MarkerDeleteHandle(TSelf(wxStyledTextCtrl) _obj, int handle);

TBool wxStyledTextCtrl_GetUndoCollection(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_GetViewWhiteSpace(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetViewWhiteSpace(TSelf(wxStyledTextCtrl) _obj, int viewWS);

int wxStyledTextCtrl_PositionFromPoint(TSelf(wxStyledTextCtrl) _obj, TPoint(pt_x,pt_y));

int wxStyledTextCtrl_PositionFromPointClose(TSelf(wxStyledTextCtrl) _obj, int x, int y);

void wxStyledTextCtrl_GotoLine(TSelf(wxStyledTextCtrl) _obj, int line);

void wxStyledTextCtrl_GotoPos(TSelf(wxStyledTextCtrl) _obj, int pos);

void wxStyledTextCtrl_SetAnchor(TSelf(wxStyledTextCtrl) _obj, int posAnchor);

int wxStyledTextCtrl_GetEndStyled(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_ConvertEOLs(TSelf(wxStyledTextCtrl) _obj, int eolMode);

int wxStyledTextCtrl_GetEOLMode(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetEOLMode(TSelf(wxStyledTextCtrl) _obj, int eolMode);

void wxStyledTextCtrl_StartStyling(TSelf(wxStyledTextCtrl) _obj, int pos, int mask);

void wxStyledTextCtrl_SetStyling(TSelf(wxStyledTextCtrl) _obj, int length, int style);

TBool wxStyledTextCtrl_GetBufferedDraw(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetBufferedDraw(TSelf(wxStyledTextCtrl) _obj, TBool buffered);

void wxStyledTextCtrl_SetTabWidth(TSelf(wxStyledTextCtrl) _obj, int tabWidth);

int wxStyledTextCtrl_GetTabWidth(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetCodePage(TSelf(wxStyledTextCtrl) _obj, int codePage);

void wxStyledTextCtrl_MarkerDefine(TSelf(wxStyledTextCtrl) _obj, int markerNumber, int markerSymbol, TColorRGB(foreground_r,foreground_g,foreground_b), TColorRGB(background_r,background_g,background_b));

void wxStyledTextCtrl_MarkerSetForeground(TSelf(wxStyledTextCtrl) _obj, int markerNumber, TColorRGB(fore_r,fore_g,fore_b));

void wxStyledTextCtrl_MarkerSetBackground(TSelf(wxStyledTextCtrl) _obj, int markerNumber, TColorRGB(back_r,back_g,back_b));

int wxStyledTextCtrl_MarkerAdd(TSelf(wxStyledTextCtrl) _obj, int line, int markerNumber);

void wxStyledTextCtrl_MarkerDelete(TSelf(wxStyledTextCtrl) _obj, int line, int markerNumber);

void wxStyledTextCtrl_MarkerDeleteAll(TSelf(wxStyledTextCtrl) _obj, int markerNumber);

int wxStyledTextCtrl_MarkerGet(TSelf(wxStyledTextCtrl) _obj, int line);

int wxStyledTextCtrl_MarkerNext(TSelf(wxStyledTextCtrl) _obj, int lineStart, int markerMask);

int wxStyledTextCtrl_MarkerPrevious(TSelf(wxStyledTextCtrl) _obj, int lineStart, int markerMask);

void wxStyledTextCtrl_MarkerDefineBitmap(TSelf(wxStyledTextCtrl) _obj, int markerNumber, TClass(wxBitmap) bmp);

void wxStyledTextCtrl_SetMarginType(TSelf(wxStyledTextCtrl) _obj, int margin, int marginType);

int wxStyledTextCtrl_GetMarginType(TSelf(wxStyledTextCtrl) _obj, int margin);

void wxStyledTextCtrl_SetMarginWidth(TSelf(wxStyledTextCtrl) _obj, int margin, int pixelWidth);

int wxStyledTextCtrl_GetMarginWidth(TSelf(wxStyledTextCtrl) _obj, int margin);

void wxStyledTextCtrl_SetMarginMask(TSelf(wxStyledTextCtrl) _obj, int margin, int mask);

int wxStyledTextCtrl_GetMarginMask(TSelf(wxStyledTextCtrl) _obj, int margin);

void wxStyledTextCtrl_SetMarginSensitive(TSelf(wxStyledTextCtrl) _obj, int margin, TBool sensitive);

TBool wxStyledTextCtrl_GetMarginSensitive(TSelf(wxStyledTextCtrl) _obj, int margin);

void wxStyledTextCtrl_StyleClearAll(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_StyleSetForeground(TSelf(wxStyledTextCtrl) _obj, int style, TColorRGB(fore_r,fore_g,fore_b));

void wxStyledTextCtrl_StyleSetBackground(TSelf(wxStyledTextCtrl) _obj, int style, TColorRGB(back_r,back_g,back_b));

void wxStyledTextCtrl_StyleSetBold(TSelf(wxStyledTextCtrl) _obj, int style, TBool bold);

void wxStyledTextCtrl_StyleSetItalic(TSelf(wxStyledTextCtrl) _obj, int style, TBool italic);

void wxStyledTextCtrl_StyleSetSize(TSelf(wxStyledTextCtrl) _obj, int style, int sizePoints);

void wxStyledTextCtrl_StyleSetFaceName(TSelf(wxStyledTextCtrl) _obj, int style, TClass(wxString) fontName);

void wxStyledTextCtrl_StyleSetEOLFilled(TSelf(wxStyledTextCtrl) _obj, int style, TBool filled);

void wxStyledTextCtrl_StyleResetDefault(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_StyleSetUnderline(TSelf(wxStyledTextCtrl) _obj, int style, TBool underline);

void wxStyledTextCtrl_StyleSetCase(TSelf(wxStyledTextCtrl) _obj, int style, int caseForce);

void wxStyledTextCtrl_StyleSetCharacterSet(TSelf(wxStyledTextCtrl) _obj, int style, int characterSet);

void wxStyledTextCtrl_StyleSetHotSpot(TSelf(wxStyledTextCtrl) _obj, int style, TBool hotspot);

void wxStyledTextCtrl_SetSelForeground(TSelf(wxStyledTextCtrl) _obj, TBool useSetting, TColorRGB(fore_r,fore_g,fore_b));

void wxStyledTextCtrl_SetSelBackground(TSelf(wxStyledTextCtrl) _obj, TBool useSetting, TColorRGB(back_r,back_g,back_b));

void wxStyledTextCtrl_SetCaretForeground(TSelf(wxStyledTextCtrl) _obj, TColorRGB(fore_r,fore_g,fore_b));

void wxStyledTextCtrl_CmdKeyAssign(TSelf(wxStyledTextCtrl) _obj, int key, int modifiers, int cmd);

void wxStyledTextCtrl_CmdKeyClear(TSelf(wxStyledTextCtrl) _obj, int key, int modifiers);

void wxStyledTextCtrl_CmdKeyClearAll(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetStyleBytes(TSelf(wxStyledTextCtrl) _obj, int length, char* styleBytes);

void wxStyledTextCtrl_StyleSetVisible(TSelf(wxStyledTextCtrl) _obj, int style, TBool visible);

int wxStyledTextCtrl_GetCaretPeriod(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetCaretPeriod(TSelf(wxStyledTextCtrl) _obj, int periodMilliseconds);

void wxStyledTextCtrl_SetWordChars(TSelf(wxStyledTextCtrl) _obj, TClass(wxString) characters);

void wxStyledTextCtrl_BeginUndoAction(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_EndUndoAction(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_IndicatorSetStyle(TSelf(wxStyledTextCtrl) _obj, int indic, int style);

int wxStyledTextCtrl_IndicatorGetStyle(TSelf(wxStyledTextCtrl) _obj, int indic);

void wxStyledTextCtrl_IndicatorSetForeground(TSelf(wxStyledTextCtrl) _obj, int indic, TColorRGB(fore_r,fore_g,fore_b));

void wxStyledTextCtrl_SetWhitespaceForeground(TSelf(wxStyledTextCtrl) _obj, TBool useSetting, TColorRGB(fore_r,fore_g,fore_b));

void wxStyledTextCtrl_SetWhitespaceBackground(TSelf(wxStyledTextCtrl) _obj, TBool useSetting, TColorRGB(back_r,back_g,back_b));

void wxStyledTextCtrl_SetStyleBits(TSelf(wxStyledTextCtrl) _obj, int bits);

int wxStyledTextCtrl_GetStyleBits(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetLineState(TSelf(wxStyledTextCtrl) _obj, int line, int state);

int wxStyledTextCtrl_GetLineState(TSelf(wxStyledTextCtrl) _obj, int line);

int wxStyledTextCtrl_GetMaxLineState(TSelf(wxStyledTextCtrl) _obj);

TBool wxStyledTextCtrl_GetCaretLineVisible(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetCaretLineVisible(TSelf(wxStyledTextCtrl) _obj, TBool show);

void wxStyledTextCtrl_StyleSetChangeable(TSelf(wxStyledTextCtrl) _obj, int style, TBool changeable);

void wxStyledTextCtrl_AutoCompShow(TSelf(wxStyledTextCtrl) _obj, int lenEntered, TClass(wxString) itemList);

void wxStyledTextCtrl_AutoCompCancel(TSelf(wxStyledTextCtrl) _obj);

TBool wxStyledTextCtrl_AutoCompActive(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_AutoCompPosStart(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_AutoCompComplete(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_AutoCompStops(TSelf(wxStyledTextCtrl) _obj, TClass(wxString) characterSet);

void wxStyledTextCtrl_AutoCompSetSeparator(TSelf(wxStyledTextCtrl) _obj, int separatorCharacter);

int wxStyledTextCtrl_AutoCompGetSeparator(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_AutoCompSelect(TSelf(wxStyledTextCtrl) _obj, TClass(wxString) text);

void wxStyledTextCtrl_AutoCompSetCancelAtStart(TSelf(wxStyledTextCtrl) _obj, TBool cancel);

TBool wxStyledTextCtrl_AutoCompGetCancelAtStart(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_AutoCompSetFillUps(TSelf(wxStyledTextCtrl) _obj, TClass(wxString) characterSet);

void wxStyledTextCtrl_AutoCompSetChooseSingle(TSelf(wxStyledTextCtrl) _obj, TBool chooseSingle);

TBool wxStyledTextCtrl_AutoCompGetChooseSingle(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_AutoCompSetIgnoreCase(TSelf(wxStyledTextCtrl) _obj, TBool ignoreCase);

TBool wxStyledTextCtrl_AutoCompGetIgnoreCase(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_UserListShow(TSelf(wxStyledTextCtrl) _obj, int listType, TClass(wxString) itemList);

void wxStyledTextCtrl_AutoCompSetAutoHide(TSelf(wxStyledTextCtrl) _obj, TBool autoHide);

TBool wxStyledTextCtrl_AutoCompGetAutoHide(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_AutoCompSetDropRestOfWord(TSelf(wxStyledTextCtrl) _obj, TBool dropRestOfWord);

TBool wxStyledTextCtrl_AutoCompGetDropRestOfWord(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_RegisterImage(TSelf(wxStyledTextCtrl) _obj, int type, TClass(wxBitmap) bmp);

void wxStyledTextCtrl_ClearRegisteredImages(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_AutoCompGetTypeSeparator(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_AutoCompSetTypeSeparator(TSelf(wxStyledTextCtrl) _obj, int separatorCharacter);

void wxStyledTextCtrl_SetIndent(TSelf(wxStyledTextCtrl) _obj, int indentSize);

int wxStyledTextCtrl_GetIndent(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetUseTabs(TSelf(wxStyledTextCtrl) _obj, TBool useTabs);

TBool wxStyledTextCtrl_GetUseTabs(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetLineIndentation(TSelf(wxStyledTextCtrl) _obj, int line, int indentSize);

int wxStyledTextCtrl_GetLineIndentation(TSelf(wxStyledTextCtrl) _obj, int line);

int wxStyledTextCtrl_GetLineIndentPosition(TSelf(wxStyledTextCtrl) _obj, int line);

int wxStyledTextCtrl_GetColumn(TSelf(wxStyledTextCtrl) _obj, int pos);

void wxStyledTextCtrl_SetUseHorizontalScrollBar(TSelf(wxStyledTextCtrl) _obj, TBool show);

TBool wxStyledTextCtrl_GetUseHorizontalScrollBar(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetIndentationGuides(TSelf(wxStyledTextCtrl) _obj, TBool show);

TBool wxStyledTextCtrl_GetIndentationGuides(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetHighlightGuide(TSelf(wxStyledTextCtrl) _obj, int column);

int wxStyledTextCtrl_GetHighlightGuide(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_GetLineEndPosition(TSelf(wxStyledTextCtrl) _obj, int line);

int wxStyledTextCtrl_GetCodePage(TSelf(wxStyledTextCtrl) _obj);

TBool wxStyledTextCtrl_GetReadOnly(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetCurrentPos(TSelf(wxStyledTextCtrl) _obj, int pos);

void wxStyledTextCtrl_SetSelectionStart(TSelf(wxStyledTextCtrl) _obj, int pos);

int wxStyledTextCtrl_GetSelectionStart(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetSelectionEnd(TSelf(wxStyledTextCtrl) _obj, int pos);

int wxStyledTextCtrl_GetSelectionEnd(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetPrintMagnification(TSelf(wxStyledTextCtrl) _obj, int magnification);

int wxStyledTextCtrl_GetPrintMagnification(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetPrintColourMode(TSelf(wxStyledTextCtrl) _obj, int mode);

int wxStyledTextCtrl_GetPrintColourMode(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_FindText(TSelf(wxStyledTextCtrl) _obj, int minPos, int maxPos, TClass(wxString) text, int flags);

int wxStyledTextCtrl_FormatRange(TSelf(wxStyledTextCtrl) _obj, TBool doDraw, int startPos, int endPos, TClass(wxDC) draw, TClass(wxDC) target, TClass(wxRect) renderRect, TClass(wxRect) pageRect);

int wxStyledTextCtrl_GetFirstVisibleLine(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_GetLineCount(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetMarginLeft(TSelf(wxStyledTextCtrl) _obj, int pixelWidth);

int wxStyledTextCtrl_GetMarginLeft(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetMarginRight(TSelf(wxStyledTextCtrl) _obj, int pixelWidth);

int wxStyledTextCtrl_GetMarginRight(TSelf(wxStyledTextCtrl) _obj);

TBool wxStyledTextCtrl_GetModify(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetSelection(TSelf(wxStyledTextCtrl) _obj, int start, int end);

void wxStyledTextCtrl_HideSelection(TSelf(wxStyledTextCtrl) _obj, TBool normal);

int wxStyledTextCtrl_LineFromPosition(TSelf(wxStyledTextCtrl) _obj, int pos);

int wxStyledTextCtrl_PositionFromLine(TSelf(wxStyledTextCtrl) _obj, int line);

void wxStyledTextCtrl_LineScroll(TSelf(wxStyledTextCtrl) _obj, int columns, int lines);

void wxStyledTextCtrl_EnsureCaretVisible(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_ReplaceSelection(TSelf(wxStyledTextCtrl) _obj, TClass(wxString) text);

void wxStyledTextCtrl_SetReadOnly(TSelf(wxStyledTextCtrl) _obj, TBool readOnly);

TBool wxStyledTextCtrl_CanPaste(TSelf(wxStyledTextCtrl) _obj);

TBool wxStyledTextCtrl_CanUndo(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_EmptyUndoBuffer(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_Undo(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_Cut(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_Copy(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_Paste(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_Clear(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetText(TSelf(wxStyledTextCtrl) _obj, TClass(wxString) text);

int wxStyledTextCtrl_GetTextLength(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetOvertype(TSelf(wxStyledTextCtrl) _obj, TBool overtype);

TBool wxStyledTextCtrl_GetOvertype(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetCaretWidth(TSelf(wxStyledTextCtrl) _obj, int pixelWidth);

int wxStyledTextCtrl_GetCaretWidth(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetTargetStart(TSelf(wxStyledTextCtrl) _obj, int pos);

int wxStyledTextCtrl_GetTargetStart(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetTargetEnd(TSelf(wxStyledTextCtrl) _obj, int pos);

int wxStyledTextCtrl_GetTargetEnd(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_ReplaceTarget(TSelf(wxStyledTextCtrl) _obj, TClass(wxString) text);

int wxStyledTextCtrl_ReplaceTargetRE(TSelf(wxStyledTextCtrl) _obj, TClass(wxString) text);

int wxStyledTextCtrl_SearchInTarget(TSelf(wxStyledTextCtrl) _obj, TClass(wxString) text);

void wxStyledTextCtrl_SetSearchFlags(TSelf(wxStyledTextCtrl) _obj, int flags);

int wxStyledTextCtrl_GetSearchFlags(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_CallTipShow(TSelf(wxStyledTextCtrl) _obj, int pos, TClass(wxString) definition);

void wxStyledTextCtrl_CallTipCancel(TSelf(wxStyledTextCtrl) _obj);

TBool wxStyledTextCtrl_CallTipActive(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_CallTipPosAtStart(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_CallTipSetHighlight(TSelf(wxStyledTextCtrl) _obj, int start, int end);

void wxStyledTextCtrl_CallTipSetBackground(TSelf(wxStyledTextCtrl) _obj, TColorRGB(back_r,back_g,back_b));

void wxStyledTextCtrl_CallTipSetForeground(TSelf(wxStyledTextCtrl) _obj, TColorRGB(fore_r,fore_g,fore_b));

void wxStyledTextCtrl_CallTipSetForegroundHighlight(TSelf(wxStyledTextCtrl) _obj, TColorRGB(fore_r,fore_g,fore_b));

int wxStyledTextCtrl_VisibleFromDocLine(TSelf(wxStyledTextCtrl) _obj, int line);

int wxStyledTextCtrl_DocLineFromVisible(TSelf(wxStyledTextCtrl) _obj, int lineDisplay);

void wxStyledTextCtrl_SetFoldLevel(TSelf(wxStyledTextCtrl) _obj, int line, int level);

int wxStyledTextCtrl_GetFoldLevel(TSelf(wxStyledTextCtrl) _obj, int line);

int wxStyledTextCtrl_GetLastChild(TSelf(wxStyledTextCtrl) _obj, int line, int level);

int wxStyledTextCtrl_GetFoldParent(TSelf(wxStyledTextCtrl) _obj, int line);

void wxStyledTextCtrl_ShowLines(TSelf(wxStyledTextCtrl) _obj, int lineStart, int lineEnd);

void wxStyledTextCtrl_HideLines(TSelf(wxStyledTextCtrl) _obj, int lineStart, int lineEnd);

TBool wxStyledTextCtrl_GetLineVisible(TSelf(wxStyledTextCtrl) _obj, int line);

void wxStyledTextCtrl_SetFoldExpanded(TSelf(wxStyledTextCtrl) _obj, int line, TBool expanded);

TBool wxStyledTextCtrl_GetFoldExpanded(TSelf(wxStyledTextCtrl) _obj, int line);

void wxStyledTextCtrl_ToggleFold(TSelf(wxStyledTextCtrl) _obj, int line);

void wxStyledTextCtrl_EnsureVisible(TSelf(wxStyledTextCtrl) _obj, int line);

void wxStyledTextCtrl_SetFoldFlags(TSelf(wxStyledTextCtrl) _obj, int flags);

void wxStyledTextCtrl_EnsureVisibleEnforcePolicy(TSelf(wxStyledTextCtrl) _obj, int line);

void wxStyledTextCtrl_SetTabIndents(TSelf(wxStyledTextCtrl) _obj, TBool tabIndents);

TBool wxStyledTextCtrl_GetTabIndents(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetBackSpaceUnIndents(TSelf(wxStyledTextCtrl) _obj, TBool bsUnIndents);

TBool wxStyledTextCtrl_GetBackSpaceUnIndents(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetMouseDwellTime(TSelf(wxStyledTextCtrl) _obj, int periodMilliseconds);

int wxStyledTextCtrl_GetMouseDwellTime(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_WordStartPosition(TSelf(wxStyledTextCtrl) _obj, int pos, TBool onlyWordCharacters);

int wxStyledTextCtrl_WordEndPosition(TSelf(wxStyledTextCtrl) _obj, int pos, TBool onlyWordCharacters);

void wxStyledTextCtrl_SetWrapMode(TSelf(wxStyledTextCtrl) _obj, int mode);

int wxStyledTextCtrl_GetWrapMode(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetLayoutCache(TSelf(wxStyledTextCtrl) _obj, int mode);

int wxStyledTextCtrl_GetLayoutCache(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetScrollWidth(TSelf(wxStyledTextCtrl) _obj, int pixelWidth);

int wxStyledTextCtrl_GetScrollWidth(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_TextWidth(TSelf(wxStyledTextCtrl) _obj, int style, TClass(wxString) text);

void wxStyledTextCtrl_SetEndAtLastLine(TSelf(wxStyledTextCtrl) _obj, TBool endAtLastLine);

int wxStyledTextCtrl_GetEndAtLastLine(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_TextHeight(TSelf(wxStyledTextCtrl) _obj, int line);

void wxStyledTextCtrl_SetUseVerticalScrollBar(TSelf(wxStyledTextCtrl) _obj, TBool show);

TBool wxStyledTextCtrl_GetUseVerticalScrollBar(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_AppendText(TSelf(wxStyledTextCtrl) _obj, TClass(wxString) text);

TBool wxStyledTextCtrl_GetTwoPhaseDraw(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetTwoPhaseDraw(TSelf(wxStyledTextCtrl) _obj, TBool twoPhase);

void wxStyledTextCtrl_TargetFromSelection(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_LinesJoin(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_LinesSplit(TSelf(wxStyledTextCtrl) _obj, int pixelWidth);

void wxStyledTextCtrl_SetFoldMarginColour(TSelf(wxStyledTextCtrl) _obj, TBool useSetting, TColorRGB(back_r,back_g,back_b));

void wxStyledTextCtrl_SetFoldMarginHiColour(TSelf(wxStyledTextCtrl) _obj, TBool useSetting, TColorRGB(fore_r,fore_g,fore_b));

void wxStyledTextCtrl_LineDuplicate(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_HomeDisplay(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_HomeDisplayExtend(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_LineEndDisplay(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_LineEndDisplayExtend(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_LineCopy(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_MoveCaretInsideView(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_LineLength(TSelf(wxStyledTextCtrl) _obj, int line);

void wxStyledTextCtrl_BraceHighlight(TSelf(wxStyledTextCtrl) _obj, int pos1, int pos2);

void wxStyledTextCtrl_BraceBadLight(TSelf(wxStyledTextCtrl) _obj, int pos);

int wxStyledTextCtrl_BraceMatch(TSelf(wxStyledTextCtrl) _obj, int pos);

TBool wxStyledTextCtrl_GetViewEOL(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetViewEOL(TSelf(wxStyledTextCtrl) _obj, TBool visible);

void wxStyledTextCtrl_SetDocPointer(TSelf(wxStyledTextCtrl) _obj, TClass(wxSTCDoc) docPointer);

void wxStyledTextCtrl_SetModEventMask(TSelf(wxStyledTextCtrl) _obj, int mask);

int wxStyledTextCtrl_GetEdgeColumn(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetEdgeColumn(TSelf(wxStyledTextCtrl) _obj, int column);

int wxStyledTextCtrl_GetEdgeMode(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetEdgeMode(TSelf(wxStyledTextCtrl) _obj, int mode);

void wxStyledTextCtrl_SetEdgeColour(TSelf(wxStyledTextCtrl) _obj, TColorRGB(edgeColour_r,edgeColour_g,edgeColour_b));

void wxStyledTextCtrl_SearchAnchor(TSelf(wxStyledTextCtrl) _obj);

int wxStyledTextCtrl_SearchNext(TSelf(wxStyledTextCtrl) _obj, int flags, TClass(wxString) text);

int wxStyledTextCtrl_SearchPrev(TSelf(wxStyledTextCtrl) _obj, int flags, TClass(wxString) text);

int wxStyledTextCtrl_LinesOnScreen(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_UsePopUp(TSelf(wxStyledTextCtrl) _obj, TBool allowPopUp);

TBool wxStyledTextCtrl_SelectionIsRectangle(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetZoom(TSelf(wxStyledTextCtrl) _obj, int zoom);

int wxStyledTextCtrl_GetZoom(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_AddRefDocument(TSelf(wxStyledTextCtrl) _obj, TClass(wxSTCDoc) docPointer);

void wxStyledTextCtrl_ReleaseDocument(TSelf(wxStyledTextCtrl) _obj, TClass(wxSTCDoc) docPointer);

int wxStyledTextCtrl_GetModEventMask(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetSTCFocus(TSelf(wxStyledTextCtrl) _obj, TBool focus);

TBool wxStyledTextCtrl_GetSTCFocus(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetStatus(TSelf(wxStyledTextCtrl) _obj, int statusCode);

int wxStyledTextCtrl_GetStatus(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetMouseDownCaptures(TSelf(wxStyledTextCtrl) _obj, TBool captures);

TBool wxStyledTextCtrl_GetMouseDownCaptures(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetSTCCursor(TSelf(wxStyledTextCtrl) _obj, int cursorType);

int wxStyledTextCtrl_GetSTCCursor(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetControlCharSymbol(TSelf(wxStyledTextCtrl) _obj, int symbol);

int wxStyledTextCtrl_GetControlCharSymbol(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_WordPartLeft(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_WordPartLeftExtend(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_WordPartRight(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_WordPartRightExtend(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetVisiblePolicy(TSelf(wxStyledTextCtrl) _obj, int visiblePolicy, int visibleSlop);

void wxStyledTextCtrl_DelLineLeft(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_DelLineRight(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetXOffset(TSelf(wxStyledTextCtrl) _obj, int newOffset);

int wxStyledTextCtrl_GetXOffset(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_ChooseCaretX(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetXCaretPolicy(TSelf(wxStyledTextCtrl) _obj, int caretPolicy, int caretSlop);

void wxStyledTextCtrl_SetYCaretPolicy(TSelf(wxStyledTextCtrl) _obj, int caretPolicy, int caretSlop);

void wxStyledTextCtrl_SetPrintWrapMode(TSelf(wxStyledTextCtrl) _obj, int mode);

int wxStyledTextCtrl_GetPrintWrapMode(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetHotspotActiveForeground(TSelf(wxStyledTextCtrl) _obj, TBool useSetting, TColorRGB(fore_r,fore_g,fore_b));

void wxStyledTextCtrl_SetHotspotActiveBackground(TSelf(wxStyledTextCtrl) _obj, TBool useSetting, TColorRGB(back_r,back_g,back_b));

void wxStyledTextCtrl_SetHotspotActiveUnderline(TSelf(wxStyledTextCtrl) _obj, TBool underline);

int wxStyledTextCtrl_PositionBefore(TSelf(wxStyledTextCtrl) _obj, int pos);

int wxStyledTextCtrl_PositionAfter(TSelf(wxStyledTextCtrl) _obj, int pos);

void wxStyledTextCtrl_CopyRange(TSelf(wxStyledTextCtrl) _obj, int start, int end);

void wxStyledTextCtrl_CopyText(TSelf(wxStyledTextCtrl) _obj, int length, TClass(wxString) text);

void wxStyledTextCtrl_StartRecord(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_StopRecord(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetLexer(TSelf(wxStyledTextCtrl) _obj, int lexer);

int wxStyledTextCtrl_GetLexer(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_Colourise(TSelf(wxStyledTextCtrl) _obj, int start, int end);

void wxStyledTextCtrl_SetProperty(TSelf(wxStyledTextCtrl) _obj, TClass(wxString) key, TClass(wxString) value);

void wxStyledTextCtrl_SetKeyWords(TSelf(wxStyledTextCtrl) _obj, int keywordSet, TClass(wxString) keyWords);

void wxStyledTextCtrl_SetLexerLanguage(TSelf(wxStyledTextCtrl) _obj, TClass(wxString) language);

int wxStyledTextCtrl_GetCurrentLine(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_StyleSetSpec(TSelf(wxStyledTextCtrl) _obj, int styleNum, TClass(wxString) spec);

void wxStyledTextCtrl_StyleSetFont(TSelf(wxStyledTextCtrl) _obj, int styleNum, TClass(wxFont) font);

void wxStyledTextCtrl_StyleSetFontAttr(TSelf(wxStyledTextCtrl) _obj, int styleNum, int size, TClass(wxString) faceName, TBool bold, TBool italic, TBool underline);

void wxStyledTextCtrl_CmdKeyExecute(TSelf(wxStyledTextCtrl) _obj, int cmd);

void wxStyledTextCtrl_SetMargins(TSelf(wxStyledTextCtrl) _obj, int left, int right);

void wxStyledTextCtrl_GetSelection(TSelf(wxStyledTextCtrl) _obj, int* startPos, int* endPos);

void wxStyledTextCtrl_ScrollToLine(TSelf(wxStyledTextCtrl) _obj, int line);

void wxStyledTextCtrl_ScrollToColumn(TSelf(wxStyledTextCtrl) _obj, int column);

void wxStyledTextCtrl_SetVScrollBar(TSelf(wxStyledTextCtrl) _obj, TClass(wxScrollBar) bar);

void wxStyledTextCtrl_SetHScrollBar(TSelf(wxStyledTextCtrl) _obj, TClass(wxScrollBar) bar);

TBool wxStyledTextCtrl_GetLastKeydownProcessed(TSelf(wxStyledTextCtrl) _obj);

void wxStyledTextCtrl_SetLastKeydownProcessed(TSelf(wxStyledTextCtrl) _obj, TBool val);

TBool wxStyledTextCtrl_SaveFile(TSelf(wxStyledTextCtrl) _obj, TClass(wxString) filename);

TBool wxStyledTextCtrl_LoadFile(TSelf(wxStyledTextCtrl) _obj, TClass(wxString) filename);
