#ifndef __ELJGRID_H
#define __ELJGRID_H

#include "wx/grid.h"

extern "C"
{
typedef int   _cdecl (*TGridGetInt)(void* _obj);
typedef int   _cdecl (*TGridIsEmpty)(void* _obj, int row, int col);
typedef void* _cdecl (*TGridGetValue)(void* _obj, int row, int col);
typedef void  _cdecl (*TGridSetValue)(void* _obj, int row, int col, void* val);
typedef void  _cdecl (*TGridClear)(void* _obj);
typedef int   _cdecl (*TGridModify)(void* _obj, int pos, int num);
typedef int   _cdecl (*TGridMultiModify)(void* _obj, int num);
typedef void  _cdecl (*TGridSetLabel)(void* _obj, int idx, void* val);
typedef void* _cdecl (*TGridGetLabel)(void* _obj, int idx);
}

class ELJGridTable : public wxGridTableBase
{
	private:
		void* EiffelObject;
		TGridGetInt EifGetNumberRows;
		TGridGetInt EifGetNumberCols;
		TGridGetValue EifGetValue;
		TGridSetValue EifSetValue;
		TGridIsEmpty EifIsEmptyCell;
		TGridClear EifClear;
		TGridModify EifInsertRows;
		TGridMultiModify EifAppendRows;
		TGridModify EifDeleteRows;
		TGridModify EifInsertCols;
		TGridMultiModify EifAppendCols;
		TGridModify EifDeleteCols;
		TGridSetLabel EifSetRowLabelValue;
		TGridSetLabel EifSetColLabelValue;
		TGridGetLabel EifGetRowLabelValue;
		TGridGetLabel EifGetColLabelValue;
	public:
		ELJGridTable (void* _obj,
		              void* _EifGetNumberRows,
		              void* _EifGetNumberCols,
		              void* _EifGetValue,
		              void* _EifSetValue,
		              void* _EifIsEmptyCell,
		              void* _EifClear,
		              void* _EifInsertRows,
		              void* _EifAppendRows,
		              void* _EifDeleteRows,
		              void* _EifInsertCols,
		              void* _EifAppendCols,
		              void* _EifDeleteCols,
		              void* _EifSetRowLabelValue,
		              void* _EifSetColLabelValue,
		              void* _EifGetRowLabelValue,
		              void* _EifGetColLabelValue): wxGridTableBase()
		{
			EiffelObject = _obj;
			EifGetNumberRows = (TGridGetInt)_EifGetNumberRows;
			EifGetNumberCols = (TGridGetInt)_EifGetNumberCols;
			EifGetValue = (TGridGetValue)_EifGetValue;
			EifSetValue = (TGridSetValue)_EifSetValue;
			EifIsEmptyCell = (TGridIsEmpty)_EifIsEmptyCell;
			EifClear = (TGridClear)_EifClear;
			EifInsertRows = (TGridModify)_EifInsertRows;
			EifAppendRows = (TGridMultiModify)_EifAppendRows;
			EifDeleteRows = (TGridModify)_EifDeleteRows;
			EifInsertCols = (TGridModify)_EifInsertCols;
			EifAppendCols = (TGridMultiModify)_EifAppendCols;
			EifDeleteCols = (TGridModify)_EifDeleteCols;
			EifSetRowLabelValue = (TGridSetLabel)_EifSetRowLabelValue;
			EifSetColLabelValue = (TGridSetLabel)_EifSetColLabelValue;
			EifGetRowLabelValue = (TGridGetLabel)_EifGetRowLabelValue;
			EifGetColLabelValue = (TGridGetLabel)_EifGetColLabelValue;
		};
		
		int GetNumberRows() {return EifGetNumberRows(EiffelObject);};
		int GetNumberCols() {return EifGetNumberCols(EiffelObject);};
		wxString GetValue(int row, int col) {return (wxChar*)EifGetValue(EiffelObject, row, col);};
		void SetValue(int row, int col, const wxString& s) {EifSetValue(EiffelObject, row, col, (void*)s.wchar_str());};
		bool IsEmptyCell(int row, int col) {return EifIsEmptyCell(EiffelObject, row, col) != 0;};

		void Clear() {EifClear(EiffelObject);};
		bool InsertRows(size_t pos, size_t numRows) {return EifInsertRows(EiffelObject, (int)pos, (int)numRows) != 0;};
		bool AppendRows(size_t numRows) {return EifAppendRows(EiffelObject, (int)numRows) != 0;};
		bool DeleteRows(size_t pos, size_t numRows) {return EifDeleteRows(EiffelObject, (int)pos, (int)numRows) != 0;};
		bool InsertCols(size_t pos, size_t numCols) {return EifInsertCols(EiffelObject, (int)pos, (int)numCols) != 0;};
		bool AppendCols(size_t numCols) {return EifAppendCols(EiffelObject, (int)numCols) != 0;};
		bool DeleteCols(size_t pos, size_t numCols) {return EifDeleteCols(EiffelObject, (int)pos, (int)numCols) != 0;};

		void SetRowLabelValue(int row, const wxString& s) {EifSetRowLabelValue(EiffelObject, row, (void*)s.wchar_str());};
		void SetColLabelValue(int col, const wxString& s) {EifSetColLabelValue(EiffelObject, col, (void*)s.wchar_str());};
		wxString GetRowLabelValue(int row) {return (wxChar*)EifGetRowLabelValue(EiffelObject, row);};
		wxString GetColLabelValue(int col) {return (wxChar*)EifGetColLabelValue(EiffelObject, col);};
};

#endif
