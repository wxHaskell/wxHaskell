-- extra constant definitions lacking from the eiffel wx-defs.e
wxBU_EXACTFIT		: BIT 32 is 00000000000000000000000000000001B				    
wxTE_PROCESS_ENTER	: BIT 32 is 00000000000000000000010000000000B
wxTE_PASSWORD		: BIT 32 is 00000000000000000000100000000000B
wxTE_RICH2		: BIT 32 is 00000000000000001000000000000000B	
wxTE_AUTO_URL		: BIT 32 is 00000000000000000001000000000000B
wxTE_NOHIDESEL		: BIT 32 is 00000000000000000010000000000000B	
wxTE_LEFT		: BIT 32 is 00000000000000000000000000000000B
wxTE_RIGHT		: BIT 32 is 00000000000000000000001000000000B
wxTE_CENTRE		: BIT 32 is 00000000000000000000000100000000B	
wxTE_LINEWRAP		: BIT 32 is 00000000000000000100000000000000B
wxTE_WORDWRAP		: BIT 32 is 00000000000000000000000000000000B
wxEXEC_ASYNC            : BIT 32 is 00000000000000000000000000000000B
wxEXEC_SYNC             : BIT 32 is 00000000000000000000000000000001B
wxEXEC_NOHIDE           : BIT 32 is 00000000000000000000000000000010B
wxEXEC_MAKE_GROUP_LEADER : BIT 32 is 00000000000000000000000000000100B
wxITEM_SEPARATOR        : INTEGER is -1
wxITEM_NORMAL           : INTEGER is 0
wxITEM_CHECK            : INTEGER is 1
wxITEM_RADIO            : INTEGER is 2
wxITEM_MAX              : INTEGER is 3

wxCLOSE_BOX		: INTEGER is 4096
wxFRAME_EX_CONTEXTHELP  : INTEGER is 4
wxDIALOG_EX_CONTEXTHELP : INTEGER is 4
wxFRAME_SHAPED		: INTEGER is 16
wxFULLSCREEN_ALL	: INTEGER is 31


wxTreeItemIcon_Normal			: INTEGER is 0
wxTreeItemIcon_Selected			: INTEGER is 1
wxTreeItemIcon_Expanded			: INTEGER is 2
wxTreeItemIcon_SelectedExpanded		: INTEGER is 3


wxSIGNONE : INTEGER is 0
wxSIGHUP  : INTEGER is 1
wxSIGINT  : INTEGER is 2
wxSIGQUIT : INTEGER is 3
wxSIGILL  : INTEGER is 4
wxSIGTRAP : INTEGER is 5 
wxSIGABRT : INTEGER is 6
wxSIGEMT  : INTEGER is 7
wxSIGFPE  : INTEGER is 8
wxSIGKILL : INTEGER is 9     
wxSIGBUS  : INTEGER is 10
wxSIGSEGV : INTEGER is 11
wxSIGSYS  : INTEGER is 12
wxSIGPIPE : INTEGER is 13
wxSIGALRM : INTEGER is 14
wxSIGTERM : INTEGER is 15   

wxKILL_OK	     : INTEGER is 0 
wxKILL_BAD_SIGNAL    : INTEGER is 1     
wxKILL_ACCESS_DENIED : INTEGER is 2  
wxKILL_NO_PROCESS    : INTEGER is 3     
wxKILL_ERROR         : INTEGER is 4    

wxSTREAM_NO_ERROR    : INTEGER is 0 
wxSTREAM_EOF         : INTEGER is 1 
wxSTREAM_WRITE_ERROR : INTEGER is 2 
wxSTREAM_READ_ERROR  : INTEGER is 3 

wxNB_TOP             : INTEGER is 0 
wxNB_MULTILINE       : INTEGER is 256

wxLC_VRULES          : INTEGER is 1
wxLC_HRULES          : INTEGER is 2

wxIMAGE_LIST_NORMAL  : INTEGER is 0
wxIMAGE_LIST_SMALL   : INTEGER is 1
wxIMAGE_LIST_STATE   : INTEGER is 2

wxTB_TEXT	     : INTEGER is 256

wxADJUST_MINSIZE     : INTEGER is 32768

wxDB_TYPE_NAME_LEN            : INTEGER is 40
wxDB_MAX_STATEMENT_LEN        : INTEGER is 4096
wxDB_MAX_WHERE_CLAUSE_LEN     : INTEGER is 2048
wxDB_MAX_ERROR_MSG_LEN        : INTEGER is 512
wxDB_MAX_ERROR_HISTORY        : INTEGER is 5
wxDB_MAX_TABLE_NAME_LEN       : INTEGER is 128
wxDB_MAX_COLUMN_NAME_LEN      : INTEGER is 128

wxDB_DATA_TYPE_VARCHAR        : INTEGER is 1
wxDB_DATA_TYPE_INTEGER        : INTEGER is 2
wxDB_DATA_TYPE_FLOAT          : INTEGER is 3
wxDB_DATA_TYPE_DATE           : INTEGER is 4
wxDB_DATA_TYPE_BLOB           : INTEGER is 5

wxDB_SELECT_KEYFIELDS         : INTEGER is 1
wxDB_SELECT_WHERE             : INTEGER is 2
wxDB_SELECT_MATCHING          : INTEGER is 3
wxDB_SELECT_STATEMENT         : INTEGER is 4

wxDB_UPD_KEYFIELDS            : INTEGER is 1
wxDB_UPD_WHERE                : INTEGER is 2

wxDB_DEL_KEYFIELDS            : INTEGER is 1
wxDB_DEL_WHERE                : INTEGER is 2
wxDB_DEL_MATCHING             : INTEGER is 3

wxDB_WHERE_KEYFIELDS          : INTEGER is 1
wxDB_WHERE_MATCHING           : INTEGER is 2

wxDB_GRANT_SELECT             : INTEGER is 1
wxDB_GRANT_INSERT             : INTEGER is 2
wxDB_GRANT_UPDATE             : INTEGER is 4
wxDB_GRANT_DELETE             : INTEGER is 8
wxDB_GRANT_ALL                : INTEGER is 15

wxSQL_INVALID_HANDLE		 : INTEGER is -2
wxSQL_ERROR			 : INTEGER is -1
wxSQL_SUCCESS 			 : INTEGER is 0
wxSQL_SUCCESS_WITH_INFO		 : INTEGER is 1
wxSQL_NO_DATA_FOUND		 : INTEGER is 100

wxSQL_CHAR			 : INTEGER is 1
wxSQL_NUMERIC 			 : INTEGER is 2
wxSQL_DECIMAL 			 : INTEGER is 3
wxSQL_INTEGER 			 : INTEGER is 4
wxSQL_SMALLINT			 : INTEGER is 5
wxSQL_FLOAT			 : INTEGER is 6
wxSQL_REAL			 : INTEGER is 7
wxSQL_DOUBLE			 : INTEGER is 8
wxSQL_VARCHAR 			 : INTEGER is 12

wxSQL_C_CHAR			 : INTEGER is 1
wxSQL_C_LONG			 : INTEGER is 4
wxSQL_C_SHORT			 : INTEGER is 5
wxSQL_C_FLOAT			 : INTEGER is 7
wxSQL_C_DOUBLE			 : INTEGER is 8
wxSQL_C_DEFAULT 		 : INTEGER is 99

wxSQL_NO_NULLS			 : INTEGER is 0
wxSQL_NULLABLE			 : INTEGER is 1
wxSQL_NULLABLE_UNKNOWN		 : INTEGER is 2

wxSQL_NULL_DATA			 : INTEGER is -1
wxSQL_DATA_AT_EXEC		 : INTEGER is -2
wxSQL_NTS 			 : INTEGER is -3

wxSQL_DATE			 : INTEGER is 9
wxSQL_TIME			 : INTEGER is 10
wxSQL_TIMESTAMP			 : INTEGER is 11

wxSQL_C_DATE			 : INTEGER is 9
wxSQL_C_TIME			 : INTEGER is 10
wxSQL_C_TIMESTAMP 		 : INTEGER is 11

wxSQL_FETCH_NEXT		 : INTEGER is 1
wxSQL_FETCH_FIRST 		 : INTEGER is 2
wxSQL_FETCH_LAST		 : INTEGER is 3
wxSQL_FETCH_PRIOR		 : INTEGER is 4
wxSQL_FETCH_ABSOLUTE		 : INTEGER is 5
wxSQL_FETCH_RELATIVE		 : INTEGER is 6
wxSQL_FETCH_BOOKMARK		 : INTEGER is 8
