000100*----------------------------------------------------------------
000200* This copybook is used as a parameter to the translation        
000300* subroutines for the new Master Database.                       
000400* ---------------------------------------------------------------
000500 01 YYYN111A.                                                    
000600     05 YYYN111A-FUNCTION             PIC X(1)   VALUE SPACES.   
000700         88 YYYN111A-NEW-2-OLD                   VALUE 'N'.      
000800         88 YYYN111A-OLD-2-NEW                   VALUE 'O'.      
000810     05 YYYN111A-WHAT-FUNC            PIC X(1)   VALUE SPACES.   
000820         88 YYYN111A-ALL                         VALUE ' '.      
000830         88 YYYN111A-KEYS-ONLY                   VALUE 'X'.      
000900     05 FILLER                        PIC X(99)  VALUE SPACES.   