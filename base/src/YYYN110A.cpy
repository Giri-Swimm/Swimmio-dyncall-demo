000100*---------------------------------------------------------------- 00000100
000200* Standard copybook for sync objects...                           00000200
000300* --------------------------------------------------------------- 00000300
000400 01 YYYN110A.                                                     00000400
000500     05 YYYN110A-IO-FUNC              PIC X(1)   VALUE SPACES.    00000500
000600         88 YYYN110A-ADD                         VALUE 'A'.       00000600
000700         88 YYYN110A-UPD                         VALUE 'U'.       00000700
000800         88 YYYN110A-DEL                         VALUE 'D'.       00000800
000900     05 YYYN110A-SYS-ENV              PIC X(2)   VALUE SPACES.    00000900
001000         88 YYYN110A-CICS-ENV                    VALUE '  '.      00001000
001100         88 YYYN110A-BATCH-ENV                   VALUE 'BT'.      00001100
001200     05 YYYN110A-CALL-SW              PIC X(1)   VALUE SPACES.    00001200
001300         88 YYYN110A-FIRST-CALL                  VALUE ' '.       00001300
001400         88 YYYN110A-LAST-CALL                   VALUE 'X'.       00001400
001500     05 YYYN110A-CONNECT              PIC X(1)   VALUE SPACES.    00001500
001600         88 YYYN110A-DB2                         VALUE ' '.       00001600
001700         88 YYYN110A-ORACLE                      VALUE 'O'.       00001700
001800     05 FILLER                        PIC X(98)  VALUE SPACES.    00001800
