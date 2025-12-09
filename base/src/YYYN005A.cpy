000100********************** START OF YYYN005A *************************00000100
000200*                                                                 00000200
000300* COPY BOOK PASSED TO THE DAO OBJECT                              00000300
000400*                                                                 00000400
000500******************************************************************00000500
000600 01 YYYN005A.                                                     00000600
000700*                                                                 00000700
000800*    =============================================================00000800
000900*    YYYN005A-TRX-DATA. CONTRAINS DATA THAT IS RELATED TO A SINGLE00000900
001000*    LOGICAL TRANSACTION (WHICH MAY OPERATE ON SEVERAL TABLES).   00001000
001100*    THIS DATA IS RESET AT THE START OF A LOGICAL TRANSACTION.    00001100
001200*    AND CONTAINS DATA ABOUT THT TRANSACTION.                     00001200
001300*    =============================================================00001300
001400*                                                                 00001400
001500     03 YYYN005A-TRX-DATA.                                        00001500
001600         05 TBL-FUNCS                    PIC X(4) VALUE SPACES.   00001600
001700         05 REDEFINES TBL-FUNCS.                                  00001700
001800             10 IO-FUNC                  PIC X(2).                00001800
001900                 88 TF-NULL-FUNC         VALUE SPACES.            00001900
002000                 88 TF-SQL-OPEN-CURSOR   VALUE 'OC'.              00002000
002100                 88 TF-SQL-CLOSE-CURSOR  VALUE 'CC'.              00002100
002200                 88 TF-GET-UNIQUE        VALUE 'GU'.              00002200
002300                 88 TF-GET-PREV          VALUE 'GV'.              00002300
002400                 88 TF-GET-NEXT          VALUE 'GN'.              00002400
002500                 88 TF-GET-NEXT-IN-PARENT VALUE 'GI'.             00002500
002600                 88 TF-GET-FIRST         VALUE 'GF'.              00002600
002700                 88 TF-GET-LAST          VALUE 'GL'.              00002700
002800                 88 TF-GET-BROWSE        VALUE 'GB'.              00002800
002900                 88 TF-PUT-MODIFY        VALUE 'PM'.              00002900
003000                 88 TF-PUT-MODIFY-OR-INSERT VALUE 'MA'.           00003000
003100                 88 TF-PUT-INSERT        VALUE 'PA'.              00003100
003200                 88 TF-PUT-PURGE         VALUE 'PP'.              00003200
003300                 88 TF-OPEN-FILE         VALUE 'FO'.              00003300
003400                 88 TF-CLOSE-FILE        VALUE 'FC'.              00003400
003500                 88 TF-POST-TRX-CALLBACK VALUE 'CB'.              00003500
003600                 88 TF-DUMMY-IO          VALUE 'XX'.              00003600
003700                 88 TF-RESET-FUNC        VALUE 'RR'.              00003700
003800                                                                  00003800
003900           05 DAO-STATUS.                                         00003900
004000               10 IMS-STUFF.                                      00004000
004100                   15 IMS-FUNCTION       PIC X(4) VALUE SPACES.   00004100
004200                   15 IMS-RETURN-CODE    PIC X(2) VALUE SPACES.   00004200
004300                       88 IMS-OK         VALUE '  '.              00004300
004400                       88 IMS-NOT-FOUND  VALUE 'GE'.              00004400
004500                       88 IMS-DUPLICATE-KEY VALUE 'II'.           00004500
004600                       88 IMS-CHK-POINT-ERR VALUE 'QC'.           00004600
004700                       88 IMS-END-OF-FILE VALUE 'GB'.             00004700
004800                                                                  00004800
004900               10 DB2-STUFF.                                      00004900
005000                   15 WS-CURSOR-STATUS   PIC X(1) VALUE SPACES.   00005000
005100                       88 CURSOR-IS-OPEN VALUE 'O'.               00005100
005200                       88 CURSOR-IS-NOT-OPEN VALUE SPACES.        00005200
005300                   15 DB2-SQL-CODE       PIC S9(4) COMP VALUE 0.  00005300
005400                       88 SQL-OK         VALUE 0.                 00005400
005500                       88 SQL-NOT-FOUND  VALUE +100.              00005500
005600                       88 SQL-RI-ERROR   VALUE -530.              00005600
005700                       88 SQL-DEADLOCK   VALUE -911.              00005700
005800                       88 SQL-RESOURCE-AVAIL VALUE -904.          00005800
005900                       88 SQL-DUPLICATE-KEY VALUE -803.           00005900
006000                   15 FILLER             PIC X(20)  VALUE SPACES. 00006000
006100                                                                  00006100
006200               10 VSAM-STUFF.                                     00006200
006300                   15 VSAM-RETURN-CODE   PIC S9(9) COMP VALUE 0.  00006300
006400                       88 VSAM-OK        VALUE 0.                 00006400
006500                       88 VSAM-REC-NOT-FOUND VALUE 1.             00006500
006600                       88 VSAM-DUPLICATE-KEY VALUE 2.             00006600
006700                       88 VSAM-END-OF-FILE VALUE 3.               00006700
006800                       88 VSAM-SYSTEM-ERR VALUE 4.                00006800
006900                   15 VSAM-STATUS        PIC X(2)   VALUE SPACES. 00006900
007000                   15 REDEFINES VSAM-STATUS.                      00007000
007100                       20 VSAM-STATUS-N  PIC 9(2).                00007100
007200                   15 FILLER             PIC X(18)  VALUE SPACES. 00007200
007300                                                                  00007300
007400               10 DAO-RETURN-CODE        PIC S9(9) COMP VALUE 0.  00007400
007500                   88 DAO-OK             VALUE 0.                 00007500
007600                   88 DAO-REC-NOT-FOUND  VALUE 1.                 00007600
007700                   88 DAO-DUPLICATE-KEY  VALUE 2.                 00007700
007800                   88 DAO-END-OF-FILE    VALUE 3.                 00007800
007900                   88 DAO-SYSTEM-ERR     VALUE 4.                 00007900
008000                                                                  00008000
008100               10 FLD-ERR-CNT           PIC S9(4) COMP VALUE 0.   00008100
008200               10 FIELD-ERRORS          OCCURS 255 TIMES.         00008200
008300                   15 ERR-TBL           PIC S9(9) COMP.           00008300
008400                   15 ERR-FLD           PIC S9(4) COMP.           00008400
008500                                                                  00008500
008600           05 YYYN005A-MSG-STUFF.                                 00008600
008700               10 YYYN005A-MSG-SW PIC X(1)  VALUE SPACES.         00008700
008800                   88 YYYN005A-DEFAULT-MSG  VALUE ' '.            00008800
008900                   88 YYYN005A-PERSIST-MSG  VALUE 'P'.            00008900
009000               10 YYYN005A-MSG   PIC X(80)  VALUE SPACES.         00009000
009100           05 YYYN005A-ERR-SW    PIC X(1)   VALUE SPACES.         00009100
009200               88 YYYN005A-NORMAL-ERR       VALUE ' '.            00009200
009300               88 YYYN005A-RESET-IO-ERR     VALUE 'X'.            00009300
009400           05 YYYN005A-RSN-CD    PIC X(1)   VALUE SPACES.         00009400
009500           05 FILLER             PIC X(072) VALUE SPACES.         00009500
009600                                                                  00009600
009700*                                                                 00009700
009800*    =============================================================00009800
009900*    YYYN005A-DRIVER-DATA. CONTRAINS DATA THAT IS RELATED TO      00009900
010000*    THE APPLICATION DRIVER.  THIS AREA IS NEVER INITIALIZED      00010000
010100*    BY ANY INTERNAL DAO MODULE.  RATHER, THE DRIVER MUST RESET   00010100
010200*    THESE VARIABLES IF AND WHEN IT IS NECESSARY.                 00010200
010300*    =============================================================00010300
010400*                                                                 00010400
010500       03 YYYN005A-DRIVER-DATA.                                   00010500
010600           05 YYYN005A-CHKPT-CNT PIC S9(9) COMP VALUE 0.          00010600
010700           05 YYYN005A-SYS-ENV   PIC X(2)  VALUE SPACES.          00010700
010800               88 YYYN005A-CICS-ENV        VALUE SPACES.          00010800
010900               88 YYYN005A-BATCH-ENV       VALUE 'BT'.            00010900
011000           05 YYYN005A-CONNECT   PIC X(1)  VALUE SPACES.          00011000
011100               88 YYYN005A-DB2             VALUE SPACES.          00011100
011200               88 YYYN005A-ORACLE          VALUE 'O'.             00011200
011300           05 FILLER             PIC X(093) VALUE SPACES.         00011300
011400                                                                  00011400
011500********************** END   OF YYYN005A *************************00011500
