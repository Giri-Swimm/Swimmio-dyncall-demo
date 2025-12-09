000100 01 NNNN0000-PARMS.                                               00000100
000200     05 NNNN0000-EXIT-INFO.                                       00000200
000300                                                                  00000300
000400* ----------------------------------------------------------------00000400
000500* EXIT FILE RELATED VARIABLES.                                    00000500
000600* ----------------------------------------------------------------00000600
000700         10 NNNN0000-FILE          PIC X(8)   VALUE SPACES.       00000700
000800             88 NNNN0000-XXXPST01             VALUE 'XXXPST01'.   00000800
000900             88 NNNN0000-XXXPIM01             VALUE 'XXXPIM01'.   00000900
001000             88 NNNN0000-XXXPIM10             VALUE 'XXXPIM10'.   00001000
001100             88 NNNN0000-XXXPTR01             VALUE 'XXXPTR01'.   00001100
001200             88 NNNN0000-XXXPTR10             VALUE 'XXXPTR10'.   00001200
001300             88 NNNN0000-XXXPTR30             VALUE 'XXXPTR30'.   00001300
001400             88 NNNN0000-XXXPTR40             VALUE 'XXXPTR40'.   00001400
001500             88 NNNN0000-XXXPTR50             VALUE 'XXXPTR50'.   00001500
001600             88 NNNN0000-WXXL050              VALUE 'WXXD050 '.   00001600
001700             88 NNNN0000-WXXL210              VALUE 'WXXD210 '.   00001700
001800             88 NNNN0000-WXXL470              VALUE 'WXXD470 '.   00001800
001900             88 NNNN0000-WXXL480              VALUE 'WXXD480 '.   00001900
002000             88 NNNN0000-WXXL551              VALUE 'WXXD550 '.   00002000
002100             88 NNNN0000-WXXL555              VALUE 'WXXD555 '.   00002100
002200             88 NNNN0000-WXXL585              VALUE 'WXXD585 '.   00002200
002300             88 NNNN0000-WXXL500              VALUE 'WXXD500 '.   00002300
002400             88 NNNN0000-WXXL510              VALUE 'WXXD510 '.   00002400
002500             88 NNNN0000-WXXL520              VALUE 'WXXD520 '.   00002500
002600             88 NNNN0000-WXXL530              VALUE 'WXXD530 '.   00002600
002700             88 NNNN0000-WXXL610              VALUE 'WXXD610 '.   00002700
002800             88 NNNN0000-WXXL680              VALUE 'WXXD680 '.   00002800
002900             88 NNNN0000-WXXL352              VALUE 'WXXD352 '.   00002900
003000             88 NNNN0000-WXXL352A             VALUE 'WXXD352A'.   00003000
003100             88 NNNN0000-WXXL240              VALUE 'WXXD240 '.   00003100
003200             88 NNNN0000-WXXD900              VALUE 'WXXD900 '.   00003200
003300             88 NNNN0000-TABLE                VALUE 'TABLE   '.   00003300
003400             88 NNNN0000-WXXL160              VALUE 'WXXD160 '.   00003400
003500             88 NNNN0000-WXXL051              VALUE 'WXXD051 '.   00003500
003600             88 NNNN0000-GLPSACCT             VALUE 'GLPSACCT'.   00003600
003700             88 NNNN0000-WXXL670              VALUE 'WXXD670 '.   00003700
003800             88 NNNN0000-VSTCLSWH             VALUE 'VSTCLSWH'.   00003800
003810             88 NNNN0000-WXXL650              VALUE 'WXXD650 '.   00003810
003820             88 NNNN0000-WXXL130              VALUE 'WXXD130 '.   00003820
003900         10 NNNN0000-KEY           PIC X(255) VALUE SPACES.       00003900
004000         10 REDEFINES NNNN0000-KEY.                               00004000
004100             15 NNNN0000-KEY-B OCCURS 255 TIMES PIC X.            00004100
004200         10 NNNN0000-KEY-LEN       PIC S9(4) COMP VALUE 0.        00004200
004300         10 NNNN0000-REC-LEN       PIC S9(4) COMP VALUE 0.        00004300
004400         10 NNNN0000-TBL           PIC S9(9) COMP VALUE 0.        00004400
004500         10 FILLER                 PIC X(26)  VALUE SPACES.       00004500
004600                                                                  00004600
004700* ----------------------------------------------------------------00004700
004800* EXIT SUBROUTINE NAMES.                                          00004800
004900* ----------------------------------------------------------------00004900
005000         10 NNNN0000-MAP-SUBR      PIC X(8)   VALUE SPACES.       00005000
005100         10 NNNN0000-IO-SUBR       PIC X(8)   VALUE SPACES.       00005100
005200             88 NNNN0000-XXXPST01-IO-SUBR     VALUE 'NNNS0099'.   00005200
005300             88 NNNN0000-WXXL050-IO-SUBR      VALUE 'NNNS0094'.   00005300
005400             88 NNNN0000-WXXL210-IO-SUBR      VALUE 'NNNS0334'.   00005400
005500             88 NNNN0000-WXXL470-IO-SUBR      VALUE 'NNNS0118'.   00005500
005600             88 NNNN0000-WXXL480-IO-SUBR      VALUE 'NNNS0250'.   00005600
005700             88 NNNN0000-WXXL520-IO-SUBR      VALUE 'NNNS0100'.   00005700
005800             88 NNNN0000-WXXL551-IO-SUBR      VALUE 'NNNS0248'.   00005800
005900             88 NNNN0000-WXXL555-IO-SUBR      VALUE 'NNNS0249'.   00005900
006000             88 NNNN0000-WXXL585-IO-SUBR      VALUE 'NNNS0337'.   00006000
006100             88 NNNN0000-WXXL500-IO-SUBR      VALUE 'NNNS0338'.   00006100
006200             88 NNNN0000-WXXL510-IO-SUBR      VALUE 'NNNS0339'.   00006200
006300             88 NNNN0000-WXXL530-IO-SUBR      VALUE 'NNNS0340'.   00006300
006400             88 NNNN0000-WXXL610-IO-SUBR      VALUE 'NNNS0347'.   00006400
006500             88 NNNN0000-WXXL680-IO-SUBR      VALUE 'NNNS0348'.   00006500
006600             88 NNNN0000-WXXL352-IO-SUBR      VALUE 'NNNS0341'.   00006600
006700             88 NNNN0000-WXXL240-IO-SUBR      VALUE 'NNNS0247'.   00006700
006800             88 NNNN0000-WXXD900-IO-SUBR      VALUE 'NNNS0380'.   00006800
006900             88 NNNN0000-TABLE-IO-SUBR        VALUE 'NNNS0342'.   00006900
007000             88 NNNN0000-WXXD160-IO-SUBR      VALUE 'NNNS0427'.   00007000
007100             88 NNNN0000-WXXL051-IO-SUBR      VALUE 'NNNS0453'.   00007100
007200             88 NNNN0000-GLPSACCT-IO-SUBR     VALUE 'NNNS1108'.   00007200
007300             88 NNNN0000-WXXL670-IO-SUBR      VALUE 'NNNS2177'.   00007300
007400             88 NNNN0000-VSTCLSWH-IO-SUBR     VALUE 'NNNS0224'.   00007400
007500         10 NNNN0000-EDIT-EXIT-SUBR PIC X(8)  VALUE SPACES.       00007500
007600         10 NNNN0000-MAP-FUNC      PIC X(2).                      00007600
007700             88 MF-INIT            VALUE 'II'.                    00007700
007800             88 MF-GET-PCB-ADDR    VALUE 'PA'.                    00007800
007900             88 MF-SET-INDEX       VALUE 'SI'.                    00007900
008000             88 MF-VIEW-TO-COL     VALUE 'VC'.                    00008000
008100             88 MF-COL-TO-VIEW     VALUE 'CV'.                    00008100
008200             88 MF-GET-DATA-TYPE   VALUE 'DT'.                    00008200
008300         10 NNNN0000-CICS-CALL-SW  PIC X(1)   VALUE SPACES.       00008300
008400             88 NNNN0000-USE-STD-IO-SUBR      VALUE ' '.          00008400
008500             88 NNNN0000-USE-CUSTOM-IO-SUBR   VALUE 'X'.          00008500
008600         10 FILLER                 PIC X(29)  VALUE SPACES.       00008600
008700                                                                  00008700
008800* ----------------------------------------------------------------00008800
008900* EXIT CONTROLS.                                                  00008900
009000* ----------------------------------------------------------------00009000
009100         10 NNNN0000-EXIT-DATA     PIC X(100) VALUE SPACES.       00009100
009200         10 REDEFINES NNNN0000-EXIT-DATA.                         00009200
009300             15 NNNN0000-EXIT-FLAGS      OCCURS 100 TIMES.        00009300
009400                 20 EXIT-FLAG      PIC X(1).                      00009400
009500                     88 EXIT-ON             VALUE ' '.            00009500
009600                     88 EXIT-OFF            VALUE 'X'.            00009600
009700         10 FILLER                 PIC X(30)  VALUE SPACES.       00009700
009800                                                                  00009800
009900* ----------------------------------------------------------------00009900
010000* EXIT CODES                                                      00010000
010100* ----------------------------------------------------------------00010100
010200         10 NNNN0000-EXIT-CODES    PIC S9(4) COMP VALUE 0.        00010200
010300             88 NO-EXIT                     VALUE 0.              00010300
010400                                                                  00010400
010500* ----------------------------------------------------------------00010500
010600* EXIT CODES FOR SPECIFIC IO CALLS.                               00010600
010700* ----------------------------------------------------------------00010700
010800             88 EXIT-OPEN-CURSOR            VALUE 1.              00010800
010900             88 EXIT-CLOSE-CURSOR           VALUE 2.              00010900
011000             88 EXIT-GET-UNIQUE-ROW         VALUE 3.              00011000
011100             88 EXIT-GET-PREV-ROW           VALUE 4.              00011100
011200             88 EXIT-GET-NEXT-ROW           VALUE 5.              00011200
011300             88 EXIT-GET-FIRST-ROW          VALUE 6.              00011300
011400             88 EXIT-GET-LAST-ROW           VALUE 7.              00011400
011500             88 EXIT-PUT-MODIFY-ROW         VALUE 8.              00011500
011600             88 EXIT-PUT-INSERT-ROW         VALUE 9.              00011600
011700             88 EXIT-PUT-PURGE-ROW          VALUE 10.             00011700
011800             88 EXIT-OPEN-FILE              VALUE 11.             00011800
011900             88 EXIT-CLOSE-FILE             VALUE 12.             00011900
012000             88 EXIT-RESET-IO               VALUE 13.             00012000
012100             88 EXIT-GET-UNIQUE-ROW-UR      VALUE 14.             00012100
012200             88 EXIT-POST-TRX-CALLBACK      VALUE 18.             00012200
012300             88 EXIT-DUMMY-IO               VALUE 19.             00012300
012400                                                                  00012400
012500* ----------------------------------------------------------------00012500
012600* EXIT CODES EXECUTED DURING A SINGLE FILE IO TRANSACTION.        00012600
012700* ----------------------------------------------------------------00012700
012800                                                                  00012800
012900*        ---------------------------------------------------      00012900
013000*        EXIT EXECUTED BEFORE/AFTER INITIALIZATION.               00013000
013100*        ---------------------------------------------------      00013100
013200             88 EXIT-BEFORE-IO-INIT         VALUE 20.             00013200
013300             88 EXIT-AFTER-IO-INIT          VALUE 21.             00013300
013400                                                                  00013400
013500*        ---------------------------------------------------      00013500
013600*        EXITS EXECUTED BEFORE/AFTER IO KEY SETUP.                00013600
013700*        ---------------------------------------------------      00013700
013800             88 EXIT-BEFORE-RECV-KEY-FLDS   VALUE 30.             00013800
013900             88 EXIT-AFTER-RECV-KEY-FLDS    VALUE 31.             00013900
014000                                                                  00014000
014100*        ---------------------------------------------------      00014100
014200*        EXITS EXECUTED BEFORE/AFTER FILE IO TRANSACTION.         00014200
014300*        ---------------------------------------------------      00014300
014400             88 EXIT-BEFORE-EXEC-IO-TRX     VALUE 40.             00014400
014500             88 EXIT-AFTER-EXEC-IO-TRX      VALUE 41.             00014500
014600                                                                  00014600
014700*        ---------------------------------------------------      00014700
014800*        EXITS EXECUTED BEFORE/AFTER LOW-LEVEL IO CALLS.          00014800
014900*        ---------------------------------------------------      00014900
015000             88 EXIT-BEFORE-IO-CALL         VALUE 50.             00015000
015100             88 EXIT-AFTER-IO-CALL          VALUE 51.             00015100
015200             88 EXIT-BEFORE-IO-RESULT-PROC  VALUE 52.             00015200
015300                                                                  00015300
015400*        ---------------------------------------------------      00015400
015500*        EXITS EXECUTED BEFORE/AFTER ROW QUALIFICATIONS.          00015500
015600*        ---------------------------------------------------      00015600
015700             88 EXIT-BEFORE-QUAL-ROW        VALUE 60.             00015700
015800             88 EXIT-AFTER-QUAL-ROW         VALUE 61.             00015800
015900                                                                  00015900
016000*        ---------------------------------------------------      00016000
016100*        EXITS EXECUTED BEFORE/AFTER DATA MOVE TO FILE VIEW.      00016100
016200*        ---------------------------------------------------      00016200
016300             88 EXIT-BEFORE-CHG-TO-NEW-VIEW VALUE 70.             00016300
016400             88 EXIT-AFTER-CHG-TO-NEW-VIEW  VALUE 71.             00016400
016500             88 EXIT-COM-STD-DATA-EDITS     VALUE 72.             00016500
016600             88 EXIT-UPD-STD-DATA-EDITS     VALUE 73.             00016600
016700             88 EXIT-ADD-STD-DATA-EDITS     VALUE 74.             00016700
016800             88 EXIT-DELETE-STD-EDITS       VALUE 75.             00016800
016900                                                                  00016900
017000*        ---------------------------------------------------      00017000
017100*        EXITS EXECUTED ON IO ERROR.                              00017100
017200*        ---------------------------------------------------      00017200
017300             88 EXIT-IO-ERROR               VALUE 80.             00017300
017400                                                                  00017400
017500*        ---------------------------------------------------      00017500
017600*        EXIT 4 SPECIAL FUNCS AFTER THE FILE IO TRANSACTION.      00017600
017700*        ---------------------------------------------------      00017700
017800             88 EXIT-DO-SPECIAL-IO-FUNCS    VALUE 90.             00017800
017900                                                                  00017900
018000         10 FILLER                      PIC X(30) VALUE SPACES.   00018000
018100                                                                  00018100
018200     05 NNNN0000-EDIT-CONTROLS.                                   00018200
018300         10 NNNN0000-EDIT-SW            PIC X(1) VALUE SPACES.    00018300
018400             88 DO-EDITS                VALUE ' '.                00018400
018500             88 DONT-DO-ANY-EDITS       VALUE 'X'.                00018500
018600         10 NNNN0000-COMMON-EDIT-SW     PIC X(1) VALUE SPACES.    00018600
018700             88 DO-COMMON-EDITS         VALUE ' '.                00018700
018800             88 DONT-DO-COMMON-EDITS    VALUE 'X'.                00018800
018900         10 NNNN0000-UPDATE-EDIT-SW     PIC X(1) VALUE SPACES.    00018900
019000             88 DO-UPDATE-EDITS         VALUE ' '.                00019000
019100             88 DONT-DO-UPDATE-EDITS    VALUE 'X'.                00019100
019200         10 NNNN0000-ADD-EDIT-SW        PIC X(1) VALUE SPACES.    00019200
019300             88 DO-ADD-EDITS            VALUE ' '.                00019300
019400             88 DONT-DO-ADD-EDITS       VALUE 'X'.                00019400
019500         10 NNNN0000-DELETE-EDIT-SW     PIC X(1) VALUE SPACES.    00019500
019600             88 DO-DELETE-EDITS         VALUE ' '.                00019600
019700             88 DONT-DO-DELETE-EDITS    VALUE 'X'.                00019700
019800         10 FILLER                      PIC X(30) VALUE SPACES.   00019800
019900                                                                  00019900
020000     05 NNNN0000-IO-CONTROLS.                                     00020000
020100         10 HOLD-REC-CMD                PIC X(1)  VALUE SPACES.   00020100
020200             88 NO-HOLD-REC                       VALUE ' '.      00020200
020300             88 HOLD-REC                          VALUE 'H'.      00020300
020400         10 QUALIFIED-SW                PIC X(1)  VALUE ' '.      00020400
020500             88 YES-QUALIFIED                     VALUE ' '.      00020500
020600             88 NO-QUALIFIED                      VALUE 'N'.      00020600
020700         10 ACCESS-MODE-SW              PIC X(1)  VALUE ' '.      00020700
020800             88 ACCESS-IO                         VALUE ' '.      00020800
020900             88 ACCESS-READ                       VALUE 'R'.      00020900
021000             88 ACCESS-EXTEND                     VALUE 'X'.      00021000
021100         10 IO-EXIT-MOD-FUNC            PIC X(1)  VALUE SPACES.   00021100
021200             88 NO-EXIT-MOD-FUNC                  VALUE ' '.      00021200
021300             88 ALWAYS-MOD-OR-INSERT              VALUE 'X'.      00021300
021400         10 IO-EXIT-GET-FUNC            PIC X(1)  VALUE SPACES.   00021400
021500             88 NO-EXIT-GET-FUNC                  VALUE ' '.      00021500
021600             88 RETURN-OK-IF-NOT-FOUND            VALUE 'X'.      00021600
021700         10 FILLER                      PIC X(25) VALUE SPACES.   00021700
021800                                                                  00021800
021900     05 NNNN0000-SPECIAL-FUNC-CNTRL.                              00021900
022000         10 NNNN0000-SPECIAL-FUNC-SW    PIC X(4)  VALUE SPACES.   00022000
022100             88 NNNN0000-NO-SPECIAL-FUNC          VALUE ' '.      00022100
022200             88 NNNN0000-DO-SPECIAL-FUNC          VALUE 'X'.      00022200
022300         10 NNNN0000-SPECIAL-FUNC       PIC X(4)  VALUE SPACES.   00022300
022400         10 FILLER                      PIC X(26) VALUE SPACES.   00022400
022500                                                                  00022500
022600     05 NNNN0000-INDEX-CONTROLS.                                  00022600
022700         10 NNNN0000-INDEX-HANDLE       PIC S9(4) COMP VALUE 0.   00022700
022800             88 NNNN0000-PRIMARY        VALUE 1.                  00022800
022900             88 NNNN0000-INDEX-02       VALUE 2.                  00022900
023000             88 NNNN0000-INDEX-03       VALUE 3.                  00023000
023100             88 NNNN0000-INDEX-04       VALUE 4.                  00023100
023200             88 NNNN0000-INDEX-05       VALUE 5.                  00023200
023300             88 NNNN0000-INDEX-06       VALUE 6.                  00023300
023400             88 NNNN0000-INDEX-07       VALUE 7.                  00023400
023500             88 NNNN0000-INDEX-08       VALUE 8.                  00023500
023600             88 NNNN0000-INDEX-09       VALUE 9.                  00023600
023700             88 NNNN0000-INDEX-010      VALUE 10.                 00023700
023800         10 FILLER                      PIC X(30) VALUE SPACES.   00023800
023900                                                                  00023900
024000     05 NNNN0000-ERROR-CONTROLS.                                  00024000
024100         10 NNNN0000-STD-ERROR-SW       PIC X(1)   VALUE SPACES.  00024100
024200             88 USE-STD-ERROR                      VALUE ' '.     00024200
024300             88 DONT-USE-STD-ERROR                 VALUE 'N'.     00024300
024400         10 FILLER                      PIC X(30)  VALUE SPACES.  00024400
024500                                                                  00024500
024600     05 FILLER                          PIC X(255) VALUE SPACES.  00024600
024700                                                                  00024700