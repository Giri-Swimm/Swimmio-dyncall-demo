000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    MMMS0291.                                         00000200
000300 AUTHOR.        NAME.                                             00000300
000400 DATE-WRITTEN.  NOV 1600.                                         00000400
001200 ENVIRONMENT DIVISION.                                            00001200
001300 DATA DIVISION.                                                   00001300
001400 WORKING-STORAGE SECTION.                                         00001400
001500* --------------------------------------------------------------- 00001500
001600* Misc working storage...                                         00001600
001700* --------------------------------------------------------------- 00001700
001800 01 WS-CNT                          PIC S9(2) COMP.               00001800
001900 01 WS-K                            PIC S9(2) COMP VALUE 14.      00001900
002000 01 WS-CURRENT-DATE-DATA            PIC X(26) VALUE SPACES.       00002000
002100 01 WS-CURRENT-TIME-STAMP.                                        00002100
002200    05  WS-CURRENT-DATE.                                          00002200
002300        10  WS-CURRENT-YEAR         PIC X(04) VALUE '1700'.       00002300
002400        10  WS-HYP-1                PIC X(01) VALUE '-'.          00002400
002500        10  WS-CURRENT-MONTH        PIC X(02) VALUE '01'.         00002500
002600        10  WS-HYP-2                PIC X(01) VALUE '-'.          00002600
002700        10  WS-CURRENT-DAY          PIC X(02) VALUE '01'.         00002700
002800        10  WS-HYP-3                PIC X(01) VALUE '-'.          00002800
002900    05  WS-CURRENT-TIME.                                          00002900
003000        10  WS-CURRENT-HOURS        PIC X(02).                    00003000
003100        10  WS-COL-1                PIC X(01) VALUE ':'.          00003100
003200        10  WS-CURRENT-MINUTE       PIC X(02).                    00003200
003300        10  WS-COL-2                PIC X(01) VALUE ':'.          00003300
003400        10  WS-CURRENT-SECOND       PIC X(02).                    00003400
003500    05  WS-MILL-SEC.                                              00003500
003600        10  WS-DOT-1                PIC X(01) VALUE '.'.          00003600
003700        10  WS-CURRENT-MILLISECONDS PIC X(06) VALUE '000000'.     00003700
003800                                                                  00003800
003900      EXEC SQL                                                    00003900
004000        INCLUDE SQLCA                                             00004000
004100      END-EXEC.                                                   00004100
004200                                                                  00004200
004300* --------------------------------------------------------------- 00004300
004400* LINKAGE SECTION                                                 00004400
004500* --------------------------------------------------------------- 00004500
004600 LINKAGE SECTION.                                                 00004600
004700 COPY XXXN001A.                                                   00004700
004800 COPY MMMC0291.                                                   00004800
004900                                                                  00004900
005000 PROCEDURE DIVISION USING                                         00005000
005100     XXXN001A                                                     00005100
005200     MMMC0291                                                     00005200
005300     .                                                            00005300
005400                                                                  00005400
005500*================================================================ 00005500
005600* Start of program main line.                                     00005600
005700*================================================================ 00005700
005800 000-MAIN.                                                        00005800
005900     PERFORM 100-INITIALIZE                                       00005900
006000                                                                  00006000
006100     EVALUATE TRUE                                                00006100
006200       WHEN MMMC0291-CVT-TM-TO-TS                                 00006200
006300         PERFORM 201-CONVERT-TM-TO-TS                             00006300
006400       WHEN MMMC0291-CVT-TS-TO-TM                                 00006400
006500         PERFORM 301-CONVERT-TS-TO-TM                             00006500
006600       WHEN OTHER                                                 00006600
006700         SET FAILURE TO TRUE                                      00006700
006800         MOVE 'MMMS0291 - Invalid MMMC0291-FUNC passed.'          00006800
006900           TO IS-RTRN-MSG-TXT                                     00006900
007000     END-EVALUATE                                                 00007000
007100                                                                  00007100
007200                                                                  00007200
007300     GOBACK                                                       00007300
007400     .                                                            00007400
007500                                                                  00007500
007600                                                                  00007600
007700*================================================================ 00007700
007800* Initialization...                                               00007800
007900*================================================================ 00007900
008000 100-INITIALIZE.                                                  00008000
008100     INITIALIZE XXXN001A                                          00008100
008200     INITIALIZE WS-CURRENT-HOURS                                  00008200
008300                WS-CURRENT-MINUTE                                 00008300
008400                WS-CURRENT-SECOND                                 00008400
008500*               MMMC0291-OUTPUT-TM                                00008500
008600*               MMMC0291-OUTPUT-TS                                00008600
008700     .                                                            00008700
008800                                                                  00008800
008900                                                                  00008900
009000 110-INITIALIZE-TM-DELIM.                                         00009000
009100     MOVE '-' TO WS-HYP-1                                         00009100
009200                 WS-HYP-2                                         00009200
009300                 WS-HYP-3                                         00009300
009400     MOVE ':' TO WS-COL-1                                         00009400
009500                 WS-COL-2                                         00009500
009600     MOVE '.' TO WS-DOT-1                                         00009600
009700     .                                                            00009700
009800                                                                  00009800
009900                                                                  00009900
010000*================================================================ 00010000
010100* CONVERT TIME TO TIMESTAMP                                       00010100
010200*================================================================ 00010200
010300 201-CONVERT-TM-TO-TS.                                            00010300
010400                                                                  00010400
010500     PERFORM VARYING WS-CNT FROM 1 BY 1 UNTIL WS-CNT > WS-K       00010500
010600       IF WS-TIME-INOUT-CONV(WS-CNT) NOT EQUAL TO SPACES          00010600
010700         MOVE WS-TIME-INOUT-CONV(WS-CNT)                          00010700
010800                                     TO  WS-CURRENT-TIME          00010800
010900         PERFORM 110-INITIALIZE-TM-DELIM                          00010900
011000         IF WS-CURRENT-HOURS <= '23' AND WS-CURRENT-MINUTE <= '59'00011000
011100                                     AND WS-CURRENT-SECOND <= '59'00011100
011200           IF WS-CURRENT-HOURS   = '  '                           00011200
011300              MOVE '00'         TO WS-CURRENT-HOURS               00011300
011400           END-IF                                                 00011400
011500           IF WS-CURRENT-MINUTE  = '  '                           00011500
011600              MOVE '00'         TO WS-CURRENT-MINUTE              00011600
011700           END-IF                                                 00011700
011800           IF WS-CURRENT-SECOND  = '  '                           00011800
011900              MOVE '00'         TO WS-CURRENT-SECOND              00011900
012000           END-IF                                                 00012000
012100                                                                  00012100
012200           MOVE WS-CURRENT-TIME-STAMP                             00012200
012300                                TO WS-CURRENT-DATE-DATA           00012300
012400           MOVE WS-CURRENT-DATE-DATA                              00012400
012500                                TO WS-TIMSTAMP-INOUT-CONV(WS-CNT) 00012500
012600         ELSE                                                     00012600
012700           SET  FAILURE TO TRUE                                   00012700
012800           MOVE 'MMMS0291 - Invalid MMMC0291-FUNC passed.'        00012800
012900             TO IS-RTRN-MSG-TXT                                   00012900
013000           STRING 'MMMS0291 - INVALID TIME. PLEASE ENTER CORRECT' 00013000
013100                  'TIME VALUE'                                    00013100
013200           DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                 00013200
013300         END-IF                                                   00013300
013400       ELSE                                                       00013400
013500         MOVE SPACES TO WS-TIMSTAMP-INOUT-CONV(WS-CNT)            00013500
013600       END-IF                                                     00013600
013700     END-PERFORM                                                  00013700
013800     .                                                            00013800
013900                                                                  00013900
014000                                                                  00014000
014100*================================================================ 00014100
014200* CONVERTING ALL TIME STAMP-X(26) TO TIME-X(8)                    00014200
014300*================================================================ 00014300
014400 301-CONVERT-TS-TO-TM.                                            00014400
014500     PERFORM VARYING WS-CNT FROM 1 BY 1 UNTIL WS-CNT > WS-K       00014500
014600       IF WS-TIMSTAMP-INOUT-CONV(WS-CNT) NOT EQUAL TO SPACES      00014600
014700         MOVE WS-TIMSTAMP-INOUT-CONV(WS-CNT)                      00014700
014800                                     TO  WS-CURRENT-TIME-STAMP    00014800
014900         PERFORM 110-INITIALIZE-TM-DELIM                          00014900
015000         IF WS-CURRENT-HOURS <= '23' AND WS-CURRENT-MINUTE <= '59'00015000
015100                                     AND WS-CURRENT-SECOND <= '59'00015100
015200           IF WS-CURRENT-HOURS   = '  '                           00015200
015300              MOVE '00'         TO WS-CURRENT-HOURS               00015300
015400           END-IF                                                 00015400
015500           IF WS-CURRENT-MINUTE  = '  '                           00015500
015600              MOVE '00'         TO WS-CURRENT-MINUTE              00015600
015700           END-IF                                                 00015700
015800           IF WS-CURRENT-SECOND  = '  '                           00015800
015900              MOVE '00'         TO WS-CURRENT-SECOND              00015900
016000           END-IF                                                 00016000
016100           MOVE WS-CURRENT-TIME TO WS-TIME-INOUT-CONV(WS-CNT)     00016100
016200         ELSE                                                     00016200
016300           SET FAILURE TO TRUE                                    00016300
016400           MOVE 'MMMS0291 - Invalid MMMC0291-FUNC passed.'        00016400
016500             TO IS-RTRN-MSG-TXT                                   00016500
016600           STRING 'MMMS0291 - INVALID TIME. PLEASE ENTER CORRECT' 00016600
016700                  'TIMESTAMP VALUE'                               00016700
016800            DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                00016800
016900         END-IF                                                   00016900
017000       ELSE                                                       00017000
017100         MOVE SPACES TO WS-TIME-INOUT-CONV(WS-CNT)                00017100
017200       END-IF                                                     00017200
017300     END-PERFORM                                                  00017300
017400     .                                                            00017400
017500                                                                  00017500
017600                                                                  00017600
