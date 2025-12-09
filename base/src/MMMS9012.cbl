000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    MMMS9012.                                         00000200
000300 AUTHOR.        Name.                                             00000300
000400 DATE-WRITTEN.  Circa 1600.                                       00000400
000500*---------------------------------------------------------------- 00000500
000600* Convert date formats to and from DB2 dates.                     00000600
001400* --------------------------------------------------------------- 00001400
001500 ENVIRONMENT DIVISION.                                            00001500
001600 DATA DIVISION.                                                   00001600
001700 WORKING-STORAGE SECTION.                                         00001700
001800* --------------------------------------------------------------- 00001800
001900* Misc working storage...                                         00001900
002000* --------------------------------------------------------------- 00002000
002100 01 WS-YYYYJJJ                          PIC 9(7) VALUE 0.         00002100
002200 01 REDEFINES WS-YYYYJJJ.                                         00002200
002300     05 WS-YYYYJJJ-YYYY                 PIC 9(4).                 00002300
002400     05 WS-YYYYJJJ-JJJ                  PIC 9(3).                 00002400
002500                                                                  00002500
002600 01 WS-MMDDYY                           PIC 9(6) VALUE 0.         00002600
002700 01 REDEFINES WS-MMDDYY.                                          00002700
002800     05 WS-MMDDYY-MM                    PIC 9(2).                 00002800
002900     05 WS-MMDDYY-DD                    PIC 9(2).                 00002900
003000     05 WS-MMDDYY-YY                    PIC 9(2).                 00003000
003100                                                                  00003100
003200 01 WS-YYMMDD                           PIC 9(6) VALUE 0.         00003200
003300 01 REDEFINES WS-YYMMDD.                                          00003300
003400     05 WS-YYMMDD-YY                    PIC 9(2).                 00003400
003500     05 WS-YYMMDD-MM                    PIC 9(2).                 00003500
003600     05 WS-YYMMDD-DD                    PIC 9(2).                 00003600
003700                                                                  00003700
003800 01 WS-MMDDYYYY                         PIC 9(8) VALUE 0.         00003800
003900 01 REDEFINES WS-MMDDYYYY.                                        00003900
004000     05 WS-MMDDYYYY-MM                  PIC 9(2).                 00004000
004100     05 WS-MMDDYYYY-DD                  PIC 9(2).                 00004100
004200     05 WS-MMDDYYYY-YYYY                PIC 9(4).                 00004200
004300                                                                  00004300
004400 01 WS-YYYYMMDD                         PIC 9(8) VALUE 0.         00004400
004500 01 REDEFINES WS-YYYYMMDD.                                        00004500
004600     05 WS-YYYYMMDD-YYYY                PIC X(4).                 00004600
004700     05 WS-YYYYMMDD-MM                  PIC X(2).                 00004700
004800     05 WS-YYYYMMDD-DD                  PIC X(2).                 00004800
004900                                                                  00004900
005000 01 WS-YYYYHMMHDD                         PIC X(10) VALUE SPACES. 00005000
005100 01 REDEFINES WS-YYYYHMMHDD.                                      00005100
005200     05 WS-YYYYHMMHDD-YYYY                PIC 9(4).               00005200
005300     05 WS-YYYYHMMHDD-H1                  PIC X(1).               00005300
005400     05 WS-YYYYHMMHDD-MM                  PIC 9(2).               00005400
005500     05 WS-YYYYHMMHDD-H2                  PIC X(1).               00005500
005600     05 WS-YYYYHMMHDD-DD                  PIC 9(2).               00005600
005700                                                                  00005700
005800  01 WS-JULIAN-CONV-TABLE.                                        00005800
005900     03 WS-JAN-ENTRY                PIC X(10) VALUE '001 031 01'. 00005900
006000     03 WS-FEB-ENTRY                PIC X(10) VALUE '032 059 02'. 00006000
006100     03 WS-MAR-ENTRY                PIC X(10) VALUE '060 090 03'. 00006100
006200     03 WS-APR-ENTRY                PIC X(10) VALUE '091 120 04'. 00006200
006300     03 WS-MAY-ENTRY                PIC X(10) VALUE '121 151 05'. 00006300
006400     03 WS-JUN-ENTRY                PIC X(10) VALUE '152 181 06'. 00006400
006500     03 WS-JUL-ENTRY                PIC X(10) VALUE '182 212 07'. 00006500
006600     03 WS-AUG-ENTRY                PIC X(10) VALUE '213 243 08'. 00006600
006700     03 WS-SEP-ENTRY                PIC X(10) VALUE '244 273 09'. 00006700
006800     03 WS-OCT-ENTRY                PIC X(10) VALUE '274 304 10'. 00006800
006900     03 WS-NOV-ENTRY                PIC X(10) VALUE '305 334 11'. 00006900
007000     03 WS-DEC-ENTRY                PIC X(10) VALUE '335 365 12'. 00007000
007100                                                                  00007100
007200  01 WS-JULIAN-WORK-TABLE REDEFINES WS-JULIAN-CONV-TABLE.         00007200
007300     03 WS-JUL-SRCH-ENTRY            OCCURS 12 TIMES.             00007300
007400        05 WS-JUL-MNTH-STRT          PIC 9(3).                    00007400
007500        05 FILLER                    PIC X(1).                    00007500
007600        05 WS-JUL-MNTH-END           PIC 9(3).                    00007600
007700        05 FILLER                    PIC X(1).                    00007700
007800        05 WS-JUL-MNTH-NUM           PIC 9(2).                    00007800
007900                                                                  00007900
008000  01 WS-CNTR                            PIC S9(4) COMP VALUE +0.  00008000
008100  01 WS-REMAINDER                       PIC S9(4) COMP VALUE +0.  00008100
008200  01 WS-TEMP                            PIC 9(2) VALUE 0.         00008200
008300  01 WS-FOUR                            PIC 9(3) VALUE 4.         00008300
008400  01 WS-HUNDRED                         PIC 9(4) VALUE 100.       00008400
008500  01 WS-FOUR-HUND                       PIC 9(4) VALUE 400.       00008500
008600  01 WS-LEAP-FLAG                       PIC X(1) VALUE ' '.       00008600
008700     88 WS-LEAP-YEAR                             VALUE 'X'.       00008700
008800     88 WS-ORDY-YEAR                             VALUE ' '.       00008800
008900                                                                  00008900
009000 COPY MMMK001B.                                                   00009000
009100                                                                  00009100
009200 LINKAGE SECTION.                                                 00009200
009300 COPY XXXN001A.                                                   00009300
009400 COPY MMMC9012.                                                   00009400
009500                                                                  00009500
009600 01 WS-DATE                             PIC X(10) VALUE SPACES.   00009600
009700 01 REDEFINES WS-DATE.                                            00009700
009800     05 WS-DATE-N7                      PIC S9(7).                00009800
009900 01 REDEFINES WS-DATE.                                            00009900
010000     05 WS-DATE-N8                      PIC 9(8).                 00010000
010100 01 REDEFINES WS-DATE.                                            00010100
010200     05 WS-DATE-N6                      PIC 9(6).                 00010200
010300 01 REDEFINES WS-DATE.                                            00010300
010400     05 WS-DATE-P7                      PIC 9(7) COMP-3.          00010400
010500                                                                  00010500
010600 01 WS-MM-DD-YYYY                       PIC X(10) VALUE SPACES.   00010600
010700 01 REDEFINES WS-MM-DD-YYYY.                                      00010700
010800     05 WS-MM-DD-YYYY-MM                PIC 9(2).                 00010800
010900     05 WS-MM-DD-YYYY-S1                PIC X(1).                 00010900
011000     05 WS-MM-DD-YYYY-DD                PIC 9(2).                 00011000
011100     05 WS-MM-DD-YYYY-S2                PIC X(1).                 00011100
011200     05 WS-MM-DD-YYYY-YYYY              PIC 9(4).                 00011200
011300     05 REDEFINES WS-MM-DD-YYYY-YYYY.                             00011300
011400         10 WS-MM-DD-YYYY-CC            PIC X(2).                 00011400
011500         10 WS-MM-DD-YYYY-YY            PIC X(2).                 00011500
011600                                                                  00011600
011700 PROCEDURE DIVISION USING                                         00011700
011800     XXXN001A                                                     00011800
011900     MMMC9012                                                     00011900
012000     WS-DATE                                                      00012000
012100     WS-MM-DD-YYYY                                                00012100
012200     .                                                            00012200
012300                                                                  00012300
012400*=================================================================00012400
012500* Start of program main line.                                     00012500
012600*=================================================================00012600
012700 000-MAIN.                                                        00012700
012800     PERFORM 100-INITIALIZE                                       00012800
012900     PERFORM 200-CHECK-INPUTS                                     00012900
013000                                                                  00013000
013100     EVALUATE TRUE                                                00013100
013200       WHEN NOT SUCCESS                                           00013200
013300         CONTINUE                                                 00013300
013400                                                                  00013400
013500       WHEN MMMC9012-CONV-TO-DB2                                  00013500
013600         PERFORM 300-CONV-TO-DB2                                  00013600
013700                                                                  00013700
013800       WHEN MMMC9012-CONV-FROM-DB2                                00013800
013900         PERFORM 400-CONV-FROM-DB2                                00013900
014000                                                                  00014000
014100       WHEN OTHER                                                 00014100
014200         SET FAILURE TO TRUE                                      00014200
014300         MOVE 'MMMS9012 - Invalid date conversion function.'      00014300
014400           TO IS-RTRN-MSG-TXT                                     00014400
014500     END-EVALUATE                                                 00014500
014600                                                                  00014600
014700     GOBACK                                                       00014700
014800     .                                                            00014800
014900                                                                  00014900
015000                                                                  00015000
015100*================================================================ 00015100
015200* Initialization...                                               00015200
015300*================================================================ 00015300
015400 100-INITIALIZE.                                                  00015400
015500     INITIALIZE XXXN001A                                          00015500
015600     .                                                            00015600
015700                                                                  00015700
015800                                                                  00015800
015900*================================================================ 00015900
016000* Make sure we have reasonable data coming in...                  00016000
016100*================================================================ 00016100
016200 200-CHECK-INPUTS.                                                00016200
016300     EVALUATE TRUE                                                00016300
016400       WHEN MMMC9012-CONV-TO-DB2                                  00016400
016500       AND  MMMC9012-PIC-N8-MMDDYYYY                              00016500
016600         IF WS-DATE-N8 NOT NUMERIC                                00016600
016700           MOVE 0 TO WS-DATE-N8                                   00016700
016800         END-IF                                                   00016800
016900       WHEN MMMC9012-CONV-TO-DB2                                  00016900
017000       AND  MMMC9012-PIC-N8-YYYYMMDD                              00017000
017100         IF WS-DATE-N8 NOT NUMERIC                                00017100
017200           MOVE 0 TO WS-DATE-N8                                   00017200
017300         END-IF                                                   00017300
017400       WHEN MMMC9012-CONV-TO-DB2                                  00017400
017500       AND  MMMC9012-PIC-N6-MMDDYY                                00017500
017600         IF WS-DATE-N6 NOT NUMERIC                                00017600
017700           MOVE 0 TO WS-DATE-N6                                   00017700
017800         END-IF                                                   00017800
017900       WHEN MMMC9012-CONV-TO-DB2                                  00017900
018000       AND  MMMC9012-PIC-N6-YYMMDD                                00018000
018100         IF WS-DATE-N6 NOT NUMERIC                                00018100
018200           MOVE 0 TO WS-DATE-N6                                   00018200
018300         END-IF                                                   00018300
018400       WHEN MMMC9012-CONV-TO-DB2                                  00018400
018500       AND  MMMC9012-PIC-P7-MMDDYY                                00018500
018600         IF WS-DATE-P7 NOT NUMERIC                                00018600
018700           MOVE 0 TO WS-DATE-P7                                   00018700
018800         END-IF                                                   00018800
018900       WHEN MMMC9012-CONV-TO-DB2                                  00018900
019000       AND  MMMC9012-PIC-P7-YYMMDD                                00019000
019100         IF WS-DATE-P7 NOT NUMERIC                                00019100
019200           MOVE 0 TO WS-DATE-P7                                   00019200
019300         END-IF                                                   00019300
019400       WHEN MMMC9012-CONV-TO-DB2                                  00019400
019500       AND  MMMC9012-PIC-X10-YYYYHMMHDD                           00019500
019600           CONTINUE                                               00019600
019700       WHEN MMMC9012-CONV-TO-DB2                                  00019700
019800       AND  MMMC9012-PIC-N7-YYYYJJJ                               00019800
019900       IF  WS-DATE-N7 NOT NUMERIC                                 00019900
020000          MOVE 0  TO WS-DATE-N7                                   00020000
020100       END-IF                                                     00020100
020200     END-EVALUATE                                                 00020200
020300                                                                  00020300
020400     IF NOT SUCCESS                                               00020400
020500       MOVE 'MMMS9012 - Invalid input date format.'               00020500
020600         TO IS-RTRN-MSG-TXT                                       00020600
020700     END-IF                                                       00020700
020800     .                                                            00020800
020900                                                                  00020900
021000                                                                  00021000
021100*================================================================ 00021100
021200* Convert the date to the DB2 format...                           00021200
021300*================================================================ 00021300
021400 300-CONV-TO-DB2.                                                 00021400
021500     EVALUATE TRUE                                                00021500
021600       WHEN MMMC9012-PIC-N8-MMDDYYYY                              00021600
021700         MOVE WS-DATE-N8       TO WS-MMDDYYYY                     00021700
021800         MOVE WS-MMDDYYYY-MM   TO WS-MM-DD-YYYY-MM                00021800
021900         MOVE WS-MMDDYYYY-DD   TO WS-MM-DD-YYYY-DD                00021900
022000         MOVE WS-MMDDYYYY-YYYY TO WS-MM-DD-YYYY-YYYY              00022000
022100       WHEN MMMC9012-PIC-N8-YYYYMMDD                              00022100
022200         MOVE WS-DATE-N8       TO WS-YYYYMMDD                     00022200
022300         MOVE WS-YYYYMMDD-MM   TO WS-MM-DD-YYYY-MM                00022300
022400         MOVE WS-YYYYMMDD-DD   TO WS-MM-DD-YYYY-DD                00022400
022500         MOVE WS-YYYYMMDD-YYYY TO WS-MM-DD-YYYY-YYYY              00022500
022600       WHEN MMMC9012-PIC-N6-MMDDYY                                00022600
022700         MOVE WS-DATE-N6       TO WS-MMDDYY                       00022700
022800         MOVE WS-MMDDYY-MM     TO WS-MM-DD-YYYY-MM                00022800
022900         MOVE WS-MMDDYY-DD     TO WS-MM-DD-YYYY-DD                00022900
023000         MOVE WS-MMDDYY-YY     TO WS-MM-DD-YYYY-YY                00023000
023100         IF WS-MMDDYY = 0                                         00023100
023200             MOVE 00 TO WS-MM-DD-YYYY-CC                          00023200
023300         ELSE                                                     00023300
023400*            IF WS-MMDDYY-YY < 20                                 00023400
023410             IF WS-MMDDYY-YY < 50                                 00023410
023500               MOVE 20 TO WS-MM-DD-YYYY-CC                        00023500
023600             ELSE                                                 00023600
023700               MOVE 19 TO WS-MM-DD-YYYY-CC                        00023700
023800             END-IF                                               00023800
023900         END-IF                                                   00023900
024000       WHEN MMMC9012-PIC-N6-YYMMDD                                00024000
024100         MOVE WS-DATE-N6       TO WS-YYMMDD                       00024100
024200         MOVE WS-YYMMDD-MM     TO WS-MM-DD-YYYY-MM                00024200
024300         MOVE WS-YYMMDD-DD     TO WS-MM-DD-YYYY-DD                00024300
024400         MOVE WS-YYMMDD-YY     TO WS-MM-DD-YYYY-YY                00024400
024500         IF WS-YYMMDD = 0                                         00024500
024600            MOVE 00 TO WS-MM-DD-YYYY-CC                           00024600
024700         ELSE                                                     00024700
024800*            IF WS-YYMMDD-YY < 20                                 00024800
024810             IF WS-YYMMDD-YY < 50                                 00024810
024900               MOVE 20 TO WS-MM-DD-YYYY-CC                        00024900
025000             ELSE                                                 00025000
025100               MOVE 19 TO WS-MM-DD-YYYY-CC                        00025100
025200             END-IF                                               00025200
025300         END-IF                                                   00025300
025400       WHEN MMMC9012-PIC-P7-MMDDYY                                00025400
025500         MOVE WS-DATE-P7       TO WS-MMDDYY                       00025500
025600         MOVE WS-MMDDYY-MM     TO WS-MM-DD-YYYY-MM                00025600
025700         MOVE WS-MMDDYY-DD     TO WS-MM-DD-YYYY-DD                00025700
025800         MOVE WS-MMDDYY-YY     TO WS-MM-DD-YYYY-YY                00025800
025900         IF WS-MMDDYY = 0                                         00025900
026000             MOVE 00 TO WS-MM-DD-YYYY-CC                          00026000
026100         ELSE                                                     00026100
026200*            IF WS-MMDDYY-YY < 40                                 00026200
026210             IF WS-MMDDYY-YY < 50                                 00026210
026300               MOVE 20 TO WS-MM-DD-YYYY-CC                        00026300
026400             ELSE                                                 00026400
026500               MOVE 19 TO WS-MM-DD-YYYY-CC                        00026500
026600             END-IF                                               00026600
026700         END-IF                                                   00026700
026800       WHEN MMMC9012-PIC-P7-YYMMDD                                00026800
026900         MOVE WS-DATE-P7       TO WS-YYMMDD                       00026900
027000         MOVE WS-YYMMDD-MM     TO WS-MM-DD-YYYY-MM                00027000
027100         MOVE WS-YYMMDD-DD     TO WS-MM-DD-YYYY-DD                00027100
027200         MOVE WS-YYMMDD-YY     TO WS-MM-DD-YYYY-YY                00027200
027300         IF WS-YYMMDD = 0                                         00027300
027400            MOVE 00 TO WS-MM-DD-YYYY-CC                           00027400
027500         ELSE                                                     00027500
027600*            IF WS-YYMMDD-YY < 20                                 00027600
027610             IF WS-YYMMDD-YY < 50                                 00027610
027700               MOVE 20 TO WS-MM-DD-YYYY-CC                        00027700
027800             ELSE                                                 00027800
027900               MOVE 19 TO WS-MM-DD-YYYY-CC                        00027900
028000             END-IF                                               00028000
028100         END-IF                                                   00028100
028200       WHEN MMMC9012-PIC-X10-YYYYHMMHDD                           00028200
028300         MOVE WS-DATE                TO   WS-YYYYHMMHDD           00028300
028400         MOVE WS-YYYYHMMHDD-MM       TO   WS-MM-DD-YYYY-MM        00028400
028500         MOVE WS-YYYYHMMHDD-DD       TO   WS-MM-DD-YYYY-DD        00028500
028600         MOVE WS-YYYYHMMHDD-YYYY     TO   WS-MM-DD-YYYY-YYYY      00028600
028700       WHEN MMMC9012-PIC-N7-YYYYJJJ                               00028700
028800         IF WS-DATE-N7 NOT EQUAL ZERO                             00028800
028900            MOVE WS-DATE-N7           TO WS-YYYYJJJ               00028900
029000            PERFORM 900-JULIAN-TO-DB2-CONV                        00029000
029100         ELSE                                                     00029100
029200            MOVE K-DEF-DT             TO WS-MM-DD-YYYY            00029200
029300         END-IF                                                   00029300
029400       WHEN OTHER                                                 00029400
029500         SET FAILURE TO TRUE                                      00029500
029600         MOVE 'MMMS9012 - Invalid date conversion type.'          00029600
029700           TO IS-RTRN-MSG-TXT                                     00029700
029800     END-EVALUATE                                                 00029800
029900                                                                  00029900
030000     MOVE '/' TO  WS-MM-DD-YYYY-S1                                00030000
030100     MOVE '/' TO  WS-MM-DD-YYYY-S2                                00030100
030200     .                                                            00030200
030300                                                                  00030300
030400                                                                  00030400
030500*================================================================ 00030500
030600* Convert the date from the DB2 format...                         00030600
030700*================================================================ 00030700
030800 400-CONV-FROM-DB2.                                               00030800
030900     EVALUATE TRUE                                                00030900
031000       WHEN WS-MM-DD-YYYY = SPACES                                00031000
031100        MOVE K-ZERO-DT TO WS-MM-DD-YYYY                           00031100
031200       WHEN WS-MM-DD-YYYY = K-DEF-DT                              00031200
031300        MOVE K-ZERO-DT TO WS-MM-DD-YYYY                           00031300
031400     END-EVALUATE                                                 00031400
031500                                                                  00031500
031600     EVALUATE TRUE                                                00031600
031700       WHEN MMMC9012-PIC-N8-MMDDYYYY                              00031700
031800         MOVE WS-MM-DD-YYYY-MM   TO WS-MMDDYYYY-MM                00031800
031900         MOVE WS-MM-DD-YYYY-DD   TO WS-MMDDYYYY-DD                00031900
032000         MOVE WS-MM-DD-YYYY-YYYY TO WS-MMDDYYYY-YYYY              00032000
032100         MOVE WS-MMDDYYYY        TO WS-DATE-N8                    00032100
032200       WHEN MMMC9012-PIC-N8-YYYYMMDD                              00032200
032300         MOVE WS-MM-DD-YYYY-MM   TO WS-YYYYMMDD-MM                00032300
032400         MOVE WS-MM-DD-YYYY-DD   TO WS-YYYYMMDD-DD                00032400
032500         MOVE WS-MM-DD-YYYY-YYYY TO WS-YYYYMMDD-YYYY              00032500
032600         MOVE WS-YYYYMMDD        TO WS-DATE-N8                    00032600
032700       WHEN MMMC9012-PIC-N6-MMDDYY                                00032700
032800         MOVE WS-MM-DD-YYYY-MM   TO WS-MMDDYY-MM                  00032800
032900         MOVE WS-MM-DD-YYYY-DD   TO WS-MMDDYY-DD                  00032900
033000         MOVE WS-MM-DD-YYYY-YY   TO WS-MMDDYY-YY                  00033000
033100         MOVE WS-MMDDYY          TO WS-DATE-N6                    00033100
033200       WHEN MMMC9012-PIC-N6-YYMMDD                                00033200
033300         MOVE WS-MM-DD-YYYY-MM   TO WS-YYMMDD-MM                  00033300
033400         MOVE WS-MM-DD-YYYY-DD   TO WS-YYMMDD-DD                  00033400
033500         MOVE WS-MM-DD-YYYY-YY   TO WS-YYMMDD-YY                  00033500
033600         MOVE WS-YYMMDD          TO WS-DATE-N6                    00033600
033700       WHEN MMMC9012-PIC-P7-MMDDYY                                00033700
033800         MOVE WS-MM-DD-YYYY-MM   TO WS-MMDDYY-MM                  00033800
033900         MOVE WS-MM-DD-YYYY-DD   TO WS-MMDDYY-DD                  00033900
034000         MOVE WS-MM-DD-YYYY-YY   TO WS-MMDDYY-YY                  00034000
034100         MOVE WS-MMDDYY          TO WS-DATE-P7                    00034100
034200       WHEN MMMC9012-PIC-P7-YYMMDD                                00034200
034300         MOVE WS-MM-DD-YYYY-MM   TO WS-YYMMDD-MM                  00034300
034400         MOVE WS-MM-DD-YYYY-DD   TO WS-YYMMDD-DD                  00034400
034500         MOVE WS-MM-DD-YYYY-YY   TO WS-YYMMDD-YY                  00034500
034600         MOVE WS-YYMMDD          TO WS-DATE-P7                    00034600
034700       WHEN MMMC9012-PIC-X10-YYYYHMMHDD                           00034700
034800         MOVE WS-MM-DD-YYYY-MM   TO WS-YYYYHMMHDD-MM              00034800
034900         MOVE WS-MM-DD-YYYY-DD   TO WS-YYYYHMMHDD-DD              00034900
035000         MOVE WS-MM-DD-YYYY-YYYY TO WS-YYYYHMMHDD-YYYY            00035000
035100         MOVE '-'                TO WS-YYYYHMMHDD-H1              00035100
035200         MOVE '-'                TO WS-YYYYHMMHDD-H2              00035200
035300         MOVE WS-YYYYHMMHDD    TO WS-DATE                         00035300
035400       WHEN MMMC9012-PIC-N7-YYYYJJJ                               00035400
035500         PERFORM 950-DB2-TO-JULIAN-CONV                           00035500
035600       WHEN OTHER                                                 00035600
035700         SET FAILURE TO TRUE                                      00035700
035800         MOVE 'MMMS9012 - Invalid date conversion type.'          00035800
035900           TO IS-RTRN-MSG-TXT                                     00035900
036000     END-EVALUATE                                                 00036000
036100     .                                                            00036100
036200                                                                  00036200
036300                                                                  00036300
036400 900-JULIAN-TO-DB2-CONV.                                          00036400
036500     INITIALIZE WS-TEMP WS-MM-DD-YYYY WS-CNTR                     00036500
036600                                                                  00036600
036700     IF WS-YYYYJJJ-JJJ IS >= 1 AND WS-YYYYJJJ-JJJ  <= 31          00036700
036800        MOVE WS-YYYYJJJ-JJJ           TO WS-MM-DD-YYYY-DD         00036800
036900        MOVE 1                        TO WS-MM-DD-YYYY-MM         00036900
037000        MOVE WS-YYYYJJJ-YYYY          TO WS-MM-DD-YYYY-YYYY       00037000
037100     ELSE IF                                                      00037100
037200            WS-YYYYJJJ-JJJ IS >= 32 AND WS-YYYYJJJ-JJJ  <= 59     00037200
037300            SUBTRACT 31 FROM WS-YYYYJJJ-JJJ                       00037300
037400                        GIVING WS-MM-DD-YYYY-DD                   00037400
037500            MOVE 2                    TO WS-MM-DD-YYYY-MM         00037500
037600            MOVE WS-YYYYJJJ-YYYY      TO WS-MM-DD-YYYY-YYYY       00037600
037700     ELSE                                                         00037700
037800       PERFORM 990-LEAP-YEAR-FIND                                 00037800
037900       IF WS-ORDY-YEAR                                            00037900
038000          PERFORM VARYING WS-CNTR FROM +1 BY +1 UNTIL WS-CNTR > 1200038000
038100           IF WS-YYYYJJJ-JJJ IS >= WS-JUL-MNTH-STRT(WS-CNTR)      00038100
038200              AND WS-YYYYJJJ-JJJ IS <= WS-JUL-MNTH-END (WS-CNTR)  00038200
038300              MOVE WS-JUL-MNTH-NUM(WS-CNTR)                       00038300
038400                                      TO WS-MM-DD-YYYY-MM         00038400
038500              SUBTRACT WS-JUL-MNTH-STRT(WS-CNTR)                  00038500
038600                                  FROM WS-YYYYJJJ-JJJ             00038600
038700                                  GIVING WS-TEMP                  00038700
038800              ADD 1                   TO WS-TEMP                  00038800
038900              MOVE WS-TEMP            TO WS-MM-DD-YYYY-DD         00038900
039000              MOVE WS-YYYYJJJ-YYYY    TO WS-MM-DD-YYYY-YYYY       00039000
039100              MOVE 13                 TO WS-CNTR                  00039100
039200           END-IF                                                 00039200
039300          END-PERFORM                                             00039300
039400       ELSE                                                       00039400
039500          PERFORM VARYING WS-CNTR FROM +1 BY +1 UNTIL WS-CNTR > 1200039500
039600           IF WS-YYYYJJJ-JJJ IS >= WS-JUL-MNTH-STRT(WS-CNTR) + 1  00039600
039700           AND WS-YYYYJJJ-JJJ IS <= WS-JUL-MNTH-END (WS-CNTR) + 1 00039700
039800              MOVE WS-JUL-MNTH-NUM(WS-CNTR)                       00039800
039900                                      TO WS-MM-DD-YYYY-MM         00039900
040000              SUBTRACT WS-JUL-MNTH-STRT(WS-CNTR)                  00040000
040100                                  FROM WS-YYYYJJJ-JJJ             00040100
040200                                  GIVING WS-TEMP                  00040200
040300*             ADD 1                   TO WS-TEMP                  00040300
040400              MOVE WS-TEMP            TO WS-MM-DD-YYYY-DD         00040400
040500              MOVE WS-YYYYJJJ-YYYY    TO WS-MM-DD-YYYY-YYYY       00040500
040600              MOVE 13                 TO WS-CNTR                  00040600
040700           END-IF                                                 00040700
040800          END-PERFORM                                             00040800
040900* MFR Deals and Cost leap year conversion change start            00040900
041000         IF WS-LEAP-YEAR AND WS-YYYYJJJ-JJJ = 60                  00041000
041100           ADD 1         TO WS-MM-DD-YYYY-DD                      00041100
041200         END-IF                                                   00041200
041300* MFR Deals and Cost leap year conversion change end              00041300
041400       END-IF                                                     00041400
041500     END-IF                                                       00041500
041600     .                                                            00041600
041700                                                                  00041700
041800                                                                  00041800
041900 950-DB2-TO-JULIAN-CONV.                                          00041900
042000     INITIALIZE WS-TEMP WS-YYYYJJJ WS-CNTR                        00042000
042100                                                                  00042100
042200     MOVE WS-MM-DD-YYYY-YYYY          TO WS-YYYYJJJ-YYYY          00042200
042300                                                                  00042300
042400     IF WS-MM-DD-YYYY-MM = 1                                      00042400
042500        MOVE WS-MM-DD-YYYY-DD         TO WS-YYYYJJJ-JJJ           00042500
042600     ELSE IF                                                      00042600
042700            WS-MM-DD-YYYY-MM = 2                                  00042700
042800            ADD 31 TO WS-MM-DD-YYYY-DD                            00042800
042900                        GIVING WS-YYYYJJJ-JJJ                     00042900
043000     ELSE                                                         00043000
043100       PERFORM 990-LEAP-YEAR-FIND                                 00043100
043200       PERFORM VARYING WS-CNTR FROM +3 BY +1 UNTIL WS-CNTR > 12   00043200
043300         IF WS-MM-DD-YYYY-MM = WS-JUL-MNTH-NUM(WS-CNTR)           00043300
043400            ADD WS-JUL-MNTH-STRT(WS-CNTR)                         00043400
043500                                      TO WS-MM-DD-YYYY-DD         00043500
043600                                  GIVING WS-YYYYJJJ-JJJ           00043600
043700            IF WS-ORDY-YEAR                                       00043700
043800              SUBTRACT 1 FROM WS-YYYYJJJ-JJJ                      00043800
043900                                  GIVING WS-YYYYJJJ-JJJ           00043900
044000            END-IF                                                00044000
044100            MOVE 13                   TO WS-CNTR                  00044100
044200         END-IF                                                   00044200
044300       END-PERFORM                                                00044300
044400       END-IF                                                     00044400
044500     END-IF                                                       00044500
044600*MFR - deals and cost                                             00044600
044700*/Added below line to move working str to linkage section variable00044700
044800     MOVE WS-YYYYJJJ TO WS-DATE                                   00044800
044900*MFR - deals and cost                                             00044900
045000                                                                  00045000
045100     .                                                            00045100
045200                                                                  00045200
045300                                                                  00045300
045400 990-LEAP-YEAR-FIND.                                              00045400
045500                                                                  00045500
045600     DIVIDE WS-YYYYJJJ-YYYY  BY WS-FOUR GIVING WS-TEMP            00045600
045700     REMAINDER WS-REMAINDER                                       00045700
045800     IF WS-REMAINDER EQUAL            TO ZERO                     00045800
045900        DIVIDE WS-YYYYJJJ-YYYY  BY WS-HUNDRED   GIVING WS-TEMP    00045900
046000        REMAINDER WS-REMAINDER                                    00046000
046100        IF WS-REMAINDER EQUAL         TO ZERO                     00046100
046200           DIVIDE WS-YYYYJJJ-YYYY  BY WS-FOUR-HUND  GIVING WS-TEMP00046200
046300           REMAINDER WS-REMAINDER                                 00046300
046400           IF WS-REMAINDER EQUAL      TO ZERO                     00046400
046500              SET WS-LEAP-YEAR        TO TRUE                     00046500
046600           ELSE                                                   00046600
046700              SET WS-ORDY-YEAR        TO TRUE                     00046700
046800           END-IF                                                 00046800
046900        ELSE                                                      00046900
047000          SET WS-LEAP-YEAR            TO TRUE                     00047000
047100        END-IF                                                    00047100
047200     ELSE                                                         00047200
047300        SET WS-ORDY-YEAR              TO TRUE                     00047300
047400     END-IF                                                       00047400
047500     .                                                            00047500
047600                                                                  00047600
047700                                                                  00047700