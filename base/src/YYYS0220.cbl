000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    YYYS0220.                                         00000200
000300 AUTHOR.        NAME.                                             00000300
000400 DATE-WRITTEN.  Aug, 1600.                                        00000400
000500 DATE-COMPILED.                                                   00000500
001500 ENVIRONMENT DIVISION.                                            00001500
001600 CONFIGURATION SECTION.                                           00001600
001700 DATA DIVISION.                                                   00001700
001800 WORKING-STORAGE SECTION.                                         00001800
001900                                                                  00001900
002000* ========================< MISC STUFF >==========================00002000
002100* Misc working storage variables go here.                         00002100
002200* ================================================================00002200
002300 01 WS-ENV-SWITCH                      PIC X(1) VALUE SPACES.     00002300
002400     88 WS-PROD                                 VALUE 'P'.        00002400
002500     88 WS-TEST                                 VALUE 'T'.        00002500
002600     88 WS-TST6                                 VALUE '6'.        00002600
002700     88 WS-TST5                                 VALUE '5'.        00002700
002800     88 WS-TST4                                 VALUE '4'.        00002800
002900     88 WS-TST3                                 VALUE '3'.        00002900
003000                                                                  00003000
003100 01 WS-YYYC0220-CURR-CON               PIC X(1) VALUE SPACES.     00003100
003200     88 WS-DEFAULT-CON                          VALUE ' '.        00003200
003300     88 WS-DB2-CON                              VALUE 'D'.        00003300
003400     88 WS-ORACLE-CON                           VALUE '0'.        00003400
003500                                                                  00003500
003600 01 WS-SQLCODE                         PIC ----9.                 00003600
003700                                                                  00003700
003800 01 WS-STATS.                                                     00003800
003900     05 WS-TOT-REQS                    PIC S9(9)  COMP VALUE 0.   00003900
004000     05 WS-DB2-REQS                    PIC S9(9)  COMP VALUE 0.   00004000
004100     05 WS-OCL-REQS                    PIC S9(9)  COMP VALUE 0.   00004100
004200     05 WS-OVR-REQS                    PIC S9(9)  COMP VALUE 0.   00004200
004300     05 WS-CON-SWITCHES                PIC S9(9)  COMP VALUE 0.   00004300
004400     05 WS-OVR-SWITCHES                PIC S9(9)  COMP VALUE 0.   00004400
004500                                                                  00004500
004600* ========================< MISC STUFF >==========================00004600
004700* DB2 stuff...                                                    00004700
004800* ================================================================00004800
004900     EXEC SQL                                                     00004900
005000         INCLUDE SQLCA                                            00005000
005100     END-EXEC.                                                    00005100
005200                                                                  00005200
005300 LINKAGE SECTION.                                                 00005300
005400 COPY XXXN001A.                                                   00005400
005500 COPY YYYC0220.                                                   00005500
005600                                                                  00005600
005700 PROCEDURE DIVISION USING                                         00005700
005800     XXXN001A                                                     00005800
005900     YYYC0220                                                     00005900
006000     .                                                            00006000
006100                                                                  00006100
006200* ================================================================00006200
006300* Main program ...                                                00006300
006400* ================================================================00006400
006500 0000-EXIT-DISPATCHER.                                            00006500
006600     PERFORM 100-INITIALIZATION                                   00006600
006700                                                                  00006700
006800     EVALUATE TRUE                                                00006800
006900       WHEN YYYC0220-GET-CURR-CON                                 00006900
007000         PERFORM 200-GET-CURR-CON                                 00007000
007100       WHEN YYYC0220-SET-DB2-CON                                  00007100
007200         PERFORM 300-SET-DB2-CON                                  00007200
007300       WHEN YYYC0220-SET-ORACLE-CON                               00007300
007400         PERFORM 400-SET-ORACLE-CON                               00007400
007500       WHEN YYYC0220-GET-STATS                                    00007500
007600         PERFORM 500-GET-STATS                                    00007600
007700       WHEN YYYC0220-SET-STATS                                    00007700
007800         PERFORM 600-SET-STATS                                    00007800
007900       WHEN YYYC0220-SET-OVERRIDE-CON                             00007900
008000         PERFORM 700-SET-OVERRIDE-CON                             00008000
008100       WHEN OTHER                                                 00008100
008200         SET  FAILURE TO TRUE                                     00008200
008300         MOVE 'YYYS0220 - Function not recognized!'               00008300
008400           TO IS-RTRN-MSG-TXT                                     00008400
008500     END-EVALUATE                                                 00008500
008600                                                                  00008600
008700     GOBACK                                                       00008700
008800     .                                                            00008800
008900                                                                  00008900
009000                                                                  00009000
009100* ================================================================00009100
009200* Initialize of course...                                         00009200
009300* ================================================================00009300
009400 100-INITIALIZATION.                                              00009400
009500     INITIALIZE XXXN001A                                          00009500
009600                                                                  00009600
009700*    SET WS-TEST TO TRUE                                          00009700
009800*    SET WS-TST3 TO TRUE                                          00009800
009900     SET WS-PROD TO TRUE                                          00009900
010000     .                                                            00010000
010100                                                                  00010100
010200                                                                  00010200
010300* ================================================================00010300
010400* Return the current connection...                                00010400
010500* ================================================================00010500
010600 200-GET-CURR-CON.                                                00010600
010700     MOVE WS-YYYC0220-CURR-CON TO YYYC0220-CURR-CON               00010700
010800     .                                                            00010800
010900                                                                  00010900
011000                                                                  00011000
011100* ================================================================00011100
011200* Set the connection to DB2 (if not already)...                   00011200
011300* ================================================================00011300
011400 300-SET-DB2-CON.                                                 00011400
011500     ADD 1 TO WS-TOT-REQS                                         00011500
011600     ADD 1 TO WS-DB2-REQS                                         00011600
011700                                                                  00011700
011800     IF WS-ORACLE-CON                                             00011800
011900     OR WS-DEFAULT-CON                                            00011900
012000       PERFORM 310-DO-SET-DB2-CON                                 00012000
012100     END-IF                                                       00012100
012200                                                                  00012200
012300     SET WS-DB2-CON TO TRUE                                       00012300
012400     PERFORM 200-GET-CURR-CON                                     00012400
012500     .                                                            00012500
012600                                                                  00012600
012700                                                                  00012700
012800 310-DO-SET-DB2-CON.                                              00012800
012900     ADD 1 TO WS-CON-SWITCHES                                     00012900
013000                                                                  00013000
013100     EVALUATE TRUE                                                00013100
013200       WHEN WS-PROD                                               00013200
013300         EXEC SQL                                                 00013300
013400           CONNECT TO DB2P                                        00013400
013500         END-EXEC                                                 00013500
013600       WHEN OTHER                                                 00013600
013700         EXEC SQL                                                 00013700
013800           CONNECT TO DB2T                                        00013800
013900         END-EXEC                                                 00013900
014000     END-EVALUATE                                                 00014000
014100                                                                  00014100
014200     EVALUATE TRUE                                                00014200
014300       WHEN SQLCODE = 0                                           00014300
014400         CONTINUE                                                 00014400
014500                                                                  00014500
014600       WHEN OTHER                                                 00014600
014700         SET  FAILURE TO TRUE                                     00014700
014800         MOVE SQLCODE TO WS-SQLCODE                               00014800
014900         STRING 'YYYS0220 - Failure connecting to DB2, SQL='      00014900
015000                WS-SQLCODE                                        00015000
015100             DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT               00015100
015200     END-EVALUATE                                                 00015200
015300     .                                                            00015300
015400                                                                  00015400
015500                                                                  00015500
015600* ================================================================00015600
015700* Set the connection to Oracle (if not already)...                00015700
015800* ================================================================00015800
015900 400-SET-ORACLE-CON.                                              00015900
016000     ADD 1 TO WS-TOT-REQS                                         00016000
016100     ADD 1 TO WS-OCL-REQS                                         00016100
016200                                                                  00016200
016300     IF NOT WS-ORACLE-CON                                         00016300
016400       PERFORM 410-DO-SET-ORACLE-CON                              00016400
016500     END-IF                                                       00016500
016600                                                                  00016600
016700     SET WS-ORACLE-CON TO TRUE                                    00016700
016800     PERFORM 200-GET-CURR-CON                                     00016800
016900     .                                                            00016900
017000                                                                  00017000
017100                                                                  00017100
017200 410-DO-SET-ORACLE-CON.                                           00017200
017300     ADD 1 TO WS-CON-SWITCHES                                     00017300
017400                                                                  00017400
017500     EVALUATE TRUE                                                00017500
017600       WHEN WS-PROD                                               00017600
017700         EXEC SQL                                                 00017700
017800           CONNECT TO DRDAASP1                                    00017800
017900         END-EXEC                                                 00017900
018000                                                                  00018000
018100       WHEN WS-TEST                                               00018100
018200         EXEC SQL                                                 00018200
018300           CONNECT TO DRDAASC7                                    00018300
018400         END-EXEC                                                 00018400
018500                                                                  00018500
018600       WHEN WS-TST6                                               00018600
018700         EXEC SQL                                                 00018700
018800           CONNECT TO DRDAASC6                                    00018800
018900         END-EXEC                                                 00018900
019000                                                                  00019000
019100       WHEN WS-TST5                                               00019100
019200         EXEC SQL                                                 00019200
019300           CONNECT TO DRDAASC5                                    00019300
019400         END-EXEC                                                 00019400
019500                                                                  00019500
019600       WHEN WS-TST4                                               00019600
019700         EXEC SQL                                                 00019700
019800           CONNECT TO DRDAASD1                                    00019800
019900         END-EXEC                                                 00019900
020000                                                                  00020000
020100       WHEN WS-TST3                                               00020100
020200         EXEC SQL                                                 00020200
020300           CONNECT TO DRDAASC1                                    00020300
020400         END-EXEC                                                 00020400
020500     END-EVALUATE                                                 00020500
020600                                                                  00020600
020700     EVALUATE TRUE                                                00020700
020800       WHEN SQLCODE = 0                                           00020800
020900         CONTINUE                                                 00020900
021000                                                                  00021000
021100       WHEN OTHER                                                 00021100
021200         SET  FAILURE TO TRUE                                     00021200
021300         MOVE SQLCODE TO WS-SQLCODE                               00021300
021400         STRING 'YYYS0220 - Failure connecting to Oracle, SQL='   00021400
021500                WS-SQLCODE                                        00021500
021600             DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT               00021600
021700     END-EVALUATE                                                 00021700
021800     .                                                            00021800
021900                                                                  00021900
022000                                                                  00022000
022100* ================================================================00022100
022200* Return the stats...                                             00022200
022300* ================================================================00022300
022400 500-GET-STATS.                                                   00022400
022500     PERFORM 200-GET-CURR-CON                                     00022500
022600                                                                  00022600
022700     MOVE WS-TOT-REQS     TO YYYC0220-TOT-REQS                    00022700
022800     MOVE WS-DB2-REQS     TO YYYC0220-DB2-REQS                    00022800
022900     MOVE WS-OCL-REQS     TO YYYC0220-OCL-REQS                    00022900
023000     MOVE WS-OVR-REQS     TO YYYC0220-OVR-REQS                    00023000
023100     MOVE WS-CON-SWITCHES TO YYYC0220-CON-SWITCHES                00023100
023200     MOVE WS-OVR-SWITCHES TO YYYC0220-OVR-SWITCHES                00023200
023300     .                                                            00023300
023400                                                                  00023400
023500                                                                  00023500
023600* ================================================================00023600
023700* Reset the stats to 0...                                         00023700
023800* ================================================================00023800
023900 600-SET-STATS.                                                   00023900
024000     INITIALIZE WS-STATS                                          00024000
024100                YYYC0220-STATS                                    00024100
024200     .                                                            00024200
024300                                                                  00024300
024400                                                                  00024400
024500* ================================================================00024500
024600* Don't like this, but allow curr con to be changed externally... 00024600
024700* ================================================================00024700
024800 700-SET-OVERRIDE-CON.                                            00024800
024900     ADD 1 TO WS-OVR-REQS                                         00024900
025000                                                                  00025000
025100     EVALUATE TRUE                                                00025100
025200       WHEN YYYC0220-DB2-CON                                      00025200
025300       AND  NOT WS-DB2-CON                                        00025300
025400         SET WS-DB2-CON    TO TRUE                                00025400
025500         ADD 1             TO WS-OVR-SWITCHES                     00025500
025600                                                                  00025600
025700       WHEN YYYC0220-ORACLE-CON                                   00025700
025800       AND NOT WS-ORACLE-CON                                      00025800
025900         SET WS-ORACLE-CON TO TRUE                                00025900
026000         ADD 1             TO WS-OVR-SWITCHES                     00026000
026100                                                                  00026100
026200       WHEN OTHER                                                 00026200
026300         SET FAILURE TO TRUE                                      00026300
026400         MOVE 'YYYS0220 - Invalid over-ride connection!'          00026400
026500           TO IS-RTRN-MSG-TXT                                     00026500
026600     END-EVALUATE                                                 00026600
026700     .                                                            00026700
