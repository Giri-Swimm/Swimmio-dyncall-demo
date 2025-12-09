000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    WWWS0003.                                         00000200
000300 AUTHOR.        Name.					                  00000300
000400 DATE-WRITTEN.  Circa 1600.                                       00000400
003100* --------------------------------------------------------------- 00003100
003200 ENVIRONMENT DIVISION.                                            00003200
003300 DATA DIVISION.                                                   00003300
003400 WORKING-STORAGE SECTION.                                         00003400
003500* --------------------------------------------------------------- 00003500
003600* Misc working storage...                                         00003600
003700* --------------------------------------------------------------- 00003700
003800 01 WS-SQLCODE                          PIC ----9.                00003800
003900                                                                  00003900
004000 01 WS-SUBRS.                                                     00004000
004100     05 WS-LR-DAO                       PIC X(8) VALUE 'NNNS0488'.00004100
004200     05 WS-CZ-DAO                       PIC X(8) VALUE 'NNNS0473'.00004200
004300     05 WS-RC-DAO                       PIC X(8) VALUE 'NNNSSS20'.00004300
004400     05 WS-AA-DAO                       PIC X(8) VALUE 'NNNS0007'.00004400
004500                                                                  00004500
004600 01 WS-LOGICALS.                                                  00004600
004700     05 WS-XXXPSTTT-SW                  PIC X(1) VALUE SPACES.    00004700
004800         88 XXXPSTTT-DOES-NOT-EXIST              VALUE ' '.       00004800
004900         88 XXXPSTTT-EXISTS                      VALUE 'X'.       00004900
005000     05 WS-DDDTRL01-SW                  PIC X(1) VALUE SPACES.    00005000
005100         88 DDDTRL01-DOES-NOT-EXIST              VALUE ' '.       00005100
005200         88 DDDTRL01-EXISTS                      VALUE 'X'.       00005200
005300     05 WS-CZ-SW                        PIC X(1) VALUE SPACES.    00005300
005400         88 CZ-EXISTS                            VALUE ' '.       00005400
005500         88 CZ-NOT-EXISTS                        VALUE 'X'.       00005500
005600     05 WS-CZ2-SW                       PIC X(1) VALUE SPACES.    00005600
005700         88 NEED-CZ                              VALUE ' '.       00005700
005800         88 NO-NEED-CZ                           VALUE 'X'.       00005800
005900                                                                  00005900
006000 01 I                                   PIC S9(4) COMP VALUE 0.   00006000
006100 01 WS-CNT                              PIC S9(9) COMP VALUE 0.   00006100
006200                                                                  00006200
006300 01 WS-NNNN0000-EXIT-CODES              PIC S9(4) COMP VALUE 0.   00006300
006400                                                                  00006400
006500 01 WS-CLASS-ARRAY             PIC X(10) VALUE '1213143637'.      00006500
006600 01 REDEFINES WS-CLASS-ARRAY.                                     00006600
006700     05 WS-CZ                  PIC 9(2) OCCURS 5 TIMES.           00006700
006800                                                                  00006800
006900 01 UPD-FLAG-CHECK           PIC X(1) VALUE SPACE.                00006900
007000     88 MMMU0003-MODIFY            VALUE 'M'.                     00007000
007100     88 MMMU0003-DELETE            VALUE 'D'.                     00007100
007200     88 MMMU0003-INSERT            VALUE 'I'.                     00007200
007300* --------------------------------------------------------------- 00007300
007400* Miscellaneous copy books go here...                             00007400
007500* --------------------------------------------------------------- 00007500
007600 COPY YYYN000A.                                                   00007600
007700 COPY YYYN000C.                                                   00007700
007800 COPY YYYN110A.                                                   00007800
007900 COPY YYYN111A.                                                   00007900
008000 COPY YYYC0097.                                                   00008000
008100 COPY MMMK002A.                                                   00008100
008200 COPY MMMK001B.                                                   00008200
008300 COPY NNNN000U.                                                   00008300
008400                                                                  00008400
008500* ----------------------------------------------------------------00008500
008600* "Old" database copybooks go here...                             00008600
008700* ----------------------------------------------------------------00008700
008800 COPY PPPTZSSS.                                                   00008800
008900                                                                  00008900
009000* ----------------------------------------------------------------00009000
009100* "New" database copybooks go here...                             00009100
009200* ----------------------------------------------------------------00009200
009300 COPY PPPTLO01.                                                   00009300
009400 COPY PPPTFX01.                                                   00009400
009500 COPY PPPTLR01.                                                   00009500
009600 COPY PPPTCZ01.                                                   00009600
009700                                                                  00009700
009800* ----------------------------------------------------------------00009800
009900* DB2 stuff...                                                    00009900
010000* ----------------------------------------------------------------00010000
010100     EXEC SQL                                                     00010100
010200       INCLUDE SQLCA                                              00010200
010300     END-EXEC                                                     00010300
010400     EXEC SQL                                                     00010400
010500       INCLUDE DDDTLO01                                           00010500
010600     END-EXEC                                                     00010600
010700                                                                  00010700
010800 LINKAGE SECTION.                                                 00010800
010900 COPY XXXN001A.                                                   00010900
011000 COPY YYYN005A.                                                   00011000
011100 COPY NNNN0000.                                                   00011100
011200 COPY WWWC0003.                                                   00011200
011300 COPY XXXPSTTT.                                                   00011300
011400 COPY PPPTRL01.                                                   00011400
011500                                                                  00011500
011600 PROCEDURE DIVISION USING                                         00011600
011700     XXXN001A                                                     00011700
011800     YYYN005A                                                     00011800
011900     NNNN0000-PARMS                                               00011900
012000     WWWC0003                                                     00012000
012100     XXXPSTTT                                                     00012100
012200     P-DDDTRL01                                                   00012200
012300     .                                                            00012300
012400                                                                  00012400
012500*================================================================ 00012500
012600* Start of program main line.                                     00012600
012700*================================================================ 00012700
012800 000-MAIN.                                                        00012800
012900     PERFORM 100-INITIALIZE                                       00012900
013000                                                                  00013000
013100     IF SUCCESS                                                   00013100
013200       PERFORM 200-CHECK-INPUTS                                   00013200
013300       IF SUCCESS                                                 00013300
013400         EVALUATE TRUE                                            00013400
013500           WHEN EXIT-OPEN-CURSOR                                  00013500
013600              PERFORM 1000-EXIT-OPEN-CURSOR                       00013600
013700           WHEN EXIT-CLOSE-CURSOR                                 00013700
013800              PERFORM 1100-EXIT-CLOSE-CURSOR                      00013800
013900           WHEN EXIT-GET-UNIQUE-ROW                               00013900
014000              PERFORM 1200-EXIT-GET-UNIQUE-ROW                    00014000
014100           WHEN EXIT-GET-NEXT-ROW                                 00014100
014200              PERFORM 1300-EXIT-GET-NEXT-ROW                      00014200
014300           WHEN EXIT-PUT-MODIFY-ROW                               00014300
014400              PERFORM 1400-EXIT-PUT-MODIFY-ROW                    00014400
014500           WHEN EXIT-PUT-INSERT-ROW                               00014500
014600              PERFORM 1500-EXIT-PUT-INSERT-ROW                    00014600
014700           WHEN EXIT-PUT-PURGE-ROW                                00014700
014800              PERFORM 1600-EXIT-PUT-PURGE-ROW                     00014800
014900         END-EVALUATE                                             00014900
015000         PERFORM 300-EXIT-STUFF                                   00015000
015100       END-IF                                                     00015100
015200     END-IF                                                       00015200
015300                                                                  00015300
015400     GOBACK                                                       00015400
015500     .                                                            00015500
015600                                                                  00015600
015700                                                                  00015700
015800*================================================================ 00015800
015900* Initialization...                                               00015900
016000*================================================================ 00016000
016100 100-INITIALIZE.                                                  00016100
016200     PERFORM 110-MISC-INITS                                       00016200
016300     IF SUCCESS                                                   00016300
016400       PERFORM 120-SETUP-KEYS                                     00016400
016500     END-IF                                                       00016500
016600     .                                                            00016600
016700                                                                  00016700
016800                                                                  00016800
016900 110-MISC-INITS.                                                  00016900
017000     INITIALIZE XXXN001A                                          00017000
017100     MOVE 'WWWS0003' TO YYYC0097-ERROR-PGM                        00017100
017200                                                                  00017200
017300     EVALUATE TRUE                                                00017300
017400       WHEN YYYN005A-CICS-ENV                                     00017400
017500         SET YYYN110A-CICS-ENV TO TRUE                            00017500
017600       WHEN YYYN005A-BATCH-ENV                                    00017600
017700         SET YYYN110A-BATCH-ENV TO TRUE                           00017700
017800       WHEN OTHER                                                 00017800
017900         SET FAILURE TO TRUE                                      00017900
018000         MOVE 'WWWS0003 - Invalid environment variable.'          00018000
018100           TO IS-RTRN-MSG-TXT                                     00018100
018200     END-EVALUATE                                                 00018200
018300     IF (YYYN005A-ORACLE       OR EXIT-PUT-INSERT-ROW             00018300
018400         OR EXIT-PUT-PURGE-ROW OR EXIT-PUT-MODIFY-ROW)            00018400
018500       PERFORM 115-CONNECT-TO-ORACLE                              00018500
018600     END-IF                                                       00018600
018700     .                                                            00018700
018800                                                                  00018800
018900                                                                  00018900
019000* ================================================================00019000
019100* Connect to Oracle Database                                      00019100
019200* ================================================================00019200
019300 115-CONNECT-TO-ORACLE.                                           00019300
019400     CALL Z-ORA-CONNECT USING XXXN001A                            00019400
019500                              SQLCA                               00019500
019600                                                                  00019600
019700     IF NOT SUCCESS                                               00019700
019800       MOVE SQLCODE TO WS-SQLCODE                                 00019800
019900       MOVE SPACES  TO IS-RTRN-MSG-TXT                            00019900
020000       STRING 'WWWS0003 - Error connecting to Oracle. Sqlcode ='  00020000
020100               WS-SQLCODE                                         00020100
020200               DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT             00020200
020300     END-IF                                                       00020300
020400     .                                                            00020400
020500                                                                  00020500
020600                                                                  00020600
020700 120-SETUP-KEYS.                                                  00020700
020800     IF  ST-STORE-TYPE = SPACES                                   00020800
020900     AND EXIT-PUT-PURGE-ROW                                       00020900
021000       MOVE ST-STORE-NUMBER TO LOC-NBR    OF DCLXXXATION          00021000
021100       MOVE 'S '            TO LOC-TYP-CD OF DCLXXXATION          00021100
021200       PERFORM 130-CHECK-TYPE                                     00021200
021300                                                                  00021300
021400       IF SUCCESS AND ST-STORE-TYPE = SPACES                      00021400
021500         MOVE 'A '            TO LOC-TYP-CD OF DCLXXXATION        00021500
021600         PERFORM 130-CHECK-TYPE                                   00021600
021700         IF SUCCESS AND ST-STORE-TYPE = SPACES                    00021700
021800           SET  FAILURE TO TRUE                                   00021800
021900           MOVE 'WWWS0003 - Cannot resolve type code for store!'  00021900
022000             TO IS-RTRN-MSG-TXT                                   00022000
022100         END-IF                                                   00022100
022200       END-IF                                                     00022200
022300     END-IF                                                       00022300
022400     .                                                            00022400
022500                                                                  00022500
022600                                                                  00022600
022700* ================================================================00022700
022800* Connecting to db2 database                                      00022800
022900* ================================================================00022900
023000 125-CONNECT-TO-DB2.                                              00023000
023100     CALL Z-DB2-CONNECT         USING XXXN001A                    00023100
023200                                      SQLCA                       00023200
023300     .                                                            00023300
023400                                                                  00023400
023500 130-CHECK-TYPE.                                                  00023500
023600     EXEC SQL                                                     00023600
023700       SELECT COUNT(*)                                            00023700
023800       INTO   :WS-CNT                                             00023800
023900       FROM   XXXATION                                            00023900
024000       WHERE  LOC_NBR = :DCLXXXATION.LOC-NBR                      00024000
024100       AND    LOC_TYP_CD = :DCLXXXATION.LOC-TYP-CD                00024100
024200     END-EXEC                                                     00024200
024300                                                                  00024300
024400     IF SQLCODE NOT = 0                                           00024400
024500       SET  FAILURE TO TRUE                                       00024500
024600       MOVE SQLCODE TO WS-SQLCODE                                 00024600
024700       STRING 'WWWS0003 - Error resolving key, SQL='              00024700
024800              WS-SQLCODE                                          00024800
024900              DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT              00024900
025000     ELSE                                                         00025000
025100       IF WS-CNT > 0                                              00025100
025200         MOVE LOC-TYP-CD OF DCLXXXATION                           00025200
025300           TO ST-STORE-TYPE OF XXXPSTTT                           00025300
025400       END-IF                                                     00025400
025500     END-IF                                                       00025500
025600     .                                                            00025600
025700                                                                  00025700
025800                                                                  00025800
025900*================================================================ 00025900
026000* First, lets make sure we have all the data we need and then     00026000
026100* put the old data in the new format...                           00026100
026200*================================================================ 00026200
026300 200-CHECK-INPUTS.                                                00026300
026400     IF WWWC0003-DDDTRL01-NOT-CURRENT                             00026400
026500       MOVE NNNN0000-EXIT-CODES TO WS-NNNN0000-EXIT-CODES         00026500
026600       INITIALIZE P-DDDTRL01                                      00026600
026700                                                                  00026700
026800       MOVE ST-STORE-NUMBER OF XXXPSTTT                           00026800
026900         TO FC-STORE-NO     OF P-DDDTRL01                         00026900
027000                                                                  00027000
027100       SET EXIT-GET-UNIQUE-ROW TO TRUE                            00027100
027200       PERFORM 2130-CALL-DDDTRL01-DAO                             00027200
027300                                                                  00027300
027400       EVALUATE TRUE                                              00027400
027500         WHEN SQLCODE = 100                                       00027500
027600           INITIALIZE XXXN001A                                    00027600
027700                                                                  00027700
027800         WHEN SQLCODE NOT = 0                                     00027800
027900           PERFORM 9999-SETUP-DB2-ERROR                           00027900
028000           STRING 'WWWS0003 - Failed getting DB2 Retail,SQL='     00028000
028100                   WS-SQLCODE                                     00028100
028200                   DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT         00028200
028300       END-EVALUATE                                               00028300
028400                                                                  00028400
028500       MOVE WS-NNNN0000-EXIT-CODES TO NNNN0000-EXIT-CODES         00028500
028600     END-IF                                                       00028600
028700     .                                                            00028700
028800                                                                  00028800
028900                                                                  00028900
029000 300-EXIT-STUFF.                                                  00029000
029100     IF YYYN005A-ORACLE                                           00029100
029200       PERFORM 125-CONNECT-TO-DB2                                 00029200
029300     END-IF                                                       00029300
029400     MOVE SQLCODE TO DB2-SQL-CODE                                 00029400
029500     .                                                            00029500
029600                                                                  00029600
029700                                                                  00029700
029800* ================================================================00029800
029900* Open cursor logic...                                            00029900
030000* ================================================================00030000
030100 1000-EXIT-OPEN-CURSOR.                                           00030100
030200     SET  FAILURE TO TRUE                                         00030200
030300     MOVE 'WWWS0003 - Open Cursor is not supported - YET!'        00030300
030400       TO IS-RTRN-MSG-TXT                                         00030400
030500     .                                                            00030500
030600                                                                  00030600
030700                                                                  00030700
030800* ================================================================00030800
030900* Close cursor logic...                                           00030900
031000* ================================================================00031000
031100 1100-EXIT-CLOSE-CURSOR.                                          00031100
031200     SET  FAILURE TO TRUE                                         00031200
031300     MOVE 'WWWS0003 - Close Cursor is not supported - YET!'       00031300
031400       TO IS-RTRN-MSG-TXT                                         00031400
031500     .                                                            00031500
031600                                                                  00031600
031700                                                                  00031700
031800* ================================================================00031800
031900* Get unique...                                                   00031900
032000* ================================================================00032000
032100 1200-EXIT-GET-UNIQUE-ROW.                                        00032100
032200     PERFORM 1210-PROCESS-LO                                      00032200
032300     IF SUCCESS                                                   00032300
032400       PERFORM 1220-PROCESS-LR                                    00032400
032500     END-IF                                                       00032500
032600     IF SUCCESS                                                   00032600
032700       PERFORM 1230-PROCESS-CZ                                    00032700
032800     END-IF                                                       00032800
032900     .                                                            00032900
033000                                                                  00033000
033100                                                                  00033100
033200 1210-PROCESS-LO.                                                 00033200
033300     PERFORM 2200-GET-LO                                          00033300
033400     IF SUCCESS                                                   00033400
033500       SET YYYN111A-NEW-2-OLD TO TRUE                             00033500
033600       PERFORM 2000-LO-TRANSLATION                                00033600
033700     END-IF                                                       00033700
033800     .                                                            00033800
033900                                                                  00033900
034000                                                                  00034000
034100 1220-PROCESS-LR.                                                 00034100
034200     PERFORM 2210-GET-LR                                          00034200
034300     IF SUCCESS                                                   00034300
034400       SET YYYN111A-NEW-2-OLD TO TRUE                             00034400
034500       PERFORM 2010-LR-TRANSLATION                                00034500
034600     END-IF                                                       00034600
034700     .                                                            00034700
034800                                                                  00034800
034900                                                                  00034900
035000 1230-PROCESS-CZ.                                                 00035000
035100     PERFORM 1235-INIT-CLASS-ZONES                                00035100
035200     IF ST-XXX-STORE                                              00035200
035300       PERFORM VARYING I FROM 1 BY 1                              00035300
035400           UNTIL I > K-CZ-MAX OR NOT SUCCESS                      00035400
035500         PERFORM 2220-GET-CZ                                      00035500
035600         IF SUCCESS AND CZ-EXISTS                                 00035600
035700           SET YYYN111A-NEW-2-OLD TO TRUE                         00035700
035800           PERFORM 2020-CZ-TRANSLATION                            00035800
035900         END-IF                                                   00035900
036000       END-PERFORM                                                00036000
036100     END-IF                                                       00036100
036200     .                                                            00036200
036300                                                                  00036300
036400                                                                  00036400
036500 1235-INIT-CLASS-ZONES.                                           00036500
036600     INITIALIZE ST-CLASS12-ZONE                                   00036600
036700                ST-CLASS12-EXCEPTION-AD-ZONE                      00036700
036800                ST-CLASS13-ZONE                                   00036800
036900                ST-CLASS13-EXCEPTION-AD-ZONE                      00036900
037000                ST-CLASS14-ZONE                                   00037000
037100                ST-CLASS14-EXCEPTION-AD-ZONE                      00037100
037200                ST-CLASS36-EXCEPTION-AD-ZONE                      00037200
037300                ST-CLASS37-EXCEPTION-AD-ZONE                      00037300
037400     .                                                            00037400
037500                                                                  00037500
037600                                                                  00037600
037700* ================================================================00037700
037800* Get next...                                                     00037800
037900* ================================================================00037900
038000 1300-EXIT-GET-NEXT-ROW.                                          00038000
038100     SET  FAILURE TO TRUE                                         00038100
038200     MOVE 'WWWS0003 - Get-Next is not supported - YET!'           00038200
038300       TO IS-RTRN-MSG-TXT                                         00038300
038400     .                                                            00038400
038500                                                                  00038500
038600                                                                  00038600
038700* ================================================================00038700
038800* Modify...                                                       00038800
038900* ================================================================00038900
039000 1400-EXIT-PUT-MODIFY-ROW.                                        00039000
039100     PERFORM 1410-PROCESS-LO                                      00039100
039200     IF SUCCESS                                                   00039200
039300       PERFORM 1420-PROCESS-LR                                    00039300
039400     END-IF                                                       00039400
039500     IF SUCCESS                                                   00039500
039600       PERFORM 1430-PROCESS-CZ                                    00039600
039700     END-IF                                                       00039700
039800     SET EXIT-PUT-MODIFY-ROW TO TRUE                              00039800
039900     .                                                            00039900
040000                                                                  00040000
040100                                                                  00040100
040200 1410-PROCESS-LO.                                                 00040200
040300     PERFORM 2200-GET-LO                                          00040300
040400     IF SUCCESS                                                   00040400
040500       SET YYYN111A-OLD-2-NEW TO TRUE                             00040500
040600       PERFORM 2000-LO-TRANSLATION                                00040600
040700       IF SUCCESS                                                 00040700
040800         PERFORM 2100-CALL-LO-DAO                                 00040800
040900                                                                  00040900
041000         EVALUATE TRUE                                            00041000
041100           WHEN NOT SUCCESS                                       00041100
041200             CONTINUE                                             00041200
041300           WHEN SQLCODE = 100                                     00041300
041400             SET FAILURE TO TRUE                                  00041400
041500             MOVE 'WWWS0003 - Store XXXATION not found!'          00041500
041600               TO IS-RTRN-MSG-TXT                                 00041600
041700           WHEN SQLCODE = -530                                    00041700
041800             PERFORM 9998-DB2-530-ERROR                           00041800
041900           WHEN SQLCODE NOT = 0                                   00041900
042000             PERFORM 9999-SETUP-DB2-ERROR                         00042000
042100             STRING 'WWWS0003 - Failed on upd XXXATION(LO),SQL='  00042100
042200                 WS-SQLCODE                                       00042200
042300                 DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT           00042300
042400         END-EVALUATE                                             00042400
042500                                                                  00042500
042600         IF SUCCESS                                               00042600
042700           SET YYYN111A-NEW-2-OLD TO TRUE                         00042700
042800           PERFORM 2000-LO-TRANSLATION                            00042800
042900         END-IF                                                   00042900
043000       END-IF                                                     00043000
043100     END-IF                                                       00043100
043200     SET EXIT-PUT-MODIFY-ROW TO TRUE                              00043200
043300     .                                                            00043300
043400                                                                  00043400
043500                                                                  00043500
043600 1420-PROCESS-LR.                                                 00043600
043700     PERFORM 2210-GET-LR                                          00043700
043800     IF SUCCESS                                                   00043800
043900       SET YYYN111A-OLD-2-NEW TO TRUE                             00043900
044000       PERFORM 2010-LR-TRANSLATION                                00044000
044100       IF SUCCESS                                                 00044100
044200         PERFORM 2110-CALL-LR-DAO                                 00044200
044300                                                                  00044300
044400         EVALUATE TRUE                                            00044400
044500           WHEN NOT SUCCESS                                       00044500
044600             CONTINUE                                             00044600
044700           WHEN SQLCODE = 100                                     00044700
044800             SET FAILURE TO TRUE                                  00044800
044900             MOVE 'WWWS0003 - Store XXXATION not found (LR)!'     00044900
045000               TO IS-RTRN-MSG-TXT                                 00045000
045100           WHEN SQLCODE = -530                                    00045100
045200             PERFORM 9998-DB2-530-ERROR                           00045200
045300           WHEN SQLCODE NOT = 0                                   00045300
045400             PERFORM 9999-SETUP-DB2-ERROR                         00045400
045500             STRING 'WWWS0003 - Failed on upd RtlLoc(LR),SQL='    00045500
045600                 WS-SQLCODE                                       00045600
045700                 DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT           00045700
045800         END-EVALUATE                                             00045800
045900                                                                  00045900
046000         IF SUCCESS                                               00046000
046100           SET YYYN111A-NEW-2-OLD TO TRUE                         00046100
046200           PERFORM 2010-LR-TRANSLATION                            00046200
046300         END-IF                                                   00046300
046400       END-IF                                                     00046400
046500     END-IF                                                       00046500
046600     SET EXIT-PUT-MODIFY-ROW TO TRUE                              00046600
046700     .                                                            00046700
046800                                                                  00046800
046900                                                                  00046900
047000 1430-PROCESS-CZ.                                                 00047000
047100     IF ST-XXX-STORE                                              00047100
047200       PERFORM VARYING I FROM 1 BY 1                              00047200
047300           UNTIL I > K-CZ-MAX OR NOT SUCCESS                      00047300
047400         PERFORM 1440-CHECK-CZ-STATUS                             00047400
047500         IF NO-NEED-CZ                                            00047500
047600           PERFORM 1450-DELETE-CZ                                 00047600
047700         ELSE                                                     00047700
047800           PERFORM 1460-UPDATE-CZ                                 00047800
047900         END-IF                                                   00047900
048000       END-PERFORM                                                00048000
048100       SET EXIT-PUT-MODIFY-ROW TO TRUE                            00048100
048200     ELSE                                                         00048200
048300       PERFORM 2300-DELETE-CZS                                    00048300
048400     END-IF                                                       00048400
048500     .                                                            00048500
048600                                                                  00048600
048700                                                                  00048700
048800 1440-CHECK-CZ-STATUS.                                            00048800
048900     SET  NEED-CZ   TO TRUE                                       00048900
049000     MOVE WS-CZ (I) TO ITM-CLS-CD OF P-DDDTCZ01                   00049000
049100     MOVE ITM-CLS-CD OF P-DDDTCZ01 to CLASS1                      00049100
049200                                                                  00049200
049300     EVALUATE TRUE                                                00049300
049400       WHEN BEEF-AND-LAMB                                         00049400
049500         IF  ST-CLASS12-ZONE              = 0                     00049500
049600         AND ST-CLASS12-EXCEPTION-AD-ZONE = 0                     00049600
049700           SET NO-NEED-CZ TO TRUE                                 00049700
049800         END-IF                                                   00049800
049900       WHEN FRESH-POULTRY                                         00049900
050000         IF  ST-CLASS13-ZONE              = 0                     00050000
050100         AND ST-CLASS13-EXCEPTION-AD-ZONE = 0                     00050100
050200           SET NO-NEED-CZ TO TRUE                                 00050200
050300         END-IF                                                   00050300
050400       WHEN FRESH-PORK                                            00050400
050500         IF  ST-CLASS14-ZONE              = 0                     00050500
050600         AND ST-CLASS14-EXCEPTION-AD-ZONE = 0                     00050600
050700           SET NO-NEED-CZ TO TRUE                                 00050700
050800         END-IF                                                   00050800
050900       WHEN NON-FROZEN-MARKET                                     00050900
051000         IF ST-CLASS36-EXCEPTION-AD-ZONE  = 0                     00051000
051100           SET NO-NEED-CZ TO TRUE                                 00051100
051200         END-IF                                                   00051200
051300       WHEN FROZEN-MARKET                                         00051300
051400         IF ST-CLASS37-EXCEPTION-AD-ZONE  = 0                     00051400
051500           SET NO-NEED-CZ TO TRUE                                 00051500
051600         END-IF                                                   00051600
051700     END-EVALUATE                                                 00051700
051800     .                                                            00051800
051900                                                                  00051900
052000                                                                  00052000
052100 1450-DELETE-CZ.                                                  00052100
052200     MOVE ST-STORE-TYPE       TO LOC-TYP-CD OF P-DDDTCZ01         00052200
052300     MOVE ST-STORE-NUMBER     TO LOC-NBR    OF P-DDDTCZ01         00052300
052400     MOVE WS-CZ (I)           TO ITM-CLS-CD OF P-DDDTCZ01         00052400
052500     SET EXIT-PUT-PURGE-ROW   TO TRUE                             00052500
052600     PERFORM 2120-CALL-CZ-DAO                                     00052600
052700                                                                  00052700
052800     EVALUATE TRUE                                                00052800
052900       WHEN SQLCODE = 100                                         00052900
053000         MOVE 0 TO SQLCODE                                        00053000
053100         INITIALIZE XXXN001A                                      00053100
053200       WHEN NOT SUCCESS                                           00053200
053300         CONTINUE                                                 00053300
053400       WHEN SQLCODE NOT = 0                                       00053400
053500         PERFORM 9999-SETUP-DB2-ERROR                             00053500
053600         STRING 'WWWS0003 - Failed deleting Zne(CZ),SQL='         00053600
053700             WS-SQLCODE                                           00053700
053800              DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT              00053800
053900     END-EVALUATE                                                 00053900
054000                                                                  00054000
054100     IF SUCCESS                                                   00054100
054200       SET YYYN111A-NEW-2-OLD TO TRUE                             00054200
054300       PERFORM 2020-CZ-TRANSLATION                                00054300
054400     END-IF                                                       00054400
054500     .                                                            00054500
054600                                                                  00054600
054700                                                                  00054700
054800 1460-UPDATE-CZ.                                                  00054800
054900     PERFORM 2220-GET-CZ                                          00054900
055000     IF SUCCESS                                                   00055000
055100       SET YYYN111A-OLD-2-NEW TO TRUE                             00055100
055200       PERFORM 2020-CZ-TRANSLATION                                00055200
055300       IF SUCCESS                                                 00055300
055400         IF CZ-NOT-EXISTS                                         00055400
055500           SET EXIT-PUT-INSERT-ROW TO TRUE                        00055500
055600         ELSE                                                     00055600
055700           SET EXIT-PUT-MODIFY-ROW TO TRUE                        00055700
055800         END-IF                                                   00055800
055900         PERFORM 2120-CALL-CZ-DAO                                 00055900
056000                                                                  00056000
056100         EVALUATE TRUE                                            00056100
056200           WHEN NOT SUCCESS                                       00056200
056300             CONTINUE                                             00056300
056400           WHEN SQLCODE = 100                                     00056400
056500             SET FAILURE TO TRUE                                  00056500
056600             MOVE 'WWWS0003 - Retail Zone not found!'             00056600
056700               TO IS-RTRN-MSG-TXT                                 00056700
056800           WHEN SQLCODE = -530                                    00056800
056900             PERFORM 9998-DB2-530-ERROR                           00056900
057000           WHEN SQLCODE NOT = 0                                   00057000
057100             PERFORM 9999-SETUP-DB2-ERROR                         00057100
057200             STRING 'WWWS0003 - Failed on upd Rtl Zne(CZ),SQL='   00057200
057300                 WS-SQLCODE                                       00057300
057400                  DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT          00057400
057500         END-EVALUATE                                             00057500
057600                                                                  00057600
057700         IF SUCCESS                                               00057700
057800           SET YYYN111A-NEW-2-OLD TO TRUE                         00057800
057900           PERFORM 2020-CZ-TRANSLATION                            00057900
058000         END-IF                                                   00058000
058100       END-IF                                                     00058100
058200     END-IF                                                       00058200
058300     .                                                            00058300
058400                                                                  00058400
058500                                                                  00058500
058600* ================================================================00058600
058700* Insert...                                                       00058700
058800* ================================================================00058800
058900 1500-EXIT-PUT-INSERT-ROW.                                        00058900
059000     PERFORM 1510-PROCESS-LO                                      00059000
059100     IF SUCCESS                                                   00059100
059200       PERFORM 1530-PROCESS-LR                                    00059200
059300     END-IF                                                       00059300
059400     IF SUCCESS                                                   00059400
059500       PERFORM 1540-PROCESS-CZ                                    00059500
059600     END-IF                                                       00059600
059700     .                                                            00059700
059800                                                                  00059800
059900                                                                  00059900
060000 1510-PROCESS-LO.                                                 00060000
060100     SET YYYN111A-OLD-2-NEW           TO TRUE                     00060100
060200     PERFORM 2000-LO-TRANSLATION                                  00060200
060300     IF SUCCESS                                                   00060300
060400       PERFORM 2100-CALL-LO-DAO                                   00060400
060500       EVALUATE TRUE                                              00060500
060600         WHEN SQLCODE = -803                                      00060600
060700           PERFORM 1520-TRY-UPDATE                                00060700
060800         WHEN SQLCODE = -530                                      00060800
060900           PERFORM 9998-DB2-530-ERROR                             00060900
061000         WHEN NOT SUCCESS                                         00061000
061100           CONTINUE                                               00061100
061200         WHEN SQLCODE NOT = 0                                     00061200
061300           PERFORM 9999-SETUP-DB2-ERROR                           00061300
061400           STRING 'WWWS0003 - Failed adding XXXATION(LO),SQL='    00061400
061500               WS-SQLCODE                                         00061500
061600               DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT             00061600
061700       END-EVALUATE                                               00061700
061800                                                                  00061800
061900       IF SUCCESS                                                 00061900
062000         SET YYYN111A-NEW-2-OLD TO TRUE                           00062000
062100         PERFORM 2000-LO-TRANSLATION                              00062100
062200       END-IF                                                     00062200
062300     END-IF                                                       00062300
062400     .                                                            00062400
062500                                                                  00062500
062600                                                                  00062600
062700 1520-TRY-UPDATE.                                                 00062700
062800     SET EXIT-PUT-MODIFY-ROW TO TRUE                              00062800
062900     PERFORM 2100-CALL-LO-DAO                                     00062900
063000     EVALUATE TRUE                                                00063000
063100       WHEN NOT SUCCESS                                           00063100
063200         CONTINUE                                                 00063200
063300       WHEN SQLCODE = -530                                        00063300
063400         PERFORM 9998-DB2-530-ERROR                               00063400
063500       WHEN SQLCODE NOT = 0                                       00063500
063600         PERFORM 9999-SETUP-DB2-ERROR                             00063600
063700         STRING 'WWWS0003 - Failed on try-upd XXXATION(LO),SQL='  00063700
063800             WS-SQLCODE                                           00063800
063900             DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT               00063900
064000     END-EVALUATE                                                 00064000
064100     SET EXIT-PUT-INSERT-ROW TO TRUE                              00064100
064200     .                                                            00064200
064300                                                                  00064300
064400                                                                  00064400
064500 1530-PROCESS-LR.                                                 00064500
064600     SET YYYN111A-OLD-2-NEW TO TRUE                               00064600
064700     PERFORM 2010-LR-TRANSLATION                                  00064700
064800     IF SUCCESS                                                   00064800
064900       PERFORM 2110-CALL-LR-DAO                                   00064900
065000                                                                  00065000
065100       EVALUATE TRUE                                              00065100
065200         WHEN SQLCODE = -803                                      00065200
065300           PERFORM 2210-GET-LR                                    00065300
065400           IF SUCCESS                                             00065400
065500             SET YYYN111A-OLD-2-NEW TO TRUE                       00065500
065600             PERFORM 2010-LR-TRANSLATION                          00065600
065700             IF SUCCESS                                           00065700
065800               PERFORM 1525-TRY-UPDATE                            00065800
065900             END-IF                                               00065900
066000           END-IF                                                 00066000
066100         WHEN NOT SUCCESS                                         00066100
066200           CONTINUE                                               00066200
066300         WHEN SQLCODE = -530                                      00066300
066400           PERFORM 9998-DB2-530-ERROR                             00066400
066500         WHEN SQLCODE NOT = 0                                     00066500
066600           PERFORM 9999-SETUP-DB2-ERROR                           00066600
066700           STRING 'WWWS0003 - Failed adding Rtl-Loc(LR),SQL='     00066700
066800               WS-SQLCODE                                         00066800
066900               DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT             00066900
067000       END-EVALUATE                                               00067000
067100                                                                  00067100
067200       IF SUCCESS                                                 00067200
067300         SET YYYN111A-NEW-2-OLD TO TRUE                           00067300
067400         PERFORM 2010-LR-TRANSLATION                              00067400
067500       END-IF                                                     00067500
067600     END-IF                                                       00067600
067700     .                                                            00067700
067800                                                                  00067800
067900                                                                  00067900
068000 1525-TRY-UPDATE.                                                 00068000
068100     SET EXIT-PUT-MODIFY-ROW TO TRUE                              00068100
068200     PERFORM 2110-CALL-LR-DAO                                     00068200
068300     EVALUATE TRUE                                                00068300
068400       WHEN NOT SUCCESS                                           00068400
068500         CONTINUE                                                 00068500
068600       WHEN SQLCODE = -530                                        00068600
068700         PERFORM 9998-DB2-530-ERROR                               00068700
068800       WHEN SQLCODE NOT = 0                                       00068800
068900         PERFORM 9999-SETUP-DB2-ERROR                             00068900
069000         STRING 'WWWS0003 - Failed on Rtl-Loc(LR),SQL='           00069000
069100             WS-SQLCODE                                           00069100
069200             DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT               00069200
069300     END-EVALUATE                                                 00069300
069400     SET EXIT-PUT-INSERT-ROW TO TRUE                              00069400
069500     .                                                            00069500
069600                                                                  00069600
069700                                                                  00069700
069800 1540-PROCESS-CZ.                                                 00069800
069900     IF ST-XXX-STORE                                              00069900
070000       PERFORM VARYING I FROM 1 BY 1                              00070000
070100           UNTIL I > K-CZ-MAX OR NOT SUCCESS                      00070100
070200         PERFORM 1440-CHECK-CZ-STATUS                             00070200
070300         IF NEED-CZ                                               00070300
070400           PERFORM 1460-UPDATE-CZ                                 00070400
070500         END-IF                                                   00070500
070600       END-PERFORM                                                00070600
070700       SET EXIT-PUT-INSERT-ROW TO TRUE                            00070700
070800     END-IF                                                       00070800
070900     .                                                            00070900
071000                                                                  00071000
071100                                                                  00071100
071200* ================================================================00071200
071300* Delete...                                                       00071300
071400* ================================================================00071400
071500 1600-EXIT-PUT-PURGE-ROW.                                         00071500
071600     SET YYYN111A-OLD-2-NEW TO TRUE                               00071600
071700     PERFORM 2000-LO-TRANSLATION                                  00071700
071800     IF SUCCESS                                                   00071800
071900       PERFORM 2100-CALL-LO-DAO                                   00071900
072000       EVALUATE TRUE                                              00072000
072100         WHEN SQLCODE = 100                                       00072100
072200           MOVE 0 TO SQLCODE                                      00072200
072300         WHEN NOT SUCCESS                                         00072300
072400           CONTINUE                                               00072400
072500         WHEN SQLCODE NOT = 0                                     00072500
072600           PERFORM 9999-SETUP-DB2-ERROR                           00072600
072700           STRING 'WWWS0003 - Failed deleting store,SQL='         00072700
072800               WS-SQLCODE                                         00072800
072900               DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT             00072900
073000       END-EVALUATE                                               00073000
073100     END-IF                                                       00073100
073200     .                                                            00073200
073300                                                                  00073300
073400                                                                  00073400
073500* ================================================================00073500
073600* Translations...                                                 00073600
073700* ================================================================00073700
073800 2000-LO-TRANSLATION.                                             00073800
073900     CALL MMMSSS58-TRANSLATE-LO USING                             00073900
074000         XXXN001A                                                 00074000
074100         YYYN111A                                                 00074100
074200         P-DDDTLO01                                               00074200
074300         XXXPSTTT                                                 00074300
074400         P-DDDTRL01                                               00074400
074500     .                                                            00074500
074600                                                                  00074600
074700                                                                  00074700
074800 2010-LR-TRANSLATION.                                             00074800
074900     CALL MMMSSS60-TRANSLATE-LR USING                             00074900
075000         XXXN001A                                                 00075000
075100         YYYN111A                                                 00075100
075200         P-DDDTLR01                                               00075200
075300         XXXPSTTT                                                 00075300
075400         P-DDDTRL01                                               00075400
075500                                                                  00075500
075600     IF YYYN111A-OLD-2-NEW                                        00075600
075700       PERFORM 2015-AA-TRANSLATION                                00075700
075800     END-IF                                                       00075800
075900     .                                                            00075900
076000                                                                  00076000
076100                                                                  00076100
076200 2015-AA-TRANSLATION.                                             00076200
076300     MOVE NNNN0000-EXIT-CODES    TO WS-NNNN0000-EXIT-CODES        00076300
076400                                                                  00076400
076500     INITIALIZE P-DDDTZS01                                        00076500
076600     MOVE ST-STORE-NUMBER        TO FC-STORE-NO OF P-DDDTZS01     00076600
076700                                                                  00076700
076800     SET EXIT-GET-UNIQUE-ROW     TO TRUE                          00076800
076900     PERFORM 2140-CALL-ADZONE-DAO                                 00076900
077000                                                                  00077000
077100     EVALUATE TRUE                                                00077100
077200       WHEN NOT SUCCESS                                           00077200
077300         CONTINUE                                                 00077300
077400       WHEN SQLCODE = 100                                         00077400
077500         INITIALIZE XXXN001A                                      00077500
077600       WHEN SQLCODE = 0                                           00077600
077700         MOVE AD-ZONE              OF P-DDDTZS01                  00077700
077800           TO CURR-AD-ZN-NBR       OF P-DDDTLR01                  00077800
077900       WHEN SQLCODE NOT = 0                                       00077900
078000         PERFORM 9999-SETUP-DB2-ERROR                             00078000
078100         STRING 'WWWS0003 - Failed getting Ad Zone,SQL='          00078100
078200                 WS-SQLCODE                                       00078200
078300                 DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT           00078300
078400     END-EVALUATE                                                 00078400
078500                                                                  00078500
078600     MOVE WS-NNNN0000-EXIT-CODES TO NNNN0000-EXIT-CODES           00078600
078700     .                                                            00078700
078800                                                                  00078800
078900                                                                  00078900
079000 2020-CZ-TRANSLATION.                                             00079000
079100     CALL MMMSSS62-TRANSLATE-CZ USING                             00079100
079200         XXXN001A                                                 00079200
079300         YYYN111A                                                 00079300
079400         P-DDDTCZ01                                               00079400
079500         XXXPSTTT                                                 00079500
079600         P-DDDTRL01                                               00079600
079700     .                                                            00079700
079800                                                                  00079800
079900                                                                  00079900
080000* ================================================================00080000
080100* DAO calls and stuff...                                          00080100
080200* ================================================================00080200
080300 2100-CALL-LO-DAO.                                                00080300
080400     CALL NNNS0487-LO-DAO USING                                   00080400
080500         XXXN001A                                                 00080500
080600         SQLCA                                                    00080600
080700         YYYN005A                                                 00080700
080800         NNNN0000-PARMS                                           00080800
080900         P-DDDTLO01                                               00080900
081000     .                                                            00081000
081100                                                                  00081100
081200                                                                  00081200
081300 2110-CALL-LR-DAO.                                                00081300
081400     CALL WS-LR-DAO USING                                         00081400
081500         XXXN001A                                                 00081500
081600         SQLCA                                                    00081600
081700         YYYN005A                                                 00081700
081800         NNNN0000-PARMS                                           00081800
081900         P-DDDTLR01                                               00081900
082000     .                                                            00082000
082100                                                                  00082100
082200                                                                  00082200
082300 2120-CALL-CZ-DAO.                                                00082300
082400     CALL WS-CZ-DAO USING                                         00082400
082500         XXXN001A                                                 00082500
082600         SQLCA                                                    00082600
082700         YYYN005A                                                 00082700
082800         NNNN0000-PARMS                                           00082800
082900         P-DDDTCZ01                                               00082900
083000     .                                                            00083000
083100                                                                  00083100
083200                                                                  00083200
083300 2130-CALL-DDDTRL01-DAO.                                          00083300
083400     CALL WS-RC-DAO USING                                         00083400
083500         XXXN001A                                                 00083500
083600         SQLCA                                                    00083600
083700         YYYN005A                                                 00083700
083800         NNNN0000-PARMS                                           00083800
083900         P-DDDTRL01                                               00083900
084000     .                                                            00084000
084100                                                                  00084100
084200                                                                  00084200
084300 2140-CALL-ADZONE-DAO.                                            00084300
084400     CALL WS-AA-DAO USING                                         00084400
084500         XXXN001A                                                 00084500
084600         SQLCA                                                    00084600
084700         YYYN005A                                                 00084700
084800         NNNN0000-PARMS                                           00084800
084900         P-DDDTZS01                                               00084900
085000     .                                                            00085000
085100                                                                  00085100
085200                                                                  00085200
085300* ================================================================00085300
085400* Get functions for the new database structures...                00085400
085500* ================================================================00085500
085600 2200-GET-LO.                                                     00085600
085700     INITIALIZE P-DDDTLO01                                        00085700
085800     MOVE ST-STORE-NUMBER     TO LOC-NBR    OF P-DDDTLO01         00085800
085900     MOVE ST-STORE-TYPE       TO LOC-TYP-CD OF P-DDDTLO01         00085900
086000                                                                  00086000
086100     MOVE NNNN0000-EXIT-CODES TO WS-NNNN0000-EXIT-CODES           00086100
086200     SET  EXIT-GET-UNIQUE-ROW TO TRUE                             00086200
086300     PERFORM 2100-CALL-LO-DAO                                     00086300
086400                                                                  00086400
086500     EVALUATE TRUE                                                00086500
086600       WHEN SQLCODE = 0                                           00086600
086700         CONTINUE                                                 00086700
086800       WHEN SQLCODE = 100                                         00086800
086900         MOVE SPACES TO IS-RTRN-MSG-TXT                           00086900
087000         SET  FAILURE TO TRUE                                     00087000
087100         STRING 'WWWS0003 - Store '  ST-STORE-NUMBER              00087100
087200                ' not found in XXXATION table.'                   00087200
087300                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT            00087300
087400       WHEN OTHER                                                 00087400
087500         PERFORM 9999-SETUP-DB2-ERROR                             00087500
087600         STRING 'WWWS0003 - Failed on XXXATION table(LO),SQL='    00087600
087700                 WS-SQLCODE                                       00087700
087800                 DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT           00087800
087900     END-EVALUATE                                                 00087900
088000     MOVE WS-NNNN0000-EXIT-CODES TO NNNN0000-EXIT-CODES           00088000
088100     .                                                            00088100
088200                                                                  00088200
088300                                                                  00088300
088400 2210-GET-LR.                                                     00088400
088500     MOVE NNNN0000-EXIT-CODES TO WS-NNNN0000-EXIT-CODES           00088500
088600     INITIALIZE P-DDDTLR01                                        00088600
088700     MOVE ST-STORE-NUMBER    TO LOC-NBR    OF P-DDDTLR01          00088700
088800     MOVE ST-STORE-TYPE      TO LOC-TYP-CD OF P-DDDTLR01          00088800
088900                                                                  00088900
089000     SET  EXIT-GET-UNIQUE-ROW TO TRUE                             00089000
089100     PERFORM 2110-CALL-LR-DAO                                     00089100
089200                                                                  00089200
089300     EVALUATE TRUE                                                00089300
089400       WHEN SQLCODE = 0                                           00089400
089500       OR   SQLCODE = 100                                         00089500
089600         INITIALIZE XXXN001A                                      00089600
089700       WHEN OTHER                                                 00089700
089800         PERFORM 9999-SETUP-DB2-ERROR                             00089800
089900         STRING 'WWWS0003 - Failed on Ret-Loc table(LR),SQL='     00089900
090000                 WS-SQLCODE                                       00090000
090100                 DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT           00090100
090200     END-EVALUATE                                                 00090200
090300     MOVE WS-NNNN0000-EXIT-CODES TO NNNN0000-EXIT-CODES           00090300
090400     .                                                            00090400
090500                                                                  00090500
090600                                                                  00090600
090700 2220-GET-CZ.                                                     00090700
090800     MOVE NNNN0000-EXIT-CODES TO WS-NNNN0000-EXIT-CODES           00090800
090900                                                                  00090900
091000     INITIALIZE P-DDDTCZ01                                        00091000
091100     MOVE ST-STORE-TYPE       TO LOC-TYP-CD OF P-DDDTCZ01         00091100
091200     MOVE ST-STORE-NUMBER     TO LOC-NBR    OF P-DDDTCZ01         00091200
091300     MOVE WS-CZ (I)           TO ITM-CLS-CD OF P-DDDTCZ01         00091300
091400     SET  EXIT-GET-UNIQUE-ROW TO TRUE                             00091400
091500     PERFORM 2120-CALL-CZ-DAO                                     00091500
091600                                                                  00091600
091700     EVALUATE TRUE                                                00091700
091800       WHEN SQLCODE = 0                                           00091800
091900         SET CZ-EXISTS TO TRUE                                    00091900
092000       WHEN SQLCODE = 100                                         00092000
092100         SET CZ-NOT-EXISTS TO TRUE                                00092100
092200         MOVE 0            TO SQLCODE                             00092200
092300       WHEN OTHER                                                 00092300
092400         PERFORM 9999-SETUP-DB2-ERROR                             00092400
092500         STRING 'WWWS0003 - Failed on Cls-Zne table(CZ),SQL='     00092500
092600                 WS-SQLCODE                                       00092600
092700                 DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT           00092700
092800     END-EVALUATE                                                 00092800
092900     MOVE WS-NNNN0000-EXIT-CODES TO NNNN0000-EXIT-CODES           00092900
093000     .                                                            00093000
093100                                                                  00093100
093200                                                                  00093200
093300 2300-DELETE-CZS.                                                 00093300
093400     SET MMMU0003-DELETE     TO TRUE                              00093400
093500     PERFORM 5000-CALL-MMMU0003-CUD-ROUTINE                       00093500
093600                                                                  00093600
093700     MOVE 0 TO SQLCODE                                            00093700
093800     .                                                            00093800
093900                                                                  00093900
094000                                                                  00094000
094100 5000-CALL-MMMU0003-CUD-ROUTINE.                                  00094100
094200     PERFORM 115-CONNECT-TO-ORACLE                                00094200
094300      CALL MMMU0003-ORACLE-UPDATE USING                           00094300
094400           XXXN001A                                               00094400
094500           SQLCA                                                  00094500
094600           YYYN005A                                               00094600
094700           NNNN0000-PARMS                                         00094700
094800           P-DDDTLO01                                             00094800
094900           UPD-FLAG-CHECK                                         00094900
095000     .                                                            00095000
095100                                                                  00095100
095200                                                                  00095200
095300* ================================================================00095300
095400* Misc functions...                                               00095400
095500* ================================================================00095500
095600 9998-DB2-530-ERROR.                                              00095600
095700     CALL Z-DB2-ERROR-HANDLER USING                               00095700
095800         XXXN001A                                                 00095800
095900         SQLCA                                                    00095900
096000         YYYN005A                                                 00096000
096100         YYYC0097                                                 00096100
096200     .                                                            00096200
096300                                                                  00096300
096400                                                                  00096400
096500 9999-SETUP-DB2-ERROR.                                            00096500
096600     MOVE SQLCODE TO WS-SQLCODE                                   00096600
096700     SET  FAILURE TO TRUE                                         00096700
096800     MOVE SPACES  TO IS-RTRN-MSG-TXT                              00096800
096900     .                                                            00096900
097000                                                                  00097000
097100                                                                  00097100
