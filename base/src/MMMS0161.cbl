000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    MMMS0161.                                         00000200
000300 AUTHOR.        NAME                                              00000300
000400 DATE-WRITTEN.  Circa 1600.                                       00000400
000500*---------------------------------------------------------------- 00000500
000600* Synchronize the "old" databaSe with the "new" master database.  00000600
000700*                                                                 00000700
000800* Note that at some time in the future, after I retire, this      00000800
000900* will no longer be neceSSary.                                    00000900
001000*                                                                 00001000
001100* Databases Synchronized..                                        00001100
001200*    XXXX_LOC_CLS_AD_ZN -  DDDTCZ01                               00001200
001300*             with                                                00001300
001400*    IMS Store  - DDDPST01                                        00001400
001500*              &                                                  00001500
001600*    DB2 Store  - DDDTRL01                                        00001600
001700*                                                                 00001700
001800* Note that this sync will fail if the DDDPST01 or DDDTRL01       00001800
001900* are not present.  The reason for this is that they should have  00001900
002000* been added when the parent (LO) was added.                      00002000
002100* --------------------------------------------------------------- 00002100
003000 ENVIRONMENT DIVISION.                                            00003000
003100 DATA DIVISION.                                                   00003100
003200 WORKING-STORAGE SECTION.                                         00003200
003300* --------------------------------------------------------------- 00003300
003400* MiSc working Storage...                                         00003400
003500* --------------------------------------------------------------- 00003500
003600 01 WS-SQLCODE                          PIC ----9.                00003600
003700                                                                  00003700
003800 01 WS-SUBRS.                                                     00003800
003900     05 NNNS0473-CZ-DAO                 PIC X(8) VALUE 'NNNS0473'.00003900
004000     05 NNNS0120-DDDTRL01-DAO           PIC X(8) VALUE 'NNNS0120'.00004000
004100                                                                  00004100
004200 01 WS-LOGICALS.                                                  00004200
004300     05 WS-DDDTRL01-SW                  PIC X(1) VALUE SPACES.    00004300
004400         88 DDDTRL01-DOES-NOT-EXIST              VALUE ' '.       00004400
004500         88 DDDTRL01-EXISTS                      VALUE 'X'.       00004500
004600                                                                  00004600
004700* --------------------------------------------------------------- 00004700
004800* Miscellaneous copy books go here...                             00004800
004900* --------------------------------------------------------------- 00004900
005000 COPY YYYN000A.                                                   00005000
005100 COPY YYYN000C.                                                   00005100
005200 COPY YYYN005A.                                                   00005200
005300 COPY YYYC0097.                                                   00005300
005400                                                                  00005400
005500 COPY NNNN0000.                                                   00005500
005600 COPY MMMK001B.                                                   00005600
005700 COPY YYYN111A.                                                   00005700
005800 COPY YYYC0131.                                                   00005800
005900 COPY TTTK0001.                                                   00005900
006000 COPY DDDBSSAS.                                                   00006000
006100* ----------------------------------------------------------------00006100
006200* "Old" database copybooks go here...                             00006200
006300* ----------------------------------------------------------------00006300
006400 COPY DDDPST01.                                                   00006400
006500 COPY PPPTRL01.                                                   00006500
006600                                                                  00006600
006700* ----------------------------------------------------------------00006700
006800* DB2 Stuff...                                                    00006800
006900* ----------------------------------------------------------------00006900
007000     EXEC SQL                                                     00007000
007100       INCLUDE SQLCA                                              00007100
007200     END-EXEC                                                     00007200
007300                                                                  00007300
007400 LINKAGE SECTION.                                                 00007400
007500 COPY XXXN001A.                                                   00007500
007600 COPY YYYN110A.                                                   00007600
007700 COPY MMMC0161.                                                   00007700
007800 COPY PPPTCZ01.                                                   00007800
007900                                                                  00007900
008000 PROCEDURE DIVISION USING                                         00008000
008100     XXXN001A                                                     00008100
008200     YYYN110A                                                     00008200
008300     MMMC0161                                                     00008300
008400     P-DDDTCZ01                                                   00008400
008500     .                                                            00008500
008600                                                                  00008600
008700*================================================================ 00008700
008800* Start of program main line.                                     00008800
008900*================================================================ 00008900
009000 000-MAIN.                                                        00009000
009100     PERFORM 100-INITIALIZE                                       00009100
009200                                                                  00009200
009300     IF SUCCESS                                                   00009300
009400       EVALUATE TRUE                                              00009400
009500         WHEN YYYN110A-ADD                                        00009500
009600         OR   YYYN110A-UPD                                        00009600
009700           PERFORM 1000-DO-THE-SYNC                               00009700
009800                                                                  00009800
009900         WHEN YYYN110A-DEL                                        00009900
010000           PERFORM 2000-DO-THE-DELETE                             00010000
010100                                                                  00010100
010200         WHEN OTHER                                               00010200
010300           SET FAILURE TO TRUE                                    00010300
010400           MOVE 'MMMS0161 - Invalid YYYN110A-IO-FUNC passed.'     00010400
010500             TO IS-RTRN-MSG-TXT                                   00010500
010600       END-EVALUATE                                               00010600
010700     END-IF                                                       00010700
010800                                                                  00010800
010900     GOBACK                                                       00010900
011000     .                                                            00011000
011100                                                                  00011100
011200                                                                  00011200
011300*================================================================ 00011300
011400* Initialization...                                               00011400
011500*================================================================ 00011500
011600 100-INITIALIZE.                                                  00011600
011700     PERFORM 110-MISC-INITS                                       00011700
011800                                                                  00011800
011900     IF LOC-TYP-CD OF P-DDDTCZ01 NOT = K-STORE-LOC-TYPE           00011900
012000       SET FAILURE TO TRUE                                        00012000
012100       MOVE 'MMMS0161 - only store types can have class zones!'   00012100
012200         TO IS-RTRN-MSG-TXT                                       00012200
012300     END-IF                                                       00012300
012400                                                                  00012400
012500     IF  SUCCESS                                                  00012500
012600     AND MMMC0161-CZ-IS-NOT-CURRENT                               00012600
012700     AND YYYN110A-UPD                                             00012700
012800       PERFORM 120-GET-CZ                                         00012800
012900     END-IF                                                       00012900
013000     .                                                            00013000
013100                                                                  00013100
013200                                                                  00013200
013300 110-MISC-INITS.                                                  00013300
013400     INITIALIZE XXXN001A                                          00013400
013500                DDDPST01                                          00013500
013600                P-DDDTRL01                                        00013600
013700                                                                  00013700
013800     EVALUATE TRUE                                                00013800
013900       WHEN YYYN110A-CICS-ENV                                     00013900
014000         SET YYYN005A-CICS-ENV        TO TRUE                     00014000
014100       WHEN YYYN110A-BATCH-ENV                                    00014100
014200         SET YYYN005A-BATCH-ENV       TO TRUE                     00014200
014300       WHEN OTHER                                                 00014300
014400         SET FAILURE TO TRUE                                      00014400
014500         MOVE 'MMMS0161 - Invalid environment variable.'          00014500
014600           TO IS-RTRN-MSG-TXT                                     00014600
014700     END-EVALUATE                                                 00014700
014710     IF YYYN110A-ORACLE                                           00014710
014720         SET YYYN005A-ORACLE  TO TRUE                             00014720
014730     END-IF                                                       00014730
014800     .                                                            00014800
014900                                                                  00014900
015000                                                                  00015000
015100 120-GET-CZ.                                                      00015100
015200     SET EXIT-GET-UNIQUE-ROW TO TRUE                              00015200
015300     CALL NNNS0473-CZ-DAO USING                                   00015300
015400         XXXN001A                                                 00015400
015500         SQLCA                                                    00015500
015600         YYYN005A                                                 00015600
015700         NNNN0000-PARMS                                           00015700
015800         P-DDDTCZ01                                               00015800
015900                                                                  00015900
016000     EVALUATE TRUE                                                00016000
016100       WHEN NOT SUCCESS                                           00016100
016200         CONTINUE                                                 00016200
016300                                                                  00016300
016400       WHEN SQLCODE = 100                                         00016400
016500         SET  FAILURE                 TO TRUE                     00016500
016600         MOVE 'MMMS0161 - Store/Cls/Zone does not exist!'         00016600
016700           TO IS-RTRN-MSG-TXT                                     00016700
016800                                                                  00016800
016900       WHEN SQLCODE NOT = 0                                       00016900
017000         MOVE SQLCODE                 TO WS-SQLCODE               00017000
017100         SET  FAILURE                 TO TRUE                     00017100
017200         MOVE SPACES                  TO IS-RTRN-MSG-TXT          00017200
017300         STRING 'MMMS0161 - Failure getting Cls Ad Zn, SQLCODE='  00017300
017400                 WS-SQLCODE                                       00017400
017500                 DELIMITED BY SIZE                                00017500
017600                 INTO IS-RTRN-MSG-TXT                             00017600
017700     END-EVALUATE                                                 00017700
017800     .                                                            00017800
017900                                                                  00017900
018000                                                                  00018000
018100*================================================================ 00018100
018200* Sync the store-class-zone table...                              00018200
018300*================================================================ 00018300
018400 1000-DO-THE-SYNC.                                                00018400
018500     IF SUCCESS                                                   00018500
018600       PERFORM 1400-GET-DDDTRL01                                  00018600
018700       IF SUCCESS                                                 00018700
018800         PERFORM 1500-UPDATE-DDDTRL01                             00018800
018900       END-IF                                                     00018900
019000     END-IF                                                       00019000
019100     .                                                            00019100
019200                                                                  00019200
019300                                                                  00019300
019400                                                                  00019400
019500                                                                  00019500
019600*=================================================================00019600
019700* GET  DDDTRL01 ...                                               00019700
019800*=================================================================00019800
019900 1400-GET-DDDTRL01.                                               00019900
020000     PERFORM 9000-TRANSLATE-TO-OLD                                00020000
020100     IF SUCCESS                                                   00020100
020200       SET DDDTRL01-EXISTS            TO TRUE                     00020200
020300       SET EXIT-GET-UNIQUE-ROW        TO TRUE                     00020300
020400       PERFORM 9200-CALL-DDDTRL01-DAO                             00020400
020500                                                                  00020500
020600       EVALUATE TRUE                                              00020600
020700         WHEN SQLCODE = 100                                       00020700
020800           SET  DDDTRL01-DOES-NOT-EXIST TO TRUE                   00020800
020900           SET  FAILURE                 TO TRUE                   00020900
021000           MOVE 'MMMS0161 - Store not found in DB2 Table (FCRL)!' 00021000
021100             TO IS-RTRN-MSG-TXT                                   00021100
021200                                                                  00021200
021300         WHEN SQLCODE NOT = 0                                     00021300
021400           SET  FAILURE TO TRUE                                   00021400
021500           MOVE SQLCODE TO WS-SQLCODE                             00021500
021600           MOVE SPACES  TO IS-RTRN-MSG-TXT                        00021600
021700           STRING 'MMMS0161 - Error reading DB2 Store, '          00021700
021800                  'key='      ST-STORE-KEY OF DDDPST01            00021800
021900                  ',SQL=' WS-SQLCODE '.'                          00021900
022000                  DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT          00022000
022100       END-EVALUATE                                               00022100
022200     END-IF                                                       00022200
022300     .                                                            00022300
022400                                                                  00022400
022500                                                                  00022500
022600*================================================================ 00022600
022700* Do the update of DDDTRL01...                                    00022700
022800*================================================================ 00022800
022900 1500-UPDATE-DDDTRL01.                                            00022900
023000     PERFORM 9000-TRANSLATE-TO-OLD                                00023000
023100     IF SUCCESS                                                   00023100
023200       PERFORM 1510-SETUP-UPD-DDDTRL01                            00023200
023300       IF SUCCESS                                                 00023300
023400         PERFORM 1520-READY-UPD-DDDTRL01                          00023400
023500       END-IF                                                     00023500
023600     END-IF                                                       00023600
023700     .                                                            00023700
023800                                                                  00023800
023900                                                                  00023900
024000 1510-SETUP-UPD-DDDTRL01.                                         00024000
024100     EXIT                                                         00024100
024200     .                                                            00024200
024300                                                                  00024300
024400                                                                  00024400
024500 1520-READY-UPD-DDDTRL01.                                         00024500
024600     SET EXIT-PUT-MODIFY-ROW          TO TRUE                     00024600
024700     PERFORM 9200-CALL-DDDTRL01-DAO                               00024700
024800                                                                  00024800
024900     EVALUATE TRUE                                                00024900
025000       WHEN SQLCODE = 100                                         00025000
025100         MOVE SQLCODE                 TO WS-SQLCODE               00025100
025200         SET  FAILURE                 TO TRUE                     00025200
025300         MOVE SPACES                  TO IS-RTRN-MSG-TXT          00025300
025400         STRING 'MMMS0161 - DB2 Store record not found,'          00025400
025500                'key='      ST-STORE-KEY OF DDDPST01              00025500
025600                ',SQL=' WS-SQLCODE '.'                            00025600
025700                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT            00025700
025800                                                                  00025800
025900       WHEN SQLCODE NOT = 0                                       00025900
026000         MOVE SQLCODE                 TO WS-SQLCODE               00026000
026100         SET  FAILURE                 TO TRUE                     00026100
026200         MOVE SPACES                  TO IS-RTRN-MSG-TXT          00026200
026300         STRING 'MMMS0161 - Error adding DB2 Store, '             00026300
026400                'key='      ST-STORE-KEY OF DDDPST01              00026400
026500                ',SQL=' WS-SQLCODE '.'                            00026500
026600                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT            00026600
026700     END-EVALUATE                                                 00026700
026800     .                                                            00026800
026900                                                                  00026900
027000                                                                  00027000
027100*================================================================ 00027100
027200* We need to clear the zone fields on the IMS and DB2 tables      00027200
027300* when a CZ is deleted.                                           00027300
027400*================================================================ 00027400
027500 2000-DO-THE-DELETE.                                              00027500
027600     IF SUCCESS                                                   00027600
027700       PERFORM 1400-GET-DDDTRL01                                  00027700
027800       IF  NOT SUCCESS                                            00027800
027900       AND DDDTRL01-DOES-NOT-EXIST                                00027900
028000         INITIALIZE XXXN001A                                      00028000
028100       END-IF                                                     00028100
028200     END-IF                                                       00028200
028300                                                                  00028300
028400     IF SUCCESS                                                   00028400
028500       PERFORM 2010-CLEAR-ZONE                                    00028500
028600       IF SUCCESS                                                 00028600
028700         IF DDDTRL01-EXISTS                                       00028700
028800           PERFORM 1520-READY-UPD-DDDTRL01                        00028800
028900         END-IF                                                   00028900
029000       END-IF                                                     00029000
029100     END-IF                                                       00029100
029200     .                                                            00029200
029300                                                                  00029300
029400                                                                  00029400
029500 2010-CLEAR-ZONE.                                                 00029500
029600     EVALUATE ITM-CLS-CD                                          00029600
029700      WHEN 12                                                     00029700
029800        INITIALIZE ST-CLASS12-ZONE                                00029800
029900                   ST-CLASS12-EXCEPTION-AD-ZONE                   00029900
030000                   FC-RL-CL12-ZONE-NO                             00030000
030100                   FC-RL-CL12-ADZN-NO                             00030100
030200      WHEN 13                                                     00030200
030300        INITIALIZE ST-CLASS13-ZONE                                00030300
030400                   ST-CLASS13-EXCEPTION-AD-ZONE                   00030400
030500                   FC-RL-CL13-ZONE-NO                             00030500
030600                   FC-RL-CL13-ADZN-NO                             00030600
030700      WHEN 14                                                     00030700
030800        INITIALIZE ST-CLASS14-ZONE                                00030800
030900                   ST-CLASS14-EXCEPTION-AD-ZONE                   00030900
031000                   FC-RL-CL14-ZONE-NO                             00031000
031100                   FC-RL-CL14-ADZN-NO                             00031100
031200      WHEN 36                                                     00031200
031300        INITIALIZE ST-CLASS36-EXCEPTION-AD-ZONE                   00031300
031400                   FC-RL-CL36-ADZN-NO                             00031400
031500      WHEN 37                                                     00031500
031600        INITIALIZE ST-CLASS37-EXCEPTION-AD-ZONE                   00031600
031700                   FC-RL-CL37-ADZN-NO                             00031700
031800     END-EVALUATE                                                 00031800
031900     .                                                            00031900
032000                                                                  00032000
032100                                                                  00032100
032200*================================================================ 00032200
032300* Misc functions...                                               00032300
032400*================================================================ 00032400
032500 9000-TRANSLATE-TO-OLD.                                           00032500
032600     INITIALIZE YYYN111A                                          00032600
032700     SET YYYN111A-NEW-2-OLD           TO TRUE                     00032700
032800                                                                  00032800
032900     CALL MMMS0162-TRANSLATE-CZ USING                             00032900
033000         XXXN001A                                                 00033000
033100         YYYN111A                                                 00033100
033200         P-DDDTCZ01                                               00033200
033300         DDDPST01                                                 00033300
033400         P-DDDTRL01                                               00033400
033500     .                                                            00033500
033600                                                                  00033600
033700 9200-CALL-DDDTRL01-DAO.                                          00033700
033800     CALL NNNS0120-DDDTRL01-DAO USING                             00033800
033900         XXXN001A                                                 00033900
034000         SQLCA                                                    00034000
034100         YYYN005A                                                 00034100
034200         NNNN0000-PARMS                                           00034200
034300         P-DDDTRL01                                               00034300
034400     .                                                            00034400
