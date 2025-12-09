000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    NNNS0573.                                         00000200
000300 AUTHOR.        NAME                                              00000300
000400 DATE-WRITTEN.  Circa, 1600.                                      00000400
000500 DATE-COMPILED.                                                   00000500
000600* ----------------------------------------------------------------00000600
000700* Description : Z-DAO Table IO object.                            00000700
000800*                                                                 00000800
000900* Source      : Generated from DCL Gen on 02/03/1999.             00000900
001000* Gen Version : Z-DAO 1.0b                                        00001000
001100* Template    : COBOL IO Subr - DB2.cob, Version 1.01b            00001100
001200*                                                                 00001200
001300* Table DBMS  : DB2                                               00001300
001400* Table Name  : XXX-DEPT , DDDTDP01                               00001400
001500* Table Handle: 00573                                             00001500
001600*                                                                 00001600
001700* Special Func: None at time of generation.                       00001700
001800* ----------------------------------------------------------------00001800
004400 ENVIRONMENT DIVISION.                                            00004400
004500 CONFIGURATION SECTION.                                           00004500
004600 DATA DIVISION.                                                   00004600
004700 WORKING-STORAGE SECTION.                                         00004700
004800                                                                  00004800
004900* ========================< MISC STUFF >==========================00004900
005000* Misc working storage variables go here.                         00005000
005100* ================================================================00005100
005200                                                                  00005200
005300 01 WS-CHECKPOINT-INC            PIC S9(4) COMP VALUE 0.          00005300
005400 01 WS-SQLCODE                   PIC ----9.                       00005400
005500                                                                  00005500
005600 01 WS-SUBRS.                                                     00005600
005700     05 MMMS0257-SYNC-DP         PIC X(8)  VALUE 'MMMS0257'.      00005700
005800                                                                  00005800
005900 01 WS-DUMMY                     PIC X(1)  VALUE SPACES.          00005900
006000 01 WS-DEPT-NM                   PIC X(30) VALUE SPACES.          00006000
006100 01 WS-STAGE-EVENT-SW            PIC X(1) VALUE SPACES.           00006100
006200   88 STAGE-EVENT                          VALUE 'Y'.             00006200
006300   88 DONT-STAGE-EVENT                     VALUE ' '.             00006300
006310 01 MMMS0304-RI-DEL-CHK          PIC X(8)  VALUE 'MMMS0304'.      00006310
006400                                                                  00006400
006500* ========================< COPY BOOKS >==========================00006500
006600* Place all copy books in this section.                           00006600
006700* ================================================================00006700
006800 COPY NNNN000U.                                                   00006800
006900 COPY HHHTDP01.                                                   00006900
007000 COPY XXXEIBLK.                                                   00007000
007100 COPY YYYN000A.                                                   00007100
007200 COPY YYYN110A.                                                   00007200
007300 COPY YYYC0107.                                                   00007300
007400 COPY YYYC0127.                                                   00007400
007500 COPY ZZZC0125.                                                   00007500
007600 COPY ZZZC0197.                                                   00007600
007700 COPY ZZZC0550.                                                   00007700
007800                                                                  00007800
007900 COPY WWWC0100.                                                   00007900
008000                                                                  00008000
008100 COPY MMMC0257.                                                   00008100
008200 COPY MMMK001D.                                                   00008200
008210 COPY MMMC0304.                                                   00008210
008300                                                                  00008300
008400* ========================< DCL GENS >============================00008400
008500* Place all DCL gens in this section.                             00008500
008600* ================================================================00008600
008700     EXEC SQL                                                     00008700
008800       INCLUDE DDDTDP01                                           00008800
008900     END-EXEC.                                                    00008900
009000                                                                  00009000
009100* ========================< CURSORS >=============================00009100
009200* Place all cursors in this section.                              00009200
009300* ================================================================00009300
009400                                                                  00009400
009500* --------------------------------------------------              00009500
009600* DDDXDP01 cursor declaration.                                    00009600
009700* --------------------------------------------------              00009700
009800     EXEC SQL                                                     00009800
009900         DECLARE DDDXDP01 CURSOR WITH HOLD FOR SELECT             00009900
010000             XXX_DEPT_NBR,                                        00010000
010100             STR_SUB_DEPT_ID,                                     00010100
010200             DEPT_NM,                                             00010200
010300             DEPT_ABB,                                            00010300
010400             REPT_GRP_CD,                                         00010400
010500             GRPRFT_LO_PCT,                                       00010500
010600             GRPRFT_HI_PCT,                                       00010600
010700             SHRNK_LO_PCT,                                        00010700
010800             SHRNK_HI_PCT,                                        00010800
010900             LST_UPDT_USR_ID,                                     00010900
011000             LST_UPDT_TS,                                         00011000
011100             ORG_ID                                               00011100
011200         FROM     XXX_DEPT                                        00011200
011300         WHERE   (XXX_DEPT_NBR >= :DCLXXX-DEPT.XXX-DEPT-NBR)      00011300
011400         AND NOT                                                  00011400
011500                 (XXX_DEPT_NBR  = :DCLXXX-DEPT.XXX-DEPT-NBR AND   00011500
011600                  STR_SUB_DEPT_ID  < :DCLXXX-DEPT.STR-SUB-DEPT-ID)00011600
011700         ORDER BY                                                 00011700
011800             XXX_DEPT_NBR,                                        00011800
011900             STR_SUB_DEPT_ID                                      00011900
012000     END-EXEC.                                                    00012000
012100* --------------------------------------------------              00012100
012200* DDDXDP02 cursor declaration.                                    00012200
012300* --------------------------------------------------              00012300
012400     EXEC SQL                                                     00012400
012500         DECLARE DDDXDP02 CURSOR WITH HOLD FOR SELECT             00012500
012600             XXX_DEPT_NBR,                                        00012600
012700             STR_SUB_DEPT_ID,                                     00012700
012800             DEPT_NM,                                             00012800
012900             DEPT_ABB,                                            00012900
013000             REPT_GRP_CD,                                         00013000
013100             GRPRFT_LO_PCT,                                       00013100
013200             GRPRFT_HI_PCT,                                       00013200
013300             SHRNK_LO_PCT,                                        00013300
013400             SHRNK_HI_PCT,                                        00013400
013500             LST_UPDT_USR_ID,                                     00013500
013600             LST_UPDT_TS,                                         00013600
013700             ORG_ID                                               00013700
013800         FROM     XXX_DEPT                                        00013800
013900         WHERE   (DEPT_NM >= :DCLXXX-DEPT.DEPT-NM)                00013900
014000         AND NOT                                                  00014000
014100                 (DEPT_NM  = :DCLXXX-DEPT.DEPT-NM AND             00014100
014200                  XXX_DEPT_NBR  < :DCLXXX-DEPT.XXX-DEPT-NBR)      00014200
014300         AND NOT                                                  00014300
014400                 (DEPT_NM  = :DCLXXX-DEPT.DEPT-NM AND             00014400
014500                  XXX_DEPT_NBR  = :DCLXXX-DEPT.XXX-DEPT-NBR AND   00014500
014600                  STR_SUB_DEPT_ID  < :DCLXXX-DEPT.STR-SUB-DEPT-ID)00014600
014700         ORDER BY                                                 00014700
014800             DEPT_NM,                                             00014800
014900             XXX_DEPT_NBR,                                        00014900
015000             STR_SUB_DEPT_ID                                      00015000
015100     END-EXEC.                                                    00015100
015200                                                                  00015200
015300 LINKAGE SECTION.                                                 00015300
015400 COPY XXXN001A.                                                   00015400
015500     EXEC SQL                                                     00015500
015600         INCLUDE SQLCA                                            00015600
015700     END-EXEC.                                                    00015700
015800 COPY YYYN005A.                                                   00015800
015900 COPY NNNN0000.                                                   00015900
016000 COPY PPPTDP01.                                                   00016000
016100                                                                  00016100
016200 PROCEDURE DIVISION USING                                         00016200
016300     XXXN001A                                                     00016300
016400     SQLCA                                                        00016400
016500     YYYN005A                                                     00016500
016600     NNNN0000-PARMS                                               00016600
016700     P-DDDTDP01                                                   00016700
016800     .                                                            00016800
016900                                                                  00016900
017000************************************************************      00017000
017100* MAIN PROGRAM LINE.                                              00017100
017200************************************************************      00017200
017300 0000-EXIT-DISPATCHER.                                            00017300
017400     PERFORM 100-INITIALIZATION                                   00017400
017500     EVALUATE TRUE                                                00017500
017600       WHEN NOT SUCCESS                                           00017600
017700          CONTINUE                                                00017700
017800       WHEN EXIT-OPEN-CURSOR                                      00017800
017900          PERFORM 1000-EXIT-OPEN-CURSOR                           00017900
018000       WHEN EXIT-CLOSE-CURSOR                                     00018000
018100          PERFORM 1100-EXIT-CLOSE-CURSOR                          00018100
018200       WHEN EXIT-GET-UNIQUE-ROW                                   00018200
018300          PERFORM 1200-EXIT-GET-UNIQUE-ROW                        00018300
018400       WHEN EXIT-GET-NEXT-ROW                                     00018400
018500          PERFORM 1300-EXIT-GET-NEXT-ROW                          00018500
018600       WHEN EXIT-PUT-MODIFY-ROW                                   00018600
018700          PERFORM 1400-EXIT-PUT-MODIFY-ROW                        00018700
018800       WHEN EXIT-PUT-INSERT-ROW                                   00018800
018900          PERFORM 1500-EXIT-PUT-INSERT-ROW                        00018900
019000       WHEN EXIT-PUT-PURGE-ROW                                    00019000
019100          PERFORM 1600-EXIT-PUT-PURGE-ROW                         00019100
019200       WHEN EXIT-DO-SPECIAL-IO-FUNCS                              00019200
019300          PERFORM 10000-DO-SPECIAL-IO-FUNCS                       00019300
019400     END-EVALUATE                                                 00019400
019500     PERFORM 120-EXIT-STUFF                                       00019500
019600     GOBACK                                                       00019600
019700     .                                                            00019700
019800                                                                  00019800
019900                                                                  00019900
020000* ================================================================00020000
020100* Initialize data areas needed to call the i/o subroutine         00020100
020200* ================================================================00020200
020300 100-INITIALIZATION.                                              00020300
020400     INITIALIZE XXXN001A                                          00020400
020500                DB2-STUFF                                         00020500
020600                DAO-RETURN-CODE                                   00020600
020700     MOVE NNNN0000-INDEX-HANDLE TO DDDTDP01-INDEX-HANDLE          00020700
020800     MOVE 0 TO WS-CHECKPOINT-INC                                  00020800
020900     MOVE 0 TO SQLCODE                                            00020900
021000     MOVE 0 TO SQL-INIT-FLAG                                      00021000
021100     IF NOT EXIT-CLOSE-CURSOR                                     00021100
021200       PERFORM 110-MOVE-PDA-FIELDS-2-DCL                          00021200
021300     END-IF                                                       00021300
021400     IF (YYYN005A-ORACLE       OR EXIT-PUT-INSERT-ROW             00021400
021500         OR EXIT-PUT-PURGE-ROW OR EXIT-PUT-MODIFY-ROW)            00021500
021600       PERFORM 115-CONNECT-TO-ORACLE                              00021600
021700     END-IF                                                       00021700
021800     .                                                            00021800
021900                                                                  00021900
022000                                                                  00022000
022100                                                                  00022100
022200* ================================================================00022200
022300* Move the elementary fields in the parameter data area to the DCL00022300
022400* ================================================================00022400
022500 110-MOVE-PDA-FIELDS-2-DCL.                                       00022500
022600     MOVE XXX-DEPT-NBR OF P-DDDTDP01                              00022600
022700       TO XXX-DEPT-NBR OF DCLXXX-DEPT                             00022700
022800     MOVE STR-SUB-DEPT-ID OF P-DDDTDP01                           00022800
022900       TO STR-SUB-DEPT-ID OF DCLXXX-DEPT                          00022900
023000     MOVE DEPT-NM OF P-DDDTDP01 TO DEPT-NM OF DCLXXX-DEPT         00023000
023100     MOVE DEPT-ABB OF P-DDDTDP01 TO DEPT-ABB OF DCLXXX-DEPT       00023100
023200     MOVE REPT-GRP-CD OF P-DDDTDP01 TO REPT-GRP-CD OF DCLXXX-DEPT 00023200
023300     MOVE GRPRFT-LO-PCT OF P-DDDTDP01                             00023300
023400       TO GRPRFT-LO-PCT OF DCLXXX-DEPT                            00023400
023500     MOVE GRPRFT-HI-PCT OF P-DDDTDP01                             00023500
023600       TO GRPRFT-HI-PCT OF DCLXXX-DEPT                            00023600
023700     MOVE SHRNK-LO-PCT OF P-DDDTDP01                              00023700
023800       TO SHRNK-LO-PCT OF DCLXXX-DEPT                             00023800
023900     MOVE SHRNK-HI-PCT OF P-DDDTDP01                              00023900
024000       TO SHRNK-HI-PCT OF DCLXXX-DEPT                             00024000
024100     MOVE LST-UPDT-USR-ID OF P-DDDTDP01                           00024100
024200       TO LST-UPDT-USR-ID OF DCLXXX-DEPT                          00024200
024300     MOVE LST-UPDT-TS OF P-DDDTDP01 TO LST-UPDT-TS OF DCLXXX-DEPT 00024300
024400     IF ORG-ID-X OF P-DDDTDP01 = SPACES                           00024400
024500       MOVE ZERO TO ORG-ID OF P-DDDTDP01                          00024500
024600     END-IF                                                       00024600
024700     MOVE ORG-ID OF P-DDDTDP01                                    00024700
024800       TO ORG-ID OF DCLXXX-DEPT                                   00024800
024900     .                                                            00024900
025000                                                                  00025000
025100                                                                  00025100
025200 115-CONNECT-TO-ORACLE.                                           00025200
025300     CALL Z-ORA-CONNECT USING XXXN001A                            00025300
025400                              SQLCA                               00025400
025500     IF NOT SUCCESS                                               00025500
025600       MOVE SQLCODE TO WS-SQLCODE                                 00025600
025700       MOVE SPACES  TO IS-RTRN-MSG-TXT                            00025700
025800       STRING 'NNNS0573 - Error connecting to Oracle. Sqlcode ='  00025800
025900               WS-SQLCODE                                         00025900
026000               DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT             00026000
026100     END-IF                                                       00026100
026200     .                                                            00026200
026300                                                                  00026300
026400                                                                  00026400
026500* ================================================================00026500
026600* Stuff to do on exit.                                            00026600
026700* ================================================================00026700
026800 120-EXIT-STUFF.                                                  00026800
026900     IF SUCCESS                                                   00026900
027000       IF NOT EXIT-CLOSE-CURSOR                                   00027000
027100         PERFORM 130-MOVE-DCL-2-PDA-FIELDS                        00027100
027200       END-IF                                                     00027200
027300       ADD WS-CHECKPOINT-INC TO YYYN005A-CHKPT-CNT                00027300
027400     END-IF                                                       00027400
027500     IF (YYYN005A-ORACLE       OR EXIT-PUT-INSERT-ROW             00027500
027600         OR EXIT-PUT-PURGE-ROW OR EXIT-PUT-MODIFY-ROW)            00027600
027700       PERFORM 125-CONNECT-TO-DB2                                 00027700
027800     END-IF                                                       00027800
027900     MOVE SQLCODE TO DB2-SQL-CODE                                 00027900
028000     .                                                            00028000
028100                                                                  00028100
028200                                                                  00028200
028300 125-CONNECT-TO-DB2.                                              00028300
028400     CALL Z-DB2-CONNECT         USING XXXN001A                    00028400
028500                                      SQLCA                       00028500
028600     .                                                            00028600
028700                                                                  00028700
028800                                                                  00028800
028900* ================================================================00028900
029000* Move the elementary fields of the DCL to parameter data area.   00029000
029100* ================================================================00029100
029200 130-MOVE-DCL-2-PDA-FIELDS.                                       00029200
029300     MOVE XXX-DEPT-NBR OF DCLXXX-DEPT                             00029300
029400       TO XXX-DEPT-NBR OF P-DDDTDP01                              00029400
029500     MOVE STR-SUB-DEPT-ID OF DCLXXX-DEPT                          00029500
029600       TO STR-SUB-DEPT-ID OF P-DDDTDP01                           00029600
029700     MOVE DEPT-NM OF DCLXXX-DEPT TO DEPT-NM OF P-DDDTDP01         00029700
029800     MOVE DEPT-ABB OF DCLXXX-DEPT TO DEPT-ABB OF P-DDDTDP01       00029800
029900     MOVE REPT-GRP-CD OF DCLXXX-DEPT TO REPT-GRP-CD OF P-DDDTDP01 00029900
030000     MOVE GRPRFT-LO-PCT OF DCLXXX-DEPT                            00030000
030100       TO GRPRFT-LO-PCT OF P-DDDTDP01                             00030100
030200     MOVE GRPRFT-HI-PCT OF DCLXXX-DEPT                            00030200
030300       TO GRPRFT-HI-PCT OF P-DDDTDP01                             00030300
030400     MOVE SHRNK-LO-PCT OF DCLXXX-DEPT                             00030400
030500       TO SHRNK-LO-PCT OF P-DDDTDP01                              00030500
030600     MOVE SHRNK-HI-PCT OF DCLXXX-DEPT                             00030600
030700       TO SHRNK-HI-PCT OF P-DDDTDP01                              00030700
030800     MOVE LST-UPDT-USR-ID OF DCLXXX-DEPT                          00030800
030900       TO LST-UPDT-USR-ID OF P-DDDTDP01                           00030900
031000     MOVE LST-UPDT-TS OF DCLXXX-DEPT TO LST-UPDT-TS OF P-DDDTDP01 00031000
031100     MOVE ORG-ID OF DCLXXX-DEPT                                   00031100
031200       TO ORG-ID OF P-DDDTDP01                                    00031200
031300     .                                                            00031300
031400                                                                  00031400
031500                                                                  00031500
031600* ================================================================00031600
031700* Code required to do static sql including cursor, select, fetch, 00031700
031800* update, insert, and delete operations.                          00031800
031900* ================================================================00031900
032000 1000-EXIT-OPEN-CURSOR.                                           00032000
032100     EVALUATE TRUE                                                00032100
032200       WHEN DDDXDP01                                              00032200
032300         EXEC SQL                                                 00032300
032400           OPEN DDDXDP01                                          00032400
032500         END-EXEC                                                 00032500
032600       WHEN DDDXDP02                                              00032600
032700         EXEC SQL                                                 00032700
032800           OPEN DDDXDP02                                          00032800
032900         END-EXEC                                                 00032900
033000       WHEN OTHER                                                 00033000
033100         SET FAILURE TO TRUE                                      00033100
033200         MOVE 'NNNS0573 - Invalid open cursor ID.'                00033200
033300           TO IS-RTRN-MSG-TXT OF XXXN001A                         00033300
033400     END-EVALUATE                                                 00033400
033500     .                                                            00033500
033600                                                                  00033600
033700                                                                  00033700
033800 1100-EXIT-CLOSE-CURSOR.                                          00033800
033900     EVALUATE TRUE                                                00033900
034000       WHEN DDDXDP01                                              00034000
034100         EXEC SQL                                                 00034100
034200           CLOSE DDDXDP01                                         00034200
034300         END-EXEC                                                 00034300
034400       WHEN DDDXDP02                                              00034400
034500         EXEC SQL                                                 00034500
034600           CLOSE DDDXDP02                                         00034600
034700         END-EXEC                                                 00034700
034800       WHEN OTHER                                                 00034800
034900         SET FAILURE TO TRUE                                      00034900
035000         MOVE 'NNNS0573 - Invalid close cursor ID.'               00035000
035100           TO IS-RTRN-MSG-TXT OF XXXN001A                         00035100
035200     END-EVALUATE                                                 00035200
035300     .                                                            00035300
035400                                                                  00035400
035500                                                                  00035500
035600 1200-EXIT-GET-UNIQUE-ROW.                                        00035600
035700     EXEC SQL                                                     00035700
035800         SELECT XXX_DEPT_NBR,                                     00035800
035900                STR_SUB_DEPT_ID,                                  00035900
036000                DEPT_NM,                                          00036000
036100                DEPT_ABB,                                         00036100
036200                REPT_GRP_CD,                                      00036200
036300                GRPRFT_LO_PCT,                                    00036300
036400                GRPRFT_HI_PCT,                                    00036400
036500                SHRNK_LO_PCT,                                     00036500
036600                SHRNK_HI_PCT,                                     00036600
036700                LST_UPDT_USR_ID,                                  00036700
036800                LST_UPDT_TS,                                      00036800
036900                ORG_ID                                            00036900
037000         INTO   :DCLXXX-DEPT.XXX-DEPT-NBR,                        00037000
037100                :DCLXXX-DEPT.STR-SUB-DEPT-ID,                     00037100
037200                :DCLXXX-DEPT.DEPT-NM,                             00037200
037300                :DCLXXX-DEPT.DEPT-ABB,                            00037300
037400                :DCLXXX-DEPT.REPT-GRP-CD,                         00037400
037500                :DCLXXX-DEPT.GRPRFT-LO-PCT,                       00037500
037600                :DCLXXX-DEPT.GRPRFT-HI-PCT,                       00037600
037700                :DCLXXX-DEPT.SHRNK-LO-PCT,                        00037700
037800                :DCLXXX-DEPT.SHRNK-HI-PCT,                        00037800
037900                :DCLXXX-DEPT.LST-UPDT-USR-ID,                     00037900
038000                :DCLXXX-DEPT.LST-UPDT-TS,                         00038000
038100                :DCLXXX-DEPT.ORG-ID                               00038100
038200         FROM   XXX_DEPT                                          00038200
038300         WHERE  XXX_DEPT_NBR = :DCLXXX-DEPT.XXX-DEPT-NBR          00038300
038400         AND    STR_SUB_DEPT_ID = :DCLXXX-DEPT.STR-SUB-DEPT-ID    00038400
038500     END-EXEC                                                     00038500
038600                                                                  00038600
038700     PERFORM 1700-CHECK-NULL-COLUMNS                              00038700
038800     .                                                            00038800
038900                                                                  00038900
039000                                                                  00039000
039100 1300-EXIT-GET-NEXT-ROW.                                          00039100
039200     EVALUATE TRUE                                                00039200
039300       WHEN DDDXDP01                                              00039300
039400         PERFORM 1301-FETCH-DDDXDP01                              00039400
039500       WHEN DDDXDP02                                              00039500
039600         PERFORM 1302-FETCH-DDDXDP02                              00039600
039700       WHEN OTHER                                                 00039700
039800         SET FAILURE TO TRUE                                      00039800
039900         MOVE 'NNNS0573 - Invalid fetch cursor ID.'               00039900
040000           TO IS-RTRN-MSG-TXT OF XXXN001A                         00040000
040100     END-EVALUATE                                                 00040100
040200                                                                  00040200
040300     PERFORM 1700-CHECK-NULL-COLUMNS                              00040300
040400     .                                                            00040400
040500                                                                  00040500
040600                                                                  00040600
040700 1301-FETCH-DDDXDP01.                                             00040700
040800     EXEC SQL                                                     00040800
040900         FETCH DDDXDP01                                           00040900
041000         INTO  :DCLXXX-DEPT.XXX-DEPT-NBR,                         00041000
041100               :DCLXXX-DEPT.STR-SUB-DEPT-ID,                      00041100
041200               :DCLXXX-DEPT.DEPT-NM,                              00041200
041300               :DCLXXX-DEPT.DEPT-ABB,                             00041300
041400               :DCLXXX-DEPT.REPT-GRP-CD,                          00041400
041500               :DCLXXX-DEPT.GRPRFT-LO-PCT,                        00041500
041600               :DCLXXX-DEPT.GRPRFT-HI-PCT,                        00041600
041700               :DCLXXX-DEPT.SHRNK-LO-PCT,                         00041700
041800               :DCLXXX-DEPT.SHRNK-HI-PCT,                         00041800
041900               :DCLXXX-DEPT.LST-UPDT-USR-ID,                      00041900
042000               :DCLXXX-DEPT.LST-UPDT-TS,                          00042000
042100               :DCLXXX-DEPT.ORG-ID                                00042100
042200     END-EXEC                                                     00042200
042300     .                                                            00042300
042400                                                                  00042400
042500                                                                  00042500
042600 1302-FETCH-DDDXDP02.                                             00042600
042700     EXEC SQL                                                     00042700
042800         FETCH DDDXDP02                                           00042800
042900         INTO  :DCLXXX-DEPT.XXX-DEPT-NBR,                         00042900
043000               :DCLXXX-DEPT.STR-SUB-DEPT-ID,                      00043000
043100               :DCLXXX-DEPT.DEPT-NM,                              00043100
043200               :DCLXXX-DEPT.DEPT-ABB,                             00043200
043300               :DCLXXX-DEPT.REPT-GRP-CD,                          00043300
043400               :DCLXXX-DEPT.GRPRFT-LO-PCT,                        00043400
043500               :DCLXXX-DEPT.GRPRFT-HI-PCT,                        00043500
043600               :DCLXXX-DEPT.SHRNK-LO-PCT,                         00043600
043700               :DCLXXX-DEPT.SHRNK-HI-PCT,                         00043700
043800               :DCLXXX-DEPT.LST-UPDT-USR-ID,                      00043800
043900               :DCLXXX-DEPT.LST-UPDT-TS,                          00043900
044000               :DCLXXX-DEPT.ORG-ID                                00044000
044100     END-EXEC                                                     00044100
044200     .                                                            00044200
044300                                                                  00044300
044400                                                                  00044400
044500 1400-EXIT-PUT-MODIFY-ROW.                                        00044500
044600     PERFORM 1800-EDIT-NULL-INDICATORS                            00044600
044700     PERFORM 2040-GET-DATE-AND-USER                               00044700
044800                                                                  00044800
044900     IF SUCCESS                                                   00044900
045000       PERFORM 1405-GET-CURR-VALUES                               00045000
045100       PERFORM 1410-DO-MODIFY-ROW                                 00045100
045200     END-IF                                                       00045200
045300     .                                                            00045300
045400                                                                  00045400
045500                                                                  00045500
045600 1405-GET-CURR-VALUES.                                            00045600
045700     EXEC SQL                                                     00045700
045800         SELECT DEPT_NM                                           00045800
045900           INTO :WS-DEPT-NM                                       00045900
046000           FROM XXX_DEPT                                          00046000
046100         WHERE  XXX_DEPT_NBR = :DCLXXX-DEPT.XXX-DEPT-NBR          00046100
046200         AND    STR_SUB_DEPT_ID = :DCLXXX-DEPT.STR-SUB-DEPT-ID    00046200
046300     END-EXEC                                                     00046300
046400                                                                  00046400
046500     IF SQLCODE NOT = 0                                           00046500
046600       SET  FAILURE TO TRUE                                       00046600
046700       MOVE SQLCODE TO WS-SQLCODE                                 00046700
046800       STRING 'NNNS0573 - Error getting current values,'          00046800
046900              'RC=' WS-SQLCODE '.'                                00046900
047000           DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                 00047000
047100     END-IF                                                       00047100
047200     .                                                            00047200
047300                                                                  00047300
047400                                                                  00047400
047500 1410-DO-MODIFY-ROW.                                              00047500
047600     MOVE YYYC0127-TS   TO LST-UPDT-TS     OF DCLXXX-DEPT         00047600
047700     MOVE YYYC0107-USER TO LST-UPDT-USR-ID OF DCLXXX-DEPT         00047700
047800                                                                  00047800
047900     PERFORM 5000-CALL-NNNS0573-CUD-ROUTINE                       00047900
048000                                                                  00048000
048100     IF SQLCODE = 0                                               00048100
048200       MOVE 1 TO WS-CHECKPOINT-INC                                00048200
048300       SET YYYN110A-UPD TO TRUE                                   00048300
048400       SET STD-UPD TO TRUE                                        00048400
048500       SET MODIFY-OPERATION     TO TRUE                           00048500
048600       IF (WS-DEPT-NM        NOT = DEPT-NM OF DCLXXX-DEPT)        00048600
048700          SET STAGE-EVENT TO TRUE                                 00048700
048800       END-IF                                                     00048800
048900       PERFORM 2000-DENORM-PROCESS                                00048900
049000     END-IF                                                       00049000
049100     .                                                            00049100
049200                                                                  00049200
049300                                                                  00049300
049400 1500-EXIT-PUT-INSERT-ROW.                                        00049400
049500     PERFORM 1800-EDIT-NULL-INDICATORS                            00049500
049600                                                                  00049600
049700     PERFORM 2040-GET-DATE-AND-USER                               00049700
049800     IF SUCCESS                                                   00049800
049900       PERFORM 1510-D0-INSERT-ROW                                 00049900
050000     END-IF                                                       00050000
050100     .                                                            00050100
050200                                                                  00050200
050300                                                                  00050300
050400 1510-D0-INSERT-ROW.                                              00050400
050500     PERFORM 1520-EXIT-GET-ORG-ID                                 00050500
050600     IF SUCCESS                                                   00050600
050700       MOVE YYYC0127-TS   TO LST-UPDT-TS     OF DCLXXX-DEPT       00050700
050800       MOVE YYYC0107-USER TO LST-UPDT-USR-ID OF DCLXXX-DEPT       00050800
050900                                                                  00050900
051000       PERFORM 5000-CALL-NNNS0573-CUD-ROUTINE                     00051000
051100                                                                  00051100
051200       IF SQLCODE = 0                                             00051200
051300         MOVE 1 TO WS-CHECKPOINT-INC                              00051300
051400         SET YYYN110A-ADD TO TRUE                                 00051400
051500         SET STD-ADD TO TRUE                                      00051500
051600         PERFORM 2000-DENORM-PROCESS                              00051600
051700       END-IF                                                     00051700
051800     END-IF                                                       00051800
051900     .                                                            00051900
052000                                                                  00052000
052100                                                                  00052100
052200 1520-EXIT-GET-ORG-ID.                                            00052200
052300     EXEC SQL                                                     00052300
052400         SELECT GREATEST (MAX (LOC.ORG_ID), MAX (DEPT.ORG_ID))    00052400
052500         INTO   :DCLXXX-DEPT.ORG-ID                               00052500
052600         FROM   XXX_DEPT DEPT, LOCATION LOC                       00052600
052700         WITH UR                                                  00052700
052800     END-EXEC                                                     00052800
052900                                                                  00052900
053000     EVALUATE TRUE                                                00053000
053100       WHEN SQLCODE = 0                                           00053100
053200         COMPUTE ORG-ID OF DCLXXX-DEPT =                          00053200
053300                 ORG-ID OF DCLXXX-DEPT + 1                        00053300
053400       WHEN OTHER                                                 00053400
053500         SET  FAILURE TO TRUE                                     00053500
053600         MOVE 'NNNS0573 - Error getting ORG_ID!'                  00053600
053700           TO IS-RTRN-MSG-TXT                                     00053700
053800     END-EVALUATE                                                 00053800
053900     .                                                            00053900
054000                                                                  00054000
054100                                                                  00054100
054200 1600-EXIT-PUT-PURGE-ROW.                                         00054200
054300                                                                  00054300
054310     PERFORM 4500-CALL-MMMS0304-RI-DEL-CHK                        00054310
054320     IF SUCCESS                                                   00054320
054400       PERFORM 5000-CALL-NNNS0573-CUD-ROUTINE                     00054400
054500                                                                  00054500
054600       IF SQLCODE = 0                                             00054600
054700         MOVE 1 TO WS-CHECKPOINT-INC                              00054700
054800         SET YYYN110A-DEL TO TRUE                                 00054800
054900         SET STD-DEL TO TRUE                                      00054900
055000         PERFORM 2000-DENORM-PROCESS                              00055000
055100       END-IF                                                     00055100
055110     END-IF                                                       00055110
055200     .                                                            00055200
055300                                                                  00055300
055400                                                                  00055400
055500* ================================================================00055500
055600* Initialize NULL variables if the column is set to NULL.         00055600
055700* ================================================================00055700
055800 1700-CHECK-NULL-COLUMNS.                                         00055800
055900     EXIT                                                         00055900
056000     .                                                            00056000
056100                                                                  00056100
056200                                                                  00056200
056300* ================================================================00056300
056400* Make sure the null indicators are valid.                        00056400
056500* ================================================================00056500
056600 1800-EDIT-NULL-INDICATORS.                                       00056600
056700     EXIT                                                         00056700
056800     .                                                            00056800
056900                                                                  00056900
057000                                                                  00057000
057100* =========================================================       00057100
057200* Misc functions...                                               00057200
057300* =========================================================       00057300
057400 2000-DENORM-PROCESS.                                             00057400
057500     MOVE YYYN005A-SYS-ENV      TO YYYN110A-SYS-ENV               00057500
057600     PERFORM 2010-CALL-CONTROL-SUBR                               00057600
057700     IF SUCCESS                                                   00057700
057800        PERFORM 2030-ISSUE-EVENTS                                 00057800
057900     END-IF                                                       00057900
058000     .                                                            00058000
058100                                                                  00058100
058200                                                                  00058200
058300 2010-CALL-CONTROL-SUBR.                                          00058300
058400     SET WWWC0100-GET-TASK  TO TRUE                               00058400
058500     CALL WWWS0100-CONTROL-SUBR USING                             00058500
058600         XXXN001A                                                 00058600
058700         WWWC0100                                                 00058700
058800     .                                                            00058800
058900                                                                  00058900
059000                                                                  00059000
059100                                                                  00059100
059200                                                                  00059200
059300 2030-ISSUE-EVENTS.                                               00059300
059400     INITIALIZE ZZZC0550                                          00059400
059500     SET YYYN110A-ORACLE    TO TRUE                               00059500
059600     SET DEPARTMENT-EVENT   TO TRUE                               00059600
059700     MOVE XXX-DEPT-NBR OF P-DDDTDP01                              00059700
059800       TO XXX-DEPT-NBR OF ZZZC0125                                00059800
059900          XXX-DEPT-NBR      OF ZZZC0550-DEPT-DATA                 00059900
060000     MOVE STR-SUB-DEPT-ID OF P-DDDTDP01                           00060000
060100       TO STR-SUB-DEPT-ID OF ZZZC0125                             00060100
060200          STR-SUB-DEPT-ID   OF ZZZC0550-DEPT-DATA                 00060200
060300                                                                  00060300
060400     MOVE 'SDPM'                TO ZZZC0197-TRX-ID                00060400
060500     MOVE ZZZC0125              TO ZZZC0197-TRX-REC               00060500
060600     MOVE 'XXXS0512'            TO ZZZC0197-PROGRAM               00060600
060700     MOVE '    '                TO ZZZC0197-USER                  00060700
060800     MOVE YYYN005A-SYS-ENV      TO YYYN110A-SYS-ENV               00060800
060900     CALL ZZZS0197-EVENT-MGR USING                                00060900
061000          XXXN001A                                                00061000
061100          YYYN110A                                                00061100
061200          ZZZC0197                                                00061200
061300                                                                  00061300
061400     IF SQLCODE = 0 AND WWWC0100-CREATE-SCAN-EVENT AND            00061400
061500        STAGE-EVENT                                               00061500
061600         SET  MODIFY-OPERATION      TO TRUE                       00061600
061700         MOVE ZZZC0550              TO ZZZC0197-TRX-REC           00061700
061800         MOVE ZZZC0550-TRX          TO ZZZC0197-TRX-ID            00061800
061900                                       ZZZC0197-TRX-CD            00061900
062000         MOVE 'NNNS0573'            TO ZZZC0197-PROGRAM           00062000
062100         MOVE YYYC0107-USER         TO ZZZC0197-USER              00062100
062200         MOVE YYYN005A-SYS-ENV      TO YYYN110A-SYS-ENV           00062200
062300         CALL ZZZS0197-EVENT-MGR USING                            00062300
062400              XXXN001A                                            00062400
062500              YYYN110A                                            00062500
062600              ZZZC0197                                            00062600
062700     END-IF                                                       00062700
062800     .                                                            00062800
062900                                                                  00062900
063000                                                                  00063000
063100 2040-GET-DATE-AND-USER.                                          00063100
063200     CALL Z-DATE-FUNCTIONS USING                                  00063200
063300         XXXN001A                                                 00063300
063400         YYYC0127                                                 00063400
063500                                                                  00063500
063600     IF  SUCCESS                                                  00063600
063700     AND YYYN005A-CICS-ENV                                        00063700
063800     CALL Z-GET-CICS-USER-ID USING                                00063800
063900         EIBLK    WS-DUMMY                                        00063900
064000         XXXN001A YYYC0107                                        00064000
064100     ELSE                                                         00064100
064200       MOVE 'NNNS0573' TO YYYC0107-USER                           00064200
064300     END-IF                                                       00064300
064400     .                                                            00064400
064500                                                                  00064500
064600 4500-CALL-MMMS0304-RI-DEL-CHK.                                   00064600
064610     INITIALIZE MMMC0304                                          00064610
064620     MOVE XXX-DEPT-NBR    OF DCLXXX-DEPT                          00064620
064630       TO MMMC0304-XXX-DEPT-NBR                                   00064630
064640     MOVE STR-SUB-DEPT-ID OF DCLXXX-DEPT                          00064640
064650       TO MMMC0304-STR-SUB-DEPT-ID                                00064650
064660     SET MMMC0304-DELETE-CHECK TO TRUE                            00064660
064670     SET MMMC0304-XXX-DEPT     TO TRUE                            00064670
064680     SET MMMC0304-ORACLE       TO TRUE                            00064680
064690     CALL MMMS0304-RI-DEL-CHK USING                               00064690
064691          XXXN001A                                                00064691
064692          MMMC0304                                                00064692
064693     .                                                            00064693
064694                                                                  00064694
064695                                                                  00064695
064700 5000-CALL-NNNS0573-CUD-ROUTINE.                                  00064700
064800     CALL XXXU0573-ORACLE-UPDATE USING                            00064800
064900          XXXN001A                                                00064900
065000          SQLCA                                                   00065000
065100          YYYN005A                                                00065100
065200          NNNN0000-PARMS                                          00065200
065300          DDDTDP01                                                00065300
065400     .                                                            00065400
065500* ================================================================00065500
065600* Special sql or functions to be performed by this subroutine.    00065600
065700* ================================================================00065700
065800 10000-DO-SPECIAL-IO-FUNCS.                                       00065800
065900     EXIT                                                         00065900
066000     .                                                            00066000
066100                                                                  00066100
