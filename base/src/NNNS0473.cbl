000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    NNNS0473.                                         00000200
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
001400* Table Name  : XXXL-LOC-CLS-AD-ZN , DDDTCZ01                     00001400
001500* Table Handle: 00473                                             00001500
001600*                                                                 00001600
001700* Special Func: None at time of generation.                       00001700
002900* ----------------------------------------------------------------00002900
003000 ENVIRONMENT DIVISION.                                            00003000
003100 CONFIGURATION SECTION.                                           00003100
003200 DATA DIVISION.                                                   00003200
003300 WORKING-STORAGE SECTION.                                         00003300
003400                                                                  00003400
003500* ========================< MISC STUFF >==========================00003500
003600* Misc working storage variables go here.                         00003600
003700* ================================================================00003700
003800 01 WS-DUMMY                           PIC X(1) VALUE SPACES.     00003800
003900 01 WS-CHECKPOINT-INC                  PIC S9(4) COMP VALUE 0.    00003900
004000                                                                  00004000
004100 01 WS-OLD-KEY                         PIC 9(9) VALUE 0.          00004100
004200 01 REDEFINES WS-OLD-KEY.                                         00004200
004300     05 FILLER                         PIC X(4).                  00004300
004400     05 WS-OLD-KEY-A5                  PIC X(5).                  00004400
004500 01 WS-SQLCODE                         PIC ----9.                 00004500
004510 01 MMMC0335-RI-INSERT-CHK             PIC X(8)  VALUE 'MMMS0335'.00004510
004600                                                                  00004600
004700* ========================< COPY BOOKS >==========================00004700
004800* Place all copy books in this section.                           00004800
004900* ================================================================00004900
005000 COPY XXXEIBLK.                                                   00005000
005100 COPY HHHTCZ01.                                                   00005100
005200 COPY YYYN000A.                                                   00005200
005300 COPY YYYN110A.                                                   00005300
005400 COPY WWWC0100.                                                   00005400
005500 COPY YYYC0107.                                                   00005500
005600 COPY ZZZC0197.                                                   00005600
005700 COPY ZZZC0032.                                                   00005700
005800 COPY MMMC0161.                                                   00005800
005900 COPY MMMK001B.                                                   00005900
006000 COPY ZZZC0094.                                                   00006000
006100 COPY NNNN000U.                                                   00006100
006110 COPY MMMC0335.                                                   00006110
006200                                                                  00006200
006300* ========================< DCL GENS >============================00006300
006400* Place all DCL gens in this section.                             00006400
006500* ================================================================00006500
006600     EXEC SQL                                                     00006600
006700       INCLUDE DDDTCZ01                                           00006700
006800     END-EXEC.                                                    00006800
006900                                                                  00006900
007000* ========================< CURSORS >=============================00007000
007100* Place all cursors in this section.                              00007100
007200* ================================================================00007200
007300                                                                  00007300
007400* --------------------------------------------------              00007400
007500* DDDXCZ01 cursor declaration.                                    00007500
007600* --------------------------------------------------              00007600
007700     EXEC SQL                                                     00007700
007800         DECLARE DDDXCZ01 CURSOR WITH HOLD FOR SELECT             00007800
007900             LOC_TYP_CD,                                          00007900
008000             LOC_NBR,                                             00008000
008100             ITM_CLS_CD,                                          00008100
008200             AD_ZONE,                                             00008200
008300             AD_ZONE_EXCP                                         00008300
008400         FROM     XXXL_LOC_CLS_AD_ZN                              00008400
008500         WHERE   (LOC_TYP_CD = :DCLXXXL-LOC-CLS-AD-ZN.LOC-TYP-CD) 00008500
008600         AND     (LOC_NBR    = :DCLXXXL-LOC-CLS-AD-ZN.LOC-NBR)    00008600
008700         AND     (ITM_CLS_CD >= :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD)00008700
008800         ORDER BY                                                 00008800
008900             LOC_TYP_CD,                                          00008900
009000             LOC_NBR,                                             00009000
009100             ITM_CLS_CD                                           00009100
009200     END-EXEC.                                                    00009200
009300* --------------------------------------------------              00009300
009400* DDDXCZ02 cursor declaration.                                    00009400
009500* --------------------------------------------------              00009500
009600     EXEC SQL                                                     00009600
009700         DECLARE DDDXCZ02 CURSOR WITH HOLD FOR SELECT             00009700
009800             LOC_TYP_CD,                                          00009800
009900             LOC_NBR,                                             00009900
010000             ITM_CLS_CD,                                          00010000
010100             AD_ZONE,                                             00010100
010200             AD_ZONE_EXCP                                         00010200
010300         FROM     XXXL_LOC_CLS_AD_ZN                              00010300
010400         WHERE   (ITM_CLS_CD = :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD) 00010400
010500         AND NOT                                                  00010500
010600                 (ITM_CLS_CD  =                                   00010600
010700                       :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD AND      00010700
010800                  LOC_TYP_CD  < :DCLXXXL-LOC-CLS-AD-ZN.LOC-TYP-CD)00010800
010900         AND NOT                                                  00010900
011000                 (ITM_CLS_CD  =                                   00011000
011100                       :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD AND      00011100
011200                  LOC_TYP_CD  =                                   00011200
011300                        :DCLXXXL-LOC-CLS-AD-ZN.LOC-TYP-CD AND     00011300
011400                  LOC_NBR  < :DCLXXXL-LOC-CLS-AD-ZN.LOC-NBR)      00011400
011500         ORDER BY                                                 00011500
011600             ITM_CLS_CD,                                          00011600
011700             LOC_TYP_CD,                                          00011700
011800             LOC_NBR                                              00011800
011900     END-EXEC.                                                    00011900
012000* --------------------------------------------------              00012000
012100* DDDXCZ03 cursor declaration.                                    00012100
012200* --------------------------------------------------              00012200
012300     EXEC SQL                                                     00012300
012400         DECLARE DDDXCZ03 CURSOR WITH HOLD FOR SELECT             00012400
012500             LOC_TYP_CD,                                          00012500
012600             LOC_NBR,                                             00012600
012700             ITM_CLS_CD,                                          00012700
012800             AD_ZONE,                                             00012800
012900             AD_ZONE_EXCP                                         00012900
013000         FROM     XXXL_LOC_CLS_AD_ZN                              00013000
013100         WHERE   (AD_ZONE = :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE)       00013100
013200         AND NOT                                                  00013200
013300                 (AD_ZONE  = :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE AND   00013300
013400                  ITM_CLS_CD  < :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD)00013400
013500         AND NOT                                                  00013500
013600                 (AD_ZONE  = :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE AND   00013600
013700                  ITM_CLS_CD  =                                   00013700
013800                        :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD AND     00013800
013900                  LOC_TYP_CD  < :DCLXXXL-LOC-CLS-AD-ZN.LOC-TYP-CD)00013900
014000         AND NOT                                                  00014000
014100                 (AD_ZONE  = :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE AND   00014100
014200                  ITM_CLS_CD  =                                   00014200
014300                        :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD AND     00014300
014400                  LOC_TYP_CD  =                                   00014400
014500                        :DCLXXXL-LOC-CLS-AD-ZN.LOC-TYP-CD AND     00014500
014600                  LOC_NBR  < :DCLXXXL-LOC-CLS-AD-ZN.LOC-NBR)      00014600
014700         ORDER BY                                                 00014700
014800             AD_ZONE,                                             00014800
014900             ITM_CLS_CD,                                          00014900
015000             LOC_TYP_CD,                                          00015000
015100             LOC_NBR                                              00015100
015200     END-EXEC.                                                    00015200
015300* --------------------------------------------------              00015300
015400* DDDXCZ04 cursor declaration.                                    00015400
015500* --------------------------------------------------              00015500
015600     EXEC SQL                                                     00015600
015700         DECLARE DDDXCZ04 CURSOR WITH HOLD FOR SELECT             00015700
015800             LOC_TYP_CD,                                          00015800
015900             LOC_NBR,                                             00015900
016000             ITM_CLS_CD,                                          00016000
016100             AD_ZONE,                                             00016100
016200             AD_ZONE_EXCP                                         00016200
016300         FROM     XXXL_LOC_CLS_AD_ZN                              00016300
016400         WHERE   (ITM_CLS_CD = :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD) 00016400
016500         AND NOT                                                  00016500
016600                 (ITM_CLS_CD  =                                   00016600
016700                       :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD AND      00016700
016800                  AD_ZONE  < :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE)      00016800
016900         AND NOT                                                  00016900
017000                 (ITM_CLS_CD  =                                   00017000
017100                       :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD AND      00017100
017200                  AD_ZONE  = :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE AND   00017200
017300                  LOC_TYP_CD  < :DCLXXXL-LOC-CLS-AD-ZN.LOC-TYP-CD)00017300
017400         AND NOT                                                  00017400
017500                 (ITM_CLS_CD  =                                   00017500
017600                       :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD AND      00017600
017700                  AD_ZONE  = :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE AND   00017700
017800                  LOC_TYP_CD  =                                   00017800
017900                        :DCLXXXL-LOC-CLS-AD-ZN.LOC-TYP-CD AND     00017900
018000                  LOC_NBR  < :DCLXXXL-LOC-CLS-AD-ZN.LOC-NBR)      00018000
018100         ORDER BY                                                 00018100
018200             ITM_CLS_CD,                                          00018200
018300             AD_ZONE,                                             00018300
018400             LOC_TYP_CD,                                          00018400
018500             LOC_NBR                                              00018500
018600     END-EXEC.                                                    00018600
018700                                                                  00018700
018800 LINKAGE SECTION.                                                 00018800
018900 COPY XXXN001A.                                                   00018900
019000     EXEC SQL                                                     00019000
019100         INCLUDE SQLCA                                            00019100
019200     END-EXEC.                                                    00019200
019300 COPY YYYN005A.                                                   00019300
019400 COPY NNNN0000.                                                   00019400
019500 COPY PPPTCZ01.                                                   00019500
019600                                                                  00019600
019700 PROCEDURE DIVISION USING                                         00019700
019800     XXXN001A                                                     00019800
019900     SQLCA                                                        00019900
020000     YYYN005A                                                     00020000
020100     NNNN0000-PARMS                                               00020100
020200     P-DDDTCZ01                                                   00020200
020300     .                                                            00020300
020400                                                                  00020400
020500************************************************************      00020500
020600* MAIN PROGRAM LINE.                                              00020600
020700************************************************************      00020700
020800 0000-EXIT-DISPATCHER.                                            00020800
020900     PERFORM 100-INITIALIZATION                                   00020900
021000     EVALUATE TRUE                                                00021000
021100       WHEN NOT SUCCESS                                           00021100
021200          CONTINUE                                                00021200
021300       WHEN EXIT-OPEN-CURSOR                                      00021300
021400          PERFORM 1000-EXIT-OPEN-CURSOR                           00021400
021500       WHEN EXIT-CLOSE-CURSOR                                     00021500
021600          PERFORM 1100-EXIT-CLOSE-CURSOR                          00021600
021700       WHEN EXIT-GET-UNIQUE-ROW                                   00021700
021800          PERFORM 1200-EXIT-GET-UNIQUE-ROW                        00021800
021900       WHEN EXIT-GET-NEXT-ROW                                     00021900
022000          PERFORM 1300-EXIT-GET-NEXT-ROW                          00022000
022100       WHEN EXIT-PUT-MODIFY-ROW                                   00022100
022200          PERFORM 1400-EXIT-PUT-MODIFY-ROW                        00022200
022300       WHEN EXIT-PUT-INSERT-ROW                                   00022300
022400          PERFORM 1500-EXIT-PUT-INSERT-ROW                        00022400
022500       WHEN EXIT-PUT-PURGE-ROW                                    00022500
022600          PERFORM 1600-EXIT-PUT-PURGE-ROW                         00022600
022700       WHEN EXIT-DO-SPECIAL-IO-FUNCS                              00022700
022800          PERFORM 10000-DO-SPECIAL-IO-FUNCS                       00022800
022900     END-EVALUATE                                                 00022900
023000     PERFORM 120-EXIT-STUFF                                       00023000
023100     GOBACK                                                       00023100
023200     .                                                            00023200
023300                                                                  00023300
023400                                                                  00023400
023500* ================================================================00023500
023600* Initialize data areas needed to call the i/o subroutine         00023600
023700* ================================================================00023700
023800 100-INITIALIZATION.                                              00023800
023900     INITIALIZE XXXN001A                                          00023900
024000                DAO-STATUS                                        00024000
024100     MOVE NNNN0000-INDEX-HANDLE TO DDDTCZ01-INDEX-HANDLE          00024100
024200     MOVE 0 TO WS-CHECKPOINT-INC                                  00024200
024300     MOVE 0 TO SQLCODE                                            00024300
024400     MOVE 0 TO SQL-INIT-FLAG                                      00024400
024500     IF NOT EXIT-CLOSE-CURSOR                                     00024500
024600       PERFORM 110-MOVE-PDA-FIELDS-2-DCL                          00024600
024700     END-IF                                                       00024700
024800     IF (YYYN005A-ORACLE       OR EXIT-PUT-INSERT-ROW             00024800
024900         OR EXIT-PUT-PURGE-ROW OR EXIT-PUT-MODIFY-ROW)            00024900
025000       PERFORM 115-CONNECT-TO-ORACLE                              00025000
025100     END-IF                                                       00025100
025200     .                                                            00025200
025300                                                                  00025300
025400                                                                  00025400
025500* ================================================================00025500
025600* Move the elementary fields in the parameter data area to the DCL00025600
025700* ================================================================00025700
025800 110-MOVE-PDA-FIELDS-2-DCL.                                       00025800
025900     MOVE LOC-TYP-CD OF P-DDDTCZ01                                00025900
026000       TO LOC-TYP-CD OF DCLXXXL-LOC-CLS-AD-ZN                     00026000
026100     MOVE LOC-NBR OF P-DDDTCZ01                                   00026100
026200       TO LOC-NBR OF DCLXXXL-LOC-CLS-AD-ZN                        00026200
026300     MOVE ITM-CLS-CD OF P-DDDTCZ01                                00026300
026400       TO ITM-CLS-CD OF DCLXXXL-LOC-CLS-AD-ZN                     00026400
026500     MOVE AD-ZONE OF P-DDDTCZ01                                   00026500
026600       TO AD-ZONE OF DCLXXXL-LOC-CLS-AD-ZN                        00026600
026700     MOVE AD-ZONE-EXCP OF P-DDDTCZ01                              00026700
026800       TO AD-ZONE-EXCP OF DCLXXXL-LOC-CLS-AD-ZN                   00026800
026900     .                                                            00026900
027000                                                                  00027000
027100                                                                  00027100
027200* ================================================================00027200
027300* CONNECTING TO ORACLE DATABASE                                   00027300
027400* ================================================================00027400
027500 115-CONNECT-TO-ORACLE.                                           00027500
027600     CALL Z-ORA-CONNECT USING XXXN001A                            00027600
027700                              SQLCA                               00027700
027800                                                                  00027800
027900     IF NOT SUCCESS                                               00027900
028000       MOVE SQLCODE TO WS-SQLCODE                                 00028000
028100       MOVE SPACES  TO IS-RTRN-MSG-TXT                            00028100
028200       STRING 'NNNS0473 - Error connecting to Oracle. Sqlcode ='  00028200
028300               WS-SQLCODE                                         00028300
028400               DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT             00028400
028500     END-IF                                                       00028500
028600     .                                                            00028600
028700                                                                  00028700
028800* ================================================================00028800
028900* Stuff to do on exit.                                            00028900
029000* ================================================================00029000
029100 120-EXIT-STUFF.                                                  00029100
029200     IF SUCCESS                                                   00029200
029300       IF NOT EXIT-CLOSE-CURSOR                                   00029300
029400         PERFORM 130-MOVE-DCL-2-PDA-FIELDS                        00029400
029500       END-IF                                                     00029500
029600       ADD WS-CHECKPOINT-INC TO YYYN005A-CHKPT-CNT                00029600
029700     END-IF                                                       00029700
029800     IF (YYYN005A-ORACLE       OR EXIT-PUT-INSERT-ROW             00029800
029900         OR EXIT-PUT-PURGE-ROW OR EXIT-PUT-MODIFY-ROW)            00029900
030000        PERFORM 125-CONNECT-TO-DB2                                00030000
030100     END-IF                                                       00030100
030200     MOVE SQLCODE            TO DB2-SQL-CODE                      00030200
030300     .                                                            00030300
030400                                                                  00030400
030500                                                                  00030500
030600* ================================================================00030600
030700* CONNECTING TO DB2 DATABASE                                      00030700
030800* ================================================================00030800
030900 125-CONNECT-TO-DB2.                                              00030900
031000     CALL Z-DB2-CONNECT         USING XXXN001A                    00031000
031100                                      SQLCA                       00031100
031200     .                                                            00031200
031300                                                                  00031300
031400                                                                  00031400
031500* ================================================================00031500
031600* Move the elementary fields of the DCL to parameter data area.   00031600
031700* ================================================================00031700
031800 130-MOVE-DCL-2-PDA-FIELDS.                                       00031800
031900     MOVE LOC-TYP-CD OF DCLXXXL-LOC-CLS-AD-ZN                     00031900
032000       TO LOC-TYP-CD OF P-DDDTCZ01                                00032000
032100     MOVE LOC-NBR OF DCLXXXL-LOC-CLS-AD-ZN                        00032100
032200       TO LOC-NBR OF P-DDDTCZ01                                   00032200
032300     MOVE ITM-CLS-CD OF DCLXXXL-LOC-CLS-AD-ZN                     00032300
032400       TO ITM-CLS-CD OF P-DDDTCZ01                                00032400
032500     MOVE AD-ZONE OF DCLXXXL-LOC-CLS-AD-ZN                        00032500
032600       TO AD-ZONE OF P-DDDTCZ01                                   00032600
032700     MOVE AD-ZONE-EXCP OF DCLXXXL-LOC-CLS-AD-ZN                   00032700
032800       TO AD-ZONE-EXCP OF P-DDDTCZ01                              00032800
032900     .                                                            00032900
033000                                                                  00033000
033100                                                                  00033100
033200* ================================================================00033200
033300* Code required to do static sql including cursor, select, fetch, 00033300
033400* update, insert, and delete operations.                          00033400
033500* ================================================================00033500
033600 1000-EXIT-OPEN-CURSOR.                                           00033600
033700     EVALUATE TRUE                                                00033700
033800       WHEN DDDXCZ01                                              00033800
033900         EXEC SQL                                                 00033900
034000           OPEN DDDXCZ01                                          00034000
034100         END-EXEC                                                 00034100
034200       WHEN DDDXCZ02                                              00034200
034300         EXEC SQL                                                 00034300
034400           OPEN DDDXCZ02                                          00034400
034500         END-EXEC                                                 00034500
034600       WHEN DDDXCZ03                                              00034600
034700         EXEC SQL                                                 00034700
034800           OPEN DDDXCZ03                                          00034800
034900         END-EXEC                                                 00034900
035000       WHEN DDDXCZ04                                              00035000
035100         EXEC SQL                                                 00035100
035200           OPEN DDDXCZ04                                          00035200
035300         END-EXEC                                                 00035300
035400       WHEN OTHER                                                 00035400
035500         SET FAILURE TO TRUE                                      00035500
035600         MOVE 'NNNS0473 - Invalid open cursor ID.'                00035600
035700           TO IS-RTRN-MSG-TXT OF XXXN001A                         00035700
035800     END-EVALUATE                                                 00035800
035900     .                                                            00035900
036000                                                                  00036000
036100                                                                  00036100
036200 1100-EXIT-CLOSE-CURSOR.                                          00036200
036300     EVALUATE TRUE                                                00036300
036400       WHEN DDDXCZ01                                              00036400
036500         EXEC SQL                                                 00036500
036600           CLOSE DDDXCZ01                                         00036600
036700         END-EXEC                                                 00036700
036800       WHEN DDDXCZ02                                              00036800
036900         EXEC SQL                                                 00036900
037000           CLOSE DDDXCZ02                                         00037000
037100         END-EXEC                                                 00037100
037200       WHEN DDDXCZ03                                              00037200
037300         EXEC SQL                                                 00037300
037400           CLOSE DDDXCZ03                                         00037400
037500         END-EXEC                                                 00037500
037600       WHEN DDDXCZ04                                              00037600
037700         EXEC SQL                                                 00037700
037800           CLOSE DDDXCZ04                                         00037800
037900         END-EXEC                                                 00037900
038000       WHEN OTHER                                                 00038000
038100         SET FAILURE TO TRUE                                      00038100
038200         MOVE 'NNNS0473 - Invalid close cursor ID.'               00038200
038300           TO IS-RTRN-MSG-TXT OF XXXN001A                         00038300
038400     END-EVALUATE                                                 00038400
038500     .                                                            00038500
038600                                                                  00038600
038700                                                                  00038700
038800 1200-EXIT-GET-UNIQUE-ROW.                                        00038800
038900     EXEC SQL                                                     00038900
039000         SELECT LOC_TYP_CD,                                       00039000
039100                LOC_NBR,                                          00039100
039200                ITM_CLS_CD,                                       00039200
039300                AD_ZONE,                                          00039300
039400                AD_ZONE_EXCP                                      00039400
039500         INTO   :DCLXXXL-LOC-CLS-AD-ZN.LOC-TYP-CD,                00039500
039600                :DCLXXXL-LOC-CLS-AD-ZN.LOC-NBR,                   00039600
039700                :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD,                00039700
039800                :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE,                   00039800
039900                :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE-EXCP               00039900
040000         FROM   XXXL_LOC_CLS_AD_ZN                                00040000
040100         WHERE  LOC_TYP_CD = :DCLXXXL-LOC-CLS-AD-ZN.LOC-TYP-CD    00040100
040200         AND    LOC_NBR = :DCLXXXL-LOC-CLS-AD-ZN.LOC-NBR          00040200
040300         AND    ITM_CLS_CD = :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD    00040300
040400     END-EXEC                                                     00040400
040500                                                                  00040500
040600     PERFORM 1700-CHECK-NULL-COLUMNS                              00040600
040700     .                                                            00040700
040800                                                                  00040800
040900                                                                  00040900
041000 1300-EXIT-GET-NEXT-ROW.                                          00041000
041100     EVALUATE TRUE                                                00041100
041200       WHEN DDDXCZ01                                              00041200
041300         PERFORM 1301-FETCH-DDDXCZ01                              00041300
041400       WHEN DDDXCZ02                                              00041400
041500         PERFORM 1302-FETCH-DDDXCZ02                              00041500
041600       WHEN DDDXCZ03                                              00041600
041700         PERFORM 1303-FETCH-DDDXCZ03                              00041700
041800       WHEN DDDXCZ04                                              00041800
041900         PERFORM 1304-FETCH-DDDXCZ04                              00041900
042000       WHEN OTHER                                                 00042000
042100         SET FAILURE TO TRUE                                      00042100
042200         MOVE 'NNNS0473 - Invalid fetch cursor ID.'               00042200
042300           TO IS-RTRN-MSG-TXT OF XXXN001A                         00042300
042400     END-EVALUATE                                                 00042400
042500                                                                  00042500
042600     PERFORM 1700-CHECK-NULL-COLUMNS                              00042600
042700     .                                                            00042700
042800                                                                  00042800
042900                                                                  00042900
043000 1301-FETCH-DDDXCZ01.                                             00043000
043100     EXEC SQL                                                     00043100
043200         FETCH DDDXCZ01                                           00043200
043300         INTO  :DCLXXXL-LOC-CLS-AD-ZN.LOC-TYP-CD,                 00043300
043400               :DCLXXXL-LOC-CLS-AD-ZN.LOC-NBR,                    00043400
043500               :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD,                 00043500
043600               :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE,                    00043600
043700               :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE-EXCP                00043700
043800     END-EXEC                                                     00043800
043900     .                                                            00043900
044000                                                                  00044000
044100                                                                  00044100
044200 1302-FETCH-DDDXCZ02.                                             00044200
044300     EXEC SQL                                                     00044300
044400         FETCH DDDXCZ02                                           00044400
044500         INTO  :DCLXXXL-LOC-CLS-AD-ZN.LOC-TYP-CD,                 00044500
044600               :DCLXXXL-LOC-CLS-AD-ZN.LOC-NBR,                    00044600
044700               :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD,                 00044700
044800               :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE,                    00044800
044900               :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE-EXCP                00044900
045000     END-EXEC                                                     00045000
045100     .                                                            00045100
045200                                                                  00045200
045300                                                                  00045300
045400 1303-FETCH-DDDXCZ03.                                             00045400
045500     EXEC SQL                                                     00045500
045600         FETCH DDDXCZ03                                           00045600
045700         INTO  :DCLXXXL-LOC-CLS-AD-ZN.LOC-TYP-CD,                 00045700
045800               :DCLXXXL-LOC-CLS-AD-ZN.LOC-NBR,                    00045800
045900               :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD,                 00045900
046000               :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE,                    00046000
046100               :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE-EXCP                00046100
046200     END-EXEC                                                     00046200
046300     .                                                            00046300
046400                                                                  00046400
046500                                                                  00046500
046600 1304-FETCH-DDDXCZ04.                                             00046600
046700     EXEC SQL                                                     00046700
046800         FETCH DDDXCZ04                                           00046800
046900         INTO  :DCLXXXL-LOC-CLS-AD-ZN.LOC-TYP-CD,                 00046900
047000               :DCLXXXL-LOC-CLS-AD-ZN.LOC-NBR,                    00047000
047100               :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD,                 00047100
047200               :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE,                    00047200
047300               :DCLXXXL-LOC-CLS-AD-ZN.AD-ZONE-EXCP                00047300
047400     END-EXEC                                                     00047400
047500     .                                                            00047500
047600                                                                  00047600
047700                                                                  00047700
047800 1400-EXIT-PUT-MODIFY-ROW.                                        00047800
047900     PERFORM 1800-EDIT-NULL-INDICATORS                            00047900
048000     PERFORM 1410-CHECK-FOR-EVENTS                                00048000
048100     IF SQLCODE = 0                                               00048100
048200       PERFORM 1420-D0-MODIFY-ROW                                 00048200
048300     END-IF                                                       00048300
048400     .                                                            00048400
048500                                                                  00048500
048600                                                                  00048600
048700 1410-CHECK-FOR-EVENTS.                                           00048700
048800     EXIT                                                         00048800
048900     .                                                            00048900
049000                                                                  00049000
049100                                                                  00049100
049200 1420-D0-MODIFY-ROW.                                              00049200
050900     PERFORM 5000-CALL-NNNS0473-CUD-ROUTINE                       00050900
051000     IF SQLCODE = 0                                               00051000
051100       MOVE 1 TO WS-CHECKPOINT-INC                                00051100
051200       SET YYYN110A-UPD TO TRUE                                   00051200
051300       SET LOC-UPD      TO TRUE                                   00051300
051400       PERFORM 2000-DENORM-PROCESS                                00051400
051500     END-IF                                                       00051500
051600     .                                                            00051600
051700                                                                  00051700
051800                                                                  00051800
051900 1500-EXIT-PUT-INSERT-ROW.                                        00051900
052000     PERFORM 1800-EDIT-NULL-INDICATORS                            00052000
052100                                                                  00052100
052200     PERFORM 4600-CALL-MMMS0335-RI-ADD-CHK                        00052200
052300     IF SUCCESS                                                   00052300
053600        PERFORM 5000-CALL-NNNS0473-CUD-ROUTINE                    00053600
053700        IF SQLCODE = 0                                            00053700
053800           MOVE 1 TO WS-CHECKPOINT-INC                            00053800
053900           SET YYYN110A-ADD TO TRUE                               00053900
054000           SET LOC-UPD      TO TRUE                               00054000
054100           PERFORM 2000-DENORM-PROCESS                            00054100
054200        END-IF                                                    00054200
054210     END-IF                                                       00054210
054300     .                                                            00054300
054400                                                                  00054400
054500                                                                  00054500
054600 1600-EXIT-PUT-PURGE-ROW.                                         00054600
055300     PERFORM 5000-CALL-NNNS0473-CUD-ROUTINE                       00055300
055400     IF SQLCODE = 0                                               00055400
055500       MOVE 1 TO WS-CHECKPOINT-INC                                00055500
055600       SET YYYN110A-DEL TO TRUE                                   00055600
055700       SET LOC-UPD      TO TRUE                                   00055700
055800       PERFORM 2000-DENORM-PROCESS                                00055800
055900     END-IF                                                       00055900
056000     .                                                            00056000
056100                                                                  00056100
056200                                                                  00056200
056300* ================================================================00056300
056400* Initialize NULL variables if the column is set to NULL.         00056400
056500* ================================================================00056500
056600 1700-CHECK-NULL-COLUMNS.                                         00056600
056700     EXIT                                                         00056700
056800     .                                                            00056800
056900                                                                  00056900
057000                                                                  00057000
057100* ================================================================00057100
057200* Make sure the null indicators are valid.                        00057200
057300* ================================================================00057300
057400 1800-EDIT-NULL-INDICATORS.                                       00057400
057500     EXIT                                                         00057500
057600     .                                                            00057600
057700                                                                  00057700
057800                                                                  00057800
057900* ==========================================================      00057900
058000* Misc functions...                                               00058000
058100* ==========================================================      00058100
058200 2000-DENORM-PROCESS.                                             00058200
058300     MOVE YYYN005A-SYS-ENV      TO YYYN110A-SYS-ENV               00058300
058400     PERFORM 2010-CALL-CONTROL-SUBR                               00058400
058500     IF  SUCCESS                                                  00058500
058600     AND WWWC0100-NORM-TASK                                       00058600
058700       PERFORM 2020-CALL-SYNC-SUBR                                00058700
058800     END-IF                                                       00058800
058900     IF SUCCESS                                                   00058900
059200          PERFORM 2030-ISSUE-EVENTS                               00059200
059400     END-IF                                                       00059400
059500     .                                                            00059500
059600                                                                  00059600
059700                                                                  00059700
059800 2010-CALL-CONTROL-SUBR.                                          00059800
059900     SET WWWC0100-GET-TASK  TO TRUE                               00059900
060000     CALL WWWS0100-CONTROL-SUBR USING                             00060000
060100         XXXN001A                                                 00060100
060200         WWWC0100                                                 00060200
060300     .                                                            00060300
060400                                                                  00060400
060500                                                                  00060500
060600 2020-CALL-SYNC-SUBR.                                             00060600
060700     SET YYYN110A-LAST-CALL     TO TRUE                           00060700
060710     SET YYYN110A-ORACLE        TO TRUE                           00060710
060800     SET MMMC0161-CZ-IS-CURRENT TO TRUE                           00060800
060900     CALL MMMS0161-SYNC-CZ USING                                  00060900
061000         XXXN001A                                                 00061000
061100         YYYN110A                                                 00061100
061200         MMMC0161                                                 00061200
061300         P-DDDTCZ01                                               00061300
061400     .                                                            00061400
061500                                                                  00061500
061600                                                                  00061600
061700 2030-ISSUE-EVENTS.                                               00061700
061710     SET YYYN110A-ORACLE        TO TRUE                           00061710
061800     PERFORM 2040-GET-CURRENT-USER                                00061800
061900     IF SUCCESS                                                   00061900
062000       MOVE LOC-NBR OF P-DDDTCZ01 TO ST-STORE-NUMBER OF ZZZC0032  00062000
062100                                     LOC-NBR OF ZZZC0094          00062100
062200       SET  ZZZC0032-UPD-FXXX     TO TRUE                         00062200
062300       MOVE ZZZC0032              TO ZZZC0197-TRX-REC             00062300
062400       MOVE 'CUST'                TO ZZZC0197-TRX-ID              00062400
062500       MOVE 'NNNS0473'            TO ZZZC0197-PROGRAM             00062500
062600       MOVE YYYC0107-USER         TO ZZZC0197-USER                00062600
062700       MOVE YYYN005A-SYS-ENV      TO YYYN110A-SYS-ENV             00062700
062800       CALL ZZZS0197-EVENT-MGR USING                              00062800
062900           XXXN001A                                               00062900
063000           YYYN110A                                               00063000
063100           ZZZC0197                                               00063100
063200                                                                  00063200
063300       EVALUATE TRUE                                              00063300
063400                                                                  00063400
063500       WHEN LOC-TYP-CD   OF P-DDDTCZ01 = K-STORE-LOC-TYPE         00063500
063600         MOVE LOC-TYP-CD OF P-DDDTCZ01 TO                         00063600
063700                                       LOC-TYP-CD OF ZZZC0094     00063700
063800         MOVE ZZZC0094              TO ZZZC0197-TRX-REC           00063800
063900         MOVE 'STRM'                TO ZZZC0197-TRX-ID            00063900
064000         MOVE 'NNNS0473'            TO ZZZC0197-PROGRAM           00064000
064100         MOVE YYYC0107-USER         TO ZZZC0197-USER              00064100
064200         MOVE YYYN005A-SYS-ENV      TO YYYN110A-SYS-ENV           00064200
064300         CALL ZZZS0197-EVENT-MGR USING                            00064300
064400              XXXN001A                                            00064400
064500              YYYN110A                                            00064500
064600              ZZZC0197                                            00064600
064700                                                                  00064700
064800       END-EVALUATE                                               00064800
064900     END-IF                                                       00064900
065000     .                                                            00065000
065100                                                                  00065100
065200                                                                  00065200
065300 2040-GET-CURRENT-USER.                                           00065300
065400     IF  SUCCESS                                                  00065400
065500     AND YYYN005A-CICS-ENV                                        00065500
065600       CALL Z-GET-CICS-USER-ID USING                              00065600
065700           EIBLK    WS-DUMMY                                      00065700
065800           XXXN001A YYYC0107                                      00065800
065900     ELSE                                                         00065900
066000       MOVE 'BATCH' TO YYYC0107-USER                              00066000
066100     END-IF                                                       00066100
066200     .                                                            00066200
066300                                                                  00066300
066310                                                                  00066310
066320 4600-CALL-MMMS0335-RI-ADD-CHK.                                   00066320
066330     INITIALIZE MMMC0335                                          00066330
066340     MOVE ITM-CLS-CD                   OF DCLXXXL-LOC-CLS-AD-ZN   00066340
066350                                       TO MMMC0335-ITM-CLS-CD     00066350
066360     MOVE AD-ZONE-EXCP                 OF DCLXXXL-LOC-CLS-AD-ZN   00066360
066370                                       TO MMMC0335-AD-ZONE-EXCP   00066370
066371     MOVE AD-ZONE                      OF DCLXXXL-LOC-CLS-AD-ZN   00066371
066372                                       TO MMMC0335-AD-ZONE        00066372
066380     SET   MMMC0335-INSERT-CHECK       TO TRUE                    00066380
066390     SET   MMMC0335-XXXL-LOC-CLS-AD-ZN TO TRUE                    00066390
066391     SET   MMMC0335-ORACLE             TO TRUE                    00066391
066392     CALL  MMMC0335-RI-INSERT-CHK      USING                      00066392
066393           XXXN001A                                               00066393
066394           MMMC0335                                               00066394
066395     .                                                            00066395
066396                                                                  00066396
066397                                                                  00066397
066400 5000-CALL-NNNS0473-CUD-ROUTINE.                                  00066400
066410     CALL NNNU0473-ORACLE-UPDATE USING                            00066410
066420          XXXN001A                                                00066420
066430          SQLCA                                                   00066430
066440          YYYN005A                                                00066440
066450          NNNN0000-PARMS                                          00066450
066460          DDDTCZ01                                                00066460
066470     .                                                            00066470
066480                                                                  00066480
066490                                                                  00066490
066500* ================================================================00066500
066600* Special sql or functions to be performed by this subroutine.    00066600
066700* ================================================================00066700
066800 10000-DO-SPECIAL-IO-FUNCS.                                       00066800
066900     EXIT                                                         00066900
067000     .                                                            00067000
