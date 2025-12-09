000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    MMMS0711.                                         00000200
000300 AUTHOR.        NAME.                                             00000300
000400 DATE-WRITTEN.  Circa 1600.                                       00000400
001700 ENVIRONMENT DIVISION.                                            00001900
001800 DATA DIVISION.                                                   00002000
001900 WORKING-STORAGE SECTION.                                         00002100
002000* --------------------------------------------------------------- 00002200
002100* Misc working storage...                                         00002300
002200* --------------------------------------------------------------- 00002400
002300 01 WS-SQLCODE                      PIC ----9.                    00002500
002400 01 WS-CNT                          PIC S9(09) COMP VALUE 0.      00002600
002500 01 KONSTANTS.                                                    00002700
002600    10 K-DSD-VEND-LOC-TYP-CD        PIC X(2)  VALUE 'D '.         00002800
002700    10 K-DSD-AP-TYP-CD              PIC X(2)  VALUE 'DS'.         00002900
002800    10 K-DSV-LOC-SUB-TYP-CD         PIC X(5)  VALUE 'DSV  '.      00003000
002900    10 K-DSD-ITEM-KEY-CD            PIC X(5)  VALUE 'DSD  '.      00003100
003000                                                                  00003200
003100* --------------------------------------------------------------- 00003300
003200* Copy books...                                                   00003400
003300  COPY WWWC0099.                                                  00003500
003400  COPY YYYN000A.                                                  00003600
003500  COPY YYYN005A.                                                  00003700
003600* --------------------------------------------------------------- 00003800
003700                                                                  00003900
003800* ========================< DCL Stuff >===========================00004000
003900* Place all DB2 things in this section please...                  00004100
004000* ================================================================00004200
004100     EXEC SQL                                                     00004300
004200       INCLUDE SQLCA                                              00004400
004300     END-EXEC.                                                    00004500
004400                                                                  00004600
004500     EXEC SQL                                                     00004700
004600       INCLUDE DDDTLS01                                           00004800
004700     END-EXEC.                                                    00004900
004800                                                                  00005000
004900     EXEC SQL                                                     00005100
005000       INCLUDE DDDTPT01                                           00005200
005100     END-EXEC.                                                    00005300
005200                                                                  00005400
005300     EXEC SQL                                                     00005500
005400       INCLUDE DDDTVI01                                           00005600
005500     END-EXEC.                                                    00005700
005600                                                                  00005800
005700     EXEC SQL                                                     00005900
005800       INCLUDE DDDTSI01                                           00006000
005900     END-EXEC.                                                    00006100
006000                                                                  00006200
006100     EXEC SQL                                                     00006300
006200       INCLUDE DDDTLO01                                           00006400
006300     END-EXEC.                                                    00006500
006400                                                                  00006600
006500                                                                  00006700
006600* ========================< DCL Stuff >===========================00006800
006700* Place all cursors here...                                       00006900
006800* ================================================================00007000
006900                                                                  00007100
007000 LINKAGE SECTION.                                                 00007200
007100 COPY XXXN001A.                                                   00007300
007200 COPY MMMC0711.                                                   00007400
007300                                                                  00007500
007400 PROCEDURE DIVISION USING                                         00007600
007500     XXXN001A                                                     00007700
007600     MMMC0711                                                     00007800
007700     .                                                            00007900
007800                                                                  00008000
007900*================================================================ 00008100
008000* Start of program main line.                                     00008200
008100*================================================================ 00008300
008200 000-MAIN.                                                        00008400
008300     PERFORM 100-INITIALIZE                                       00008500
008400                                                                  00008600
008500     EVALUATE TRUE                                                00008700
008600       WHEN MMMC0711-IS-DSV-FUNC                                  00008800
008700         PERFORM 200-CHECK-VEND-IS-DSV                            00008900
008800       WHEN MMMC0711-IS-DSV-ITEM-FUNC                             00009000
008900         PERFORM 300-CHECK-ENTY-IS-DSV                            00009100
009000       WHEN OTHER                                                 00009200
009100         SET FAILURE TO TRUE                                      00009300
009200         MOVE 'ZZZS0033 - Invalid MMMC0711-FUNC passed.'          00009400
009300           TO IS-RTRN-MSG-TXT                                     00009500
009400     END-EVALUATE                                                 00009600
009500                                                                  00009700
009600                                                                  00009800
009700*    IF YYYN005A-ORACLE                                           00009900
009800*      PERFORM 125-CONNECT-TO-DB2                                 00010000
009900*    END-IF                                                       00010100
010000     GOBACK                                                       00010200
010100     .                                                            00010300
010200                                                                  00010400
010300                                                                  00010500
010400*================================================================ 00010600
010500* Initialization...                                               00010700
010600*================================================================ 00010800
010700 100-INITIALIZE.                                                  00010900
010701*    DISPLAY 'IM IN MMMS0711'                                     00011000
010800     INITIALIZE XXXN001A                                          00011100
010900                MMMC0711-OUTPUTS                                  00011200
011000     SET ENTY-IS-NOT-DSV TO TRUE                                  00011300
011100     SET VEND-IS-NOT-DSV TO TRUE                                  00011400
011200     SET ENTY-EXISTS     TO TRUE                                  00011500
011300                                                                  00011600
011400     IF  MMMC0711-I-VEND-TYP-CD NOT EQUAL 'D'                     00011700
011500       MOVE 'D' TO MMMC0711-I-VEND-TYP-CD                         00011800
011600     END-IF                                                       00011900
011700                                                                  00012000
011800     EVALUATE TRUE                                                00012100
011900       WHEN SUCCESS                                               00012200
012000         CONTINUE                                                 00012300
012100                                                                  00012400
012200       WHEN MMMC0711-IS-DSV-FUNC                                  00012500
012300        AND ( MMMC0711-I-VEND-NBR = 0                             00012600
012400         OR   MMMC0711-I-VEND-TYP-CD = SPACES)                    00012700
012500         SET  FAILURE TO TRUE                                     00012800
012600         MOVE 'MMMS0711 - Invalid Vendor Number or Type!'         00012900
012700           TO IS-RTRN-MSG-TXT                                     00013000
012800                                                                  00013100
012900       WHEN MMMC0711-IS-DSV-ITEM-FUNC                             00013200
013000        AND ( MMMC0711-I-ENTY-ID  = 0                             00013300
013100         OR   MMMC0711-I-ENTY-TYP = SPACES)                       00013400
013200         SET  FAILURE TO TRUE                                     00013500
013300         MOVE 'MMMS0711 - Invalid Enty id/Enty Type!'             00013600
013400           TO IS-RTRN-MSG-TXT                                     00013700
013500                                                                  00013800
013600                                                                  00013900
013700       WHEN OTHER                                                 00014000
013800         SET  FAILURE TO TRUE                                     00014100
013900         MOVE 'MMMS0711 - Invalid Entity type!'                   00014200
014000           TO IS-RTRN-MSG-TXT                                     00014300
014100     END-EVALUATE                                                 00014400
014200     IF SUCCESS                                                   00014500
014300*       PERFORM 900-GET-TASK                                      00014600
014400*       IF SUCCESS AND WWWC0099-ORACLE                            00014700
014500*          SET YYYN005A-ORACLE  TO TRUE                           00014800
014600*          PERFORM 115-CONNECT-TO-ORACLE                          00014900
014700*       END-IF                                                    00015000
 14400*       IF SUCCESS                                                00015100
014600           PERFORM 125-CONNECT-TO-DB2                             00015200
014700*       END-IF                                                    00015300
014800     END-IF                                                       00015400
014900     .                                                            00015500
015000                                                                  00015600
015100                                                                  00015700
015200* ================================================================00015800
015300* Connecting to oracle database                                   00015900
015400* ================================================================00016000
015500 115-CONNECT-TO-ORACLE.                                           00016100
015600     CALL Z-ORA-CONNECT USING XXXN001A                            00016200
015700                              SQLCA                               00016300
015800     IF NOT SUCCESS                                               00016400
015900       MOVE SQLCODE TO WS-SQLCODE                                 00016500
016000       MOVE SPACES  TO IS-RTRN-MSG-TXT                            00016600
016100       STRING 'MMMS0711 - Error connecting to Oracle. Sqlcode ='  00016700
016200               WS-SQLCODE                                         00016800
016300               DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT             00016900
016400     END-IF                                                       00017000
016500     .                                                            00017100
016600                                                                  00017200
016700                                                                  00017300
016800* ================================================================00017400
016900* Connecting to db2 database                                      00017500
017000* ================================================================00017600
017100 125-CONNECT-TO-DB2.                                              00017700
017200     CALL Z-DB2-CONNECT         USING XXXN001A                    00017800
017300                                      SQLCA                       00017900
017400     .                                                            00018000
017500                                                                  00018100
017600                                                                  00018200
017700*================================================================ 00018300
017800* Check if Vendor is DSV ...                                      00018400
017900*================================================================ 00018500
018000 200-CHECK-VEND-IS-DSV.                                           00018600
018100     MOVE MMMC0711-I-VEND-NBR     TO LOC-NBR        OF DCLXXXATION00018700
018200     MOVE MMMC0711-I-VEND-TYP-CD  TO LOC-TYP-CD     OF DCLXXXATION00018800
018300     EXEC SQL                                                     00018900
018400         SELECT COALESCE(COUNT (*),0)                             00019000
018500           INTO :WS-CNT                                           00019100
018600           FROM XXXATION LOC,                                     00019200
018700                AP_XXX_SUB_TYP SUB                                00019300
018800         WHERE LOC.LOC_NBR         = :DCLXXXATION.LOC-NBR         00019400
018900           AND LOC.LOC_TYP_CD      = :DCLXXXATION.LOC-TYP-CD      00019500
019000           AND SUB.AP_NBR          = LOC.AP_NBR                   00019600
019100           AND SUB.AP_TYP_CD       = LOC.AP_TYP_CD                00019700
019200           AND SUB.AP_SUB_TYP_CD   = :K-DSV-LOC-SUB-TYP-CD        00019800
019300     END-EXEC                                                     00019900
019400     EVALUATE TRUE                                                00020000
019500       WHEN SQLCODE = 0                                           00020100
019600         IF WS-CNT > 0                                            00020200
019700           SET VEND-IS-DSV TO TRUE                                00020300
019800         ELSE                                                     00020400
019900           SET VEND-IS-NOT-DSV TO TRUE                            00020500
020000         END-IF                                                   00020600
020100       WHEN SQLCODE = 100                                         00020700
020200         MOVE 0 TO SQLCODE                                        00020800
020300       WHEN SQLCODE NOT = 0                                       00020900
020400         SET  FAILURE TO TRUE                                     00021000
020500         MOVE SPACES  TO IS-RTRN-MSG-TXT                          00021100
020600         MOVE SQLCODE TO WS-SQLCODE                               00021200
020700         STRING 'MMMS0711 - Error checking SUB/VLI, '             00021300
020800                'RC=' WS-SQLCODE '.'                              00021400
020900                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT            00021500
021000     END-EVALUATE                                                 00021600
021100      .                                                           00021700
021200*================================================================ 00021800
021300* Check if Entity is DSV                                          00021900
021400*================================================================ 00022000
021500 300-CHECK-ENTY-IS-DSV.                                           00022100
021600      EVALUATE TRUE                                               00022200
021700        WHEN ( DSD-ITEM-KEY-CD OR UPC-ITEM-KEY-CD )               00022300
021800          PERFORM 310-CHECK-UPC-VEND                              00022400
021900                                                                  00022500
022000        WHEN PRD-ITEM-KEY-CD                                      00022600
022100          PERFORM 320-CHECK-PROD-VEND                             00022700
022200      END-EVALUATE                                                00022800
022300      .                                                           00022900
022400*================================================================ 00023000
022500* Check if UPC is DSV ...                                         00023100
022600*================================================================ 00023200
022700 310-CHECK-UPC-VEND.                                              00023300
022800     MOVE MMMC0711-I-ENTY-ID      TO ITM-ID     OF DCLXXXD-LOC-ITM00023400
022900     MOVE MMMC0711-I-VEND-NBR     TO LOC-NBR    OF DCLXXXATION    00023500
023000     MOVE MMMC0711-I-VEND-TYP-CD                                  00023600
023100                                  TO LOC-TYP-CD OF DCLXXXATION    00023700
023200                                                                  00023800
023300     PERFORM 315-CHECK-UPC-IS-DSV                                 00023900
023400     IF SUCCESS                                                   00024000
023500       PERFORM 200-CHECK-VEND-IS-DSV                              00024100
023600     END-IF                                                       00024200
023700     .                                                            00024300
023800                                                                  00024400
023900                                                                  00024500
024000*================================================================ 00024600
024100* Check if UPC is DSV ...                                         00024700
024200*================================================================ 00024800
024300 315-CHECK-UPC-IS-DSV.                                            00024900
024400     IF MMMC0711-I-VEND-NBR > 0 AND CHECK-WITH-VEND               00025000
024500       EXEC SQL                                                   00025100
024600           SELECT COALESCE(COUNT (*),0)                           00025200
024700             INTO :WS-CNT                                         00025300
024800             FROM AP_XXX_SUB_TYP SUB ,                            00025400
024900                  XXXD_LOC_ITM VLI,                               00025500
025000                  XXXATION     LOC                                00025600
025100           WHERE SUB.AP_SUB_TYP_CD   = :K-DSV-LOC-SUB-TYP-CD      00025700
025200             AND SUB.AP_NBR          = LOC.AP_NBR                 00025800
025300             AND SUB.AP_TYP_CD       = LOC.AP_TYP_CD              00025900
025400             AND VLI.VEND_LOC_TYP_CD = LOC.LOC_TYP_CD             00026000
025500             AND VLI.VEND_LOC_NBR    = LOC.LOC_NBR                00026100
025600             AND VLI.ITM_KEY_TYP_CD  = :K-DSD-ITEM-KEY-CD         00026200
025700             AND VLI.ITM_ID          = :DCLXXXD-LOC-ITM.ITM-ID    00026300
025800             AND LOC.LOC_NBR         = :DCLXXXATION.LOC-NBR       00026400
025900             AND LOC.LOC_TYP_CD      = :DCLXXXATION.LOC-TYP-CD    00026500
026000       END-EXEC                                                   00026600
026100     ELSE                                                         00026700
026200       EXEC SQL                                                   00026800
026300           SELECT COALESCE(COUNT (*),0)                           00026900
026400             INTO :WS-CNT                                         00027000
026500             FROM AP_XXX_SUB_TYP SUB ,                            00027100
026600                  XXXD_LOC_ITM VLI                                00027200
026700           WHERE SUB.AP_SUB_TYP_CD   = :K-DSV-LOC-SUB-TYP-CD      00027300
026800             AND SUB.AP_NBR          =  VLI.VEND_LOC_NBR          00027400
026900             AND SUB.AP_TYP_CD       = :K-DSD-AP-TYP-CD           00027500
027000             AND VLI.VEND_LOC_TYP_CD = :K-DSD-VEND-LOC-TYP-CD     00027600
027100             AND VLI.ITM_KEY_TYP_CD  = :K-DSD-ITEM-KEY-CD         00027700
027200             AND VLI.ITM_ID          = :DCLXXXD-LOC-ITM.ITM-ID    00027800
027300       END-EXEC                                                   00027900
027400     END-IF                                                       00028000
027500                                                                  00028100
027600     EVALUATE TRUE                                                00028200
027700       WHEN SQLCODE = 0                                           00028300
027800         IF WS-CNT > 0                                            00028400
027900           SET ENTY-IS-DSV TO TRUE                                00028500
028000         ELSE                                                     00028600
028100           SET ENTY-IS-NOT-DSV TO TRUE                            00028700
028200         END-IF                                                   00028800
028300         PERFORM 315-CHECK-IF-UPC-EXISTS                          00028900
028400       WHEN SQLCODE = 100                                         00029000
028500         MOVE 0 TO SQLCODE                                        00029100
028600       WHEN SQLCODE NOT = 0                                       00029200
028700         SET  FAILURE TO TRUE                                     00029300
028800         MOVE SPACES  TO IS-RTRN-MSG-TXT                          00029400
028900         MOVE SQLCODE TO WS-SQLCODE                               00029500
029000         STRING 'MMMS0711 - Error checking SUB/VLI, '             00029600
029100                'RC=' WS-SQLCODE '.'                              00029700
029200                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT            00029800
029300     END-EVALUATE                                                 00029900
029400     .                                                            00030000
029500                                                                  00030100
029600                                                                  00030200
029700                                                                  00030300
029800*================================================================ 00030400
029900* Check if Product is DSV ...                                     00030500
030000*================================================================ 00030600
030100 315-CHECK-IF-UPC-EXISTS.                                         00030700
030200     MOVE MMMC0711-I-ENTY-ID  TO SCN-CD-ID  OF DCLXXXX-SCN-CODES  00030800
030300     EXEC SQL                                                     00030900
030400         SELECT COUNT (*)                                         00031000
030500           INTO :WS-CNT                                           00031100
030600           FROM PROD_SCN_CODES                                    00031200
030700         WHERE SCN_CD_ID           = :DCLXXXX-SCN-CODES.SCN-CD-ID 00031300
030800     END-EXEC                                                     00031400
030900                                                                  00031500
031000     EVALUATE TRUE                                                00031600
031100       WHEN SQLCODE = 0                                           00031700
031200         IF WS-CNT > 0                                            00031800
031300           SET ENTY-EXISTS          TO TRUE                       00031900
031400         ELSE                                                     00032000
031500           SET ENTY-DOES-NOT-EXIST  TO TRUE                       00032100
031600         END-IF                                                   00032200
031700       WHEN SQLCODE = 100                                         00032300
031800         MOVE 0 TO SQLCODE                                        00032400
031900       WHEN SQLCODE NOT = 0                                       00032500
032000         SET  FAILURE TO TRUE                                     00032600
032100         MOVE SPACES  TO IS-RTRN-MSG-TXT                          00032700
032200         MOVE SQLCODE TO WS-SQLCODE                               00032800
032300         STRING 'MMMS0711 - Error checking Scan Code '            00032900
032400                'RC=' WS-SQLCODE '.'                              00033000
032500                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT            00033100
032600     END-EVALUATE                                                 00033200
032700     .                                                            00033300
032800                                                                  00033400
032900*================================================================ 00033500
033000* Check if Product is DSV ...                                     00033600
033100*================================================================ 00033700
033200 320-CHECK-PROD-VEND.                                             00033800
033300     MOVE MMMC0711-I-ENTY-ID  TO PROD-ID    OF DCLPROD-ITEM       00033900
033400     MOVE MMMC0711-I-VEND-NBR TO LOC-NBR    OF DCLXXXATION        00034000
033500                                 AP-NBR     OF DCLAP-XXX-SUB-TYP  00034100
033600     MOVE MMMC0711-I-VEND-TYP-CD                                  00034200
033700                              TO LOC-TYP-CD OF DCLXXXATION        00034300
033800                                                                  00034400
033900     IF MMMC0711-I-VEND-NBR > 0                                   00034500
034000       EXEC SQL                                                   00034600
034100           SELECT COUNT (*)                                       00034700
034200             INTO :WS-CNT                                         00034800
034300             FROM AP_XXX_SUB_TYP SUB ,                            00034900
034400                  XXXD_LOC_ITM VLI,                               00035000
034500                  XXXATION LOC,                                   00035100
034600                  PROD_ITEM PI                                    00035200
034700           WHERE SUB.AP_SUB_TYP_CD   = :K-DSV-LOC-SUB-TYP-CD      00035300
034800             AND SUB.AP_NBR          = LOC.AP_NBR                 00035400
034900             AND SUB.AP_TYP_CD       = LOC.AP_TYP_CD              00035500
035000             AND VLI.VEND_LOC_NBR    = LOC.LOC_NBR                00035600
035100             AND VLI.VEND_LOC_TYP_CD = LOC.LOC_TYP_CD             00035700
035200             AND LOC.LOC_NBR         = :DCLXXXATION.LOC-NBR       00035800
035300             AND LOC.LOC_TYP_CD      = :DCLXXXATION.LOC-TYP-CD    00035900
035400             AND VLI.ITM_ID          = PI.ITM_ID                  00036000
035500             AND VLI.ITM_KEY_TYP_CD  = PI.ITM_KEY_TYP_CD          00036100
035600             AND PI.PROD_ID          = :DCLPROD-ITEM.PROD-ID      00036200
035700       END-EXEC                                                   00036300
035800     ELSE                                                         00036400
035900       EXEC SQL                                                   00036500
036000           SELECT COUNT (*)                                       00036600
036100             INTO :WS-CNT                                         00036700
036200             FROM AP_XXX_SUB_TYP SUB ,                            00036800
036300                  XXXD_LOC_ITM VLI,                               00036900
036400                  PROD_ITEM PI                                    00037000
036500           WHERE SUB.AP_SUB_TYP_CD   = :K-DSV-LOC-SUB-TYP-CD      00037100
036600             AND VLI.VEND_LOC_NBR    = SUB.AP_NBR                 00037200
036700             AND VLI.VEND_LOC_TYP_CD = :K-DSD-VEND-LOC-TYP-CD     00037300
036800             AND VLI.ITM_ID          = PI.ITM_ID                  00037400
036900             AND VLI.ITM_KEY_TYP_CD  = PI.ITM_KEY_TYP_CD          00037500
037000             AND PI.PROD_ID          = :DCLPROD-ITEM.PROD-ID      00037600
037100       END-EXEC                                                   00037700
037200     END-IF                                                       00037800
037300                                                                  00037900
037400     EVALUATE TRUE                                                00038000
037500       WHEN SQLCODE = 0                                           00038100
037600         IF WS-CNT > 0                                            00038200
037700           SET ENTY-IS-DSV TO TRUE                                00038300
037800         ELSE                                                     00038400
037900           SET ENTY-IS-NOT-DSV TO TRUE                            00038500
038000         END-IF                                                   00038600
038100       WHEN SQLCODE NOT = 0                                       00038700
038200         SET  FAILURE TO TRUE                                     00038800
038300         MOVE SPACES  TO IS-RTRN-MSG-TXT                          00038900
038400         MOVE SQLCODE TO WS-SQLCODE                               00039000
038500         STRING 'MMMS0711 - Error checking SUB/VLI, '             00039100
038600                'RC=' WS-SQLCODE '.'                              00039200
038700                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT            00039300
038800     END-EVALUATE                                                 00039400
038900     .                                                            00039500
039000******************************************************            00039600
039100 900-GET-TASK.                                                    00039700
039200     SET WWWC0099-GET-TASK      TO TRUE                           00039800
039300     CALL WWWS0099-CONTROL-SUBR USING                             00039900
039400         XXXN001A                                                 00040000
039500         WWWC0099                                                 00040100
039600     .                                                            00040200
039700                                                                  00040300
039800                                                                  00040400
