000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    MMMS0335.                                         00000200
000300 AUTHOR.        NAME.                                             00000300
000400 DATE-WRITTEN.  NOV 1600.                                         00000400
000500*-----------------------------------------------------------------00000500
000600* Enable RI constraints for vendor location                       00000600
000700*-----------------------------------------------------------------00000700
001200 ENVIRONMENT DIVISION.                                            00001200
001300 DATA DIVISION.                                                   00001300
001400 WORKING-STORAGE SECTION.                                         00001400
001500*=================================================================00001500
001600* Misc working storage.                                           00001600
001700*=================================================================00001700
001800 01 WS-SQLCODE                  PIC ----9.                        00001800
001900 01 Z-ORA-CONNECT               PIC X(8) VALUE 'YYYS0210'.        00001900
002000 01 Z-DB2-CONNECT               PIC X(8) VALUE 'YYYS0211'.        00002000
002100 01 WS-XXXN001A                 PIC X(265) VALUE SPACES.          00002100
002200 01 WS-SQLCA                    PIC X(136) VALUE SPACES.          00002200
002300                                                                  00002300
002400* ================================================================00002400
002500* Misc copy books.                                                00002500
002600*=================================================================00002600
002700*COPY MMMC0335.                                                   00002700
002800                                                                  00002800
002900*=================================================================00002900
003000*  DB2 Areas                                                      00003000
003100*=================================================================00003100
003200     EXEC SQL                                                     00003200
003300       INCLUDE SQLCA                                              00003300
003400     END-EXEC.                                                    00003400
003500                                                                  00003500
003600     EXEC SQL                                                     00003600
003700       INCLUDE DDDTAC01                                           00003700
003800     END-EXEC.                                                    00003800
003900                                                                  00003900
005200     EXEC SQL                                                     00005200
005300       INCLUDE DDDTCM01                                           00005300
005400     END-EXEC.                                                    00005400
005500                                                                  00005500
005600     EXEC SQL                                                     00005600
005700       INCLUDE DDDTCS01                                           00005700
005800     END-EXEC.                                                    00005800
005900                                                                  00005900
006000     EXEC SQL                                                     00006000
006100       INCLUDE DDDTEA01                                           00006100
006200     END-EXEC.                                                    00006200
006300                                                                  00006300
006400     EXEC SQL                                                     00006400
006500       INCLUDE DDDTIC01                                           00006500
006600     END-EXEC.                                                    00006600
006700                                                                  00006700
006800     EXEC SQL                                                     00006800
006900       INCLUDE DDDTAV01                                           00006900
007000     END-EXEC.                                                    00007000
007100                                                                  00007100
007200     EXEC SQL                                                     00007200
007300       INCLUDE DDDTSC01                                           00007300
007400     END-EXEC.                                                    00007400
007500                                                                  00007500
007600     EXEC SQL                                                     00007600
007700       INCLUDE DDDTBF01                                           00007700
007800     END-EXEC.                                                    00007800
008300                                                                  00008300
008400     EXEC SQL                                                     00008400
008500       INCLUDE DDDTZN01                                           00008500
008600     END-EXEC.                                                    00008600
008700                                                                  00008700
008800     EXEC SQL                                                     00008800
008900       INCLUDE DDDTCT01                                           00008900
009000     END-EXEC.                                                    00009000
009100                                                                  00009100
009200                                                                  00009200
009300                                                                  00009300
009400 LINKAGE SECTION.                                                 00009400
009500 COPY XXXN001A.                                                   00009500
009600 COPY MMMC0335.                                                   00009600
009700                                                                  00009700
009800 PROCEDURE DIVISION USING                                         00009800
009900     XXXN001A                                                     00009900
010000     MMMC0335                                                     00010000
010100     .                                                            00010100
010200                                                                  00010200
010300*=================================================================00010300
010400* Main program logic...                                           00010400
010500*=================================================================00010500
010600 000-MAIN-LINE.                                                   00010600
010700     PERFORM 010-INITIALIZE                                       00010700
010800     EVALUATE MMMC0335-TABLE                                      00010800
010900       WHEN 001                                                   00010900
011100       WHEN 002                                                   00011100
011300       WHEN 003                                                   00011300
011500       WHEN 004                                                   00011500
011600         CONTINUE                                                 00011600
011700       WHEN 005                                                   00011700
011800         PERFORM 500-CHK-XXX-VEND-LOC                             00011800
011900       WHEN 006                                                   00011900
012000         PERFORM 600-CHK-LOC-SHPNG-OPT                            00012000
012100       WHEN 007                                                   00012100
012200         PERFORM 700-CHK-RETAIL-LOC                               00012200
012300       WHEN 008                                                   00012300
012400         PERFORM 800-CHK-RETL-LOC-CLS-AD-ZN                       00012400
012500       WHEN 009                                                   00012500
012600         PERFORM 900-CHK-XXX-VEND-LOC                             00012600
012700       WHEN 010                                                   00012700
012800         PERFORM 1000-CHK-VEND-TRXAL-CNTL                         00012800
012900       WHEN 011                                                   00012900
013000         PERFORM 1100-CHK-VENDOR-COMMENTS                         00013000
013100       WHEN OTHER                                                 00013100
013200         SET FAILURE   TO TRUE                                    00013200
013300         MOVE SPACES  TO IS-RTRN-MSG-TXT                          00013300
013400         STRING 'MMMS0335 - invalid table passed '                00013400
013500                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT            00013500
013600     END-EVALUATE                                                 00013600
013700     IF MMMC0335-ORACLE                                           00013700
013800       MOVE XXXN001A   TO  WS-XXXN001A                            00013800
013900       MOVE SQLCA      TO  WS-SQLCA                               00013900
014000       PERFORM 020-CONNECT-TO-ORACLE                              00014000
014100       MOVE WS-XXXN001A  TO XXXN001A                              00014100
014200       MOVE WS-SQLCA     TO SQLCA                                 00014200
014300     END-IF                                                       00014300
014400                                                                  00014400
014500     GOBACK                                                       00014500
014600     .                                                            00014600
014700                                                                  00014700
014800                                                                  00014800
014900*=================================================================00014900
015000* Initialization and program start functions.                     00015000
015100*=================================================================00015100
015200 010-INITIALIZE.                                                  00015200
015300     INITIALIZE XXXN001A                                          00015300
015400                WS-XXXN001A                                       00015400
015500                WS-SQLCA                                          00015500
015600                                                                  00015600
015700     IF MMMC0335-ORACLE                                           00015700
015800       IF MMMC0335-FUNC  = 'I'                                    00015800
015900         PERFORM 015-CONNECT-TO-DB2                               00015900
016000       ELSE                                                       00016000
016100         SET FAILURE TO TRUE                                      00016100
016200         MOVE SPACES  TO IS-RTRN-MSG-TXT                          00016200
016300         STRING 'MMMS0335 - invalid function passed '             00016300
016400                'function should be insert'                       00016400
016500                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT            00016500
016600       END-IF                                                     00016600
016700     END-IF                                                       00016700
016800                                                                  00016800
016900     MOVE SPACES   TO MMMC0335-STATUS                             00016900
017000     .                                                            00017000
017100                                                                  00017100
017200                                                                  00017200
017300*=================================================================00017300
017400* Connecting to db2 database                                      00017400
017500*=================================================================00017500
017600 015-CONNECT-TO-DB2.                                              00017600
017700     CALL Z-DB2-CONNECT         USING XXXN001A                    00017700
017800                                      SQLCA                       00017800
017900     .                                                            00017900
018000                                                                  00018000
018100                                                                  00018100
018200*=================================================================00018200
018300* Connecting to oracle database                                   00018300
018400*=================================================================00018400
018500 020-CONNECT-TO-ORACLE.                                           00018500
018600     CALL Z-ORA-CONNECT USING XXXN001A                            00018600
018700                              SQLCA                               00018700
018800     .                                                            00018800
018900                                                                  00018900
019000                                                                  00019000
019100*=================================================================00019100
019200* RI Checking starts here                                         00019200
019300*=================================================================00019300
037300 500-CHK-XXX-VEND-LOC.                                            00037300
037400      IF SUCCESS                                                  00037400
037500*       PERFORM 505-CHK-XXX                                       00037500
037600        CONTINUE                                                  00037600
037700      END-IF                                                      00037700
037800      .                                                           00037800
037900                                                                  00037900
038000                                                                  00038000
038100 505-CHK-XXX.                                                     00038100
038200     MOVE MMMC0335-XXX-CD                                         00038200
038300       TO XXX-CD                 OF DCLXXX                        00038300
038400                                                                  00038400
038500     EXEC SQL                                                     00038500
038600        SELECT XXX_CD                                             00038600
038700         INTO : DCLXXX.XXX-CD                                     00038700
038800        FROM XXX                                                  00038800
038900        WHERE XXX_CD                                              00038900
039000                      = :DCLXXX.XXX-CD                            00039000
039100        FETCH FIRST 1 ROWS ONLY                                   00039100
039200     END-EXEC                                                     00039200
039300                                                                  00039300
039400     EVALUATE TRUE                                                00039400
039500       WHEN SQLCODE = 0                                           00039500
039600         SET MMMC0335-PARENT     TO  TRUE                         00039600
039700       WHEN SQLCODE = 100                                         00039700
039800         SET  MMMC0335-NO-PARENT TO TRUE                          00039800
039900         SET FAILURE             TO TRUE                          00039900
040000         MOVE SQLCODE            TO WS-SQLCODE                    00040000
040100         MOVE SPACE              TO IS-RTRN-MSG-TXT               00040100
040200         STRING 'MMMS0335 - XXX_CD'                               00040200
040300                 ' should be in XXX,'                             00040300
040400                 ' rule = MDCVMDCM '                              00040400
040500         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00040500
040600       WHEN OTHER                                                 00040600
040700         SET FAILURE             TO TRUE                          00040700
040800         MOVE SQLCODE            TO WS-SQLCODE                    00040800
040900         MOVE SPACE              TO IS-RTRN-MSG-TXT               00040900
041000         STRING 'MMMS0335 - SQL error on table XXX, '             00041000
041100                 'Sqlcode = ' WS-SQLCODE                          00041100
041200         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00041200
041300     END-EVALUATE                                                 00041300
041400     .                                                            00041400
041500                                                                  00041500
041600                                                                  00041600
041700 600-CHK-LOC-SHPNG-OPT.                                           00041700
041800      IF SUCCESS                                                  00041800
041900*       PERFORM 605-CHK-CUST-SHPNG-METH                           00041900
042000        CONTINUE                                                  00042000
042100      END-IF                                                      00042100
042200      .                                                           00042200
042300                                                                  00042300
042400                                                                  00042400
042500 605-CHK-CUST-SHPNG-METH.                                         00042500
042600     MOVE MMMC0335-CUST-SHPNG-METH-CD                             00042600
042700       TO CUST-SHPNG-METH-CD     OF DCLCUST-SHPNG-METH            00042700
042800                                                                  00042800
042900     EXEC SQL                                                     00042900
043000        SELECT XXXT_SHPNG_METH_CD                                 00043000
043100         INTO : DCLCUST-SHPNG-METH.CUST-SHPNG-METH-CD             00043100
043200        FROM XXXT_SHPNG_METH                                      00043200
043300        WHERE XXXT_SHPNG_METH_CD                                  00043300
043400                      = :DCLCUST-SHPNG-METH.CUST-SHPNG-METH-CD    00043400
043500        FETCH FIRST 1 ROWS ONLY                                   00043500
043600     END-EXEC                                                     00043600
043700                                                                  00043700
043800     EVALUATE TRUE                                                00043800
043900       WHEN SQLCODE = 0                                           00043900
044000         SET MMMC0335-PARENT     TO  TRUE                         00044000
044100       WHEN SQLCODE = 100                                         00044100
044200         SET  MMMC0335-NO-PARENT TO TRUE                          00044200
044300         SET FAILURE             TO TRUE                          00044300
044400         MOVE SQLCODE            TO WS-SQLCODE                    00044400
044500         MOVE SPACE              TO IS-RTRN-MSG-TXT               00044500
044600         STRING 'MMMS0335 - XXXT_SHPNG_METH_CD'                   00044600
044700                 ' should be in XXXT_SHPNG_METH,'                 00044700
044800                 ' rule = MESOMECS '                              00044800
044900         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00044900
045000       WHEN OTHER                                                 00045000
045100         SET FAILURE             TO TRUE                          00045100
045200         MOVE SQLCODE            TO WS-SQLCODE                    00045200
045300         MOVE SPACE              TO IS-RTRN-MSG-TXT               00045300
045400         STRING 'MMMS0335 - SQL error on table '                  00045400
045500                'XXXT_SHPNG_METH, '                               00045500
045600                 'Sqlcode = ' WS-SQLCODE                          00045600
045700         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00045700
045800     END-EVALUATE                                                 00045800
045900     .                                                            00045900
046000                                                                  00046000
046100                                                                  00046100
046200 700-CHK-RETAIL-LOC.                                              00046200
046300      IF SUCCESS                                                  00046300
046400*       PERFORM 705-CHK-ECOMM-MKT-AREA                            00046400
046500*       IF SUCCESS AND MMMC0335-PARENT                            00046500
046600*         PERFORM 710-CHK-CMPTR-TYP                               00046600
046700*       END-IF                                                    00046700
046800        CONTINUE                                                  00046800
046900      END-IF                                                      00046900
047000      .                                                           00047000
047100                                                                  00047100
047200                                                                  00047200
047300 705-CHK-ECOMM-MKT-AREA.                                          00047300
047400     MOVE MMMC0335-ECOMM-MKT-AREA-CD                              00047400
047500       TO ECOMM-MKT-AREA-CD      OF DCLECOMM-MKT-AREA             00047500
047600                                                                  00047600
047700     EXEC SQL                                                     00047700
047800        SELECT XXXMM_MKT_AREA_CD                                  00047800
047900         INTO : DCLECOMM-MKT-AREA.ECOMM-MKT-AREA-CD               00047900
048000        FROM XXXMM_MKT_AREA                                       00048000
048100        WHERE XXXMM_MKT_AREA_CD                                   00048100
048200                      = :DCLECOMM-MKT-AREA.ECOMM-MKT-AREA-CD      00048200
048300        FETCH FIRST 1 ROWS ONLY                                   00048300
048400     END-EXEC                                                     00048400
048500                                                                  00048500
048600     EVALUATE TRUE                                                00048600
048700       WHEN SQLCODE = 0                                           00048700
048800         SET MMMC0335-PARENT     TO  TRUE                         00048800
048900       WHEN SQLCODE = 100                                         00048900
049000         SET  MMMC0335-NO-PARENT TO TRUE                          00049000
049100         SET FAILURE             TO TRUE                          00049100
049200         MOVE SQLCODE            TO WS-SQLCODE                    00049200
049300         MOVE SPACE              TO IS-RTRN-MSG-TXT               00049300
049400         STRING 'MMMS0335 - XXXMM_MKT_AREA_CD'                    00049400
049500                 ' should be in XXXMM_MKT_AREA,'                  00049500
049600                 ' rule = MDLDDDEA '                              00049600
049700         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00049700
049800       WHEN OTHER                                                 00049800
049900         SET FAILURE             TO TRUE                          00049900
050000         MOVE SQLCODE            TO WS-SQLCODE                    00050000
050100         MOVE SPACE              TO IS-RTRN-MSG-TXT               00050100
050200         STRING 'MMMS0335 - SQL error on table '                  00050200
050300                'XXXMM_MKT_AREA, '                                00050300
050400                 'Sqlcode = ' WS-SQLCODE                          00050400
050500         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00050500
050600     END-EVALUATE                                                 00050600
050700     .                                                            00050700
050900                                                                  00050900
054600                                                                  00054600
054700 800-CHK-RETL-LOC-CLS-AD-ZN.                                      00054700
054800      IF SUCCESS                                                  00054800
054900*       PERFORM 805-CHK-ITM-CLS                                   00054900
055000*       IF SUCCESS AND MMMC0335-PARENT                            00055000
055100*         PERFORM 810-CHK-AA-ZONES                                00055100
055200*       END-IF                                                    00055200
055300        CONTINUE                                                  00055300
055400      END-IF                                                      00055400
055500      .                                                           00055500
055600                                                                  00055600
055700                                                                  00055700
055800 805-CHK-ITM-CLS.                                                 00055800
055900     MOVE MMMC0335-ITM-CLS-CD                                     00055900
056000       TO ITEM-CLS-CODE          OF DCLITM-CLS                    00056000
056100                                                                  00056100
056200     EXEC SQL                                                     00056200
056300        SELECT ITEM_CLS_CODE                                      00056300
056400         INTO : DCLITM-CLS.ITEM-CLS-CODE                          00056400
056500        FROM XXX_CLS                                              00056500
056600        WHERE ITEM_CLS_CODE                                       00056600
056700                      = : DCLITM-CLS.ITEM-CLS-CODE                00056700
056800        FETCH FIRST 1 ROWS ONLY                                   00056800
056900     END-EXEC                                                     00056900
057000                                                                  00057000
057100     EVALUATE TRUE                                                00057100
057200       WHEN SQLCODE = 0                                           00057200
057300         SET MMMC0335-PARENT     TO  TRUE                         00057300
057400       WHEN SQLCODE = 100                                         00057400
057500         SET  MMMC0335-NO-PARENT TO TRUE                          00057500
057600         SET FAILURE             TO TRUE                          00057600
057700         MOVE SQLCODE            TO WS-SQLCODE                    00057700
057800         MOVE SPACE              TO IS-RTRN-MSG-TXT               00057800
057900         STRING 'MMMS0335 - XXX_CLS_CD'                           00057900
058000                 ' should be in XXX_CLS,'                         00058000
058100                 ' rule = MDCZMDIC '                              00058100
058200         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00058200
058300       WHEN OTHER                                                 00058300
058400         SET FAILURE             TO TRUE                          00058400
058500         MOVE SQLCODE            TO WS-SQLCODE                    00058500
058600         MOVE SPACE              TO IS-RTRN-MSG-TXT               00058600
058700         STRING 'MMMS0335 - SQL error on table '                  00058700
058800                'XXX_CLS, '                                       00058800
058900                 'Sqlcode = ' WS-SQLCODE                          00058900
059000         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00059000
059100     END-EVALUATE                                                 00059100
059200     .                                                            00059200
059300                                                                  00059300
059400                                                                  00059400
059500 810-CHK-AA-ZONES.                                                00059500
059600     MOVE MMMC0335-AD-ZONE                                        00059600
059700       TO AD-ZONE                OF DCLAA-ZONES                   00059700
059800     MOVE MMMC0335-AD-ZONE-EXCP                                   00059800
059900       TO AD-ZONE                OF DCLAA-ZONES                   00059900
060000                                                                  00060000
060100     EXEC SQL                                                     00060100
060200        SELECT AD_ZONE                                            00060200
060300         INTO : DCLAA-ZONES.AD-ZONE                               00060300
060400        FROM XX_ZONES                                             00060400
060500        WHERE AD_ZONE                                             00060500
060600                      = :DCLAA-ZONES.AD-ZONE                      00060600
060700        FETCH FIRST 1 ROWS ONLY                                   00060700
060800     END-EXEC                                                     00060800
060900                                                                  00060900
061000     EVALUATE TRUE                                                00061000
061100       WHEN SQLCODE = 0                                           00061100
061200         SET MMMC0335-PARENT     TO  TRUE                         00061200
061300       WHEN SQLCODE = 100                                         00061300
061400         SET  MMMC0335-NO-PARENT TO TRUE                          00061400
061500         SET FAILURE             TO TRUE                          00061500
061600         MOVE SQLCODE            TO WS-SQLCODE                    00061600
061700         MOVE SPACE              TO IS-RTRN-MSG-TXT               00061700
061800         STRING 'MMMS0335 - AD_ZONE'                              00061800
061900                 ' should be in XX_ZONES,'                        00061900
062000                 ' rule = MDCZAAA2 '                              00062000
062100         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00062100
062200       WHEN OTHER                                                 00062200
062300         SET FAILURE             TO TRUE                          00062300
062400         MOVE SQLCODE            TO WS-SQLCODE                    00062400
062500         MOVE SPACE              TO IS-RTRN-MSG-TXT               00062500
062600         STRING 'MMMS0335 - SQL error on table '                  00062600
062700                'XX_ZONES, '                                      00062700
062800                 'Sqlcode = ' WS-SQLCODE                          00062800
062900         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00062900
063000     END-EVALUATE                                                 00063000
063100     .                                                            00063100
063200                                                                  00063200
063300                                                                  00063300
063400 900-CHK-XXX-VEND-LOC.                                            00063400
063500      IF SUCCESS                                                  00063500
063600*       PERFORM 905-CHK-XXX                                       00063600
063700        CONTINUE                                                  00063700
063800      END-IF                                                      00063800
063900      .                                                           00063900
064000                                                                  00064000
064100                                                                  00064100
064200 905-CHK-XXX.                                                     00064200
064300     MOVE MMMC0335-XXX-CD                                         00064300
064400       TO XXX-CD                 OF DCLXXX                        00064400
064500                                                                  00064500
064600     EXEC SQL                                                     00064600
064700        SELECT XXX_CD                                             00064700
064800         INTO : DCLXXX.XXX-CD                                     00064800
064900        FROM XXX                                                  00064900
065000        WHERE XXX_CD                                              00065000
065100                      = :DCLXXX.XXX-CD                            00065100
065200        FETCH FIRST 1 ROWS ONLY                                   00065200
065300     END-EXEC                                                     00065300
065400                                                                  00065400
065500     EVALUATE TRUE                                                00065500
065600       WHEN SQLCODE = 0                                           00065600
065700         SET MMMC0335-PARENT     TO  TRUE                         00065700
065800       WHEN SQLCODE = 100                                         00065800
065900         SET  MMMC0335-NO-PARENT TO TRUE                          00065900
066000         SET FAILURE             TO TRUE                          00066000
066100         MOVE SQLCODE            TO WS-SQLCODE                    00066100
066200         MOVE SPACE              TO IS-RTRN-MSG-TXT               00066200
066300         STRING 'MMMS0335 - XXX_CD'                               00066300
066400                 ' should be in XXX,'                             00066400
066500                 ' rule = MDLSMDSC '                              00066500
066600         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00066600
066700       WHEN OTHER                                                 00066700
066800         SET FAILURE             TO TRUE                          00066800
066900         MOVE SQLCODE            TO WS-SQLCODE                    00066900
067000         MOVE SPACE              TO IS-RTRN-MSG-TXT               00067000
067100         STRING 'MMMS0335 - SQL error on table XXX, '             00067100
067200                 'Sqlcode = ' WS-SQLCODE                          00067200
067300         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00067300
067400     END-EVALUATE                                                 00067400
067500     .                                                            00067500
067600                                                                  00067600
067700                                                                  00067700
067800 1000-CHK-VEND-TRXAL-CNTL.                                        00067800
067900      IF SUCCESS                                                  00067900
068000*       PERFORM 1005-CHK-BUSINESS-FUNCTION                        00068000
068100        CONTINUE                                                  00068100
068200      END-IF                                                      00068200
068300      .                                                           00068300
068400                                                                  00068400
068500                                                                  00068500
068600 1005-CHK-BUSINESS-FUNCTION.                                      00068600
068700     MOVE MMMC0335-BUS-FUNC-ID                                    00068700
068800       TO BUS-FUNC-ID            OF DCLBUSINESS-FUNCTION          00068800
068900                                                                  00068900
069000     EXEC SQL                                                     00069000
069100        SELECT BUS_FUNC_ID                                        00069100
069200         INTO : DCLBUSINESS-FUNCTION.BUS-FUNC-ID                  00069200
069300        FROM XXXINESS_FUNCTION                                    00069300
069400        WHERE BUS_FUNC_ID                                         00069400
069500                      = :DCLBUSINESS-FUNCTION.BUS-FUNC-ID         00069500
069600        FETCH FIRST 1 ROWS ONLY                                   00069600
069700     END-EXEC                                                     00069700
069800                                                                  00069800
069900     EVALUATE TRUE                                                00069900
070000       WHEN SQLCODE = 0                                           00070000
070100         SET MMMC0335-PARENT     TO  TRUE                         00070100
070200       WHEN SQLCODE = 100                                         00070200
070300         SET  MMMC0335-NO-PARENT TO TRUE                          00070300
070400         SET FAILURE             TO TRUE                          00070400
070500         MOVE SQLCODE            TO WS-SQLCODE                    00070500
070600         MOVE SPACE              TO IS-RTRN-MSG-TXT               00070600
070700         STRING 'MMMS0335 - BUS_FUNC_ID'                          00070700
070800                 ' should be in XXXINESS_FUNCTION,'               00070800
070900                 ' rule = MDVTMDBF '                              00070900
071000         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00071000
071100       WHEN OTHER                                                 00071100
071200         SET FAILURE             TO TRUE                          00071200
071300         MOVE SQLCODE            TO WS-SQLCODE                    00071300
071400         MOVE SPACE              TO IS-RTRN-MSG-TXT               00071400
071500         STRING 'MMMS0335 - SQL error on table '                  00071500
071600                'XXXINESS_FUNCTION, '                             00071600
071700                 'Sqlcode = ' WS-SQLCODE                          00071700
071800         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00071800
071900     END-EVALUATE                                                 00071900
072000     .                                                            00072000
072100                                                                  00072100
072200                                                                  00072200
072300 1100-CHK-VENDOR-COMMENTS.                                        00072300
072400      IF SUCCESS                                                  00072400
072500        PERFORM 1105-CHK-CMT-TYP-CD                               00072500
072600      END-IF                                                      00072600
072700      .                                                           00072700
072800                                                                  00072800
072900                                                                  00072900
073000 1105-CHK-CMT-TYP-CD.                                             00073000
073100     MOVE MMMC0335-CMT-TYP-CD                                     00073100
073200       TO CMT-TYP-CD             OF DCLCMT-TYP-CD                 00073200
073300                                                                  00073300
073400     EXEC SQL                                                     00073400
073500        SELECT XXX_TYP_CD                                         00073500
073600         INTO : DCLCMT-TYP-CD.CMT-TYP-CD                          00073600
073700        FROM XXX_TYP_CD                                           00073700
073800        WHERE XXX_TYP_CD                                          00073800
073900                      = :DCLCMT-TYP-CD.CMT-TYP-CD                 00073900
074000        FETCH FIRST 1 ROWS ONLY                                   00074000
074100     END-EXEC                                                     00074100
074200                                                                  00074200
074300     EVALUATE TRUE                                                00074300
074400       WHEN SQLCODE = 0                                           00074400
074500         SET MMMC0335-PARENT     TO  TRUE                         00074500
074600       WHEN SQLCODE = 100                                         00074600
074700         SET  MMMC0335-NO-PARENT TO TRUE                          00074700
074800         SET FAILURE             TO TRUE                          00074800
074900         MOVE SQLCODE            TO WS-SQLCODE                    00074900
075000         MOVE SPACE              TO IS-RTRN-MSG-TXT               00075000
075100         STRING 'MMMS0335 - XXX_TYP_CD'                           00075100
075200                 ' should be in XXX_TYP_CD,'                      00075200
075300                 ' rule = MDVCMDCT '                              00075300
075400         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00075400
075500       WHEN OTHER                                                 00075500
075600         SET FAILURE             TO TRUE                          00075600
075700         MOVE SQLCODE            TO WS-SQLCODE                    00075700
075800         MOVE SPACE              TO IS-RTRN-MSG-TXT               00075800
075900         STRING 'MMMS0335 - SQL error on table '                  00075900
076000                'XXX_TYP_CD, '                                    00076000
076100                 'Sqlcode = ' WS-SQLCODE                          00076100
076200         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00076200
076300     END-EVALUATE                                                 00076300
076400     .                                                            00076400
076500                                                                  00076500
076600                                                                  00076600
