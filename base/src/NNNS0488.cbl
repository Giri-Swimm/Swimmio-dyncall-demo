000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    NNNS0488.                                         00000200
000300 AUTHOR.        NAME.                                             00000300
000400 DATE-WRITTEN.  Circa, 1600.                                      00000400
000500 DATE-COMPILED.                                                   00000500
000600* ----------------------------------------------------------------00000600
005200 ENVIRONMENT DIVISION.                                            00005200
005300 CONFIGURATION SECTION.                                           00005300
005400 DATA DIVISION.                                                   00005400
005500 WORKING-STORAGE SECTION.                                         00005500
005600                                                                  00005600
005700* ========================< MISC STUFF >==========================00005700
005800* Misc working storage variables go here.                         00005800
005900* ================================================================00005900
006000 01 WS-SQLCODE                         PIC ----9.                 00006000
006100 01 WS-DUMMY                           PIC X(1) VALUE SPACES.     00006100
006200 01 WS-CHECKPOINT-INC                  PIC S9(4) COMP VALUE 0.    00006200
006300 01 K-DEF-TM                           PIC X(8)  VALUE '00.00.00'.00006300
006400 01 K-DB2-MAX-TM                       PIC X(8)  VALUE '24.00.00'.00006400
006500 01 K-ORA-MAX-TM                       PIC X(8)  VALUE '23.59.59'.00006500
006600 01 WS-TIME-FIELDS.                                               00006600
006700    05 WS-MON-OPEN-TS                  PIC X(26) VALUE SPACES.    00006700
006800    05 WS-MON-CLOS-TS                  PIC X(26) VALUE SPACES.    00006800
006900    05 WS-TUE-OPEN-TS                  PIC X(26) VALUE SPACES.    00006900
007000    05 WS-TUE-CLOS-TS                  PIC X(26) VALUE SPACES.    00007000
007100    05 WS-WED-OPEN-TS                  PIC X(26) VALUE SPACES.    00007100
007200    05 WS-WED-CLOS-TS                  PIC X(26) VALUE SPACES.    00007200
007300    05 WS-THUR-OPEN-TS                 PIC X(26) VALUE SPACES.    00007300
007400    05 WS-THUR-CLOS-TS                 PIC X(26) VALUE SPACES.    00007400
007500    05 WS-FRI-OPEN-TS                  PIC X(26) VALUE SPACES.    00007500
007600    05 WS-FRI-CLOS-TS                  PIC X(26) VALUE SPACES.    00007600
007700    05 WS-SAT-OPEN-TS                  PIC X(26) VALUE SPACES.    00007700
007800    05 WS-SUN-OPEN-TS                  PIC X(26) VALUE SPACES.    00007800
007900    05 WS-SAT-CLOS-TS                  PIC X(26) VALUE SPACES.    00007900
008000    05 WS-SUN-CLOS-TS                  PIC X(26) VALUE SPACES.    00008000
008100                                                                  00008100
008200 01 WS-CURR-VALUES.                                               00008200
008300     05 WS-STR-ST-CD                   PIC X(1) VALUE SPACES.     00008300
008400     05 WS-CURR-AD-ZONE                PIC S9(7) COMP-3 VALUE 0.  00008400
008500     05 WS-CURR-LOB                    PIC S9(5) COMP-3 VALUE 0.  00008500
008600 01 WS-CNT                             PIC S9(4) COMP VALUE 0.    00008600
008700                                                                  00008700
008800 01 WS-NULL-INDS.                                                 00008800
008900     05 WS-ASSOC-ST-NO-IND             PIC S9(4) COMP VALUE 0.    00008900
009000     05 WS-ASSOC-ST-TYPE-IND           PIC S9(4) COMP VALUE 0.    00009000
009100                                                                  00009100
009200 01 WS-LOGICALS.                                                  00009200
009300     05 WS-LOC-STAT-SW                 PIC X(1)  VALUE SPACES.    00009300
009400         88 WS-LOC-STAT-RETAINED                 VALUE ' '.       00009400
009500         88 WS-LOC-STAT-CHANGED                  VALUE 'X'.       00009500
009600     05 WS-REPT-TBL-TXT                PIC X(20).                 00009600
009700     05 WS-REPT-TBL-NUMERIC REDEFINES WS-REPT-TBL-TXT             00009700
009800                                       PIC S9(3) COMP-3 OCCURS    00009800
009900                                       10 TIMES.                  00009900
010000                                                                  00010000
010100                                                                  00010100
010200 01 WS-MMMS0291-PGM                    PIC X(8) VALUE 'MMMS0291'. 00010200
010300                                                                  00010300
010400 01 WS-NNNN0000-EXIT-CODES             PIC S9(4) COMP VALUE 0.    00010400
010500 01 WS-CURRENT-DATE-DATA               PIC X(26) VALUE SPACES.    00010500
010600 01 MMMS0304-RI-DEL-CHK                PIC X(8)  VALUE 'MMMS0304'.00010600
010700 01 MMMC0335-RI-INSERT-CHK             PIC X(8)  VALUE 'MMMS0335'.00010700
010800                                                                  00010800
010900* ========================< COPY BOOKS >==========================00010900
011000* Place all copy books in this section.                           00011000
011100* ================================================================00011100
011200 COPY DFHEIBLK.                                                   00011200
011300 COPY NNNN000U.                                                   00011300
011400 COPY HHHTLR01.                                                   00011400
011500 COPY PPPTLO01.                                                   00011500
011600                                                                  00011600
011700 COPY YYYN000A.                                                   00011700
011800 COPY YYYN000C.                                                   00011800
011900 COPY YYYN110A.                                                   00011900
012000 COPY YYYC0127.                                                   00012000
012100                                                                  00012100
012200 COPY YYYC0107.                                                   00012200
012300 COPY YYYC0175.                                                   00012300
012400                                                                  00012400
012500 COPY ZZZC0197.                                                   00012500
012600 COPY ZZZC0032.                                                   00012600
012700 COPY ZZZC0094.                                                   00012700
012800 COPY ZZZC0210.                                                   00012800
012900                                                                  00012900
013000 COPY WWWC0100.                                                   00013000
013100                                                                  00013100
013200 COPY MMMC0159.                                                   00013200
013300 COPY MMMK001B.                                                   00013300
013400 COPY MMMC0291.                                                   00013400
013500 COPY MMMC0304.                                                   00013500
013600 COPY MMMC0335.                                                   00013600
013700                                                                  00013700
013800* ========================< DCL GENS >============================00013800
013900* Place all DCL gens in this section.                             00013900
014000* ================================================================00014000
014100     EXEC SQL                                                     00014100
014200       INCLUDE DDDTLR01                                           00014200
014300     END-EXEC.                                                    00014300
014400                                                                  00014400
014500* ========================< CURSORS >=============================00014500
014600* Place all cursors in this section.                              00014600
014700* ================================================================00014700
014800                                                                  00014800
014900* --------------------------------------------------              00014900
015000* DDDXLR01 cursor declaration.                                    00015000
015100* --------------------------------------------------              00015100
015200     EXEC SQL                                                     00015200
015300         DECLARE DDDXLR01 CURSOR WITH HOLD FOR SELECT             00015300
015400             LOC_NBR,                                             00015400
015500             LOC_TYP_CD,                                          00015500
015600             ASSOC_STR_TYP_CD,                                    00015600
015700             ASSOC_STR_NBR,                                       00015700
015800             STR_REMODL_DT,                                       00015800
015900             RETL_LOC_STAT_CD,                                    00015900
016000             RETL_LOC_STAT_DT,                                    00016000
016100             COMPANY_ID,                                          00016100
016200             FINANCIAL_DIV_ID,                                    00016200
016300             LIN_OF_BUS_ID,                                       00016300
016400             DIST_ID,                                             00016400
016500             MKT_RGN_ID,                                          00016500
016600             GEO_ZN_CD,                                           00016600
016700             RETL_GEO_ZN_ID,                                      00016700
016800             SCN_MAINT_SW,                                        00016800
016900             FRNT_END_CD,                                         00016900
017000             PRC_BUL_SW,                                          00017000
017100             UPC_ON_PRC_BUL_SW,                                   00017100
017200             CMPTR_TYP_CD,                                        00017200
017300             RETL_VID_ZN_NBR,                                     00017300
017400             RETL_UNLD_CD,                                        00017400
017500             ROLUP_REPT_TBL_TXT,                                  00017500
017600             NEW_STR_SW,                                          00017600
017700             SEL_CIR_SW,                                          00017700
017800             BKRM_SQ_FT,                                          00017800
017900             FD_LINER_FT,                                         00017900
018000             NON_FD_LINER_FT,                                     00018000
018100             SETOFF_ROOM_SW,                                      00018100
018200             CAT_CLS_TBL_TXT,                                     00018200
018300             LAT_K,                                               00018300
018400             LON_K,                                               00018400
018500             CK_COLL_REPT_SW,                                     00018500
018600             CK_COLL_CNTL_CD,                                     00018600
018700             CK_COLL_ADD_DEL_SW,                                  00018700
018800             CK_ALT_STR_ID,                                       00018800
018900             CK_COLL_FEE_AMT,                                     00018900
019000             SALS_TAX_PCT,                                        00019000
019100             SOAP_SALE_VAR_PCT,                                   00019100
019200             ON_SRS_CD,                                           00019200
019300             SRS_DSD_ORD_SW,                                      00019300
019400             RETL_LOC_TYP_CD,                                     00019400
019500             DEA_NBR,                                             00019500
019600             STR_OPSTMT_SRT_CD,                                   00019600
019700             STR_OPSTMT_TYP_CD,                                   00019700
019800             STR_OPSTMT_HDR_CD,                                   00019800
019900             DPS_NBR,                                             00019900
020000             MEDICARE_ID,                                         00020000
020100             NABP_NBR,                                            00020100
020200             NATL_PROV_ID,                                        00020200
020300             CURR_AD_ZN_NBR,                                      00020300
020400             PD_ZONE_NO,                                          00020400
020500             SOS_PROC_SW,                                         00020500
020600             RPRT_SEQ_NBR,                                        00020600
020700             GRP_CD,                                              00020700
020800             PRIM_GRP_CD_1,                                       00020800
020900             PRIM_GRP_CD_2,                                       00020900
021000             SECY_GRP_CD_1,                                       00021000
021100             SECY_GRP_CD_2,                                       00021100
021200             PRIM_CLS_NBR_1,                                      00021200
021300             PRIM_CLS_NBR_2,                                      00021300
021400             SECY_CLS_NBR_1,                                      00021400
021500             SECY_CLS_NBR_2,                                      00021500
021600             VAL_STR_SW ,                                         00021600
021700             SLS_CLOSED_DT,                                       00021700
021800             TBCO_PRMT_NBR,                                       00021800
021900             SUB_UNLIKE_PROD_CD,                                  00021900
022000             SUB_DSPLY_PAL_CD,                                    00022000
022100             RLTM_SCN_MAINT_SW,                                   00022100
022200             TOP_LEADER_NM,                                       00022200
022300             CUST_FRNDLY_NM,                                      00022300
022400             SLS_OPEN_DT,                                         00022400
022500             MON_OPEN_TM,                                         00022500
022600             MON_CLOS_TM,                                         00022600
022700             TUE_OPEN_TM,                                         00022700
022800             TUE_CLOS_TM,                                         00022800
022900             WED_OPEN_TM,                                         00022900
023000             WED_CLOS_TM,                                         00023000
023100             THUR_OPEN_TM,                                        00023100
023200             THUR_CLOS_TM,                                        00023200
023300             FRI_OPEN_TM,                                         00023300
023400             FRI_CLOS_TM,                                         00023400
023500             SAT_OPEN_TM,                                         00023500
023600             SUN_OPEN_TM,                                         00023600
023700             SAT_CLOS_TM,                                         00023700
023800             SUN_CLOS_TM,                                         00023800
023900             RETL_LOC_FRMAT_CD,                                   00023900
024000             RETL_LOC_SEGM_CD,                                    00024000
024100             ECOMM_MKT_AREA_CD,                                   00024100
024200             ECOMM_STRT_DT,                                       00024200
024300             ECOMM_END_DT,                                        00024300
024400             ROLUP_REPT_TBL_01_NBR,                               00024400
024500             ROLUP_REPT_TBL_02_NBR,                               00024500
024600             ROLUP_REPT_TBL_03_NBR,                               00024600
024700             ROLUP_REPT_TBL_04_NBR,                               00024700
024800             ROLUP_REPT_TBL_05_NBR,                               00024800
024900             ROLUP_REPT_TBL_06_NBR,                               00024900
025000             ROLUP_REPT_TBL_07_NBR,                               00025000
025100             ROLUP_REPT_TBL_08_NBR,                               00025100
025200             ROLUP_REPT_TBL_09_NBR,                               00025200
025300             ROLUP_REPT_TBL_10_NBR,                               00025300
025400             ONLIN_SSON_SW,                                       00025400
025500             RPLACD_BY_STR_NBR                                    00025500
025600         FROM     XXXAIL_LOC                                      00025600
025700         WHERE   (LOC_NBR >= :DCLXXXAIL-LOC.LOC-NBR)              00025700
025800         AND NOT                                                  00025800
025900                 (LOC_NBR  = :DCLXXXAIL-LOC.LOC-NBR AND           00025900
026000                  LOC_TYP_CD  < :DCLXXXAIL-LOC.LOC-TYP-CD)        00026000
026100         ORDER BY                                                 00026100
026200             LOC_NBR,                                             00026200
026300             LOC_TYP_CD                                           00026300
026400     END-EXEC.                                                    00026400
026500* --------------------------------------------------              00026500
026600* DDDXLR02 cursor declaration.                                    00026600
026700* --------------------------------------------------              00026700
026800     EXEC SQL                                                     00026800
026900         DECLARE DDDXLR02 CURSOR WITH HOLD FOR SELECT             00026900
027000             LOC_NBR,                                             00027000
027100             LOC_TYP_CD,                                          00027100
027200             ASSOC_STR_TYP_CD,                                    00027200
027300             ASSOC_STR_NBR,                                       00027300
027400             STR_REMODL_DT,                                       00027400
027500             RETL_LOC_STAT_CD,                                    00027500
027600             RETL_LOC_STAT_DT,                                    00027600
027700             COMPANY_ID,                                          00027700
027800             FINANCIAL_DIV_ID,                                    00027800
027900             LIN_OF_BUS_ID,                                       00027900
028000             DIST_ID,                                             00028000
028100             MKT_RGN_ID,                                          00028100
028200             GEO_ZN_CD,                                           00028200
028300             RETL_GEO_ZN_ID,                                      00028300
028400             SCN_MAINT_SW,                                        00028400
028500             FRNT_END_CD,                                         00028500
028600             PRC_BUL_SW,                                          00028600
028700             UPC_ON_PRC_BUL_SW,                                   00028700
028800             CMPTR_TYP_CD,                                        00028800
028900             RETL_VID_ZN_NBR,                                     00028900
029000             RETL_UNLD_CD,                                        00029000
029100             ROLUP_REPT_TBL_TXT,                                  00029100
029200             NEW_STR_SW,                                          00029200
029300             SEL_CIR_SW,                                          00029300
029400             BKRM_SQ_FT,                                          00029400
029500             FD_LINER_FT,                                         00029500
029600             NON_FD_LINER_FT,                                     00029600
029700             SETOFF_ROOM_SW,                                      00029700
029800             CAT_CLS_TBL_TXT,                                     00029800
029900             LAT_K,                                               00029900
030000             LON_K,                                               00030000
030100             CK_COLL_REPT_SW,                                     00030100
030200             CK_COLL_CNTL_CD,                                     00030200
030300             CK_COLL_ADD_DEL_SW,                                  00030300
030400             CK_ALT_STR_ID,                                       00030400
030500             CK_COLL_FEE_AMT,                                     00030500
030600             SALS_TAX_PCT,                                        00030600
030700             SOAP_SALE_VAR_PCT,                                   00030700
030800             ON_SRS_CD,                                           00030800
030900             SRS_DSD_ORD_SW,                                      00030900
031000             RETL_LOC_TYP_CD,                                     00031000
031100             DEA_NBR,                                             00031100
031200             STR_OPSTMT_SRT_CD,                                   00031200
031300             STR_OPSTMT_TYP_CD,                                   00031300
031400             STR_OPSTMT_HDR_CD,                                   00031400
031500             DPS_NBR,                                             00031500
031600             MEDICARE_ID,                                         00031600
031700             NABP_NBR,                                            00031700
031800             NATL_PROV_ID,                                        00031800
031900             CURR_AD_ZN_NBR,                                      00031900
032000             PD_ZONE_NO,                                          00032000
032100             SOS_PROC_SW,                                         00032100
032200             RPRT_SEQ_NBR,                                        00032200
032300             GRP_CD,                                              00032300
032400             PRIM_GRP_CD_1,                                       00032400
032500             PRIM_GRP_CD_2,                                       00032500
032600             SECY_GRP_CD_1,                                       00032600
032700             SECY_GRP_CD_2,                                       00032700
032800             PRIM_CLS_NBR_1,                                      00032800
032900             PRIM_CLS_NBR_2,                                      00032900
033000             SECY_CLS_NBR_1,                                      00033000
033100             SECY_CLS_NBR_2,                                      00033100
033200             VAL_STR_SW,                                          00033200
033300             SLS_CLOSED_DT,                                       00033300
033400             TBCO_PRMT_NBR,                                       00033400
033500             SUB_UNLIKE_PROD_CD,                                  00033500
033600             SUB_DSPLY_PAL_CD,                                    00033600
033700             RLTM_SCN_MAINT_SW,                                   00033700
033800             TOP_LEADER_NM,                                       00033800
033900             CUST_FRNDLY_NM,                                      00033900
034000             SLS_OPEN_DT,                                         00034000
034100             MON_OPEN_TM,                                         00034100
034200             MON_CLOS_TM,                                         00034200
034300             TUE_OPEN_TM,                                         00034300
034400             TUE_CLOS_TM,                                         00034400
034500             WED_OPEN_TM,                                         00034500
034600             WED_CLOS_TM,                                         00034600
034700             THUR_OPEN_TM,                                        00034700
034800             THUR_CLOS_TM,                                        00034800
034900             FRI_OPEN_TM,                                         00034900
035000             FRI_CLOS_TM,                                         00035000
035100             SAT_OPEN_TM,                                         00035100
035200             SUN_OPEN_TM,                                         00035200
035300             SAT_CLOS_TM,                                         00035300
035400             SUN_CLOS_TM,                                         00035400
035500             RETL_LOC_FRMAT_CD,                                   00035500
035600             RETL_LOC_SEGM_CD,                                    00035600
035700             ECOMM_MKT_AREA_CD,                                   00035700
035800             ECOMM_STRT_DT,                                       00035800
035900             ECOMM_END_DT,                                        00035900
036000             ROLUP_REPT_TBL_01_NBR,                               00036000
036100             ROLUP_REPT_TBL_02_NBR,                               00036100
036200             ROLUP_REPT_TBL_03_NBR,                               00036200
036300             ROLUP_REPT_TBL_04_NBR,                               00036300
036400             ROLUP_REPT_TBL_05_NBR,                               00036400
036500             ROLUP_REPT_TBL_06_NBR,                               00036500
036600             ROLUP_REPT_TBL_07_NBR,                               00036600
036700             ROLUP_REPT_TBL_08_NBR,                               00036700
036800             ROLUP_REPT_TBL_09_NBR,                               00036800
036900             ROLUP_REPT_TBL_10_NBR,                               00036900
037000             ONLIN_SSON_SW,                                       00037000
037100             RPLACD_BY_STR_NBR                                    00037100
037200         FROM     XXXAIL_LOC                                      00037200
037300         WHERE   (LOC_TYP_CD >= :DCLXXXAIL-LOC.LOC-TYP-CD)        00037300
037400         AND NOT                                                  00037400
037500                 (LOC_TYP_CD  = :DCLXXXAIL-LOC.LOC-TYP-CD AND     00037500
037600                  LOC_NBR  < :DCLXXXAIL-LOC.LOC-NBR)              00037600
037700         ORDER BY                                                 00037700
037800             LOC_TYP_CD,                                          00037800
037900             LOC_NBR                                              00037900
038000     END-EXEC.                                                    00038000
038100* --------------------------------------------------              00038100
038200* DDDXLR03 cursor declaration.                                    00038200
038300* --------------------------------------------------              00038300
038400     EXEC SQL                                                     00038400
038500         DECLARE DDDXLR03 CURSOR WITH HOLD FOR SELECT             00038500
038600             LOC_NBR,                                             00038600
038700             LOC_TYP_CD,                                          00038700
038800             ASSOC_STR_TYP_CD,                                    00038800
038900             ASSOC_STR_NBR,                                       00038900
039000             STR_REMODL_DT,                                       00039000
039100             RETL_LOC_STAT_CD,                                    00039100
039200             RETL_LOC_STAT_DT,                                    00039200
039300             COMPANY_ID,                                          00039300
039400             FINANCIAL_DIV_ID,                                    00039400
039500             LIN_OF_BUS_ID,                                       00039500
039600             DIST_ID,                                             00039600
039700             MKT_RGN_ID,                                          00039700
039800             GEO_ZN_CD,                                           00039800
039900             RETL_GEO_ZN_ID,                                      00039900
040000             SCN_MAINT_SW,                                        00040000
040100             FRNT_END_CD,                                         00040100
040200             PRC_BUL_SW,                                          00040200
040300             UPC_ON_PRC_BUL_SW,                                   00040300
040400             CMPTR_TYP_CD,                                        00040400
040500             RETL_VID_ZN_NBR,                                     00040500
040600             RETL_UNLD_CD,                                        00040600
040700             ROLUP_REPT_TBL_TXT,                                  00040700
040800             NEW_STR_SW,                                          00040800
040900             SEL_CIR_SW,                                          00040900
041000             BKRM_SQ_FT,                                          00041000
041100             FD_LINER_FT,                                         00041100
041200             NON_FD_LINER_FT,                                     00041200
041300             SETOFF_ROOM_SW,                                      00041300
041400             CAT_CLS_TBL_TXT,                                     00041400
041500             LAT_K,                                               00041500
041600             LON_K,                                               00041600
041700             CK_COLL_REPT_SW,                                     00041700
041800             CK_COLL_CNTL_CD,                                     00041800
041900             CK_COLL_ADD_DEL_SW,                                  00041900
042000             CK_ALT_STR_ID,                                       00042000
042100             CK_COLL_FEE_AMT,                                     00042100
042200             SALS_TAX_PCT,                                        00042200
042300             SOAP_SALE_VAR_PCT,                                   00042300
042400             ON_SRS_CD,                                           00042400
042500             SRS_DSD_ORD_SW,                                      00042500
042600             RETL_LOC_TYP_CD,                                     00042600
042700             DEA_NBR,                                             00042700
042800             STR_OPSTMT_SRT_CD,                                   00042800
042900             STR_OPSTMT_TYP_CD,                                   00042900
043000             STR_OPSTMT_HDR_CD,                                   00043000
043100             DPS_NBR,                                             00043100
043200             MEDICARE_ID,                                         00043200
043300             NABP_NBR,                                            00043300
043400             NATL_PROV_ID,                                        00043400
043500             CURR_AD_ZN_NBR,                                      00043500
043600             PD_ZONE_NO,                                          00043600
043700             SOS_PROC_SW,                                         00043700
043800             RPRT_SEQ_NBR,                                        00043800
043900             GRP_CD,                                              00043900
044000             PRIM_GRP_CD_1,                                       00044000
044100             PRIM_GRP_CD_2,                                       00044100
044200             SECY_GRP_CD_1,                                       00044200
044300             SECY_GRP_CD_2,                                       00044300
044400             PRIM_CLS_NBR_1,                                      00044400
044500             PRIM_CLS_NBR_2,                                      00044500
044600             SECY_CLS_NBR_1,                                      00044600
044700             SECY_CLS_NBR_2,                                      00044700
044800             VAL_STR_SW,                                          00044800
044900             SLS_CLOSED_DT,                                       00044900
045000             TBCO_PRMT_NBR,                                       00045000
045100             SUB_UNLIKE_PROD_CD,                                  00045100
045200             SUB_DSPLY_PAL_CD,                                    00045200
045300             RLTM_SCN_MAINT_SW,                                   00045300
045400             TOP_LEADER_NM,                                       00045400
045500             CUST_FRNDLY_NM,                                      00045500
045600             SLS_OPEN_DT,                                         00045600
045700             MON_OPEN_TM,                                         00045700
045800             MON_CLOS_TM,                                         00045800
045900             TUE_OPEN_TM,                                         00045900
046000             TUE_CLOS_TM,                                         00046000
046100             WED_OPEN_TM,                                         00046100
046200             WED_CLOS_TM,                                         00046200
046300             THUR_OPEN_TM,                                        00046300
046400             THUR_CLOS_TM,                                        00046400
046500             FRI_OPEN_TM,                                         00046500
046600             FRI_CLOS_TM,                                         00046600
046700             SAT_OPEN_TM,                                         00046700
046800             SUN_OPEN_TM,                                         00046800
046900             SAT_CLOS_TM,                                         00046900
047000             SUN_CLOS_TM,                                         00047000
047100             RETL_LOC_FRMAT_CD,                                   00047100
047200             RETL_LOC_SEGM_CD,                                    00047200
047300             ECOMM_MKT_AREA_CD,                                   00047300
047400             ECOMM_STRT_DT,                                       00047400
047500             ECOMM_END_DT,                                        00047500
047600             ROLUP_REPT_TBL_01_NBR,                               00047600
047700             ROLUP_REPT_TBL_02_NBR,                               00047700
047800             ROLUP_REPT_TBL_03_NBR,                               00047800
047900             ROLUP_REPT_TBL_04_NBR,                               00047900
048000             ROLUP_REPT_TBL_05_NBR,                               00048000
048100             ROLUP_REPT_TBL_06_NBR,                               00048100
048200             ROLUP_REPT_TBL_07_NBR,                               00048200
048300             ROLUP_REPT_TBL_08_NBR,                               00048300
048400             ROLUP_REPT_TBL_09_NBR,                               00048400
048500             ROLUP_REPT_TBL_10_NBR,                               00048500
048600             ONLIN_SSON_SW,                                       00048600
048700             RPLACD_BY_STR_NBR                                    00048700
048800         FROM     XXXAIL_LOC                                      00048800
048900         WHERE   (ASSOC_STR_NBR >= :DCLXXXAIL-LOC.ASSOC-STR-NBR)  00048900
049000         AND NOT                                                  00049000
049100                 (ASSOC_STR_NBR  =                                00049100
049200                       :DCLXXXAIL-LOC.ASSOC-STR-NBR AND           00049200
049300                  ASSOC_STR_TYP_CD  <                             00049300
049400                        :DCLXXXAIL-LOC.ASSOC-STR-TYP-CD)          00049400
049500         AND NOT                                                  00049500
049600                 (ASSOC_STR_NBR  =                                00049600
049700                       :DCLXXXAIL-LOC.ASSOC-STR-NBR AND           00049700
049800                  ASSOC_STR_TYP_CD  =                             00049800
049900                        :DCLXXXAIL-LOC.ASSOC-STR-TYP-CD AND       00049900
050000                  LOC_NBR  < :DCLXXXAIL-LOC.LOC-NBR)              00050000
050100         AND NOT                                                  00050100
050200                 (ASSOC_STR_NBR  =                                00050200
050300                       :DCLXXXAIL-LOC.ASSOC-STR-NBR AND           00050300
050400                  ASSOC_STR_TYP_CD  =                             00050400
050500                        :DCLXXXAIL-LOC.ASSOC-STR-TYP-CD AND       00050500
050600                  LOC_NBR  = :DCLXXXAIL-LOC.LOC-NBR AND           00050600
050700                  LOC_TYP_CD  < :DCLXXXAIL-LOC.LOC-TYP-CD)        00050700
050800         ORDER BY                                                 00050800
050900             ASSOC_STR_NBR,                                       00050900
051000             ASSOC_STR_TYP_CD,                                    00051000
051100             LOC_NBR,                                             00051100
051200             LOC_TYP_CD                                           00051200
051300     END-EXEC.                                                    00051300
051400* --------------------------------------------------              00051400
051500* DDDXLR04 cursor declaration.                                    00051500
051600* --------------------------------------------------              00051600
051700     EXEC SQL                                                     00051700
051800         DECLARE DDDXLR04 CURSOR WITH HOLD FOR SELECT             00051800
051900             LOC_NBR,                                             00051900
052000             LOC_TYP_CD,                                          00052000
052100             ASSOC_STR_TYP_CD,                                    00052100
052200             ASSOC_STR_NBR,                                       00052200
052300             STR_REMODL_DT,                                       00052300
052400             RETL_LOC_STAT_CD,                                    00052400
052500             RETL_LOC_STAT_DT,                                    00052500
052600             COMPANY_ID,                                          00052600
052700             FINANCIAL_DIV_ID,                                    00052700
052800             LIN_OF_BUS_ID,                                       00052800
052900             DIST_ID,                                             00052900
053000             MKT_RGN_ID,                                          00053000
053100             GEO_ZN_CD,                                           00053100
053200             RETL_GEO_ZN_ID,                                      00053200
053300             SCN_MAINT_SW,                                        00053300
053400             FRNT_END_CD,                                         00053400
053500             PRC_BUL_SW,                                          00053500
053600             UPC_ON_PRC_BUL_SW,                                   00053600
053700             CMPTR_TYP_CD,                                        00053700
053800             RETL_VID_ZN_NBR,                                     00053800
053900             RETL_UNLD_CD,                                        00053900
054000             ROLUP_REPT_TBL_TXT,                                  00054000
054100             NEW_STR_SW,                                          00054100
054200             SEL_CIR_SW,                                          00054200
054300             BKRM_SQ_FT,                                          00054300
054400             FD_LINER_FT,                                         00054400
054500             NON_FD_LINER_FT,                                     00054500
054600             SETOFF_ROOM_SW,                                      00054600
054700             CAT_CLS_TBL_TXT,                                     00054700
054800             LAT_K,                                               00054800
054900             LON_K,                                               00054900
055000             CK_COLL_REPT_SW,                                     00055000
055100             CK_COLL_CNTL_CD,                                     00055100
055200             CK_COLL_ADD_DEL_SW,                                  00055200
055300             CK_ALT_STR_ID,                                       00055300
055400             CK_COLL_FEE_AMT,                                     00055400
055500             SALS_TAX_PCT,                                        00055500
055600             SOAP_SALE_VAR_PCT,                                   00055600
055700             ON_SRS_CD,                                           00055700
055800             SRS_DSD_ORD_SW,                                      00055800
055900             RETL_LOC_TYP_CD,                                     00055900
056000             DEA_NBR,                                             00056000
056100             STR_OPSTMT_SRT_CD,                                   00056100
056200             STR_OPSTMT_TYP_CD,                                   00056200
056300             STR_OPSTMT_HDR_CD,                                   00056300
056400             DPS_NBR,                                             00056400
056500             MEDICARE_ID,                                         00056500
056600             NABP_NBR,                                            00056600
056700             NATL_PROV_ID,                                        00056700
056800             CURR_AD_ZN_NBR,                                      00056800
056900             PD_ZONE_NO,                                          00056900
057000             SOS_PROC_SW,                                         00057000
057100             RPRT_SEQ_NBR,                                        00057100
057200             GRP_CD,                                              00057200
057300             PRIM_GRP_CD_1,                                       00057300
057400             PRIM_GRP_CD_2,                                       00057400
057500             SECY_GRP_CD_1,                                       00057500
057600             SECY_GRP_CD_2,                                       00057600
057700             PRIM_CLS_NBR_1,                                      00057700
057800             PRIM_CLS_NBR_2,                                      00057800
057900             SECY_CLS_NBR_1,                                      00057900
058000             SECY_CLS_NBR_2,                                      00058000
058100             VAL_STR_SW,                                          00058100
058200             SLS_CLOSED_DT,                                       00058200
058300             TBCO_PRMT_NBR,                                       00058300
058400             SUB_UNLIKE_PROD_CD,                                  00058400
058500             SUB_DSPLY_PAL_CD,                                    00058500
058600             RLTM_SCN_MAINT_SW,                                   00058600
058700             TOP_LEADER_NM,                                       00058700
058800             CUST_FRNDLY_NM,                                      00058800
058900             SLS_OPEN_DT,                                         00058900
059000             MON_OPEN_TM,                                         00059000
059100             MON_CLOS_TM,                                         00059100
059200             TUE_OPEN_TM,                                         00059200
059300             TUE_CLOS_TM,                                         00059300
059400             WED_OPEN_TM,                                         00059400
059500             WED_CLOS_TM,                                         00059500
059600             THUR_OPEN_TM,                                        00059600
059700             THUR_CLOS_TM,                                        00059700
059800             FRI_OPEN_TM,                                         00059800
059900             FRI_CLOS_TM,                                         00059900
060000             SAT_OPEN_TM,                                         00060000
060100             SUN_OPEN_TM,                                         00060100
060200             SAT_CLOS_TM,                                         00060200
060300             SUN_CLOS_TM,                                         00060300
060400             RETL_LOC_FRMAT_CD,                                   00060400
060500             RETL_LOC_SEGM_CD,                                    00060500
060600             ECOMM_MKT_AREA_CD,                                   00060600
060700             ECOMM_STRT_DT,                                       00060700
060800             ECOMM_END_DT,                                        00060800
060900             ROLUP_REPT_TBL_01_NBR,                               00060900
061000             ROLUP_REPT_TBL_02_NBR,                               00061000
061100             ROLUP_REPT_TBL_03_NBR,                               00061100
061200             ROLUP_REPT_TBL_04_NBR,                               00061200
061300             ROLUP_REPT_TBL_05_NBR,                               00061300
061400             ROLUP_REPT_TBL_06_NBR,                               00061400
061500             ROLUP_REPT_TBL_07_NBR,                               00061500
061600             ROLUP_REPT_TBL_08_NBR,                               00061600
061700             ROLUP_REPT_TBL_09_NBR,                               00061700
061800             ROLUP_REPT_TBL_10_NBR,                               00061800
061900             ONLIN_SSON_SW,                                       00061900
062000             RPLACD_BY_STR_NBR                                    00062000
062100         FROM     XXXAIL_LOC                                      00062100
062200         WHERE   (COMPANY_ID >= :DCLXXXAIL-LOC.COMPANY-ID)        00062200
062300         AND NOT                                                  00062300
062400                 (COMPANY_ID  = :DCLXXXAIL-LOC.COMPANY-ID AND     00062400
062500                  LOC_NBR  < :DCLXXXAIL-LOC.LOC-NBR)              00062500
062600         AND NOT                                                  00062600
062700                 (COMPANY_ID  = :DCLXXXAIL-LOC.COMPANY-ID AND     00062700
062800                  LOC_NBR  = :DCLXXXAIL-LOC.LOC-NBR AND           00062800
062900                  LOC_TYP_CD  < :DCLXXXAIL-LOC.LOC-TYP-CD)        00062900
063000         ORDER BY                                                 00063000
063100             COMPANY_ID,                                          00063100
063200             LOC_NBR,                                             00063200
063300             LOC_TYP_CD                                           00063300
063400     END-EXEC.                                                    00063400
063500* --------------------------------------------------              00063500
063600* DDDXLR05 cursor declaration.                                    00063600
063700* --------------------------------------------------              00063700
063800     EXEC SQL                                                     00063800
063900         DECLARE DDDXLR05 CURSOR WITH HOLD FOR SELECT             00063900
064000             LOC_NBR,                                             00064000
064100             LOC_TYP_CD,                                          00064100
064200             ASSOC_STR_TYP_CD,                                    00064200
064300             ASSOC_STR_NBR,                                       00064300
064400             STR_REMODL_DT,                                       00064400
064500             RETL_LOC_STAT_CD,                                    00064500
064600             RETL_LOC_STAT_DT,                                    00064600
064700             COMPANY_ID,                                          00064700
064800             FINANCIAL_DIV_ID,                                    00064800
064900             LIN_OF_BUS_ID,                                       00064900
065000             DIST_ID,                                             00065000
065100             MKT_RGN_ID,                                          00065100
065200             GEO_ZN_CD,                                           00065200
065300             RETL_GEO_ZN_ID,                                      00065300
065400             SCN_MAINT_SW,                                        00065400
065500             FRNT_END_CD,                                         00065500
065600             PRC_BUL_SW,                                          00065600
065700             UPC_ON_PRC_BUL_SW,                                   00065700
065800             CMPTR_TYP_CD,                                        00065800
065900             RETL_VID_ZN_NBR,                                     00065900
066000             RETL_UNLD_CD,                                        00066000
066100             ROLUP_REPT_TBL_TXT,                                  00066100
066200             NEW_STR_SW,                                          00066200
066300             SEL_CIR_SW,                                          00066300
066400             BKRM_SQ_FT,                                          00066400
066500             FD_LINER_FT,                                         00066500
066600             NON_FD_LINER_FT,                                     00066600
066700             SETOFF_ROOM_SW,                                      00066700
066800             CAT_CLS_TBL_TXT,                                     00066800
066900             LAT_K,                                               00066900
067000             LON_K,                                               00067000
067100             CK_COLL_REPT_SW,                                     00067100
067200             CK_COLL_CNTL_CD,                                     00067200
067300             CK_COLL_ADD_DEL_SW,                                  00067300
067400             CK_ALT_STR_ID,                                       00067400
067500             CK_COLL_FEE_AMT,                                     00067500
067600             SALS_TAX_PCT,                                        00067600
067700             SOAP_SALE_VAR_PCT,                                   00067700
067800             ON_SRS_CD,                                           00067800
067900             SRS_DSD_ORD_SW,                                      00067900
068000             RETL_LOC_TYP_CD,                                     00068000
068100             DEA_NBR,                                             00068100
068200             STR_OPSTMT_SRT_CD,                                   00068200
068300             STR_OPSTMT_TYP_CD,                                   00068300
068400             STR_OPSTMT_HDR_CD,                                   00068400
068500             DPS_NBR,                                             00068500
068600             MEDICARE_ID,                                         00068600
068700             NABP_NBR,                                            00068700
068800             NATL_PROV_ID,                                        00068800
068900             CURR_AD_ZN_NBR,                                      00068900
069000             PD_ZONE_NO,                                          00069000
069100             SOS_PROC_SW,                                         00069100
069200             RPRT_SEQ_NBR,                                        00069200
069300             GRP_CD,                                              00069300
069400             PRIM_GRP_CD_1,                                       00069400
069500             PRIM_GRP_CD_2,                                       00069500
069600             SECY_GRP_CD_1,                                       00069600
069700             SECY_GRP_CD_2,                                       00069700
069800             PRIM_CLS_NBR_1,                                      00069800
069900             PRIM_CLS_NBR_2,                                      00069900
070000             SECY_CLS_NBR_1,                                      00070000
070100             SECY_CLS_NBR_2,                                      00070100
070200             VAL_STR_SW,                                          00070200
070300             SLS_CLOSED_DT,                                       00070300
070400             TBCO_PRMT_NBR,                                       00070400
070500             SUB_UNLIKE_PROD_CD,                                  00070500
070600             SUB_DSPLY_PAL_CD,                                    00070600
070700             RLTM_SCN_MAINT_SW,                                   00070700
070800             TOP_LEADER_NM,                                       00070800
070900             CUST_FRNDLY_NM,                                      00070900
071000             SLS_OPEN_DT,                                         00071000
071100             MON_OPEN_TM,                                         00071100
071200             MON_CLOS_TM,                                         00071200
071300             TUE_OPEN_TM,                                         00071300
071400             TUE_CLOS_TM,                                         00071400
071500             WED_OPEN_TM,                                         00071500
071600             WED_CLOS_TM,                                         00071600
071700             THUR_OPEN_TM,                                        00071700
071800             THUR_CLOS_TM,                                        00071800
071900             FRI_OPEN_TM,                                         00071900
072000             FRI_CLOS_TM,                                         00072000
072100             SAT_OPEN_TM,                                         00072100
072200             SUN_OPEN_TM,                                         00072200
072300             SAT_CLOS_TM,                                         00072300
072400             SUN_CLOS_TM,                                         00072400
072500             RETL_LOC_FRMAT_CD,                                   00072500
072600             RETL_LOC_SEGM_CD,                                    00072600
072700             ECOMM_MKT_AREA_CD,                                   00072700
072800             ECOMM_STRT_DT,                                       00072800
072900             ECOMM_END_DT,                                        00072900
073000             ROLUP_REPT_TBL_01_NBR,                               00073000
073100             ROLUP_REPT_TBL_02_NBR,                               00073100
073200             ROLUP_REPT_TBL_03_NBR,                               00073200
073300             ROLUP_REPT_TBL_04_NBR,                               00073300
073400             ROLUP_REPT_TBL_05_NBR,                               00073400
073500             ROLUP_REPT_TBL_06_NBR,                               00073500
073600             ROLUP_REPT_TBL_07_NBR,                               00073600
073700             ROLUP_REPT_TBL_08_NBR,                               00073700
073800             ROLUP_REPT_TBL_09_NBR,                               00073800
073900             ROLUP_REPT_TBL_10_NBR,                               00073900
074000             ONLIN_SSON_SW,                                       00074000
074100             RPLACD_BY_STR_NBR                                    00074100
074200         FROM     XXXAIL_LOC                                      00074200
074300         WHERE   (DIST_ID >= :DCLXXXAIL-LOC.DIST-ID)              00074300
074400         AND NOT                                                  00074400
074500                 (DIST_ID  = :DCLXXXAIL-LOC.DIST-ID AND           00074500
074600                  LOC_NBR  < :DCLXXXAIL-LOC.LOC-NBR)              00074600
074700         AND NOT                                                  00074700
074800                 (DIST_ID  = :DCLXXXAIL-LOC.DIST-ID AND           00074800
074900                  LOC_NBR  = :DCLXXXAIL-LOC.LOC-NBR AND           00074900
075000                  LOC_TYP_CD  < :DCLXXXAIL-LOC.LOC-TYP-CD)        00075000
075100         ORDER BY                                                 00075100
075200             DIST_ID,                                             00075200
075300             LOC_NBR,                                             00075300
075400             LOC_TYP_CD                                           00075400
075500     END-EXEC.                                                    00075500
075600* --------------------------------------------------              00075600
075700* DDDXLR06 cursor declaration.                                    00075700
075800* --------------------------------------------------              00075800
075900     EXEC SQL                                                     00075900
076000         DECLARE DDDXLR06 CURSOR WITH HOLD FOR SELECT             00076000
076100             LOC_NBR,                                             00076100
076200             LOC_TYP_CD,                                          00076200
076300             ASSOC_STR_TYP_CD,                                    00076300
076400             ASSOC_STR_NBR,                                       00076400
076500             STR_REMODL_DT,                                       00076500
076600             RETL_LOC_STAT_CD,                                    00076600
076700             RETL_LOC_STAT_DT,                                    00076700
076800             COMPANY_ID,                                          00076800
076900             FINANCIAL_DIV_ID,                                    00076900
077000             LIN_OF_BUS_ID,                                       00077000
077100             DIST_ID,                                             00077100
077200             MKT_RGN_ID,                                          00077200
077300             GEO_ZN_CD,                                           00077300
077400             RETL_GEO_ZN_ID,                                      00077400
077500             SCN_MAINT_SW,                                        00077500
077600             FRNT_END_CD,                                         00077600
077700             PRC_BUL_SW,                                          00077700
077800             UPC_ON_PRC_BUL_SW,                                   00077800
077900             CMPTR_TYP_CD,                                        00077900
078000             RETL_VID_ZN_NBR,                                     00078000
078100             RETL_UNLD_CD,                                        00078100
078200             ROLUP_REPT_TBL_TXT,                                  00078200
078300             NEW_STR_SW,                                          00078300
078400             SEL_CIR_SW,                                          00078400
078500             BKRM_SQ_FT,                                          00078500
078600             FD_LINER_FT,                                         00078600
078700             NON_FD_LINER_FT,                                     00078700
078800             SETOFF_ROOM_SW,                                      00078800
078900             CAT_CLS_TBL_TXT,                                     00078900
079000             LAT_K,                                               00079000
079100             LON_K,                                               00079100
079200             CK_COLL_REPT_SW,                                     00079200
079300             CK_COLL_CNTL_CD,                                     00079300
079400             CK_COLL_ADD_DEL_SW,                                  00079400
079500             CK_ALT_STR_ID,                                       00079500
079600             CK_COLL_FEE_AMT,                                     00079600
079700             SALS_TAX_PCT,                                        00079700
079800             SOAP_SALE_VAR_PCT,                                   00079800
079900             ON_SRS_CD,                                           00079900
080000             SRS_DSD_ORD_SW,                                      00080000
080100             RETL_LOC_TYP_CD,                                     00080100
080200             DEA_NBR,                                             00080200
080300             STR_OPSTMT_SRT_CD,                                   00080300
080400             STR_OPSTMT_TYP_CD,                                   00080400
080500             STR_OPSTMT_HDR_CD,                                   00080500
080600             DPS_NBR,                                             00080600
080700             MEDICARE_ID,                                         00080700
080800             NABP_NBR,                                            00080800
080900             NATL_PROV_ID,                                        00080900
081000             CURR_AD_ZN_NBR,                                      00081000
081100             PD_ZONE_NO,                                          00081100
081200             SOS_PROC_SW,                                         00081200
081300             RPRT_SEQ_NBR,                                        00081300
081400             GRP_CD,                                              00081400
081500             PRIM_GRP_CD_1,                                       00081500
081600             PRIM_GRP_CD_2,                                       00081600
081700             SECY_GRP_CD_1,                                       00081700
081800             SECY_GRP_CD_2,                                       00081800
081900             PRIM_CLS_NBR_1,                                      00081900
082000             PRIM_CLS_NBR_2,                                      00082000
082100             SECY_CLS_NBR_1,                                      00082100
082200             SECY_CLS_NBR_2,                                      00082200
082300             VAL_STR_SW,                                          00082300
082400             SLS_CLOSED_DT,                                       00082400
082500             TBCO_PRMT_NBR,                                       00082500
082600             SUB_UNLIKE_PROD_CD,                                  00082600
082700             SUB_DSPLY_PAL_CD,                                    00082700
082800             RLTM_SCN_MAINT_SW,                                   00082800
082900             TOP_LEADER_NM,                                       00082900
083000             CUST_FRNDLY_NM,                                      00083000
083100             SLS_OPEN_DT,                                         00083100
083200             MON_OPEN_TM,                                         00083200
083300             MON_CLOS_TM,                                         00083300
083400             TUE_OPEN_TM,                                         00083400
083500             TUE_CLOS_TM,                                         00083500
083600             WED_OPEN_TM,                                         00083600
083700             WED_CLOS_TM,                                         00083700
083800             THUR_OPEN_TM,                                        00083800
083900             THUR_CLOS_TM,                                        00083900
084000             FRI_OPEN_TM,                                         00084000
084100             FRI_CLOS_TM,                                         00084100
084200             SAT_OPEN_TM,                                         00084200
084300             SUN_OPEN_TM,                                         00084300
084400             SAT_CLOS_TM,                                         00084400
084500             SUN_CLOS_TM,                                         00084500
084600             RETL_LOC_FRMAT_CD,                                   00084600
084700             RETL_LOC_SEGM_CD,                                    00084700
084800             ECOMM_MKT_AREA_CD,                                   00084800
084900             ECOMM_STRT_DT,                                       00084900
085000             ECOMM_END_DT,                                        00085000
085100             ROLUP_REPT_TBL_01_NBR,                               00085100
085200             ROLUP_REPT_TBL_02_NBR,                               00085200
085300             ROLUP_REPT_TBL_03_NBR,                               00085300
085400             ROLUP_REPT_TBL_04_NBR,                               00085400
085500             ROLUP_REPT_TBL_05_NBR,                               00085500
085600             ROLUP_REPT_TBL_06_NBR,                               00085600
085700             ROLUP_REPT_TBL_07_NBR,                               00085700
085800             ROLUP_REPT_TBL_08_NBR,                               00085800
085900             ROLUP_REPT_TBL_09_NBR,                               00085900
086000             ROLUP_REPT_TBL_10_NBR,                               00086000
086100             ONLIN_SSON_SW,                                       00086100
086200             RPLACD_BY_STR_NBR                                    00086200
086300         FROM     XXXAIL_LOC                                      00086300
086400         WHERE   (MKT_RGN_ID >= :DCLXXXAIL-LOC.MKT-RGN-ID)        00086400
086500         AND NOT                                                  00086500
086600                 (MKT_RGN_ID  = :DCLXXXAIL-LOC.MKT-RGN-ID AND     00086600
086700                  LOC_NBR  < :DCLXXXAIL-LOC.LOC-NBR)              00086700
086800         AND NOT                                                  00086800
086900                 (MKT_RGN_ID  = :DCLXXXAIL-LOC.MKT-RGN-ID AND     00086900
087000                  LOC_NBR  = :DCLXXXAIL-LOC.LOC-NBR AND           00087000
087100                  LOC_TYP_CD  < :DCLXXXAIL-LOC.LOC-TYP-CD)        00087100
087200         ORDER BY                                                 00087200
087300             MKT_RGN_ID,                                          00087300
087400             LOC_NBR,                                             00087400
087500             LOC_TYP_CD                                           00087500
087600     END-EXEC.                                                    00087600
087700* --------------------------------------------------              00087700
087800* DDDXLR07 cursor declaration.                                    00087800
087900* --------------------------------------------------              00087900
088000     EXEC SQL                                                     00088000
088100         DECLARE DDDXLR07 CURSOR WITH HOLD FOR SELECT             00088100
088200             LOC_NBR,                                             00088200
088300             LOC_TYP_CD,                                          00088300
088400             ASSOC_STR_TYP_CD,                                    00088400
088500             ASSOC_STR_NBR,                                       00088500
088600             STR_REMODL_DT,                                       00088600
088700             RETL_LOC_STAT_CD,                                    00088700
088800             RETL_LOC_STAT_DT,                                    00088800
088900             COMPANY_ID,                                          00088900
089000             FINANCIAL_DIV_ID,                                    00089000
089100             LIN_OF_BUS_ID,                                       00089100
089200             DIST_ID,                                             00089200
089300             MKT_RGN_ID,                                          00089300
089400             GEO_ZN_CD,                                           00089400
089500             RETL_GEO_ZN_ID,                                      00089500
089600             SCN_MAINT_SW,                                        00089600
089700             FRNT_END_CD,                                         00089700
089800             PRC_BUL_SW,                                          00089800
089900             UPC_ON_PRC_BUL_SW,                                   00089900
090000             CMPTR_TYP_CD,                                        00090000
090100             RETL_VID_ZN_NBR,                                     00090100
090200             RETL_UNLD_CD,                                        00090200
090300             ROLUP_REPT_TBL_TXT,                                  00090300
090400             NEW_STR_SW,                                          00090400
090500             SEL_CIR_SW,                                          00090500
090600             BKRM_SQ_FT,                                          00090600
090700             FD_LINER_FT,                                         00090700
090800             NON_FD_LINER_FT,                                     00090800
090900             SETOFF_ROOM_SW,                                      00090900
091000             CAT_CLS_TBL_TXT,                                     00091000
091100             LAT_K,                                               00091100
091200             LON_K,                                               00091200
091300             CK_COLL_REPT_SW,                                     00091300
091400             CK_COLL_CNTL_CD,                                     00091400
091500             CK_COLL_ADD_DEL_SW,                                  00091500
091600             CK_ALT_STR_ID,                                       00091600
091700             CK_COLL_FEE_AMT,                                     00091700
091800             SALS_TAX_PCT,                                        00091800
091900             SOAP_SALE_VAR_PCT,                                   00091900
092000             ON_SRS_CD,                                           00092000
092100             SRS_DSD_ORD_SW,                                      00092100
092200             RETL_LOC_TYP_CD,                                     00092200
092300             DEA_NBR,                                             00092300
092400             STR_OPSTMT_SRT_CD,                                   00092400
092500             STR_OPSTMT_TYP_CD,                                   00092500
092600             STR_OPSTMT_HDR_CD,                                   00092600
092700             DPS_NBR,                                             00092700
092800             MEDICARE_ID,                                         00092800
092900             NABP_NBR,                                            00092900
093000             NATL_PROV_ID,                                        00093000
093100             CURR_AD_ZN_NBR,                                      00093100
093200             PD_ZONE_NO,                                          00093200
093300             SOS_PROC_SW,                                         00093300
093400             RPRT_SEQ_NBR,                                        00093400
093500             GRP_CD,                                              00093500
093600             PRIM_GRP_CD_1,                                       00093600
093700             PRIM_GRP_CD_2,                                       00093700
093800             SECY_GRP_CD_1,                                       00093800
093900             SECY_GRP_CD_2,                                       00093900
094000             PRIM_CLS_NBR_1,                                      00094000
094100             PRIM_CLS_NBR_2,                                      00094100
094200             SECY_CLS_NBR_1,                                      00094200
094300             SECY_CLS_NBR_2,                                      00094300
094400             VAL_STR_SW,                                          00094400
094500             SLS_CLOSED_DT,                                       00094500
094600             TBCO_PRMT_NBR,                                       00094600
094700             SUB_UNLIKE_PROD_CD,                                  00094700
094800             SUB_DSPLY_PAL_CD,                                    00094800
094900             RLTM_SCN_MAINT_SW,                                   00094900
095000             TOP_LEADER_NM,                                       00095000
095100             CUST_FRNDLY_NM,                                      00095100
095200             SLS_OPEN_DT,                                         00095200
095300             MON_OPEN_TM,                                         00095300
095400             MON_CLOS_TM,                                         00095400
095500             TUE_OPEN_TM,                                         00095500
095600             TUE_CLOS_TM,                                         00095600
095700             WED_OPEN_TM,                                         00095700
095800             WED_CLOS_TM,                                         00095800
095900             THUR_OPEN_TM,                                        00095900
096000             THUR_CLOS_TM,                                        00096000
096100             FRI_OPEN_TM,                                         00096100
096200             FRI_CLOS_TM,                                         00096200
096300             SAT_OPEN_TM,                                         00096300
096400             SUN_OPEN_TM,                                         00096400
096500             SAT_CLOS_TM,                                         00096500
096600             SUN_CLOS_TM,                                         00096600
096700             RETL_LOC_FRMAT_CD,                                   00096700
096800             RETL_LOC_SEGM_CD,                                    00096800
096900             ECOMM_MKT_AREA_CD,                                   00096900
097000             ECOMM_STRT_DT,                                       00097000
097100             ECOMM_END_DT,                                        00097100
097200             ROLUP_REPT_TBL_01_NBR,                               00097200
097300             ROLUP_REPT_TBL_02_NBR,                               00097300
097400             ROLUP_REPT_TBL_03_NBR,                               00097400
097500             ROLUP_REPT_TBL_04_NBR,                               00097500
097600             ROLUP_REPT_TBL_05_NBR,                               00097600
097700             ROLUP_REPT_TBL_06_NBR,                               00097700
097800             ROLUP_REPT_TBL_07_NBR,                               00097800
097900             ROLUP_REPT_TBL_08_NBR,                               00097900
098000             ROLUP_REPT_TBL_09_NBR,                               00098000
098100             ROLUP_REPT_TBL_10_NBR,                               00098100
098200             ONLIN_SSON_SW,                                       00098200
098300             RPLACD_BY_STR_NBR                                    00098300
098400         FROM     XXXAIL_LOC                                      00098400
098500         WHERE   (RETL_LOC_TYP_CD >=                              00098500
098600               :DCLXXXAIL-LOC.LOC-TYP-CD)                         00098600
098700         AND NOT                                                  00098700
098800                 (RETL_LOC_TYP_CD  =                              00098800
098900                       :DCLXXXAIL-LOC.LOC-TYP-CD AND              00098900
099000                  LOC_NBR  < :DCLXXXAIL-LOC.LOC-NBR)              00099000
099100         AND NOT                                                  00099100
099200                 (RETL_LOC_TYP_CD  =                              00099200
099300                       :DCLXXXAIL-LOC.LOC-TYP-CD AND              00099300
099400                  LOC_NBR  = :DCLXXXAIL-LOC.LOC-NBR AND           00099400
099500                  LOC_TYP_CD  < :DCLXXXAIL-LOC.LOC-TYP-CD)        00099500
099600         ORDER BY                                                 00099600
099700             RETL_LOC_TYP_CD,                                     00099700
099800             LOC_NBR,                                             00099800
099900             LOC_TYP_CD                                           00099900
100000     END-EXEC.                                                    00100000
100100* --------------------------------------------------              00100100
100200* DDDXLR08 cursor declaration.                                    00100200
100300* --------------------------------------------------              00100300
100400     EXEC SQL                                                     00100400
100500         DECLARE DDDXLR08 CURSOR WITH HOLD FOR SELECT             00100500
100600             LOC_NBR,                                             00100600
100700             LOC_TYP_CD,                                          00100700
100800             ASSOC_STR_TYP_CD,                                    00100800
100900             ASSOC_STR_NBR,                                       00100900
101000             STR_REMODL_DT,                                       00101000
101100             RETL_LOC_STAT_CD,                                    00101100
101200             RETL_LOC_STAT_DT,                                    00101200
101300             COMPANY_ID,                                          00101300
101400             FINANCIAL_DIV_ID,                                    00101400
101500             LIN_OF_BUS_ID,                                       00101500
101600             DIST_ID,                                             00101600
101700             MKT_RGN_ID,                                          00101700
101800             GEO_ZN_CD,                                           00101800
101900             RETL_GEO_ZN_ID,                                      00101900
102000             SCN_MAINT_SW,                                        00102000
102100             FRNT_END_CD,                                         00102100
102200             PRC_BUL_SW,                                          00102200
102300             UPC_ON_PRC_BUL_SW,                                   00102300
102400             CMPTR_TYP_CD,                                        00102400
102500             RETL_VID_ZN_NBR,                                     00102500
102600             RETL_UNLD_CD,                                        00102600
102700             ROLUP_REPT_TBL_TXT,                                  00102700
102800             NEW_STR_SW,                                          00102800
102900             SEL_CIR_SW,                                          00102900
103000             BKRM_SQ_FT,                                          00103000
103100             FD_LINER_FT,                                         00103100
103200             NON_FD_LINER_FT,                                     00103200
103300             SETOFF_ROOM_SW,                                      00103300
103400             CAT_CLS_TBL_TXT,                                     00103400
103500             LAT_K,                                               00103500
103600             LON_K,                                               00103600
103700             CK_COLL_REPT_SW,                                     00103700
103800             CK_COLL_CNTL_CD,                                     00103800
103900             CK_COLL_ADD_DEL_SW,                                  00103900
104000             CK_ALT_STR_ID,                                       00104000
104100             CK_COLL_FEE_AMT,                                     00104100
104200             SALS_TAX_PCT,                                        00104200
104300             SOAP_SALE_VAR_PCT,                                   00104300
104400             ON_SRS_CD,                                           00104400
104500             SRS_DSD_ORD_SW,                                      00104500
104600             RETL_LOC_TYP_CD,                                     00104600
104700             DEA_NBR,                                             00104700
104800             STR_OPSTMT_SRT_CD,                                   00104800
104900             STR_OPSTMT_TYP_CD,                                   00104900
105000             STR_OPSTMT_HDR_CD,                                   00105000
105100             DPS_NBR,                                             00105100
105200             MEDICARE_ID,                                         00105200
105300             NABP_NBR,                                            00105300
105400             NATL_PROV_ID,                                        00105400
105500             CURR_AD_ZN_NBR,                                      00105500
105600             PD_ZONE_NO,                                          00105600
105700             SOS_PROC_SW,                                         00105700
105800             RPRT_SEQ_NBR,                                        00105800
105900             GRP_CD,                                              00105900
106000             PRIM_GRP_CD_1,                                       00106000
106100             PRIM_GRP_CD_2,                                       00106100
106200             SECY_GRP_CD_1,                                       00106200
106300             SECY_GRP_CD_2,                                       00106300
106400             PRIM_CLS_NBR_1,                                      00106400
106500             PRIM_CLS_NBR_2,                                      00106500
106600             SECY_CLS_NBR_1,                                      00106600
106700             SECY_CLS_NBR_2,                                      00106700
106800             VAL_STR_SW,                                          00106800
106900             SLS_CLOSED_DT,                                       00106900
107000             TBCO_PRMT_NBR,                                       00107000
107100             SUB_UNLIKE_PROD_CD,                                  00107100
107200             SUB_DSPLY_PAL_CD,                                    00107200
107300             RLTM_SCN_MAINT_SW,                                   00107300
107400             TOP_LEADER_NM,                                       00107400
107500             CUST_FRNDLY_NM,                                      00107500
107600             SLS_OPEN_DT,                                         00107600
107700             MON_OPEN_TM,                                         00107700
107800             MON_CLOS_TM,                                         00107800
107900             TUE_OPEN_TM,                                         00107900
108000             TUE_CLOS_TM,                                         00108000
108100             WED_OPEN_TM,                                         00108100
108200             WED_CLOS_TM,                                         00108200
108300             THUR_OPEN_TM,                                        00108300
108400             THUR_CLOS_TM,                                        00108400
108500             FRI_OPEN_TM,                                         00108500
108600             FRI_CLOS_TM,                                         00108600
108700             SAT_OPEN_TM,                                         00108700
108800             SUN_OPEN_TM,                                         00108800
108900             SAT_CLOS_TM,                                         00108900
109000             SUN_CLOS_TM,                                         00109000
109100             RETL_LOC_FRMAT_CD,                                   00109100
109200             RETL_LOC_SEGM_CD,                                    00109200
109300             ECOMM_MKT_AREA_CD,                                   00109300
109400             ECOMM_STRT_DT,                                       00109400
109500             ECOMM_END_DT,                                        00109500
109600             ROLUP_REPT_TBL_01_NBR,                               00109600
109700             ROLUP_REPT_TBL_02_NBR,                               00109700
109800             ROLUP_REPT_TBL_03_NBR,                               00109800
109900             ROLUP_REPT_TBL_04_NBR,                               00109900
110000             ROLUP_REPT_TBL_05_NBR,                               00110000
110100             ROLUP_REPT_TBL_06_NBR,                               00110100
110200             ROLUP_REPT_TBL_07_NBR,                               00110200
110300             ROLUP_REPT_TBL_08_NBR,                               00110300
110400             ROLUP_REPT_TBL_09_NBR,                               00110400
110500             ROLUP_REPT_TBL_10_NBR,                               00110500
110600             ONLIN_SSON_SW,                                       00110600
110700             RPLACD_BY_STR_NBR                                    00110700
110800         FROM XXXAIL_LOC                                          00110800
110900         WHERE (LOC_TYP_CD  = :DCLXXXAIL-LOC.LOC-TYP-CD AND       00110900
111000                LOC_NBR    >= :DCLXXXAIL-LOC.LOC-NBR)             00111000
111100         ORDER BY                                                 00111100
111200             LOC_TYP_CD,                                          00111200
111300             LOC_NBR                                              00111300
111400     END-EXEC.                                                    00111400
111500* --------------------------------------------------              00111500
111600* DDDXLR09 cursor declaration.                                    00111600
111700* --------------------------------------------------              00111700
111800     EXEC SQL                                                     00111800
111900         DECLARE DDDXLR09 CURSOR WITH HOLD FOR SELECT             00111900
112000             LOC_NBR,                                             00112000
112100             LOC_TYP_CD,                                          00112100
112200             ASSOC_STR_TYP_CD,                                    00112200
112300             ASSOC_STR_NBR,                                       00112300
112400             STR_REMODL_DT,                                       00112400
112500             RETL_LOC_STAT_CD,                                    00112500
112600             RETL_LOC_STAT_DT,                                    00112600
112700             COMPANY_ID,                                          00112700
112800             FINANCIAL_DIV_ID,                                    00112800
112900             LIN_OF_BUS_ID,                                       00112900
113000             DIST_ID,                                             00113000
113100             MKT_RGN_ID,                                          00113100
113200             GEO_ZN_CD,                                           00113200
113300             RETL_GEO_ZN_ID,                                      00113300
113400             SCN_MAINT_SW,                                        00113400
113500             FRNT_END_CD,                                         00113500
113600             PRC_BUL_SW,                                          00113600
113700             UPC_ON_PRC_BUL_SW,                                   00113700
113800             CMPTR_TYP_CD,                                        00113800
113900             RETL_VID_ZN_NBR,                                     00113900
114000             RETL_UNLD_CD,                                        00114000
114100             ROLUP_REPT_TBL_TXT,                                  00114100
114200             NEW_STR_SW,                                          00114200
114300             SEL_CIR_SW,                                          00114300
114400             BKRM_SQ_FT,                                          00114400
114500             FD_LINER_FT,                                         00114500
114600             NON_FD_LINER_FT,                                     00114600
114700             SETOFF_ROOM_SW,                                      00114700
114800             CAT_CLS_TBL_TXT,                                     00114800
114900             LAT_K,                                               00114900
115000             LON_K,                                               00115000
115100             CK_COLL_REPT_SW,                                     00115100
115200             CK_COLL_CNTL_CD,                                     00115200
115300             CK_COLL_ADD_DEL_SW,                                  00115300
115400             CK_ALT_STR_ID,                                       00115400
115500             CK_COLL_FEE_AMT,                                     00115500
115600             SALS_TAX_PCT,                                        00115600
115700             SOAP_SALE_VAR_PCT,                                   00115700
115800             ON_SRS_CD,                                           00115800
115900             SRS_DSD_ORD_SW,                                      00115900
116000             RETL_LOC_TYP_CD,                                     00116000
116100             DEA_NBR,                                             00116100
116200             STR_OPSTMT_SRT_CD,                                   00116200
116300             STR_OPSTMT_TYP_CD,                                   00116300
116400             STR_OPSTMT_HDR_CD,                                   00116400
116500             DPS_NBR,                                             00116500
116600             MEDICARE_ID,                                         00116600
116700             NABP_NBR,                                            00116700
116800             NATL_PROV_ID,                                        00116800
116900             CURR_AD_ZN_NBR,                                      00116900
117000             PD_ZONE_NO,                                          00117000
117100             SOS_PROC_SW,                                         00117100
117200             RPRT_SEQ_NBR,                                        00117200
117300             GRP_CD,                                              00117300
117400             PRIM_GRP_CD_1,                                       00117400
117500             PRIM_GRP_CD_2,                                       00117500
117600             SECY_GRP_CD_1,                                       00117600
117700             SECY_GRP_CD_2,                                       00117700
117800             PRIM_CLS_NBR_1,                                      00117800
117900             PRIM_CLS_NBR_2,                                      00117900
118000             SECY_CLS_NBR_1,                                      00118000
118100             SECY_CLS_NBR_2,                                      00118100
118200             VAL_STR_SW,                                          00118200
118300             SLS_CLOSED_DT,                                       00118300
118400             TBCO_PRMT_NBR,                                       00118400
118500             SUB_UNLIKE_PROD_CD,                                  00118500
118600             SUB_DSPLY_PAL_CD,                                    00118600
118700             RLTM_SCN_MAINT_SW,                                   00118700
118800             TOP_LEADER_NM,                                       00118800
118900             CUST_FRNDLY_NM,                                      00118900
119000             SLS_OPEN_DT,                                         00119000
119100             MON_OPEN_TM,                                         00119100
119200             MON_CLOS_TM,                                         00119200
119300             TUE_OPEN_TM,                                         00119300
119400             TUE_CLOS_TM,                                         00119400
119500             WED_OPEN_TM,                                         00119500
119600             WED_CLOS_TM,                                         00119600
119700             THUR_OPEN_TM,                                        00119700
119800             THUR_CLOS_TM,                                        00119800
119900             FRI_OPEN_TM,                                         00119900
120000             FRI_CLOS_TM,                                         00120000
120100             SAT_OPEN_TM,                                         00120100
120200             SUN_OPEN_TM,                                         00120200
120300             SAT_CLOS_TM,                                         00120300
120400             SUN_CLOS_TM,                                         00120400
120500             RETL_LOC_FRMAT_CD,                                   00120500
120600             RETL_LOC_SEGM_CD,                                    00120600
120700             ECOMM_MKT_AREA_CD,                                   00120700
120800             ECOMM_STRT_DT,                                       00120800
120900             ECOMM_END_DT,                                        00120900
121000             ROLUP_REPT_TBL_01_NBR,                               00121000
121100             ROLUP_REPT_TBL_02_NBR,                               00121100
121200             ROLUP_REPT_TBL_03_NBR,                               00121200
121300             ROLUP_REPT_TBL_04_NBR,                               00121300
121400             ROLUP_REPT_TBL_05_NBR,                               00121400
121500             ROLUP_REPT_TBL_06_NBR,                               00121500
121600             ROLUP_REPT_TBL_07_NBR,                               00121600
121700             ROLUP_REPT_TBL_08_NBR,                               00121700
121800             ROLUP_REPT_TBL_09_NBR,                               00121800
121900             ROLUP_REPT_TBL_10_NBR,                               00121900
122000             ONLIN_SSON_SW,                                       00122000
122100             RPLACD_BY_STR_NBR                                    00122100
122200         FROM XXXAIL_LOC                                          00122200
122300         WHERE   (LIN_OF_BUS_ID = :DCLXXXAIL-LOC.LIN-OF-BUS-ID)   00122300
122400         AND     (LOC_NBR >= :DCLXXXAIL-LOC.LOC-NBR)              00122400
122500         AND NOT                                                  00122500
122600                 (LOC_NBR  = :DCLXXXAIL-LOC.LOC-NBR AND           00122600
122700                  LOC_TYP_CD  < :DCLXXXAIL-LOC.LOC-TYP-CD)        00122700
122800         ORDER BY                                                 00122800
122900             LOC_NBR,                                             00122900
123000             LOC_TYP_CD                                           00123000
123100     END-EXEC.                                                    00123100
123200                                                                  00123200
123300                                                                  00123300
123400 LINKAGE SECTION.                                                 00123400
123500 COPY XXXN001A.                                                   00123500
123600     EXEC SQL                                                     00123600
123700         INCLUDE SQLCA                                            00123700
123800     END-EXEC.                                                    00123800
123900 COPY YYYN005A.                                                   00123900
124000 COPY NNNN0000.                                                   00124000
124100 COPY PPPTLR01.                                                   00124100
124200                                                                  00124200
124300 PROCEDURE DIVISION USING                                         00124300
124400     XXXN001A                                                     00124400
124500     SQLCA                                                        00124500
124600     YYYN005A                                                     00124600
124700     NNNN0000-PARMS                                               00124700
124800     P-DDDTLR01                                                   00124800
124900     .                                                            00124900
125000                                                                  00125000
125100************************************************************      00125100
125200* MAIN PROGRAM LINE.                                              00125200
125300************************************************************      00125300
125400 0000-EXIT-DISPATCHER.                                            00125400
125500     PERFORM 100-INITIALIZATION                                   00125500
125600     EVALUATE TRUE                                                00125600
125700       WHEN NOT SUCCESS                                           00125700
125800          CONTINUE                                                00125800
125900       WHEN EXIT-OPEN-CURSOR                                      00125900
126000          PERFORM 1000-EXIT-OPEN-CURSOR                           00126000
126100       WHEN EXIT-CLOSE-CURSOR                                     00126100
126200          PERFORM 1100-EXIT-CLOSE-CURSOR                          00126200
126300       WHEN EXIT-GET-UNIQUE-ROW                                   00126300
126400          PERFORM 1200-EXIT-GET-UNIQUE-ROW                        00126400
126500       WHEN EXIT-GET-NEXT-ROW                                     00126500
126600          PERFORM 1300-EXIT-GET-NEXT-ROW                          00126600
126700       WHEN EXIT-PUT-MODIFY-ROW                                   00126700
126800          PERFORM 1400-EXIT-PUT-MODIFY-ROW                        00126800
126900       WHEN EXIT-PUT-INSERT-ROW                                   00126900
127000          PERFORM 1500-EXIT-PUT-INSERT-ROW                        00127000
127100       WHEN EXIT-PUT-PURGE-ROW                                    00127100
127200          PERFORM 1600-EXIT-PUT-PURGE-ROW                         00127200
127300       WHEN EXIT-DO-SPECIAL-IO-FUNCS                              00127300
127400          PERFORM 10000-DO-SPECIAL-IO-FUNCS                       00127400
127500     END-EVALUATE                                                 00127500
127600     PERFORM 120-EXIT-STUFF                                       00127600
127700     GOBACK                                                       00127700
127800     .                                                            00127800
127900                                                                  00127900
128000                                                                  00128000
128100* ================================================================00128100
128200* Initialize data areas needed to call the i/o subroutine         00128200
128300* ================================================================00128300
128400 100-INITIALIZATION.                                              00128400
128500     INITIALIZE XXXN001A                                          00128500
128600                DAO-STATUS                                        00128600
128700                WS-LOC-STAT-SW                                    00128700
128800                WS-CURR-VALUES                                    00128800
128900     MOVE NNNN0000-INDEX-HANDLE TO DDDTLR01-INDEX-HANDLE          00128900
129000     MOVE 0 TO WS-CHECKPOINT-INC                                  00129000
129100     MOVE 0 TO SQLCODE                                            00129100
129200     MOVE 0 TO SQL-INIT-FLAG                                      00129200
129300     IF NOT EXIT-CLOSE-CURSOR                                     00129300
129400       PERFORM 110-MOVE-PDA-FIELDS-2-DCL                          00129400
129500     END-IF                                                       00129500
129600     IF (YYYN005A-ORACLE       OR EXIT-PUT-INSERT-ROW             00129600
129700         OR EXIT-PUT-PURGE-ROW OR EXIT-PUT-MODIFY-ROW)            00129700
129800       PERFORM 115-CONNECT-TO-ORACLE                              00129800
129900     END-IF                                                       00129900
130000     .                                                            00130000
130100                                                                  00130100
130200                                                                  00130200
130300* ================================================================00130300
130400* Move the elementary fields in the parameter data area to the DCL00130400
130500* ================================================================00130500
130600 110-MOVE-PDA-FIELDS-2-DCL.                                       00130600
130700     MOVE LOC-NBR OF P-DDDTLR01 TO LOC-NBR OF DCLXXXAIL-LOC       00130700
130800     MOVE LOC-TYP-CD OF P-DDDTLR01 TO LOC-TYP-CD OF DCLXXXAIL-LOC 00130800
130900                                                                  00130900
131000     IF ASSOC-STR-TYP-CD OF P-DDDTLR01 = SPACES                   00131000
131100        MOVE 'S' TO ASSOC-STR-TYP-CD OF P-DDDTLR01                00131100
131200     END-IF                                                       00131200
131300     MOVE ASSOC-STR-TYP-CD OF P-DDDTLR01                          00131300
131400       TO ASSOC-STR-TYP-CD OF DCLXXXAIL-LOC                       00131400
131500     MOVE 0 TO  WS-ASSOC-ST-TYPE-IND                              00131500
131600     MOVE ASSOC-STR-NBR OF P-DDDTLR01                             00131600
131700       TO ASSOC-STR-NBR OF DCLXXXAIL-LOC                          00131700
131800     IF ASSOC-STR-NBR OF P-DDDTLR01  =  ZERO                      00131800
131900        MOVE -1 TO WS-ASSOC-ST-NO-IND                             00131900
132000     ELSE                                                         00132000
132100        MOVE 0 TO  WS-ASSOC-ST-NO-IND                             00132100
132200     END-IF                                                       00132200
132300                                                                  00132300
132400     IF SLS-CLOSED-DT OF P-DDDTLR01 = SPACES                      00132400
132500     OR SLS-CLOSED-DT OF P-DDDTLR01 = K-ZERO-DT                   00132500
132600       MOVE K-DEF-DT TO SLS-CLOSED-DT OF P-DDDTLR01               00132600
132700     END-IF                                                       00132700
132800     MOVE SLS-CLOSED-DT OF P-DDDTLR01                             00132800
132900       TO SLS-CLOSED-DT OF DCLXXXAIL-LOC                          00132900
133000     IF STR-REMODL-DT OF P-DDDTLR01 = SPACES                      00133000
133100     OR STR-REMODL-DT OF P-DDDTLR01 = K-ZERO-DT                   00133100
133200       MOVE K-DEF-DT TO STR-REMODL-DT OF P-DDDTLR01               00133200
133300     END-IF                                                       00133300
133400     MOVE STR-REMODL-DT OF P-DDDTLR01                             00133400
133500       TO STR-REMODL-DT OF DCLXXXAIL-LOC                          00133500
133600                                                                  00133600
133700     MOVE RETL-LOC-STAT-CD OF P-DDDTLR01                          00133700
133800       TO RETL-LOC-STAT-CD OF DCLXXXAIL-LOC                       00133800
133900                                                                  00133900
134000     IF RETL-LOC-STAT-DT OF P-DDDTLR01 = SPACES                   00134000
134100     OR RETL-LOC-STAT-DT OF P-DDDTLR01 = K-ZERO-DT                00134100
134200       MOVE K-DEF-DT TO RETL-LOC-STAT-DT OF P-DDDTLR01            00134200
134300     END-IF                                                       00134300
134400     MOVE RETL-LOC-STAT-DT OF P-DDDTLR01                          00134400
134500       TO RETL-LOC-STAT-DT OF DCLXXXAIL-LOC                       00134500
134600                                                                  00134600
134700     MOVE COMPANY-ID OF P-DDDTLR01 TO COMPANY-ID OF DCLXXXAIL-LOC 00134700
134800     MOVE FINANCIAL-DIV-ID OF P-DDDTLR01                          00134800
134900       TO FINANCIAL-DIV-ID OF DCLXXXAIL-LOC                       00134900
135000     MOVE LIN-OF-BUS-ID OF P-DDDTLR01                             00135000
135100       TO LIN-OF-BUS-ID OF DCLXXXAIL-LOC                          00135100
135200     MOVE DIST-ID OF P-DDDTLR01 TO DIST-ID OF DCLXXXAIL-LOC       00135200
135300                                   MKT-RGN-ID OF P-DDDTLR01       00135300
135400     MOVE MKT-RGN-ID OF P-DDDTLR01 TO MKT-RGN-ID OF DCLXXXAIL-LOC 00135400
135500     MOVE GEO-ZN-CD OF P-DDDTLR01 TO GEO-ZN-CD OF DCLXXXAIL-LOC   00135500
135600     MOVE RETL-GEO-ZN-ID OF P-DDDTLR01                            00135600
135700       TO RETL-GEO-ZN-ID OF DCLXXXAIL-LOC                         00135700
135800     MOVE SCN-MAINT-SW OF P-DDDTLR01                              00135800
135900       TO SCN-MAINT-SW OF DCLXXXAIL-LOC                           00135900
136000     MOVE FRNT-END-CD OF P-DDDTLR01                               00136000
136100       TO FRNT-END-CD OF DCLXXXAIL-LOC                            00136100
136200     MOVE PRC-BUL-SW OF P-DDDTLR01 TO PRC-BUL-SW OF DCLXXXAIL-LOC 00136200
136300     MOVE UPC-ON-PRC-BUL-SW OF P-DDDTLR01                         00136300
136400       TO UPC-ON-PRC-BUL-SW OF DCLXXXAIL-LOC                      00136400
136500     MOVE CMPTR-TYP-CD OF P-DDDTLR01                              00136500
136600       TO CMPTR-TYP-CD OF DCLXXXAIL-LOC                           00136600
136700     MOVE RETL-VID-ZN-NBR OF P-DDDTLR01                           00136700
136800       TO RETL-VID-ZN-NBR OF DCLXXXAIL-LOC                        00136800
136900     MOVE RETL-UNLD-CD OF P-DDDTLR01                              00136900
137000       TO RETL-UNLD-CD OF DCLXXXAIL-LOC                           00137000
137100*    MOVE ROLUP-REPT-TBL-TXT OF P-DDDTLR01                        00137100
137200     MOVE SPACES                                                  00137200
137300       TO ROLUP-REPT-TBL-TXT OF DCLXXXAIL-LOC                     00137300
137400     PERFORM 117-MOVE-ROLLUP-DATA                                 00137400
137500     MOVE NEW-STR-SW OF P-DDDTLR01 TO NEW-STR-SW OF DCLXXXAIL-LOC 00137500
137600     MOVE SEL-CIR-SW OF P-DDDTLR01 TO SEL-CIR-SW OF DCLXXXAIL-LOC 00137600
137700     MOVE BKRM-SQ-FT OF P-DDDTLR01 TO BKRM-SQ-FT OF DCLXXXAIL-LOC 00137700
137800     MOVE FD-LINER-FT OF P-DDDTLR01                               00137800
137900       TO FD-LINER-FT OF DCLXXXAIL-LOC                            00137900
138000     MOVE NON-FD-LINER-FT OF P-DDDTLR01                           00138000
138100       TO NON-FD-LINER-FT OF DCLXXXAIL-LOC                        00138100
138200     MOVE SETOFF-ROOM-SW OF P-DDDTLR01                            00138200
138300       TO SETOFF-ROOM-SW OF DCLXXXAIL-LOC                         00138300
138400     MOVE CAT-CLS-TBL-TXT OF P-DDDTLR01                           00138400
138500       TO CAT-CLS-TBL-TXT OF DCLXXXAIL-LOC                        00138500
138600                                                                  00138600
138700     IF LAT-K OF P-DDDTLR01 IS NOT NUMERIC                        00138700
138800       MOVE 0 TO LAT-K OF P-DDDTLR01                              00138800
138900     END-IF                                                       00138900
139000     MOVE LAT-K OF P-DDDTLR01 TO LAT-K OF DCLXXXAIL-LOC           00139000
139100                                                                  00139100
139200     IF LON-K OF P-DDDTLR01 IS NOT NUMERIC                        00139200
139300       MOVE 0 TO LON-K OF P-DDDTLR01                              00139300
139400     END-IF                                                       00139400
139500     MOVE LON-K OF P-DDDTLR01 TO LON-K OF DCLXXXAIL-LOC           00139500
139600                                                                  00139600
139700     MOVE CK-COLL-REPT-SW OF P-DDDTLR01                           00139700
139800       TO CK-COLL-REPT-SW OF DCLXXXAIL-LOC                        00139800
139900     MOVE CK-COLL-CNTL-CD OF P-DDDTLR01                           00139900
140000       TO CK-COLL-CNTL-CD OF DCLXXXAIL-LOC                        00140000
140100     MOVE CK-COLL-ADD-DEL-SW OF P-DDDTLR01                        00140100
140200       TO CK-COLL-ADD-DEL-SW OF DCLXXXAIL-LOC                     00140200
140300     MOVE CK-ALT-STR-ID OF P-DDDTLR01                             00140300
140400       TO CK-ALT-STR-ID OF DCLXXXAIL-LOC                          00140400
140500     MOVE CK-COLL-FEE-AMT OF P-DDDTLR01                           00140500
140600       TO CK-COLL-FEE-AMT OF DCLXXXAIL-LOC                        00140600
140700     MOVE SALS-TAX-PCT OF P-DDDTLR01                              00140700
140800       TO SALS-TAX-PCT OF DCLXXXAIL-LOC                           00140800
140900     MOVE SOAP-SALE-VAR-PCT OF P-DDDTLR01                         00140900
141000       TO SOAP-SALE-VAR-PCT OF DCLXXXAIL-LOC                      00141000
141100     MOVE ON-SRS-CD OF P-DDDTLR01 TO ON-SRS-CD OF DCLXXXAIL-LOC   00141100
141200     MOVE SRS-DSD-ORD-SW OF P-DDDTLR01                            00141200
141300       TO SRS-DSD-ORD-SW OF DCLXXXAIL-LOC                         00141300
141400     MOVE RETL-LOC-TYP-CD OF P-DDDTLR01                           00141400
141500       TO RETL-LOC-TYP-CD OF DCLXXXAIL-LOC                        00141500
141600     MOVE DEA-NBR OF P-DDDTLR01 TO DEA-NBR OF DCLXXXAIL-LOC       00141600
141700     IF YYYN005A-CICS-ENV                                         00141700
141800       MOVE STR-OPSTMT-SRT-CD OF P-DDDTLR01                       00141800
141900         TO RPRT-SEQ-NBR      OF P-DDDTLR01                       00141900
142000     END-IF                                                       00142000
142100     MOVE RPRT-SEQ-NBR OF P-DDDTLR01                              00142100
142200       TO RPRT-SEQ-NBR OF DCLXXXAIL-LOC                           00142200
142300     MOVE STR-OPSTMT-SRT-CD OF P-DDDTLR01                         00142300
142400       TO STR-OPSTMT-SRT-CD OF DCLXXXAIL-LOC                      00142400
142500     MOVE STR-OPSTMT-TYP-CD OF P-DDDTLR01                         00142500
142600       TO STR-OPSTMT-TYP-CD OF DCLXXXAIL-LOC                      00142600
142700     MOVE STR-OPSTMT-HDR-CD OF P-DDDTLR01                         00142700
142800       TO STR-OPSTMT-HDR-CD OF DCLXXXAIL-LOC                      00142800
142900     MOVE DPS-NBR OF P-DDDTLR01 TO DPS-NBR OF DCLXXXAIL-LOC       00142900
143000     MOVE MEDICARE-ID OF P-DDDTLR01                               00143000
143100       TO MEDICARE-ID OF DCLXXXAIL-LOC                            00143100
143200     MOVE NABP-NBR OF P-DDDTLR01 TO NABP-NBR OF DCLXXXAIL-LOC     00143200
143300     MOVE NATL-PROV-ID OF P-DDDTLR01                              00143300
143400       TO NATL-PROV-ID OF DCLXXXAIL-LOC                           00143400
143500     MOVE CURR-AD-ZN-NBR OF P-DDDTLR01                            00143500
143600       TO CURR-AD-ZN-NBR OF DCLXXXAIL-LOC                         00143600
143700     MOVE PD-ZONE-NO OF P-DDDTLR01 TO PD-ZONE-NO OF DCLXXXAIL-LOC 00143700
143800     MOVE SOS-PROC-SW OF P-DDDTLR01                               00143800
143900       TO SOS-PROC-SW OF DCLXXXAIL-LOC                            00143900
144000     MOVE GRP-CD OF P-DDDTLR01 TO GRP-CD OF DCLXXXAIL-LOC         00144000
144100     MOVE PRIM-GRP-CD-1 OF P-DDDTLR01                             00144100
144200       TO PRIM-GRP-CD-1 OF DCLXXXAIL-LOC                          00144200
144300     MOVE PRIM-GRP-CD-2 OF P-DDDTLR01                             00144300
144400       TO PRIM-GRP-CD-2 OF DCLXXXAIL-LOC                          00144400
144500     MOVE SECY-GRP-CD-1 OF P-DDDTLR01                             00144500
144600       TO SECY-GRP-CD-1 OF DCLXXXAIL-LOC                          00144600
144700     MOVE SECY-GRP-CD-2 OF P-DDDTLR01                             00144700
144800       TO SECY-GRP-CD-2 OF DCLXXXAIL-LOC                          00144800
144900     MOVE PRIM-CLS-NBR-1 OF P-DDDTLR01                            00144900
145000       TO PRIM-CLS-NBR-1 OF DCLXXXAIL-LOC                         00145000
145100     MOVE PRIM-CLS-NBR-2 OF P-DDDTLR01                            00145100
145200       TO PRIM-CLS-NBR-2 OF DCLXXXAIL-LOC                         00145200
145300     MOVE SECY-CLS-NBR-1 OF P-DDDTLR01                            00145300
145400       TO SECY-CLS-NBR-1 OF DCLXXXAIL-LOC                         00145400
145500     MOVE SECY-CLS-NBR-2 OF P-DDDTLR01                            00145500
145600       TO SECY-CLS-NBR-2 OF DCLXXXAIL-LOC                         00145600
145700     MOVE VAL-STR-SW OF P-DDDTLR01 TO VAL-STR-SW OF DCLXXXAIL-LOC 00145700
145800     IF TBCO-PRMT-NBR OF P-DDDTLR01 NOT NUMERIC                   00145800
145900       MOVE ZEROES TO TBCO-PRMT-NBR OF P-DDDTLR01                 00145900
146000     END-IF                                                       00146000
146100                                                                  00146100
146200     MOVE TBCO-PRMT-NBR OF P-DDDTLR01                             00146200
146300       TO TBCO-PRMT-NBR OF DCLXXXAIL-LOC                          00146300
146400                                                                  00146400
146500     IF  NOT OK-TO-SUB-UNLIKE-PRODS   OF P-DDDTLR01               00146500
146600     AND NOT DONT-SUB-UNLIKE-PRODS    OF P-DDDTLR01               00146600
146700       SET NO-UNLIKE-SUB-STORE-PREF   OF P-DDDTLR01 TO TRUE       00146700
146800     END-IF                                                       00146800
146900     MOVE SUB-UNLIKE-PROD-CD OF P-DDDTLR01                        00146900
147000       TO SUB-UNLIKE-PROD-CD OF DCLXXXAIL-LOC                     00147000
147100                                                                  00147100
147200     IF  NOT OK-TO-SUB-DISP-PALS      OF P-DDDTLR01               00147200
147300     AND NOT DONT-SUB-DISP-PALS       OF P-DDDTLR01               00147300
147400       SET NO-DISP-PAL-SUB-STORE-PREF OF P-DDDTLR01 TO TRUE       00147400
147500     END-IF                                                       00147500
147600     MOVE SUB-DSPLY-PAL-CD   OF P-DDDTLR01                        00147600
147700       TO SUB-DSPLY-PAL-CD   OF DCLXXXAIL-LOC                     00147700
147800                                                                  00147800
147900     IF  NOT SEND-REAL-TIME-G3        OF P-DDDTLR01               00147900
148000       SET DONT-SEND-REAL-TIME-G3     OF P-DDDTLR01 TO TRUE       00148000
148100     END-IF                                                       00148100
148200     MOVE RLTM-SCN-MAINT-SW  OF P-DDDTLR01                        00148200
148300       TO RLTM-SCN-MAINT-SW  OF DCLXXXAIL-LOC                     00148300
148400     MOVE TOP-LEADER-NM  OF P-DDDTLR01                            00148400
148500       TO TOP-LEADER-NM  OF DCLXXXAIL-LOC                         00148500
148600     MOVE CUST-FRNDLY-NM OF P-DDDTLR01                            00148600
148700       TO CUST-FRNDLY-NM OF DCLXXXAIL-LOC                         00148700
148800     IF SLS-OPEN-DT       OF P-DDDTLR01 = SPACES                  00148800
148900     OR SLS-OPEN-DT       OF P-DDDTLR01 = K-ZERO-DT               00148900
149000        MOVE K-DEF-DT     TO SLS-OPEN-DT OF P-DDDTLR01            00149000
149100     END-IF                                                       00149100
149200     MOVE SLS-OPEN-DT    OF P-DDDTLR01                            00149200
149300       TO SLS-OPEN-DT    OF DCLXXXAIL-LOC                         00149300
149400     IF MON-OPEN-TM       OF P-DDDTLR01 = SPACES                  00149400
149500     OR MON-OPEN-TM       OF P-DDDTLR01 = K-DEF-TM                00149500
149600        MOVE K-DEF-TM     TO MON-OPEN-TM OF P-DDDTLR01            00149600
149700     END-IF                                                       00149700
149800     IF MON-CLOS-TM       OF P-DDDTLR01 = SPACES                  00149800
149900     OR MON-CLOS-TM       OF P-DDDTLR01 = K-DEF-TM                00149900
150000        MOVE K-DEF-TM     TO MON-CLOS-TM OF P-DDDTLR01            00150000
150100     END-IF                                                       00150100
150200     IF MON-CLOS-TM       OF P-DDDTLR01 = K-DB2-MAX-TM            00150200
150300        MOVE K-ORA-MAX-TM TO MON-CLOS-TM OF P-DDDTLR01            00150300
150400     END-IF                                                       00150400
150500     IF TUE-OPEN-TM       OF P-DDDTLR01 = SPACES                  00150500
150600     OR TUE-OPEN-TM       OF P-DDDTLR01 = K-DEF-TM                00150600
150700        MOVE K-DEF-TM     TO TUE-OPEN-TM OF P-DDDTLR01            00150700
150800     END-IF                                                       00150800
150900     IF TUE-CLOS-TM       OF P-DDDTLR01 = SPACES                  00150900
151000     OR TUE-CLOS-TM       OF P-DDDTLR01 = K-DEF-TM                00151000
151100        MOVE K-DEF-TM     TO TUE-CLOS-TM OF P-DDDTLR01            00151100
151200     END-IF                                                       00151200
151300     IF TUE-CLOS-TM       OF P-DDDTLR01 = K-DB2-MAX-TM            00151300
151400        MOVE K-ORA-MAX-TM TO TUE-CLOS-TM OF P-DDDTLR01            00151400
151500     END-IF                                                       00151500
151600     IF WED-OPEN-TM       OF P-DDDTLR01 = SPACES                  00151600
151700     OR WED-OPEN-TM       OF P-DDDTLR01 = K-DEF-TM                00151700
151800        MOVE K-DEF-TM     TO WED-OPEN-TM OF P-DDDTLR01            00151800
151900     END-IF                                                       00151900
152000     IF WED-CLOS-TM       OF P-DDDTLR01 = SPACES                  00152000
152100     OR WED-CLOS-TM       OF P-DDDTLR01 = K-DEF-TM                00152100
152200        MOVE K-DEF-TM     TO WED-CLOS-TM OF P-DDDTLR01            00152200
152300     END-IF                                                       00152300
152400     IF WED-CLOS-TM       OF P-DDDTLR01 = K-DB2-MAX-TM            00152400
152500        MOVE K-ORA-MAX-TM TO WED-CLOS-TM OF P-DDDTLR01            00152500
152600     END-IF                                                       00152600
152700     IF THUR-OPEN-TM      OF P-DDDTLR01 = SPACES                  00152700
152800     OR THUR-OPEN-TM      OF P-DDDTLR01 = K-DEF-TM                00152800
152900        MOVE K-DEF-TM     TO THUR-OPEN-TM  OF P-DDDTLR01          00152900
153000     END-IF                                                       00153000
153100     IF THUR-CLOS-TM      OF P-DDDTLR01 = SPACES                  00153100
153200     OR THUR-CLOS-TM      OF P-DDDTLR01 = K-DEF-TM                00153200
153300        MOVE K-DEF-TM     TO THUR-CLOS-TM OF P-DDDTLR01           00153300
153400     END-IF                                                       00153400
153500     IF THUR-CLOS-TM      OF P-DDDTLR01 = K-DB2-MAX-TM            00153500
153600        MOVE K-ORA-MAX-TM TO THUR-CLOS-TM OF P-DDDTLR01           00153600
153700     END-IF                                                       00153700
153800     IF FRI-OPEN-TM       OF P-DDDTLR01 = SPACES                  00153800
153900     OR FRI-OPEN-TM       OF P-DDDTLR01 = K-DEF-TM                00153900
154000        MOVE K-DEF-TM     TO FRI-OPEN-TM OF P-DDDTLR01            00154000
154100     END-IF                                                       00154100
154200     IF FRI-CLOS-TM       OF P-DDDTLR01 = SPACES                  00154200
154300     OR FRI-CLOS-TM       OF P-DDDTLR01 = K-DEF-TM                00154300
154400        MOVE K-DEF-TM     TO FRI-CLOS-TM OF P-DDDTLR01            00154400
154500     END-IF                                                       00154500
154600     IF FRI-CLOS-TM       OF P-DDDTLR01 = K-DB2-MAX-TM            00154600
154700        MOVE K-ORA-MAX-TM TO FRI-CLOS-TM OF P-DDDTLR01            00154700
154800     END-IF                                                       00154800
154900     IF SAT-OPEN-TM       OF P-DDDTLR01 = SPACES                  00154900
155000     OR SAT-OPEN-TM       OF P-DDDTLR01 = K-DEF-TM                00155000
155100        MOVE K-DEF-TM     TO SAT-OPEN-TM OF P-DDDTLR01            00155100
155200     END-IF                                                       00155200
155300     IF SAT-CLOS-TM       OF P-DDDTLR01 = SPACES                  00155300
155400     OR SAT-CLOS-TM       OF P-DDDTLR01 = K-DEF-TM                00155400
155500        MOVE K-DEF-TM     TO SAT-CLOS-TM OF P-DDDTLR01            00155500
155600     END-IF                                                       00155600
155700     IF SAT-CLOS-TM       OF P-DDDTLR01 = K-DB2-MAX-TM            00155700
155800        MOVE K-ORA-MAX-TM TO SAT-CLOS-TM OF P-DDDTLR01            00155800
155900     END-IF                                                       00155900
156000     IF SUN-OPEN-TM       OF P-DDDTLR01 = SPACES                  00156000
156100     OR SUN-OPEN-TM       OF P-DDDTLR01 = K-DEF-TM                00156100
156200        MOVE K-DEF-TM     TO SUN-OPEN-TM OF P-DDDTLR01            00156200
156300     END-IF                                                       00156300
156400     IF SUN-CLOS-TM       OF P-DDDTLR01 = SPACES                  00156400
156500     OR SUN-CLOS-TM       OF P-DDDTLR01 = K-DEF-TM                00156500
156600        MOVE K-DEF-TM     TO SUN-CLOS-TM OF P-DDDTLR01            00156600
156700     END-IF                                                       00156700
156800     IF SUN-CLOS-TM       OF P-DDDTLR01 = K-DB2-MAX-TM            00156800
156900        MOVE K-ORA-MAX-TM TO SUN-CLOS-TM OF P-DDDTLR01            00156900
157000     END-IF                                                       00157000
157100     PERFORM 112-MOVE-TIME-FIELDS                                 00157100
157200     MOVE RETL-LOC-FRMAT-CD OF P-DDDTLR01                         00157200
157300       TO RETL-LOC-FRMAT-CD OF DCLXXXAIL-LOC                      00157300
157400     MOVE RETL-LOC-SEGM-CD OF P-DDDTLR01                          00157400
157500       TO RETL-LOC-SEGM-CD OF DCLXXXAIL-LOC                       00157500
157600     MOVE ECOMM-MKT-AREA-CD OF P-DDDTLR01                         00157600
157700       TO ECOMM-MKT-AREA-CD OF DCLXXXAIL-LOC                      00157700
157800     IF ECOMM-STRT-DT OF P-DDDTLR01 = SPACES                      00157800
157900     OR ECOMM-STRT-DT OF P-DDDTLR01 = K-ZERO-DT                   00157900
158000       MOVE K-DEF-DT TO ECOMM-STRT-DT OF P-DDDTLR01               00158000
158100     END-IF                                                       00158100
158200     MOVE ECOMM-STRT-DT OF P-DDDTLR01                             00158200
158300       TO ECOMM-STRT-DT OF DCLXXXAIL-LOC                          00158300
158400     MOVE 0 TO ECOMM-STRT-DT-IND OF P-DDDTLR01                    00158400
158500     MOVE 0 TO ECOMM-STRT-DT-IND OF DCLXXXAIL-LOC-IND             00158500
158600     IF ECOMM-END-DT OF P-DDDTLR01 = SPACES                       00158600
158700     OR ECOMM-END-DT OF P-DDDTLR01 = K-ZERO-DT                    00158700
158800       MOVE K-DEF-DT TO ECOMM-END-DT OF P-DDDTLR01                00158800
158900     END-IF                                                       00158900
159000     MOVE ECOMM-END-DT OF P-DDDTLR01                              00159000
159100       TO ECOMM-END-DT OF DCLXXXAIL-LOC                           00159100
159200     MOVE 0 TO ECOMM-END-DT-IND OF P-DDDTLR01                     00159200
159300     MOVE 0 TO ECOMM-END-DT-IND OF DCLXXXAIL-LOC-IND              00159300
159400     MOVE ONLIN-SSON-SW OF P-DDDTLR01                             00159400
159500                        TO ONLIN-SSON-SW  OF DCLXXXAIL-LOC        00159500
159510     MOVE 0 TO  RPLACD-BY-STR-NBR-IND                             00159510
159600     MOVE RPLACD-BY-STR-NBR OF P-DDDTLR01                         00159600
159700                     TO RPLACD-BY-STR-NBR OF DCLXXXAIL-LOC        00159700
159710     IF RPLACD-BY-STR-NBR  OF P-DDDTLR01  =  ZERO                 00159710
159720        MOVE -1 TO RPLACD-BY-STR-NBR-IND  OF DCLXXXAIL-LOC-IND    00159720
159730     ELSE                                                         00159730
159740        MOVE 0 TO  RPLACD-BY-STR-NBR-IND OF DCLXXXAIL-LOC-IND     00159740
159750     END-IF                                                       00159750
159760     .                                                            00159760
159770                                                                  00159770
159800                                                                  00159800
159900* ================================================================00159900
160000* Convert DB2 TIME field format to Oracle format.                 00160000
160100* ================================================================00160100
160200 112-MOVE-TIME-FIELDS.                                            00160200
160300     IF (YYYN005A-ORACLE OR EXIT-PUT-INSERT-ROW                   00160300
160400         OR EXIT-PUT-MODIFY-ROW)                                  00160400
160500       INITIALIZE MMMC0291-INPUT-TM                               00160500
160600                  MMMC0291-INPUT-TS                               00160600
160700       MOVE MON-OPEN-TM OF P-DDDTLR01                             00160700
160800         TO WS-TIME-INOUT-CONV(1)                                 00160800
160900       MOVE MON-CLOS-TM OF P-DDDTLR01                             00160900
161000         TO WS-TIME-INOUT-CONV(2)                                 00161000
161100       MOVE TUE-OPEN-TM OF P-DDDTLR01                             00161100
161200         TO WS-TIME-INOUT-CONV(3)                                 00161200
161300       MOVE TUE-CLOS-TM OF P-DDDTLR01                             00161300
161400         TO WS-TIME-INOUT-CONV(4)                                 00161400
161500       MOVE WED-OPEN-TM OF P-DDDTLR01                             00161500
161600         TO WS-TIME-INOUT-CONV(5)                                 00161600
161700       MOVE WED-CLOS-TM OF P-DDDTLR01                             00161700
161800         TO WS-TIME-INOUT-CONV(6)                                 00161800
161900       MOVE THUR-OPEN-TM OF P-DDDTLR01                            00161900
162000         TO WS-TIME-INOUT-CONV(7)                                 00162000
162100       MOVE THUR-CLOS-TM OF P-DDDTLR01                            00162100
162200         TO WS-TIME-INOUT-CONV(8)                                 00162200
162300       MOVE FRI-OPEN-TM OF P-DDDTLR01                             00162300
162400         TO WS-TIME-INOUT-CONV(9)                                 00162400
162500       MOVE FRI-CLOS-TM OF P-DDDTLR01                             00162500
162600         TO WS-TIME-INOUT-CONV(10)                                00162600
162700       MOVE SAT-OPEN-TM OF P-DDDTLR01                             00162700
162800         TO WS-TIME-INOUT-CONV(11)                                00162800
162900       MOVE SAT-CLOS-TM OF P-DDDTLR01                             00162900
163000         TO WS-TIME-INOUT-CONV(12)                                00163000
163100       MOVE SUN-OPEN-TM OF P-DDDTLR01                             00163100
163200         TO WS-TIME-INOUT-CONV(13)                                00163200
163300       MOVE SUN-CLOS-TM OF P-DDDTLR01                             00163300
163400         TO WS-TIME-INOUT-CONV(14)                                00163400
163500                                                                  00163500
163600       SET  MMMC0291-CVT-TM-TO-TS  TO TRUE                        00163600
163700       CALL WS-MMMS0291-PGM USING                                 00163700
163800                          XXXN001A                                00163800
163900                          MMMC0291                                00163900
164000                                                                  00164000
164100       IF NOT SUCCESS                                             00164100
164200         STRING 'NNNS0488 - INVALID TIME.PLS VERIFY Sqlcode ='    00164200
164300             WS-SQLCODE                                           00164300
164400             DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT               00164400
164500       ELSE                                                       00164500
164600         MOVE WS-TIMSTAMP-INOUT-CONV(1)                           00164600
164700           TO WS-MON-OPEN-TS                                      00164700
164800         MOVE WS-TIMSTAMP-INOUT-CONV(2)                           00164800
164900           TO WS-MON-CLOS-TS                                      00164900
165000         MOVE WS-TIMSTAMP-INOUT-CONV(3)                           00165000
165100           TO WS-TUE-OPEN-TS                                      00165100
165200         MOVE WS-TIMSTAMP-INOUT-CONV(4)                           00165200
165300           TO WS-TUE-CLOS-TS                                      00165300
165400         MOVE WS-TIMSTAMP-INOUT-CONV(5)                           00165400
165500           TO WS-WED-OPEN-TS                                      00165500
165600         MOVE WS-TIMSTAMP-INOUT-CONV(6)                           00165600
165700           TO WS-WED-CLOS-TS                                      00165700
165800         MOVE WS-TIMSTAMP-INOUT-CONV(7)                           00165800
165900           TO WS-THUR-OPEN-TS                                     00165900
166000         MOVE WS-TIMSTAMP-INOUT-CONV(8)                           00166000
166100           TO WS-THUR-CLOS-TS                                     00166100
166200         MOVE WS-TIMSTAMP-INOUT-CONV(9)                           00166200
166300           TO WS-FRI-OPEN-TS                                      00166300
166400         MOVE WS-TIMSTAMP-INOUT-CONV(10)                          00166400
166500           TO WS-FRI-CLOS-TS                                      00166500
166600         MOVE WS-TIMSTAMP-INOUT-CONV(11)                          00166600
166700           TO WS-SAT-OPEN-TS                                      00166700
166800         MOVE WS-TIMSTAMP-INOUT-CONV(12)                          00166800
166900           TO WS-SAT-CLOS-TS                                      00166900
167000         MOVE WS-TIMSTAMP-INOUT-CONV(13)                          00167000
167100           TO WS-SUN-OPEN-TS                                      00167100
167200         MOVE WS-TIMSTAMP-INOUT-CONV(14)                          00167200
167300           TO WS-SUN-CLOS-TS                                      00167300
167400       END-IF                                                     00167400
167500     ELSE                                                         00167500
167600       MOVE MON-OPEN-TM OF P-DDDTLR01                             00167600
167700         TO MON-OPEN-TM OF DCLXXXAIL-LOC                          00167700
167800       MOVE MON-CLOS-TM OF P-DDDTLR01                             00167800
167900         TO MON-CLOS-TM OF DCLXXXAIL-LOC                          00167900
168000       MOVE TUE-OPEN-TM OF P-DDDTLR01                             00168000
168100         TO TUE-OPEN-TM OF DCLXXXAIL-LOC                          00168100
168200       MOVE TUE-CLOS-TM OF P-DDDTLR01                             00168200
168300         TO TUE-CLOS-TM OF DCLXXXAIL-LOC                          00168300
168400       MOVE WED-OPEN-TM OF P-DDDTLR01                             00168400
168500         TO WED-OPEN-TM OF DCLXXXAIL-LOC                          00168500
168600       MOVE WED-CLOS-TM OF P-DDDTLR01                             00168600
168700         TO WED-CLOS-TM OF DCLXXXAIL-LOC                          00168700
168800       MOVE THUR-OPEN-TM OF P-DDDTLR01                            00168800
168900         TO THUR-OPEN-TM OF DCLXXXAIL-LOC                         00168900
169000       MOVE THUR-CLOS-TM OF P-DDDTLR01                            00169000
169100         TO THUR-CLOS-TM OF DCLXXXAIL-LOC                         00169100
169200       MOVE FRI-OPEN-TM OF P-DDDTLR01                             00169200
169300         TO FRI-OPEN-TM OF DCLXXXAIL-LOC                          00169300
169400       MOVE FRI-CLOS-TM OF P-DDDTLR01                             00169400
169500         TO FRI-CLOS-TM OF DCLXXXAIL-LOC                          00169500
169600       MOVE SAT-OPEN-TM OF P-DDDTLR01                             00169600
169700         TO SAT-OPEN-TM OF DCLXXXAIL-LOC                          00169700
169800       MOVE SAT-CLOS-TM OF P-DDDTLR01                             00169800
169900         TO SAT-CLOS-TM OF DCLXXXAIL-LOC                          00169900
170000       MOVE SUN-OPEN-TM OF P-DDDTLR01                             00170000
170100         TO SUN-OPEN-TM OF DCLXXXAIL-LOC                          00170100
170200       MOVE SUN-CLOS-TM OF P-DDDTLR01                             00170200
170300         TO SUN-CLOS-TM OF DCLXXXAIL-LOC                          00170300
170400     END-IF                                                       00170400
170500     .                                                            00170500
170600                                                                  00170600
170700                                                                  00170700
170800* ================================================================00170800
170900* Connect to Oracle database.                                     00170900
171000* ================================================================00171000
171100 115-CONNECT-TO-ORACLE.                                           00171100
171200     CALL Z-ORA-CONNECT USING XXXN001A                            00171200
171300                              SQLCA                               00171300
171400     IF NOT SUCCESS                                               00171400
171500       MOVE SQLCODE TO WS-SQLCODE                                 00171500
171600       MOVE SPACES  TO IS-RTRN-MSG-TXT                            00171600
171700       STRING 'NNNS0488 - Error connecting to Oracle. Sqlcode ='  00171700
171800               WS-SQLCODE                                         00171800
171900               DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT             00171900
172000     END-IF                                                       00172000
172100     .                                                            00172100
172200                                                                  00172200
172300                                                                  00172300
172400 117-MOVE-ROLLUP-DATA.                                            00172400
172500     MOVE ROLUP-REPT-TBL-TXT OF P-DDDTLR01                        00172500
172600       TO WS-REPT-TBL-TXT                                         00172600
172700     IF WS-REPT-TBL-NUMERIC(1) IS NUMERIC                         00172700
172800        MOVE WS-REPT-TBL-NUMERIC(1)                               00172800
172900          TO ROLUP-REPT-TBL-01-NBR  OF DCLXXXAIL-LOC              00172900
173000     ELSE                                                         00173000
173100        MOVE ZERO TO ROLUP-REPT-TBL-01-NBR OF DCLXXXAIL-LOC       00173100
173200     END-IF                                                       00173200
173300     IF WS-REPT-TBL-NUMERIC(2) IS NUMERIC                         00173300
173400        MOVE WS-REPT-TBL-NUMERIC(2)                               00173400
173500          TO ROLUP-REPT-TBL-02-NBR  OF DCLXXXAIL-LOC              00173500
173600     ELSE                                                         00173600
173700        MOVE ZERO TO ROLUP-REPT-TBL-02-NBR OF DCLXXXAIL-LOC       00173700
173800     END-IF                                                       00173800
173900     IF WS-REPT-TBL-NUMERIC(3) IS NUMERIC                         00173900
174000        MOVE WS-REPT-TBL-NUMERIC(3)                               00174000
174100          TO ROLUP-REPT-TBL-03-NBR  OF DCLXXXAIL-LOC              00174100
174200     ELSE                                                         00174200
174300        MOVE ZERO TO ROLUP-REPT-TBL-03-NBR OF DCLXXXAIL-LOC       00174300
174400     END-IF                                                       00174400
174500     IF WS-REPT-TBL-NUMERIC(4) IS NUMERIC                         00174500
174600        MOVE WS-REPT-TBL-NUMERIC(4)                               00174600
174700          TO ROLUP-REPT-TBL-04-NBR  OF DCLXXXAIL-LOC              00174700
174800     ELSE                                                         00174800
174900        MOVE ZERO TO ROLUP-REPT-TBL-04-NBR OF DCLXXXAIL-LOC       00174900
175000     END-IF                                                       00175000
175100     IF WS-REPT-TBL-NUMERIC(5) IS NUMERIC                         00175100
175200        MOVE WS-REPT-TBL-NUMERIC(5)                               00175200
175300          TO ROLUP-REPT-TBL-05-NBR  OF DCLXXXAIL-LOC              00175300
175400     ELSE                                                         00175400
175500        MOVE ZERO TO ROLUP-REPT-TBL-05-NBR OF DCLXXXAIL-LOC       00175500
175600     END-IF                                                       00175600
175700     IF WS-REPT-TBL-NUMERIC(6) IS NUMERIC                         00175700
175800        MOVE WS-REPT-TBL-NUMERIC(6)                               00175800
175900          TO ROLUP-REPT-TBL-06-NBR  OF DCLXXXAIL-LOC              00175900
176000     ELSE                                                         00176000
176100        MOVE ZERO TO ROLUP-REPT-TBL-06-NBR OF DCLXXXAIL-LOC       00176100
176200     END-IF                                                       00176200
176300     IF WS-REPT-TBL-NUMERIC(7) IS NUMERIC                         00176300
176400        MOVE WS-REPT-TBL-NUMERIC(7)                               00176400
176500          TO ROLUP-REPT-TBL-07-NBR  OF DCLXXXAIL-LOC              00176500
176600     ELSE                                                         00176600
176700        MOVE ZERO TO ROLUP-REPT-TBL-07-NBR OF DCLXXXAIL-LOC       00176700
176800     END-IF                                                       00176800
176900     IF WS-REPT-TBL-NUMERIC(8) IS NUMERIC                         00176900
177000        MOVE WS-REPT-TBL-NUMERIC(8)                               00177000
177100          TO ROLUP-REPT-TBL-08-NBR  OF DCLXXXAIL-LOC              00177100
177200     ELSE                                                         00177200
177300        MOVE ZERO TO ROLUP-REPT-TBL-08-NBR OF DCLXXXAIL-LOC       00177300
177400     END-IF                                                       00177400
177500     IF WS-REPT-TBL-NUMERIC(9) IS NUMERIC                         00177500
177600        MOVE WS-REPT-TBL-NUMERIC(9)                               00177600
177700          TO ROLUP-REPT-TBL-09-NBR  OF DCLXXXAIL-LOC              00177700
177800     ELSE                                                         00177800
177900        MOVE ZERO TO ROLUP-REPT-TBL-09-NBR OF DCLXXXAIL-LOC       00177900
178000     END-IF                                                       00178000
178100     IF WS-REPT-TBL-NUMERIC(10) IS NUMERIC                        00178100
178200        MOVE WS-REPT-TBL-NUMERIC(10)                              00178200
178300          TO ROLUP-REPT-TBL-10-NBR  OF DCLXXXAIL-LOC              00178300
178400     ELSE                                                         00178400
178500        MOVE ZERO TO ROLUP-REPT-TBL-10-NBR OF DCLXXXAIL-LOC       00178500
178600     END-IF                                                       00178600
178700     .                                                            00178700
178800                                                                  00178800
178900                                                                  00178900
179000* ================================================================00179000
179100* Stuff to do on exit.                                            00179100
179200* ================================================================00179200
179300 120-EXIT-STUFF.                                                  00179300
179400     IF SUCCESS                                                   00179400
179500       IF NOT EXIT-CLOSE-CURSOR                                   00179500
179600         PERFORM 130-MOVE-DCL-2-PDA-FIELDS                        00179600
179700       END-IF                                                     00179700
179800       ADD WS-CHECKPOINT-INC TO YYYN005A-CHKPT-CNT                00179800
179900     END-IF                                                       00179900
180000     IF (YYYN005A-ORACLE       OR EXIT-PUT-INSERT-ROW             00180000
180100         OR EXIT-PUT-PURGE-ROW OR EXIT-PUT-MODIFY-ROW)            00180100
180200*      SET YYYN005A-DB2        TO TRUE                            00180200
180300       PERFORM 125-CONNECT-TO-DB2                                 00180300
180400     END-IF                                                       00180400
180500     MOVE SQLCODE            TO DB2-SQL-CODE                      00180500
180600     .                                                            00180600
180700                                                                  00180700
180800                                                                  00180800
180900* ================================================================00180900
181000* Connect to DB2 database.                                        00181000
181100* ================================================================00181100
181200 125-CONNECT-TO-DB2.                                              00181200
181300     CALL Z-DB2-CONNECT         USING XXXN001A                    00181300
181400                                      SQLCA                       00181400
181500     .                                                            00181500
181600                                                                  00181600
181700                                                                  00181700
181800* ================================================================00181800
181900* Move the elementary fields of the DCL to parameter data area.   00181900
182000* ================================================================00182000
182100 130-MOVE-DCL-2-PDA-FIELDS.                                       00182100
182200     MOVE LOC-NBR OF DCLXXXAIL-LOC TO LOC-NBR OF P-DDDTLR01       00182200
182300     MOVE LOC-TYP-CD OF DCLXXXAIL-LOC TO LOC-TYP-CD OF P-DDDTLR01 00182300
182400     MOVE ASSOC-STR-TYP-CD OF DCLXXXAIL-LOC                       00182400
182500       TO ASSOC-STR-TYP-CD OF P-DDDTLR01                          00182500
182600     MOVE ASSOC-STR-NBR OF DCLXXXAIL-LOC                          00182600
182700       TO ASSOC-STR-NBR OF P-DDDTLR01                             00182700
182800                                                                  00182800
182900     MOVE STR-REMODL-DT OF DCLXXXAIL-LOC                          00182900
183000       TO STR-REMODL-DT OF P-DDDTLR01                             00183000
183100     IF STR-REMODL-DT OF P-DDDTLR01 = K-DEF-DT                    00183100
183200       MOVE SPACES TO STR-REMODL-DT OF P-DDDTLR01                 00183200
183300     END-IF                                                       00183300
183400                                                                  00183400
183500     MOVE SLS-CLOSED-DT OF DCLXXXAIL-LOC                          00183500
183600       TO SLS-CLOSED-DT OF P-DDDTLR01                             00183600
183700     IF SLS-CLOSED-DT OF P-DDDTLR01 = K-DEF-DT                    00183700
183800       MOVE SPACES TO SLS-CLOSED-DT OF P-DDDTLR01                 00183800
183900     END-IF                                                       00183900
184000                                                                  00184000
184100     MOVE RETL-LOC-STAT-CD OF DCLXXXAIL-LOC                       00184100
184200       TO RETL-LOC-STAT-CD OF P-DDDTLR01                          00184200
184300                                                                  00184300
184400     MOVE RETL-LOC-STAT-DT OF DCLXXXAIL-LOC                       00184400
184500       TO RETL-LOC-STAT-DT OF P-DDDTLR01                          00184500
184600     IF RETL-LOC-STAT-DT OF P-DDDTLR01 = K-DEF-DT                 00184600
184700       MOVE SPACES TO RETL-LOC-STAT-DT OF P-DDDTLR01              00184700
184800     END-IF                                                       00184800
184900                                                                  00184900
185000     MOVE COMPANY-ID OF DCLXXXAIL-LOC TO COMPANY-ID OF P-DDDTLR01 00185000
185100     MOVE FINANCIAL-DIV-ID OF DCLXXXAIL-LOC                       00185100
185200       TO FINANCIAL-DIV-ID OF P-DDDTLR01                          00185200
185300     MOVE LIN-OF-BUS-ID OF DCLXXXAIL-LOC                          00185300
185400       TO LIN-OF-BUS-ID OF P-DDDTLR01                             00185400
185500     MOVE DIST-ID OF DCLXXXAIL-LOC TO DIST-ID OF P-DDDTLR01       00185500
185600     MOVE MKT-RGN-ID OF DCLXXXAIL-LOC TO MKT-RGN-ID OF P-DDDTLR01 00185600
185700     MOVE GEO-ZN-CD OF DCLXXXAIL-LOC TO GEO-ZN-CD OF P-DDDTLR01   00185700
185800     MOVE RETL-GEO-ZN-ID OF DCLXXXAIL-LOC                         00185800
185900       TO RETL-GEO-ZN-ID OF P-DDDTLR01                            00185900
186000     MOVE SCN-MAINT-SW OF DCLXXXAIL-LOC                           00186000
186100       TO SCN-MAINT-SW OF P-DDDTLR01                              00186100
186200     MOVE FRNT-END-CD OF DCLXXXAIL-LOC                            00186200
186300       TO FRNT-END-CD OF P-DDDTLR01                               00186300
186400     MOVE PRC-BUL-SW OF DCLXXXAIL-LOC TO PRC-BUL-SW OF P-DDDTLR01 00186400
186500     MOVE UPC-ON-PRC-BUL-SW OF DCLXXXAIL-LOC                      00186500
186600       TO UPC-ON-PRC-BUL-SW OF P-DDDTLR01                         00186600
186700     MOVE CMPTR-TYP-CD OF DCLXXXAIL-LOC                           00186700
186800       TO CMPTR-TYP-CD OF P-DDDTLR01                              00186800
186900     MOVE RETL-VID-ZN-NBR OF DCLXXXAIL-LOC                        00186900
187000       TO RETL-VID-ZN-NBR OF P-DDDTLR01                           00187000
187100     MOVE RETL-UNLD-CD OF DCLXXXAIL-LOC                           00187100
187200       TO RETL-UNLD-CD OF P-DDDTLR01                              00187200
187300*    MOVE ROLUP-REPT-TBL-TXT OF DCLXXXAIL-LOC                     00187300
187400*      TO ROLUP-REPT-TBL-TXT OF P-DDDTLR01                        00187400
187500     PERFORM 135-MOVE-DC-ROLLUP-DATA                              00187500
187600     MOVE NEW-STR-SW OF DCLXXXAIL-LOC TO NEW-STR-SW OF P-DDDTLR01 00187600
187700     MOVE SEL-CIR-SW OF DCLXXXAIL-LOC TO SEL-CIR-SW OF P-DDDTLR01 00187700
187800     MOVE BKRM-SQ-FT OF DCLXXXAIL-LOC TO BKRM-SQ-FT OF P-DDDTLR01 00187800
187900     MOVE FD-LINER-FT OF DCLXXXAIL-LOC                            00187900
188000       TO FD-LINER-FT OF P-DDDTLR01                               00188000
188100     MOVE NON-FD-LINER-FT OF DCLXXXAIL-LOC                        00188100
188200       TO NON-FD-LINER-FT OF P-DDDTLR01                           00188200
188300     MOVE SETOFF-ROOM-SW OF DCLXXXAIL-LOC                         00188300
188400       TO SETOFF-ROOM-SW OF P-DDDTLR01                            00188400
188500     MOVE CAT-CLS-TBL-TXT OF DCLXXXAIL-LOC                        00188500
188600       TO CAT-CLS-TBL-TXT OF P-DDDTLR01                           00188600
188700     MOVE LAT-K OF DCLXXXAIL-LOC TO LAT-K OF P-DDDTLR01           00188700
188800     MOVE LON-K OF DCLXXXAIL-LOC TO LON-K OF P-DDDTLR01           00188800
188900     MOVE CK-COLL-REPT-SW OF DCLXXXAIL-LOC                        00188900
189000       TO CK-COLL-REPT-SW OF P-DDDTLR01                           00189000
189100     MOVE CK-COLL-CNTL-CD OF DCLXXXAIL-LOC                        00189100
189200       TO CK-COLL-CNTL-CD OF P-DDDTLR01                           00189200
189300     MOVE CK-COLL-ADD-DEL-SW OF DCLXXXAIL-LOC                     00189300
189400       TO CK-COLL-ADD-DEL-SW OF P-DDDTLR01                        00189400
189500     MOVE CK-ALT-STR-ID OF DCLXXXAIL-LOC                          00189500
189600       TO CK-ALT-STR-ID OF P-DDDTLR01                             00189600
189700     MOVE CK-COLL-FEE-AMT OF DCLXXXAIL-LOC                        00189700
189800       TO CK-COLL-FEE-AMT OF P-DDDTLR01                           00189800
189900     MOVE SALS-TAX-PCT OF DCLXXXAIL-LOC                           00189900
190000       TO SALS-TAX-PCT OF P-DDDTLR01                              00190000
190100     MOVE SOAP-SALE-VAR-PCT OF DCLXXXAIL-LOC                      00190100
190200       TO SOAP-SALE-VAR-PCT OF P-DDDTLR01                         00190200
190300     MOVE ON-SRS-CD OF DCLXXXAIL-LOC TO ON-SRS-CD OF P-DDDTLR01   00190300
190400     MOVE SRS-DSD-ORD-SW OF DCLXXXAIL-LOC                         00190400
190500       TO SRS-DSD-ORD-SW OF P-DDDTLR01                            00190500
190600     MOVE RETL-LOC-TYP-CD OF DCLXXXAIL-LOC                        00190600
190700       TO RETL-LOC-TYP-CD OF P-DDDTLR01                           00190700
190800     MOVE DEA-NBR OF DCLXXXAIL-LOC TO DEA-NBR OF P-DDDTLR01       00190800
190900     MOVE STR-OPSTMT-SRT-CD OF DCLXXXAIL-LOC                      00190900
191000       TO STR-OPSTMT-SRT-CD OF P-DDDTLR01                         00191000
191100     MOVE STR-OPSTMT-TYP-CD OF DCLXXXAIL-LOC                      00191100
191200       TO STR-OPSTMT-TYP-CD OF P-DDDTLR01                         00191200
191300     MOVE STR-OPSTMT-HDR-CD OF DCLXXXAIL-LOC                      00191300
191400       TO STR-OPSTMT-HDR-CD OF P-DDDTLR01                         00191400
191500     MOVE DPS-NBR OF DCLXXXAIL-LOC TO DPS-NBR OF P-DDDTLR01       00191500
191600     MOVE MEDICARE-ID OF DCLXXXAIL-LOC                            00191600
191700       TO MEDICARE-ID OF P-DDDTLR01                               00191700
191800     MOVE NABP-NBR OF DCLXXXAIL-LOC TO NABP-NBR OF P-DDDTLR01     00191800
191900     MOVE NATL-PROV-ID OF DCLXXXAIL-LOC                           00191900
192000       TO NATL-PROV-ID OF P-DDDTLR01                              00192000
192100     MOVE CURR-AD-ZN-NBR OF DCLXXXAIL-LOC                         00192100
192200       TO CURR-AD-ZN-NBR OF P-DDDTLR01                            00192200
192300     MOVE PD-ZONE-NO OF DCLXXXAIL-LOC TO PD-ZONE-NO OF P-DDDTLR01 00192300
192400     MOVE SOS-PROC-SW OF DCLXXXAIL-LOC                            00192400
192500       TO SOS-PROC-SW OF P-DDDTLR01                               00192500
192600     MOVE RPRT-SEQ-NBR OF DCLXXXAIL-LOC                           00192600
192700       TO RPRT-SEQ-NBR OF P-DDDTLR01                              00192700
192800     MOVE GRP-CD OF DCLXXXAIL-LOC TO GRP-CD OF P-DDDTLR01         00192800
192900     MOVE PRIM-GRP-CD-1 OF DCLXXXAIL-LOC                          00192900
193000       TO PRIM-GRP-CD-1 OF P-DDDTLR01                             00193000
193100     MOVE PRIM-GRP-CD-2 OF DCLXXXAIL-LOC                          00193100
193200       TO PRIM-GRP-CD-2 OF P-DDDTLR01                             00193200
193300     MOVE SECY-GRP-CD-1 OF DCLXXXAIL-LOC                          00193300
193400       TO SECY-GRP-CD-1 OF P-DDDTLR01                             00193400
193500     MOVE SECY-GRP-CD-2 OF DCLXXXAIL-LOC                          00193500
193600       TO SECY-GRP-CD-2 OF P-DDDTLR01                             00193600
193700     MOVE PRIM-CLS-NBR-1 OF DCLXXXAIL-LOC                         00193700
193800       TO PRIM-CLS-NBR-1 OF P-DDDTLR01                            00193800
193900     MOVE PRIM-CLS-NBR-2 OF DCLXXXAIL-LOC                         00193900
194000       TO PRIM-CLS-NBR-2 OF P-DDDTLR01                            00194000
194100     MOVE SECY-CLS-NBR-1 OF DCLXXXAIL-LOC                         00194100
194200       TO SECY-CLS-NBR-1 OF P-DDDTLR01                            00194200
194300     MOVE SECY-CLS-NBR-2 OF DCLXXXAIL-LOC                         00194300
194400       TO SECY-CLS-NBR-2 OF P-DDDTLR01                            00194400
194500     MOVE VAL-STR-SW OF DCLXXXAIL-LOC TO VAL-STR-SW OF P-DDDTLR01 00194500
194600     MOVE TBCO-PRMT-NBR OF DCLXXXAIL-LOC                          00194600
194700       TO TBCO-PRMT-NBR OF P-DDDTLR01                             00194700
194800                                                                  00194800
194900     MOVE SUB-UNLIKE-PROD-CD OF DCLXXXAIL-LOC                     00194900
195000       TO SUB-UNLIKE-PROD-CD OF P-DDDTLR01                        00195000
195100     IF  NOT OK-TO-SUB-UNLIKE-PRODS     OF P-DDDTLR01             00195100
195200     AND NOT DONT-SUB-UNLIKE-PRODS      OF P-DDDTLR01             00195200
195300       SET NO-UNLIKE-SUB-STORE-PREF     OF P-DDDTLR01 TO TRUE     00195300
195400     END-IF                                                       00195400
195500                                                                  00195500
195600     MOVE SUB-DSPLY-PAL-CD   OF DCLXXXAIL-LOC                     00195600
195700       TO SUB-DSPLY-PAL-CD   OF P-DDDTLR01                        00195700
195800     IF  NOT OK-TO-SUB-DISP-PALS        OF P-DDDTLR01             00195800
195900     AND NOT DONT-SUB-DISP-PALS         OF P-DDDTLR01             00195900
196000       SET NO-DISP-PAL-SUB-STORE-PREF   OF P-DDDTLR01 TO TRUE     00196000
196100     END-IF                                                       00196100
196200                                                                  00196200
196300     MOVE RLTM-SCN-MAINT-SW  OF DCLXXXAIL-LOC                     00196300
196400       TO RLTM-SCN-MAINT-SW  OF P-DDDTLR01                        00196400
196500     IF  NOT SEND-REAL-TIME-G3          OF P-DDDTLR01             00196500
196600       SET DONT-SEND-REAL-TIME-G3       OF P-DDDTLR01 TO TRUE     00196600
196700     END-IF                                                       00196700
196800     MOVE 0                                                       00196800
196900       TO DONTUSE1 OF P-DDDTLR01                                  00196900
197000     MOVE 0                                                       00197000
197100       TO DONTUSE2 OF P-DDDTLR01                                  00197100
197200     MOVE TOP-LEADER-NM   OF DCLXXXAIL-LOC                        00197200
197300       TO TOP-LEADER-NM   OF P-DDDTLR01                           00197300
197400     MOVE CUST-FRNDLY-NM  OF DCLXXXAIL-LOC                        00197400
197500       TO CUST-FRNDLY-NM  OF P-DDDTLR01                           00197500
197600     MOVE SLS-OPEN-DT     OF DCLXXXAIL-LOC                        00197600
197700       TO SLS-OPEN-DT     OF P-DDDTLR01                           00197700
197800     IF SLS-OPEN-DT OF P-DDDTLR01 = K-DEF-DT                      00197800
197900       MOVE SPACES TO SLS-OPEN-DT  OF P-DDDTLR01                  00197900
198000     END-IF                                                       00198000
198100     IF MON-OPEN-TM OF P-DDDTLR01 = K-DEF-TM                      00198100
198200       MOVE SPACES TO MON-OPEN-TM  OF P-DDDTLR01                  00198200
198300     END-IF                                                       00198300
198400     IF MON-CLOS-TM OF DCLXXXAIL-LOC = K-DB2-MAX-TM               00198400
198500       MOVE K-ORA-MAX-TM  TO MON-CLOS-TM OF DCLXXXAIL-LOC         00198500
198600     END-IF                                                       00198600
198700     IF MON-CLOS-TM OF P-DDDTLR01 = K-DEF-TM                      00198700
198800       MOVE SPACES TO MON-CLOS-TM  OF P-DDDTLR01                  00198800
198900     END-IF                                                       00198900
199000     IF TUE-OPEN-TM OF P-DDDTLR01 = K-DEF-TM                      00199000
199100       MOVE SPACES TO TUE-OPEN-TM  OF P-DDDTLR01                  00199100
199200     END-IF                                                       00199200
199300     IF TUE-CLOS-TM OF DCLXXXAIL-LOC = K-DB2-MAX-TM               00199300
199400       MOVE K-ORA-MAX-TM  TO TUE-CLOS-TM OF DCLXXXAIL-LOC         00199400
199500     END-IF                                                       00199500
199600     IF TUE-CLOS-TM OF P-DDDTLR01 = K-DEF-TM                      00199600
199700       MOVE SPACES TO TUE-CLOS-TM  OF P-DDDTLR01                  00199700
199800     END-IF                                                       00199800
199900     IF WED-OPEN-TM OF P-DDDTLR01 = K-DEF-TM                      00199900
200000       MOVE SPACES TO WED-OPEN-TM  OF P-DDDTLR01                  00200000
200100     END-IF                                                       00200100
200200     IF WED-CLOS-TM OF DCLXXXAIL-LOC = K-DB2-MAX-TM               00200200
200300       MOVE K-ORA-MAX-TM  TO WED-CLOS-TM OF DCLXXXAIL-LOC         00200300
200400     END-IF                                                       00200400
200500     IF WED-CLOS-TM OF P-DDDTLR01 = K-DEF-TM                      00200500
200600       MOVE SPACES TO WED-CLOS-TM  OF P-DDDTLR01                  00200600
200700     END-IF                                                       00200700
200800     IF THUR-OPEN-TM OF P-DDDTLR01 = K-DEF-TM                     00200800
200900       MOVE SPACES TO THUR-OPEN-TM OF P-DDDTLR01                  00200900
201000     END-IF                                                       00201000
201100     IF THUR-CLOS-TM OF DCLXXXAIL-LOC = K-DB2-MAX-TM              00201100
201200       MOVE K-ORA-MAX-TM  TO THUR-CLOS-TM OF DCLXXXAIL-LOC        00201200
201300     END-IF                                                       00201300
201400     IF THUR-CLOS-TM OF P-DDDTLR01 = K-DEF-TM                     00201400
201500       MOVE SPACES TO THUR-CLOS-TM OF P-DDDTLR01                  00201500
201600     END-IF                                                       00201600
201700     IF FRI-OPEN-TM OF P-DDDTLR01 = K-DEF-TM                      00201700
201800       MOVE SPACES TO FRI-OPEN-TM  OF P-DDDTLR01                  00201800
201900     END-IF                                                       00201900
202000     IF FRI-CLOS-TM OF DCLXXXAIL-LOC = K-DB2-MAX-TM               00202000
202100       MOVE K-ORA-MAX-TM  TO FRI-CLOS-TM OF DCLXXXAIL-LOC         00202100
202200     END-IF                                                       00202200
202300     IF FRI-CLOS-TM OF P-DDDTLR01 = K-DEF-TM                      00202300
202400       MOVE SPACES TO FRI-CLOS-TM  OF P-DDDTLR01                  00202400
202500     END-IF                                                       00202500
202600     IF SAT-OPEN-TM OF P-DDDTLR01 = K-DEF-TM                      00202600
202700       MOVE SPACES TO SAT-OPEN-TM  OF P-DDDTLR01                  00202700
202800     END-IF                                                       00202800
202900     IF SUN-OPEN-TM OF P-DDDTLR01 = K-DEF-TM                      00202900
203000       MOVE SPACES TO SUN-OPEN-TM  OF P-DDDTLR01                  00203000
203100     END-IF                                                       00203100
203200     IF SAT-CLOS-TM OF DCLXXXAIL-LOC = K-DB2-MAX-TM               00203200
203300       MOVE K-ORA-MAX-TM  TO SAT-CLOS-TM OF DCLXXXAIL-LOC         00203300
203400     END-IF                                                       00203400
203500     IF SAT-CLOS-TM OF P-DDDTLR01 = K-DEF-TM                      00203500
203600       MOVE SPACES TO SAT-CLOS-TM  OF P-DDDTLR01                  00203600
203700     END-IF                                                       00203700
203800     IF SUN-CLOS-TM OF DCLXXXAIL-LOC = K-DB2-MAX-TM               00203800
203900       MOVE K-ORA-MAX-TM  TO SUN-CLOS-TM OF DCLXXXAIL-LOC         00203900
204000     END-IF                                                       00204000
204100     IF SUN-CLOS-TM OF P-DDDTLR01 = K-DEF-TM                      00204100
204200       MOVE SPACES TO SUN-CLOS-TM  OF P-DDDTLR01                  00204200
204300     END-IF                                                       00204300
204400     PERFORM 132-MOVE-TIME-FIELDS                                 00204400
204500     MOVE RETL-LOC-FRMAT-CD OF DCLXXXAIL-LOC                      00204500
204600       TO RETL-LOC-FRMAT-CD OF P-DDDTLR01                         00204600
204700     MOVE RETL-LOC-SEGM-CD OF DCLXXXAIL-LOC                       00204700
204800       TO RETL-LOC-SEGM-CD OF P-DDDTLR01                          00204800
204900     MOVE ECOMM-MKT-AREA-CD OF DCLXXXAIL-LOC                      00204900
205000       TO ECOMM-MKT-AREA-CD OF P-DDDTLR01                         00205000
205100     MOVE ECOMM-STRT-DT OF DCLXXXAIL-LOC                          00205100
205200       TO ECOMM-STRT-DT OF P-DDDTLR01                             00205200
205300     IF ECOMM-STRT-DT OF P-DDDTLR01 = K-DEF-DT                    00205300
205400       MOVE SPACES TO ECOMM-STRT-DT OF P-DDDTLR01                 00205400
205500     END-IF                                                       00205500
205600     MOVE ECOMM-END-DT OF DCLXXXAIL-LOC                           00205600
205700       TO ECOMM-END-DT OF P-DDDTLR01                              00205700
205800     IF ECOMM-END-DT OF P-DDDTLR01 = K-DEF-DT                     00205800
205900       MOVE SPACES TO ECOMM-END-DT OF P-DDDTLR01                  00205900
206000     END-IF                                                       00206000
206100     MOVE ONLIN-SSON-SW OF DCLXXXAIL-LOC                          00206100
206200                        TO ONLIN-SSON-SW OF P-DDDTLR01            00206200
206300     MOVE RPLACD-BY-STR-NBR OF DCLXXXAIL-LOC                      00206300
206400                        TO RPLACD-BY-STR-NBR OF P-DDDTLR01.       00206400
206500                                                                  00206500
206600 135-MOVE-DC-ROLLUP-DATA.                                         00206600
206700     IF ROLUP-REPT-TBL-01-NBR OF DCLXXXAIL-LOC IS NUMERIC         00206700
206800        MOVE ROLUP-REPT-TBL-01-NBR OF DCLXXXAIL-LOC               00206800
206900          TO WS-REPT-TBL-NUMERIC(1)                               00206900
207000     ELSE                                                         00207000
207100        MOVE ZERO TO WS-REPT-TBL-NUMERIC(1)                       00207100
207200     END-IF                                                       00207200
207300     IF ROLUP-REPT-TBL-02-NBR OF DCLXXXAIL-LOC IS NUMERIC         00207300
207400        MOVE ROLUP-REPT-TBL-02-NBR OF DCLXXXAIL-LOC               00207400
207500          TO WS-REPT-TBL-NUMERIC(2)                               00207500
207600     ELSE                                                         00207600
207700        MOVE ZERO TO WS-REPT-TBL-NUMERIC(2)                       00207700
207800     END-IF                                                       00207800
207900     IF ROLUP-REPT-TBL-03-NBR OF DCLXXXAIL-LOC IS NUMERIC         00207900
208000        MOVE ROLUP-REPT-TBL-03-NBR OF DCLXXXAIL-LOC               00208000
208100          TO WS-REPT-TBL-NUMERIC(3)                               00208100
208200     ELSE                                                         00208200
208300        MOVE ZERO TO WS-REPT-TBL-NUMERIC(3)                       00208300
208400     END-IF                                                       00208400
208500     IF ROLUP-REPT-TBL-04-NBR OF DCLXXXAIL-LOC IS NUMERIC         00208500
208600        MOVE ROLUP-REPT-TBL-04-NBR OF DCLXXXAIL-LOC               00208600
208700          TO WS-REPT-TBL-NUMERIC(4)                               00208700
208800     ELSE                                                         00208800
208900        MOVE ZERO TO WS-REPT-TBL-NUMERIC(4)                       00208900
209000     END-IF                                                       00209000
209100     IF ROLUP-REPT-TBL-05-NBR OF DCLXXXAIL-LOC IS NUMERIC         00209100
209200        MOVE ROLUP-REPT-TBL-05-NBR OF DCLXXXAIL-LOC               00209200
209300          TO WS-REPT-TBL-NUMERIC(5)                               00209300
209400     ELSE                                                         00209400
209500        MOVE ZERO TO WS-REPT-TBL-NUMERIC(5)                       00209500
209600     END-IF                                                       00209600
209700     IF ROLUP-REPT-TBL-06-NBR OF DCLXXXAIL-LOC IS NUMERIC         00209700
209800        MOVE ROLUP-REPT-TBL-06-NBR OF DCLXXXAIL-LOC               00209800
209900          TO WS-REPT-TBL-NUMERIC(6)                               00209900
210000     ELSE                                                         00210000
210100        MOVE ZERO TO WS-REPT-TBL-NUMERIC(6)                       00210100
210200     END-IF                                                       00210200
210300     IF ROLUP-REPT-TBL-07-NBR OF DCLXXXAIL-LOC IS NUMERIC         00210300
210400        MOVE ROLUP-REPT-TBL-07-NBR OF DCLXXXAIL-LOC               00210400
210500          TO WS-REPT-TBL-NUMERIC(7)                               00210500
210600     ELSE                                                         00210600
210700        MOVE ZERO TO WS-REPT-TBL-NUMERIC(7)                       00210700
210800     END-IF                                                       00210800
210900     IF ROLUP-REPT-TBL-08-NBR OF DCLXXXAIL-LOC IS NUMERIC         00210900
211000        MOVE ROLUP-REPT-TBL-08-NBR OF DCLXXXAIL-LOC               00211000
211100          TO WS-REPT-TBL-NUMERIC(8)                               00211100
211200     ELSE                                                         00211200
211300        MOVE ZERO TO WS-REPT-TBL-NUMERIC(8)                       00211300
211400     END-IF                                                       00211400
211500     IF ROLUP-REPT-TBL-09-NBR OF DCLXXXAIL-LOC IS NUMERIC         00211500
211600        MOVE ROLUP-REPT-TBL-09-NBR OF DCLXXXAIL-LOC               00211600
211700          TO WS-REPT-TBL-NUMERIC(9)                               00211700
211800     ELSE                                                         00211800
211900        MOVE ZERO TO WS-REPT-TBL-NUMERIC(9)                       00211900
212000     END-IF                                                       00212000
212100     IF ROLUP-REPT-TBL-10-NBR OF DCLXXXAIL-LOC IS NUMERIC         00212100
212200        MOVE ROLUP-REPT-TBL-10-NBR OF DCLXXXAIL-LOC               00212200
212300          TO WS-REPT-TBL-NUMERIC(10)                              00212300
212400     ELSE                                                         00212400
212500        MOVE ZERO TO WS-REPT-TBL-NUMERIC(10)                      00212500
212600     END-IF                                                       00212600
212700     MOVE WS-REPT-TBL-TXT                                         00212700
212800       TO ROLUP-REPT-TBL-TXT OF P-DDDTLR01                        00212800
212900     .                                                            00212900
213000                                                                  00213000
213100* ================================================================00213100
213200* Convert Oracle timestamp format to DB2 format                   00213200
213300* ================================================================00213300
213400 132-MOVE-TIME-FIELDS.                                            00213400
213500     IF (YYYN005A-ORACLE OR EXIT-PUT-INSERT-ROW                   00213500
213600         OR EXIT-PUT-MODIFY-ROW)                                  00213600
213700       INITIALIZE MMMC0291-INPUT-TM                               00213700
213800                  MMMC0291-INPUT-TS                               00213800
213900       MOVE WS-MON-OPEN-TS                                        00213900
214000         TO WS-TIMSTAMP-INOUT-CONV(1)                             00214000
214100       MOVE WS-MON-CLOS-TS                                        00214100
214200         TO WS-TIMSTAMP-INOUT-CONV(2)                             00214200
214300       MOVE WS-TUE-OPEN-TS                                        00214300
214400         TO WS-TIMSTAMP-INOUT-CONV(3)                             00214400
214500       MOVE WS-TUE-CLOS-TS                                        00214500
214600         TO WS-TIMSTAMP-INOUT-CONV(4)                             00214600
214700       MOVE WS-WED-OPEN-TS                                        00214700
214800         TO WS-TIMSTAMP-INOUT-CONV(5)                             00214800
214900       MOVE WS-WED-CLOS-TS                                        00214900
215000         TO WS-TIMSTAMP-INOUT-CONV(6)                             00215000
215100       MOVE WS-THUR-OPEN-TS                                       00215100
215200         TO WS-TIMSTAMP-INOUT-CONV(7)                             00215200
215300       MOVE WS-THUR-CLOS-TS                                       00215300
215400         TO WS-TIMSTAMP-INOUT-CONV(8)                             00215400
215500       MOVE WS-FRI-OPEN-TS                                        00215500
215600         TO WS-TIMSTAMP-INOUT-CONV(9)                             00215600
215700       MOVE WS-FRI-CLOS-TS                                        00215700
215800         TO WS-TIMSTAMP-INOUT-CONV(10)                            00215800
215900       MOVE WS-SAT-OPEN-TS                                        00215900
216000         TO WS-TIMSTAMP-INOUT-CONV(11)                            00216000
216100       MOVE WS-SAT-CLOS-TS                                        00216100
216200         TO WS-TIMSTAMP-INOUT-CONV(12)                            00216200
216300       MOVE WS-SUN-OPEN-TS                                        00216300
216400         TO WS-TIMSTAMP-INOUT-CONV(13)                            00216400
216500       MOVE WS-SUN-CLOS-TS                                        00216500
216600         TO WS-TIMSTAMP-INOUT-CONV(14)                            00216600
216700                                                                  00216700
216800        SET MMMC0291-CVT-TS-TO-TM  TO TRUE                        00216800
216900        CALL WS-MMMS0291-PGM USING                                00216900
217000                           XXXN001A                               00217000
217100                           MMMC0291                               00217100
217200        IF NOT SUCCESS                                            00217200
217300          STRING 'NNNS0488 - INVALD TIMSTMP.PLS VERIFY Sqlcode =' 00217300
217400              WS-SQLCODE                                          00217400
217500              DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT              00217500
217600        ELSE                                                      00217600
217700          MOVE WS-TIME-INOUT-CONV(1)                              00217700
217800            TO MON-OPEN-TM OF P-DDDTLR01                          00217800
217900          MOVE WS-TIME-INOUT-CONV(2)                              00217900
218000            TO MON-CLOS-TM OF P-DDDTLR01                          00218000
218100          MOVE WS-TIME-INOUT-CONV(3)                              00218100
218200            TO TUE-OPEN-TM OF P-DDDTLR01                          00218200
218300          MOVE WS-TIME-INOUT-CONV(4)                              00218300
218400            TO TUE-CLOS-TM OF P-DDDTLR01                          00218400
218500          MOVE WS-TIME-INOUT-CONV(5)                              00218500
218600            TO WED-OPEN-TM OF P-DDDTLR01                          00218600
218700          MOVE WS-TIME-INOUT-CONV(6)                              00218700
218800            TO WED-CLOS-TM OF P-DDDTLR01                          00218800
218900          MOVE WS-TIME-INOUT-CONV(7)                              00218900
219000            TO THUR-OPEN-TM OF P-DDDTLR01                         00219000
219100          MOVE WS-TIME-INOUT-CONV(8)                              00219100
219200            TO THUR-CLOS-TM OF P-DDDTLR01                         00219200
219300          MOVE WS-TIME-INOUT-CONV(9)                              00219300
219400            TO FRI-OPEN-TM OF P-DDDTLR01                          00219400
219500          MOVE WS-TIME-INOUT-CONV(10)                             00219500
219600            TO FRI-CLOS-TM OF P-DDDTLR01                          00219600
219700          MOVE WS-TIME-INOUT-CONV(11)                             00219700
219800            TO SAT-OPEN-TM OF P-DDDTLR01                          00219800
219900          MOVE WS-TIME-INOUT-CONV(12)                             00219900
220000            TO SAT-CLOS-TM OF P-DDDTLR01                          00220000
220100          MOVE WS-TIME-INOUT-CONV(13)                             00220100
220200            TO SUN-OPEN-TM OF P-DDDTLR01                          00220200
220300          MOVE WS-TIME-INOUT-CONV(14)                             00220300
220400            TO SUN-CLOS-TM OF P-DDDTLR01                          00220400
220500        END-IF                                                    00220500
220600     ELSE                                                         00220600
220700       MOVE MON-OPEN-TM OF DCLXXXAIL-LOC                          00220700
220800         TO MON-OPEN-TM    OF P-DDDTLR01                          00220800
220900       MOVE MON-CLOS-TM    OF DCLXXXAIL-LOC                       00220900
221000         TO MON-CLOS-TM    OF P-DDDTLR01                          00221000
221100       MOVE TUE-OPEN-TM    OF DCLXXXAIL-LOC                       00221100
221200         TO TUE-OPEN-TM    OF P-DDDTLR01                          00221200
221300       MOVE TUE-CLOS-TM    OF DCLXXXAIL-LOC                       00221300
221400         TO TUE-CLOS-TM    OF P-DDDTLR01                          00221400
221500       MOVE WED-OPEN-TM    OF DCLXXXAIL-LOC                       00221500
221600         TO WED-OPEN-TM    OF P-DDDTLR01                          00221600
221700       MOVE WED-CLOS-TM    OF DCLXXXAIL-LOC                       00221700
221800         TO WED-CLOS-TM    OF P-DDDTLR01                          00221800
221900       MOVE THUR-OPEN-TM   OF DCLXXXAIL-LOC                       00221900
222000         TO THUR-OPEN-TM   OF P-DDDTLR01                          00222000
222100       MOVE THUR-CLOS-TM   OF DCLXXXAIL-LOC                       00222100
222200         TO THUR-CLOS-TM   OF P-DDDTLR01                          00222200
222300       MOVE FRI-OPEN-TM    OF DCLXXXAIL-LOC                       00222300
222400         TO FRI-OPEN-TM    OF P-DDDTLR01                          00222400
222500       MOVE FRI-CLOS-TM    OF DCLXXXAIL-LOC                       00222500
222600         TO FRI-CLOS-TM    OF P-DDDTLR01                          00222600
222700       MOVE SAT-OPEN-TM    OF DCLXXXAIL-LOC                       00222700
222800         TO SAT-OPEN-TM    OF P-DDDTLR01                          00222800
222900       MOVE SAT-CLOS-TM    OF DCLXXXAIL-LOC                       00222900
223000         TO SAT-CLOS-TM    OF P-DDDTLR01                          00223000
223100       MOVE SUN-OPEN-TM    OF DCLXXXAIL-LOC                       00223100
223200         TO SUN-OPEN-TM    OF P-DDDTLR01                          00223200
223300       MOVE SUN-CLOS-TM    OF DCLXXXAIL-LOC                       00223300
223400         TO SUN-CLOS-TM    OF P-DDDTLR01                          00223400
223500     END-IF                                                       00223500
223600     .                                                            00223600
223700                                                                  00223700
223800                                                                  00223800
223900* ================================================================00223900
224000* Code required to do static sql including cursor, select, fetch, 00224000
224100* update, insert, and delete operations.                          00224100
224200* ================================================================00224200
224300 1000-EXIT-OPEN-CURSOR.                                           00224300
224400     EVALUATE TRUE                                                00224400
224500       WHEN DDDXLR01                                              00224500
224600         EXEC SQL                                                 00224600
224700           OPEN DDDXLR01                                          00224700
224800         END-EXEC                                                 00224800
224900       WHEN DDDXLR02                                              00224900
225000         EXEC SQL                                                 00225000
225100           OPEN DDDXLR02                                          00225100
225200         END-EXEC                                                 00225200
225300       WHEN DDDXLR03                                              00225300
225400         EXEC SQL                                                 00225400
225500           OPEN DDDXLR03                                          00225500
225600         END-EXEC                                                 00225600
225700       WHEN DDDXLR04                                              00225700
225800         EXEC SQL                                                 00225800
225900           OPEN DDDXLR04                                          00225900
226000         END-EXEC                                                 00226000
226100       WHEN DDDXLR05                                              00226100
226200         EXEC SQL                                                 00226200
226300           OPEN DDDXLR05                                          00226300
226400         END-EXEC                                                 00226400
226500       WHEN DDDXLR06                                              00226500
226600         EXEC SQL                                                 00226600
226700           OPEN DDDXLR06                                          00226700
226800         END-EXEC                                                 00226800
226900       WHEN DDDXLR07                                              00226900
227000         EXEC SQL                                                 00227000
227100           OPEN DDDXLR07                                          00227100
227200         END-EXEC                                                 00227200
227300       WHEN DDDXLR08                                              00227300
227400         EXEC SQL                                                 00227400
227500           OPEN DDDXLR08                                          00227500
227600         END-EXEC                                                 00227600
227700       WHEN DDDXLR09                                              00227700
227800         EXEC SQL                                                 00227800
227900           OPEN DDDXLR09                                          00227900
228000         END-EXEC                                                 00228000
228100       WHEN OTHER                                                 00228100
228200         SET FAILURE TO TRUE                                      00228200
228300         MOVE 'NNNS0488 - Invalid open cursor ID.'                00228300
228400           TO IS-RTRN-MSG-TXT OF XXXN001A                         00228400
228500     END-EVALUATE                                                 00228500
228600     .                                                            00228600
228700                                                                  00228700
228800                                                                  00228800
228900 1100-EXIT-CLOSE-CURSOR.                                          00228900
229000     EVALUATE TRUE                                                00229000
229100       WHEN DDDXLR01                                              00229100
229200         EXEC SQL                                                 00229200
229300           CLOSE DDDXLR01                                         00229300
229400         END-EXEC                                                 00229400
229500       WHEN DDDXLR02                                              00229500
229600         EXEC SQL                                                 00229600
229700           CLOSE DDDXLR02                                         00229700
229800         END-EXEC                                                 00229800
229900       WHEN DDDXLR03                                              00229900
230000         EXEC SQL                                                 00230000
230100           CLOSE DDDXLR03                                         00230100
230200         END-EXEC                                                 00230200
230300       WHEN DDDXLR04                                              00230300
230400         EXEC SQL                                                 00230400
230500           CLOSE DDDXLR04                                         00230500
230600         END-EXEC                                                 00230600
230700       WHEN DDDXLR05                                              00230700
230800         EXEC SQL                                                 00230800
230900           CLOSE DDDXLR05                                         00230900
231000         END-EXEC                                                 00231000
231100       WHEN DDDXLR06                                              00231100
231200         EXEC SQL                                                 00231200
231300           CLOSE DDDXLR06                                         00231300
231400         END-EXEC                                                 00231400
231500       WHEN DDDXLR07                                              00231500
231600         EXEC SQL                                                 00231600
231700           CLOSE DDDXLR07                                         00231700
231800         END-EXEC                                                 00231800
231900       WHEN DDDXLR09                                              00231900
232000         EXEC SQL                                                 00232000
232100           CLOSE DDDXLR09                                         00232100
232200         END-EXEC                                                 00232200
232300       WHEN OTHER                                                 00232300
232400         SET FAILURE TO TRUE                                      00232400
232500         MOVE 'NNNS0488 - Invalid close cursor ID.'               00232500
232600           TO IS-RTRN-MSG-TXT OF XXXN001A                         00232600
232700     END-EVALUATE                                                 00232700
232800     .                                                            00232800
232900                                                                  00232900
233000                                                                  00233000
233100 1200-EXIT-GET-UNIQUE-ROW.                                        00233100
233200       EXEC SQL                                                   00233200
233300           SELECT LOC_NBR,                                        00233300
233400                  LOC_TYP_CD,                                     00233400
233500                  ASSOC_STR_TYP_CD,                               00233500
233600                  ASSOC_STR_NBR,                                  00233600
233700                  STR_REMODL_DT,                                  00233700
233800                  RETL_LOC_STAT_CD,                               00233800
233900                  RETL_LOC_STAT_DT,                               00233900
234000                  COMPANY_ID,                                     00234000
234100                  FINANCIAL_DIV_ID,                               00234100
234200                  LIN_OF_BUS_ID,                                  00234200
234300                  DIST_ID,                                        00234300
234400                  MKT_RGN_ID,                                     00234400
234500                  GEO_ZN_CD,                                      00234500
234600                  RETL_GEO_ZN_ID,                                 00234600
234700                  SCN_MAINT_SW,                                   00234700
234800                  FRNT_END_CD,                                    00234800
234900                  PRC_BUL_SW,                                     00234900
235000                  UPC_ON_PRC_BUL_SW,                              00235000
235100                  CMPTR_TYP_CD,                                   00235100
235200                  RETL_VID_ZN_NBR,                                00235200
235300                  RETL_UNLD_CD,                                   00235300
235400                  ROLUP_REPT_TBL_TXT,                             00235400
235500                  NEW_STR_SW,                                     00235500
235600                  SEL_CIR_SW,                                     00235600
235700                  BKRM_SQ_FT,                                     00235700
235800                  FD_LINER_FT,                                    00235800
235900                  NON_FD_LINER_FT,                                00235900
236000                  SETOFF_ROOM_SW,                                 00236000
236100                  CAT_CLS_TBL_TXT,                                00236100
236200                  LAT_K,                                          00236200
236300                  LON_K,                                          00236300
236400                  CK_COLL_REPT_SW,                                00236400
236500                  CK_COLL_CNTL_CD,                                00236500
236600                  CK_COLL_ADD_DEL_SW,                             00236600
236700                  CK_ALT_STR_ID,                                  00236700
236800                  CK_COLL_FEE_AMT,                                00236800
236900                  SALS_TAX_PCT,                                   00236900
237000                  SOAP_SALE_VAR_PCT,                              00237000
237100                  ON_SRS_CD,                                      00237100
237200                  SRS_DSD_ORD_SW,                                 00237200
237300                  RETL_LOC_TYP_CD,                                00237300
237400                  DEA_NBR,                                        00237400
237500                  STR_OPSTMT_SRT_CD,                              00237500
237600                  STR_OPSTMT_TYP_CD,                              00237600
237700                  STR_OPSTMT_HDR_CD,                              00237700
237800                  DPS_NBR,                                        00237800
237900                  MEDICARE_ID,                                    00237900
238000                  NABP_NBR,                                       00238000
238100                  NATL_PROV_ID,                                   00238100
238200                  CURR_AD_ZN_NBR,                                 00238200
238300                  PD_ZONE_NO,                                     00238300
238400                  SOS_PROC_SW,                                    00238400
238500                  RPRT_SEQ_NBR,                                   00238500
238600                  GRP_CD,                                         00238600
238700                  PRIM_GRP_CD_1,                                  00238700
238800                  PRIM_GRP_CD_2,                                  00238800
238900                  SECY_GRP_CD_1,                                  00238900
239000                  SECY_GRP_CD_2,                                  00239000
239100                  PRIM_CLS_NBR_1,                                 00239100
239200                  PRIM_CLS_NBR_2,                                 00239200
239300                  SECY_CLS_NBR_1,                                 00239300
239400                  SECY_CLS_NBR_2,                                 00239400
239500                  VAL_STR_SW,                                     00239500
239600                  SLS_CLOSED_DT,                                  00239600
239700                  TBCO_PRMT_NBR,                                  00239700
239800                  SUB_UNLIKE_PROD_CD,                             00239800
239900                  SUB_DSPLY_PAL_CD,                               00239900
240000                  RLTM_SCN_MAINT_SW,                              00240000
240100                  TOP_LEADER_NM,                                  00240100
240200                  CUST_FRNDLY_NM,                                 00240200
240300                  SLS_OPEN_DT,                                    00240300
240400                  MON_OPEN_TM,                                    00240400
240500                  MON_CLOS_TM,                                    00240500
240600                  TUE_OPEN_TM,                                    00240600
240700                  TUE_CLOS_TM,                                    00240700
240800                  WED_OPEN_TM,                                    00240800
240900                  WED_CLOS_TM,                                    00240900
241000                  THUR_OPEN_TM,                                   00241000
241100                  THUR_CLOS_TM,                                   00241100
241200                  FRI_OPEN_TM,                                    00241200
241300                  FRI_CLOS_TM,                                    00241300
241400                  SAT_OPEN_TM,                                    00241400
241500                  SAT_CLOS_TM,                                    00241500
241600                  SUN_OPEN_TM,                                    00241600
241700                  SUN_CLOS_TM,                                    00241700
241800                  RETL_LOC_FRMAT_CD,                              00241800
241900                  RETL_LOC_SEGM_CD,                               00241900
242000                  ECOMM_MKT_AREA_CD,                              00242000
242100                  ECOMM_STRT_DT,                                  00242100
242200                  ECOMM_END_DT,                                   00242200
242300                  ROLUP_REPT_TBL_01_NBR,                          00242300
242400                  ROLUP_REPT_TBL_02_NBR,                          00242400
242500                  ROLUP_REPT_TBL_03_NBR,                          00242500
242600                  ROLUP_REPT_TBL_04_NBR,                          00242600
242700                  ROLUP_REPT_TBL_05_NBR,                          00242700
242800                  ROLUP_REPT_TBL_06_NBR,                          00242800
242900                  ROLUP_REPT_TBL_07_NBR,                          00242900
243000                  ROLUP_REPT_TBL_08_NBR,                          00243000
243100                  ROLUP_REPT_TBL_09_NBR,                          00243100
243200                  ROLUP_REPT_TBL_10_NBR,                          00243200
243300                  ONLIN_SSON_SW,                                  00243300
243400                  RPLACD_BY_STR_NBR                               00243400
243500           INTO   :DCLXXXAIL-LOC.LOC-NBR,                         00243500
243600                  :DCLXXXAIL-LOC.LOC-TYP-CD,                      00243600
243700                  :DCLXXXAIL-LOC.ASSOC-STR-TYP-CD                 00243700
243800                  :WS-ASSOC-ST-TYPE-IND,                          00243800
243900                  :DCLXXXAIL-LOC.ASSOC-STR-NBR                    00243900
244000                  :WS-ASSOC-ST-NO-IND,                            00244000
244100                  :DCLXXXAIL-LOC.STR-REMODL-DT,                   00244100
244200                  :DCLXXXAIL-LOC.RETL-LOC-STAT-CD,                00244200
244300                  :DCLXXXAIL-LOC.RETL-LOC-STAT-DT,                00244300
244400                  :DCLXXXAIL-LOC.COMPANY-ID,                      00244400
244500                  :DCLXXXAIL-LOC.FINANCIAL-DIV-ID,                00244500
244600                  :DCLXXXAIL-LOC.LIN-OF-BUS-ID,                   00244600
244700                  :DCLXXXAIL-LOC.DIST-ID,                         00244700
244800                  :DCLXXXAIL-LOC.MKT-RGN-ID,                      00244800
244900                  :DCLXXXAIL-LOC.GEO-ZN-CD,                       00244900
245000                  :DCLXXXAIL-LOC.RETL-GEO-ZN-ID,                  00245000
245100                  :DCLXXXAIL-LOC.SCN-MAINT-SW,                    00245100
245200                  :DCLXXXAIL-LOC.FRNT-END-CD,                     00245200
245300                  :DCLXXXAIL-LOC.PRC-BUL-SW,                      00245300
245400                  :DCLXXXAIL-LOC.UPC-ON-PRC-BUL-SW,               00245400
245500                  :DCLXXXAIL-LOC.CMPTR-TYP-CD,                    00245500
245600                  :DCLXXXAIL-LOC.RETL-VID-ZN-NBR,                 00245600
245700                  :DCLXXXAIL-LOC.RETL-UNLD-CD,                    00245700
245800                  :DCLXXXAIL-LOC.ROLUP-REPT-TBL-TXT,              00245800
245900                  :DCLXXXAIL-LOC.NEW-STR-SW,                      00245900
246000                  :DCLXXXAIL-LOC.SEL-CIR-SW,                      00246000
246100                  :DCLXXXAIL-LOC.BKRM-SQ-FT,                      00246100
246200                  :DCLXXXAIL-LOC.FD-LINER-FT,                     00246200
246300                  :DCLXXXAIL-LOC.NON-FD-LINER-FT,                 00246300
246400                  :DCLXXXAIL-LOC.SETOFF-ROOM-SW,                  00246400
246500                  :DCLXXXAIL-LOC.CAT-CLS-TBL-TXT,                 00246500
246600                  :DCLXXXAIL-LOC.LAT-K,                           00246600
246700                  :DCLXXXAIL-LOC.LON-K,                           00246700
246800                  :DCLXXXAIL-LOC.CK-COLL-REPT-SW,                 00246800
246900                  :DCLXXXAIL-LOC.CK-COLL-CNTL-CD,                 00246900
247000                  :DCLXXXAIL-LOC.CK-COLL-ADD-DEL-SW,              00247000
247100                  :DCLXXXAIL-LOC.CK-ALT-STR-ID,                   00247100
247200                  :DCLXXXAIL-LOC.CK-COLL-FEE-AMT,                 00247200
247300                  :DCLXXXAIL-LOC.SALS-TAX-PCT,                    00247300
247400                  :DCLXXXAIL-LOC.SOAP-SALE-VAR-PCT,               00247400
247500                  :DCLXXXAIL-LOC.ON-SRS-CD,                       00247500
247600                  :DCLXXXAIL-LOC.SRS-DSD-ORD-SW,                  00247600
247700                  :DCLXXXAIL-LOC.RETL-LOC-TYP-CD,                 00247700
247800                  :DCLXXXAIL-LOC.DEA-NBR,                         00247800
247900                  :DCLXXXAIL-LOC.STR-OPSTMT-SRT-CD,               00247900
248000                  :DCLXXXAIL-LOC.STR-OPSTMT-TYP-CD,               00248000
248100                  :DCLXXXAIL-LOC.STR-OPSTMT-HDR-CD,               00248100
248200                  :DCLXXXAIL-LOC.DPS-NBR,                         00248200
248300                  :DCLXXXAIL-LOC.MEDICARE-ID,                     00248300
248400                  :DCLXXXAIL-LOC.NABP-NBR,                        00248400
248500                  :DCLXXXAIL-LOC.NATL-PROV-ID,                    00248500
248600                  :DCLXXXAIL-LOC.CURR-AD-ZN-NBR,                  00248600
248700                  :DCLXXXAIL-LOC.PD-ZONE-NO,                      00248700
248800                  :DCLXXXAIL-LOC.SOS-PROC-SW,                     00248800
248900                  :DCLXXXAIL-LOC.RPRT-SEQ-NBR,                    00248900
249000                  :DCLXXXAIL-LOC.GRP-CD,                          00249000
249100                  :DCLXXXAIL-LOC.PRIM-GRP-CD-1,                   00249100
249200                  :DCLXXXAIL-LOC.PRIM-GRP-CD-2,                   00249200
249300                  :DCLXXXAIL-LOC.SECY-GRP-CD-1,                   00249300
249400                  :DCLXXXAIL-LOC.SECY-GRP-CD-2,                   00249400
249500                  :DCLXXXAIL-LOC.PRIM-CLS-NBR-1,                  00249500
249600                  :DCLXXXAIL-LOC.PRIM-CLS-NBR-2,                  00249600
249700                  :DCLXXXAIL-LOC.SECY-CLS-NBR-1,                  00249700
249800                  :DCLXXXAIL-LOC.SECY-CLS-NBR-2,                  00249800
249900                  :DCLXXXAIL-LOC.VAL-STR-SW,                      00249900
250000                  :DCLXXXAIL-LOC.SLS-CLOSED-DT,                   00250000
250100                  :DCLXXXAIL-LOC.TBCO-PRMT-NBR,                   00250100
250200                  :DCLXXXAIL-LOC.SUB-UNLIKE-PROD-CD,              00250200
250300                  :DCLXXXAIL-LOC.SUB-DSPLY-PAL-CD,                00250300
250400                  :DCLXXXAIL-LOC.RLTM-SCN-MAINT-SW,               00250400
250500                  :DCLXXXAIL-LOC.TOP-LEADER-NM,                   00250500
250600                  :DCLXXXAIL-LOC.CUST-FRNDLY-NM,                  00250600
250700                  :DCLXXXAIL-LOC.SLS-OPEN-DT,                     00250700
250800                  :WS-MON-OPEN-TS,                                00250800
250900                  :WS-MON-CLOS-TS,                                00250900
251000                  :WS-TUE-OPEN-TS,                                00251000
251100                  :WS-TUE-CLOS-TS,                                00251100
251200                  :WS-WED-OPEN-TS,                                00251200
251300                  :WS-WED-CLOS-TS,                                00251300
251400                  :WS-THUR-OPEN-TS,                               00251400
251500                  :WS-THUR-CLOS-TS,                               00251500
251600                  :WS-FRI-OPEN-TS ,                               00251600
251700                  :WS-FRI-CLOS-TS,                                00251700
251800                  :WS-SAT-OPEN-TS,                                00251800
251900                  :WS-SAT-CLOS-TS,                                00251900
252000                  :WS-SUN-OPEN-TS,                                00252000
252100                  :WS-SUN-CLOS-TS,                                00252100
252200                  :DCLXXXAIL-LOC.RETL-LOC-FRMAT-CD,               00252200
252300                  :DCLXXXAIL-LOC.RETL-LOC-SEGM-CD,                00252300
252400                  :DCLXXXAIL-LOC.ECOMM-MKT-AREA-CD,               00252400
252500                  :DCLXXXAIL-LOC.ECOMM-STRT-DT                    00252500
252600                  :DCLXXXAIL-LOC-IND.ECOMM-STRT-DT-IND,           00252600
252700                  :DCLXXXAIL-LOC.ECOMM-END-DT                     00252700
252800                  :DCLXXXAIL-LOC-IND.ECOMM-END-DT-IND,            00252800
252900                  :DCLXXXAIL-LOC.ROLUP-REPT-TBL-01-NBR,           00252900
253000                  :DCLXXXAIL-LOC.ROLUP-REPT-TBL-02-NBR,           00253000
253100                  :DCLXXXAIL-LOC.ROLUP-REPT-TBL-03-NBR,           00253100
253200                  :DCLXXXAIL-LOC.ROLUP-REPT-TBL-04-NBR,           00253200
253300                  :DCLXXXAIL-LOC.ROLUP-REPT-TBL-05-NBR,           00253300
253400                  :DCLXXXAIL-LOC.ROLUP-REPT-TBL-06-NBR,           00253400
253500                  :DCLXXXAIL-LOC.ROLUP-REPT-TBL-07-NBR,           00253500
253600                  :DCLXXXAIL-LOC.ROLUP-REPT-TBL-08-NBR,           00253600
253700                  :DCLXXXAIL-LOC.ROLUP-REPT-TBL-09-NBR,           00253700
253800                  :DCLXXXAIL-LOC.ROLUP-REPT-TBL-10-NBR,           00253800
253900                  :DCLXXXAIL-LOC.ONLIN-SSON-SW,                   00253900
254000                  :DCLXXXAIL-LOC.RPLACD-BY-STR-NBR                00254000
254010                  :DCLXXXAIL-LOC-IND.RPLACD-BY-STR-NBR-IND        00254010
254100           FROM   XXXAIL_LOC                                      00254100
254200           WHERE  LOC_NBR = :DCLXXXAIL-LOC.LOC-NBR                00254200
254300           AND    LOC_TYP_CD = :DCLXXXAIL-LOC.LOC-TYP-CD          00254300
254400       END-EXEC                                                   00254400
254500                                                                  00254500
254600     PERFORM 1700-CHECK-NULL-COLUMNS                              00254600
254700     .                                                            00254700
254800                                                                  00254800
254900                                                                  00254900
255000 1300-EXIT-GET-NEXT-ROW.                                          00255000
255100     EVALUATE TRUE                                                00255100
255200       WHEN DDDXLR01                                              00255200
255300         PERFORM 1301-FETCH-DDDXLR01                              00255300
255400       WHEN DDDXLR02                                              00255400
255500         PERFORM 1302-FETCH-DDDXLR02                              00255500
255600       WHEN DDDXLR03                                              00255600
255700         PERFORM 1303-FETCH-DDDXLR03                              00255700
255800       WHEN DDDXLR04                                              00255800
255900         PERFORM 1304-FETCH-DDDXLR04                              00255900
256000       WHEN DDDXLR05                                              00256000
256100         PERFORM 1305-FETCH-DDDXLR05                              00256100
256200       WHEN DDDXLR06                                              00256200
256300         PERFORM 1306-FETCH-DDDXLR06                              00256300
256400       WHEN DDDXLR07                                              00256400
256500         PERFORM 1307-FETCH-DDDXLR07                              00256500
256600       WHEN DDDXLR09                                              00256600
256700         PERFORM 1309-FETCH-DDDXLR09                              00256700
256800       WHEN OTHER                                                 00256800
256900         SET FAILURE TO TRUE                                      00256900
257000         MOVE 'NNNS0488 - Invalid fetch cursor ID.'               00257000
257100           TO IS-RTRN-MSG-TXT OF XXXN001A                         00257100
257200     END-EVALUATE                                                 00257200
257300                                                                  00257300
257400     PERFORM 1700-CHECK-NULL-COLUMNS                              00257400
257500     .                                                            00257500
257600                                                                  00257600
257700                                                                  00257700
257800 1301-FETCH-DDDXLR01.                                             00257800
257900     EXEC SQL                                                     00257900
258000         FETCH DDDXLR01                                           00258000
258100         INTO  :DCLXXXAIL-LOC.LOC-NBR,                            00258100
258200               :DCLXXXAIL-LOC.LOC-TYP-CD,                         00258200
258300               :DCLXXXAIL-LOC.ASSOC-STR-TYP-CD                    00258300
258400               :WS-ASSOC-ST-TYPE-IND,                             00258400
258500               :DCLXXXAIL-LOC.ASSOC-STR-NBR                       00258500
258600               :WS-ASSOC-ST-NO-IND,                               00258600
258700               :DCLXXXAIL-LOC.STR-REMODL-DT,                      00258700
258800               :DCLXXXAIL-LOC.RETL-LOC-STAT-CD,                   00258800
258900               :DCLXXXAIL-LOC.RETL-LOC-STAT-DT,                   00258900
259000               :DCLXXXAIL-LOC.COMPANY-ID,                         00259000
259100               :DCLXXXAIL-LOC.FINANCIAL-DIV-ID,                   00259100
259200               :DCLXXXAIL-LOC.LIN-OF-BUS-ID,                      00259200
259300               :DCLXXXAIL-LOC.DIST-ID,                            00259300
259400               :DCLXXXAIL-LOC.MKT-RGN-ID,                         00259400
259500               :DCLXXXAIL-LOC.GEO-ZN-CD,                          00259500
259600               :DCLXXXAIL-LOC.RETL-GEO-ZN-ID,                     00259600
259700               :DCLXXXAIL-LOC.SCN-MAINT-SW,                       00259700
259800               :DCLXXXAIL-LOC.FRNT-END-CD,                        00259800
259900               :DCLXXXAIL-LOC.PRC-BUL-SW,                         00259900
260000               :DCLXXXAIL-LOC.UPC-ON-PRC-BUL-SW,                  00260000
260100               :DCLXXXAIL-LOC.CMPTR-TYP-CD,                       00260100
260200               :DCLXXXAIL-LOC.RETL-VID-ZN-NBR,                    00260200
260300               :DCLXXXAIL-LOC.RETL-UNLD-CD,                       00260300
260400               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-TXT,                 00260400
260500               :DCLXXXAIL-LOC.NEW-STR-SW,                         00260500
260600               :DCLXXXAIL-LOC.SEL-CIR-SW,                         00260600
260700               :DCLXXXAIL-LOC.BKRM-SQ-FT,                         00260700
260800               :DCLXXXAIL-LOC.FD-LINER-FT,                        00260800
260900               :DCLXXXAIL-LOC.NON-FD-LINER-FT,                    00260900
261000               :DCLXXXAIL-LOC.SETOFF-ROOM-SW,                     00261000
261100               :DCLXXXAIL-LOC.CAT-CLS-TBL-TXT,                    00261100
261200               :DCLXXXAIL-LOC.LAT-K,                              00261200
261300               :DCLXXXAIL-LOC.LON-K,                              00261300
261400               :DCLXXXAIL-LOC.CK-COLL-REPT-SW,                    00261400
261500               :DCLXXXAIL-LOC.CK-COLL-CNTL-CD,                    00261500
261600               :DCLXXXAIL-LOC.CK-COLL-ADD-DEL-SW,                 00261600
261700               :DCLXXXAIL-LOC.CK-ALT-STR-ID,                      00261700
261800               :DCLXXXAIL-LOC.CK-COLL-FEE-AMT,                    00261800
261900               :DCLXXXAIL-LOC.SALS-TAX-PCT,                       00261900
262000               :DCLXXXAIL-LOC.SOAP-SALE-VAR-PCT,                  00262000
262100               :DCLXXXAIL-LOC.ON-SRS-CD,                          00262100
262200               :DCLXXXAIL-LOC.SRS-DSD-ORD-SW,                     00262200
262300               :DCLXXXAIL-LOC.RETL-LOC-TYP-CD,                    00262300
262400               :DCLXXXAIL-LOC.DEA-NBR,                            00262400
262500               :DCLXXXAIL-LOC.STR-OPSTMT-SRT-CD,                  00262500
262600               :DCLXXXAIL-LOC.STR-OPSTMT-TYP-CD,                  00262600
262700               :DCLXXXAIL-LOC.STR-OPSTMT-HDR-CD,                  00262700
262800               :DCLXXXAIL-LOC.DPS-NBR,                            00262800
262900               :DCLXXXAIL-LOC.MEDICARE-ID,                        00262900
263000               :DCLXXXAIL-LOC.NABP-NBR,                           00263000
263100               :DCLXXXAIL-LOC.NATL-PROV-ID,                       00263100
263200               :DCLXXXAIL-LOC.CURR-AD-ZN-NBR,                     00263200
263300               :DCLXXXAIL-LOC.PD-ZONE-NO,                         00263300
263400               :DCLXXXAIL-LOC.SOS-PROC-SW,                        00263400
263500               :DCLXXXAIL-LOC.RPRT-SEQ-NBR,                       00263500
263600               :DCLXXXAIL-LOC.GRP-CD,                             00263600
263700               :DCLXXXAIL-LOC.PRIM-GRP-CD-1,                      00263700
263800               :DCLXXXAIL-LOC.PRIM-GRP-CD-2,                      00263800
263900               :DCLXXXAIL-LOC.SECY-GRP-CD-1,                      00263900
264000               :DCLXXXAIL-LOC.SECY-GRP-CD-2,                      00264000
264100               :DCLXXXAIL-LOC.PRIM-CLS-NBR-1,                     00264100
264200               :DCLXXXAIL-LOC.PRIM-CLS-NBR-2,                     00264200
264300               :DCLXXXAIL-LOC.SECY-CLS-NBR-1,                     00264300
264400               :DCLXXXAIL-LOC.SECY-CLS-NBR-2,                     00264400
264500               :DCLXXXAIL-LOC.VAL-STR-SW,                         00264500
264600               :DCLXXXAIL-LOC.SLS-CLOSED-DT,                      00264600
264700               :DCLXXXAIL-LOC.TBCO-PRMT-NBR,                      00264700
264800               :DCLXXXAIL-LOC.SUB-UNLIKE-PROD-CD,                 00264800
264900               :DCLXXXAIL-LOC.SUB-DSPLY-PAL-CD,                   00264900
265000               :DCLXXXAIL-LOC.RLTM-SCN-MAINT-SW,                  00265000
265100               :DCLXXXAIL-LOC.TOP-LEADER-NM,                      00265100
265200               :DCLXXXAIL-LOC.CUST-FRNDLY-NM,                     00265200
265300               :DCLXXXAIL-LOC.SLS-OPEN-DT,                        00265300
265400               :WS-MON-OPEN-TS,                                   00265400
265500               :WS-MON-CLOS-TS,                                   00265500
265600               :WS-TUE-OPEN-TS,                                   00265600
265700               :WS-TUE-CLOS-TS,                                   00265700
265800               :WS-WED-OPEN-TS,                                   00265800
265900               :WS-WED-CLOS-TS,                                   00265900
266000               :WS-THUR-OPEN-TS,                                  00266000
266100               :WS-THUR-CLOS-TS,                                  00266100
266200               :WS-FRI-OPEN-TS ,                                  00266200
266300               :WS-FRI-CLOS-TS,                                   00266300
266400               :WS-SAT-OPEN-TS,                                   00266400
266500               :WS-SUN-OPEN-TS,                                   00266500
266600               :WS-SAT-CLOS-TS,                                   00266600
266700               :WS-SUN-CLOS-TS,                                   00266700
266800               :DCLXXXAIL-LOC.RETL-LOC-FRMAT-CD,                  00266800
266900               :DCLXXXAIL-LOC.RETL-LOC-SEGM-CD,                   00266900
267000               :DCLXXXAIL-LOC.ECOMM-MKT-AREA-CD,                  00267000
267100               :DCLXXXAIL-LOC.ECOMM-STRT-DT                       00267100
267200               :DCLXXXAIL-LOC-IND.ECOMM-STRT-DT-IND,              00267200
267300               :DCLXXXAIL-LOC.ECOMM-END-DT                        00267300
267400               :DCLXXXAIL-LOC-IND.ECOMM-END-DT-IND,               00267400
267500               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-01-NBR,              00267500
267600               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-02-NBR,              00267600
267700               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-03-NBR,              00267700
267800               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-04-NBR,              00267800
267900               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-05-NBR,              00267900
268000               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-06-NBR,              00268000
268100               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-07-NBR,              00268100
268200               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-08-NBR,              00268200
268300               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-09-NBR,              00268300
268400               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-10-NBR,              00268400
268500               :DCLXXXAIL-LOC.ONLIN-SSON-SW,                      00268500
268600               :DCLXXXAIL-LOC.RPLACD-BY-STR-NBR                   00268600
268610               :DCLXXXAIL-LOC-IND.RPLACD-BY-STR-NBR-IND           00268610
268700     END-EXEC                                                     00268700
268800     .                                                            00268800
268900                                                                  00268900
269000                                                                  00269000
269100 1302-FETCH-DDDXLR02.                                             00269100
269200     EXEC SQL                                                     00269200
269300         FETCH DDDXLR02                                           00269300
269400         INTO  :DCLXXXAIL-LOC.LOC-NBR,                            00269400
269500               :DCLXXXAIL-LOC.LOC-TYP-CD,                         00269500
269600               :DCLXXXAIL-LOC.ASSOC-STR-TYP-CD                    00269600
269700               :WS-ASSOC-ST-TYPE-IND,                             00269700
269800               :DCLXXXAIL-LOC.ASSOC-STR-NBR                       00269800
269900               :WS-ASSOC-ST-NO-IND,                               00269900
270000               :DCLXXXAIL-LOC.STR-REMODL-DT,                      00270000
270100               :DCLXXXAIL-LOC.RETL-LOC-STAT-CD,                   00270100
270200               :DCLXXXAIL-LOC.RETL-LOC-STAT-DT,                   00270200
270300               :DCLXXXAIL-LOC.COMPANY-ID,                         00270300
270400               :DCLXXXAIL-LOC.FINANCIAL-DIV-ID,                   00270400
270500               :DCLXXXAIL-LOC.LIN-OF-BUS-ID,                      00270500
270600               :DCLXXXAIL-LOC.DIST-ID,                            00270600
270700               :DCLXXXAIL-LOC.MKT-RGN-ID,                         00270700
270800               :DCLXXXAIL-LOC.GEO-ZN-CD,                          00270800
270900               :DCLXXXAIL-LOC.RETL-GEO-ZN-ID,                     00270900
271000               :DCLXXXAIL-LOC.SCN-MAINT-SW,                       00271000
271100               :DCLXXXAIL-LOC.FRNT-END-CD,                        00271100
271200               :DCLXXXAIL-LOC.PRC-BUL-SW,                         00271200
271300               :DCLXXXAIL-LOC.UPC-ON-PRC-BUL-SW,                  00271300
271400               :DCLXXXAIL-LOC.CMPTR-TYP-CD,                       00271400
271500               :DCLXXXAIL-LOC.RETL-VID-ZN-NBR,                    00271500
271600               :DCLXXXAIL-LOC.RETL-UNLD-CD,                       00271600
271700               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-TXT,                 00271700
271800               :DCLXXXAIL-LOC.NEW-STR-SW,                         00271800
271900               :DCLXXXAIL-LOC.SEL-CIR-SW,                         00271900
272000               :DCLXXXAIL-LOC.BKRM-SQ-FT,                         00272000
272100               :DCLXXXAIL-LOC.FD-LINER-FT,                        00272100
272200               :DCLXXXAIL-LOC.NON-FD-LINER-FT,                    00272200
272300               :DCLXXXAIL-LOC.SETOFF-ROOM-SW,                     00272300
272400               :DCLXXXAIL-LOC.CAT-CLS-TBL-TXT,                    00272400
272500               :DCLXXXAIL-LOC.LAT-K,                              00272500
272600               :DCLXXXAIL-LOC.LON-K,                              00272600
272700               :DCLXXXAIL-LOC.CK-COLL-REPT-SW,                    00272700
272800               :DCLXXXAIL-LOC.CK-COLL-CNTL-CD,                    00272800
272900               :DCLXXXAIL-LOC.CK-COLL-ADD-DEL-SW,                 00272900
273000               :DCLXXXAIL-LOC.CK-ALT-STR-ID,                      00273000
273100               :DCLXXXAIL-LOC.CK-COLL-FEE-AMT,                    00273100
273200               :DCLXXXAIL-LOC.SALS-TAX-PCT,                       00273200
273300               :DCLXXXAIL-LOC.SOAP-SALE-VAR-PCT,                  00273300
273400               :DCLXXXAIL-LOC.ON-SRS-CD,                          00273400
273500               :DCLXXXAIL-LOC.SRS-DSD-ORD-SW,                     00273500
273600               :DCLXXXAIL-LOC.RETL-LOC-TYP-CD,                    00273600
273700               :DCLXXXAIL-LOC.DEA-NBR,                            00273700
273800               :DCLXXXAIL-LOC.STR-OPSTMT-SRT-CD,                  00273800
273900               :DCLXXXAIL-LOC.STR-OPSTMT-TYP-CD,                  00273900
274000               :DCLXXXAIL-LOC.STR-OPSTMT-HDR-CD,                  00274000
274100               :DCLXXXAIL-LOC.DPS-NBR,                            00274100
274200               :DCLXXXAIL-LOC.MEDICARE-ID,                        00274200
274300               :DCLXXXAIL-LOC.NABP-NBR,                           00274300
274400               :DCLXXXAIL-LOC.NATL-PROV-ID,                       00274400
274500               :DCLXXXAIL-LOC.CURR-AD-ZN-NBR,                     00274500
274600               :DCLXXXAIL-LOC.PD-ZONE-NO,                         00274600
274700               :DCLXXXAIL-LOC.SOS-PROC-SW,                        00274700
274800               :DCLXXXAIL-LOC.RPRT-SEQ-NBR,                       00274800
274900               :DCLXXXAIL-LOC.GRP-CD,                             00274900
275000               :DCLXXXAIL-LOC.PRIM-GRP-CD-1,                      00275000
275100               :DCLXXXAIL-LOC.PRIM-GRP-CD-2,                      00275100
275200               :DCLXXXAIL-LOC.SECY-GRP-CD-1,                      00275200
275300               :DCLXXXAIL-LOC.SECY-GRP-CD-2,                      00275300
275400               :DCLXXXAIL-LOC.PRIM-CLS-NBR-1,                     00275400
275500               :DCLXXXAIL-LOC.PRIM-CLS-NBR-2,                     00275500
275600               :DCLXXXAIL-LOC.SECY-CLS-NBR-1,                     00275600
275700               :DCLXXXAIL-LOC.SECY-CLS-NBR-2,                     00275700
275800               :DCLXXXAIL-LOC.VAL-STR-SW ,                        00275800
275900               :DCLXXXAIL-LOC.SLS-CLOSED-DT,                      00275900
276000               :DCLXXXAIL-LOC.TBCO-PRMT-NBR,                      00276000
276100               :DCLXXXAIL-LOC.SUB-UNLIKE-PROD-CD,                 00276100
276200               :DCLXXXAIL-LOC.SUB-DSPLY-PAL-CD,                   00276200
276300               :DCLXXXAIL-LOC.RLTM-SCN-MAINT-SW,                  00276300
276400               :DCLXXXAIL-LOC.TOP-LEADER-NM,                      00276400
276500               :DCLXXXAIL-LOC.CUST-FRNDLY-NM,                     00276500
276600               :DCLXXXAIL-LOC.SLS-OPEN-DT,                        00276600
276700               :WS-MON-OPEN-TS,                                   00276700
276800               :WS-MON-CLOS-TS,                                   00276800
276900               :WS-TUE-OPEN-TS,                                   00276900
277000               :WS-TUE-CLOS-TS,                                   00277000
277100               :WS-WED-OPEN-TS,                                   00277100
277200               :WS-WED-CLOS-TS,                                   00277200
277300               :WS-THUR-OPEN-TS,                                  00277300
277400               :WS-THUR-CLOS-TS,                                  00277400
277500               :WS-FRI-OPEN-TS ,                                  00277500
277600               :WS-FRI-CLOS-TS,                                   00277600
277700               :WS-SAT-OPEN-TS,                                   00277700
277800               :WS-SUN-OPEN-TS,                                   00277800
277900               :WS-SAT-CLOS-TS,                                   00277900
278000               :WS-SUN-CLOS-TS,                                   00278000
278100               :DCLXXXAIL-LOC.RETL-LOC-FRMAT-CD,                  00278100
278200               :DCLXXXAIL-LOC.RETL-LOC-SEGM-CD,                   00278200
278300               :DCLXXXAIL-LOC.ECOMM-MKT-AREA-CD,                  00278300
278400               :DCLXXXAIL-LOC.ECOMM-STRT-DT                       00278400
278500               :DCLXXXAIL-LOC-IND.ECOMM-STRT-DT-IND,              00278500
278600               :DCLXXXAIL-LOC.ECOMM-END-DT                        00278600
278700               :DCLXXXAIL-LOC-IND.ECOMM-END-DT-IND,               00278700
278800               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-01-NBR,              00278800
278900               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-02-NBR,              00278900
279000               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-03-NBR,              00279000
279100               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-04-NBR,              00279100
279200               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-05-NBR,              00279200
279300               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-06-NBR,              00279300
279400               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-07-NBR,              00279400
279500               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-08-NBR,              00279500
279600               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-09-NBR,              00279600
279700               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-10-NBR,              00279700
279800               :DCLXXXAIL-LOC.ONLIN-SSON-SW,                      00279800
279900               :DCLXXXAIL-LOC.RPLACD-BY-STR-NBR                   00279900
279910               :DCLXXXAIL-LOC-IND.RPLACD-BY-STR-NBR-IND           00279910
280000     END-EXEC                                                     00280000
280100     .                                                            00280100
280200                                                                  00280200
280300                                                                  00280300
280400 1303-FETCH-DDDXLR03.                                             00280400
280500     EXEC SQL                                                     00280500
280600         FETCH DDDXLR03                                           00280600
280700         INTO  :DCLXXXAIL-LOC.LOC-NBR,                            00280700
280800               :DCLXXXAIL-LOC.LOC-TYP-CD,                         00280800
280900               :DCLXXXAIL-LOC.ASSOC-STR-TYP-CD                    00280900
281000               :WS-ASSOC-ST-TYPE-IND,                             00281000
281100               :DCLXXXAIL-LOC.ASSOC-STR-NBR                       00281100
281200               :WS-ASSOC-ST-NO-IND,                               00281200
281300               :DCLXXXAIL-LOC.STR-REMODL-DT,                      00281300
281400               :DCLXXXAIL-LOC.RETL-LOC-STAT-CD,                   00281400
281500               :DCLXXXAIL-LOC.RETL-LOC-STAT-DT,                   00281500
281600               :DCLXXXAIL-LOC.COMPANY-ID,                         00281600
281700               :DCLXXXAIL-LOC.FINANCIAL-DIV-ID,                   00281700
281800               :DCLXXXAIL-LOC.LIN-OF-BUS-ID,                      00281800
281900               :DCLXXXAIL-LOC.DIST-ID,                            00281900
282000               :DCLXXXAIL-LOC.MKT-RGN-ID,                         00282000
282100               :DCLXXXAIL-LOC.GEO-ZN-CD,                          00282100
282200               :DCLXXXAIL-LOC.RETL-GEO-ZN-ID,                     00282200
282300               :DCLXXXAIL-LOC.SCN-MAINT-SW,                       00282300
282400               :DCLXXXAIL-LOC.FRNT-END-CD,                        00282400
282500               :DCLXXXAIL-LOC.PRC-BUL-SW,                         00282500
282600               :DCLXXXAIL-LOC.UPC-ON-PRC-BUL-SW,                  00282600
282700               :DCLXXXAIL-LOC.CMPTR-TYP-CD,                       00282700
282800               :DCLXXXAIL-LOC.RETL-VID-ZN-NBR,                    00282800
282900               :DCLXXXAIL-LOC.RETL-UNLD-CD,                       00282900
283000               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-TXT,                 00283000
283100               :DCLXXXAIL-LOC.NEW-STR-SW,                         00283100
283200               :DCLXXXAIL-LOC.SEL-CIR-SW,                         00283200
283300               :DCLXXXAIL-LOC.BKRM-SQ-FT,                         00283300
283400               :DCLXXXAIL-LOC.FD-LINER-FT,                        00283400
283500               :DCLXXXAIL-LOC.NON-FD-LINER-FT,                    00283500
283600               :DCLXXXAIL-LOC.SETOFF-ROOM-SW,                     00283600
283700               :DCLXXXAIL-LOC.CAT-CLS-TBL-TXT,                    00283700
283800               :DCLXXXAIL-LOC.LAT-K,                              00283800
283900               :DCLXXXAIL-LOC.LON-K,                              00283900
284000               :DCLXXXAIL-LOC.CK-COLL-REPT-SW,                    00284000
284100               :DCLXXXAIL-LOC.CK-COLL-CNTL-CD,                    00284100
284200               :DCLXXXAIL-LOC.CK-COLL-ADD-DEL-SW,                 00284200
284300               :DCLXXXAIL-LOC.CK-ALT-STR-ID,                      00284300
284400               :DCLXXXAIL-LOC.CK-COLL-FEE-AMT,                    00284400
284500               :DCLXXXAIL-LOC.SALS-TAX-PCT,                       00284500
284600               :DCLXXXAIL-LOC.SOAP-SALE-VAR-PCT,                  00284600
284700               :DCLXXXAIL-LOC.ON-SRS-CD,                          00284700
284800               :DCLXXXAIL-LOC.SRS-DSD-ORD-SW,                     00284800
284900               :DCLXXXAIL-LOC.RETL-LOC-TYP-CD,                    00284900
285000               :DCLXXXAIL-LOC.DEA-NBR,                            00285000
285100               :DCLXXXAIL-LOC.STR-OPSTMT-SRT-CD,                  00285100
285200               :DCLXXXAIL-LOC.STR-OPSTMT-TYP-CD,                  00285200
285300               :DCLXXXAIL-LOC.STR-OPSTMT-HDR-CD,                  00285300
285400               :DCLXXXAIL-LOC.DPS-NBR,                            00285400
285500               :DCLXXXAIL-LOC.MEDICARE-ID,                        00285500
285600               :DCLXXXAIL-LOC.NABP-NBR,                           00285600
285700               :DCLXXXAIL-LOC.NATL-PROV-ID,                       00285700
285800               :DCLXXXAIL-LOC.CURR-AD-ZN-NBR,                     00285800
285900               :DCLXXXAIL-LOC.PD-ZONE-NO,                         00285900
286000               :DCLXXXAIL-LOC.SOS-PROC-SW,                        00286000
286100               :DCLXXXAIL-LOC.RPRT-SEQ-NBR,                       00286100
286200               :DCLXXXAIL-LOC.GRP-CD,                             00286200
286300               :DCLXXXAIL-LOC.PRIM-GRP-CD-1,                      00286300
286400               :DCLXXXAIL-LOC.PRIM-GRP-CD-2,                      00286400
286500               :DCLXXXAIL-LOC.SECY-GRP-CD-1,                      00286500
286600               :DCLXXXAIL-LOC.SECY-GRP-CD-2,                      00286600
286700               :DCLXXXAIL-LOC.PRIM-CLS-NBR-1,                     00286700
286800               :DCLXXXAIL-LOC.PRIM-CLS-NBR-2,                     00286800
286900               :DCLXXXAIL-LOC.SECY-CLS-NBR-1,                     00286900
287000               :DCLXXXAIL-LOC.SECY-CLS-NBR-2,                     00287000
287100               :DCLXXXAIL-LOC.VAL-STR-SW ,                        00287100
287200               :DCLXXXAIL-LOC.SLS-CLOSED-DT,                      00287200
287300               :DCLXXXAIL-LOC.TBCO-PRMT-NBR,                      00287300
287400               :DCLXXXAIL-LOC.SUB-UNLIKE-PROD-CD,                 00287400
287500               :DCLXXXAIL-LOC.SUB-DSPLY-PAL-CD,                   00287500
287600               :DCLXXXAIL-LOC.RLTM-SCN-MAINT-SW,                  00287600
287700               :DCLXXXAIL-LOC.TOP-LEADER-NM,                      00287700
287800               :DCLXXXAIL-LOC.CUST-FRNDLY-NM,                     00287800
287900               :DCLXXXAIL-LOC.SLS-OPEN-DT,                        00287900
288000               :WS-MON-OPEN-TS,                                   00288000
288100               :WS-MON-CLOS-TS,                                   00288100
288200               :WS-TUE-OPEN-TS,                                   00288200
288300               :WS-TUE-CLOS-TS,                                   00288300
288400               :WS-WED-OPEN-TS,                                   00288400
288500               :WS-WED-CLOS-TS,                                   00288500
288600               :WS-THUR-OPEN-TS,                                  00288600
288700               :WS-THUR-CLOS-TS,                                  00288700
288800               :WS-FRI-OPEN-TS ,                                  00288800
288900               :WS-FRI-CLOS-TS,                                   00288900
289000               :WS-SAT-OPEN-TS,                                   00289000
289100               :WS-SUN-OPEN-TS,                                   00289100
289200               :WS-SAT-CLOS-TS,                                   00289200
289300               :WS-SUN-CLOS-TS,                                   00289300
289400               :DCLXXXAIL-LOC.RETL-LOC-FRMAT-CD,                  00289400
289500               :DCLXXXAIL-LOC.RETL-LOC-SEGM-CD,                   00289500
289600               :DCLXXXAIL-LOC.ECOMM-MKT-AREA-CD,                  00289600
289700               :DCLXXXAIL-LOC.ECOMM-STRT-DT                       00289700
289800               :DCLXXXAIL-LOC-IND.ECOMM-STRT-DT-IND,              00289800
289900               :DCLXXXAIL-LOC.ECOMM-END-DT                        00289900
290000               :DCLXXXAIL-LOC-IND.ECOMM-END-DT-IND,               00290000
290100               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-01-NBR,              00290100
290200               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-02-NBR,              00290200
290300               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-03-NBR,              00290300
290400               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-04-NBR,              00290400
290500               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-05-NBR,              00290500
290600               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-06-NBR,              00290600
290700               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-07-NBR,              00290700
290800               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-08-NBR,              00290800
290900               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-09-NBR,              00290900
291000               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-10-NBR,              00291000
291100               :DCLXXXAIL-LOC.ONLIN-SSON-SW,                      00291100
291200               :DCLXXXAIL-LOC.RPLACD-BY-STR-NBR                   00291200
291210               :DCLXXXAIL-LOC-IND.RPLACD-BY-STR-NBR-IND           00291210
291300     END-EXEC                                                     00291300
291400     .                                                            00291400
291500                                                                  00291500
291600                                                                  00291600
291700 1304-FETCH-DDDXLR04.                                             00291700
291800     EXEC SQL                                                     00291800
291900         FETCH DDDXLR04                                           00291900
292000         INTO  :DCLXXXAIL-LOC.LOC-NBR,                            00292000
292100               :DCLXXXAIL-LOC.LOC-TYP-CD,                         00292100
292200               :DCLXXXAIL-LOC.ASSOC-STR-TYP-CD                    00292200
292300               :WS-ASSOC-ST-TYPE-IND,                             00292300
292400               :DCLXXXAIL-LOC.ASSOC-STR-NBR                       00292400
292500               :WS-ASSOC-ST-NO-IND,                               00292500
292600               :DCLXXXAIL-LOC.STR-REMODL-DT,                      00292600
292700               :DCLXXXAIL-LOC.RETL-LOC-STAT-CD,                   00292700
292800               :DCLXXXAIL-LOC.RETL-LOC-STAT-DT,                   00292800
292900               :DCLXXXAIL-LOC.COMPANY-ID,                         00292900
293000               :DCLXXXAIL-LOC.FINANCIAL-DIV-ID,                   00293000
293100               :DCLXXXAIL-LOC.LIN-OF-BUS-ID,                      00293100
293200               :DCLXXXAIL-LOC.DIST-ID,                            00293200
293300               :DCLXXXAIL-LOC.MKT-RGN-ID,                         00293300
293400               :DCLXXXAIL-LOC.GEO-ZN-CD,                          00293400
293500               :DCLXXXAIL-LOC.RETL-GEO-ZN-ID,                     00293500
293600               :DCLXXXAIL-LOC.SCN-MAINT-SW,                       00293600
293700               :DCLXXXAIL-LOC.FRNT-END-CD,                        00293700
293800               :DCLXXXAIL-LOC.PRC-BUL-SW,                         00293800
293900               :DCLXXXAIL-LOC.UPC-ON-PRC-BUL-SW,                  00293900
294000               :DCLXXXAIL-LOC.CMPTR-TYP-CD,                       00294000
294100               :DCLXXXAIL-LOC.RETL-VID-ZN-NBR,                    00294100
294200               :DCLXXXAIL-LOC.RETL-UNLD-CD,                       00294200
294300               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-TXT,                 00294300
294400               :DCLXXXAIL-LOC.NEW-STR-SW,                         00294400
294500               :DCLXXXAIL-LOC.SEL-CIR-SW,                         00294500
294600               :DCLXXXAIL-LOC.BKRM-SQ-FT,                         00294600
294700               :DCLXXXAIL-LOC.FD-LINER-FT,                        00294700
294800               :DCLXXXAIL-LOC.NON-FD-LINER-FT,                    00294800
294900               :DCLXXXAIL-LOC.SETOFF-ROOM-SW,                     00294900
295000               :DCLXXXAIL-LOC.CAT-CLS-TBL-TXT,                    00295000
295100               :DCLXXXAIL-LOC.LAT-K,                              00295100
295200               :DCLXXXAIL-LOC.LON-K,                              00295200
295300               :DCLXXXAIL-LOC.CK-COLL-REPT-SW,                    00295300
295400               :DCLXXXAIL-LOC.CK-COLL-CNTL-CD,                    00295400
295500               :DCLXXXAIL-LOC.CK-COLL-ADD-DEL-SW,                 00295500
295600               :DCLXXXAIL-LOC.CK-ALT-STR-ID,                      00295600
295700               :DCLXXXAIL-LOC.CK-COLL-FEE-AMT,                    00295700
295800               :DCLXXXAIL-LOC.SALS-TAX-PCT,                       00295800
295900               :DCLXXXAIL-LOC.SOAP-SALE-VAR-PCT,                  00295900
296000               :DCLXXXAIL-LOC.ON-SRS-CD,                          00296000
296100               :DCLXXXAIL-LOC.SRS-DSD-ORD-SW,                     00296100
296200               :DCLXXXAIL-LOC.RETL-LOC-TYP-CD,                    00296200
296300               :DCLXXXAIL-LOC.DEA-NBR,                            00296300
296400               :DCLXXXAIL-LOC.STR-OPSTMT-SRT-CD,                  00296400
296500               :DCLXXXAIL-LOC.STR-OPSTMT-TYP-CD,                  00296500
296600               :DCLXXXAIL-LOC.STR-OPSTMT-HDR-CD,                  00296600
296700               :DCLXXXAIL-LOC.DPS-NBR,                            00296700
296800               :DCLXXXAIL-LOC.MEDICARE-ID,                        00296800
296900               :DCLXXXAIL-LOC.NABP-NBR,                           00296900
297000               :DCLXXXAIL-LOC.NATL-PROV-ID,                       00297000
297100               :DCLXXXAIL-LOC.CURR-AD-ZN-NBR,                     00297100
297200               :DCLXXXAIL-LOC.PD-ZONE-NO,                         00297200
297300               :DCLXXXAIL-LOC.SOS-PROC-SW,                        00297300
297400               :DCLXXXAIL-LOC.RPRT-SEQ-NBR,                       00297400
297500               :DCLXXXAIL-LOC.GRP-CD,                             00297500
297600               :DCLXXXAIL-LOC.PRIM-GRP-CD-1,                      00297600
297700               :DCLXXXAIL-LOC.PRIM-GRP-CD-2,                      00297700
297800               :DCLXXXAIL-LOC.SECY-GRP-CD-1,                      00297800
297900               :DCLXXXAIL-LOC.SECY-GRP-CD-2,                      00297900
298000               :DCLXXXAIL-LOC.PRIM-CLS-NBR-1,                     00298000
298100               :DCLXXXAIL-LOC.PRIM-CLS-NBR-2,                     00298100
298200               :DCLXXXAIL-LOC.SECY-CLS-NBR-1,                     00298200
298300               :DCLXXXAIL-LOC.SECY-CLS-NBR-2,                     00298300
298400               :DCLXXXAIL-LOC.VAL-STR-SW,                         00298400
298500               :DCLXXXAIL-LOC.SLS-CLOSED-DT,                      00298500
298600               :DCLXXXAIL-LOC.TBCO-PRMT-NBR,                      00298600
298700               :DCLXXXAIL-LOC.SUB-UNLIKE-PROD-CD,                 00298700
298800               :DCLXXXAIL-LOC.SUB-DSPLY-PAL-CD,                   00298800
298900               :DCLXXXAIL-LOC.RLTM-SCN-MAINT-SW,                  00298900
299000               :DCLXXXAIL-LOC.TOP-LEADER-NM,                      00299000
299100               :DCLXXXAIL-LOC.CUST-FRNDLY-NM,                     00299100
299200               :DCLXXXAIL-LOC.SLS-OPEN-DT,                        00299200
299300               :WS-MON-OPEN-TS,                                   00299300
299400               :WS-MON-CLOS-TS,                                   00299400
299500               :WS-TUE-OPEN-TS,                                   00299500
299600               :WS-TUE-CLOS-TS,                                   00299600
299700               :WS-WED-OPEN-TS,                                   00299700
299800               :WS-WED-CLOS-TS,                                   00299800
299900               :WS-THUR-OPEN-TS,                                  00299900
300000               :WS-THUR-CLOS-TS,                                  00300000
300100               :WS-FRI-OPEN-TS ,                                  00300100
300200               :WS-FRI-CLOS-TS,                                   00300200
300300               :WS-SAT-OPEN-TS,                                   00300300
300400               :WS-SUN-OPEN-TS,                                   00300400
300500               :WS-SAT-CLOS-TS,                                   00300500
300600               :WS-SUN-CLOS-TS,                                   00300600
300700               :DCLXXXAIL-LOC.RETL-LOC-FRMAT-CD,                  00300700
300800               :DCLXXXAIL-LOC.RETL-LOC-SEGM-CD,                   00300800
300900               :DCLXXXAIL-LOC.ECOMM-MKT-AREA-CD,                  00300900
301000               :DCLXXXAIL-LOC.ECOMM-STRT-DT                       00301000
301100               :DCLXXXAIL-LOC-IND.ECOMM-STRT-DT-IND,              00301100
301200               :DCLXXXAIL-LOC.ECOMM-END-DT                        00301200
301300               :DCLXXXAIL-LOC-IND.ECOMM-END-DT-IND,               00301300
301400               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-01-NBR,              00301400
301500               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-02-NBR,              00301500
301600               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-03-NBR,              00301600
301700               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-04-NBR,              00301700
301800               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-05-NBR,              00301800
301900               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-06-NBR,              00301900
302000               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-07-NBR,              00302000
302100               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-08-NBR,              00302100
302200               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-09-NBR,              00302200
302300               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-10-NBR,              00302300
302400               :DCLXXXAIL-LOC.ONLIN-SSON-SW,                      00302400
302500               :DCLXXXAIL-LOC.RPLACD-BY-STR-NBR                   00302500
302510               :DCLXXXAIL-LOC-IND.RPLACD-BY-STR-NBR-IND           00302510
302600     END-EXEC                                                     00302600
302700     .                                                            00302700
302800                                                                  00302800
302900                                                                  00302900
303000 1305-FETCH-DDDXLR05.                                             00303000
303100     EXEC SQL                                                     00303100
303200         FETCH DDDXLR05                                           00303200
303300         INTO  :DCLXXXAIL-LOC.LOC-NBR,                            00303300
303400               :DCLXXXAIL-LOC.LOC-TYP-CD,                         00303400
303500               :DCLXXXAIL-LOC.ASSOC-STR-TYP-CD                    00303500
303600               :WS-ASSOC-ST-TYPE-IND,                             00303600
303700               :DCLXXXAIL-LOC.ASSOC-STR-NBR                       00303700
303800               :WS-ASSOC-ST-NO-IND,                               00303800
303900               :DCLXXXAIL-LOC.STR-REMODL-DT,                      00303900
304000               :DCLXXXAIL-LOC.RETL-LOC-STAT-CD,                   00304000
304100               :DCLXXXAIL-LOC.RETL-LOC-STAT-DT,                   00304100
304200               :DCLXXXAIL-LOC.COMPANY-ID,                         00304200
304300               :DCLXXXAIL-LOC.FINANCIAL-DIV-ID,                   00304300
304400               :DCLXXXAIL-LOC.LIN-OF-BUS-ID,                      00304400
304500               :DCLXXXAIL-LOC.DIST-ID,                            00304500
304600               :DCLXXXAIL-LOC.MKT-RGN-ID,                         00304600
304700               :DCLXXXAIL-LOC.GEO-ZN-CD,                          00304700
304800               :DCLXXXAIL-LOC.RETL-GEO-ZN-ID,                     00304800
304900               :DCLXXXAIL-LOC.SCN-MAINT-SW,                       00304900
305000               :DCLXXXAIL-LOC.FRNT-END-CD,                        00305000
305100               :DCLXXXAIL-LOC.PRC-BUL-SW,                         00305100
305200               :DCLXXXAIL-LOC.UPC-ON-PRC-BUL-SW,                  00305200
305300               :DCLXXXAIL-LOC.CMPTR-TYP-CD,                       00305300
305400               :DCLXXXAIL-LOC.RETL-VID-ZN-NBR,                    00305400
305500               :DCLXXXAIL-LOC.RETL-UNLD-CD,                       00305500
305600               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-TXT,                 00305600
305700               :DCLXXXAIL-LOC.NEW-STR-SW,                         00305700
305800               :DCLXXXAIL-LOC.SEL-CIR-SW,                         00305800
305900               :DCLXXXAIL-LOC.BKRM-SQ-FT,                         00305900
306000               :DCLXXXAIL-LOC.FD-LINER-FT,                        00306000
306100               :DCLXXXAIL-LOC.NON-FD-LINER-FT,                    00306100
306200               :DCLXXXAIL-LOC.SETOFF-ROOM-SW,                     00306200
306300               :DCLXXXAIL-LOC.CAT-CLS-TBL-TXT,                    00306300
306400               :DCLXXXAIL-LOC.LAT-K,                              00306400
306500               :DCLXXXAIL-LOC.LON-K,                              00306500
306600               :DCLXXXAIL-LOC.CK-COLL-REPT-SW,                    00306600
306700               :DCLXXXAIL-LOC.CK-COLL-CNTL-CD,                    00306700
306800               :DCLXXXAIL-LOC.CK-COLL-ADD-DEL-SW,                 00306800
306900               :DCLXXXAIL-LOC.CK-ALT-STR-ID,                      00306900
307000               :DCLXXXAIL-LOC.CK-COLL-FEE-AMT,                    00307000
307100               :DCLXXXAIL-LOC.SALS-TAX-PCT,                       00307100
307200               :DCLXXXAIL-LOC.SOAP-SALE-VAR-PCT,                  00307200
307300               :DCLXXXAIL-LOC.ON-SRS-CD,                          00307300
307400               :DCLXXXAIL-LOC.SRS-DSD-ORD-SW,                     00307400
307500               :DCLXXXAIL-LOC.RETL-LOC-TYP-CD,                    00307500
307600               :DCLXXXAIL-LOC.DEA-NBR,                            00307600
307700               :DCLXXXAIL-LOC.STR-OPSTMT-SRT-CD,                  00307700
307800               :DCLXXXAIL-LOC.STR-OPSTMT-TYP-CD,                  00307800
307900               :DCLXXXAIL-LOC.STR-OPSTMT-HDR-CD,                  00307900
308000               :DCLXXXAIL-LOC.DPS-NBR,                            00308000
308100               :DCLXXXAIL-LOC.MEDICARE-ID,                        00308100
308200               :DCLXXXAIL-LOC.NABP-NBR,                           00308200
308300               :DCLXXXAIL-LOC.NATL-PROV-ID,                       00308300
308400               :DCLXXXAIL-LOC.CURR-AD-ZN-NBR,                     00308400
308500               :DCLXXXAIL-LOC.PD-ZONE-NO,                         00308500
308600               :DCLXXXAIL-LOC.SOS-PROC-SW,                        00308600
308700               :DCLXXXAIL-LOC.RPRT-SEQ-NBR,                       00308700
308800               :DCLXXXAIL-LOC.GRP-CD,                             00308800
308900               :DCLXXXAIL-LOC.PRIM-GRP-CD-1,                      00308900
309000               :DCLXXXAIL-LOC.PRIM-GRP-CD-2,                      00309000
309100               :DCLXXXAIL-LOC.SECY-GRP-CD-1,                      00309100
309200               :DCLXXXAIL-LOC.SECY-GRP-CD-2,                      00309200
309300               :DCLXXXAIL-LOC.PRIM-CLS-NBR-1,                     00309300
309400               :DCLXXXAIL-LOC.PRIM-CLS-NBR-2,                     00309400
309500               :DCLXXXAIL-LOC.SECY-CLS-NBR-1,                     00309500
309600               :DCLXXXAIL-LOC.SECY-CLS-NBR-2,                     00309600
309700               :DCLXXXAIL-LOC.VAL-STR-SW ,                        00309700
309800               :DCLXXXAIL-LOC.SLS-CLOSED-DT,                      00309800
309900               :DCLXXXAIL-LOC.TBCO-PRMT-NBR,                      00309900
310000               :DCLXXXAIL-LOC.SUB-UNLIKE-PROD-CD,                 00310000
310100               :DCLXXXAIL-LOC.SUB-DSPLY-PAL-CD,                   00310100
310200               :DCLXXXAIL-LOC.RLTM-SCN-MAINT-SW,                  00310200
310300               :DCLXXXAIL-LOC.TOP-LEADER-NM,                      00310300
310400               :DCLXXXAIL-LOC.CUST-FRNDLY-NM,                     00310400
310500               :DCLXXXAIL-LOC.SLS-OPEN-DT,                        00310500
310600               :WS-MON-OPEN-TS,                                   00310600
310700               :WS-MON-CLOS-TS,                                   00310700
310800               :WS-TUE-OPEN-TS,                                   00310800
310900               :WS-TUE-CLOS-TS,                                   00310900
311000               :WS-WED-OPEN-TS,                                   00311000
311100               :WS-WED-CLOS-TS,                                   00311100
311200               :WS-THUR-OPEN-TS,                                  00311200
311300               :WS-THUR-CLOS-TS,                                  00311300
311400               :WS-FRI-OPEN-TS ,                                  00311400
311500               :WS-FRI-CLOS-TS,                                   00311500
311600               :WS-SAT-OPEN-TS,                                   00311600
311700               :WS-SUN-OPEN-TS,                                   00311700
311800               :WS-SAT-CLOS-TS,                                   00311800
311900               :WS-SUN-CLOS-TS,                                   00311900
312000               :DCLXXXAIL-LOC.RETL-LOC-FRMAT-CD,                  00312000
312100               :DCLXXXAIL-LOC.RETL-LOC-SEGM-CD,                   00312100
312200               :DCLXXXAIL-LOC.ECOMM-MKT-AREA-CD,                  00312200
312300               :DCLXXXAIL-LOC.ECOMM-STRT-DT                       00312300
312400               :DCLXXXAIL-LOC-IND.ECOMM-STRT-DT-IND,              00312400
312500               :DCLXXXAIL-LOC.ECOMM-END-DT                        00312500
312600               :DCLXXXAIL-LOC-IND.ECOMM-END-DT-IND,               00312600
312700               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-01-NBR,              00312700
312800               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-02-NBR,              00312800
312900               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-03-NBR,              00312900
313000               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-04-NBR,              00313000
313100               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-05-NBR,              00313100
313200               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-06-NBR,              00313200
313300               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-07-NBR,              00313300
313400               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-08-NBR,              00313400
313500               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-09-NBR,              00313500
313600               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-10-NBR,              00313600
313700               :DCLXXXAIL-LOC.ONLIN-SSON-SW,                      00313700
313800               :DCLXXXAIL-LOC.RPLACD-BY-STR-NBR                   00313800
313810               :DCLXXXAIL-LOC-IND.RPLACD-BY-STR-NBR-IND           00313810
313900     END-EXEC                                                     00313900
314000     .                                                            00314000
314100                                                                  00314100
314200                                                                  00314200
314300 1306-FETCH-DDDXLR06.                                             00314300
314400     EXEC SQL                                                     00314400
314500         FETCH DDDXLR06                                           00314500
314600         INTO  :DCLXXXAIL-LOC.LOC-NBR,                            00314600
314700               :DCLXXXAIL-LOC.LOC-TYP-CD,                         00314700
314800               :DCLXXXAIL-LOC.ASSOC-STR-TYP-CD                    00314800
314900               :WS-ASSOC-ST-TYPE-IND,                             00314900
315000               :DCLXXXAIL-LOC.ASSOC-STR-NBR                       00315000
315100               :WS-ASSOC-ST-NO-IND,                               00315100
315200               :DCLXXXAIL-LOC.STR-REMODL-DT,                      00315200
315300               :DCLXXXAIL-LOC.RETL-LOC-STAT-CD,                   00315300
315400               :DCLXXXAIL-LOC.RETL-LOC-STAT-DT,                   00315400
315500               :DCLXXXAIL-LOC.COMPANY-ID,                         00315500
315600               :DCLXXXAIL-LOC.FINANCIAL-DIV-ID,                   00315600
315700               :DCLXXXAIL-LOC.LIN-OF-BUS-ID,                      00315700
315800               :DCLXXXAIL-LOC.DIST-ID,                            00315800
315900               :DCLXXXAIL-LOC.MKT-RGN-ID,                         00315900
316000               :DCLXXXAIL-LOC.GEO-ZN-CD,                          00316000
316100               :DCLXXXAIL-LOC.RETL-GEO-ZN-ID,                     00316100
316200               :DCLXXXAIL-LOC.SCN-MAINT-SW,                       00316200
316300               :DCLXXXAIL-LOC.FRNT-END-CD,                        00316300
316400               :DCLXXXAIL-LOC.PRC-BUL-SW,                         00316400
316500               :DCLXXXAIL-LOC.UPC-ON-PRC-BUL-SW,                  00316500
316600               :DCLXXXAIL-LOC.CMPTR-TYP-CD,                       00316600
316700               :DCLXXXAIL-LOC.RETL-VID-ZN-NBR,                    00316700
316800               :DCLXXXAIL-LOC.RETL-UNLD-CD,                       00316800
316900               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-TXT,                 00316900
317000               :DCLXXXAIL-LOC.NEW-STR-SW,                         00317000
317100               :DCLXXXAIL-LOC.SEL-CIR-SW,                         00317100
317200               :DCLXXXAIL-LOC.BKRM-SQ-FT,                         00317200
317300               :DCLXXXAIL-LOC.FD-LINER-FT,                        00317300
317400               :DCLXXXAIL-LOC.NON-FD-LINER-FT,                    00317400
317500               :DCLXXXAIL-LOC.SETOFF-ROOM-SW,                     00317500
317600               :DCLXXXAIL-LOC.CAT-CLS-TBL-TXT,                    00317600
317700               :DCLXXXAIL-LOC.LAT-K,                              00317700
317800               :DCLXXXAIL-LOC.LON-K,                              00317800
317900               :DCLXXXAIL-LOC.CK-COLL-REPT-SW,                    00317900
318000               :DCLXXXAIL-LOC.CK-COLL-CNTL-CD,                    00318000
318100               :DCLXXXAIL-LOC.CK-COLL-ADD-DEL-SW,                 00318100
318200               :DCLXXXAIL-LOC.CK-ALT-STR-ID,                      00318200
318300               :DCLXXXAIL-LOC.CK-COLL-FEE-AMT,                    00318300
318400               :DCLXXXAIL-LOC.SALS-TAX-PCT,                       00318400
318500               :DCLXXXAIL-LOC.SOAP-SALE-VAR-PCT,                  00318500
318600               :DCLXXXAIL-LOC.ON-SRS-CD,                          00318600
318700               :DCLXXXAIL-LOC.SRS-DSD-ORD-SW,                     00318700
318800               :DCLXXXAIL-LOC.RETL-LOC-TYP-CD,                    00318800
318900               :DCLXXXAIL-LOC.DEA-NBR,                            00318900
319000               :DCLXXXAIL-LOC.STR-OPSTMT-SRT-CD,                  00319000
319100               :DCLXXXAIL-LOC.STR-OPSTMT-TYP-CD,                  00319100
319200               :DCLXXXAIL-LOC.STR-OPSTMT-HDR-CD,                  00319200
319300               :DCLXXXAIL-LOC.DPS-NBR,                            00319300
319400               :DCLXXXAIL-LOC.MEDICARE-ID,                        00319400
319500               :DCLXXXAIL-LOC.NABP-NBR,                           00319500
319600               :DCLXXXAIL-LOC.NATL-PROV-ID,                       00319600
319700               :DCLXXXAIL-LOC.CURR-AD-ZN-NBR,                     00319700
319800               :DCLXXXAIL-LOC.PD-ZONE-NO,                         00319800
319900               :DCLXXXAIL-LOC.SOS-PROC-SW,                        00319900
320000               :DCLXXXAIL-LOC.RPRT-SEQ-NBR,                       00320000
320100               :DCLXXXAIL-LOC.GRP-CD,                             00320100
320200               :DCLXXXAIL-LOC.PRIM-GRP-CD-1,                      00320200
320300               :DCLXXXAIL-LOC.PRIM-GRP-CD-2,                      00320300
320400               :DCLXXXAIL-LOC.SECY-GRP-CD-1,                      00320400
320500               :DCLXXXAIL-LOC.SECY-GRP-CD-2,                      00320500
320600               :DCLXXXAIL-LOC.PRIM-CLS-NBR-1,                     00320600
320700               :DCLXXXAIL-LOC.PRIM-CLS-NBR-2,                     00320700
320800               :DCLXXXAIL-LOC.SECY-CLS-NBR-1,                     00320800
320900               :DCLXXXAIL-LOC.SECY-CLS-NBR-2,                     00320900
321000               :DCLXXXAIL-LOC.VAL-STR-SW ,                        00321000
321100               :DCLXXXAIL-LOC.SLS-CLOSED-DT,                      00321100
321200               :DCLXXXAIL-LOC.TBCO-PRMT-NBR,                      00321200
321300               :DCLXXXAIL-LOC.SUB-UNLIKE-PROD-CD,                 00321300
321400               :DCLXXXAIL-LOC.SUB-DSPLY-PAL-CD,                   00321400
321500               :DCLXXXAIL-LOC.RLTM-SCN-MAINT-SW,                  00321500
321600               :DCLXXXAIL-LOC.TOP-LEADER-NM,                      00321600
321700               :DCLXXXAIL-LOC.CUST-FRNDLY-NM,                     00321700
321800               :DCLXXXAIL-LOC.SLS-OPEN-DT,                        00321800
321900               :WS-MON-OPEN-TS,                                   00321900
322000               :WS-MON-CLOS-TS,                                   00322000
322100               :WS-TUE-OPEN-TS,                                   00322100
322200               :WS-TUE-CLOS-TS,                                   00322200
322300               :WS-WED-OPEN-TS,                                   00322300
322400               :WS-WED-CLOS-TS,                                   00322400
322500               :WS-THUR-OPEN-TS,                                  00322500
322600               :WS-THUR-CLOS-TS,                                  00322600
322700               :WS-FRI-OPEN-TS ,                                  00322700
322800               :WS-FRI-CLOS-TS,                                   00322800
322900               :WS-SAT-OPEN-TS,                                   00322900
323000               :WS-SUN-OPEN-TS,                                   00323000
323100               :WS-SAT-CLOS-TS,                                   00323100
323200               :WS-SUN-CLOS-TS,                                   00323200
323300               :DCLXXXAIL-LOC.RETL-LOC-FRMAT-CD,                  00323300
323400               :DCLXXXAIL-LOC.RETL-LOC-SEGM-CD,                   00323400
323500               :DCLXXXAIL-LOC.ECOMM-MKT-AREA-CD,                  00323500
323600               :DCLXXXAIL-LOC.ECOMM-STRT-DT                       00323600
323700               :DCLXXXAIL-LOC-IND.ECOMM-STRT-DT-IND,              00323700
323800               :DCLXXXAIL-LOC.ECOMM-END-DT                        00323800
323900               :DCLXXXAIL-LOC-IND.ECOMM-END-DT-IND,               00323900
324000               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-01-NBR,              00324000
324100               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-02-NBR,              00324100
324200               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-03-NBR,              00324200
324300               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-04-NBR,              00324300
324400               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-05-NBR,              00324400
324500               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-06-NBR,              00324500
324600               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-07-NBR,              00324600
324700               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-08-NBR,              00324700
324800               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-09-NBR,              00324800
324900               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-10-NBR,              00324900
325000               :DCLXXXAIL-LOC.ONLIN-SSON-SW,                      00325000
325100               :DCLXXXAIL-LOC.RPLACD-BY-STR-NBR                   00325100
325110               :DCLXXXAIL-LOC-IND.RPLACD-BY-STR-NBR-IND           00325110
325200     END-EXEC                                                     00325200
325300     .                                                            00325300
325400                                                                  00325400
325500                                                                  00325500
325600 1307-FETCH-DDDXLR07.                                             00325600
325700     EXEC SQL                                                     00325700
325800         FETCH DDDXLR07                                           00325800
325900         INTO  :DCLXXXAIL-LOC.LOC-NBR,                            00325900
326000               :DCLXXXAIL-LOC.LOC-TYP-CD,                         00326000
326100               :DCLXXXAIL-LOC.ASSOC-STR-TYP-CD                    00326100
326200               :WS-ASSOC-ST-TYPE-IND,                             00326200
326300               :DCLXXXAIL-LOC.STR-REMODL-DT,                      00326300
326400               :DCLXXXAIL-LOC.RETL-LOC-STAT-CD,                   00326400
326500               :DCLXXXAIL-LOC.RETL-LOC-STAT-DT,                   00326500
326600               :DCLXXXAIL-LOC.COMPANY-ID,                         00326600
326700               :DCLXXXAIL-LOC.FINANCIAL-DIV-ID,                   00326700
326800               :DCLXXXAIL-LOC.LIN-OF-BUS-ID,                      00326800
326900               :DCLXXXAIL-LOC.DIST-ID,                            00326900
327000               :DCLXXXAIL-LOC.MKT-RGN-ID,                         00327000
327100               :DCLXXXAIL-LOC.GEO-ZN-CD,                          00327100
327200               :DCLXXXAIL-LOC.RETL-GEO-ZN-ID,                     00327200
327300               :DCLXXXAIL-LOC.SCN-MAINT-SW,                       00327300
327400               :DCLXXXAIL-LOC.FRNT-END-CD,                        00327400
327500               :DCLXXXAIL-LOC.PRC-BUL-SW,                         00327500
327600               :DCLXXXAIL-LOC.UPC-ON-PRC-BUL-SW,                  00327600
327700               :DCLXXXAIL-LOC.CMPTR-TYP-CD,                       00327700
327800               :DCLXXXAIL-LOC.RETL-VID-ZN-NBR,                    00327800
327900               :DCLXXXAIL-LOC.RETL-UNLD-CD,                       00327900
328000               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-TXT,                 00328000
328100               :DCLXXXAIL-LOC.NEW-STR-SW,                         00328100
328200               :DCLXXXAIL-LOC.SEL-CIR-SW,                         00328200
328300               :DCLXXXAIL-LOC.BKRM-SQ-FT,                         00328300
328400               :DCLXXXAIL-LOC.FD-LINER-FT,                        00328400
328500               :DCLXXXAIL-LOC.NON-FD-LINER-FT,                    00328500
328600               :DCLXXXAIL-LOC.SETOFF-ROOM-SW,                     00328600
328700               :DCLXXXAIL-LOC.CAT-CLS-TBL-TXT,                    00328700
328800               :DCLXXXAIL-LOC.LAT-K,                              00328800
328900               :DCLXXXAIL-LOC.LON-K,                              00328900
329000               :DCLXXXAIL-LOC.CK-COLL-REPT-SW,                    00329000
329100               :DCLXXXAIL-LOC.CK-COLL-CNTL-CD,                    00329100
329200               :DCLXXXAIL-LOC.CK-COLL-ADD-DEL-SW,                 00329200
329300               :DCLXXXAIL-LOC.CK-ALT-STR-ID,                      00329300
329400               :DCLXXXAIL-LOC.CK-COLL-FEE-AMT,                    00329400
329500               :DCLXXXAIL-LOC.SALS-TAX-PCT,                       00329500
329600               :DCLXXXAIL-LOC.SOAP-SALE-VAR-PCT,                  00329600
329700               :DCLXXXAIL-LOC.ON-SRS-CD,                          00329700
329800               :DCLXXXAIL-LOC.SRS-DSD-ORD-SW,                     00329800
329900               :DCLXXXAIL-LOC.RETL-LOC-TYP-CD,                    00329900
330000               :DCLXXXAIL-LOC.DEA-NBR,                            00330000
330100               :DCLXXXAIL-LOC.STR-OPSTMT-SRT-CD,                  00330100
330200               :DCLXXXAIL-LOC.STR-OPSTMT-TYP-CD,                  00330200
330300               :DCLXXXAIL-LOC.STR-OPSTMT-HDR-CD,                  00330300
330400               :DCLXXXAIL-LOC.DPS-NBR,                            00330400
330500               :DCLXXXAIL-LOC.MEDICARE-ID,                        00330500
330600               :DCLXXXAIL-LOC.NABP-NBR,                           00330600
330700               :DCLXXXAIL-LOC.NATL-PROV-ID,                       00330700
330800               :DCLXXXAIL-LOC.CURR-AD-ZN-NBR,                     00330800
330900               :DCLXXXAIL-LOC.PD-ZONE-NO,                         00330900
331000               :DCLXXXAIL-LOC.SOS-PROC-SW,                        00331000
331100               :DCLXXXAIL-LOC.RPRT-SEQ-NBR,                       00331100
331200               :DCLXXXAIL-LOC.GRP-CD,                             00331200
331300               :DCLXXXAIL-LOC.PRIM-GRP-CD-1,                      00331300
331400               :DCLXXXAIL-LOC.PRIM-GRP-CD-2,                      00331400
331500               :DCLXXXAIL-LOC.SECY-GRP-CD-1,                      00331500
331600               :DCLXXXAIL-LOC.SECY-GRP-CD-2,                      00331600
331700               :DCLXXXAIL-LOC.PRIM-CLS-NBR-1,                     00331700
331800               :DCLXXXAIL-LOC.PRIM-CLS-NBR-2,                     00331800
331900               :DCLXXXAIL-LOC.SECY-CLS-NBR-1,                     00331900
332000               :DCLXXXAIL-LOC.SECY-CLS-NBR-2,                     00332000
332100               :DCLXXXAIL-LOC.VAL-STR-SW,                         00332100
332200               :DCLXXXAIL-LOC.SLS-CLOSED-DT,                      00332200
332300               :DCLXXXAIL-LOC.TBCO-PRMT-NBR,                      00332300
332400               :DCLXXXAIL-LOC.SUB-UNLIKE-PROD-CD,                 00332400
332500               :DCLXXXAIL-LOC.SUB-DSPLY-PAL-CD,                   00332500
332600               :DCLXXXAIL-LOC.RLTM-SCN-MAINT-SW,                  00332600
332700               :DCLXXXAIL-LOC.TOP-LEADER-NM,                      00332700
332800               :DCLXXXAIL-LOC.CUST-FRNDLY-NM,                     00332800
332900               :DCLXXXAIL-LOC.SLS-OPEN-DT,                        00332900
333000               :WS-MON-OPEN-TS,                                   00333000
333100               :WS-MON-CLOS-TS,                                   00333100
333200               :WS-TUE-OPEN-TS,                                   00333200
333300               :WS-TUE-CLOS-TS,                                   00333300
333400               :WS-WED-OPEN-TS,                                   00333400
333500               :WS-WED-CLOS-TS,                                   00333500
333600               :WS-THUR-OPEN-TS,                                  00333600
333700               :WS-THUR-CLOS-TS,                                  00333700
333800               :WS-FRI-OPEN-TS ,                                  00333800
333900               :WS-FRI-CLOS-TS,                                   00333900
334000               :WS-SAT-OPEN-TS,                                   00334000
334100               :WS-SUN-OPEN-TS,                                   00334100
334200               :WS-SAT-CLOS-TS,                                   00334200
334300               :WS-SUN-CLOS-TS,                                   00334300
334400               :DCLXXXAIL-LOC.RETL-LOC-FRMAT-CD,                  00334400
334500               :DCLXXXAIL-LOC.RETL-LOC-SEGM-CD,                   00334500
334600               :DCLXXXAIL-LOC.ECOMM-MKT-AREA-CD,                  00334600
334700               :DCLXXXAIL-LOC.ECOMM-STRT-DT                       00334700
334800               :DCLXXXAIL-LOC-IND.ECOMM-STRT-DT-IND,              00334800
334900               :DCLXXXAIL-LOC.ECOMM-END-DT                        00334900
335000               :DCLXXXAIL-LOC-IND.ECOMM-END-DT-IND,               00335000
335100               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-01-NBR,              00335100
335200               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-02-NBR,              00335200
335300               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-03-NBR,              00335300
335400               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-04-NBR,              00335400
335500               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-05-NBR,              00335500
335600               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-06-NBR,              00335600
335700               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-07-NBR,              00335700
335800               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-08-NBR,              00335800
335900               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-09-NBR,              00335900
336000               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-10-NBR,              00336000
336100               :DCLXXXAIL-LOC.ONLIN-SSON-SW,                      00336100
336200               :DCLXXXAIL-LOC.RPLACD-BY-STR-NBR                   00336200
336210               :DCLXXXAIL-LOC-IND.RPLACD-BY-STR-NBR-IND           00336210
336300     END-EXEC                                                     00336300
336400     .                                                            00336400
336500                                                                  00336500
336600                                                                  00336600
336700 1309-FETCH-DDDXLR09.                                             00336700
336800     EXEC SQL                                                     00336800
336900         FETCH DDDXLR09                                           00336900
337000         INTO  :DCLXXXAIL-LOC.LOC-NBR,                            00337000
337100               :DCLXXXAIL-LOC.LOC-TYP-CD,                         00337100
337200               :DCLXXXAIL-LOC.ASSOC-STR-TYP-CD                    00337200
337300               :WS-ASSOC-ST-TYPE-IND,                             00337300
337400               :DCLXXXAIL-LOC.ASSOC-STR-NBR                       00337400
337500               :WS-ASSOC-ST-NO-IND,                               00337500
337600               :DCLXXXAIL-LOC.STR-REMODL-DT,                      00337600
337700               :DCLXXXAIL-LOC.RETL-LOC-STAT-CD,                   00337700
337800               :DCLXXXAIL-LOC.RETL-LOC-STAT-DT,                   00337800
337900               :DCLXXXAIL-LOC.COMPANY-ID,                         00337900
338000               :DCLXXXAIL-LOC.FINANCIAL-DIV-ID,                   00338000
338100               :DCLXXXAIL-LOC.LIN-OF-BUS-ID,                      00338100
338200               :DCLXXXAIL-LOC.DIST-ID,                            00338200
338300               :DCLXXXAIL-LOC.MKT-RGN-ID,                         00338300
338400               :DCLXXXAIL-LOC.GEO-ZN-CD,                          00338400
338500               :DCLXXXAIL-LOC.RETL-GEO-ZN-ID,                     00338500
338600               :DCLXXXAIL-LOC.SCN-MAINT-SW,                       00338600
338700               :DCLXXXAIL-LOC.FRNT-END-CD,                        00338700
338800               :DCLXXXAIL-LOC.PRC-BUL-SW,                         00338800
338900               :DCLXXXAIL-LOC.UPC-ON-PRC-BUL-SW,                  00338900
339000               :DCLXXXAIL-LOC.CMPTR-TYP-CD,                       00339000
339100               :DCLXXXAIL-LOC.RETL-VID-ZN-NBR,                    00339100
339200               :DCLXXXAIL-LOC.RETL-UNLD-CD,                       00339200
339300               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-TXT,                 00339300
339400               :DCLXXXAIL-LOC.NEW-STR-SW,                         00339400
339500               :DCLXXXAIL-LOC.SEL-CIR-SW,                         00339500
339600               :DCLXXXAIL-LOC.BKRM-SQ-FT,                         00339600
339700               :DCLXXXAIL-LOC.FD-LINER-FT,                        00339700
339800               :DCLXXXAIL-LOC.NON-FD-LINER-FT,                    00339800
339900               :DCLXXXAIL-LOC.SETOFF-ROOM-SW,                     00339900
340000               :DCLXXXAIL-LOC.CAT-CLS-TBL-TXT,                    00340000
340100               :DCLXXXAIL-LOC.LAT-K,                              00340100
340200               :DCLXXXAIL-LOC.LON-K,                              00340200
340300               :DCLXXXAIL-LOC.CK-COLL-REPT-SW,                    00340300
340400               :DCLXXXAIL-LOC.CK-COLL-CNTL-CD,                    00340400
340500               :DCLXXXAIL-LOC.CK-COLL-ADD-DEL-SW,                 00340500
340600               :DCLXXXAIL-LOC.CK-ALT-STR-ID,                      00340600
340700               :DCLXXXAIL-LOC.CK-COLL-FEE-AMT,                    00340700
340800               :DCLXXXAIL-LOC.SALS-TAX-PCT,                       00340800
340900               :DCLXXXAIL-LOC.SOAP-SALE-VAR-PCT,                  00340900
341000               :DCLXXXAIL-LOC.ON-SRS-CD,                          00341000
341100               :DCLXXXAIL-LOC.SRS-DSD-ORD-SW,                     00341100
341200               :DCLXXXAIL-LOC.RETL-LOC-TYP-CD,                    00341200
341300               :DCLXXXAIL-LOC.DEA-NBR,                            00341300
341400               :DCLXXXAIL-LOC.STR-OPSTMT-SRT-CD,                  00341400
341500               :DCLXXXAIL-LOC.STR-OPSTMT-TYP-CD,                  00341500
341600               :DCLXXXAIL-LOC.STR-OPSTMT-HDR-CD,                  00341600
341700               :DCLXXXAIL-LOC.DPS-NBR,                            00341700
341800               :DCLXXXAIL-LOC.MEDICARE-ID,                        00341800
341900               :DCLXXXAIL-LOC.NABP-NBR,                           00341900
342000               :DCLXXXAIL-LOC.NATL-PROV-ID,                       00342000
342100               :DCLXXXAIL-LOC.CURR-AD-ZN-NBR,                     00342100
342200               :DCLXXXAIL-LOC.PD-ZONE-NO,                         00342200
342300               :DCLXXXAIL-LOC.SOS-PROC-SW,                        00342300
342400               :DCLXXXAIL-LOC.RPRT-SEQ-NBR,                       00342400
342500               :DCLXXXAIL-LOC.GRP-CD,                             00342500
342600               :DCLXXXAIL-LOC.PRIM-GRP-CD-1,                      00342600
342700               :DCLXXXAIL-LOC.PRIM-GRP-CD-2,                      00342700
342800               :DCLXXXAIL-LOC.SECY-GRP-CD-1,                      00342800
342900               :DCLXXXAIL-LOC.SECY-GRP-CD-2,                      00342900
343000               :DCLXXXAIL-LOC.PRIM-CLS-NBR-1,                     00343000
343100               :DCLXXXAIL-LOC.PRIM-CLS-NBR-2,                     00343100
343200               :DCLXXXAIL-LOC.SECY-CLS-NBR-1,                     00343200
343300               :DCLXXXAIL-LOC.SECY-CLS-NBR-2,                     00343300
343400               :DCLXXXAIL-LOC.VAL-STR-SW,                         00343400
343500               :DCLXXXAIL-LOC.SLS-CLOSED-DT,                      00343500
343600               :DCLXXXAIL-LOC.TBCO-PRMT-NBR,                      00343600
343700               :DCLXXXAIL-LOC.SUB-UNLIKE-PROD-CD,                 00343700
343800               :DCLXXXAIL-LOC.SUB-DSPLY-PAL-CD,                   00343800
343900               :DCLXXXAIL-LOC.RLTM-SCN-MAINT-SW,                  00343900
344000               :DCLXXXAIL-LOC.TOP-LEADER-NM,                      00344000
344100               :DCLXXXAIL-LOC.CUST-FRNDLY-NM,                     00344100
344200               :DCLXXXAIL-LOC.SLS-OPEN-DT,                        00344200
344300               :WS-MON-OPEN-TS,                                   00344300
344400               :WS-MON-CLOS-TS,                                   00344400
344500               :WS-TUE-OPEN-TS,                                   00344500
344600               :WS-TUE-CLOS-TS,                                   00344600
344700               :WS-WED-OPEN-TS,                                   00344700
344800               :WS-WED-CLOS-TS,                                   00344800
344900               :WS-THUR-OPEN-TS,                                  00344900
345000               :WS-THUR-CLOS-TS,                                  00345000
345100               :WS-FRI-OPEN-TS ,                                  00345100
345200               :WS-FRI-CLOS-TS,                                   00345200
345300               :WS-SAT-OPEN-TS,                                   00345300
345400               :WS-SUN-OPEN-TS,                                   00345400
345500               :WS-SAT-CLOS-TS,                                   00345500
345600               :WS-SUN-CLOS-TS,                                   00345600
345700               :DCLXXXAIL-LOC.RETL-LOC-FRMAT-CD,                  00345700
345800               :DCLXXXAIL-LOC.RETL-LOC-SEGM-CD,                   00345800
345900               :DCLXXXAIL-LOC.ECOMM-MKT-AREA-CD,                  00345900
346000               :DCLXXXAIL-LOC.ECOMM-STRT-DT                       00346000
346100               :DCLXXXAIL-LOC-IND.ECOMM-STRT-DT-IND,              00346100
346200               :DCLXXXAIL-LOC.ECOMM-END-DT                        00346200
346300               :DCLXXXAIL-LOC-IND.ECOMM-END-DT-IND,               00346300
346400               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-01-NBR,              00346400
346500               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-02-NBR,              00346500
346600               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-03-NBR,              00346600
346700               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-04-NBR,              00346700
346800               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-05-NBR,              00346800
346900               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-06-NBR,              00346900
347000               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-07-NBR,              00347000
347100               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-08-NBR,              00347100
347200               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-09-NBR,              00347200
347300               :DCLXXXAIL-LOC.ROLUP-REPT-TBL-10-NBR,              00347300
347400               :DCLXXXAIL-LOC.ONLIN-SSON-SW,                      00347400
347500               :DCLXXXAIL-LOC.RPLACD-BY-STR-NBR                   00347500
347510               :DCLXXXAIL-LOC-IND.RPLACD-BY-STR-NBR-IND           00347510
347600     END-EXEC                                                     00347600
347700     .                                                            00347700
347800                                                                  00347800
347900                                                                  00347900
348000 1400-EXIT-PUT-MODIFY-ROW.                                        00348000
348100     PERFORM 1800-EDIT-NULL-INDICATORS                            00348100
348200     PERFORM 1410-MODIFY-EDITS                                    00348200
348300     IF SUCCESS                                                   00348300
348400       PERFORM 1430-CHECK-FOR-EVENTS                              00348400
348500       IF SQLCODE = 0                                             00348500
348600         PERFORM 1440-D0-MODIFY-ROW                               00348600
348700         IF SUCCESS AND SQLCODE = 0                               00348700
348800           PERFORM 2400-CHECK-FOR-DCM-EVENT                       00348800
348900         END-IF                                                   00348900
349000       END-IF                                                     00349000
349100     END-IF                                                       00349100
349200     .                                                            00349200
349300                                                                  00349300
349400                                                                  00349400
349500 1410-MODIFY-EDITS.                                               00349500
349600     PERFORM 1420-VALIDATE-ALT-STORE                              00349600
349700     .                                                            00349700
349800                                                                  00349800
349900                                                                  00349900
350000 1420-VALIDATE-ALT-STORE.                                         00350000
350100     IF CK-COLL-ADD-DEL-SW OF DCLXXXAIL-LOC = 'Y' AND             00350100
350200        CK-ALT-STR-ID OF DCLXXXAIL-LOC NOT = 0    AND             00350200
350300        CK-COLL-REPT-SW OF DCLXXXAIL-LOC   = 'Y'                  00350300
350400          EXEC SQL                                                00350400
350500            SELECT COUNT(*)                                       00350500
350600            INTO :WS-CNT                                          00350600
350700            FROM XXXAIL_LOC                                       00350700
350800            WHERE LOC_NBR    = :DCLXXXAIL-LOC.CK-ALT-STR-ID       00350800
350900            AND   LOC_TYP_CD = :DCLXXXAIL-LOC.LOC-TYP-CD          00350900
351000          END-EXEC                                                00351000
351100          EVALUATE TRUE                                           00351100
351200           WHEN SQLCODE = 0                                       00351200
351300            IF WS-CNT = 0                                         00351300
351400              SET  FAILURE                 TO TRUE                00351400
351500              MOVE SPACES                  TO IS-RTRN-MSG-TXT     00351500
351600              STRING 'NNNS0488 - Alt. store does not exists'      00351600
351700                     DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT       00351700
351800            ELSE                                                  00351800
351900              CONTINUE                                            00351900
352000            END-IF                                                00352000
352100           WHEN OTHER                                             00352100
352200             SET  FAILURE                 TO TRUE                 00352200
352300             MOVE SPACES                  TO IS-RTRN-MSG-TXT      00352300
352400             STRING 'NNNS0488 - Error accessing DB2'              00352400
352500                    DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT        00352500
352600     END-IF                                                       00352600
352700     .                                                            00352700
352800                                                                  00352800
352900                                                                  00352900
353000 1430-CHECK-FOR-EVENTS.                                           00353000
353100     EXEC SQL                                                     00353100
353200         SELECT RETL_LOC_STAT_CD,                                 00353200
353300                CURR_AD_ZN_NBR,                                   00353300
353400                LIN_OF_BUS_ID                                     00353400
353500         INTO   :WS-STR-ST-CD,                                    00353500
353600                :WS-CURR-AD-ZONE,                                 00353600
353700                :WS-CURR-LOB                                      00353700
353800         FROM   XXXAIL_LOC                                        00353800
353900         WHERE  LOC_NBR = :DCLXXXAIL-LOC.LOC-NBR                  00353900
354000         AND    LOC_TYP_CD = :DCLXXXAIL-LOC.LOC-TYP-CD            00354000
354100     END-EXEC                                                     00354100
354200                                                                  00354200
354300     EVALUATE TRUE                                                00354300
354400       WHEN SQLCODE = 0                                           00354400
354500         IF WS-STR-ST-CD NOT = RETL-LOC-STAT-CD OF DCLXXXAIL-LOC  00354500
354600           SET WS-LOC-STAT-CHANGED TO TRUE                        00354600
354700           PERFORM 2040-GET-CURRENT-DATE                          00354700
354800           IF SUCCESS                                             00354800
354900             MOVE DTA10-MM-DD-YYYY                                00354900
355000               TO RETL-LOC-STAT-DT OF DCLXXXAIL-LOC               00355000
355100           END-IF                                                 00355100
355200         ELSE                                                     00355200
355300           SET  WS-LOC-STAT-RETAINED TO TRUE                      00355300
355400         END-IF                                                   00355400
355500       WHEN SQLCODE = 100                                         00355500
355600         SET  FAILURE TO TRUE                                     00355600
355700         MOVE 'NNNS0488 - xxxail xxxation not found!'             00355700
355800           TO IS-RTRN-MSG-TXT                                     00355800
355900       WHEN SQLCODE NOT = 0                                       00355900
356000         MOVE SQLCODE                 TO WS-SQLCODE               00356000
356100         SET  FAILURE                 TO TRUE                     00356100
356200         MOVE SPACES                  TO IS-RTRN-MSG-TXT          00356200
356300         STRING 'NNNS0488 - Error checking for changes, SQL='     00356300
356400                 WS-SQLCODE                                       00356400
356500                 DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT           00356500
356600     END-EVALUATE                                                 00356600
356700     .                                                            00356700
356800                                                                  00356800
356900                                                                  00356900
357000 1440-D0-MODIFY-ROW.                                              00357000
357100     PERFORM 4670-REP-LOWVALUE-WITH-SPACES                        00357100
357200     PERFORM 5000-CALL-NNNU0488-CUD-ROUTINE                       00357200
357300                                                                  00357300
357400     IF SQLCODE = 0                                               00357400
357500       SET YYYN110A-UPD TO TRUE                                   00357500
357600       SET LOC-UPD      TO TRUE                                   00357600
357700       PERFORM 2000-DENORM-PROCESS                                00357700
357800     END-IF                                                       00357800
357900     .                                                            00357900
358000                                                                  00358000
358100                                                                  00358100
358200 1500-EXIT-PUT-INSERT-ROW.                                        00358200
358300     PERFORM 1800-EDIT-NULL-INDICATORS                            00358300
358400     PERFORM 1510-ADD-EDITS                                       00358400
358500     IF SUCCESS                                                   00358500
358600       PERFORM 1520-D0-INSERT-ROW                                 00358600
358700     END-IF                                                       00358700
358800     .                                                            00358800
358900                                                                  00358900
359000                                                                  00359000
359100 1510-ADD-EDITS.                                                  00359100
359200     IF RETL-LOC-STAT-CD OF DCLXXXAIL-LOC = SPACES                00359200
359300        MOVE 'X' TO RETL-LOC-STAT-CD OF DCLXXXAIL-LOC             00359300
359400     END-IF                                                       00359400
359500                                                                  00359500
359600     IF NEW-STR-SW       OF DCLXXXAIL-LOC = SPACES                00359600
359700        MOVE 'Y' TO NEW-STR-SW OF DCLXXXAIL-LOC                   00359700
359800     END-IF                                                       00359800
359900                                                                  00359900
360000     IF SETOFF-ROOM-SW   OF DCLXXXAIL-LOC = SPACES                00360000
360100        MOVE 'N' TO SETOFF-ROOM-SW OF DCLXXXAIL-LOC               00360100
360200     END-IF                                                       00360200
360300                                                                  00360300
360400     IF PRC-BUL-SW       OF DCLXXXAIL-LOC = SPACES                00360400
360500        MOVE 'N' TO PRC-BUL-SW OF DCLXXXAIL-LOC                   00360500
360600     END-IF                                                       00360600
360700                                                                  00360700
360800     IF FRNT-END-CD      OF DCLXXXAIL-LOC = SPACES                00360800
360900        MOVE 'N' TO FRNT-END-CD OF DCLXXXAIL-LOC                  00360900
361000     END-IF                                                       00361000
361100                                                                  00361100
361200     IF SCN-MAINT-SW     OF DCLXXXAIL-LOC = SPACES                00361200
361300        MOVE 'N' TO SCN-MAINT-SW OF DCLXXXAIL-LOC                 00361300
361400     END-IF                                                       00361400
361500                                                                  00361500
361600     IF UPC-ON-PRC-BUL-SW OF DCLXXXAIL-LOC = SPACES               00361600
361700        MOVE 'N' TO  UPC-ON-PRC-BUL-SW OF DCLXXXAIL-LOC           00361700
361800     END-IF                                                       00361800
361900                                                                  00361900
362000     MOVE STR-OPSTMT-SRT-CD OF DCLXXXAIL-LOC TO                   00362000
362100          RPRT-SEQ-NBR      OF DCLXXXAIL-LOC                      00362100
362200     .                                                            00362200
362300                                                                  00362300
362400                                                                  00362400
362500 1520-D0-INSERT-ROW.                                              00362500
362600     PERFORM 4600-CALL-MMMS0335-RI-ADD-CHK                        00362600
362700     IF SUCCESS                                                   00362700
362800        PERFORM 4670-REP-LOWVALUE-WITH-SPACES                     00362800
362900        PERFORM 5000-CALL-NNNU0488-CUD-ROUTINE                    00362900
363000                                                                  00363000
363100        IF SQLCODE = 0                                            00363100
363200          SET WS-LOC-STAT-CHANGED TO TRUE                         00363200
363300          SET YYYN110A-ADD TO TRUE                                00363300
363400          SET LOC-ADD      TO TRUE                                00363400
363500          PERFORM 2000-DENORM-PROCESS                             00363500
363600        END-IF                                                    00363600
363700     END-IF                                                       00363700
363800     .                                                            00363800
363900                                                                  00363900
364000                                                                  00364000
364100 1600-EXIT-PUT-PURGE-ROW.                                         00364100
364200     PERFORM 4500-CALL-MMMS0304-RI-DEL-CHK                        00364200
364300     IF SUCCESS                                                   00364300
364400       PERFORM 5000-CALL-NNNU0488-CUD-ROUTINE                     00364400
364500                                                                  00364500
364600       IF SQLCODE = 0 OR 100                                      00364600
364700         MOVE 0            TO SQLCODE                             00364700
364800         SET  YYYN110A-DEL TO TRUE                                00364800
364900         SET  LOC-DEL      TO TRUE                                00364900
365000       END-IF                                                     00365000
365100     END-IF                                                       00365100
365200                                                                  00365200
365300*    EVALUATE TRUE                                                00365300
365400*      WHEN SQLCODE = 0                                           00365400
365500*      OR   SQLCODE = 100                                         00365500
365600*        MOVE 0            TO SQLCODE                             00365600
365700*        SET  YYYN110A-DEL TO TRUE                                00365700
365800*        SET  LOC-DEL      TO TRUE                                00365800
365900*      WHEN SQLCODE = -532                                        00365900
366000*        SET  FAILURE TO TRUE                                     00366000
366100*        MOVE 'NNNS0488 - xxxation in use - it cannot be deleted!'00366100
366200*          TO IS-RTRN-MSG-TXT                                     00366200
366300*      WHEN SQLCODE NOT = 0                                       00366300
366400*        MOVE SQLCODE                 TO WS-SQLCODE               00366400
366500*        SET  FAILURE                 TO TRUE                     00366500
366600*        MOVE SPACES                  TO IS-RTRN-MSG-TXT          00366600
366700*        STRING 'NNNS0488 - Error deleting xxxail loc, SQL='      00366700
366800*                WS-SQLCODE                                       00366800
366900*                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT           00366900
367000*    END-EVALUATE                                                 00367000
367100     .                                                            00367100
367200                                                                  00367200
367300                                                                  00367300
367400* ================================================================00367400
367500* Initialize NULL variables if the column is set to NULL.         00367500
367600* ================================================================00367600
367700 1700-CHECK-NULL-COLUMNS.                                         00367700
367800     IF WS-ASSOC-ST-TYPE-IND < 0                                  00367800
367900     OR WS-ASSOC-ST-NO-IND < 0                                    00367900
368000       MOVE SPACES TO ASSOC-STR-TYP-CD OF DCLXXXAIL-LOC           00368000
368100       MOVE 0      TO ASSOC-STR-NBR OF DCLXXXAIL-LOC              00368100
368200     END-IF                                                       00368200
368210     IF RPLACD-BY-STR-NBR-IND < 0                                 00368210
368211       MOVE 0      TO RPLACD-BY-STR-NBR-IND                       00368211
368220     END-IF                                                       00368220
368300     IF ECOMM-STRT-DT-IND OF DCLXXXAIL-LOC-IND < 0                00368300
368400*      MOVE 0      TO ECOMM-STRT-DT OF DCLXXXAIL-LOC              00368400
368500       MOVE K-ZERO-DT TO  ECOMM-STRT-DT OF DCLXXXAIL-LOC          00368500
368600     END-IF                                                       00368600
368700     IF ECOMM-END-DT-IND OF DCLXXXAIL-LOC-IND < 0                 00368700
368800*      MOVE 0      TO ECOMM-END-DT OF DCLXXXAIL-LOC               00368800
368900       MOVE K-ZERO-DT TO ECOMM-END-DT OF DCLXXXAIL-LOC            00368900
369000     END-IF                                                       00369000
369100     .                                                            00369100
369200                                                                  00369200
369300                                                                  00369300
369400* ================================================================00369400
369500* Make sure the null indicators are valid.                        00369500
369600* ================================================================00369600
369700 1800-EDIT-NULL-INDICATORS.                                       00369700
369800     INITIALIZE WS-ASSOC-ST-TYPE-IND                              00369800
369900                WS-ASSOC-ST-NO-IND                                00369900
370000                                                                  00370000
370100     IF ASSOC-STR-TYP-CD OF DCLXXXAIL-LOC = SPACES                00370100
370200     OR ASSOC-STR-NBR OF DCLXXXAIL-LOC = 0                        00370200
370300       MOVE -1 TO WS-ASSOC-ST-TYPE-IND                            00370300
370400       MOVE SPACES TO ASSOC-STR-TYP-CD OF DCLXXXAIL-LOC           00370400
370500       MOVE -1 TO WS-ASSOC-ST-NO-IND                              00370500
370600       MOVE ZERO   TO ASSOC-STR-NBR OF DCLXXXAIL-LOC              00370600
370700     END-IF                                                       00370700
370800     IF RPLACD-BY-STR-NBR OF DCLXXXAIL-LOC = 0                    00370800
370900        MOVE -1   TO RPLACD-BY-STR-NBR-IND                        00370900
371000        MOVE ZERO TO RPLACD-BY-STR-NBR     OF DCLXXXAIL-LOC       00371000
371001     END-IF                                                       00371001
371002     IF ECOMM-STRT-DT-IND OF DCLXXXAIL-LOC-IND  < 0               00371002
371003       MOVE -1 TO ECOMM-STRT-DT-IND OF DCLXXXAIL-LOC-IND          00371003
371004     ELSE                                                         00371004
371100       MOVE 0  TO ECOMM-STRT-DT-IND OF DCLXXXAIL-LOC-IND          00371100
371200     END-IF                                                       00371200
371300     IF ECOMM-END-DT-IND OF DCLXXXAIL-LOC-IND  < 0                00371300
371400       MOVE -1 TO ECOMM-END-DT-IND OF DCLXXXAIL-LOC-IND           00371400
371500     ELSE                                                         00371500
371600       MOVE 0  TO ECOMM-END-DT-IND OF DCLXXXAIL-LOC-IND           00371600
371700     END-IF                                                       00371700
371800     .                                                            00371800
371900* ================================================================00371900
372000* Misc functions...                                               00372000
372100* ================================================================00372100
372200 2000-DENORM-PROCESS.                                             00372200
372300     IF WS-LOC-STAT-CHANGED                                       00372300
372400       PERFORM 2100-UPD-STAT-IN-xxxation                          00372400
372500     END-IF                                                       00372500
372600     MOVE 1 TO WS-CHECKPOINT-INC                                  00372600
372700     IF SUCCESS                                                   00372700
372800       PERFORM 2010-CALL-CONTROL-SUBR                             00372800
372900     END-IF                                                       00372900
373000     IF  SUCCESS                                                  00373000
373100     AND WWWC0100-NORM-TASK                                       00373100
373200       PERFORM 2020-CALL-SYNC-SUBR                                00373200
373300     END-IF                                                       00373300
373400     IF SUCCESS                                                   00373400
373500       PERFORM 2030-ISSUE-EVENTS                                  00373500
373600     END-IF                                                       00373600
373700     .                                                            00373700
373800                                                                  00373800
373900                                                                  00373900
374000                                                                  00374000
374100 2010-CALL-CONTROL-SUBR.                                          00374100
374200     SET WWWC0100-GET-TASK  TO TRUE                               00374200
374300     CALL WWWS0100-CONTROL-SUBR USING                             00374300
374400         XXXN001A                                                 00374400
374500         WWWC0100                                                 00374500
374600     .                                                            00374600
374700                                                                  00374700
374800                                                                  00374800
374900 2020-CALL-SYNC-SUBR.                                             00374900
375000     SET YYYN110A-ORACLE        TO TRUE                           00375000
375100     SET YYYN110A-LAST-CALL     TO TRUE                           00375100
375200     SET MMMC0159-LR-IS-CURRENT TO TRUE                           00375200
375300     CALL MMMS0159-SYNC-LR USING                                  00375300
375400         XXXN001A                                                 00375400
375500         YYYN110A                                                 00375500
375600         MMMC0159                                                 00375600
375700         P-DDDTLR01                                               00375700
375800     .                                                            00375800
375900                                                                  00375900
376000                                                                  00376000
376100 2030-ISSUE-EVENTS.                                               00376100
376200     PERFORM 2050-GET-CURRENT-USER                                00376200
376300     SET  YYYN110A-ORACLE      TO TRUE                            00376300
376400     IF SUCCESS                                                   00376400
376500       MOVE LOC-NBR OF P-DDDTLR01 TO ST-STORE-NUMBER OF ZZZC0032  00376500
376600                                     LOC-NBR OF ZZZC0094          00376600
376700       SET  ZZZC0032-UPD-FXXX     TO TRUE                         00376700
376800       MOVE ZZZC0032              TO ZZZC0197-TRX-REC             00376800
376900       MOVE 'CUST'                TO ZZZC0197-TRX-ID              00376900
377000       MOVE 'NNNS0488'            TO ZZZC0197-PROGRAM             00377000
377100       MOVE YYYC0107-USER         TO ZZZC0197-USER                00377100
377200       MOVE YYYN005A-SYS-ENV      TO YYYN110A-SYS-ENV             00377200
377300       CALL ZZZS0197-EVENT-MGR USING                              00377300
377400            XXXN001A                                              00377400
377500            YYYN110A                                              00377500
377600            ZZZC0197                                              00377600
377700                                                                  00377700
377800       EVALUATE TRUE                                              00377800
377900                                                                  00377900
378000       WHEN LOC-TYP-CD OF P-DDDTLR01 = K-STORE-LOC-TYPE           00378000
378100         MOVE LOC-TYP-CD OF P-DDDTLR01 TO                         00378100
378200                                       LOC-TYP-CD OF ZZZC0094     00378200
378300         MOVE ZZZC0094              TO ZZZC0197-TRX-REC           00378300
378400         MOVE 'STRM'                TO ZZZC0197-TRX-ID            00378400
378500         MOVE 'NNNS0488'            TO ZZZC0197-PROGRAM           00378500
378600         MOVE YYYC0107-USER         TO ZZZC0197-USER              00378600
378700         MOVE YYYN005A-SYS-ENV      TO YYYN110A-SYS-ENV           00378700
378800         CALL ZZZS0197-EVENT-MGR USING                            00378800
378900              XXXN001A                                            00378900
379000              YYYN110A                                            00379000
379100              ZZZC0197                                            00379100
379200                                                                  00379200
379300       END-EVALUATE                                               00379300
379400                                                                  00379400
379500     END-IF                                                       00379500
379600     .                                                            00379600
379700                                                                  00379700
379800                                                                  00379800
379900 2040-GET-CURRENT-DATE.                                           00379900
380000     CALL Z-DATE-FUNCTIONS USING                                  00380000
380100         XXXN001A                                                 00380100
380200         YYYC0127                                                 00380200
380300     .                                                            00380300
380400                                                                  00380400
380500                                                                  00380500
380600 2050-GET-CURRENT-USER.                                           00380600
380700     IF  SUCCESS                                                  00380700
380800     AND YYYN005A-CICS-ENV                                        00380800
380900       CALL Z-GET-CICS-USER-ID USING                              00380900
381000           EIBLK    WS-DUMMY                                      00381000
381100           XXXN001A YYYC0107                                      00381100
381200     ELSE                                                         00381200
381300       MOVE 'BATCH' TO YYYC0107-USER                              00381300
381400     END-IF                                                       00381400
381500     .                                                            00381500
381600                                                                  00381600
381700* ================================================================00381700
381800* Updating the xxxation table                                     00381800
381900* ================================================================00381900
382000 2100-UPD-STAT-IN-xxxation.                                       00382000
382100     MOVE NNNN0000-EXIT-CODES TO WS-NNNN0000-EXIT-CODES           00382100
382200                                                                  00382200
382300     PERFORM 2200-GET-xxxation                                    00382300
382400     IF SUCCESS                                                   00382400
382500       PERFORM 2300-SYNC-STAT-CODE                                00382500
382600     END-IF                                                       00382600
382700                                                                  00382700
382800     MOVE WS-NNNN0000-EXIT-CODES TO NNNN0000-EXIT-CODES           00382800
382900     .                                                            00382900
383000                                                                  00383000
383100                                                                  00383100
383200* ================================================================00383200
383300* Passing the keys and fetching the values from xxxation table    00383300
383400* ================================================================00383400
383500 2200-GET-xxxation.                                               00383500
383600     INITIALIZE P-DDDTLO01                                        00383600
383700     MOVE LOC-NBR                    OF DCLXXXAIL-LOC             00383700
383800       TO LOC-NBR                    OF P-DDDTLO01                00383800
383900     MOVE LOC-TYP-CD                 OF DCLXXXAIL-LOC             00383900
384000       TO LOC-TYP-CD                 OF P-DDDTLO01                00384000
384100                                                                  00384100
384200     SET EXIT-GET-UNIQUE-ROW         TO TRUE                      00384200
384300     PERFORM 3000-CALL-LO-DAO                                     00384300
384400                                                                  00384400
384500     EVALUATE TRUE                                                00384500
384600         WHEN NOT SUCCESS                                         00384600
384700           CONTINUE                                               00384700
384800         WHEN SQLCODE = 0                                         00384800
384900           CONTINUE                                               00384900
385000         WHEN OTHER                                               00385000
385100           SET FAILURE               TO TRUE                      00385100
385200           MOVE SPACES               TO IS-RTRN-MSG-TXT           00385200
385300           MOVE SQLCODE              TO WS-SQLCODE                00385300
385400           STRING 'NNNS0488 - Get unique error on xxxation table' 00385400
385500                  'SQL = ' WS-SQLCODE                             00385500
385600           DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                 00385600
385700     END-EVALUATE                                                 00385700
385800     .                                                            00385800
385900                                                                  00385900
386000                                                                  00386000
386100* ================================================================00386100
386200* Sync the status flags on with xxxail Loc & xxxation tables      00386200
386300* ================================================================00386300
386400 2300-SYNC-STAT-CODE.                                             00386400
386500     EVALUATE TRUE                                                00386500
386600       WHEN RETL-LOC-STAT-CD OF DCLXXXAIL-LOC = 'A'               00386600
386700          MOVE 'A'           TO INACTIVE-SW OF P-DDDTLO01         00386700
386800       WHEN RETL-LOC-STAT-CD OF DCLXXXAIL-LOC = 'X'               00386800
386900          MOVE 'I'           TO INACTIVE-SW OF P-DDDTLO01         00386900
387000       WHEN RETL-LOC-STAT-CD OF DCLXXXAIL-LOC = SPACES            00387000
387100          MOVE 'I'           TO INACTIVE-SW OF P-DDDTLO01         00387100
387200       WHEN OTHER                                                 00387200
387300          MOVE 'I'           TO INACTIVE-SW OF P-DDDTLO01         00387300
387400     END-EVALUATE                                                 00387400
387500                                                                  00387500
387600     SET EXIT-PUT-MODIFY-ROW         TO TRUE                      00387600
387700     PERFORM 3000-CALL-LO-DAO                                     00387700
387800                                                                  00387800
387900     EVALUATE TRUE                                                00387900
388000         WHEN NOT SUCCESS                                         00388000
388100           CONTINUE                                               00388100
388200         WHEN SQLCODE = 0                                         00388200
388300           CONTINUE                                               00388300
388400         WHEN OTHER                                               00388400
388500           SET FAILURE               TO TRUE                      00388500
388600           MOVE SPACES               TO IS-RTRN-MSG-TXT           00388600
388700           MOVE SQLCODE              TO WS-SQLCODE                00388700
388800           STRING 'NNNS0488 - Modify error on xxxation table'     00388800
388900                  'SQL = ' WS-SQLCODE                             00388900
389000           DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                 00389000
389100     END-EVALUATE                                                 00389100
389200     .                                                            00389200
389300                                                                  00389300
389400                                                                  00389400
389500* ================================================================00389500
389600* Deals and cost may change if these fields changed on update...  00389600
389700* ================================================================00389700
389800 2400-CHECK-FOR-DCM-EVENT.                                        00389800
389900     IF WS-CURR-LOB     NOT = LIN-OF-BUS-ID  OF DCLXXXAIL-LOC     00389900
390400         PERFORM 2410-SETUP-EVENT                                 00390400
390400         PERFORM 2430-ISSUE-EVENT2                                00390500
390600     END-IF                                                       00390600
390700     .                                                            00390700
390800                                                                  00390800
390900                                                                  00390900
391000 2410-SETUP-EVENT.                                                00391000
391100     INITIALIZE ZZZC0210                                          00391100
391300     MOVE YYYC0127-TS TO EVENT-TS OF ZZZC0210                     00391300
391400                                                                  00391400
391500     MOVE LOC-TYP-CD       OF DCLXXXAIL-LOC                       00391500
391600       TO LOC-TYP-CD       OF ZZZC0210                            00391600
391700     MOVE LOC-NBR          OF DCLXXXAIL-LOC                       00391700
391800       TO LOC-NBR          OF ZZZC0210                            00391800
391900     MOVE WS-CURR-LOB                                             00391900
392000       TO O-LIN-OF-BUS-ID  OF ZZZC0210                            00392000
392100     MOVE WS-CURR-AD-ZONE                                         00392100
392200       TO O-CURR-AD-ZN-NBR OF ZZZC0210                            00392200
392300     MOVE LIN-OF-BUS-ID    OF DCLXXXAIL-LOC                       00392300
392400       TO N-LIN-OF-BUS-ID  OF ZZZC0210                            00392400
392500     MOVE CURR-AD-ZN-NBR   OF DCLXXXAIL-LOC                       00392500
392600       TO N-CURR-AD-ZN-NBR OF ZZZC0210                            00392600
392700     .                                                            00392700
392800                                                                  00392800
392900                                                                  00392900
393000 2420-ISSUE-EVENT.                                                00393000
393100     INITIALIZE YYYC0175                                          00393100
393200     SET YYYC0175-ORACLE      TO TRUE                             00393200
393300     IF YYYN005A-CICS-ENV                                         00393300
393400       SET YYYC0175-CICS-ENV  TO TRUE                             00393400
393500     ELSE                                                         00393500
393600       SET YYYC0175-BATCH-ENV TO TRUE                             00393600
393700     END-IF                                                       00393700
393800                                                                  00393800
393900     MOVE ZZZC0210-TRX-ID TO YYYC0175-TRX-CD                      00393900
394000     MOVE ZZZC0210        TO YYYC0175-DATA                        00394000
394100     MOVE 'M'             TO YYYC0175-ACTION-CD                   00394100
394200                                                                  00394200
394300     MOVE 'NNNS0488'      TO YYYC0175-CALLING-PROG                00394300
394400     MOVE YYYC0107-USER   TO YYYC0175-CALLING-USER                00394400
394500                                                                  00394500
394600     SET  YYYC0175-SOURCE-MAINFRAME TO TRUE                       00394600
394700     SET  YYYC0175-TARGET-WMS       TO TRUE                       00394700
394800                                                                  00394800
394900     CALL Z-EVENT-STAGER USING                                    00394900
395000         XXXN001A                                                 00395000
395100         YYYC0175                                                 00395100
395200     .                                                            00395200
395300                                                                  00395300
395400                                                                  00395400
395500 2430-ISSUE-EVENT2.                                               00395500
395600     MOVE 'SLO2' TO ZZZC0210-TRX-ID                               00395600
395700     PERFORM 2420-ISSUE-EVENT                                     00395700
395800     .                                                            00395800
395900                                                                  00395900
396000                                                                  00396000
396100* ================================================================00396100
396200* xxxation table DAO is called here.....                          00396200
396300* ================================================================00396300
396400 3000-CALL-LO-DAO.                                                00396400
396500     CALL NNNS0487-LO-DAO USING                                   00396500
396600          XXXN001A                                                00396600
396700          SQLCA                                                   00396700
396800          YYYN005A                                                00396800
396900          NNNN0000-PARMS                                          00396900
397000          P-DDDTLO01                                              00397000
397100     .                                                            00397100
397200                                                                  00397200
397300                                                                  00397300
397400 4500-CALL-MMMS0304-RI-DEL-CHK.                                   00397400
397500     INITIALIZE MMMC0304                                          00397500
397600     MOVE LOC-TYP-CD OF DCLXXXAIL-LOC                             00397600
397700                                 TO MMMC0304-LOC-TYP-CD           00397700
397800     MOVE LOC-NBR OF DCLXXXAIL-LOC                                00397800
397900                                 TO MMMC0304-LOC-NBR              00397900
398000     SET   MMMC0304-DELETE-CHECK TO TRUE                          00398000
398100     SET   MMMC0304-XXXAIL-LOC   TO TRUE                          00398100
398200     SET   MMMC0304-ORACLE       TO TRUE                          00398200
398300     CALL  MMMS0304-RI-DEL-CHK   USING                            00398300
398400              XXXN001A                                            00398400
398500              MMMC0304                                            00398500
398600     .                                                            00398600
398700                                                                  00398700
398800                                                                  00398800
398900 4600-CALL-MMMS0335-RI-ADD-CHK.                                   00398900
399000     INITIALIZE MMMC0335                                          00399000
399100     MOVE ECOMM-MKT-AREA-CD OF DCLXXXAIL-LOC                      00399100
399200                                 TO MMMC0335-ECOMM-MKT-AREA-CD    00399200
399300     MOVE CMPTR-TYP-CD  OF DCLXXXAIL-LOC                          00399300
399400                                 TO MMMC0335-CMPTR-TYP-CD         00399400
399500     SET   MMMC0335-INSERT-CHECK TO TRUE                          00399500
399600     SET   MMMC0335-XXXAIL-LOC   TO TRUE                          00399600
399700     SET   MMMC0335-ORACLE       TO TRUE                          00399700
399800     CALL  MMMC0335-RI-INSERT-CHK USING                           00399800
399900           XXXN001A                                               00399900
400000           MMMC0335                                               00400000
400100     .                                                            00400100
400200                                                                  00400200
400300                                                                  00400300
400400 4670-REP-LOWVALUE-WITH-SPACES.                                   00400400
400500     IF SRS-DSD-ORD-SW     OF DCLXXXAIL-LOC EQUAL LOW-VALUES      00400500
400600        MOVE SPACES        TO SRS-DSD-ORD-SW OF DCLXXXAIL-LOC     00400600
400700     END-IF                                                       00400700
400800     .                                                            00400800
400900                                                                  00400900
401000                                                                  00401000
401100* =========================================================       00401100
401200* Call Oracle table update program                                00401200
401300* =========================================================       00401300
401400 5000-CALL-NNNU0488-CUD-ROUTINE.                                  00401400
401500     CALL NNNU0488-ORACLE-UPDATE  USING                           00401500
401600          XXXN001A                                                00401600
401700          SQLCA                                                   00401700
401800          YYYN005A                                                00401800
401900          NNNN0000-PARMS                                          00401900
402000          DCLXXXAIL-LOC                                           00402000
402100          WS-TIME-FIELDS                                          00402100
402200          WS-ASSOC-ST-TYPE-IND                                    00402200
402300          WS-ASSOC-ST-NO-IND                                      00402300
402400          DCLXXXAIL-LOC-IND                                       00402400
402500     .                                                            00402500
402600                                                                  00402600
402700                                                                  00402700
402800* ================================================================00402800
402900* Special sql or functions to be performed by this subroutine.    00402900
403000* ================================================================00403000
403100 10000-DO-SPECIAL-IO-FUNCS.                                       00403100
403200     EXIT                                                         00403200
403300     .                                                            00403300