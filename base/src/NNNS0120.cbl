000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    NNNS0120.                                         00000200
000300 AUTHOR.        Name.                                             00000300
000400 DATE-WRITTEN.  Circa, 1600.                                      00000400
000500 DATE-COMPILED.                                                   00000500
002700* ----------------------------------------------------------      00002700
002800 ENVIRONMENT DIVISION.                                            00002800
002900 CONFIGURATION SECTION.                                           00002900
003000 DATA DIVISION.                                                   00003000
003100 WORKING-STORAGE SECTION.                                         00003100
003200                                                                  00003200
003300* ========================< MISC STUFF >==========================00003300
003400* Misc working storage variables go here.                         00003400
003500* ================================================================00003500
003600 01 WS-CHECKPOINT-INC PIC S9(4) COMP VALUE 0.                     00003600
003700 01 WS-SQLCODE        PIC ---9.                                   00003700
003800                                                                  00003800
003900 01 WS-LOGICALS.                                                  00003900
004000     05 WS-REPT-TBL-TXT                PIC X(20).                 00004000
004100     05 WS-REPT-TBL-NUMERIC REDEFINES WS-REPT-TBL-TXT             00004100
004200                                       PIC S9(3) COMP-3 OCCURS    00004200
004300                                       10 TIMES.                  00004300
004400                                                                  00004400
004500 01 MMMS0304-RI-DEL-CHK                PIC X(8) VALUE 'MMMS0304'. 00004500
004600* ========================< COPY BOOKS >==========================00004600
004700* Place all copy books in this section.                           00004700
004800* ================================================================00004800
004900 COPY NNNN000U.                                                   00004900
005000 COPY HHHTRL01.                                                   00005000
005100 COPY YYYN000A.                                                   00005100
005200 COPY MMMC0304.                                                   00005200
005300                                                                  00005300
005400* ========================< DCL GENS >============================00005400
005500* Place all DCL gens in this section.                             00005500
005600* ================================================================00005600
005700     EXEC SQL                                                     00005700
005800       INCLUDE DDDTRL01                                           00005800
005900     END-EXEC.                                                    00005900
006000                                                                  00006000
006100* ========================< CURSORS >=============================00006100
006200* Place all cursors in this section.                              00006200
006300* ================================================================00006300
006400                                                                  00006400
006500* --------------------------------------------------              00006500
006600* RFCXRL01 cursor declaration.                                    00006600
006700* --------------------------------------------------              00006700
006800     EXEC SQL                                                     00006800
006900         DECLARE RFCXRL01 CURSOR FOR SELECT                       00006900
007000             FC_STORE_NO,                                         00007000
007100             FC_RL_STORE_NM,                                      00007100
007200             FC_RL_STORE_CD,                                      00007200
007300             FC_RL_STORE_DIR_NM,                                  00007300
007400             FC_RL_STORE_LOC_NM,                                  00007400
007500             FC_RL_OPENING_DT,                                    00007500
007600             FC_RL_CLOSING_DT,                                    00007600
007700             FC_RL_REMODEL_DT,                                    00007700
007800             FC_RL_DELETED_DT,                                    00007800
007900             FC_RL_DISTRICT_NO,                                   00007900
008000             FC_RL_MARKET_AR_NO,                                  00008000
008100             FC_RL_PAYROL_AR_NO,                                  00008100
008200             FC_RL_PAY_GROUP_NO,                                  00008200
008300             FC_RL_COMPANY_NO,                                    00008300
008400             FC_RL_GEO_ZONE_CD,                                   00008400
008500             FC_RL_GEO_ZONE_NO,                                   00008500
008600             FC_RL_SCAN_MAIN_CD,                                  00008600
008700             FC_RL_FRONT_END_CD,                                  00008700
008800             FC_RL_PRICE_BUL_CD,                                  00008800
008900             FC_RL_UPC_ON_PB_CD,                                  00008900
009000             FC_RL_COMPETITR_CD,                                  00009000
009100             FC_RL_ASSOC_STR_NO,                                  00009100
009200             FC_RL_RPRT_SEQ_NO,                                   00009200
009300             FC_RL_SORT_SOS_NO,                                   00009300
009400             FC_RL_VID_PRZN_NO,                                   00009400
009500             FC_RL_CITY_ID_CD,                                    00009500
009600             FC_RL_ADZONE_ABB,                                    00009600
009700             FC_RL_ADZONE_DES,                                    00009700
009800             FC_RL_UNLOAD_SW,                                     00009800
009900             FN_ROLLUP_REPT_CD,                                   00009900
010000             FC_RL_STATUS_CD,                                     00010000
010100             FC_RL_NEW_STORE_CD,                                  00010100
010200             FC_RL_TYPE_CD,                                       00010200
010300             FC_RL_GROUP_CD,                                      00010300
010400             FC_RL_SELECTCIR_CD,                                  00010400
010500             FC_RL_AREA_CODE_NO,                                  00010500
010600             FC_RL_TELEPHONE_NO,                                  00010600
010700             FC_RL_STORE_ABB,                                     00010700
010800             FC_RL_BCKRM_FT_QTY,                                  00010800
010900             FC_RL_LFT_FOOD_QTY,                                  00010900
011000             FC_RL_LFT_NONF_QTY,                                  00011000
011100             FC_RL_SETOFF_CD,                                     00011100
011200             FC_RL_CL12_ZONE_NO,                                  00011200
011300             FC_RL_CL12_ADZN_NO,                                  00011300
011400             FC_RL_CL13_ZONE_NO,                                  00011400
011500             FC_RL_CL13_ADZN_NO,                                  00011500
011600             FC_RL_CL14_ZONE_NO,                                  00011600
011700             FC_RL_CL14_ADZN_NO,                                  00011700
011800             FC_RL_CL36_ADZN_NO,                                  00011800
011900             FC_RL_CL37_ADZN_NO,                                  00011900
012000             FC_RL_STORE_DEA_NO,                                  00012000
012100             FC_RL_RETL_ZONE_NO,                                  00012100
012200             FC_RL_STOR2_LOC_NM,                                  00012200
012300             FC_CITY_ADR,                                         00012300
012400             FC_STATE_ADR,                                        00012400
012500             FC_ZIP_CODE5_ADR,                                    00012500
012600             FC_ZIP_CODE4_ADR,                                    00012600
012700             FC_RL_SOS_TYPE_CD,                                   00012700
012800             FC_RL_NOPROCESS_CD,                                  00012800
012900             FC_RL_SOSHDRTYP_CD,                                  00012900
013000             FC_RL_CAT_CLASS_TB,                                  00013000
013100             FC_RL_LATITUDE_K,                                    00013100
013200             FC_RL_LONGITUDE_K,                                   00013200
013300             FN_DIVISION_CD,                                      00013300
013400             FN_LINE_OF_BUS_CD,                                   00013400
013500             FN_ROLLUP_REPT_01_NBR,                               00013500
013600             FN_ROLLUP_REPT_02_NBR,                               00013600
013700             FN_ROLLUP_REPT_03_NBR,                               00013700
013800             FN_ROLLUP_REPT_04_NBR,                               00013800
013900             FN_ROLLUP_REPT_05_NBR,                               00013900
014000             FN_ROLLUP_REPT_06_NBR,                               00014000
014100             FN_ROLLUP_REPT_07_NBR,                               00014100
014200             FN_ROLLUP_REPT_08_NBR,                               00014200
014300             FN_ROLLUP_REPT_09_NBR,                               00014300
014400             FN_ROLLUP_REPT_10_NBR                                00014400
014500         FROM     FC_XXXAIL_STORES                                00014500
014600         WHERE   (FC_STORE_NO >= :DCLFC-XXXAIL-STORES.FC-STORE-NO)00014600
014700         ORDER BY                                                 00014700
014800             FC_STORE_NO                                          00014800
014900     END-EXEC.                                                    00014900
015000* --------------------------------------------------              00015000
015100* RFCXRL02 cursor declaration.                                    00015100
015200* --------------------------------------------------              00015200
015300     EXEC SQL                                                     00015300
015400         DECLARE RFCXRL02 CURSOR FOR SELECT                       00015400
015500             FC_STORE_NO,                                         00015500
015600             FC_RL_STORE_NM,                                      00015600
015700             FC_RL_STORE_CD,                                      00015700
015800             FC_RL_STORE_DIR_NM,                                  00015800
015900             FC_RL_STORE_LOC_NM,                                  00015900
016000             FC_RL_OPENING_DT,                                    00016000
016100             FC_RL_CLOSING_DT,                                    00016100
016200             FC_RL_REMODEL_DT,                                    00016200
016300             FC_RL_DELETED_DT,                                    00016300
016400             FC_RL_DISTRICT_NO,                                   00016400
016500             FC_RL_MARKET_AR_NO,                                  00016500
016600             FC_RL_PAYROL_AR_NO,                                  00016600
016700             FC_RL_PAY_GROUP_NO,                                  00016700
016800             FC_RL_COMPANY_NO,                                    00016800
016900             FC_RL_GEO_ZONE_CD,                                   00016900
017000             FC_RL_GEO_ZONE_NO,                                   00017000
017100             FC_RL_SCAN_MAIN_CD,                                  00017100
017200             FC_RL_FRONT_END_CD,                                  00017200
017300             FC_RL_PRICE_BUL_CD,                                  00017300
017400             FC_RL_UPC_ON_PB_CD,                                  00017400
017500             FC_RL_COMPETITR_CD,                                  00017500
017600             FC_RL_ASSOC_STR_NO,                                  00017600
017700             FC_RL_RPRT_SEQ_NO,                                   00017700
017800             FC_RL_SORT_SOS_NO,                                   00017800
017900             FC_RL_VID_PRZN_NO,                                   00017900
018000             FC_RL_CITY_ID_CD,                                    00018000
018100             FC_RL_ADZONE_ABB,                                    00018100
018200             FC_RL_ADZONE_DES,                                    00018200
018300             FC_RL_UNLOAD_SW,                                     00018300
018400             FN_ROLLUP_REPT_CD,                                   00018400
018500             FC_RL_STATUS_CD,                                     00018500
018600             FC_RL_NEW_STORE_CD,                                  00018600
018700             FC_RL_TYPE_CD,                                       00018700
018800             FC_RL_GROUP_CD,                                      00018800
018900             FC_RL_SELECTCIR_CD,                                  00018900
019000             FC_RL_AREA_CODE_NO,                                  00019000
019100             FC_RL_TELEPHONE_NO,                                  00019100
019200             FC_RL_STORE_ABB,                                     00019200
019300             FC_RL_BCKRM_FT_QTY,                                  00019300
019400             FC_RL_LFT_FOOD_QTY,                                  00019400
019500             FC_RL_LFT_NONF_QTY,                                  00019500
019600             FC_RL_SETOFF_CD,                                     00019600
019700             FC_RL_CL12_ZONE_NO,                                  00019700
019800             FC_RL_CL12_ADZN_NO,                                  00019800
019900             FC_RL_CL13_ZONE_NO,                                  00019900
020000             FC_RL_CL13_ADZN_NO,                                  00020000
020100             FC_RL_CL14_ZONE_NO,                                  00020100
020200             FC_RL_CL14_ADZN_NO,                                  00020200
020300             FC_RL_CL36_ADZN_NO,                                  00020300
020400             FC_RL_CL37_ADZN_NO,                                  00020400
020500             FC_RL_STORE_DEA_NO,                                  00020500
020600             FC_RL_RETL_ZONE_NO,                                  00020600
020700             FC_RL_STOR2_LOC_NM,                                  00020700
020800             FC_CITY_ADR,                                         00020800
020900             FC_STATE_ADR,                                        00020900
021000             FC_ZIP_CODE5_ADR,                                    00021000
021100             FC_ZIP_CODE4_ADR,                                    00021100
021200             FC_RL_SOS_TYPE_CD,                                   00021200
021300             FC_RL_NOPROCESS_CD,                                  00021300
021400             FC_RL_SOSHDRTYP_CD,                                  00021400
021500             FC_RL_CAT_CLASS_TB,                                  00021500
021600             FC_RL_LATITUDE_K,                                    00021600
021700             FC_RL_LONGITUDE_K,                                   00021700
021800             FN_DIVISION_CD,                                      00021800
021900             FN_LINE_OF_BUS_CD,                                   00021900
022000             FN_ROLLUP_REPT_01_NBR,                               00022000
022100             FN_ROLLUP_REPT_02_NBR,                               00022100
022200             FN_ROLLUP_REPT_03_NBR,                               00022200
022300             FN_ROLLUP_REPT_04_NBR,                               00022300
022400             FN_ROLLUP_REPT_05_NBR,                               00022400
022500             FN_ROLLUP_REPT_06_NBR,                               00022500
022600             FN_ROLLUP_REPT_07_NBR,                               00022600
022700             FN_ROLLUP_REPT_08_NBR,                               00022700
022800             FN_ROLLUP_REPT_09_NBR,                               00022800
022900             FN_ROLLUP_REPT_10_NBR                                00022900
023000         FROM     FC_XXXAIL_STORES                                00023000
023100         WHERE   (FC_RL_STORE_NM >=                               00023100
023200               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM)               00023200
023300         AND NOT                                                  00023300
023400                 (FC_RL_STORE_NM  =                               00023400
023500                       :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM AND    00023500
023600                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00023600
023700         ORDER BY                                                 00023700
023800             FC_RL_STORE_NM,                                      00023800
023900             FC_STORE_NO                                          00023900
024000     END-EXEC.                                                    00024000
024100* --------------------------------------------------              00024100
024200* RFCXRL03 cursor declaration.                                    00024200
024300* --------------------------------------------------              00024300
024400     EXEC SQL                                                     00024400
024500         DECLARE RFCXRL03 CURSOR FOR SELECT                       00024500
024600             FC_STORE_NO,                                         00024600
024700             FC_RL_STORE_NM,                                      00024700
024800             FC_RL_STORE_CD,                                      00024800
024900             FC_RL_STORE_DIR_NM,                                  00024900
025000             FC_RL_STORE_LOC_NM,                                  00025000
025100             FC_RL_OPENING_DT,                                    00025100
025200             FC_RL_CLOSING_DT,                                    00025200
025300             FC_RL_REMODEL_DT,                                    00025300
025400             FC_RL_DELETED_DT,                                    00025400
025500             FC_RL_DISTRICT_NO,                                   00025500
025600             FC_RL_MARKET_AR_NO,                                  00025600
025700             FC_RL_PAYROL_AR_NO,                                  00025700
025800             FC_RL_PAY_GROUP_NO,                                  00025800
025900             FC_RL_COMPANY_NO,                                    00025900
026000             FC_RL_GEO_ZONE_CD,                                   00026000
026100             FC_RL_GEO_ZONE_NO,                                   00026100
026200             FC_RL_SCAN_MAIN_CD,                                  00026200
026300             FC_RL_FRONT_END_CD,                                  00026300
026400             FC_RL_PRICE_BUL_CD,                                  00026400
026500             FC_RL_UPC_ON_PB_CD,                                  00026500
026600             FC_RL_COMPETITR_CD,                                  00026600
026700             FC_RL_ASSOC_STR_NO,                                  00026700
026800             FC_RL_RPRT_SEQ_NO,                                   00026800
026900             FC_RL_SORT_SOS_NO,                                   00026900
027000             FC_RL_VID_PRZN_NO,                                   00027000
027100             FC_RL_CITY_ID_CD,                                    00027100
027200             FC_RL_ADZONE_ABB,                                    00027200
027300             FC_RL_ADZONE_DES,                                    00027300
027400             FC_RL_UNLOAD_SW,                                     00027400
027500             FN_ROLLUP_REPT_CD,                                   00027500
027600             FC_RL_STATUS_CD,                                     00027600
027700             FC_RL_NEW_STORE_CD,                                  00027700
027800             FC_RL_TYPE_CD,                                       00027800
027900             FC_RL_GROUP_CD,                                      00027900
028000             FC_RL_SELECTCIR_CD,                                  00028000
028100             FC_RL_AREA_CODE_NO,                                  00028100
028200             FC_RL_TELEPHONE_NO,                                  00028200
028300             FC_RL_STORE_ABB,                                     00028300
028400             FC_RL_BCKRM_FT_QTY,                                  00028400
028500             FC_RL_LFT_FOOD_QTY,                                  00028500
028600             FC_RL_LFT_NONF_QTY,                                  00028600
028700             FC_RL_SETOFF_CD,                                     00028700
028800             FC_RL_CL12_ZONE_NO,                                  00028800
028900             FC_RL_CL12_ADZN_NO,                                  00028900
029000             FC_RL_CL13_ZONE_NO,                                  00029000
029100             FC_RL_CL13_ADZN_NO,                                  00029100
029200             FC_RL_CL14_ZONE_NO,                                  00029200
029300             FC_RL_CL14_ADZN_NO,                                  00029300
029400             FC_RL_CL36_ADZN_NO,                                  00029400
029500             FC_RL_CL37_ADZN_NO,                                  00029500
029600             FC_RL_STORE_DEA_NO,                                  00029600
029700             FC_RL_RETL_ZONE_NO,                                  00029700
029800             FC_RL_STOR2_LOC_NM,                                  00029800
029900             FC_CITY_ADR,                                         00029900
030000             FC_STATE_ADR,                                        00030000
030100             FC_ZIP_CODE5_ADR,                                    00030100
030200             FC_ZIP_CODE4_ADR,                                    00030200
030300             FC_RL_SOS_TYPE_CD,                                   00030300
030400             FC_RL_NOPROCESS_CD,                                  00030400
030500             FC_RL_SOSHDRTYP_CD,                                  00030500
030600             FC_RL_CAT_CLASS_TB,                                  00030600
030700             FC_RL_LATITUDE_K,                                    00030700
030800             FC_RL_LONGITUDE_K,                                   00030800
030900             FN_DIVISION_CD,                                      00030900
031000             FN_LINE_OF_BUS_CD,                                   00031000
031100             FN_ROLLUP_REPT_01_NBR,                               00031100
031200             FN_ROLLUP_REPT_02_NBR,                               00031200
031300             FN_ROLLUP_REPT_03_NBR,                               00031300
031400             FN_ROLLUP_REPT_04_NBR,                               00031400
031500             FN_ROLLUP_REPT_05_NBR,                               00031500
031600             FN_ROLLUP_REPT_06_NBR,                               00031600
031700             FN_ROLLUP_REPT_07_NBR,                               00031700
031800             FN_ROLLUP_REPT_08_NBR,                               00031800
031900             FN_ROLLUP_REPT_09_NBR,                               00031900
032000             FN_ROLLUP_REPT_10_NBR                                00032000
032100         FROM     FC_XXXAIL_STORES                                00032100
032200         WHERE   (FC_RL_STORE_CD >=                               00032200
032300               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD)               00032300
032400         AND NOT                                                  00032400
032500                 (FC_RL_STORE_CD  =                               00032500
032600                       :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD AND    00032600
032700                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00032700
032800         ORDER BY                                                 00032800
032900             FC_RL_STORE_CD,                                      00032900
033000             FC_STORE_NO                                          00033000
033100     END-EXEC.                                                    00033100
033200* --------------------------------------------------              00033200
033300* RFCXRL04 cursor declaration.                                    00033300
033400* --------------------------------------------------              00033400
033500     EXEC SQL                                                     00033500
033600         DECLARE RFCXRL04 CURSOR FOR SELECT                       00033600
033700             FC_STORE_NO,                                         00033700
033800             FC_RL_STORE_NM,                                      00033800
033900             FC_RL_STORE_CD,                                      00033900
034000             FC_RL_STORE_DIR_NM,                                  00034000
034100             FC_RL_STORE_LOC_NM,                                  00034100
034200             FC_RL_OPENING_DT,                                    00034200
034300             FC_RL_CLOSING_DT,                                    00034300
034400             FC_RL_REMODEL_DT,                                    00034400
034500             FC_RL_DELETED_DT,                                    00034500
034600             FC_RL_DISTRICT_NO,                                   00034600
034700             FC_RL_MARKET_AR_NO,                                  00034700
034800             FC_RL_PAYROL_AR_NO,                                  00034800
034900             FC_RL_PAY_GROUP_NO,                                  00034900
035000             FC_RL_COMPANY_NO,                                    00035000
035100             FC_RL_GEO_ZONE_CD,                                   00035100
035200             FC_RL_GEO_ZONE_NO,                                   00035200
035300             FC_RL_SCAN_MAIN_CD,                                  00035300
035400             FC_RL_FRONT_END_CD,                                  00035400
035500             FC_RL_PRICE_BUL_CD,                                  00035500
035600             FC_RL_UPC_ON_PB_CD,                                  00035600
035700             FC_RL_COMPETITR_CD,                                  00035700
035800             FC_RL_ASSOC_STR_NO,                                  00035800
035900             FC_RL_RPRT_SEQ_NO,                                   00035900
036000             FC_RL_SORT_SOS_NO,                                   00036000
036100             FC_RL_VID_PRZN_NO,                                   00036100
036200             FC_RL_CITY_ID_CD,                                    00036200
036300             FC_RL_ADZONE_ABB,                                    00036300
036400             FC_RL_ADZONE_DES,                                    00036400
036500             FC_RL_UNLOAD_SW,                                     00036500
036600             FN_ROLLUP_REPT_CD,                                   00036600
036700             FC_RL_STATUS_CD,                                     00036700
036800             FC_RL_NEW_STORE_CD,                                  00036800
036900             FC_RL_TYPE_CD,                                       00036900
037000             FC_RL_GROUP_CD,                                      00037000
037100             FC_RL_SELECTCIR_CD,                                  00037100
037200             FC_RL_AREA_CODE_NO,                                  00037200
037300             FC_RL_TELEPHONE_NO,                                  00037300
037400             FC_RL_STORE_ABB,                                     00037400
037500             FC_RL_BCKRM_FT_QTY,                                  00037500
037600             FC_RL_LFT_FOOD_QTY,                                  00037600
037700             FC_RL_LFT_NONF_QTY,                                  00037700
037800             FC_RL_SETOFF_CD,                                     00037800
037900             FC_RL_CL12_ZONE_NO,                                  00037900
038000             FC_RL_CL12_ADZN_NO,                                  00038000
038100             FC_RL_CL13_ZONE_NO,                                  00038100
038200             FC_RL_CL13_ADZN_NO,                                  00038200
038300             FC_RL_CL14_ZONE_NO,                                  00038300
038400             FC_RL_CL14_ADZN_NO,                                  00038400
038500             FC_RL_CL36_ADZN_NO,                                  00038500
038600             FC_RL_CL37_ADZN_NO,                                  00038600
038700             FC_RL_STORE_DEA_NO,                                  00038700
038800             FC_RL_RETL_ZONE_NO,                                  00038800
038900             FC_RL_STOR2_LOC_NM,                                  00038900
039000             FC_CITY_ADR,                                         00039000
039100             FC_STATE_ADR,                                        00039100
039200             FC_ZIP_CODE5_ADR,                                    00039200
039300             FC_ZIP_CODE4_ADR,                                    00039300
039400             FC_RL_SOS_TYPE_CD,                                   00039400
039500             FC_RL_NOPROCESS_CD,                                  00039500
039600             FC_RL_SOSHDRTYP_CD,                                  00039600
039700             FC_RL_CAT_CLASS_TB,                                  00039700
039800             FC_RL_LATITUDE_K,                                    00039800
039900             FC_RL_LONGITUDE_K,                                   00039900
040000             FN_DIVISION_CD,                                      00040000
040100             FN_LINE_OF_BUS_CD,                                   00040100
040200             FN_ROLLUP_REPT_01_NBR,                               00040200
040300             FN_ROLLUP_REPT_02_NBR,                               00040300
040400             FN_ROLLUP_REPT_03_NBR,                               00040400
040500             FN_ROLLUP_REPT_04_NBR,                               00040500
040600             FN_ROLLUP_REPT_05_NBR,                               00040600
040700             FN_ROLLUP_REPT_06_NBR,                               00040700
040800             FN_ROLLUP_REPT_07_NBR,                               00040800
040900             FN_ROLLUP_REPT_08_NBR,                               00040900
041000             FN_ROLLUP_REPT_09_NBR,                               00041000
041100             FN_ROLLUP_REPT_10_NBR                                00041100
041200         FROM     FC_XXXAIL_STORES                                00041200
041300         WHERE   (FC_RL_STORE_CD >=                               00041300
041400               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD)               00041400
041500         AND NOT                                                  00041500
041600                 (FC_RL_STORE_CD  =                               00041600
041700                       :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD AND    00041700
041800                  FC_RL_STORE_NM  <                               00041800
041900                        :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM)      00041900
042000         AND NOT                                                  00042000
042100                 (FC_RL_STORE_CD  =                               00042100
042200                       :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD AND    00042200
042300                  FC_RL_STORE_NM  =                               00042300
042400                        :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM AND   00042400
042500                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00042500
042600         ORDER BY                                                 00042600
042700             FC_RL_STORE_CD,                                      00042700
042800             FC_RL_STORE_NM,                                      00042800
042900             FC_STORE_NO                                          00042900
043000     END-EXEC.                                                    00043000
043100* --------------------------------------------------              00043100
043200* RFCXRL05 cursor declaration.                                    00043200
043300* --------------------------------------------------              00043300
043400     EXEC SQL                                                     00043400
043500         DECLARE RFCXRL05 CURSOR FOR SELECT                       00043500
043600             FC_STORE_NO,                                         00043600
043700             FC_RL_STORE_NM,                                      00043700
043800             FC_RL_STORE_CD,                                      00043800
043900             FC_RL_STORE_DIR_NM,                                  00043900
044000             FC_RL_STORE_LOC_NM,                                  00044000
044100             FC_RL_OPENING_DT,                                    00044100
044200             FC_RL_CLOSING_DT,                                    00044200
044300             FC_RL_REMODEL_DT,                                    00044300
044400             FC_RL_DELETED_DT,                                    00044400
044500             FC_RL_DISTRICT_NO,                                   00044500
044600             FC_RL_MARKET_AR_NO,                                  00044600
044700             FC_RL_PAYROL_AR_NO,                                  00044700
044800             FC_RL_PAY_GROUP_NO,                                  00044800
044900             FC_RL_COMPANY_NO,                                    00044900
045000             FC_RL_GEO_ZONE_CD,                                   00045000
045100             FC_RL_GEO_ZONE_NO,                                   00045100
045200             FC_RL_SCAN_MAIN_CD,                                  00045200
045300             FC_RL_FRONT_END_CD,                                  00045300
045400             FC_RL_PRICE_BUL_CD,                                  00045400
045500             FC_RL_UPC_ON_PB_CD,                                  00045500
045600             FC_RL_COMPETITR_CD,                                  00045600
045700             FC_RL_ASSOC_STR_NO,                                  00045700
045800             FC_RL_RPRT_SEQ_NO,                                   00045800
045900             FC_RL_SORT_SOS_NO,                                   00045900
046000             FC_RL_VID_PRZN_NO,                                   00046000
046100             FC_RL_CITY_ID_CD,                                    00046100
046200             FC_RL_ADZONE_ABB,                                    00046200
046300             FC_RL_ADZONE_DES,                                    00046300
046400             FC_RL_UNLOAD_SW,                                     00046400
046500             FN_ROLLUP_REPT_CD,                                   00046500
046600             FC_RL_STATUS_CD,                                     00046600
046700             FC_RL_NEW_STORE_CD,                                  00046700
046800             FC_RL_TYPE_CD,                                       00046800
046900             FC_RL_GROUP_CD,                                      00046900
047000             FC_RL_SELECTCIR_CD,                                  00047000
047100             FC_RL_AREA_CODE_NO,                                  00047100
047200             FC_RL_TELEPHONE_NO,                                  00047200
047300             FC_RL_STORE_ABB,                                     00047300
047400             FC_RL_BCKRM_FT_QTY,                                  00047400
047500             FC_RL_LFT_FOOD_QTY,                                  00047500
047600             FC_RL_LFT_NONF_QTY,                                  00047600
047700             FC_RL_SETOFF_CD,                                     00047700
047800             FC_RL_CL12_ZONE_NO,                                  00047800
047900             FC_RL_CL12_ADZN_NO,                                  00047900
048000             FC_RL_CL13_ZONE_NO,                                  00048000
048100             FC_RL_CL13_ADZN_NO,                                  00048100
048200             FC_RL_CL14_ZONE_NO,                                  00048200
048300             FC_RL_CL14_ADZN_NO,                                  00048300
048400             FC_RL_CL36_ADZN_NO,                                  00048400
048500             FC_RL_CL37_ADZN_NO,                                  00048500
048600             FC_RL_STORE_DEA_NO,                                  00048600
048700             FC_RL_RETL_ZONE_NO,                                  00048700
048800             FC_RL_STOR2_LOC_NM,                                  00048800
048900             FC_CITY_ADR,                                         00048900
049000             FC_STATE_ADR,                                        00049000
049100             FC_ZIP_CODE5_ADR,                                    00049100
049200             FC_ZIP_CODE4_ADR,                                    00049200
049300             FC_RL_SOS_TYPE_CD,                                   00049300
049400             FC_RL_NOPROCESS_CD,                                  00049400
049500             FC_RL_SOSHDRTYP_CD,                                  00049500
049600             FC_RL_CAT_CLASS_TB,                                  00049600
049700             FC_RL_LATITUDE_K,                                    00049700
049800             FC_RL_LONGITUDE_K,                                   00049800
049900             FN_DIVISION_CD,                                      00049900
050000             FN_LINE_OF_BUS_CD,                                   00050000
050100             FN_ROLLUP_REPT_01_NBR,                               00050100
050200             FN_ROLLUP_REPT_02_NBR,                               00050200
050300             FN_ROLLUP_REPT_03_NBR,                               00050300
050400             FN_ROLLUP_REPT_04_NBR,                               00050400
050500             FN_ROLLUP_REPT_05_NBR,                               00050500
050600             FN_ROLLUP_REPT_06_NBR,                               00050600
050700             FN_ROLLUP_REPT_07_NBR,                               00050700
050800             FN_ROLLUP_REPT_08_NBR,                               00050800
050900             FN_ROLLUP_REPT_09_NBR,                               00050900
051000             FN_ROLLUP_REPT_10_NBR                                00051000
051100         FROM     FC_XXXAIL_STORES                                00051100
051200         WHERE   (FC_RL_STORE_DIR_NM >=                           00051200
051300               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM)           00051300
051400         AND NOT                                                  00051400
051500                 (FC_RL_STORE_DIR_NM  =                           00051500
051600                       :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM AND00051600
051700                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00051700
051800         ORDER BY                                                 00051800
051900             FC_RL_STORE_DIR_NM,                                  00051900
052000             FC_STORE_NO                                          00052000
052100     END-EXEC.                                                    00052100
052200* --------------------------------------------------              00052200
052300* RFCXRL06 cursor declaration.                                    00052300
052400* --------------------------------------------------              00052400
052500     EXEC SQL                                                     00052500
052600         DECLARE RFCXRL06 CURSOR FOR SELECT                       00052600
052700             FC_STORE_NO,                                         00052700
052800             FC_RL_STORE_NM,                                      00052800
052900             FC_RL_STORE_CD,                                      00052900
053000             FC_RL_STORE_DIR_NM,                                  00053000
053100             FC_RL_STORE_LOC_NM,                                  00053100
053200             FC_RL_OPENING_DT,                                    00053200
053300             FC_RL_CLOSING_DT,                                    00053300
053400             FC_RL_REMODEL_DT,                                    00053400
053500             FC_RL_DELETED_DT,                                    00053500
053600             FC_RL_DISTRICT_NO,                                   00053600
053700             FC_RL_MARKET_AR_NO,                                  00053700
053800             FC_RL_PAYROL_AR_NO,                                  00053800
053900             FC_RL_PAY_GROUP_NO,                                  00053900
054000             FC_RL_COMPANY_NO,                                    00054000
054100             FC_RL_GEO_ZONE_CD,                                   00054100
054200             FC_RL_GEO_ZONE_NO,                                   00054200
054300             FC_RL_SCAN_MAIN_CD,                                  00054300
054400             FC_RL_FRONT_END_CD,                                  00054400
054500             FC_RL_PRICE_BUL_CD,                                  00054500
054600             FC_RL_UPC_ON_PB_CD,                                  00054600
054700             FC_RL_COMPETITR_CD,                                  00054700
054800             FC_RL_ASSOC_STR_NO,                                  00054800
054900             FC_RL_RPRT_SEQ_NO,                                   00054900
055000             FC_RL_SORT_SOS_NO,                                   00055000
055100             FC_RL_VID_PRZN_NO,                                   00055100
055200             FC_RL_CITY_ID_CD,                                    00055200
055300             FC_RL_ADZONE_ABB,                                    00055300
055400             FC_RL_ADZONE_DES,                                    00055400
055500             FC_RL_UNLOAD_SW,                                     00055500
055600             FN_ROLLUP_REPT_CD,                                   00055600
055700             FC_RL_STATUS_CD,                                     00055700
055800             FC_RL_NEW_STORE_CD,                                  00055800
055900             FC_RL_TYPE_CD,                                       00055900
056000             FC_RL_GROUP_CD,                                      00056000
056100             FC_RL_SELECTCIR_CD,                                  00056100
056200             FC_RL_AREA_CODE_NO,                                  00056200
056300             FC_RL_TELEPHONE_NO,                                  00056300
056400             FC_RL_STORE_ABB,                                     00056400
056500             FC_RL_BCKRM_FT_QTY,                                  00056500
056600             FC_RL_LFT_FOOD_QTY,                                  00056600
056700             FC_RL_LFT_NONF_QTY,                                  00056700
056800             FC_RL_SETOFF_CD,                                     00056800
056900             FC_RL_CL12_ZONE_NO,                                  00056900
057000             FC_RL_CL12_ADZN_NO,                                  00057000
057100             FC_RL_CL13_ZONE_NO,                                  00057100
057200             FC_RL_CL13_ADZN_NO,                                  00057200
057300             FC_RL_CL14_ZONE_NO,                                  00057300
057400             FC_RL_CL14_ADZN_NO,                                  00057400
057500             FC_RL_CL36_ADZN_NO,                                  00057500
057600             FC_RL_CL37_ADZN_NO,                                  00057600
057700             FC_RL_STORE_DEA_NO,                                  00057700
057800             FC_RL_RETL_ZONE_NO,                                  00057800
057900             FC_RL_STOR2_LOC_NM,                                  00057900
058000             FC_CITY_ADR,                                         00058000
058100             FC_STATE_ADR,                                        00058100
058200             FC_ZIP_CODE5_ADR,                                    00058200
058300             FC_ZIP_CODE4_ADR,                                    00058300
058400             FC_RL_SOS_TYPE_CD,                                   00058400
058500             FC_RL_NOPROCESS_CD,                                  00058500
058600             FC_RL_SOSHDRTYP_CD,                                  00058600
058700             FC_RL_CAT_CLASS_TB,                                  00058700
058800             FC_RL_LATITUDE_K,                                    00058800
058900             FC_RL_LONGITUDE_K,                                   00058900
059000             FN_DIVISION_CD,                                      00059000
059100             FN_LINE_OF_BUS_CD,                                   00059100
059200             FN_ROLLUP_REPT_01_NBR,                               00059200
059300             FN_ROLLUP_REPT_02_NBR,                               00059300
059400             FN_ROLLUP_REPT_03_NBR,                               00059400
059500             FN_ROLLUP_REPT_04_NBR,                               00059500
059600             FN_ROLLUP_REPT_05_NBR,                               00059600
059700             FN_ROLLUP_REPT_06_NBR,                               00059700
059800             FN_ROLLUP_REPT_07_NBR,                               00059800
059900             FN_ROLLUP_REPT_08_NBR,                               00059900
060000             FN_ROLLUP_REPT_09_NBR,                               00060000
060100             FN_ROLLUP_REPT_10_NBR                                00060100
060200         FROM     FC_XXXAIL_STORES                                00060200
060300         WHERE   (FC_RL_OPENING_DT >=                             00060300
060400               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT)             00060400
060500         AND NOT                                                  00060500
060600                 (FC_RL_OPENING_DT  =                             00060600
060700                       :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT AND  00060700
060800                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00060800
060900         ORDER BY                                                 00060900
061000             FC_RL_OPENING_DT,                                    00061000
061100             FC_STORE_NO                                          00061100
061200     END-EXEC.                                                    00061200
061300* --------------------------------------------------              00061300
061400* RFCXRL07 cursor declaration.                                    00061400
061500* --------------------------------------------------              00061500
061600     EXEC SQL                                                     00061600
061700         DECLARE RFCXRL07 CURSOR FOR SELECT                       00061700
061800             FC_STORE_NO,                                         00061800
061900             FC_RL_STORE_NM,                                      00061900
062000             FC_RL_STORE_CD,                                      00062000
062100             FC_RL_STORE_DIR_NM,                                  00062100
062200             FC_RL_STORE_LOC_NM,                                  00062200
062300             FC_RL_OPENING_DT,                                    00062300
062400             FC_RL_CLOSING_DT,                                    00062400
062500             FC_RL_REMODEL_DT,                                    00062500
062600             FC_RL_DELETED_DT,                                    00062600
062700             FC_RL_DISTRICT_NO,                                   00062700
062800             FC_RL_MARKET_AR_NO,                                  00062800
062900             FC_RL_PAYROL_AR_NO,                                  00062900
063000             FC_RL_PAY_GROUP_NO,                                  00063000
063100             FC_RL_COMPANY_NO,                                    00063100
063200             FC_RL_GEO_ZONE_CD,                                   00063200
063300             FC_RL_GEO_ZONE_NO,                                   00063300
063400             FC_RL_SCAN_MAIN_CD,                                  00063400
063500             FC_RL_FRONT_END_CD,                                  00063500
063600             FC_RL_PRICE_BUL_CD,                                  00063600
063700             FC_RL_UPC_ON_PB_CD,                                  00063700
063800             FC_RL_COMPETITR_CD,                                  00063800
063900             FC_RL_ASSOC_STR_NO,                                  00063900
064000             FC_RL_RPRT_SEQ_NO,                                   00064000
064100             FC_RL_SORT_SOS_NO,                                   00064100
064200             FC_RL_VID_PRZN_NO,                                   00064200
064300             FC_RL_CITY_ID_CD,                                    00064300
064400             FC_RL_ADZONE_ABB,                                    00064400
064500             FC_RL_ADZONE_DES,                                    00064500
064600             FC_RL_UNLOAD_SW,                                     00064600
064700             FN_ROLLUP_REPT_CD,                                   00064700
064800             FC_RL_STATUS_CD,                                     00064800
064900             FC_RL_NEW_STORE_CD,                                  00064900
065000             FC_RL_TYPE_CD,                                       00065000
065100             FC_RL_GROUP_CD,                                      00065100
065200             FC_RL_SELECTCIR_CD,                                  00065200
065300             FC_RL_AREA_CODE_NO,                                  00065300
065400             FC_RL_TELEPHONE_NO,                                  00065400
065500             FC_RL_STORE_ABB,                                     00065500
065600             FC_RL_BCKRM_FT_QTY,                                  00065600
065700             FC_RL_LFT_FOOD_QTY,                                  00065700
065800             FC_RL_LFT_NONF_QTY,                                  00065800
065900             FC_RL_SETOFF_CD,                                     00065900
066000             FC_RL_CL12_ZONE_NO,                                  00066000
066100             FC_RL_CL12_ADZN_NO,                                  00066100
066200             FC_RL_CL13_ZONE_NO,                                  00066200
066300             FC_RL_CL13_ADZN_NO,                                  00066300
066400             FC_RL_CL14_ZONE_NO,                                  00066400
066500             FC_RL_CL14_ADZN_NO,                                  00066500
066600             FC_RL_CL36_ADZN_NO,                                  00066600
066700             FC_RL_CL37_ADZN_NO,                                  00066700
066800             FC_RL_STORE_DEA_NO,                                  00066800
066900             FC_RL_RETL_ZONE_NO,                                  00066900
067000             FC_RL_STOR2_LOC_NM,                                  00067000
067100             FC_CITY_ADR,                                         00067100
067200             FC_STATE_ADR,                                        00067200
067300             FC_ZIP_CODE5_ADR,                                    00067300
067400             FC_ZIP_CODE4_ADR,                                    00067400
067500             FC_RL_SOS_TYPE_CD,                                   00067500
067600             FC_RL_NOPROCESS_CD,                                  00067600
067700             FC_RL_SOSHDRTYP_CD,                                  00067700
067800             FC_RL_CAT_CLASS_TB,                                  00067800
067900             FC_RL_LATITUDE_K,                                    00067900
068000             FC_RL_LONGITUDE_K,                                   00068000
068100             FN_DIVISION_CD,                                      00068100
068200             FN_LINE_OF_BUS_CD,                                   00068200
068300             FN_ROLLUP_REPT_01_NBR,                               00068300
068400             FN_ROLLUP_REPT_02_NBR,                               00068400
068500             FN_ROLLUP_REPT_03_NBR,                               00068500
068600             FN_ROLLUP_REPT_04_NBR,                               00068600
068700             FN_ROLLUP_REPT_05_NBR,                               00068700
068800             FN_ROLLUP_REPT_06_NBR,                               00068800
068900             FN_ROLLUP_REPT_07_NBR,                               00068900
069000             FN_ROLLUP_REPT_08_NBR,                               00069000
069100             FN_ROLLUP_REPT_09_NBR,                               00069100
069200             FN_ROLLUP_REPT_10_NBR                                00069200
069300         FROM     FC_XXXAIL_STORES                                00069300
069400         WHERE   (FC_RL_OPENING_DT >=                             00069400
069500               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT)             00069500
069600         AND NOT                                                  00069600
069700                 (FC_RL_OPENING_DT  =                             00069700
069800                       :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT AND  00069800
069900                  FC_RL_STORE_NM  <                               00069900
070000                        :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM)      00070000
070100         AND NOT                                                  00070100
070200                 (FC_RL_OPENING_DT  =                             00070200
070300                       :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT AND  00070300
070400                  FC_RL_STORE_NM  =                               00070400
070500                        :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM AND   00070500
070600                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00070600
070700         ORDER BY                                                 00070700
070800             FC_RL_OPENING_DT,                                    00070800
070900             FC_RL_STORE_NM,                                      00070900
071000             FC_STORE_NO                                          00071000
071100     END-EXEC.                                                    00071100
071200* --------------------------------------------------              00071200
071300* RFCXRL08 cursor declaration.                                    00071300
071400* --------------------------------------------------              00071400
071500     EXEC SQL                                                     00071500
071600         DECLARE RFCXRL08 CURSOR FOR SELECT                       00071600
071700             FC_STORE_NO,                                         00071700
071800             FC_RL_STORE_NM,                                      00071800
071900             FC_RL_STORE_CD,                                      00071900
072000             FC_RL_STORE_DIR_NM,                                  00072000
072100             FC_RL_STORE_LOC_NM,                                  00072100
072200             FC_RL_OPENING_DT,                                    00072200
072300             FC_RL_CLOSING_DT,                                    00072300
072400             FC_RL_REMODEL_DT,                                    00072400
072500             FC_RL_DELETED_DT,                                    00072500
072600             FC_RL_DISTRICT_NO,                                   00072600
072700             FC_RL_MARKET_AR_NO,                                  00072700
072800             FC_RL_PAYROL_AR_NO,                                  00072800
072900             FC_RL_PAY_GROUP_NO,                                  00072900
073000             FC_RL_COMPANY_NO,                                    00073000
073100             FC_RL_GEO_ZONE_CD,                                   00073100
073200             FC_RL_GEO_ZONE_NO,                                   00073200
073300             FC_RL_SCAN_MAIN_CD,                                  00073300
073400             FC_RL_FRONT_END_CD,                                  00073400
073500             FC_RL_PRICE_BUL_CD,                                  00073500
073600             FC_RL_UPC_ON_PB_CD,                                  00073600
073700             FC_RL_COMPETITR_CD,                                  00073700
073800             FC_RL_ASSOC_STR_NO,                                  00073800
073900             FC_RL_RPRT_SEQ_NO,                                   00073900
074000             FC_RL_SORT_SOS_NO,                                   00074000
074100             FC_RL_VID_PRZN_NO,                                   00074100
074200             FC_RL_CITY_ID_CD,                                    00074200
074300             FC_RL_ADZONE_ABB,                                    00074300
074400             FC_RL_ADZONE_DES,                                    00074400
074500             FC_RL_UNLOAD_SW,                                     00074500
074600             FN_ROLLUP_REPT_CD,                                   00074600
074700             FC_RL_STATUS_CD,                                     00074700
074800             FC_RL_NEW_STORE_CD,                                  00074800
074900             FC_RL_TYPE_CD,                                       00074900
075000             FC_RL_GROUP_CD,                                      00075000
075100             FC_RL_SELECTCIR_CD,                                  00075100
075200             FC_RL_AREA_CODE_NO,                                  00075200
075300             FC_RL_TELEPHONE_NO,                                  00075300
075400             FC_RL_STORE_ABB,                                     00075400
075500             FC_RL_BCKRM_FT_QTY,                                  00075500
075600             FC_RL_LFT_FOOD_QTY,                                  00075600
075700             FC_RL_LFT_NONF_QTY,                                  00075700
075800             FC_RL_SETOFF_CD,                                     00075800
075900             FC_RL_CL12_ZONE_NO,                                  00075900
076000             FC_RL_CL12_ADZN_NO,                                  00076000
076100             FC_RL_CL13_ZONE_NO,                                  00076100
076200             FC_RL_CL13_ADZN_NO,                                  00076200
076300             FC_RL_CL14_ZONE_NO,                                  00076300
076400             FC_RL_CL14_ADZN_NO,                                  00076400
076500             FC_RL_CL36_ADZN_NO,                                  00076500
076600             FC_RL_CL37_ADZN_NO,                                  00076600
076700             FC_RL_STORE_DEA_NO,                                  00076700
076800             FC_RL_RETL_ZONE_NO,                                  00076800
076900             FC_RL_STOR2_LOC_NM,                                  00076900
077000             FC_CITY_ADR,                                         00077000
077100             FC_STATE_ADR,                                        00077100
077200             FC_ZIP_CODE5_ADR,                                    00077200
077300             FC_ZIP_CODE4_ADR,                                    00077300
077400             FC_RL_SOS_TYPE_CD,                                   00077400
077500             FC_RL_NOPROCESS_CD,                                  00077500
077600             FC_RL_SOSHDRTYP_CD,                                  00077600
077700             FC_RL_CAT_CLASS_TB,                                  00077700
077800             FC_RL_LATITUDE_K,                                    00077800
077900             FC_RL_LONGITUDE_K,                                   00077900
078000             FN_DIVISION_CD,                                      00078000
078100             FN_LINE_OF_BUS_CD,                                   00078100
078200             FN_ROLLUP_REPT_01_NBR,                               00078200
078300             FN_ROLLUP_REPT_02_NBR,                               00078300
078400             FN_ROLLUP_REPT_03_NBR,                               00078400
078500             FN_ROLLUP_REPT_04_NBR,                               00078500
078600             FN_ROLLUP_REPT_05_NBR,                               00078600
078700             FN_ROLLUP_REPT_06_NBR,                               00078700
078800             FN_ROLLUP_REPT_07_NBR,                               00078800
078900             FN_ROLLUP_REPT_08_NBR,                               00078900
079000             FN_ROLLUP_REPT_09_NBR,                               00079000
079100             FN_ROLLUP_REPT_10_NBR                                00079100
079200         FROM     FC_XXXAIL_STORES                                00079200
079300         WHERE   (FC_RL_DISTRICT_NO >=                            00079300
079400               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO)            00079400
079500         AND NOT                                                  00079500
079600                 (FC_RL_DISTRICT_NO  =                            00079600
079700                       :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO AND 00079700
079800                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00079800
079900         ORDER BY                                                 00079900
080000             FC_RL_DISTRICT_NO,                                   00080000
080100             FC_STORE_NO                                          00080100
080200     END-EXEC.                                                    00080200
080300* --------------------------------------------------              00080300
080400* RFCXRL09 cursor declaration.                                    00080400
080500* --------------------------------------------------              00080500
080600     EXEC SQL                                                     00080600
080700         DECLARE RFCXRL09 CURSOR FOR SELECT                       00080700
080800             FC_STORE_NO,                                         00080800
080900             FC_RL_STORE_NM,                                      00080900
081000             FC_RL_STORE_CD,                                      00081000
081100             FC_RL_STORE_DIR_NM,                                  00081100
081200             FC_RL_STORE_LOC_NM,                                  00081200
081300             FC_RL_OPENING_DT,                                    00081300
081400             FC_RL_CLOSING_DT,                                    00081400
081500             FC_RL_REMODEL_DT,                                    00081500
081600             FC_RL_DELETED_DT,                                    00081600
081700             FC_RL_DISTRICT_NO,                                   00081700
081800             FC_RL_MARKET_AR_NO,                                  00081800
081900             FC_RL_PAYROL_AR_NO,                                  00081900
082000             FC_RL_PAY_GROUP_NO,                                  00082000
082100             FC_RL_COMPANY_NO,                                    00082100
082200             FC_RL_GEO_ZONE_CD,                                   00082200
082300             FC_RL_GEO_ZONE_NO,                                   00082300
082400             FC_RL_SCAN_MAIN_CD,                                  00082400
082500             FC_RL_FRONT_END_CD,                                  00082500
082600             FC_RL_PRICE_BUL_CD,                                  00082600
082700             FC_RL_UPC_ON_PB_CD,                                  00082700
082800             FC_RL_COMPETITR_CD,                                  00082800
082900             FC_RL_ASSOC_STR_NO,                                  00082900
083000             FC_RL_RPRT_SEQ_NO,                                   00083000
083100             FC_RL_SORT_SOS_NO,                                   00083100
083200             FC_RL_VID_PRZN_NO,                                   00083200
083300             FC_RL_CITY_ID_CD,                                    00083300
083400             FC_RL_ADZONE_ABB,                                    00083400
083500             FC_RL_ADZONE_DES,                                    00083500
083600             FC_RL_UNLOAD_SW,                                     00083600
083700             FN_ROLLUP_REPT_CD,                                   00083700
083800             FC_RL_STATUS_CD,                                     00083800
083900             FC_RL_NEW_STORE_CD,                                  00083900
084000             FC_RL_TYPE_CD,                                       00084000
084100             FC_RL_GROUP_CD,                                      00084100
084200             FC_RL_SELECTCIR_CD,                                  00084200
084300             FC_RL_AREA_CODE_NO,                                  00084300
084400             FC_RL_TELEPHONE_NO,                                  00084400
084500             FC_RL_STORE_ABB,                                     00084500
084600             FC_RL_BCKRM_FT_QTY,                                  00084600
084700             FC_RL_LFT_FOOD_QTY,                                  00084700
084800             FC_RL_LFT_NONF_QTY,                                  00084800
084900             FC_RL_SETOFF_CD,                                     00084900
085000             FC_RL_CL12_ZONE_NO,                                  00085000
085100             FC_RL_CL12_ADZN_NO,                                  00085100
085200             FC_RL_CL13_ZONE_NO,                                  00085200
085300             FC_RL_CL13_ADZN_NO,                                  00085300
085400             FC_RL_CL14_ZONE_NO,                                  00085400
085500             FC_RL_CL14_ADZN_NO,                                  00085500
085600             FC_RL_CL36_ADZN_NO,                                  00085600
085700             FC_RL_CL37_ADZN_NO,                                  00085700
085800             FC_RL_STORE_DEA_NO,                                  00085800
085900             FC_RL_RETL_ZONE_NO,                                  00085900
086000             FC_RL_STOR2_LOC_NM,                                  00086000
086100             FC_CITY_ADR,                                         00086100
086200             FC_STATE_ADR,                                        00086200
086300             FC_ZIP_CODE5_ADR,                                    00086300
086400             FC_ZIP_CODE4_ADR,                                    00086400
086500             FC_RL_SOS_TYPE_CD,                                   00086500
086600             FC_RL_NOPROCESS_CD,                                  00086600
086700             FC_RL_SOSHDRTYP_CD,                                  00086700
086800             FC_RL_CAT_CLASS_TB,                                  00086800
086900             FC_RL_LATITUDE_K,                                    00086900
087000             FC_RL_LONGITUDE_K,                                   00087000
087100             FN_DIVISION_CD,                                      00087100
087200             FN_LINE_OF_BUS_CD,                                   00087200
087300             FN_ROLLUP_REPT_01_NBR,                               00087300
087400             FN_ROLLUP_REPT_02_NBR,                               00087400
087500             FN_ROLLUP_REPT_03_NBR,                               00087500
087600             FN_ROLLUP_REPT_04_NBR,                               00087600
087700             FN_ROLLUP_REPT_05_NBR,                               00087700
087800             FN_ROLLUP_REPT_06_NBR,                               00087800
087900             FN_ROLLUP_REPT_07_NBR,                               00087900
088000             FN_ROLLUP_REPT_08_NBR,                               00088000
088100             FN_ROLLUP_REPT_09_NBR,                               00088100
088200             FN_ROLLUP_REPT_10_NBR                                00088200
088300         FROM     FC_XXXAIL_STORES                                00088300
088400         WHERE   (FC_RL_DISTRICT_NO >=                            00088400
088500               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO)            00088500
088600         AND NOT                                                  00088600
088700                 (FC_RL_DISTRICT_NO  =                            00088700
088800                       :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO AND 00088800
088900                  FC_RL_STORE_NM  <                               00088900
089000                        :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM)      00089000
089100         AND NOT                                                  00089100
089200                 (FC_RL_DISTRICT_NO  =                            00089200
089300                       :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO AND 00089300
089400                  FC_RL_STORE_NM  =                               00089400
089500                        :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM AND   00089500
089600                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00089600
089700         ORDER BY                                                 00089700
089800             FC_RL_DISTRICT_NO,                                   00089800
089900             FC_RL_STORE_NM,                                      00089900
090000             FC_STORE_NO                                          00090000
090100     END-EXEC.                                                    00090100
090200* --------------------------------------------------              00090200
090300* RFCXRL10 cursor declaration.                                    00090300
090400* --------------------------------------------------              00090400
090500     EXEC SQL                                                     00090500
090600         DECLARE RFCXRL10 CURSOR FOR SELECT                       00090600
090700             FC_STORE_NO,                                         00090700
090800             FC_RL_STORE_NM,                                      00090800
090900             FC_RL_STORE_CD,                                      00090900
091000             FC_RL_STORE_DIR_NM,                                  00091000
091100             FC_RL_STORE_LOC_NM,                                  00091100
091200             FC_RL_OPENING_DT,                                    00091200
091300             FC_RL_CLOSING_DT,                                    00091300
091400             FC_RL_REMODEL_DT,                                    00091400
091500             FC_RL_DELETED_DT,                                    00091500
091600             FC_RL_DISTRICT_NO,                                   00091600
091700             FC_RL_MARKET_AR_NO,                                  00091700
091800             FC_RL_PAYROL_AR_NO,                                  00091800
091900             FC_RL_PAY_GROUP_NO,                                  00091900
092000             FC_RL_COMPANY_NO,                                    00092000
092100             FC_RL_GEO_ZONE_CD,                                   00092100
092200             FC_RL_GEO_ZONE_NO,                                   00092200
092300             FC_RL_SCAN_MAIN_CD,                                  00092300
092400             FC_RL_FRONT_END_CD,                                  00092400
092500             FC_RL_PRICE_BUL_CD,                                  00092500
092600             FC_RL_UPC_ON_PB_CD,                                  00092600
092700             FC_RL_COMPETITR_CD,                                  00092700
092800             FC_RL_ASSOC_STR_NO,                                  00092800
092900             FC_RL_RPRT_SEQ_NO,                                   00092900
093000             FC_RL_SORT_SOS_NO,                                   00093000
093100             FC_RL_VID_PRZN_NO,                                   00093100
093200             FC_RL_CITY_ID_CD,                                    00093200
093300             FC_RL_ADZONE_ABB,                                    00093300
093400             FC_RL_ADZONE_DES,                                    00093400
093500             FC_RL_UNLOAD_SW,                                     00093500
093600             FN_ROLLUP_REPT_CD,                                   00093600
093700             FC_RL_STATUS_CD,                                     00093700
093800             FC_RL_NEW_STORE_CD,                                  00093800
093900             FC_RL_TYPE_CD,                                       00093900
094000             FC_RL_GROUP_CD,                                      00094000
094100             FC_RL_SELECTCIR_CD,                                  00094100
094200             FC_RL_AREA_CODE_NO,                                  00094200
094300             FC_RL_TELEPHONE_NO,                                  00094300
094400             FC_RL_STORE_ABB,                                     00094400
094500             FC_RL_BCKRM_FT_QTY,                                  00094500
094600             FC_RL_LFT_FOOD_QTY,                                  00094600
094700             FC_RL_LFT_NONF_QTY,                                  00094700
094800             FC_RL_SETOFF_CD,                                     00094800
094900             FC_RL_CL12_ZONE_NO,                                  00094900
095000             FC_RL_CL12_ADZN_NO,                                  00095000
095100             FC_RL_CL13_ZONE_NO,                                  00095100
095200             FC_RL_CL13_ADZN_NO,                                  00095200
095300             FC_RL_CL14_ZONE_NO,                                  00095300
095400             FC_RL_CL14_ADZN_NO,                                  00095400
095500             FC_RL_CL36_ADZN_NO,                                  00095500
095600             FC_RL_CL37_ADZN_NO,                                  00095600
095700             FC_RL_STORE_DEA_NO,                                  00095700
095800             FC_RL_RETL_ZONE_NO,                                  00095800
095900             FC_RL_STOR2_LOC_NM,                                  00095900
096000             FC_CITY_ADR,                                         00096000
096100             FC_STATE_ADR,                                        00096100
096200             FC_ZIP_CODE5_ADR,                                    00096200
096300             FC_ZIP_CODE4_ADR,                                    00096300
096400             FC_RL_SOS_TYPE_CD,                                   00096400
096500             FC_RL_NOPROCESS_CD,                                  00096500
096600             FC_RL_SOSHDRTYP_CD,                                  00096600
096700             FC_RL_CAT_CLASS_TB,                                  00096700
096800             FC_RL_LATITUDE_K,                                    00096800
096900             FC_RL_LONGITUDE_K,                                   00096900
097000             FN_DIVISION_CD,                                      00097000
097100             FN_LINE_OF_BUS_CD,                                   00097100
097200             FN_ROLLUP_REPT_01_NBR,                               00097200
097300             FN_ROLLUP_REPT_02_NBR,                               00097300
097400             FN_ROLLUP_REPT_03_NBR,                               00097400
097500             FN_ROLLUP_REPT_04_NBR,                               00097500
097600             FN_ROLLUP_REPT_05_NBR,                               00097600
097700             FN_ROLLUP_REPT_06_NBR,                               00097700
097800             FN_ROLLUP_REPT_07_NBR,                               00097800
097900             FN_ROLLUP_REPT_08_NBR,                               00097900
098000             FN_ROLLUP_REPT_09_NBR,                               00098000
098100             FN_ROLLUP_REPT_10_NBR                                00098100
098200         FROM     FC_XXXAIL_STORES                                00098200
098300         WHERE   (FC_RL_COMPANY_NO >=                             00098300
098400               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO)             00098400
098500         AND NOT                                                  00098500
098600                 (FC_RL_COMPANY_NO  =                             00098600
098700                       :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO AND  00098700
098800                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00098800
098900         ORDER BY                                                 00098900
099000             FC_RL_COMPANY_NO,                                    00099000
099100             FC_STORE_NO                                          00099100
099200     END-EXEC.                                                    00099200
099300* --------------------------------------------------              00099300
099400* RFCXRL11 cursor declaration.                                    00099400
099500* --------------------------------------------------              00099500
099600     EXEC SQL                                                     00099600
099700         DECLARE RFCXRL11 CURSOR FOR SELECT                       00099700
099800             FC_STORE_NO,                                         00099800
099900             FC_RL_STORE_NM,                                      00099900
100000             FC_RL_STORE_CD,                                      00100000
100100             FC_RL_STORE_DIR_NM,                                  00100100
100200             FC_RL_STORE_LOC_NM,                                  00100200
100300             FC_RL_OPENING_DT,                                    00100300
100400             FC_RL_CLOSING_DT,                                    00100400
100500             FC_RL_REMODEL_DT,                                    00100500
100600             FC_RL_DELETED_DT,                                    00100600
100700             FC_RL_DISTRICT_NO,                                   00100700
100800             FC_RL_MARKET_AR_NO,                                  00100800
100900             FC_RL_PAYROL_AR_NO,                                  00100900
101000             FC_RL_PAY_GROUP_NO,                                  00101000
101100             FC_RL_COMPANY_NO,                                    00101100
101200             FC_RL_GEO_ZONE_CD,                                   00101200
101300             FC_RL_GEO_ZONE_NO,                                   00101300
101400             FC_RL_SCAN_MAIN_CD,                                  00101400
101500             FC_RL_FRONT_END_CD,                                  00101500
101600             FC_RL_PRICE_BUL_CD,                                  00101600
101700             FC_RL_UPC_ON_PB_CD,                                  00101700
101800             FC_RL_COMPETITR_CD,                                  00101800
101900             FC_RL_ASSOC_STR_NO,                                  00101900
102000             FC_RL_RPRT_SEQ_NO,                                   00102000
102100             FC_RL_SORT_SOS_NO,                                   00102100
102200             FC_RL_VID_PRZN_NO,                                   00102200
102300             FC_RL_CITY_ID_CD,                                    00102300
102400             FC_RL_ADZONE_ABB,                                    00102400
102500             FC_RL_ADZONE_DES,                                    00102500
102600             FC_RL_UNLOAD_SW,                                     00102600
102700             FN_ROLLUP_REPT_CD,                                   00102700
102800             FC_RL_STATUS_CD,                                     00102800
102900             FC_RL_NEW_STORE_CD,                                  00102900
103000             FC_RL_TYPE_CD,                                       00103000
103100             FC_RL_GROUP_CD,                                      00103100
103200             FC_RL_SELECTCIR_CD,                                  00103200
103300             FC_RL_AREA_CODE_NO,                                  00103300
103400             FC_RL_TELEPHONE_NO,                                  00103400
103500             FC_RL_STORE_ABB,                                     00103500
103600             FC_RL_BCKRM_FT_QTY,                                  00103600
103700             FC_RL_LFT_FOOD_QTY,                                  00103700
103800             FC_RL_LFT_NONF_QTY,                                  00103800
103900             FC_RL_SETOFF_CD,                                     00103900
104000             FC_RL_CL12_ZONE_NO,                                  00104000
104100             FC_RL_CL12_ADZN_NO,                                  00104100
104200             FC_RL_CL13_ZONE_NO,                                  00104200
104300             FC_RL_CL13_ADZN_NO,                                  00104300
104400             FC_RL_CL14_ZONE_NO,                                  00104400
104500             FC_RL_CL14_ADZN_NO,                                  00104500
104600             FC_RL_CL36_ADZN_NO,                                  00104600
104700             FC_RL_CL37_ADZN_NO,                                  00104700
104800             FC_RL_STORE_DEA_NO,                                  00104800
104900             FC_RL_RETL_ZONE_NO,                                  00104900
105000             FC_RL_STOR2_LOC_NM,                                  00105000
105100             FC_CITY_ADR,                                         00105100
105200             FC_STATE_ADR,                                        00105200
105300             FC_ZIP_CODE5_ADR,                                    00105300
105400             FC_ZIP_CODE4_ADR,                                    00105400
105500             FC_RL_SOS_TYPE_CD,                                   00105500
105600             FC_RL_NOPROCESS_CD,                                  00105600
105700             FC_RL_SOSHDRTYP_CD,                                  00105700
105800             FC_RL_CAT_CLASS_TB,                                  00105800
105900             FC_RL_LATITUDE_K,                                    00105900
106000             FC_RL_LONGITUDE_K,                                   00106000
106100             FN_DIVISION_CD,                                      00106100
106200             FN_LINE_OF_BUS_CD,                                   00106200
106300             FN_ROLLUP_REPT_01_NBR,                               00106300
106400             FN_ROLLUP_REPT_02_NBR,                               00106400
106500             FN_ROLLUP_REPT_03_NBR,                               00106500
106600             FN_ROLLUP_REPT_04_NBR,                               00106600
106700             FN_ROLLUP_REPT_05_NBR,                               00106700
106800             FN_ROLLUP_REPT_06_NBR,                               00106800
106900             FN_ROLLUP_REPT_07_NBR,                               00106900
107000             FN_ROLLUP_REPT_08_NBR,                               00107000
107100             FN_ROLLUP_REPT_09_NBR,                               00107100
107200             FN_ROLLUP_REPT_10_NBR                                00107200
107300         FROM     FC_XXXAIL_STORES                                00107300
107400         WHERE   (FC_RL_COMPANY_NO >=                             00107400
107500               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO)             00107500
107600         AND NOT                                                  00107600
107700                 (FC_RL_COMPANY_NO  =                             00107700
107800                       :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO AND  00107800
107900                  FC_RL_STORE_NM  <                               00107900
108000                        :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM)      00108000
108100         AND NOT                                                  00108100
108200                 (FC_RL_COMPANY_NO  =                             00108200
108300                       :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO AND  00108300
108400                  FC_RL_STORE_NM  =                               00108400
108500                        :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM AND   00108500
108600                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00108600
108700         ORDER BY                                                 00108700
108800             FC_RL_COMPANY_NO,                                    00108800
108900             FC_RL_STORE_NM,                                      00108900
109000             FC_STORE_NO                                          00109000
109100     END-EXEC.                                                    00109100
109200* --------------------------------------------------              00109200
109300* RFCXRL12 cursor declaration.                                    00109300
109400* --------------------------------------------------              00109400
109500     EXEC SQL                                                     00109500
109600         DECLARE RFCXRL12 CURSOR FOR SELECT                       00109600
109700             FC_STORE_NO,                                         00109700
109800             FC_RL_STORE_NM,                                      00109800
109900             FC_RL_STORE_CD,                                      00109900
110000             FC_RL_STORE_DIR_NM,                                  00110000
110100             FC_RL_STORE_LOC_NM,                                  00110100
110200             FC_RL_OPENING_DT,                                    00110200
110300             FC_RL_CLOSING_DT,                                    00110300
110400             FC_RL_REMODEL_DT,                                    00110400
110500             FC_RL_DELETED_DT,                                    00110500
110600             FC_RL_DISTRICT_NO,                                   00110600
110700             FC_RL_MARKET_AR_NO,                                  00110700
110800             FC_RL_PAYROL_AR_NO,                                  00110800
110900             FC_RL_PAY_GROUP_NO,                                  00110900
111000             FC_RL_COMPANY_NO,                                    00111000
111100             FC_RL_GEO_ZONE_CD,                                   00111100
111200             FC_RL_GEO_ZONE_NO,                                   00111200
111300             FC_RL_SCAN_MAIN_CD,                                  00111300
111400             FC_RL_FRONT_END_CD,                                  00111400
111500             FC_RL_PRICE_BUL_CD,                                  00111500
111600             FC_RL_UPC_ON_PB_CD,                                  00111600
111700             FC_RL_COMPETITR_CD,                                  00111700
111800             FC_RL_ASSOC_STR_NO,                                  00111800
111900             FC_RL_RPRT_SEQ_NO,                                   00111900
112000             FC_RL_SORT_SOS_NO,                                   00112000
112100             FC_RL_VID_PRZN_NO,                                   00112100
112200             FC_RL_CITY_ID_CD,                                    00112200
112300             FC_RL_ADZONE_ABB,                                    00112300
112400             FC_RL_ADZONE_DES,                                    00112400
112500             FC_RL_UNLOAD_SW,                                     00112500
112600             FN_ROLLUP_REPT_CD,                                   00112600
112700             FC_RL_STATUS_CD,                                     00112700
112800             FC_RL_NEW_STORE_CD,                                  00112800
112900             FC_RL_TYPE_CD,                                       00112900
113000             FC_RL_GROUP_CD,                                      00113000
113100             FC_RL_SELECTCIR_CD,                                  00113100
113200             FC_RL_AREA_CODE_NO,                                  00113200
113300             FC_RL_TELEPHONE_NO,                                  00113300
113400             FC_RL_STORE_ABB,                                     00113400
113500             FC_RL_BCKRM_FT_QTY,                                  00113500
113600             FC_RL_LFT_FOOD_QTY,                                  00113600
113700             FC_RL_LFT_NONF_QTY,                                  00113700
113800             FC_RL_SETOFF_CD,                                     00113800
113900             FC_RL_CL12_ZONE_NO,                                  00113900
114000             FC_RL_CL12_ADZN_NO,                                  00114000
114100             FC_RL_CL13_ZONE_NO,                                  00114100
114200             FC_RL_CL13_ADZN_NO,                                  00114200
114300             FC_RL_CL14_ZONE_NO,                                  00114300
114400             FC_RL_CL14_ADZN_NO,                                  00114400
114500             FC_RL_CL36_ADZN_NO,                                  00114500
114600             FC_RL_CL37_ADZN_NO,                                  00114600
114700             FC_RL_STORE_DEA_NO,                                  00114700
114800             FC_RL_RETL_ZONE_NO,                                  00114800
114900             FC_RL_STOR2_LOC_NM,                                  00114900
115000             FC_CITY_ADR,                                         00115000
115100             FC_STATE_ADR,                                        00115100
115200             FC_ZIP_CODE5_ADR,                                    00115200
115300             FC_ZIP_CODE4_ADR,                                    00115300
115400             FC_RL_SOS_TYPE_CD,                                   00115400
115500             FC_RL_NOPROCESS_CD,                                  00115500
115600             FC_RL_SOSHDRTYP_CD,                                  00115600
115700             FC_RL_CAT_CLASS_TB,                                  00115700
115800             FC_RL_LATITUDE_K,                                    00115800
115900             FC_RL_LONGITUDE_K,                                   00115900
116000             FN_DIVISION_CD,                                      00116000
116100             FN_LINE_OF_BUS_CD,                                   00116100
116200             FN_ROLLUP_REPT_01_NBR,                               00116200
116300             FN_ROLLUP_REPT_02_NBR,                               00116300
116400             FN_ROLLUP_REPT_03_NBR,                               00116400
116500             FN_ROLLUP_REPT_04_NBR,                               00116500
116600             FN_ROLLUP_REPT_05_NBR,                               00116600
116700             FN_ROLLUP_REPT_06_NBR,                               00116700
116800             FN_ROLLUP_REPT_07_NBR,                               00116800
116900             FN_ROLLUP_REPT_08_NBR,                               00116900
117000             FN_ROLLUP_REPT_09_NBR,                               00117000
117100             FN_ROLLUP_REPT_10_NBR                                00117100
117200         FROM     FC_XXXAIL_STORES                                00117200
117300         WHERE   (FC_RL_AREA_CODE_NO >=                           00117300
117400               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO)           00117400
117500         AND NOT                                                  00117500
117600                 (FC_RL_AREA_CODE_NO  =                           00117600
117700                       :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO AND00117700
117800                  FC_RL_TELEPHONE_NO  <                           00117800
117900                        :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO)  00117900
118000         AND NOT                                                  00118000
118100                 (FC_RL_AREA_CODE_NO  =                           00118100
118200                       :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO AND00118200
118300                  FC_RL_TELEPHONE_NO  =                           00118300
118400                        :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO   00118400
118500                        AND                                       00118500
118600                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00118600
118700         ORDER BY                                                 00118700
118800             FC_RL_AREA_CODE_NO,                                  00118800
118900             FC_RL_TELEPHONE_NO,                                  00118900
119000             FC_STORE_NO                                          00119000
119100     END-EXEC.                                                    00119100
119200* --------------------------------------------------              00119200
119300* RFCXRL13 cursor declaration.                                    00119300
119400* --------------------------------------------------              00119400
119500     EXEC SQL                                                     00119500
119600         DECLARE RFCXRL13 CURSOR FOR SELECT                       00119600
119700             FC_STORE_NO,                                         00119700
119800             FC_RL_STORE_NM,                                      00119800
119900             FC_RL_STORE_CD,                                      00119900
120000             FC_RL_STORE_DIR_NM,                                  00120000
120100             FC_RL_STORE_LOC_NM,                                  00120100
120200             FC_RL_OPENING_DT,                                    00120200
120300             FC_RL_CLOSING_DT,                                    00120300
120400             FC_RL_REMODEL_DT,                                    00120400
120500             FC_RL_DELETED_DT,                                    00120500
120600             FC_RL_DISTRICT_NO,                                   00120600
120700             FC_RL_MARKET_AR_NO,                                  00120700
120800             FC_RL_PAYROL_AR_NO,                                  00120800
120900             FC_RL_PAY_GROUP_NO,                                  00120900
121000             FC_RL_COMPANY_NO,                                    00121000
121100             FC_RL_GEO_ZONE_CD,                                   00121100
121200             FC_RL_GEO_ZONE_NO,                                   00121200
121300             FC_RL_SCAN_MAIN_CD,                                  00121300
121400             FC_RL_FRONT_END_CD,                                  00121400
121500             FC_RL_PRICE_BUL_CD,                                  00121500
121600             FC_RL_UPC_ON_PB_CD,                                  00121600
121700             FC_RL_COMPETITR_CD,                                  00121700
121800             FC_RL_ASSOC_STR_NO,                                  00121800
121900             FC_RL_RPRT_SEQ_NO,                                   00121900
122000             FC_RL_SORT_SOS_NO,                                   00122000
122100             FC_RL_VID_PRZN_NO,                                   00122100
122200             FC_RL_CITY_ID_CD,                                    00122200
122300             FC_RL_ADZONE_ABB,                                    00122300
122400             FC_RL_ADZONE_DES,                                    00122400
122500             FC_RL_UNLOAD_SW,                                     00122500
122600             FN_ROLLUP_REPT_CD,                                   00122600
122700             FC_RL_STATUS_CD,                                     00122700
122800             FC_RL_NEW_STORE_CD,                                  00122800
122900             FC_RL_TYPE_CD,                                       00122900
123000             FC_RL_GROUP_CD,                                      00123000
123100             FC_RL_SELECTCIR_CD,                                  00123100
123200             FC_RL_AREA_CODE_NO,                                  00123200
123300             FC_RL_TELEPHONE_NO,                                  00123300
123400             FC_RL_STORE_ABB,                                     00123400
123500             FC_RL_BCKRM_FT_QTY,                                  00123500
123600             FC_RL_LFT_FOOD_QTY,                                  00123600
123700             FC_RL_LFT_NONF_QTY,                                  00123700
123800             FC_RL_SETOFF_CD,                                     00123800
123900             FC_RL_CL12_ZONE_NO,                                  00123900
124000             FC_RL_CL12_ADZN_NO,                                  00124000
124100             FC_RL_CL13_ZONE_NO,                                  00124100
124200             FC_RL_CL13_ADZN_NO,                                  00124200
124300             FC_RL_CL14_ZONE_NO,                                  00124300
124400             FC_RL_CL14_ADZN_NO,                                  00124400
124500             FC_RL_CL36_ADZN_NO,                                  00124500
124600             FC_RL_CL37_ADZN_NO,                                  00124600
124700             FC_RL_STORE_DEA_NO,                                  00124700
124800             FC_RL_RETL_ZONE_NO,                                  00124800
124900             FC_RL_STOR2_LOC_NM,                                  00124900
125000             FC_CITY_ADR,                                         00125000
125100             FC_STATE_ADR,                                        00125100
125200             FC_ZIP_CODE5_ADR,                                    00125200
125300             FC_ZIP_CODE4_ADR,                                    00125300
125400             FC_RL_SOS_TYPE_CD,                                   00125400
125500             FC_RL_NOPROCESS_CD,                                  00125500
125600             FC_RL_SOSHDRTYP_CD,                                  00125600
125700             FC_RL_CAT_CLASS_TB,                                  00125700
125800             FC_RL_LATITUDE_K,                                    00125800
125900             FC_RL_LONGITUDE_K,                                   00125900
126000             FN_DIVISION_CD,                                      00126000
126100             FN_LINE_OF_BUS_CD,                                   00126100
126200             FN_ROLLUP_REPT_01_NBR,                               00126200
126300             FN_ROLLUP_REPT_02_NBR,                               00126300
126400             FN_ROLLUP_REPT_03_NBR,                               00126400
126500             FN_ROLLUP_REPT_04_NBR,                               00126500
126600             FN_ROLLUP_REPT_05_NBR,                               00126600
126700             FN_ROLLUP_REPT_06_NBR,                               00126700
126800             FN_ROLLUP_REPT_07_NBR,                               00126800
126900             FN_ROLLUP_REPT_08_NBR,                               00126900
127000             FN_ROLLUP_REPT_09_NBR,                               00127000
127100             FN_ROLLUP_REPT_10_NBR                                00127100
127200         FROM     FC_XXXAIL_STORES                                00127200
127300         WHERE   (FC_CITY_ADR >= :DCLFC-XXXAIL-STORES.FC-CITY-ADR)00127300
127400         AND NOT                                                  00127400
127500                 (FC_CITY_ADR  =                                  00127500
127600                       :DCLFC-XXXAIL-STORES.FC-CITY-ADR AND       00127600
127700                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00127700
127800         ORDER BY                                                 00127800
127900             FC_CITY_ADR,                                         00127900
128000             FC_STORE_NO                                          00128000
128100     END-EXEC.                                                    00128100
128200* --------------------------------------------------              00128200
128300* RFCXRL14 cursor declaration.                                    00128300
128400* --------------------------------------------------              00128400
128500     EXEC SQL                                                     00128500
128600         DECLARE RFCXRL14 CURSOR FOR SELECT                       00128600
128700             FC_STORE_NO,                                         00128700
128800             FC_RL_STORE_NM,                                      00128800
128900             FC_RL_STORE_CD,                                      00128900
129000             FC_RL_STORE_DIR_NM,                                  00129000
129100             FC_RL_STORE_LOC_NM,                                  00129100
129200             FC_RL_OPENING_DT,                                    00129200
129300             FC_RL_CLOSING_DT,                                    00129300
129400             FC_RL_REMODEL_DT,                                    00129400
129500             FC_RL_DELETED_DT,                                    00129500
129600             FC_RL_DISTRICT_NO,                                   00129600
129700             FC_RL_MARKET_AR_NO,                                  00129700
129800             FC_RL_PAYROL_AR_NO,                                  00129800
129900             FC_RL_PAY_GROUP_NO,                                  00129900
130000             FC_RL_COMPANY_NO,                                    00130000
130100             FC_RL_GEO_ZONE_CD,                                   00130100
130200             FC_RL_GEO_ZONE_NO,                                   00130200
130300             FC_RL_SCAN_MAIN_CD,                                  00130300
130400             FC_RL_FRONT_END_CD,                                  00130400
130500             FC_RL_PRICE_BUL_CD,                                  00130500
130600             FC_RL_UPC_ON_PB_CD,                                  00130600
130700             FC_RL_COMPETITR_CD,                                  00130700
130800             FC_RL_ASSOC_STR_NO,                                  00130800
130900             FC_RL_RPRT_SEQ_NO,                                   00130900
131000             FC_RL_SORT_SOS_NO,                                   00131000
131100             FC_RL_VID_PRZN_NO,                                   00131100
131200             FC_RL_CITY_ID_CD,                                    00131200
131300             FC_RL_ADZONE_ABB,                                    00131300
131400             FC_RL_ADZONE_DES,                                    00131400
131500             FC_RL_UNLOAD_SW,                                     00131500
131600             FN_ROLLUP_REPT_CD,                                   00131600
131700             FC_RL_STATUS_CD,                                     00131700
131800             FC_RL_NEW_STORE_CD,                                  00131800
131900             FC_RL_TYPE_CD,                                       00131900
132000             FC_RL_GROUP_CD,                                      00132000
132100             FC_RL_SELECTCIR_CD,                                  00132100
132200             FC_RL_AREA_CODE_NO,                                  00132200
132300             FC_RL_TELEPHONE_NO,                                  00132300
132400             FC_RL_STORE_ABB,                                     00132400
132500             FC_RL_BCKRM_FT_QTY,                                  00132500
132600             FC_RL_LFT_FOOD_QTY,                                  00132600
132700             FC_RL_LFT_NONF_QTY,                                  00132700
132800             FC_RL_SETOFF_CD,                                     00132800
132900             FC_RL_CL12_ZONE_NO,                                  00132900
133000             FC_RL_CL12_ADZN_NO,                                  00133000
133100             FC_RL_CL13_ZONE_NO,                                  00133100
133200             FC_RL_CL13_ADZN_NO,                                  00133200
133300             FC_RL_CL14_ZONE_NO,                                  00133300
133400             FC_RL_CL14_ADZN_NO,                                  00133400
133500             FC_RL_CL36_ADZN_NO,                                  00133500
133600             FC_RL_CL37_ADZN_NO,                                  00133600
133700             FC_RL_STORE_DEA_NO,                                  00133700
133800             FC_RL_RETL_ZONE_NO,                                  00133800
133900             FC_RL_STOR2_LOC_NM,                                  00133900
134000             FC_CITY_ADR,                                         00134000
134100             FC_STATE_ADR,                                        00134100
134200             FC_ZIP_CODE5_ADR,                                    00134200
134300             FC_ZIP_CODE4_ADR,                                    00134300
134400             FC_RL_SOS_TYPE_CD,                                   00134400
134500             FC_RL_NOPROCESS_CD,                                  00134500
134600             FC_RL_SOSHDRTYP_CD,                                  00134600
134700             FC_RL_CAT_CLASS_TB,                                  00134700
134800             FC_RL_LATITUDE_K,                                    00134800
134900             FC_RL_LONGITUDE_K,                                   00134900
135000             FN_DIVISION_CD,                                      00135000
135100             FN_LINE_OF_BUS_CD,                                   00135100
135200             FN_ROLLUP_REPT_01_NBR,                               00135200
135300             FN_ROLLUP_REPT_02_NBR,                               00135300
135400             FN_ROLLUP_REPT_03_NBR,                               00135400
135500             FN_ROLLUP_REPT_04_NBR,                               00135500
135600             FN_ROLLUP_REPT_05_NBR,                               00135600
135700             FN_ROLLUP_REPT_06_NBR,                               00135700
135800             FN_ROLLUP_REPT_07_NBR,                               00135800
135900             FN_ROLLUP_REPT_08_NBR,                               00135900
136000             FN_ROLLUP_REPT_09_NBR,                               00136000
136100             FN_ROLLUP_REPT_10_NBR                                00136100
136200         FROM     FC_XXXAIL_STORES                                00136200
136300         WHERE   (FC_CITY_ADR >= :DCLFC-XXXAIL-STORES.FC-CITY-ADR)00136300
136400         AND NOT                                                  00136400
136500                 (FC_CITY_ADR  =                                  00136500
136600                       :DCLFC-XXXAIL-STORES.FC-CITY-ADR AND       00136600
136700                  FC_RL_STORE_NM  <                               00136700
136800                        :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM)      00136800
136900         AND NOT                                                  00136900
137000                 (FC_CITY_ADR  =                                  00137000
137100                       :DCLFC-XXXAIL-STORES.FC-CITY-ADR AND       00137100
137200                  FC_RL_STORE_NM  =                               00137200
137300                        :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM AND   00137300
137400                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00137400
137500         ORDER BY                                                 00137500
137600             FC_CITY_ADR,                                         00137600
137700             FC_RL_STORE_NM,                                      00137700
137800             FC_STORE_NO                                          00137800
137900     END-EXEC.                                                    00137900
138000* --------------------------------------------------              00138000
138100* RFCXRL15 cursor declaration.                                    00138100
138200* --------------------------------------------------              00138200
138300     EXEC SQL                                                     00138300
138400         DECLARE RFCXRL15 CURSOR FOR SELECT                       00138400
138500             FC_STORE_NO,                                         00138500
138600             FC_RL_STORE_NM,                                      00138600
138700             FC_RL_STORE_CD,                                      00138700
138800             FC_RL_STORE_DIR_NM,                                  00138800
138900             FC_RL_STORE_LOC_NM,                                  00138900
139000             FC_RL_OPENING_DT,                                    00139000
139100             FC_RL_CLOSING_DT,                                    00139100
139200             FC_RL_REMODEL_DT,                                    00139200
139300             FC_RL_DELETED_DT,                                    00139300
139400             FC_RL_DISTRICT_NO,                                   00139400
139500             FC_RL_MARKET_AR_NO,                                  00139500
139600             FC_RL_PAYROL_AR_NO,                                  00139600
139700             FC_RL_PAY_GROUP_NO,                                  00139700
139800             FC_RL_COMPANY_NO,                                    00139800
139900             FC_RL_GEO_ZONE_CD,                                   00139900
140000             FC_RL_GEO_ZONE_NO,                                   00140000
140100             FC_RL_SCAN_MAIN_CD,                                  00140100
140200             FC_RL_FRONT_END_CD,                                  00140200
140300             FC_RL_PRICE_BUL_CD,                                  00140300
140400             FC_RL_UPC_ON_PB_CD,                                  00140400
140500             FC_RL_COMPETITR_CD,                                  00140500
140600             FC_RL_ASSOC_STR_NO,                                  00140600
140700             FC_RL_RPRT_SEQ_NO,                                   00140700
140800             FC_RL_SORT_SOS_NO,                                   00140800
140900             FC_RL_VID_PRZN_NO,                                   00140900
141000             FC_RL_CITY_ID_CD,                                    00141000
141100             FC_RL_ADZONE_ABB,                                    00141100
141200             FC_RL_ADZONE_DES,                                    00141200
141300             FC_RL_UNLOAD_SW,                                     00141300
141400             FN_ROLLUP_REPT_CD,                                   00141400
141500             FC_RL_STATUS_CD,                                     00141500
141600             FC_RL_NEW_STORE_CD,                                  00141600
141700             FC_RL_TYPE_CD,                                       00141700
141800             FC_RL_GROUP_CD,                                      00141800
141900             FC_RL_SELECTCIR_CD,                                  00141900
142000             FC_RL_AREA_CODE_NO,                                  00142000
142100             FC_RL_TELEPHONE_NO,                                  00142100
142200             FC_RL_STORE_ABB,                                     00142200
142300             FC_RL_BCKRM_FT_QTY,                                  00142300
142400             FC_RL_LFT_FOOD_QTY,                                  00142400
142500             FC_RL_LFT_NONF_QTY,                                  00142500
142600             FC_RL_SETOFF_CD,                                     00142600
142700             FC_RL_CL12_ZONE_NO,                                  00142700
142800             FC_RL_CL12_ADZN_NO,                                  00142800
142900             FC_RL_CL13_ZONE_NO,                                  00142900
143000             FC_RL_CL13_ADZN_NO,                                  00143000
143100             FC_RL_CL14_ZONE_NO,                                  00143100
143200             FC_RL_CL14_ADZN_NO,                                  00143200
143300             FC_RL_CL36_ADZN_NO,                                  00143300
143400             FC_RL_CL37_ADZN_NO,                                  00143400
143500             FC_RL_STORE_DEA_NO,                                  00143500
143600             FC_RL_RETL_ZONE_NO,                                  00143600
143700             FC_RL_STOR2_LOC_NM,                                  00143700
143800             FC_CITY_ADR,                                         00143800
143900             FC_STATE_ADR,                                        00143900
144000             FC_ZIP_CODE5_ADR,                                    00144000
144100             FC_ZIP_CODE4_ADR,                                    00144100
144200             FC_RL_SOS_TYPE_CD,                                   00144200
144300             FC_RL_NOPROCESS_CD,                                  00144300
144400             FC_RL_SOSHDRTYP_CD,                                  00144400
144500             FC_RL_CAT_CLASS_TB,                                  00144500
144600             FC_RL_LATITUDE_K,                                    00144600
144700             FC_RL_LONGITUDE_K,                                   00144700
144800             FN_DIVISION_CD,                                      00144800
144900             FN_LINE_OF_BUS_CD,                                   00144900
145000             FN_ROLLUP_REPT_01_NBR,                               00145000
145100             FN_ROLLUP_REPT_02_NBR,                               00145100
145200             FN_ROLLUP_REPT_03_NBR,                               00145200
145300             FN_ROLLUP_REPT_04_NBR,                               00145300
145400             FN_ROLLUP_REPT_05_NBR,                               00145400
145500             FN_ROLLUP_REPT_06_NBR,                               00145500
145600             FN_ROLLUP_REPT_07_NBR,                               00145600
145700             FN_ROLLUP_REPT_08_NBR,                               00145700
145800             FN_ROLLUP_REPT_09_NBR,                               00145800
145900             FN_ROLLUP_REPT_10_NBR                                00145900
146000         FROM     FC_XXXAIL_STORES                                00146000
146100         WHERE   (FC_STATE_ADR >=                                 00146100
146200               :DCLFC-XXXAIL-STORES.FC-STATE-ADR)                 00146200
146300         AND NOT                                                  00146300
146400                 (FC_STATE_ADR  =                                 00146400
146500                       :DCLFC-XXXAIL-STORES.FC-STATE-ADR AND      00146500
146600                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00146600
146700         ORDER BY                                                 00146700
146800             FC_STATE_ADR,                                        00146800
146900             FC_STORE_NO                                          00146900
147000     END-EXEC.                                                    00147000
147100* --------------------------------------------------              00147100
147200* RFCXRL16 cursor declaration.                                    00147200
147300* --------------------------------------------------              00147300
147400     EXEC SQL                                                     00147400
147500         DECLARE RFCXRL16 CURSOR FOR SELECT                       00147500
147600             FC_STORE_NO,                                         00147600
147700             FC_RL_STORE_NM,                                      00147700
147800             FC_RL_STORE_CD,                                      00147800
147900             FC_RL_STORE_DIR_NM,                                  00147900
148000             FC_RL_STORE_LOC_NM,                                  00148000
148100             FC_RL_OPENING_DT,                                    00148100
148200             FC_RL_CLOSING_DT,                                    00148200
148300             FC_RL_REMODEL_DT,                                    00148300
148400             FC_RL_DELETED_DT,                                    00148400
148500             FC_RL_DISTRICT_NO,                                   00148500
148600             FC_RL_MARKET_AR_NO,                                  00148600
148700             FC_RL_PAYROL_AR_NO,                                  00148700
148800             FC_RL_PAY_GROUP_NO,                                  00148800
148900             FC_RL_COMPANY_NO,                                    00148900
149000             FC_RL_GEO_ZONE_CD,                                   00149000
149100             FC_RL_GEO_ZONE_NO,                                   00149100
149200             FC_RL_SCAN_MAIN_CD,                                  00149200
149300             FC_RL_FRONT_END_CD,                                  00149300
149400             FC_RL_PRICE_BUL_CD,                                  00149400
149500             FC_RL_UPC_ON_PB_CD,                                  00149500
149600             FC_RL_COMPETITR_CD,                                  00149600
149700             FC_RL_ASSOC_STR_NO,                                  00149700
149800             FC_RL_RPRT_SEQ_NO,                                   00149800
149900             FC_RL_SORT_SOS_NO,                                   00149900
150000             FC_RL_VID_PRZN_NO,                                   00150000
150100             FC_RL_CITY_ID_CD,                                    00150100
150200             FC_RL_ADZONE_ABB,                                    00150200
150300             FC_RL_ADZONE_DES,                                    00150300
150400             FC_RL_UNLOAD_SW,                                     00150400
150500             FN_ROLLUP_REPT_CD,                                   00150500
150600             FC_RL_STATUS_CD,                                     00150600
150700             FC_RL_NEW_STORE_CD,                                  00150700
150800             FC_RL_TYPE_CD,                                       00150800
150900             FC_RL_GROUP_CD,                                      00150900
151000             FC_RL_SELECTCIR_CD,                                  00151000
151100             FC_RL_AREA_CODE_NO,                                  00151100
151200             FC_RL_TELEPHONE_NO,                                  00151200
151300             FC_RL_STORE_ABB,                                     00151300
151400             FC_RL_BCKRM_FT_QTY,                                  00151400
151500             FC_RL_LFT_FOOD_QTY,                                  00151500
151600             FC_RL_LFT_NONF_QTY,                                  00151600
151700             FC_RL_SETOFF_CD,                                     00151700
151800             FC_RL_CL12_ZONE_NO,                                  00151800
151900             FC_RL_CL12_ADZN_NO,                                  00151900
152000             FC_RL_CL13_ZONE_NO,                                  00152000
152100             FC_RL_CL13_ADZN_NO,                                  00152100
152200             FC_RL_CL14_ZONE_NO,                                  00152200
152300             FC_RL_CL14_ADZN_NO,                                  00152300
152400             FC_RL_CL36_ADZN_NO,                                  00152400
152500             FC_RL_CL37_ADZN_NO,                                  00152500
152600             FC_RL_STORE_DEA_NO,                                  00152600
152700             FC_RL_RETL_ZONE_NO,                                  00152700
152800             FC_RL_STOR2_LOC_NM,                                  00152800
152900             FC_CITY_ADR,                                         00152900
153000             FC_STATE_ADR,                                        00153000
153100             FC_ZIP_CODE5_ADR,                                    00153100
153200             FC_ZIP_CODE4_ADR,                                    00153200
153300             FC_RL_SOS_TYPE_CD,                                   00153300
153400             FC_RL_NOPROCESS_CD,                                  00153400
153500             FC_RL_SOSHDRTYP_CD,                                  00153500
153600             FC_RL_CAT_CLASS_TB,                                  00153600
153700             FC_RL_LATITUDE_K,                                    00153700
153800             FC_RL_LONGITUDE_K,                                   00153800
153900             FN_DIVISION_CD,                                      00153900
154000             FN_LINE_OF_BUS_CD,                                   00154000
154100             FN_ROLLUP_REPT_01_NBR,                               00154100
154200             FN_ROLLUP_REPT_02_NBR,                               00154200
154300             FN_ROLLUP_REPT_03_NBR,                               00154300
154400             FN_ROLLUP_REPT_04_NBR,                               00154400
154500             FN_ROLLUP_REPT_05_NBR,                               00154500
154600             FN_ROLLUP_REPT_06_NBR,                               00154600
154700             FN_ROLLUP_REPT_07_NBR,                               00154700
154800             FN_ROLLUP_REPT_08_NBR,                               00154800
154900             FN_ROLLUP_REPT_09_NBR,                               00154900
155000             FN_ROLLUP_REPT_10_NBR                                00155000
155100         FROM     FC_XXXAIL_STORES                                00155100
155200         WHERE   (FC_STATE_ADR >=                                 00155200
155300               :DCLFC-XXXAIL-STORES.FC-STATE-ADR)                 00155300
155400         AND NOT                                                  00155400
155500                 (FC_STATE_ADR  =                                 00155500
155600                       :DCLFC-XXXAIL-STORES.FC-STATE-ADR AND      00155600
155700                  FC_RL_STORE_NM  <                               00155700
155800                        :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM)      00155800
155900         AND NOT                                                  00155900
156000                 (FC_STATE_ADR  =                                 00156000
156100                       :DCLFC-XXXAIL-STORES.FC-STATE-ADR AND      00156100
156200                  FC_RL_STORE_NM  =                               00156200
156300                        :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM AND   00156300
156400                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00156400
156500         ORDER BY                                                 00156500
156600             FC_STATE_ADR,                                        00156600
156700             FC_RL_STORE_NM,                                      00156700
156800             FC_STORE_NO                                          00156800
156900     END-EXEC.                                                    00156900
157000* --------------------------------------------------              00157000
157100* RFCXRL17 cursor declaration.                                    00157100
157200* --------------------------------------------------              00157200
157300     EXEC SQL                                                     00157300
157400         DECLARE RFCXRL17 CURSOR FOR SELECT                       00157400
157500             FC_STORE_NO,                                         00157500
157600             FC_RL_STORE_NM,                                      00157600
157700             FC_RL_STORE_CD,                                      00157700
157800             FC_RL_STORE_DIR_NM,                                  00157800
157900             FC_RL_STORE_LOC_NM,                                  00157900
158000             FC_RL_OPENING_DT,                                    00158000
158100             FC_RL_CLOSING_DT,                                    00158100
158200             FC_RL_REMODEL_DT,                                    00158200
158300             FC_RL_DELETED_DT,                                    00158300
158400             FC_RL_DISTRICT_NO,                                   00158400
158500             FC_RL_MARKET_AR_NO,                                  00158500
158600             FC_RL_PAYROL_AR_NO,                                  00158600
158700             FC_RL_PAY_GROUP_NO,                                  00158700
158800             FC_RL_COMPANY_NO,                                    00158800
158900             FC_RL_GEO_ZONE_CD,                                   00158900
159000             FC_RL_GEO_ZONE_NO,                                   00159000
159100             FC_RL_SCAN_MAIN_CD,                                  00159100
159200             FC_RL_FRONT_END_CD,                                  00159200
159300             FC_RL_PRICE_BUL_CD,                                  00159300
159400             FC_RL_UPC_ON_PB_CD,                                  00159400
159500             FC_RL_COMPETITR_CD,                                  00159500
159600             FC_RL_ASSOC_STR_NO,                                  00159600
159700             FC_RL_RPRT_SEQ_NO,                                   00159700
159800             FC_RL_SORT_SOS_NO,                                   00159800
159900             FC_RL_VID_PRZN_NO,                                   00159900
160000             FC_RL_CITY_ID_CD,                                    00160000
160100             FC_RL_ADZONE_ABB,                                    00160100
160200             FC_RL_ADZONE_DES,                                    00160200
160300             FC_RL_UNLOAD_SW,                                     00160300
160400             FN_ROLLUP_REPT_CD,                                   00160400
160500             FC_RL_STATUS_CD,                                     00160500
160600             FC_RL_NEW_STORE_CD,                                  00160600
160700             FC_RL_TYPE_CD,                                       00160700
160800             FC_RL_GROUP_CD,                                      00160800
160900             FC_RL_SELECTCIR_CD,                                  00160900
161000             FC_RL_AREA_CODE_NO,                                  00161000
161100             FC_RL_TELEPHONE_NO,                                  00161100
161200             FC_RL_STORE_ABB,                                     00161200
161300             FC_RL_BCKRM_FT_QTY,                                  00161300
161400             FC_RL_LFT_FOOD_QTY,                                  00161400
161500             FC_RL_LFT_NONF_QTY,                                  00161500
161600             FC_RL_SETOFF_CD,                                     00161600
161700             FC_RL_CL12_ZONE_NO,                                  00161700
161800             FC_RL_CL12_ADZN_NO,                                  00161800
161900             FC_RL_CL13_ZONE_NO,                                  00161900
162000             FC_RL_CL13_ADZN_NO,                                  00162000
162100             FC_RL_CL14_ZONE_NO,                                  00162100
162200             FC_RL_CL14_ADZN_NO,                                  00162200
162300             FC_RL_CL36_ADZN_NO,                                  00162300
162400             FC_RL_CL37_ADZN_NO,                                  00162400
162500             FC_RL_STORE_DEA_NO,                                  00162500
162600             FC_RL_RETL_ZONE_NO,                                  00162600
162700             FC_RL_STOR2_LOC_NM,                                  00162700
162800             FC_CITY_ADR,                                         00162800
162900             FC_STATE_ADR,                                        00162900
163000             FC_ZIP_CODE5_ADR,                                    00163000
163100             FC_ZIP_CODE4_ADR,                                    00163100
163200             FC_RL_SOS_TYPE_CD,                                   00163200
163300             FC_RL_NOPROCESS_CD,                                  00163300
163400             FC_RL_SOSHDRTYP_CD,                                  00163400
163500             FC_RL_CAT_CLASS_TB,                                  00163500
163600             FC_RL_LATITUDE_K,                                    00163600
163700             FC_RL_LONGITUDE_K,                                   00163700
163800             FN_DIVISION_CD,                                      00163800
163900             FN_LINE_OF_BUS_CD,                                   00163900
164000             FN_ROLLUP_REPT_01_NBR,                               00164000
164100             FN_ROLLUP_REPT_02_NBR,                               00164100
164200             FN_ROLLUP_REPT_03_NBR,                               00164200
164300             FN_ROLLUP_REPT_04_NBR,                               00164300
164400             FN_ROLLUP_REPT_05_NBR,                               00164400
164500             FN_ROLLUP_REPT_06_NBR,                               00164500
164600             FN_ROLLUP_REPT_07_NBR,                               00164600
164700             FN_ROLLUP_REPT_08_NBR,                               00164700
164800             FN_ROLLUP_REPT_09_NBR,                               00164800
164900             FN_ROLLUP_REPT_10_NBR                                00164900
165000         FROM     FC_XXXAIL_STORES                                00165000
165100         WHERE   (FC_ZIP_CODE5_ADR >=                             00165100
165200               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR)             00165200
165300         AND NOT                                                  00165300
165400                 (FC_ZIP_CODE5_ADR  =                             00165400
165500                       :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR AND  00165500
165600                  FC_STORE_NO  < :DCLFC-XXXAIL-STORES.FC-STORE-NO)00165600
165700         ORDER BY                                                 00165700
165800             FC_ZIP_CODE5_ADR,                                    00165800
165900             FC_STORE_NO                                          00165900
166000     END-EXEC.                                                    00166000
166100                                                                  00166100
166200 LINKAGE SECTION.                                                 00166200
166300 COPY W00N001A.                                                   00166300
166400     EXEC SQL                                                     00166400
166500         INCLUDE SQLCA                                            00166500
166600     END-EXEC.                                                    00166600
166700 COPY YYYN005A.                                                   00166700
166800 COPY NNNN0000.                                                   00166800
166900 COPY PPPTRL01.                                                   00166900
167000                                                                  00167000
167100 PROCEDURE DIVISION USING                                         00167100
167200     W00N001A                                                     00167200
167300     SQLCA                                                        00167300
167400     YYYN005A                                                     00167400
167500     NNNN0000-PARMS                                               00167500
167600     P-DDDTRL01                                                   00167600
167700     .                                                            00167700
167800                                                                  00167800
167900************************************************************      00167900
168000* MAIN PROGRAM LINE.                                              00168000
168100************************************************************      00168100
168200 0000-EXIT-DISPATCHER.                                            00168200
168300     PERFORM 100-INITIALIZATION                                   00168300
168400     EVALUATE TRUE                                                00168400
168500       WHEN NOT SUCCESS                                           00168500
168600          CONTINUE                                                00168600
168700       WHEN EXIT-OPEN-CURSOR                                      00168700
168800          PERFORM 1000-EXIT-OPEN-CURSOR                           00168800
168900       WHEN EXIT-CLOSE-CURSOR                                     00168900
169000          PERFORM 1100-EXIT-CLOSE-CURSOR                          00169000
169100       WHEN EXIT-GET-UNIQUE-ROW                                   00169100
169200          PERFORM 1200-EXIT-GET-UNIQUE-ROW                        00169200
169300       WHEN EXIT-GET-NEXT-ROW                                     00169300
169400          PERFORM 1300-EXIT-GET-NEXT-ROW                          00169400
169500       WHEN EXIT-PUT-MODIFY-ROW                                   00169500
169600          PERFORM 1400-EXIT-PUT-MODIFY-ROW                        00169600
169700       WHEN EXIT-PUT-INSERT-ROW                                   00169700
169800          PERFORM 1500-EXIT-PUT-INSERT-ROW                        00169800
169900       WHEN EXIT-PUT-PURGE-ROW                                    00169900
170000          PERFORM 1600-EXIT-PUT-PURGE-ROW                         00170000
170100       WHEN EXIT-DO-SPECIAL-IO-FUNCS                              00170100
170200          PERFORM 10000-DO-SPECIAL-IO-FUNCS                       00170200
170300     END-EVALUATE                                                 00170300
170400     PERFORM 120-EXIT-STUFF                                       00170400
170500     GOBACK                                                       00170500
170600     .                                                            00170600
170700                                                                  00170700
170800                                                                  00170800
170900* ================================================================00170900
171000* Initialize data areas needed to call the i/o subroutine         00171000
171100* ================================================================00171100
171200 100-INITIALIZATION.                                              00171200
171300     INITIALIZE W00N001A                                          00171300
171400     MOVE NNNN0000-INDEX-HANDLE TO DDDTRL01-INDEX-HANDLE          00171400
171500     MOVE 0 TO WS-CHECKPOINT-INC                                  00171500
171600     MOVE 0 TO SQLCODE                                            00171600
171700     MOVE 0 TO SQL-INIT-FLAG                                      00171700
171800     IF NOT EXIT-CLOSE-CURSOR                                     00171800
171900       PERFORM 110-MOVE-PDA-FIELDS-2-DCL                          00171900
172000     END-IF                                                       00172000
172100                                                                  00172100
172200     IF (YYYN005A-ORACLE       OR EXIT-PUT-INSERT-ROW             00172200
172300         OR EXIT-PUT-PURGE-ROW OR EXIT-PUT-MODIFY-ROW)            00172300
172400       PERFORM 115-CONNECT-TO-ORACLE                              00172400
172500     END-IF                                                       00172500
172600     .                                                            00172600
172700                                                                  00172700
172800                                                                  00172800
172900* ================================================================00172900
173000* Move the elementary fields in the parameter data area to the DCL00173000
173100* ================================================================00173100
173200 110-MOVE-PDA-FIELDS-2-DCL.                                       00173200
173300     IF FC-RL-OPENING-DT OF P-DDDTRL01 = SPACES                   00173300
173400     OR FC-RL-OPENING-DT OF P-DDDTRL01 = '00/00/0000'             00173400
173500     OR FC-RL-OPENING-DT OF P-DDDTRL01 = '01/01/1600'             00173500
173600       MOVE '12/31/9999' TO FC-RL-OPENING-DT OF P-DDDTRL01        00173600
173700     END-IF                                                       00173700
173800     IF FC-RL-CLOSING-DT OF P-DDDTRL01 = SPACES                   00173800
173900     OR FC-RL-CLOSING-DT OF P-DDDTRL01 = '00/00/0000'             00173900
174000     OR FC-RL-CLOSING-DT OF P-DDDTRL01 = '01/01/1600'             00174000
174100       MOVE '12/31/9999' TO FC-RL-CLOSING-DT OF P-DDDTRL01        00174100
174200     END-IF                                                       00174200
174300     IF FC-RL-REMODEL-DT OF P-DDDTRL01 = SPACES                   00174300
174400     OR FC-RL-REMODEL-DT OF P-DDDTRL01 = '00/00/0000'             00174400
174500     OR FC-RL-REMODEL-DT OF P-DDDTRL01 = '01/01/1600'             00174500
174600       MOVE '12/31/9999' TO FC-RL-REMODEL-DT OF P-DDDTRL01        00174600
174700     END-IF                                                       00174700
174800     IF FC-RL-DELETED-DT OF P-DDDTRL01 = SPACES                   00174800
174900     OR FC-RL-DELETED-DT OF P-DDDTRL01 = '00/00/0000'             00174900
175000     OR FC-RL-DELETED-DT OF P-DDDTRL01 = '01/01/1600'             00175000
175100       MOVE '12/31/9999' TO FC-RL-DELETED-DT OF P-DDDTRL01        00175100
175200     END-IF                                                       00175200
175300                                                                  00175300
175400     MOVE FC-STORE-NO OF P-DDDTRL01                               00175400
175500       TO FC-STORE-NO OF DCLFC-XXXAIL-STORES                      00175500
175600     MOVE FC-RL-STORE-NM OF P-DDDTRL01                            00175600
175700       TO FC-RL-STORE-NM OF DCLFC-XXXAIL-STORES                   00175700
175800     MOVE FC-RL-STORE-CD OF P-DDDTRL01                            00175800
175900       TO FC-RL-STORE-CD OF DCLFC-XXXAIL-STORES                   00175900
176000     MOVE FC-RL-STORE-DIR-NM OF P-DDDTRL01                        00176000
176100       TO FC-RL-STORE-DIR-NM OF DCLFC-XXXAIL-STORES               00176100
176200     MOVE FC-RL-STORE-LOC-NM OF P-DDDTRL01                        00176200
176300       TO FC-RL-STORE-LOC-NM OF DCLFC-XXXAIL-STORES               00176300
176400     MOVE FC-RL-OPENING-DT OF P-DDDTRL01                          00176400
176500       TO FC-RL-OPENING-DT OF DCLFC-XXXAIL-STORES                 00176500
176600     MOVE FC-RL-CLOSING-DT OF P-DDDTRL01                          00176600
176700       TO FC-RL-CLOSING-DT OF DCLFC-XXXAIL-STORES                 00176700
176800     MOVE FC-RL-REMODEL-DT OF P-DDDTRL01                          00176800
176900       TO FC-RL-REMODEL-DT OF DCLFC-XXXAIL-STORES                 00176900
177000     MOVE FC-RL-DELETED-DT OF P-DDDTRL01                          00177000
177100       TO FC-RL-DELETED-DT OF DCLFC-XXXAIL-STORES                 00177100
177200     MOVE FC-RL-DISTRICT-NO OF P-DDDTRL01                         00177200
177300       TO FC-RL-DISTRICT-NO OF DCLFC-XXXAIL-STORES                00177300
177400     MOVE FC-RL-MARKET-AR-NO OF P-DDDTRL01                        00177400
177500       TO FC-RL-MARKET-AR-NO OF DCLFC-XXXAIL-STORES               00177500
177600     MOVE FC-RL-PAYROL-AR-NO OF P-DDDTRL01                        00177600
177700       TO FC-RL-PAYROL-AR-NO OF DCLFC-XXXAIL-STORES               00177700
177800     MOVE FC-RL-PAY-GROUP-NO OF P-DDDTRL01                        00177800
177900       TO FC-RL-PAY-GROUP-NO OF DCLFC-XXXAIL-STORES               00177900
178000     MOVE FC-RL-COMPANY-NO OF P-DDDTRL01                          00178000
178100       TO FC-RL-COMPANY-NO OF DCLFC-XXXAIL-STORES                 00178100
178200     MOVE FC-RL-GEO-ZONE-CD OF P-DDDTRL01                         00178200
178300       TO FC-RL-GEO-ZONE-CD OF DCLFC-XXXAIL-STORES                00178300
178400     MOVE FC-RL-GEO-ZONE-NO OF P-DDDTRL01                         00178400
178500       TO FC-RL-GEO-ZONE-NO OF DCLFC-XXXAIL-STORES                00178500
178600     MOVE FC-RL-SCAN-MAIN-CD OF P-DDDTRL01                        00178600
178700       TO FC-RL-SCAN-MAIN-CD OF DCLFC-XXXAIL-STORES               00178700
178800     MOVE FC-RL-FRONT-END-CD OF P-DDDTRL01                        00178800
178900       TO FC-RL-FRONT-END-CD OF DCLFC-XXXAIL-STORES               00178900
179000     MOVE FC-RL-PRICE-BUL-CD OF P-DDDTRL01                        00179000
179100       TO FC-RL-PRICE-BUL-CD OF DCLFC-XXXAIL-STORES               00179100
179200     MOVE FC-RL-UPC-ON-PB-CD OF P-DDDTRL01                        00179200
179300       TO FC-RL-UPC-ON-PB-CD OF DCLFC-XXXAIL-STORES               00179300
179400     MOVE FC-RL-COMPETITR-CD OF P-DDDTRL01                        00179400
179500       TO FC-RL-COMPETITR-CD OF DCLFC-XXXAIL-STORES               00179500
179600     MOVE FC-RL-ASSOC-STR-NO OF P-DDDTRL01                        00179600
179700       TO FC-RL-ASSOC-STR-NO OF DCLFC-XXXAIL-STORES               00179700
179800     MOVE FC-RL-RPRT-SEQ-NO OF P-DDDTRL01                         00179800
179900       TO FC-RL-RPRT-SEQ-NO OF DCLFC-XXXAIL-STORES                00179900
180000     MOVE FC-RL-SORT-SOS-NO OF P-DDDTRL01                         00180000
180100       TO FC-RL-SORT-SOS-NO OF DCLFC-XXXAIL-STORES                00180100
180200     MOVE FC-RL-VID-PRZN-NO OF P-DDDTRL01                         00180200
180300       TO FC-RL-VID-PRZN-NO OF DCLFC-XXXAIL-STORES                00180300
180400     MOVE FC-RL-CITY-ID-CD OF P-DDDTRL01                          00180400
180500       TO FC-RL-CITY-ID-CD OF DCLFC-XXXAIL-STORES                 00180500
180600     MOVE FC-RL-ADZONE-ABB OF P-DDDTRL01                          00180600
180700       TO FC-RL-ADZONE-ABB OF DCLFC-XXXAIL-STORES                 00180700
180800     MOVE FC-RL-ADZONE-DES OF P-DDDTRL01                          00180800
180900       TO FC-RL-ADZONE-DES OF DCLFC-XXXAIL-STORES                 00180900
181000     MOVE FC-RL-UNLOAD-SW OF P-DDDTRL01                           00181000
181100       TO FC-RL-UNLOAD-SW OF DCLFC-XXXAIL-STORES                  00181100
181200*    MOVE FN-ROLLUP-REPT-CD OF P-DDDTRL01                         00181200
181300     MOVE SPACES                                                  00181300
181400       TO FN-ROLLUP-REPT-CD OF DCLFC-XXXAIL-STORES                00181400
181500     PERFORM 117-MOVE-ROLLUP-DATA                                 00181500
181600     MOVE FC-RL-STATUS-CD OF P-DDDTRL01                           00181600
181700       TO FC-RL-STATUS-CD OF DCLFC-XXXAIL-STORES                  00181700
181800     MOVE FC-RL-NEW-STORE-CD OF P-DDDTRL01                        00181800
181900       TO FC-RL-NEW-STORE-CD OF DCLFC-XXXAIL-STORES               00181900
182000     MOVE FC-RL-TYPE-CD OF P-DDDTRL01                             00182000
182100       TO FC-RL-TYPE-CD OF DCLFC-XXXAIL-STORES                    00182100
182200     MOVE FC-RL-GROUP-CD OF P-DDDTRL01                            00182200
182300       TO FC-RL-GROUP-CD OF DCLFC-XXXAIL-STORES                   00182300
182400     MOVE FC-RL-SELECTCIR-CD OF P-DDDTRL01                        00182400
182500       TO FC-RL-SELECTCIR-CD OF DCLFC-XXXAIL-STORES               00182500
182600     MOVE FC-RL-AREA-CODE-NO OF P-DDDTRL01                        00182600
182700       TO FC-RL-AREA-CODE-NO OF DCLFC-XXXAIL-STORES               00182700
182800     MOVE FC-RL-TELEPHONE-NO OF P-DDDTRL01                        00182800
182900       TO FC-RL-TELEPHONE-NO OF DCLFC-XXXAIL-STORES               00182900
183000     MOVE FC-RL-STORE-ABB OF P-DDDTRL01                           00183000
183100       TO FC-RL-STORE-ABB OF DCLFC-XXXAIL-STORES                  00183100
183200     MOVE FC-RL-BCKRM-FT-QTY OF P-DDDTRL01                        00183200
183300       TO FC-RL-BCKRM-FT-QTY OF DCLFC-XXXAIL-STORES               00183300
183400     MOVE FC-RL-LFT-FOOD-QTY OF P-DDDTRL01                        00183400
183500       TO FC-RL-LFT-FOOD-QTY OF DCLFC-XXXAIL-STORES               00183500
183600     MOVE FC-RL-LFT-NONF-QTY OF P-DDDTRL01                        00183600
183700       TO FC-RL-LFT-NONF-QTY OF DCLFC-XXXAIL-STORES               00183700
183800     MOVE FC-RL-SETOFF-CD OF P-DDDTRL01                           00183800
183900       TO FC-RL-SETOFF-CD OF DCLFC-XXXAIL-STORES                  00183900
184000     MOVE FC-RL-CL12-ZONE-NO OF P-DDDTRL01                        00184000
184100       TO FC-RL-CL12-ZONE-NO OF DCLFC-XXXAIL-STORES               00184100
184200     MOVE FC-RL-CL12-ADZN-NO OF P-DDDTRL01                        00184200
184300       TO FC-RL-CL12-ADZN-NO OF DCLFC-XXXAIL-STORES               00184300
184400     MOVE FC-RL-CL13-ZONE-NO OF P-DDDTRL01                        00184400
184500       TO FC-RL-CL13-ZONE-NO OF DCLFC-XXXAIL-STORES               00184500
184600     MOVE FC-RL-CL13-ADZN-NO OF P-DDDTRL01                        00184600
184700       TO FC-RL-CL13-ADZN-NO OF DCLFC-XXXAIL-STORES               00184700
184800     MOVE FC-RL-CL14-ZONE-NO OF P-DDDTRL01                        00184800
184900       TO FC-RL-CL14-ZONE-NO OF DCLFC-XXXAIL-STORES               00184900
185000     MOVE FC-RL-CL14-ADZN-NO OF P-DDDTRL01                        00185000
185100       TO FC-RL-CL14-ADZN-NO OF DCLFC-XXXAIL-STORES               00185100
185200     MOVE FC-RL-CL36-ADZN-NO OF P-DDDTRL01                        00185200
185300       TO FC-RL-CL36-ADZN-NO OF DCLFC-XXXAIL-STORES               00185300
185400     MOVE FC-RL-CL37-ADZN-NO OF P-DDDTRL01                        00185400
185500       TO FC-RL-CL37-ADZN-NO OF DCLFC-XXXAIL-STORES               00185500
185600     MOVE FC-RL-STORE-DEA-NO OF P-DDDTRL01                        00185600
185700       TO FC-RL-STORE-DEA-NO OF DCLFC-XXXAIL-STORES               00185700
185800     MOVE FC-RL-RETL-ZONE-NO OF P-DDDTRL01                        00185800
185900       TO FC-RL-RETL-ZONE-NO OF DCLFC-XXXAIL-STORES               00185900
186000     MOVE FC-RL-STOR2-LOC-NM OF P-DDDTRL01                        00186000
186100       TO FC-RL-STOR2-LOC-NM OF DCLFC-XXXAIL-STORES               00186100
186200     MOVE FC-CITY-ADR OF P-DDDTRL01                               00186200
186300       TO FC-CITY-ADR OF DCLFC-XXXAIL-STORES                      00186300
186400     MOVE FC-STATE-ADR OF P-DDDTRL01                              00186400
186500       TO FC-STATE-ADR OF DCLFC-XXXAIL-STORES                     00186500
186600     MOVE FC-ZIP-CODE5-ADR OF P-DDDTRL01                          00186600
186700       TO FC-ZIP-CODE5-ADR OF DCLFC-XXXAIL-STORES                 00186700
186800     MOVE FC-ZIP-CODE4-ADR OF P-DDDTRL01                          00186800
186900       TO FC-ZIP-CODE4-ADR OF DCLFC-XXXAIL-STORES                 00186900
187000     MOVE FC-RL-SOS-TYPE-CD OF P-DDDTRL01                         00187000
187100       TO FC-RL-SOS-TYPE-CD OF DCLFC-XXXAIL-STORES                00187100
187200     MOVE FC-RL-NOPROCESS-CD OF P-DDDTRL01                        00187200
187300       TO FC-RL-NOPROCESS-CD OF DCLFC-XXXAIL-STORES               00187300
187400     MOVE FC-RL-SOSHDRTYP-CD OF P-DDDTRL01                        00187400
187500       TO FC-RL-SOSHDRTYP-CD OF DCLFC-XXXAIL-STORES               00187500
187600     MOVE FC-RL-CAT-CLASS-TB OF P-DDDTRL01                        00187600
187700       TO FC-RL-CAT-CLASS-TB OF DCLFC-XXXAIL-STORES               00187700
187800     MOVE FC-RL-LATITUDE-K OF P-DDDTRL01                          00187800
187900       TO FC-RL-LATITUDE-K OF DCLFC-XXXAIL-STORES                 00187900
188000     MOVE FC-RL-LONGITUDE-K OF P-DDDTRL01                         00188000
188100       TO FC-RL-LONGITUDE-K OF DCLFC-XXXAIL-STORES                00188100
188200     MOVE FN-DIVISION-CD OF P-DDDTRL01                            00188200
188300       TO FN-DIVISION-CD OF DCLFC-XXXAIL-STORES                   00188300
188400     MOVE FN-LINE-OF-BUS-CD OF P-DDDTRL01                         00188400
188500       TO FN-LINE-OF-BUS-CD OF DCLFC-XXXAIL-STORES                00188500
188600     .                                                            00188600
188700                                                                  00188700
188800                                                                  00188800
188900* ================================================================00188900
189000* Connecting to oracle database                                   00189000
189100* ================================================================00189100
189200 115-CONNECT-TO-ORACLE.                                           00189200
189300     CALL Z-ORA-CONNECT USING W00N001A                            00189300
189400                              SQLCA                               00189400
189500     IF NOT SUCCESS                                               00189500
189600       MOVE SQLCODE TO WS-SQLCODE                                 00189600
189700       MOVE SPACES  TO IS-RTRN-MSG-TXT                            00189700
189800       STRING 'NNNS0120 - Error connecting to Oracle. Sqlcode ='  00189800
189900               WS-SQLCODE                                         00189900
190000               DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT             00190000
190100     END-IF                                                       00190100
190200     .                                                            00190200
190300                                                                  00190300
190400                                                                  00190400
190500 117-MOVE-ROLLUP-DATA.                                            00190500
190600     MOVE FN-ROLLUP-REPT-CD OF P-DDDTRL01                         00190600
190700       TO WS-REPT-TBL-TXT                                         00190700
190800     IF WS-REPT-TBL-NUMERIC(1) IS NUMERIC                         00190800
190900        MOVE WS-REPT-TBL-NUMERIC(1)                               00190900
191000          TO FN-ROLLUP-REPT-01-NBR  OF DCLFC-XXXAIL-STORES        00191000
191100     ELSE                                                         00191100
191200        MOVE ZERO TO FN-ROLLUP-REPT-01-NBR OF DCLFC-XXXAIL-STORES 00191200
191300     END-IF                                                       00191300
191400     IF WS-REPT-TBL-NUMERIC(2) IS NUMERIC                         00191400
191500        MOVE WS-REPT-TBL-NUMERIC(2)                               00191500
191600          TO FN-ROLLUP-REPT-02-NBR  OF DCLFC-XXXAIL-STORES        00191600
191700     ELSE                                                         00191700
191800        MOVE ZERO TO FN-ROLLUP-REPT-02-NBR OF DCLFC-XXXAIL-STORES 00191800
191900     END-IF                                                       00191900
192000     IF WS-REPT-TBL-NUMERIC(3) IS NUMERIC                         00192000
192100        MOVE WS-REPT-TBL-NUMERIC(3)                               00192100
192200          TO FN-ROLLUP-REPT-03-NBR  OF DCLFC-XXXAIL-STORES        00192200
192300     ELSE                                                         00192300
192400        MOVE ZERO TO FN-ROLLUP-REPT-03-NBR OF DCLFC-XXXAIL-STORES 00192400
192500     END-IF                                                       00192500
192600     IF WS-REPT-TBL-NUMERIC(4) IS NUMERIC                         00192600
192700        MOVE WS-REPT-TBL-NUMERIC(4)                               00192700
192800          TO FN-ROLLUP-REPT-04-NBR  OF DCLFC-XXXAIL-STORES        00192800
192900     ELSE                                                         00192900
193000        MOVE ZERO TO FN-ROLLUP-REPT-04-NBR OF DCLFC-XXXAIL-STORES 00193000
193100     END-IF                                                       00193100
193200     IF WS-REPT-TBL-NUMERIC(5) IS NUMERIC                         00193200
193300        MOVE WS-REPT-TBL-NUMERIC(5)                               00193300
193400          TO FN-ROLLUP-REPT-05-NBR  OF DCLFC-XXXAIL-STORES        00193400
193500     ELSE                                                         00193500
193600        MOVE ZERO TO FN-ROLLUP-REPT-05-NBR OF DCLFC-XXXAIL-STORES 00193600
193700     END-IF                                                       00193700
193800     IF WS-REPT-TBL-NUMERIC(6) IS NUMERIC                         00193800
193900        MOVE WS-REPT-TBL-NUMERIC(6)                               00193900
194000          TO FN-ROLLUP-REPT-06-NBR  OF DCLFC-XXXAIL-STORES        00194000
194100     ELSE                                                         00194100
194200        MOVE ZERO TO FN-ROLLUP-REPT-06-NBR OF DCLFC-XXXAIL-STORES 00194200
194300     END-IF                                                       00194300
194400     IF WS-REPT-TBL-NUMERIC(7) IS NUMERIC                         00194400
194500        MOVE WS-REPT-TBL-NUMERIC(7)                               00194500
194600          TO FN-ROLLUP-REPT-07-NBR  OF DCLFC-XXXAIL-STORES        00194600
194700     ELSE                                                         00194700
194800        MOVE ZERO TO FN-ROLLUP-REPT-07-NBR OF DCLFC-XXXAIL-STORES 00194800
194900     END-IF                                                       00194900
195000     IF WS-REPT-TBL-NUMERIC(8) IS NUMERIC                         00195000
195100        MOVE WS-REPT-TBL-NUMERIC(8)                               00195100
195200          TO FN-ROLLUP-REPT-08-NBR  OF DCLFC-XXXAIL-STORES        00195200
195300     ELSE                                                         00195300
195400        MOVE ZERO TO FN-ROLLUP-REPT-08-NBR OF DCLFC-XXXAIL-STORES 00195400
195500     END-IF                                                       00195500
195600     IF WS-REPT-TBL-NUMERIC(9) IS NUMERIC                         00195600
195700        MOVE WS-REPT-TBL-NUMERIC(9)                               00195700
195800          TO FN-ROLLUP-REPT-09-NBR  OF DCLFC-XXXAIL-STORES        00195800
195900     ELSE                                                         00195900
196000        MOVE ZERO TO FN-ROLLUP-REPT-09-NBR OF DCLFC-XXXAIL-STORES 00196000
196100     END-IF                                                       00196100
196200     IF WS-REPT-TBL-NUMERIC(10) IS NUMERIC                        00196200
196300        MOVE WS-REPT-TBL-NUMERIC(10)                              00196300
196400          TO FN-ROLLUP-REPT-10-NBR  OF DCLFC-XXXAIL-STORES        00196400
196500     ELSE                                                         00196500
196600        MOVE ZERO TO FN-ROLLUP-REPT-10-NBR OF DCLFC-XXXAIL-STORES 00196600
196700     END-IF                                                       00196700
196800     .                                                            00196800
196900                                                                  00196900
197000                                                                  00197000
197100* ================================================================00197100
197200* Stuff to do on exit.                                            00197200
197300* ================================================================00197300
197400 120-EXIT-STUFF.                                                  00197400
197500     IF SUCCESS                                                   00197500
197600       IF NOT EXIT-CLOSE-CURSOR                                   00197600
197700         PERFORM 130-MOVE-DCL-2-PDA-FIELDS                        00197700
197800       END-IF                                                     00197800
197900         ADD WS-CHECKPOINT-INC TO YYYN005A-CHKPT-CNT              00197900
198000     END-IF                                                       00198000
198100     IF (YYYN005A-ORACLE       OR EXIT-PUT-INSERT-ROW             00198100
198200         OR EXIT-PUT-PURGE-ROW OR EXIT-PUT-MODIFY-ROW)            00198200
198300        PERFORM  125-CONNECT-TO-DB2                               00198300
198400     END-IF                                                       00198400
198500     .                                                            00198500
198600                                                                  00198600
198700                                                                  00198700
198800* ================================================================00198800
198900* Connecting to DB2 database                                      00198900
199000* ================================================================00199000
199100 125-CONNECT-TO-DB2.                                              00199100
199200     CALL Z-DB2-CONNECT         USING W00N001A                    00199200
199300                                      SQLCA                       00199300
199400     .                                                            00199400
199500                                                                  00199500
199600                                                                  00199600
199700* ================================================================00199700
199800* Move the elementary fields of the DCL to parameter data area.   00199800
199900* ================================================================00199900
200000 130-MOVE-DCL-2-PDA-FIELDS.                                       00200000
200100     MOVE FC-STORE-NO OF DCLFC-XXXAIL-STORES                      00200100
200200       TO FC-STORE-NO OF P-DDDTRL01                               00200200
200300     MOVE FC-RL-STORE-NM OF DCLFC-XXXAIL-STORES                   00200300
200400       TO FC-RL-STORE-NM OF P-DDDTRL01                            00200400
200500     MOVE FC-RL-STORE-CD OF DCLFC-XXXAIL-STORES                   00200500
200600       TO FC-RL-STORE-CD OF P-DDDTRL01                            00200600
200700     MOVE FC-RL-STORE-DIR-NM OF DCLFC-XXXAIL-STORES               00200700
200800       TO FC-RL-STORE-DIR-NM OF P-DDDTRL01                        00200800
200900     MOVE FC-RL-STORE-LOC-NM OF DCLFC-XXXAIL-STORES               00200900
201000       TO FC-RL-STORE-LOC-NM OF P-DDDTRL01                        00201000
201100     MOVE FC-RL-OPENING-DT OF DCLFC-XXXAIL-STORES                 00201100
201200       TO FC-RL-OPENING-DT OF P-DDDTRL01                          00201200
201300     MOVE FC-RL-CLOSING-DT OF DCLFC-XXXAIL-STORES                 00201300
201400       TO FC-RL-CLOSING-DT OF P-DDDTRL01                          00201400
201500     MOVE FC-RL-REMODEL-DT OF DCLFC-XXXAIL-STORES                 00201500
201600       TO FC-RL-REMODEL-DT OF P-DDDTRL01                          00201600
201700     MOVE FC-RL-DELETED-DT OF DCLFC-XXXAIL-STORES                 00201700
201800       TO FC-RL-DELETED-DT OF P-DDDTRL01                          00201800
201900     MOVE FC-RL-DISTRICT-NO OF DCLFC-XXXAIL-STORES                00201900
202000       TO FC-RL-DISTRICT-NO OF P-DDDTRL01                         00202000
202100     MOVE FC-RL-MARKET-AR-NO OF DCLFC-XXXAIL-STORES               00202100
202200       TO FC-RL-MARKET-AR-NO OF P-DDDTRL01                        00202200
202300     MOVE FC-RL-PAYROL-AR-NO OF DCLFC-XXXAIL-STORES               00202300
202400       TO FC-RL-PAYROL-AR-NO OF P-DDDTRL01                        00202400
202500     MOVE FC-RL-PAY-GROUP-NO OF DCLFC-XXXAIL-STORES               00202500
202600       TO FC-RL-PAY-GROUP-NO OF P-DDDTRL01                        00202600
202700     MOVE FC-RL-COMPANY-NO OF DCLFC-XXXAIL-STORES                 00202700
202800       TO FC-RL-COMPANY-NO OF P-DDDTRL01                          00202800
202900     MOVE FC-RL-GEO-ZONE-CD OF DCLFC-XXXAIL-STORES                00202900
203000       TO FC-RL-GEO-ZONE-CD OF P-DDDTRL01                         00203000
203100     MOVE FC-RL-GEO-ZONE-NO OF DCLFC-XXXAIL-STORES                00203100
203200       TO FC-RL-GEO-ZONE-NO OF P-DDDTRL01                         00203200
203300     MOVE FC-RL-SCAN-MAIN-CD OF DCLFC-XXXAIL-STORES               00203300
203400       TO FC-RL-SCAN-MAIN-CD OF P-DDDTRL01                        00203400
203500     MOVE FC-RL-FRONT-END-CD OF DCLFC-XXXAIL-STORES               00203500
203600       TO FC-RL-FRONT-END-CD OF P-DDDTRL01                        00203600
203700     MOVE FC-RL-PRICE-BUL-CD OF DCLFC-XXXAIL-STORES               00203700
203800       TO FC-RL-PRICE-BUL-CD OF P-DDDTRL01                        00203800
203900     MOVE FC-RL-UPC-ON-PB-CD OF DCLFC-XXXAIL-STORES               00203900
204000       TO FC-RL-UPC-ON-PB-CD OF P-DDDTRL01                        00204000
204100     MOVE FC-RL-COMPETITR-CD OF DCLFC-XXXAIL-STORES               00204100
204200       TO FC-RL-COMPETITR-CD OF P-DDDTRL01                        00204200
204300     MOVE FC-RL-ASSOC-STR-NO OF DCLFC-XXXAIL-STORES               00204300
204400       TO FC-RL-ASSOC-STR-NO OF P-DDDTRL01                        00204400
204500     MOVE FC-RL-RPRT-SEQ-NO OF DCLFC-XXXAIL-STORES                00204500
204600       TO FC-RL-RPRT-SEQ-NO OF P-DDDTRL01                         00204600
204700     MOVE FC-RL-SORT-SOS-NO OF DCLFC-XXXAIL-STORES                00204700
204800       TO FC-RL-SORT-SOS-NO OF P-DDDTRL01                         00204800
204900     MOVE FC-RL-VID-PRZN-NO OF DCLFC-XXXAIL-STORES                00204900
205000       TO FC-RL-VID-PRZN-NO OF P-DDDTRL01                         00205000
205100     MOVE FC-RL-CITY-ID-CD OF DCLFC-XXXAIL-STORES                 00205100
205200       TO FC-RL-CITY-ID-CD OF P-DDDTRL01                          00205200
205300     MOVE FC-RL-ADZONE-ABB OF DCLFC-XXXAIL-STORES                 00205300
205400       TO FC-RL-ADZONE-ABB OF P-DDDTRL01                          00205400
205500     MOVE FC-RL-ADZONE-DES OF DCLFC-XXXAIL-STORES                 00205500
205600       TO FC-RL-ADZONE-DES OF P-DDDTRL01                          00205600
205700     MOVE FC-RL-UNLOAD-SW OF DCLFC-XXXAIL-STORES                  00205700
205800       TO FC-RL-UNLOAD-SW OF P-DDDTRL01                           00205800
205900*    MOVE FN-ROLLUP-REPT-CD OF DCLFC-XXXAIL-STORES                00205900
206000*      TO FN-ROLLUP-REPT-CD OF P-DDDTRL01                         00206000
206100     PERFORM 135-MOVE-DC-ROLLUP-DATA                              00206100
206200     MOVE FC-RL-STATUS-CD OF DCLFC-XXXAIL-STORES                  00206200
206300       TO FC-RL-STATUS-CD OF P-DDDTRL01                           00206300
206400     MOVE FC-RL-NEW-STORE-CD OF DCLFC-XXXAIL-STORES               00206400
206500       TO FC-RL-NEW-STORE-CD OF P-DDDTRL01                        00206500
206600     MOVE FC-RL-TYPE-CD OF DCLFC-XXXAIL-STORES                    00206600
206700       TO FC-RL-TYPE-CD OF P-DDDTRL01                             00206700
206800     MOVE FC-RL-GROUP-CD OF DCLFC-XXXAIL-STORES                   00206800
206900       TO FC-RL-GROUP-CD OF P-DDDTRL01                            00206900
207000     MOVE FC-RL-SELECTCIR-CD OF DCLFC-XXXAIL-STORES               00207000
207100       TO FC-RL-SELECTCIR-CD OF P-DDDTRL01                        00207100
207200     MOVE FC-RL-AREA-CODE-NO OF DCLFC-XXXAIL-STORES               00207200
207300       TO FC-RL-AREA-CODE-NO OF P-DDDTRL01                        00207300
207400     MOVE FC-RL-TELEPHONE-NO OF DCLFC-XXXAIL-STORES               00207400
207500       TO FC-RL-TELEPHONE-NO OF P-DDDTRL01                        00207500
207600     MOVE FC-RL-STORE-ABB OF DCLFC-XXXAIL-STORES                  00207600
207700       TO FC-RL-STORE-ABB OF P-DDDTRL01                           00207700
207800     MOVE FC-RL-BCKRM-FT-QTY OF DCLFC-XXXAIL-STORES               00207800
207900       TO FC-RL-BCKRM-FT-QTY OF P-DDDTRL01                        00207900
208000     MOVE FC-RL-LFT-FOOD-QTY OF DCLFC-XXXAIL-STORES               00208000
208100       TO FC-RL-LFT-FOOD-QTY OF P-DDDTRL01                        00208100
208200     MOVE FC-RL-LFT-NONF-QTY OF DCLFC-XXXAIL-STORES               00208200
208300       TO FC-RL-LFT-NONF-QTY OF P-DDDTRL01                        00208300
208400     MOVE FC-RL-SETOFF-CD OF DCLFC-XXXAIL-STORES                  00208400
208500       TO FC-RL-SETOFF-CD OF P-DDDTRL01                           00208500
208600     MOVE FC-RL-CL12-ZONE-NO OF DCLFC-XXXAIL-STORES               00208600
208700       TO FC-RL-CL12-ZONE-NO OF P-DDDTRL01                        00208700
208800     MOVE FC-RL-CL12-ADZN-NO OF DCLFC-XXXAIL-STORES               00208800
208900       TO FC-RL-CL12-ADZN-NO OF P-DDDTRL01                        00208900
209000     MOVE FC-RL-CL13-ZONE-NO OF DCLFC-XXXAIL-STORES               00209000
209100       TO FC-RL-CL13-ZONE-NO OF P-DDDTRL01                        00209100
209200     MOVE FC-RL-CL13-ADZN-NO OF DCLFC-XXXAIL-STORES               00209200
209300       TO FC-RL-CL13-ADZN-NO OF P-DDDTRL01                        00209300
209400     MOVE FC-RL-CL14-ZONE-NO OF DCLFC-XXXAIL-STORES               00209400
209500       TO FC-RL-CL14-ZONE-NO OF P-DDDTRL01                        00209500
209600     MOVE FC-RL-CL14-ADZN-NO OF DCLFC-XXXAIL-STORES               00209600
209700       TO FC-RL-CL14-ADZN-NO OF P-DDDTRL01                        00209700
209800     MOVE FC-RL-CL36-ADZN-NO OF DCLFC-XXXAIL-STORES               00209800
209900       TO FC-RL-CL36-ADZN-NO OF P-DDDTRL01                        00209900
210000     MOVE FC-RL-CL37-ADZN-NO OF DCLFC-XXXAIL-STORES               00210000
210100       TO FC-RL-CL37-ADZN-NO OF P-DDDTRL01                        00210100
210200     MOVE FC-RL-STORE-DEA-NO OF DCLFC-XXXAIL-STORES               00210200
210300       TO FC-RL-STORE-DEA-NO OF P-DDDTRL01                        00210300
210400     MOVE FC-RL-RETL-ZONE-NO OF DCLFC-XXXAIL-STORES               00210400
210500       TO FC-RL-RETL-ZONE-NO OF P-DDDTRL01                        00210500
210600     MOVE FC-RL-STOR2-LOC-NM OF DCLFC-XXXAIL-STORES               00210600
210700       TO FC-RL-STOR2-LOC-NM OF P-DDDTRL01                        00210700
210800     MOVE FC-CITY-ADR OF DCLFC-XXXAIL-STORES                      00210800
210900       TO FC-CITY-ADR OF P-DDDTRL01                               00210900
211000     MOVE FC-STATE-ADR OF DCLFC-XXXAIL-STORES                     00211000
211100       TO FC-STATE-ADR OF P-DDDTRL01                              00211100
211200     MOVE FC-ZIP-CODE5-ADR OF DCLFC-XXXAIL-STORES                 00211200
211300       TO FC-ZIP-CODE5-ADR OF P-DDDTRL01                          00211300
211400     MOVE FC-ZIP-CODE4-ADR OF DCLFC-XXXAIL-STORES                 00211400
211500       TO FC-ZIP-CODE4-ADR OF P-DDDTRL01                          00211500
211600     MOVE FC-RL-SOS-TYPE-CD OF DCLFC-XXXAIL-STORES                00211600
211700       TO FC-RL-SOS-TYPE-CD OF P-DDDTRL01                         00211700
211800     MOVE FC-RL-NOPROCESS-CD OF DCLFC-XXXAIL-STORES               00211800
211900       TO FC-RL-NOPROCESS-CD OF P-DDDTRL01                        00211900
212000     MOVE FC-RL-SOSHDRTYP-CD OF DCLFC-XXXAIL-STORES               00212000
212100       TO FC-RL-SOSHDRTYP-CD OF P-DDDTRL01                        00212100
212200     MOVE FC-RL-CAT-CLASS-TB OF DCLFC-XXXAIL-STORES               00212200
212300       TO FC-RL-CAT-CLASS-TB OF P-DDDTRL01                        00212300
212400     MOVE FC-RL-LATITUDE-K OF DCLFC-XXXAIL-STORES                 00212400
212500       TO FC-RL-LATITUDE-K OF P-DDDTRL01                          00212500
212600     MOVE FC-RL-LONGITUDE-K OF DCLFC-XXXAIL-STORES                00212600
212700       TO FC-RL-LONGITUDE-K OF P-DDDTRL01                         00212700
212800     MOVE FN-DIVISION-CD OF DCLFC-XXXAIL-STORES                   00212800
212900       TO FN-DIVISION-CD OF P-DDDTRL01                            00212900
213000     MOVE FN-LINE-OF-BUS-CD OF DCLFC-XXXAIL-STORES                00213000
213100       TO FN-LINE-OF-BUS-CD OF P-DDDTRL01                         00213100
213200     .                                                            00213200
213300                                                                  00213300
213400                                                                  00213400
213500 135-MOVE-DC-ROLLUP-DATA.                                         00213500
213600     IF FN-ROLLUP-REPT-01-NBR OF DCLFC-XXXAIL-STORES IS NUMERIC   00213600
213700        MOVE FN-ROLLUP-REPT-01-NBR OF DCLFC-XXXAIL-STORES         00213700
213800          TO WS-REPT-TBL-NUMERIC(1)                               00213800
213900     ELSE                                                         00213900
214000        MOVE ZERO TO WS-REPT-TBL-NUMERIC(1)                       00214000
214100     END-IF                                                       00214100
214200     IF FN-ROLLUP-REPT-02-NBR OF DCLFC-XXXAIL-STORES IS NUMERIC   00214200
214300        MOVE FN-ROLLUP-REPT-02-NBR OF DCLFC-XXXAIL-STORES         00214300
214400          TO WS-REPT-TBL-NUMERIC(2)                               00214400
214500     ELSE                                                         00214500
214600        MOVE ZERO TO WS-REPT-TBL-NUMERIC(2)                       00214600
214700     END-IF                                                       00214700
214800     IF FN-ROLLUP-REPT-03-NBR OF DCLFC-XXXAIL-STORES IS NUMERIC   00214800
214900        MOVE FN-ROLLUP-REPT-03-NBR OF DCLFC-XXXAIL-STORES         00214900
215000          TO WS-REPT-TBL-NUMERIC(3)                               00215000
215100     ELSE                                                         00215100
215200        MOVE ZERO TO WS-REPT-TBL-NUMERIC(3)                       00215200
215300     END-IF                                                       00215300
215400     IF FN-ROLLUP-REPT-04-NBR OF DCLFC-XXXAIL-STORES IS NUMERIC   00215400
215500        MOVE FN-ROLLUP-REPT-04-NBR OF DCLFC-XXXAIL-STORES         00215500
215600          TO WS-REPT-TBL-NUMERIC(4)                               00215600
215700     ELSE                                                         00215700
215800        MOVE ZERO TO WS-REPT-TBL-NUMERIC(4)                       00215800
215900     END-IF                                                       00215900
216000     IF FN-ROLLUP-REPT-05-NBR OF DCLFC-XXXAIL-STORES IS NUMERIC   00216000
216100        MOVE FN-ROLLUP-REPT-05-NBR OF DCLFC-XXXAIL-STORES         00216100
216200          TO WS-REPT-TBL-NUMERIC(5)                               00216200
216300     ELSE                                                         00216300
216400        MOVE ZERO TO WS-REPT-TBL-NUMERIC(5)                       00216400
216500     END-IF                                                       00216500
216600     IF FN-ROLLUP-REPT-06-NBR OF DCLFC-XXXAIL-STORES IS NUMERIC   00216600
216700        MOVE FN-ROLLUP-REPT-06-NBR OF DCLFC-XXXAIL-STORES         00216700
216800          TO WS-REPT-TBL-NUMERIC(6)                               00216800
216900     ELSE                                                         00216900
217000        MOVE ZERO TO WS-REPT-TBL-NUMERIC(6)                       00217000
217100     END-IF                                                       00217100
217200     IF FN-ROLLUP-REPT-07-NBR OF DCLFC-XXXAIL-STORES IS NUMERIC   00217200
217300        MOVE FN-ROLLUP-REPT-07-NBR OF DCLFC-XXXAIL-STORES         00217300
217400          TO WS-REPT-TBL-NUMERIC(7)                               00217400
217500     ELSE                                                         00217500
217600        MOVE ZERO TO WS-REPT-TBL-NUMERIC(7)                       00217600
217700     END-IF                                                       00217700
217800     IF FN-ROLLUP-REPT-08-NBR OF DCLFC-XXXAIL-STORES IS NUMERIC   00217800
217900        MOVE FN-ROLLUP-REPT-08-NBR OF DCLFC-XXXAIL-STORES         00217900
218000          TO WS-REPT-TBL-NUMERIC(8)                               00218000
218100     ELSE                                                         00218100
218200        MOVE ZERO TO WS-REPT-TBL-NUMERIC(8)                       00218200
218300     END-IF                                                       00218300
218400     IF FN-ROLLUP-REPT-09-NBR OF DCLFC-XXXAIL-STORES IS NUMERIC   00218400
218500        MOVE FN-ROLLUP-REPT-09-NBR OF DCLFC-XXXAIL-STORES         00218500
218600          TO WS-REPT-TBL-NUMERIC(9)                               00218600
218700     ELSE                                                         00218700
218800        MOVE ZERO TO WS-REPT-TBL-NUMERIC(9)                       00218800
218900     END-IF                                                       00218900
219000     IF FN-ROLLUP-REPT-10-NBR OF DCLFC-XXXAIL-STORES IS NUMERIC   00219000
219100        MOVE FN-ROLLUP-REPT-10-NBR OF DCLFC-XXXAIL-STORES         00219100
219200          TO WS-REPT-TBL-NUMERIC(10)                              00219200
219300     ELSE                                                         00219300
219400        MOVE ZERO TO WS-REPT-TBL-NUMERIC(10)                      00219400
219500     END-IF                                                       00219500
219600     MOVE WS-REPT-TBL-TXT                                         00219600
219700       TO FN-ROLLUP-REPT-CD OF P-DDDTRL01                         00219700
219800     .                                                            00219800
219900                                                                  00219900
220000                                                                  00220000
220100* ================================================================00220100
220200* Code required to do static sql including cursor, select, fetch, 00220200
220300* update, insert, and delete operations.                          00220300
220400* ================================================================00220400
220500 1000-EXIT-OPEN-CURSOR.                                           00220500
220600     EVALUATE TRUE                                                00220600
220700       WHEN RFCXRL01                                              00220700
220800         EXEC SQL                                                 00220800
220900           OPEN RFCXRL01                                          00220900
221000         END-EXEC                                                 00221000
221100       WHEN RFCXRL02                                              00221100
221200         EXEC SQL                                                 00221200
221300           OPEN RFCXRL02                                          00221300
221400         END-EXEC                                                 00221400
221500       WHEN RFCXRL03                                              00221500
221600         EXEC SQL                                                 00221600
221700           OPEN RFCXRL03                                          00221700
221800         END-EXEC                                                 00221800
221900       WHEN RFCXRL04                                              00221900
222000         EXEC SQL                                                 00222000
222100           OPEN RFCXRL04                                          00222100
222200         END-EXEC                                                 00222200
222300       WHEN RFCXRL05                                              00222300
222400         EXEC SQL                                                 00222400
222500           OPEN RFCXRL05                                          00222500
222600         END-EXEC                                                 00222600
222700       WHEN RFCXRL06                                              00222700
222800         EXEC SQL                                                 00222800
222900           OPEN RFCXRL06                                          00222900
223000         END-EXEC                                                 00223000
223100       WHEN RFCXRL07                                              00223100
223200         EXEC SQL                                                 00223200
223300           OPEN RFCXRL07                                          00223300
223400         END-EXEC                                                 00223400
223500       WHEN RFCXRL08                                              00223500
223600         EXEC SQL                                                 00223600
223700           OPEN RFCXRL08                                          00223700
223800         END-EXEC                                                 00223800
223900       WHEN RFCXRL09                                              00223900
224000         EXEC SQL                                                 00224000
224100           OPEN RFCXRL09                                          00224100
224200         END-EXEC                                                 00224200
224300       WHEN RFCXRL10                                              00224300
224400         EXEC SQL                                                 00224400
224500           OPEN RFCXRL10                                          00224500
224600         END-EXEC                                                 00224600
224700       WHEN RFCXRL11                                              00224700
224800         EXEC SQL                                                 00224800
224900           OPEN RFCXRL11                                          00224900
225000         END-EXEC                                                 00225000
225100       WHEN RFCXRL12                                              00225100
225200         EXEC SQL                                                 00225200
225300           OPEN RFCXRL12                                          00225300
225400         END-EXEC                                                 00225400
225500       WHEN RFCXRL13                                              00225500
225600         EXEC SQL                                                 00225600
225700           OPEN RFCXRL13                                          00225700
225800         END-EXEC                                                 00225800
225900       WHEN RFCXRL14                                              00225900
226000         EXEC SQL                                                 00226000
226100           OPEN RFCXRL14                                          00226100
226200         END-EXEC                                                 00226200
226300       WHEN RFCXRL15                                              00226300
226400         EXEC SQL                                                 00226400
226500           OPEN RFCXRL15                                          00226500
226600         END-EXEC                                                 00226600
226700       WHEN RFCXRL16                                              00226700
226800         EXEC SQL                                                 00226800
226900           OPEN RFCXRL16                                          00226900
227000         END-EXEC                                                 00227000
227100       WHEN RFCXRL17                                              00227100
227200         EXEC SQL                                                 00227200
227300           OPEN RFCXRL17                                          00227300
227400         END-EXEC                                                 00227400
227500       WHEN OTHER                                                 00227500
227600         SET FAILURE TO TRUE                                      00227600
227700         MOVE 'NNNS0120 - Invalid open cursor ID.'                00227700
227800           TO IS-RTRN-MSG-TXT OF W00N001A                         00227800
227900     END-EVALUATE                                                 00227900
228000     .                                                            00228000
228100                                                                  00228100
228200                                                                  00228200
228300 1100-EXIT-CLOSE-CURSOR.                                          00228300
228400     EVALUATE TRUE                                                00228400
228500       WHEN RFCXRL01                                              00228500
228600         EXEC SQL                                                 00228600
228700           CLOSE RFCXRL01                                         00228700
228800         END-EXEC                                                 00228800
228900       WHEN RFCXRL02                                              00228900
229000         EXEC SQL                                                 00229000
229100           CLOSE RFCXRL02                                         00229100
229200         END-EXEC                                                 00229200
229300       WHEN RFCXRL03                                              00229300
229400         EXEC SQL                                                 00229400
229500           CLOSE RFCXRL03                                         00229500
229600         END-EXEC                                                 00229600
229700       WHEN RFCXRL04                                              00229700
229800         EXEC SQL                                                 00229800
229900           CLOSE RFCXRL04                                         00229900
230000         END-EXEC                                                 00230000
230100       WHEN RFCXRL05                                              00230100
230200         EXEC SQL                                                 00230200
230300           CLOSE RFCXRL05                                         00230300
230400         END-EXEC                                                 00230400
230500       WHEN RFCXRL06                                              00230500
230600         EXEC SQL                                                 00230600
230700           CLOSE RFCXRL06                                         00230700
230800         END-EXEC                                                 00230800
230900       WHEN RFCXRL07                                              00230900
231000         EXEC SQL                                                 00231000
231100           CLOSE RFCXRL07                                         00231100
231200         END-EXEC                                                 00231200
231300       WHEN RFCXRL08                                              00231300
231400         EXEC SQL                                                 00231400
231500           CLOSE RFCXRL08                                         00231500
231600         END-EXEC                                                 00231600
231700       WHEN RFCXRL09                                              00231700
231800         EXEC SQL                                                 00231800
231900           CLOSE RFCXRL09                                         00231900
232000         END-EXEC                                                 00232000
232100       WHEN RFCXRL10                                              00232100
232200         EXEC SQL                                                 00232200
232300           CLOSE RFCXRL10                                         00232300
232400         END-EXEC                                                 00232400
232500       WHEN RFCXRL11                                              00232500
232600         EXEC SQL                                                 00232600
232700           CLOSE RFCXRL11                                         00232700
232800         END-EXEC                                                 00232800
232900       WHEN RFCXRL12                                              00232900
233000         EXEC SQL                                                 00233000
233100           CLOSE RFCXRL12                                         00233100
233200         END-EXEC                                                 00233200
233300       WHEN RFCXRL13                                              00233300
233400         EXEC SQL                                                 00233400
233500           CLOSE RFCXRL13                                         00233500
233600         END-EXEC                                                 00233600
233700       WHEN RFCXRL14                                              00233700
233800         EXEC SQL                                                 00233800
233900           CLOSE RFCXRL14                                         00233900
234000         END-EXEC                                                 00234000
234100       WHEN RFCXRL15                                              00234100
234200         EXEC SQL                                                 00234200
234300           CLOSE RFCXRL15                                         00234300
234400         END-EXEC                                                 00234400
234500       WHEN RFCXRL16                                              00234500
234600         EXEC SQL                                                 00234600
234700           CLOSE RFCXRL16                                         00234700
234800         END-EXEC                                                 00234800
234900       WHEN RFCXRL17                                              00234900
235000         EXEC SQL                                                 00235000
235100           CLOSE RFCXRL17                                         00235100
235200         END-EXEC                                                 00235200
235300       WHEN OTHER                                                 00235300
235400         SET FAILURE TO TRUE                                      00235400
235500         MOVE 'NNNS0120 - Invalid close cursor ID.'               00235500
235600           TO IS-RTRN-MSG-TXT OF W00N001A                         00235600
235700     END-EVALUATE                                                 00235700
235800     .                                                            00235800
235900                                                                  00235900
236000                                                                  00236000
236100 1200-EXIT-GET-UNIQUE-ROW.                                        00236100
236200     EXEC SQL                                                     00236200
236300         SELECT FC_STORE_NO,                                      00236300
236400                FC_RL_STORE_NM,                                   00236400
236500                FC_RL_STORE_CD,                                   00236500
236600                FC_RL_STORE_DIR_NM,                               00236600
236700                FC_RL_STORE_LOC_NM,                               00236700
236800                FC_RL_OPENING_DT,                                 00236800
236900                FC_RL_CLOSING_DT,                                 00236900
237000                FC_RL_REMODEL_DT,                                 00237000
237100                FC_RL_DELETED_DT,                                 00237100
237200                FC_RL_DISTRICT_NO,                                00237200
237300                FC_RL_MARKET_AR_NO,                               00237300
237400                FC_RL_PAYROL_AR_NO,                               00237400
237500                FC_RL_PAY_GROUP_NO,                               00237500
237600                FC_RL_COMPANY_NO,                                 00237600
237700                FC_RL_GEO_ZONE_CD,                                00237700
237800                FC_RL_GEO_ZONE_NO,                                00237800
237900                FC_RL_SCAN_MAIN_CD,                               00237900
238000                FC_RL_FRONT_END_CD,                               00238000
238100                FC_RL_PRICE_BUL_CD,                               00238100
238200                FC_RL_UPC_ON_PB_CD,                               00238200
238300                FC_RL_COMPETITR_CD,                               00238300
238400                FC_RL_ASSOC_STR_NO,                               00238400
238500                FC_RL_RPRT_SEQ_NO,                                00238500
238600                FC_RL_SORT_SOS_NO,                                00238600
238700                FC_RL_VID_PRZN_NO,                                00238700
238800                FC_RL_CITY_ID_CD,                                 00238800
238900                FC_RL_ADZONE_ABB,                                 00238900
239000                FC_RL_ADZONE_DES,                                 00239000
239100                FC_RL_UNLOAD_SW,                                  00239100
239200                FN_ROLLUP_REPT_CD,                                00239200
239300                FC_RL_STATUS_CD,                                  00239300
239400                FC_RL_NEW_STORE_CD,                               00239400
239500                FC_RL_TYPE_CD,                                    00239500
239600                FC_RL_GROUP_CD,                                   00239600
239700                FC_RL_SELECTCIR_CD,                               00239700
239800                FC_RL_AREA_CODE_NO,                               00239800
239900                FC_RL_TELEPHONE_NO,                               00239900
240000                FC_RL_STORE_ABB,                                  00240000
240100                FC_RL_BCKRM_FT_QTY,                               00240100
240200                FC_RL_LFT_FOOD_QTY,                               00240200
240300                FC_RL_LFT_NONF_QTY,                               00240300
240400                FC_RL_SETOFF_CD,                                  00240400
240500                FC_RL_CL12_ZONE_NO,                               00240500
240600                FC_RL_CL12_ADZN_NO,                               00240600
240700                FC_RL_CL13_ZONE_NO,                               00240700
240800                FC_RL_CL13_ADZN_NO,                               00240800
240900                FC_RL_CL14_ZONE_NO,                               00240900
241000                FC_RL_CL14_ADZN_NO,                               00241000
241100                FC_RL_CL36_ADZN_NO,                               00241100
241200                FC_RL_CL37_ADZN_NO,                               00241200
241300                FC_RL_STORE_DEA_NO,                               00241300
241400                FC_RL_RETL_ZONE_NO,                               00241400
241500                FC_RL_STOR2_LOC_NM,                               00241500
241600                FC_CITY_ADR,                                      00241600
241700                FC_STATE_ADR,                                     00241700
241800                FC_ZIP_CODE5_ADR,                                 00241800
241900                FC_ZIP_CODE4_ADR,                                 00241900
242000                FC_RL_SOS_TYPE_CD,                                00242000
242100                FC_RL_NOPROCESS_CD,                               00242100
242200                FC_RL_SOSHDRTYP_CD,                               00242200
242300                FC_RL_CAT_CLASS_TB,                               00242300
242400                FC_RL_LATITUDE_K,                                 00242400
242500                FC_RL_LONGITUDE_K,                                00242500
242600                FN_DIVISION_CD,                                   00242600
242700                FN_LINE_OF_BUS_CD,                                00242700
242800                FN_ROLLUP_REPT_01_NBR,                            00242800
242900                FN_ROLLUP_REPT_02_NBR,                            00242900
243000                FN_ROLLUP_REPT_03_NBR,                            00243000
243100                FN_ROLLUP_REPT_04_NBR,                            00243100
243200                FN_ROLLUP_REPT_05_NBR,                            00243200
243300                FN_ROLLUP_REPT_06_NBR,                            00243300
243400                FN_ROLLUP_REPT_07_NBR,                            00243400
243500                FN_ROLLUP_REPT_08_NBR,                            00243500
243600                FN_ROLLUP_REPT_09_NBR,                            00243600
243700                FN_ROLLUP_REPT_10_NBR                             00243700
243800         INTO   :DCLFC-XXXAIL-STORES.FC-STORE-NO,                 00243800
243900                :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,              00243900
244000                :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,              00244000
244100                :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,          00244100
244200                :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,          00244200
244300                :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,            00244300
244400                :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,            00244400
244500                :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,            00244500
244600                :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,            00244600
244700                :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,           00244700
244800                :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,          00244800
244900                :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,          00244900
245000                :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,          00245000
245100                :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,            00245100
245200                :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,           00245200
245300                :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,           00245300
245400                :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,          00245400
245500                :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,          00245500
245600                :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,          00245600
245700                :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,          00245700
245800                :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,          00245800
245900                :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,          00245900
246000                :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,           00246000
246100                :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,           00246100
246200                :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,           00246200
246300                :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,            00246300
246400                :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,            00246400
246500                :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,            00246500
246600                :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,             00246600
246700                :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,           00246700
246800                :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,             00246800
246900                :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,          00246900
247000                :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,               00247000
247100                :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,              00247100
247200                :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,          00247200
247300                :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,          00247300
247400                :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,          00247400
247500                :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,             00247500
247600                :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,          00247600
247700                :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,          00247700
247800                :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,          00247800
247900                :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,             00247900
248000                :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,          00248000
248100                :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,          00248100
248200                :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,          00248200
248300                :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,          00248300
248400                :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,          00248400
248500                :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,          00248500
248600                :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,          00248600
248700                :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,          00248700
248800                :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,          00248800
248900                :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,          00248900
249000                :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,          00249000
249100                :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                 00249100
249200                :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                00249200
249300                :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,            00249300
249400                :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,            00249400
249500                :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,           00249500
249600                :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,          00249600
249700                :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,          00249700
249800                :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,          00249800
249900                :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,            00249900
250000                :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,           00250000
250100                :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,              00250100
250200                :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,           00250200
250300                :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,       00250300
250400                :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,       00250400
250500                :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,       00250500
250600                :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,       00250600
250700                :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,       00250700
250800                :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,       00250800
250900                :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,       00250900
251000                :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,       00251000
251100                :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,       00251100
251200                :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR        00251200
251300         FROM   FC_XXXAIL_STORES                                  00251300
251400         WHERE  FC_STORE_NO = :DCLFC-XXXAIL-STORES.FC-STORE-NO    00251400
251500     END-EXEC                                                     00251500
251600                                                                  00251600
251700     PERFORM 1700-CHECK-NULL-COLUMNS                              00251700
251800     .                                                            00251800
251900                                                                  00251900
252000                                                                  00252000
252100 1300-EXIT-GET-NEXT-ROW.                                          00252100
252200     EVALUATE TRUE                                                00252200
252300       WHEN RFCXRL01                                              00252300
252400         PERFORM 1301-FETCH-RFCXRL01                              00252400
252500       WHEN RFCXRL02                                              00252500
252600         PERFORM 1302-FETCH-RFCXRL02                              00252600
252700       WHEN RFCXRL03                                              00252700
252800         PERFORM 1303-FETCH-RFCXRL03                              00252800
252900       WHEN RFCXRL04                                              00252900
253000         PERFORM 1304-FETCH-RFCXRL04                              00253000
253100       WHEN RFCXRL05                                              00253100
253200         PERFORM 1305-FETCH-RFCXRL05                              00253200
253300       WHEN RFCXRL06                                              00253300
253400         PERFORM 1306-FETCH-RFCXRL06                              00253400
253500       WHEN RFCXRL07                                              00253500
253600         PERFORM 1307-FETCH-RFCXRL07                              00253600
253700       WHEN RFCXRL08                                              00253700
253800         PERFORM 1308-FETCH-RFCXRL08                              00253800
253900       WHEN RFCXRL09                                              00253900
254000         PERFORM 1309-FETCH-RFCXRL09                              00254000
254100       WHEN RFCXRL10                                              00254100
254200         PERFORM 1310-FETCH-RFCXRL10                              00254200
254300       WHEN RFCXRL11                                              00254300
254400         PERFORM 1311-FETCH-RFCXRL11                              00254400
254500       WHEN RFCXRL12                                              00254500
254600         PERFORM 1312-FETCH-RFCXRL12                              00254600
254700       WHEN RFCXRL13                                              00254700
254800         PERFORM 1313-FETCH-RFCXRL13                              00254800
254900       WHEN RFCXRL14                                              00254900
255000         PERFORM 1314-FETCH-RFCXRL14                              00255000
255100       WHEN RFCXRL15                                              00255100
255200         PERFORM 1315-FETCH-RFCXRL15                              00255200
255300       WHEN RFCXRL16                                              00255300
255400         PERFORM 1316-FETCH-RFCXRL16                              00255400
255500       WHEN RFCXRL17                                              00255500
255600         PERFORM 1317-FETCH-RFCXRL17                              00255600
255700       WHEN OTHER                                                 00255700
255800         SET FAILURE TO TRUE                                      00255800
255900         MOVE 'NNNS0120 - Invalid fetch cursor ID.'               00255900
256000           TO IS-RTRN-MSG-TXT OF W00N001A                         00256000
256100     END-EVALUATE                                                 00256100
256200                                                                  00256200
256300     PERFORM 1700-CHECK-NULL-COLUMNS                              00256300
256400     .                                                            00256400
256500                                                                  00256500
256600                                                                  00256600
256700 1301-FETCH-RFCXRL01.                                             00256700
256800     EXEC SQL                                                     00256800
256900         FETCH RFCXRL01                                           00256900
257000         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00257000
257100               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00257100
257200               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00257200
257300               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00257300
257400               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00257400
257500               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00257500
257600               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00257600
257700               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00257700
257800               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00257800
257900               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00257900
258000               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00258000
258100               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00258100
258200               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00258200
258300               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00258300
258400               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00258400
258500               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00258500
258600               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00258600
258700               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00258700
258800               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00258800
258900               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00258900
259000               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00259000
259100               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00259100
259200               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00259200
259300               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00259300
259400               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00259400
259500               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00259500
259600               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00259600
259700               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00259700
259800               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00259800
259900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00259900
260000               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00260000
260100               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00260100
260200               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00260200
260300               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00260300
260400               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00260400
260500               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00260500
260600               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00260600
260700               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00260700
260800               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00260800
260900               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00260900
261000               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00261000
261100               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00261100
261200               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00261200
261300               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00261300
261400               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00261400
261500               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00261500
261600               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00261600
261700               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00261700
261800               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00261800
261900               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00261900
262000               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00262000
262100               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00262100
262200               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00262200
262300               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00262300
262400               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00262400
262500               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00262500
262600               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00262600
262700               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00262700
262800               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00262800
262900               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00262900
263000               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00263000
263100               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00263100
263200               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00263200
263300               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00263300
263400               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00263400
263500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00263500
263600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00263600
263700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00263700
263800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00263800
263900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00263900
264000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00264000
264100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00264100
264200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00264200
264300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00264300
264400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00264400
264500     END-EXEC                                                     00264500
264600     .                                                            00264600
264700                                                                  00264700
264800                                                                  00264800
264900 1302-FETCH-RFCXRL02.                                             00264900
265000     EXEC SQL                                                     00265000
265100         FETCH RFCXRL02                                           00265100
265200         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00265200
265300               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00265300
265400               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00265400
265500               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00265500
265600               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00265600
265700               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00265700
265800               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00265800
265900               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00265900
266000               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00266000
266100               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00266100
266200               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00266200
266300               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00266300
266400               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00266400
266500               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00266500
266600               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00266600
266700               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00266700
266800               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00266800
266900               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00266900
267000               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00267000
267100               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00267100
267200               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00267200
267300               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00267300
267400               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00267400
267500               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00267500
267600               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00267600
267700               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00267700
267800               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00267800
267900               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00267900
268000               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00268000
268100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00268100
268200               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00268200
268300               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00268300
268400               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00268400
268500               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00268500
268600               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00268600
268700               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00268700
268800               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00268800
268900               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00268900
269000               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00269000
269100               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00269100
269200               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00269200
269300               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00269300
269400               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00269400
269500               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00269500
269600               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00269600
269700               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00269700
269800               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00269800
269900               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00269900
270000               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00270000
270100               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00270100
270200               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00270200
270300               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00270300
270400               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00270400
270500               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00270500
270600               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00270600
270700               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00270700
270800               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00270800
270900               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00270900
271000               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00271000
271100               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00271100
271200               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00271200
271300               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00271300
271400               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00271400
271500               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00271500
271600               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00271600
271700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00271700
271800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00271800
271900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00271900
272000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00272000
272100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00272100
272200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00272200
272300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00272300
272400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00272400
272500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00272500
272600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00272600
272700     END-EXEC                                                     00272700
272800     .                                                            00272800
272900                                                                  00272900
273000                                                                  00273000
273100 1303-FETCH-RFCXRL03.                                             00273100
273200     EXEC SQL                                                     00273200
273300         FETCH RFCXRL03                                           00273300
273400         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00273400
273500               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00273500
273600               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00273600
273700               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00273700
273800               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00273800
273900               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00273900
274000               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00274000
274100               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00274100
274200               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00274200
274300               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00274300
274400               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00274400
274500               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00274500
274600               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00274600
274700               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00274700
274800               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00274800
274900               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00274900
275000               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00275000
275100               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00275100
275200               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00275200
275300               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00275300
275400               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00275400
275500               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00275500
275600               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00275600
275700               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00275700
275800               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00275800
275900               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00275900
276000               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00276000
276100               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00276100
276200               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00276200
276300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00276300
276400               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00276400
276500               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00276500
276600               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00276600
276700               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00276700
276800               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00276800
276900               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00276900
277000               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00277000
277100               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00277100
277200               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00277200
277300               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00277300
277400               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00277400
277500               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00277500
277600               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00277600
277700               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00277700
277800               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00277800
277900               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00277900
278000               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00278000
278100               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00278100
278200               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00278200
278300               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00278300
278400               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00278400
278500               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00278500
278600               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00278600
278700               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00278700
278800               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00278800
278900               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00278900
279000               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00279000
279100               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00279100
279200               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00279200
279300               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00279300
279400               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00279400
279500               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00279500
279600               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00279600
279700               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00279700
279800               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00279800
279900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00279900
280000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00280000
280100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00280100
280200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00280200
280300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00280300
280400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00280400
280500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00280500
280600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00280600
280700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00280700
280800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00280800
280900     END-EXEC                                                     00280900
281000     .                                                            00281000
281100                                                                  00281100
281200                                                                  00281200
281300 1304-FETCH-RFCXRL04.                                             00281300
281400     EXEC SQL                                                     00281400
281500         FETCH RFCXRL04                                           00281500
281600         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00281600
281700               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00281700
281800               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00281800
281900               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00281900
282000               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00282000
282100               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00282100
282200               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00282200
282300               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00282300
282400               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00282400
282500               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00282500
282600               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00282600
282700               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00282700
282800               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00282800
282900               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00282900
283000               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00283000
283100               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00283100
283200               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00283200
283300               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00283300
283400               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00283400
283500               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00283500
283600               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00283600
283700               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00283700
283800               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00283800
283900               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00283900
284000               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00284000
284100               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00284100
284200               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00284200
284300               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00284300
284400               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00284400
284500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00284500
284600               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00284600
284700               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00284700
284800               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00284800
284900               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00284900
285000               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00285000
285100               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00285100
285200               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00285200
285300               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00285300
285400               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00285400
285500               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00285500
285600               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00285600
285700               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00285700
285800               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00285800
285900               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00285900
286000               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00286000
286100               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00286100
286200               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00286200
286300               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00286300
286400               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00286400
286500               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00286500
286600               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00286600
286700               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00286700
286800               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00286800
286900               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00286900
287000               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00287000
287100               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00287100
287200               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00287200
287300               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00287300
287400               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00287400
287500               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00287500
287600               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00287600
287700               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00287700
287800               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00287800
287900               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00287900
288000               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00288000
288100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00288100
288200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00288200
288300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00288300
288400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00288400
288500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00288500
288600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00288600
288700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00288700
288800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00288800
288900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00288900
289000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00289000
289100     END-EXEC                                                     00289100
289200     .                                                            00289200
289300                                                                  00289300
289400                                                                  00289400
289500 1305-FETCH-RFCXRL05.                                             00289500
289600     EXEC SQL                                                     00289600
289700         FETCH RFCXRL05                                           00289700
289800         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00289800
289900               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00289900
290000               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00290000
290100               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00290100
290200               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00290200
290300               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00290300
290400               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00290400
290500               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00290500
290600               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00290600
290700               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00290700
290800               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00290800
290900               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00290900
291000               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00291000
291100               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00291100
291200               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00291200
291300               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00291300
291400               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00291400
291500               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00291500
291600               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00291600
291700               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00291700
291800               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00291800
291900               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00291900
292000               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00292000
292100               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00292100
292200               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00292200
292300               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00292300
292400               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00292400
292500               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00292500
292600               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00292600
292700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00292700
292800               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00292800
292900               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00292900
293000               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00293000
293100               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00293100
293200               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00293200
293300               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00293300
293400               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00293400
293500               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00293500
293600               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00293600
293700               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00293700
293800               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00293800
293900               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00293900
294000               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00294000
294100               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00294100
294200               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00294200
294300               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00294300
294400               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00294400
294500               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00294500
294600               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00294600
294700               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00294700
294800               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00294800
294900               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00294900
295000               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00295000
295100               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00295100
295200               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00295200
295300               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00295300
295400               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00295400
295500               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00295500
295600               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00295600
295700               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00295700
295800               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00295800
295900               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00295900
296000               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00296000
296100               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00296100
296200               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00296200
296300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00296300
296400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00296400
296500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00296500
296600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00296600
296700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00296700
296800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00296800
296900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00296900
297000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00297000
297100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00297100
297200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00297200
297300     END-EXEC                                                     00297300
297400     .                                                            00297400
297500                                                                  00297500
297600                                                                  00297600
297700 1306-FETCH-RFCXRL06.                                             00297700
297800     EXEC SQL                                                     00297800
297900         FETCH RFCXRL06                                           00297900
298000         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00298000
298100               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00298100
298200               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00298200
298300               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00298300
298400               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00298400
298500               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00298500
298600               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00298600
298700               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00298700
298800               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00298800
298900               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00298900
299000               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00299000
299100               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00299100
299200               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00299200
299300               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00299300
299400               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00299400
299500               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00299500
299600               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00299600
299700               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00299700
299800               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00299800
299900               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00299900
300000               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00300000
300100               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00300100
300200               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00300200
300300               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00300300
300400               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00300400
300500               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00300500
300600               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00300600
300700               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00300700
300800               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00300800
300900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00300900
301000               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00301000
301100               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00301100
301200               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00301200
301300               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00301300
301400               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00301400
301500               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00301500
301600               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00301600
301700               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00301700
301800               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00301800
301900               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00301900
302000               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00302000
302100               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00302100
302200               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00302200
302300               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00302300
302400               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00302400
302500               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00302500
302600               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00302600
302700               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00302700
302800               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00302800
302900               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00302900
303000               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00303000
303100               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00303100
303200               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00303200
303300               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00303300
303400               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00303400
303500               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00303500
303600               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00303600
303700               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00303700
303800               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00303800
303900               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00303900
304000               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00304000
304100               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00304100
304200               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00304200
304300               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00304300
304400               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00304400
304500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00304500
304600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00304600
304700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00304700
304800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00304800
304900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00304900
305000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00305000
305100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00305100
305200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00305200
305300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00305300
305400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00305400
305500     END-EXEC                                                     00305500
305600     .                                                            00305600
305700                                                                  00305700
305800                                                                  00305800
305900 1307-FETCH-RFCXRL07.                                             00305900
306000     EXEC SQL                                                     00306000
306100         FETCH RFCXRL07                                           00306100
306200         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00306200
306300               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00306300
306400               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00306400
306500               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00306500
306600               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00306600
306700               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00306700
306800               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00306800
306900               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00306900
307000               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00307000
307100               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00307100
307200               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00307200
307300               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00307300
307400               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00307400
307500               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00307500
307600               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00307600
307700               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00307700
307800               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00307800
307900               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00307900
308000               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00308000
308100               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00308100
308200               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00308200
308300               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00308300
308400               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00308400
308500               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00308500
308600               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00308600
308700               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00308700
308800               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00308800
308900               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00308900
309000               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00309000
309100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00309100
309200               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00309200
309300               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00309300
309400               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00309400
309500               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00309500
309600               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00309600
309700               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00309700
309800               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00309800
309900               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00309900
310000               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00310000
310100               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00310100
310200               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00310200
310300               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00310300
310400               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00310400
310500               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00310500
310600               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00310600
310700               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00310700
310800               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00310800
310900               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00310900
311000               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00311000
311100               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00311100
311200               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00311200
311300               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00311300
311400               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00311400
311500               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00311500
311600               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00311600
311700               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00311700
311800               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00311800
311900               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00311900
312000               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00312000
312100               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00312100
312200               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00312200
312300               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00312300
312400               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00312400
312500               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00312500
312600               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00312600
312700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00312700
312800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00312800
312900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00312900
313000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00313000
313100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00313100
313200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00313200
313300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00313300
313400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00313400
313500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00313500
313600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00313600
313700     END-EXEC                                                     00313700
313800     .                                                            00313800
313900                                                                  00313900
314000                                                                  00314000
314100 1308-FETCH-RFCXRL08.                                             00314100
314200     EXEC SQL                                                     00314200
314300         FETCH RFCXRL08                                           00314300
314400         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00314400
314500               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00314500
314600               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00314600
314700               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00314700
314800               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00314800
314900               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00314900
315000               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00315000
315100               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00315100
315200               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00315200
315300               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00315300
315400               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00315400
315500               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00315500
315600               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00315600
315700               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00315700
315800               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00315800
315900               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00315900
316000               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00316000
316100               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00316100
316200               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00316200
316300               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00316300
316400               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00316400
316500               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00316500
316600               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00316600
316700               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00316700
316800               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00316800
316900               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00316900
317000               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00317000
317100               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00317100
317200               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00317200
317300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00317300
317400               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00317400
317500               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00317500
317600               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00317600
317700               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00317700
317800               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00317800
317900               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00317900
318000               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00318000
318100               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00318100
318200               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00318200
318300               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00318300
318400               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00318400
318500               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00318500
318600               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00318600
318700               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00318700
318800               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00318800
318900               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00318900
319000               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00319000
319100               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00319100
319200               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00319200
319300               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00319300
319400               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00319400
319500               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00319500
319600               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00319600
319700               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00319700
319800               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00319800
319900               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00319900
320000               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00320000
320100               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00320100
320200               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00320200
320300               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00320300
320400               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00320400
320500               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00320500
320600               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00320600
320700               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00320700
320800               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00320800
320900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00320900
321000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00321000
321100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00321100
321200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00321200
321300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00321300
321400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00321400
321500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00321500
321600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00321600
321700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00321700
321800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00321800
321900     END-EXEC                                                     00321900
322000     .                                                            00322000
322100                                                                  00322100
322200                                                                  00322200
322300 1309-FETCH-RFCXRL09.                                             00322300
322400     EXEC SQL                                                     00322400
322500         FETCH RFCXRL09                                           00322500
322600         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00322600
322700               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00322700
322800               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00322800
322900               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00322900
323000               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00323000
323100               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00323100
323200               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00323200
323300               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00323300
323400               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00323400
323500               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00323500
323600               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00323600
323700               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00323700
323800               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00323800
323900               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00323900
324000               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00324000
324100               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00324100
324200               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00324200
324300               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00324300
324400               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00324400
324500               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00324500
324600               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00324600
324700               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00324700
324800               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00324800
324900               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00324900
325000               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00325000
325100               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00325100
325200               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00325200
325300               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00325300
325400               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00325400
325500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00325500
325600               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00325600
325700               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00325700
325800               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00325800
325900               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00325900
326000               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00326000
326100               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00326100
326200               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00326200
326300               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00326300
326400               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00326400
326500               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00326500
326600               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00326600
326700               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00326700
326800               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00326800
326900               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00326900
327000               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00327000
327100               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00327100
327200               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00327200
327300               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00327300
327400               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00327400
327500               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00327500
327600               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00327600
327700               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00327700
327800               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00327800
327900               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00327900
328000               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00328000
328100               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00328100
328200               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00328200
328300               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00328300
328400               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00328400
328500               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00328500
328600               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00328600
328700               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00328700
328800               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00328800
328900               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00328900
329000               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00329000
329100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00329100
329200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00329200
329300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00329300
329400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00329400
329500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00329500
329600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00329600
329700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00329700
329800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00329800
329900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00329900
330000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00330000
330100     END-EXEC                                                     00330100
330200     .                                                            00330200
330300                                                                  00330300
330400                                                                  00330400
330500 1310-FETCH-RFCXRL10.                                             00330500
330600     EXEC SQL                                                     00330600
330700         FETCH RFCXRL10                                           00330700
330800         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00330800
330900               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00330900
331000               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00331000
331100               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00331100
331200               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00331200
331300               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00331300
331400               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00331400
331500               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00331500
331600               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00331600
331700               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00331700
331800               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00331800
331900               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00331900
332000               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00332000
332100               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00332100
332200               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00332200
332300               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00332300
332400               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00332400
332500               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00332500
332600               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00332600
332700               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00332700
332800               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00332800
332900               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00332900
333000               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00333000
333100               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00333100
333200               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00333200
333300               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00333300
333400               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00333400
333500               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00333500
333600               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00333600
333700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00333700
333800               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00333800
333900               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00333900
334000               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00334000
334100               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00334100
334200               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00334200
334300               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00334300
334400               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00334400
334500               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00334500
334600               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00334600
334700               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00334700
334800               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00334800
334900               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00334900
335000               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00335000
335100               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00335100
335200               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00335200
335300               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00335300
335400               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00335400
335500               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00335500
335600               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00335600
335700               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00335700
335800               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00335800
335900               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00335900
336000               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00336000
336100               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00336100
336200               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00336200
336300               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00336300
336400               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00336400
336500               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00336500
336600               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00336600
336700               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00336700
336800               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00336800
336900               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00336900
337000               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00337000
337100               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00337100
337200               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00337200
337300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00337300
337400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00337400
337500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00337500
337600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00337600
337700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00337700
337800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00337800
337900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00337900
338000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00338000
338100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00338100
338200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00338200
338300     END-EXEC                                                     00338300
338400     .                                                            00338400
338500                                                                  00338500
338600                                                                  00338600
338700 1311-FETCH-RFCXRL11.                                             00338700
338800     EXEC SQL                                                     00338800
338900         FETCH RFCXRL11                                           00338900
339000         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00339000
339100               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00339100
339200               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00339200
339300               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00339300
339400               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00339400
339500               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00339500
339600               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00339600
339700               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00339700
339800               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00339800
339900               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00339900
340000               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00340000
340100               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00340100
340200               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00340200
340300               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00340300
340400               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00340400
340500               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00340500
340600               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00340600
340700               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00340700
340800               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00340800
340900               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00340900
341000               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00341000
341100               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00341100
341200               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00341200
341300               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00341300
341400               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00341400
341500               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00341500
341600               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00341600
341700               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00341700
341800               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00341800
341900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00341900
342000               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00342000
342100               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00342100
342200               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00342200
342300               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00342300
342400               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00342400
342500               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00342500
342600               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00342600
342700               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00342700
342800               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00342800
342900               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00342900
343000               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00343000
343100               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00343100
343200               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00343200
343300               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00343300
343400               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00343400
343500               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00343500
343600               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00343600
343700               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00343700
343800               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00343800
343900               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00343900
344000               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00344000
344100               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00344100
344200               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00344200
344300               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00344300
344400               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00344400
344500               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00344500
344600               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00344600
344700               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00344700
344800               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00344800
344900               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00344900
345000               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00345000
345100               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00345100
345200               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00345200
345300               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00345300
345400               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00345400
345500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00345500
345600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00345600
345700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00345700
345800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00345800
345900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00345900
346000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00346000
346100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00346100
346200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00346200
346300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00346300
346400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00346400
346500     END-EXEC                                                     00346500
346600     .                                                            00346600
346700                                                                  00346700
346800                                                                  00346800
346900 1312-FETCH-RFCXRL12.                                             00346900
347000     EXEC SQL                                                     00347000
347100         FETCH RFCXRL12                                           00347100
347200         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00347200
347300               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00347300
347400               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00347400
347500               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00347500
347600               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00347600
347700               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00347700
347800               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00347800
347900               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00347900
348000               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00348000
348100               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00348100
348200               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00348200
348300               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00348300
348400               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00348400
348500               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00348500
348600               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00348600
348700               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00348700
348800               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00348800
348900               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00348900
349000               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00349000
349100               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00349100
349200               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00349200
349300               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00349300
349400               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00349400
349500               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00349500
349600               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00349600
349700               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00349700
349800               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00349800
349900               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00349900
350000               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00350000
350100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00350100
350200               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00350200
350300               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00350300
350400               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00350400
350500               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00350500
350600               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00350600
350700               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00350700
350800               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00350800
350900               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00350900
351000               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00351000
351100               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00351100
351200               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00351200
351300               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00351300
351400               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00351400
351500               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00351500
351600               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00351600
351700               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00351700
351800               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00351800
351900               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00351900
352000               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00352000
352100               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00352100
352200               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00352200
352300               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00352300
352400               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00352400
352500               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00352500
352600               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00352600
352700               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00352700
352800               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00352800
352900               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00352900
353000               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00353000
353100               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00353100
353200               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00353200
353300               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00353300
353400               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00353400
353500               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00353500
353600               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00353600
353700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00353700
353800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00353800
353900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00353900
354000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00354000
354100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00354100
354200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00354200
354300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00354300
354400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00354400
354500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00354500
354600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00354600
354700     END-EXEC                                                     00354700
354800     .                                                            00354800
354900                                                                  00354900
355000                                                                  00355000
355100 1313-FETCH-RFCXRL13.                                             00355100
355200     EXEC SQL                                                     00355200
355300         FETCH RFCXRL13                                           00355300
355400         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00355400
355500               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00355500
355600               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00355600
355700               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00355700
355800               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00355800
355900               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00355900
356000               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00356000
356100               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00356100
356200               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00356200
356300               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00356300
356400               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00356400
356500               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00356500
356600               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00356600
356700               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00356700
356800               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00356800
356900               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00356900
357000               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00357000
357100               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00357100
357200               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00357200
357300               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00357300
357400               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00357400
357500               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00357500
357600               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00357600
357700               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00357700
357800               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00357800
357900               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00357900
358000               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00358000
358100               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00358100
358200               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00358200
358300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00358300
358400               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00358400
358500               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00358500
358600               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00358600
358700               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00358700
358800               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00358800
358900               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00358900
359000               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00359000
359100               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00359100
359200               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00359200
359300               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00359300
359400               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00359400
359500               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00359500
359600               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00359600
359700               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00359700
359800               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00359800
359900               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00359900
360000               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00360000
360100               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00360100
360200               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00360200
360300               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00360300
360400               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00360400
360500               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00360500
360600               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00360600
360700               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00360700
360800               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00360800
360900               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00360900
361000               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00361000
361100               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00361100
361200               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00361200
361300               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00361300
361400               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00361400
361500               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00361500
361600               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00361600
361700               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00361700
361800               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00361800
361900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00361900
362000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00362000
362100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00362100
362200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00362200
362300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00362300
362400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00362400
362500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00362500
362600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00362600
362700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00362700
362800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00362800
362900     END-EXEC                                                     00362900
363000     .                                                            00363000
363100                                                                  00363100
363200                                                                  00363200
363300 1314-FETCH-RFCXRL14.                                             00363300
363400     EXEC SQL                                                     00363400
363500         FETCH RFCXRL14                                           00363500
363600         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00363600
363700               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00363700
363800               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00363800
363900               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00363900
364000               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00364000
364100               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00364100
364200               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00364200
364300               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00364300
364400               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00364400
364500               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00364500
364600               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00364600
364700               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00364700
364800               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00364800
364900               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00364900
365000               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00365000
365100               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00365100
365200               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00365200
365300               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00365300
365400               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00365400
365500               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00365500
365600               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00365600
365700               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00365700
365800               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00365800
365900               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00365900
366000               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00366000
366100               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00366100
366200               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00366200
366300               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00366300
366400               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00366400
366500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00366500
366600               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00366600
366700               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00366700
366800               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00366800
366900               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00366900
367000               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00367000
367100               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00367100
367200               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00367200
367300               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00367300
367400               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00367400
367500               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00367500
367600               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00367600
367700               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00367700
367800               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00367800
367900               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00367900
368000               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00368000
368100               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00368100
368200               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00368200
368300               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00368300
368400               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00368400
368500               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00368500
368600               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00368600
368700               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00368700
368800               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00368800
368900               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00368900
369000               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00369000
369100               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00369100
369200               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00369200
369300               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00369300
369400               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00369400
369500               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00369500
369600               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00369600
369700               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00369700
369800               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00369800
369900               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00369900
370000               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00370000
370100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00370100
370200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00370200
370300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00370300
370400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00370400
370500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00370500
370600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00370600
370700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00370700
370800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00370800
370900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00370900
371000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00371000
371100     END-EXEC                                                     00371100
371200     .                                                            00371200
371300                                                                  00371300
371400                                                                  00371400
371500 1315-FETCH-RFCXRL15.                                             00371500
371600     EXEC SQL                                                     00371600
371700         FETCH RFCXRL15                                           00371700
371800         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00371800
371900               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00371900
372000               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00372000
372100               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00372100
372200               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00372200
372300               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00372300
372400               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00372400
372500               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00372500
372600               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00372600
372700               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00372700
372800               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00372800
372900               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00372900
373000               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00373000
373100               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00373100
373200               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00373200
373300               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00373300
373400               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00373400
373500               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00373500
373600               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00373600
373700               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00373700
373800               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00373800
373900               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00373900
374000               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00374000
374100               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00374100
374200               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00374200
374300               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00374300
374400               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00374400
374500               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00374500
374600               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00374600
374700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00374700
374800               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00374800
374900               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00374900
375000               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00375000
375100               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00375100
375200               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00375200
375300               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00375300
375400               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00375400
375500               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00375500
375600               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00375600
375700               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00375700
375800               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00375800
375900               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00375900
376000               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00376000
376100               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00376100
376200               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00376200
376300               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00376300
376400               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00376400
376500               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00376500
376600               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00376600
376700               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00376700
376800               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00376800
376900               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00376900
377000               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00377000
377100               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00377100
377200               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00377200
377300               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00377300
377400               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00377400
377500               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00377500
377600               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00377600
377700               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00377700
377800               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00377800
377900               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00377900
378000               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00378000
378100               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00378100
378200               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00378200
378300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00378300
378400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00378400
378500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00378500
378600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00378600
378700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00378700
378800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00378800
378900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00378900
379000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00379000
379100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00379100
379200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00379200
379300     END-EXEC                                                     00379300
379400     .                                                            00379400
379500                                                                  00379500
379600                                                                  00379600
379700 1316-FETCH-RFCXRL16.                                             00379700
379800     EXEC SQL                                                     00379800
379900         FETCH RFCXRL16                                           00379900
380000         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00380000
380100               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00380100
380200               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00380200
380300               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00380300
380400               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00380400
380500               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00380500
380600               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00380600
380700               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00380700
380800               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00380800
380900               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00380900
381000               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00381000
381100               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00381100
381200               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00381200
381300               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00381300
381400               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00381400
381500               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00381500
381600               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00381600
381700               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00381700
381800               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00381800
381900               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00381900
382000               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00382000
382100               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00382100
382200               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00382200
382300               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00382300
382400               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00382400
382500               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00382500
382600               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00382600
382700               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00382700
382800               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00382800
382900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00382900
383000               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00383000
383100               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00383100
383200               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00383200
383300               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00383300
383400               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00383400
383500               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00383500
383600               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00383600
383700               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00383700
383800               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00383800
383900               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00383900
384000               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00384000
384100               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00384100
384200               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00384200
384300               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00384300
384400               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00384400
384500               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00384500
384600               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00384600
384700               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00384700
384800               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00384800
384900               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00384900
385000               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00385000
385100               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00385100
385200               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00385200
385300               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00385300
385400               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00385400
385500               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00385500
385600               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00385600
385700               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00385700
385800               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00385800
385900               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00385900
386000               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00386000
386100               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00386100
386200               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00386200
386300               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00386300
386400               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00386400
386500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00386500
386600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00386600
386700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00386700
386800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00386800
386900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00386900
387000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00387000
387100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00387100
387200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00387200
387300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00387300
387400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00387400
387500     END-EXEC                                                     00387500
387600     .                                                            00387600
387700                                                                  00387700
387800                                                                  00387800
387900 1317-FETCH-RFCXRL17.                                             00387900
388000     EXEC SQL                                                     00388000
388100         FETCH RFCXRL17                                           00388100
388200         INTO  :DCLFC-XXXAIL-STORES.FC-STORE-NO,                  00388200
388300               :DCLFC-XXXAIL-STORES.FC-RL-STORE-NM,               00388300
388400               :DCLFC-XXXAIL-STORES.FC-RL-STORE-CD,               00388400
388500               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DIR-NM,           00388500
388600               :DCLFC-XXXAIL-STORES.FC-RL-STORE-LOC-NM,           00388600
388700               :DCLFC-XXXAIL-STORES.FC-RL-OPENING-DT,             00388700
388800               :DCLFC-XXXAIL-STORES.FC-RL-CLOSING-DT,             00388800
388900               :DCLFC-XXXAIL-STORES.FC-RL-REMODEL-DT,             00388900
389000               :DCLFC-XXXAIL-STORES.FC-RL-DELETED-DT,             00389000
389100               :DCLFC-XXXAIL-STORES.FC-RL-DISTRICT-NO,            00389100
389200               :DCLFC-XXXAIL-STORES.FC-RL-MARKET-AR-NO,           00389200
389300               :DCLFC-XXXAIL-STORES.FC-RL-PAYROL-AR-NO,           00389300
389400               :DCLFC-XXXAIL-STORES.FC-RL-PAY-GROUP-NO,           00389400
389500               :DCLFC-XXXAIL-STORES.FC-RL-COMPANY-NO,             00389500
389600               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-CD,            00389600
389700               :DCLFC-XXXAIL-STORES.FC-RL-GEO-ZONE-NO,            00389700
389800               :DCLFC-XXXAIL-STORES.FC-RL-SCAN-MAIN-CD,           00389800
389900               :DCLFC-XXXAIL-STORES.FC-RL-FRONT-END-CD,           00389900
390000               :DCLFC-XXXAIL-STORES.FC-RL-PRICE-BUL-CD,           00390000
390100               :DCLFC-XXXAIL-STORES.FC-RL-UPC-ON-PB-CD,           00390100
390200               :DCLFC-XXXAIL-STORES.FC-RL-COMPETITR-CD,           00390200
390300               :DCLFC-XXXAIL-STORES.FC-RL-ASSOC-STR-NO,           00390300
390400               :DCLFC-XXXAIL-STORES.FC-RL-RPRT-SEQ-NO,            00390400
390500               :DCLFC-XXXAIL-STORES.FC-RL-SORT-SOS-NO,            00390500
390600               :DCLFC-XXXAIL-STORES.FC-RL-VID-PRZN-NO,            00390600
390700               :DCLFC-XXXAIL-STORES.FC-RL-CITY-ID-CD,             00390700
390800               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-ABB,             00390800
390900               :DCLFC-XXXAIL-STORES.FC-RL-ADZONE-DES,             00390900
391000               :DCLFC-XXXAIL-STORES.FC-RL-UNLOAD-SW,              00391000
391100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-CD,            00391100
391200               :DCLFC-XXXAIL-STORES.FC-RL-STATUS-CD,              00391200
391300               :DCLFC-XXXAIL-STORES.FC-RL-NEW-STORE-CD,           00391300
391400               :DCLFC-XXXAIL-STORES.FC-RL-TYPE-CD,                00391400
391500               :DCLFC-XXXAIL-STORES.FC-RL-GROUP-CD,               00391500
391600               :DCLFC-XXXAIL-STORES.FC-RL-SELECTCIR-CD,           00391600
391700               :DCLFC-XXXAIL-STORES.FC-RL-AREA-CODE-NO,           00391700
391800               :DCLFC-XXXAIL-STORES.FC-RL-TELEPHONE-NO,           00391800
391900               :DCLFC-XXXAIL-STORES.FC-RL-STORE-ABB,              00391900
392000               :DCLFC-XXXAIL-STORES.FC-RL-BCKRM-FT-QTY,           00392000
392100               :DCLFC-XXXAIL-STORES.FC-RL-LFT-FOOD-QTY,           00392100
392200               :DCLFC-XXXAIL-STORES.FC-RL-LFT-NONF-QTY,           00392200
392300               :DCLFC-XXXAIL-STORES.FC-RL-SETOFF-CD,              00392300
392400               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ZONE-NO,           00392400
392500               :DCLFC-XXXAIL-STORES.FC-RL-CL12-ADZN-NO,           00392500
392600               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ZONE-NO,           00392600
392700               :DCLFC-XXXAIL-STORES.FC-RL-CL13-ADZN-NO,           00392700
392800               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ZONE-NO,           00392800
392900               :DCLFC-XXXAIL-STORES.FC-RL-CL14-ADZN-NO,           00392900
393000               :DCLFC-XXXAIL-STORES.FC-RL-CL36-ADZN-NO,           00393000
393100               :DCLFC-XXXAIL-STORES.FC-RL-CL37-ADZN-NO,           00393100
393200               :DCLFC-XXXAIL-STORES.FC-RL-STORE-DEA-NO,           00393200
393300               :DCLFC-XXXAIL-STORES.FC-RL-RETL-ZONE-NO,           00393300
393400               :DCLFC-XXXAIL-STORES.FC-RL-STOR2-LOC-NM,           00393400
393500               :DCLFC-XXXAIL-STORES.FC-CITY-ADR,                  00393500
393600               :DCLFC-XXXAIL-STORES.FC-STATE-ADR,                 00393600
393700               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE5-ADR,             00393700
393800               :DCLFC-XXXAIL-STORES.FC-ZIP-CODE4-ADR,             00393800
393900               :DCLFC-XXXAIL-STORES.FC-RL-SOS-TYPE-CD,            00393900
394000               :DCLFC-XXXAIL-STORES.FC-RL-NOPROCESS-CD,           00394000
394100               :DCLFC-XXXAIL-STORES.FC-RL-SOSHDRTYP-CD,           00394100
394200               :DCLFC-XXXAIL-STORES.FC-RL-CAT-CLASS-TB,           00394200
394300               :DCLFC-XXXAIL-STORES.FC-RL-LATITUDE-K,             00394300
394400               :DCLFC-XXXAIL-STORES.FC-RL-LONGITUDE-K,            00394400
394500               :DCLFC-XXXAIL-STORES.FN-DIVISION-CD,               00394500
394600               :DCLFC-XXXAIL-STORES.FN-LINE-OF-BUS-CD,            00394600
394700               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-01-NBR,        00394700
394800               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-02-NBR,        00394800
394900               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-03-NBR,        00394900
395000               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-04-NBR,        00395000
395100               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-05-NBR,        00395100
395200               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-06-NBR,        00395200
395300               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-07-NBR,        00395300
395400               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-08-NBR,        00395400
395500               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-09-NBR,        00395500
395600               :DCLFC-XXXAIL-STORES.FN-ROLLUP-REPT-10-NBR         00395600
395700     END-EXEC                                                     00395700
395800     .                                                            00395800
395900                                                                  00395900
396000                                                                  00396000
396100 1400-EXIT-PUT-MODIFY-ROW.                                        00396100
396200     PERFORM 1800-EDIT-NULL-INDICATORS                            00396200
396300     PERFORM 5000-CALL-NNNU0120-CUD-ROUTINE                       00396300
396400                                                                  00396400
396500     MOVE 1 TO WS-CHECKPOINT-INC                                  00396500
396600     .                                                            00396600
396700                                                                  00396700
396800                                                                  00396800
396900 1500-EXIT-PUT-INSERT-ROW.                                        00396900
397000     PERFORM 1800-EDIT-NULL-INDICATORS                            00397000
397100     PERFORM 5000-CALL-NNNU0120-CUD-ROUTINE                       00397100
397200                                                                  00397200
397300     MOVE 1 TO WS-CHECKPOINT-INC                                  00397300
397400     .                                                            00397400
397500                                                                  00397500
397600                                                                  00397600
397700 1600-EXIT-PUT-PURGE-ROW.                                         00397700
397800     PERFORM 4000-CALL-MMMS0304-RI-DEL-CHK                        00397800
397900     IF SUCCESS                                                   00397900
398000        PERFORM 5000-CALL-NNNU0120-CUD-ROUTINE                    00398000
398100                                                                  00398100
398200        MOVE 1 TO WS-CHECKPOINT-INC                               00398200
398300     END-IF                                                       00398300
398400     .                                                            00398400
398500                                                                  00398500
398600                                                                  00398600
398700* ================================================================00398700
398800* Initialize NULL variables if the column is set to NULL.         00398800
398900* ================================================================00398900
399000 1700-CHECK-NULL-COLUMNS.                                         00399000
399100     EXIT                                                         00399100
399200     .                                                            00399200
399300                                                                  00399300
399400                                                                  00399400
399500* ================================================================00399500
399600* Make sure the null indicators are valid.                        00399600
399700* ================================================================00399700
399800 1800-EDIT-NULL-INDICATORS.                                       00399800
399900     EXIT                                                         00399900
400000     .                                                            00400000
400100                                                                  00400100
400200                                                                  00400200
400300 4000-CALL-MMMS0304-RI-DEL-CHK.                                   00400300
400400     INITIALIZE MMMC0304                                          00400400
400500                                                                  00400500
400600     MOVE FC-STORE-NO                OF DCLFC-XXXAIL-STORES       00400600
400700                                     TO MMMC0304-FC-STORE-NO      00400700
400800     SET MMMC0304-DELETE-CHECK       TO TRUE                      00400800
400900     SET MMMC0304-FC-XXXAIL-STORES   TO TRUE                      00400900
401000     SET MMMC0304-ORACLE             TO TRUE                      00401000
401100     CALL MMMS0304-RI-DEL-CHK        USING                        00401100
401200          W00N001A                                                00401200
401300          MMMC0304                                                00401300
401400     .                                                            00401400
401500                                                                  00401500
401600                                                                  00401600
401700* =========================================================       00401700
401800* Call Oracle table update program                                00401800
401900* =========================================================       00401900
402000 5000-CALL-NNNU0120-CUD-ROUTINE.                                  00402000
402100     CALL NNNU0120-ORACLE-UPDATE USING                            00402100
402200          W00N001A                                                00402200
402300          SQLCA                                                   00402300
402400          YYYN005A                                                00402400
402500          NNNN0000-PARMS                                          00402500
402600          DDDTRL01                                                00402600
402700     .                                                            00402700
402800                                                                  00402800
402900                                                                  00402900
403000* ================================================================00403000
403100* Special sql or functions to be performed by this subroutine.    00403100
403200* ================================================================00403200
403300 10000-DO-SPECIAL-IO-FUNCS.                                       00403300
403400     EXIT                                                         00403400
403500     .                                                            00403500
