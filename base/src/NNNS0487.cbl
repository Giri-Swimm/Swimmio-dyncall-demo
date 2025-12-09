000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    NNNS0487.                                         00000200
000300 AUTHOR.        NAME                                              00000300
000400 DATE-WRITTEN.  Circa, 1600.                                      00000400
000500 DATE-COMPILED.                                                   00000500
008300 ENVIRONMENT DIVISION.                                            00008300
008400 CONFIGURATION SECTION.                                           00008400
008500 DATA DIVISION.                                                   00008500
008600 WORKING-STORAGE SECTION.                                         00008600
008700                                                                  00008700
008800* ========================< MISC STUFF >==========================00008800
008900* Misc working storage variables go here.                         00008900
009000* ================================================================00009000
009100 01 WS-SQLCODE                         PIC ----9.                 00009100
009200 01 WS-STR-LEN                         PIC S9(2) COMP VALUE 0.    00009200
009300 01 WS-CHECKPOINT-INC                  PIC S9(4) COMP VALUE 0.    00009300
009400                                                                  00009400
009500 01 WS-OLD-KEY                         PIC 9(9) VALUE 0.          00009500
009600 01 REDEFINES WS-OLD-KEY.                                         00009600
009700     05 FILLER                         PIC X(1).                  00009700
009800     05 WS-OLD-KEY-A8                  PIC X(8).                  00009800
009900 01 REDEFINES WS-OLD-KEY.                                         00009900
010000     05 FILLER                         PIC X(4).                  00010000
010100     05 WS-OLD-KEY-A5                  PIC X(5).                  00010100
010200 01 REDEFINES WS-OLD-KEY.                                         00010200
010300     05 FILLER                         PIC 9(1).                  00010300
010400     05 WS-VENDOR-FACILITY             PIC 9(2).                  00010400
010500     05 WS-VENDOR-NUMBER               PIC 9(6).                  00010500
010600                                                                  00010600
010700 01 WS-DUMMY                           PIC X(1) VALUE SPACES.     00010700
010800 01 WS-NULL-TM                         PIC X(8) VALUE '00:00:01'. 00010800
010900                                                                  00010900
011000 01 WS-NULL-INDS.                                                 00011000
011100     05 WS-AP-NBR-IND                  PIC S9(4) COMP VALUE 0.    00011100
011200     05 WS-AP-TYP-CD-IND               PIC S9(4) COMP VALUE 0.    00011200
011300 01 WS-CSR-SW                          PIC X(1) VALUE SPACES.     00011300
011400     88 NOT-END-OF-DEL-CSR             VALUE ' '.                 00011400
011500     88 END-OF-DEL-CSR                 VALUE 'Y'.                 00011500
011600                                                                  00011600
011700 01 WS-CSR-SW1                         PIC X(1) VALUE SPACES.     00011700
011800     88 NOT-END-OF-DEL-CSR1            VALUE ' '.                 00011800
011900     88 END-OF-DEL-CSR1                VALUE 'Y'.                 00011900
012000 01 WS-AP-NBR-CNT                      PIC S9(4) COMP VALUE 0.    00012000
012100 01 WS-FAC-ID                          PIC S9(9) COMP VALUE 0.    00012100
012200 01 WS-ORG-ID                          PIC S9(9) COMP VALUE 0.    00012200
012300 01 WS-AP-NUM                          PIC S9(9) COMP VALUE 0.    00012300
012400 01 WS-AP-TYPE                         PIC X(2)  VALUE SPACES.    00012400
012500 01 WS-MMMS0265-SYNC                   PIC X(8)  VALUE 'MMMS0265'.00012500
012600 01 MMMS0474-DO-AVP-MAIN               PIC X(8)  VALUE 'MMMS0474'.00012600
012700 01 NNNS0457-AP-LOC-DAO                PIC X(8)  VALUE 'NNNS0457'.00012700
012800 01 NNNS0486-DSD-LOC-DAO               PIC X(8)  VALUE 'NNNS0486'.00012800
012900 01 NNNS0488-RETAIL-LOC-DAO            PIC X(8)  VALUE 'NNNS0488'.00012900
013000 01 NNNS0575-STR-LOC-DEPT-DAO          PIC X(8)  VALUE 'NNNS0575'.00013000
013100 01 NNNS0483-FAX-DAO                   PIC X(8)  VALUE 'NNNS0483'.00013100
013200 01 NNNS0473-RETL-DAO                  PIC X(8)  VALUE 'NNNS0473'.00013200
013300 01 NNNS2294-MECY-DAO                  PIC X(8)  VALUE 'NNNS2294'.00013300
013400 01 MMMS0711-DSV-CHECK                 PIC X(8)  VALUE 'MMMS0711'.00013400
013500 01 MMMS0304-RI-DEL-CHK                PIC X(8)  VALUE 'MMMS0304'.00013500
013600                                                                  00013600
013700 01 WS-STAGE-EVENT-SW                  PIC X(1) VALUE SPACES.     00013700
013800   88 STAGE-EVENT                               VALUE 'Y'.        00013800
013900   88 DONT-STAGE-EVENT                          VALUE ' '.        00013900
014000                                                                  00014000
014100 01 WS-MAIL-TO-LOC-NM                  PIC X(30) VALUE SPACES.    00014100
014200 01 WS-MAIL-TO-ADR-1                   PIC X(30) VALUE SPACES.    00014200
014300 01 WS-MAIL-TO-ADR-2                   PIC X(30) VALUE SPACES.    00014300
014400 01 WS-MAIL-TO-CITY                    PIC X(30) VALUE SPACES.    00014400
014500 01 WS-MAIL-TO-STATE-CD                PIC X(02) VALUE SPACES.    00014500
014600 01 WS-SEC-CONTACT-NM                  PIC X(30) VALUE SPACES.    00014600
014700 01 WS-MAIL-TO-ZIP5-CD                 PIC S9(5)V USAGE COMP-3.   00014700
014800 01 WS-MAIL-TO-ZIP4-CD                 PIC S9(4)V USAGE COMP-3.   00014800
014900 01 WS-MAIL-TO-PHONE-NBR               PIC S9(7)V USAGE COMP-3.   00014900
015000 01 WS-MMMS0291-PGM                    PIC X(8) VALUE 'MMMS0291'. 00015000
015100 01 WS-ORD-PROCNG-CTOF-TM              PIC X(26) VALUE SPACES.    00015100
015200 01 WS-FILLER1-TS                      PIC X(26) VALUE SPACES.    00015200
015300 01 WS-FILLER2-TS                      PIC X(26) VALUE SPACES.    00015300
015400* ========================< COPY BOOKS >==========================00015400
015500* Place all copy books in this section.                           00015500
015600* ================================================================00015600
015700 COPY NNNN000U.                                                   00015700
015800 COPY MMMC0291.                                                   00015800
015900 COPY PPPTAP01.                                                   00015900
016000 COPY PPPTLD01.                                                   00016000
016100 COPY PPPTLR01.                                                   00016100
016200 COPY PPPTFX01.                                                   00016200
016300 COPY PPPTCZ01.                                                   00016300
016400 COPY PPPTDT01.                                                   00016400
016500 COPY PPPTCY01.                                                   00016500
016600 COPY DFHEIBLK.                                                   00016600
016700 COPY HHHTLO01.                                                   00016700
016800 COPY MMMC0474.                                                   00016800
016900 COPY YYYN000A.                                                   00016900
017000 COPY YYYN110A.                                                   00017000
017100 COPY YYYC0107.                                                   00017100
017200 COPY YYYC0127.                                                   00017200
017300 COPY ZZZC0094.                                                   00017300
017400 COPY ZZZC0122.                                                   00017400
017500 COPY ZZZC0123.                                                   00017500
017600 COPY ZZZC0124.                                                   00017600
017700 COPY ZZZC0020.                                                   00017700
017800 COPY ZZZC0032.                                                   00017800
017900 COPY ZZZC0044.                                                   00017900
018000 COPY ZZZC0197.                                                   00018000
018100                                                                  00018100
018200 COPY WWWC0100.                                                   00018200
018300 COPY ZZZC0550.                                                   00018300
018400                                                                  00018400
018500 COPY MMMC0135.                                                   00018500
018600 COPY MMMC0157.                                                   00018600
018700 COPY MMMC0153.                                                   00018700
018800 COPY MMMC0265.                                                   00018800
018900 COPY MMMC0711.                                                   00018900
019000 COPY MMMK001B.                                                   00019000
019100 COPY MMMC0304.                                                   00019100
019200                                                                  00019200
019300* ========================< DCL GENS >============================00019300
019400* Place all DCL gens in this section.                             00019400
019500* ================================================================00019500
019600     EXEC SQL                                                     00019600
019700       INCLUDE DDDTLO01                                           00019700
019800     END-EXEC.                                                    00019800
019900     EXEC SQL                                                     00019900
020000       INCLUDE DDDTCZ01                                           00020000
020100     END-EXEC.                                                    00020100
020200     EXEC SQL                                                     00020200
020300       INCLUDE DDDTLR01                                           00020300
020400     END-EXEC.                                                    00020400
020500     EXEC SQL                                                     00020500
020600       INCLUDE DDDTDT01                                           00020600
020700     END-EXEC.                                                    00020700
020800     EXEC SQL                                                     00020800
020900       INCLUDE DDDTFX01                                           00020900
021000     END-EXEC.                                                    00021000
021100     EXEC SQL                                                     00021100
021200       INCLUDE DDDTAP01                                           00021200
021300     END-EXEC.                                                    00021300
021400     EXEC SQL                                                     00021400
021500       INCLUDE DDDTLW01                                           00021500
021600     END-EXEC.                                                    00021600
021700                                                                  00021700
021800     EXEC SQL                                                     00021800
021900       INCLUDE DDDTLB01                                           00021900
022000     END-EXEC.                                                    00022000
022100                                                                  00022100
022200     EXEC SQL                                                     00022200
022300       INCLUDE DDDTLS01                                           00022300
022400     END-EXEC.                                                    00022400
022500                                                                  00022500
022600* ========================< CURSORS >=============================00022600
022700* Place all cursors in this section.                              00022700
022800* ================================================================00022800
022900                                                                  00022900
023000* --------------------------------------------------              00023000
023100* DDDXLO01 cursor declaration.                                    00023100
023200* --------------------------------------------------              00023200
023300     EXEC SQL                                                     00023300
023400         DECLARE DDDXLO01 CURSOR WITH HOLD FOR SELECT             00023400
023500             LOC_TYP_CD,                                          00023500
023600             LOC_NBR,                                             00023600
023700             LOC_NM,                                              00023700
023800             LOC_ABB,                                             00023800
023900             LGL_LOC_NAM,                                         00023900
024000             PRIM_CONTACT_NM,                                     00024000
024100             PRIM_ADR_1,                                          00024100
024200             PRIM_ADR_2,                                          00024200
024300             PRIM_ADR_3,                                          00024300
024400             PRIM_ADR_4,                                          00024400
024500             PRIM_CITY,                                           00024500
024600             PRIM_CITY_ID,                                        00024600
024700             PRIM_STATE_CD,                                       00024700
024800             PRIM_ZIP5_CD,                                        00024800
024900             PRIM_ZIP4_CD,                                        00024900
025000             PRIM_PHN_CNTRY_CD,                                   00025000
025100             PRIM_AREA_CD,                                        00025100
025200             PRIM_PHONE_NBR,                                      00025200
025300             PRIM_CNTRY_NM,                                       00025300
025400             PRIM_CNTRY_ABB,                                      00025400
025500             SEC_LOC_NM,                                          00025500
025600             SEC_CONTACT_NM,                                      00025600
025700             SEC_ADR_1,                                           00025700
025800             SEC_ADR_2,                                           00025800
025900             SEC_ADR_3,                                           00025900
026000             SEC_ADR_4,                                           00026000
026100             SEC_CITY,                                            00026100
026200             SEC_STATE_CD,                                        00026200
026300             SEC_ZIP5_CD,                                         00026300
026400             SEC_ZIP4_CD,                                         00026400
026500             SEC_PHN_CNTRY_CD,                                    00026500
026600             SEC_AREA_CD,                                         00026600
026700             SEC_PHONE_NBR,                                       00026700
026800             SEC_CNTRY_NM,                                        00026800
026900             SEC_CNTRY_ABB,                                       00026900
027000             MAIL_TO_LOC_NM,                                      00027000
027100             MAIL_TO_CNTCT_NM,                                    00027100
027200             MAIL_TO_ADR_1,                                       00027200
027300             MAIL_TO_ADR_2,                                       00027300
027400             MAIL_TO_ADR_3,                                       00027400
027500             MAIL_TO_ADR_4,                                       00027500
027600             MAIL_TO_CITY,                                        00027600
027700             MAIL_TO_STATE_CD,                                    00027700
027800             MAIL_TO_ZIP5_CD,                                     00027800
027900             MAIL_TO_ZIP4_CD,                                     00027900
028000             MAIL_PHN_CNTRY_CD,                                   00028000
028100             MAIL_TO_AREA_CD,                                     00028100
028200             MAIL_TO_PHONE_NBR,                                   00028200
028300             MAIL_TO_CNTRY_NM,                                    00028300
028400             MAIL_TO_CNTRY_AB,                                    00028400
028500             CURR_FAX_ID,                                         00028500
028600             ADDED_DT,                                            00028600
028700             DELETE_DT,                                           00028700
028800             OPENED_DT,                                           00028800
028900             CLOSED_DT,                                           00028900
029000             INACTIVE_SW,                                         00029000
029100             INACTIVE_DT,                                         00029100
029200             AP_NBR,                                              00029200
029300             AP_TYP_CD,                                           00029300
029400             LST_UPDT_TS,                                         00029400
029500             LST_UPDT_USR_ID,                                     00029500
029600             PRIM_EMAIL_ID  ,                                     00029600
029700             SECY_EMAIL_ID  ,                                     00029700
029800             MAIL_TO_EMAIL_ID,                                    00029800
029900             FAC_ID,                                              00029900
030000             ORG_ID,                                              00030000
030100             B2B_PRIM_RTNG_ID,                                    00030100
030200             PRIM_CNTY_TXT,                                       00030200
030300             SECY_CNTY_TXT,                                       00030300
030400             MAIL_TO_CNTY_TXT,                                    00030400
030500             DIR_SHP_LOC_SW,                                      00030500
030600             LOC_ORD_PROCNG_DD,                                   00030600
030700             ORD_PROCNG_CTOF_TM,                                  00030700
030800             SCH_SHP_DD_TXT,                                      00030800
030900             ORD_LEAD_TM_DD,                                      00030900
031000             ORD_BUFFER_TM_DD                                     00031000
031100         FROM     XXXATION                                        00031100
031200         WHERE   (LOC_TYP_CD = :DCLXXXATION.LOC-TYP-CD)           00031200
031300         AND     (LOC_NBR >= :DCLXXXATION.LOC-NBR)                00031300
031400         ORDER BY                                                 00031400
031500             LOC_NBR                                              00031500
031600     END-EXEC.                                                    00031600
031700* --------------------------------------------------              00031700
031800* DDDXLO02 cursor declaration.                                    00031800
031900* --------------------------------------------------              00031900
032000     EXEC SQL                                                     00032000
032100         DECLARE DDDXLO02 CURSOR WITH HOLD FOR SELECT             00032100
032200             LOC_TYP_CD,                                          00032200
032300             LOC_NBR,                                             00032300
032400             LOC_NM,                                              00032400
032500             LOC_ABB,                                             00032500
032600             LGL_LOC_NAM,                                         00032600
032700             PRIM_CONTACT_NM,                                     00032700
032800             PRIM_ADR_1,                                          00032800
032900             PRIM_ADR_2,                                          00032900
033000             PRIM_ADR_3,                                          00033000
033100             PRIM_ADR_4,                                          00033100
033200             PRIM_CITY,                                           00033200
033300             PRIM_CITY_ID,                                        00033300
033400             PRIM_STATE_CD,                                       00033400
033500             PRIM_ZIP5_CD,                                        00033500
033600             PRIM_ZIP4_CD,                                        00033600
033700             PRIM_PHN_CNTRY_CD,                                   00033700
033800             PRIM_AREA_CD,                                        00033800
033900             PRIM_PHONE_NBR,                                      00033900
034000             PRIM_CNTRY_NM,                                       00034000
034100             PRIM_CNTRY_ABB,                                      00034100
034200             SEC_LOC_NM,                                          00034200
034300             SEC_CONTACT_NM,                                      00034300
034400             SEC_ADR_1,                                           00034400
034500             SEC_ADR_2,                                           00034500
034600             SEC_ADR_3,                                           00034600
034700             SEC_ADR_4,                                           00034700
034800             SEC_CITY,                                            00034800
034900             SEC_STATE_CD,                                        00034900
035000             SEC_ZIP5_CD,                                         00035000
035100             SEC_ZIP4_CD,                                         00035100
035200             SEC_PHN_CNTRY_CD,                                    00035200
035300             SEC_AREA_CD,                                         00035300
035400             SEC_PHONE_NBR,                                       00035400
035500             SEC_CNTRY_NM,                                        00035500
035600             SEC_CNTRY_ABB,                                       00035600
035700             MAIL_TO_LOC_NM,                                      00035700
035800             MAIL_TO_CNTCT_NM,                                    00035800
035900             MAIL_TO_ADR_1,                                       00035900
036000             MAIL_TO_ADR_2,                                       00036000
036100             MAIL_TO_ADR_3,                                       00036100
036200             MAIL_TO_ADR_4,                                       00036200
036300             MAIL_TO_CITY,                                        00036300
036400             MAIL_TO_STATE_CD,                                    00036400
036500             MAIL_TO_ZIP5_CD,                                     00036500
036600             MAIL_TO_ZIP4_CD,                                     00036600
036700             MAIL_PHN_CNTRY_CD,                                   00036700
036800             MAIL_TO_AREA_CD,                                     00036800
036900             MAIL_TO_PHONE_NBR,                                   00036900
037000             MAIL_TO_CNTRY_NM,                                    00037000
037100             MAIL_TO_CNTRY_AB,                                    00037100
037200             CURR_FAX_ID,                                         00037200
037300             ADDED_DT,                                            00037300
037400             DELETE_DT,                                           00037400
037500             OPENED_DT,                                           00037500
037600             CLOSED_DT,                                           00037600
037700             INACTIVE_SW,                                         00037700
037800             INACTIVE_DT,                                         00037800
037900             AP_NBR,                                              00037900
038000             AP_TYP_CD,                                           00038000
038100             LST_UPDT_TS,                                         00038100
038200             LST_UPDT_USR_ID,                                     00038200
038300             PRIM_EMAIL_ID  ,                                     00038300
038400             SECY_EMAIL_ID  ,                                     00038400
038500             MAIL_TO_EMAIL_ID,                                    00038500
038600             FAC_ID,                                              00038600
038700             ORG_ID,                                              00038700
038800             B2B_PRIM_RTNG_ID,                                    00038800
038900             PRIM_CNTY_TXT,                                       00038900
039000             SECY_CNTY_TXT,                                       00039000
039100             MAIL_TO_CNTY_TXT,                                    00039100
039200             DIR_SHP_LOC_SW,                                      00039200
039300             LOC_ORD_PROCNG_DD,                                   00039300
039400             ORD_PROCNG_CTOF_TM,                                  00039400
039500             SCH_SHP_DD_TXT,                                      00039500
039600             ORD_LEAD_TM_DD,                                      00039600
039700             ORD_BUFFER_TM_DD                                     00039700
039800         FROM     XXXATION                                        00039800
039900         WHERE   (LOC_NBR >= :DCLXXXATION.LOC-NBR)                00039900
040000         AND NOT                                                  00040000
040100                 (LOC_NBR  = :DCLXXXATION.LOC-NBR AND             00040100
040200                  LOC_TYP_CD  < :DCLXXXATION.LOC-TYP-CD)          00040200
040300         ORDER BY                                                 00040300
040400             LOC_NBR,                                             00040400
040500             LOC_TYP_CD                                           00040500
040600     END-EXEC.                                                    00040600
040700* --------------------------------------------------              00040700
040800* DDDXLO03 cursor declaration.                                    00040800
040900* --------------------------------------------------              00040900
041000     EXEC SQL                                                     00041000
041100         DECLARE DDDXLO03 CURSOR WITH HOLD FOR SELECT             00041100
041200             LOC_TYP_CD,                                          00041200
041300             LOC_NBR,                                             00041300
041400             LOC_NM,                                              00041400
041500             LOC_ABB,                                             00041500
041600             LGL_LOC_NAM,                                         00041600
041700             PRIM_CONTACT_NM,                                     00041700
041800             PRIM_ADR_1,                                          00041800
041900             PRIM_ADR_2,                                          00041900
042000             PRIM_ADR_3,                                          00042000
042100             PRIM_ADR_4,                                          00042100
042200             PRIM_CITY,                                           00042200
042300             PRIM_CITY_ID,                                        00042300
042400             PRIM_STATE_CD,                                       00042400
042500             PRIM_ZIP5_CD,                                        00042500
042600             PRIM_ZIP4_CD,                                        00042600
042700             PRIM_PHN_CNTRY_CD,                                   00042700
042800             PRIM_AREA_CD,                                        00042800
042900             PRIM_PHONE_NBR,                                      00042900
043000             PRIM_CNTRY_NM,                                       00043000
043100             PRIM_CNTRY_ABB,                                      00043100
043200             SEC_LOC_NM,                                          00043200
043300             SEC_CONTACT_NM,                                      00043300
043400             SEC_ADR_1,                                           00043400
043500             SEC_ADR_2,                                           00043500
043600             SEC_ADR_3,                                           00043600
043700             SEC_ADR_4,                                           00043700
043800             SEC_CITY,                                            00043800
043900             SEC_STATE_CD,                                        00043900
044000             SEC_ZIP5_CD,                                         00044000
044100             SEC_ZIP4_CD,                                         00044100
044200             SEC_PHN_CNTRY_CD,                                    00044200
044300             SEC_AREA_CD,                                         00044300
044400             SEC_PHONE_NBR,                                       00044400
044500             SEC_CNTRY_NM,                                        00044500
044600             SEC_CNTRY_ABB,                                       00044600
044700             MAIL_TO_LOC_NM,                                      00044700
044800             MAIL_TO_CNTCT_NM,                                    00044800
044900             MAIL_TO_ADR_1,                                       00044900
045000             MAIL_TO_ADR_2,                                       00045000
045100             MAIL_TO_ADR_3,                                       00045100
045200             MAIL_TO_ADR_4,                                       00045200
045300             MAIL_TO_CITY,                                        00045300
045400             MAIL_TO_STATE_CD,                                    00045400
045500             MAIL_TO_ZIP5_CD,                                     00045500
045600             MAIL_TO_ZIP4_CD,                                     00045600
045700             MAIL_PHN_CNTRY_CD,                                   00045700
045800             MAIL_TO_AREA_CD,                                     00045800
045900             MAIL_TO_PHONE_NBR,                                   00045900
046000             MAIL_TO_CNTRY_NM,                                    00046000
046100             MAIL_TO_CNTRY_AB,                                    00046100
046200             CURR_FAX_ID,                                         00046200
046300             ADDED_DT,                                            00046300
046400             DELETE_DT,                                           00046400
046500             OPENED_DT,                                           00046500
046600             CLOSED_DT,                                           00046600
046700             INACTIVE_SW,                                         00046700
046800             INACTIVE_DT,                                         00046800
046900             AP_NBR,                                              00046900
047000             AP_TYP_CD,                                           00047000
047100             LST_UPDT_TS,                                         00047100
047200             LST_UPDT_USR_ID,                                     00047200
047300             PRIM_EMAIL_ID  ,                                     00047300
047400             SECY_EMAIL_ID  ,                                     00047400
047500             MAIL_TO_EMAIL_ID,                                    00047500
047600             FAC_ID,                                              00047600
047700             ORG_ID,                                              00047700
047800             B2B_PRIM_RTNG_ID,                                    00047800
047900             PRIM_CNTY_TXT,                                       00047900
048000             SECY_CNTY_TXT,                                       00048000
048100             MAIL_TO_CNTY_TXT,                                    00048100
048200             DIR_SHP_LOC_SW,                                      00048200
048300             LOC_ORD_PROCNG_DD,                                   00048300
048400             ORD_PROCNG_CTOF_TM,                                  00048400
048500             SCH_SHP_DD_TXT,                                      00048500
048600             ORD_LEAD_TM_DD,                                      00048600
048700             ORD_BUFFER_TM_DD                                     00048700
048800         FROM     XXXATION                                        00048800
048900         WHERE   (LOC_TYP_CD = :DCLXXXATION.LOC-TYP-CD)           00048900
049000         AND     (LOC_NM >= :DCLXXXATION.LOC-NM)                  00049000
049100         AND NOT                                                  00049100
049200                 (LOC_NM  = :DCLXXXATION.LOC-NM AND               00049200
049300                  LOC_NBR  < :DCLXXXATION.LOC-NBR)                00049300
049400         ORDER BY                                                 00049400
049500             LOC_NM,                                              00049500
049600             LOC_NBR                                              00049600
049700     END-EXEC.                                                    00049700
049800* --------------------------------------------------              00049800
049900* DDDXLO04 cursor declaration.                                    00049900
050000* --------------------------------------------------              00050000
050100     EXEC SQL                                                     00050100
050200         DECLARE DDDXLO04 CURSOR WITH HOLD FOR SELECT             00050200
050300             LOC_TYP_CD,                                          00050300
050400             LOC_NBR,                                             00050400
050500             LOC_NM,                                              00050500
050600             LOC_ABB,                                             00050600
050700             LGL_LOC_NAM,                                         00050700
050800             PRIM_CONTACT_NM,                                     00050800
050900             PRIM_ADR_1,                                          00050900
051000             PRIM_ADR_2,                                          00051000
051100             PRIM_ADR_3,                                          00051100
051200             PRIM_ADR_4,                                          00051200
051300             PRIM_CITY,                                           00051300
051400             PRIM_CITY_ID,                                        00051400
051500             PRIM_STATE_CD,                                       00051500
051600             PRIM_ZIP5_CD,                                        00051600
051700             PRIM_ZIP4_CD,                                        00051700
051800             PRIM_PHN_CNTRY_CD,                                   00051800
051900             PRIM_AREA_CD,                                        00051900
052000             PRIM_PHONE_NBR,                                      00052000
052100             PRIM_CNTRY_NM,                                       00052100
052200             PRIM_CNTRY_ABB,                                      00052200
052300             SEC_LOC_NM,                                          00052300
052400             SEC_CONTACT_NM,                                      00052400
052500             SEC_ADR_1,                                           00052500
052600             SEC_ADR_2,                                           00052600
052700             SEC_ADR_3,                                           00052700
052800             SEC_ADR_4,                                           00052800
052900             SEC_CITY,                                            00052900
053000             SEC_STATE_CD,                                        00053000
053100             SEC_ZIP5_CD,                                         00053100
053200             SEC_ZIP4_CD,                                         00053200
053300             SEC_PHN_CNTRY_CD,                                    00053300
053400             SEC_AREA_CD,                                         00053400
053500             SEC_PHONE_NBR,                                       00053500
053600             SEC_CNTRY_NM,                                        00053600
053700             SEC_CNTRY_ABB,                                       00053700
053800             MAIL_TO_LOC_NM,                                      00053800
053900             MAIL_TO_CNTCT_NM,                                    00053900
054000             MAIL_TO_ADR_1,                                       00054000
054100             MAIL_TO_ADR_2,                                       00054100
054200             MAIL_TO_ADR_3,                                       00054200
054300             MAIL_TO_ADR_4,                                       00054300
054400             MAIL_TO_CITY,                                        00054400
054500             MAIL_TO_STATE_CD,                                    00054500
054600             MAIL_TO_ZIP5_CD,                                     00054600
054700             MAIL_TO_ZIP4_CD,                                     00054700
054800             MAIL_PHN_CNTRY_CD,                                   00054800
054900             MAIL_TO_AREA_CD,                                     00054900
055000             MAIL_TO_PHONE_NBR,                                   00055000
055100             MAIL_TO_CNTRY_NM,                                    00055100
055200             MAIL_TO_CNTRY_AB,                                    00055200
055300             CURR_FAX_ID,                                         00055300
055400             ADDED_DT,                                            00055400
055500             DELETE_DT,                                           00055500
055600             OPENED_DT,                                           00055600
055700             CLOSED_DT,                                           00055700
055800             INACTIVE_SW,                                         00055800
055900             INACTIVE_DT,                                         00055900
056000             AP_NBR,                                              00056000
056100             AP_TYP_CD,                                           00056100
056200             LST_UPDT_TS,                                         00056200
056300             LST_UPDT_USR_ID,                                     00056300
056400             PRIM_EMAIL_ID  ,                                     00056400
056500             SECY_EMAIL_ID  ,                                     00056500
056600             MAIL_TO_EMAIL_ID,                                    00056600
056700             FAC_ID,                                              00056700
056800             ORG_ID,                                              00056800
056900             B2B_PRIM_RTNG_ID,                                    00056900
057000             PRIM_CNTY_TXT,                                       00057000
057100             SECY_CNTY_TXT,                                       00057100
057200             MAIL_TO_CNTY_TXT,                                    00057200
057300             DIR_SHP_LOC_SW,                                      00057300
057400             LOC_ORD_PROCNG_DD,                                   00057400
057500             ORD_PROCNG_CTOF_TM,                                  00057500
057600             SCH_SHP_DD_TXT,                                      00057600
057700             ORD_LEAD_TM_DD,                                      00057700
057800             ORD_BUFFER_TM_DD                                     00057800
057900         FROM     XXXATION                                        00057900
058000         WHERE   (LOC_TYP_CD  = :DCLXXXATION.LOC-TYP-CD)          00058000
058100         AND     (PRIM_CITY >= :DCLXXXATION.PRIM-CITY)            00058100
058200         AND NOT                                                  00058200
058300                 (PRIM_CITY  = :DCLXXXATION.PRIM-CITY AND         00058300
058400                  LOC_NBR  < :DCLXXXATION.LOC-NBR)                00058400
058500         ORDER BY                                                 00058500
058600             PRIM_CITY,                                           00058600
058700             LOC_NBR                                              00058700
058800     END-EXEC.                                                    00058800
058900                                                                  00058900
059000* --------------------------------------------------              00059000
059100* DDDXLO05 cursor declaration.                                    00059100
059200* --------------------------------------------------              00059200
059300     EXEC SQL                                                     00059300
059400         DECLARE DDDXLO05 CURSOR WITH HOLD FOR SELECT             00059400
059500             LOC_TYP_CD,                                          00059500
059600             LOC_NBR,                                             00059600
059700             LOC_NM,                                              00059700
059800             LOC_ABB,                                             00059800
059900             LGL_LOC_NAM,                                         00059900
060000             PRIM_CONTACT_NM,                                     00060000
060100             PRIM_ADR_1,                                          00060100
060200             PRIM_ADR_2,                                          00060200
060300             PRIM_ADR_3,                                          00060300
060400             PRIM_ADR_4,                                          00060400
060500             PRIM_CITY,                                           00060500
060600             PRIM_CITY_ID,                                        00060600
060700             PRIM_STATE_CD,                                       00060700
060800             PRIM_ZIP5_CD,                                        00060800
060900             PRIM_ZIP4_CD,                                        00060900
061000             PRIM_PHN_CNTRY_CD,                                   00061000
061100             PRIM_AREA_CD,                                        00061100
061200             PRIM_PHONE_NBR,                                      00061200
061300             PRIM_CNTRY_NM,                                       00061300
061400             PRIM_CNTRY_ABB,                                      00061400
061500             SEC_LOC_NM,                                          00061500
061600             SEC_CONTACT_NM,                                      00061600
061700             SEC_ADR_1,                                           00061700
061800             SEC_ADR_2,                                           00061800
061900             SEC_ADR_3,                                           00061900
062000             SEC_ADR_4,                                           00062000
062100             SEC_CITY,                                            00062100
062200             SEC_STATE_CD,                                        00062200
062300             SEC_ZIP5_CD,                                         00062300
062400             SEC_ZIP4_CD,                                         00062400
062500             SEC_PHN_CNTRY_CD,                                    00062500
062600             SEC_AREA_CD,                                         00062600
062700             SEC_PHONE_NBR,                                       00062700
062800             SEC_CNTRY_NM,                                        00062800
062900             SEC_CNTRY_ABB,                                       00062900
063000             MAIL_TO_LOC_NM,                                      00063000
063100             MAIL_TO_CNTCT_NM,                                    00063100
063200             MAIL_TO_ADR_1,                                       00063200
063300             MAIL_TO_ADR_2,                                       00063300
063400             MAIL_TO_ADR_3,                                       00063400
063500             MAIL_TO_ADR_4,                                       00063500
063600             MAIL_TO_CITY,                                        00063600
063700             MAIL_TO_STATE_CD,                                    00063700
063800             MAIL_TO_ZIP5_CD,                                     00063800
063900             MAIL_TO_ZIP4_CD,                                     00063900
064000             MAIL_PHN_CNTRY_CD,                                   00064000
064100             MAIL_TO_AREA_CD,                                     00064100
064200             MAIL_TO_PHONE_NBR,                                   00064200
064300             MAIL_TO_CNTRY_NM,                                    00064300
064400             MAIL_TO_CNTRY_AB,                                    00064400
064500             CURR_FAX_ID,                                         00064500
064600             ADDED_DT,                                            00064600
064700             DELETE_DT,                                           00064700
064800             OPENED_DT,                                           00064800
064900             CLOSED_DT,                                           00064900
065000             INACTIVE_SW,                                         00065000
065100             INACTIVE_DT,                                         00065100
065200             AP_NBR,                                              00065200
065300             AP_TYP_CD,                                           00065300
065400             LST_UPDT_TS,                                         00065400
065500             LST_UPDT_USR_ID,                                     00065500
065600             PRIM_EMAIL_ID  ,                                     00065600
065700             SECY_EMAIL_ID  ,                                     00065700
065800             MAIL_TO_EMAIL_ID,                                    00065800
065900             FAC_ID,                                              00065900
066000             ORG_ID,                                              00066000
066100             B2B_PRIM_RTNG_ID,                                    00066100
066200             PRIM_CNTY_TXT,                                       00066200
066300             SECY_CNTY_TXT,                                       00066300
066400             MAIL_TO_CNTY_TXT,                                    00066400
066500             DIR_SHP_LOC_SW,                                      00066500
066600             LOC_ORD_PROCNG_DD,                                   00066600
066700             ORD_PROCNG_CTOF_TM,                                  00066700
066800             SCH_SHP_DD_TXT,                                      00066800
066900             ORD_LEAD_TM_DD,                                      00066900
067000             ORD_BUFFER_TM_DD                                     00067000
067100         FROM     XXXATION                                        00067100
067200         WHERE   (AP_TYP_CD >=                                    00067200
067300                  :DCLXXXATION.AP-TYP-CD)                         00067300
067400         AND NOT                                                  00067400
067500                 (AP_TYP_CD  =                                    00067500
067600                  :DCLXXXATION.AP-TYP-CD AND                      00067600
067700                  AP_NBR  < :DCLXXXATION.AP-NBR)                  00067700
067800         AND NOT                                                  00067800
067900                 (AP_TYP_CD  =                                    00067900
068000                  :DCLXXXATION.AP-TYP-CD AND                      00068000
068100                  AP_NBR  = :DCLXXXATION.AP-NBR AND               00068100
068200                  LOC_TYP_CD  <                                   00068200
068300                        :DCLXXXATION.LOC-TYP-CD)                  00068300
068400         AND NOT                                                  00068400
068500                 (AP_TYP_CD  =                                    00068500
068600                  :DCLXXXATION.AP-TYP-CD AND                      00068600
068700                  AP_NBR  = :DCLXXXATION.AP-NBR AND               00068700
068800                  LOC_TYP_CD  =                                   00068800
068900                  :DCLXXXATION.LOC-TYP-CD AND                     00068900
069000                  LOC_NBR  < :DCLXXXATION.LOC-NBR)                00069000
069100         ORDER BY                                                 00069100
069200                     AP_TYP_CD,                                   00069200
069300                     AP_NBR,                                      00069300
069400                     LOC_TYP_CD,                                  00069400
069500                     LOC_NBR                                      00069500
069600     END-EXEC.                                                    00069600
069700* --------------------------------------------------              00069700
069800* DDDXLO06 cursor declaration.                                    00069800
069900* --------------------------------------------------              00069900
070000     EXEC SQL                                                     00070000
070100         DECLARE DDDXLO06 CURSOR WITH HOLD FOR SELECT             00070100
070200             LOC_TYP_CD,                                          00070200
070300             LOC_NBR,                                             00070300
070400             LOC_NM,                                              00070400
070500             LOC_ABB,                                             00070500
070600             LGL_LOC_NAM,                                         00070600
070700             PRIM_CONTACT_NM,                                     00070700
070800             PRIM_ADR_1,                                          00070800
070900             PRIM_ADR_2,                                          00070900
071000             PRIM_ADR_3,                                          00071000
071100             PRIM_ADR_4,                                          00071100
071200             PRIM_CITY,                                           00071200
071300             PRIM_CITY_ID,                                        00071300
071400             PRIM_STATE_CD,                                       00071400
071500             PRIM_ZIP5_CD,                                        00071500
071600             PRIM_ZIP4_CD,                                        00071600
071700             PRIM_PHN_CNTRY_CD,                                   00071700
071800             PRIM_AREA_CD,                                        00071800
071900             PRIM_PHONE_NBR,                                      00071900
072000             PRIM_CNTRY_NM,                                       00072000
072100             PRIM_CNTRY_ABB,                                      00072100
072200             SEC_LOC_NM,                                          00072200
072300             SEC_CONTACT_NM,                                      00072300
072400             SEC_ADR_1,                                           00072400
072500             SEC_ADR_2,                                           00072500
072600             SEC_ADR_3,                                           00072600
072700             SEC_ADR_4,                                           00072700
072800             SEC_CITY,                                            00072800
072900             SEC_STATE_CD,                                        00072900
073000             SEC_ZIP5_CD,                                         00073000
073100             SEC_ZIP4_CD,                                         00073100
073200             SEC_PHN_CNTRY_CD,                                    00073200
073300             SEC_AREA_CD,                                         00073300
073400             SEC_PHONE_NBR,                                       00073400
073500             SEC_CNTRY_NM,                                        00073500
073600             SEC_CNTRY_ABB,                                       00073600
073700             MAIL_TO_LOC_NM,                                      00073700
073800             MAIL_TO_CNTCT_NM,                                    00073800
073900             MAIL_TO_ADR_1,                                       00073900
074000             MAIL_TO_ADR_2,                                       00074000
074100             MAIL_TO_ADR_3,                                       00074100
074200             MAIL_TO_ADR_4,                                       00074200
074300             MAIL_TO_CITY,                                        00074300
074400             MAIL_TO_STATE_CD,                                    00074400
074500             MAIL_TO_ZIP5_CD,                                     00074500
074600             MAIL_TO_ZIP4_CD,                                     00074600
074700             MAIL_PHN_CNTRY_CD,                                   00074700
074800             MAIL_TO_AREA_CD,                                     00074800
074900             MAIL_TO_PHONE_NBR,                                   00074900
075000             MAIL_TO_CNTRY_NM,                                    00075000
075100             MAIL_TO_CNTRY_AB,                                    00075100
075200             CURR_FAX_ID,                                         00075200
075300             ADDED_DT,                                            00075300
075400             DELETE_DT,                                           00075400
075500             OPENED_DT,                                           00075500
075600             CLOSED_DT,                                           00075600
075700             INACTIVE_SW,                                         00075700
075800             INACTIVE_DT,                                         00075800
075900             AP_NBR,                                              00075900
076000             AP_TYP_CD,                                           00076000
076100             LST_UPDT_TS,                                         00076100
076200             LST_UPDT_USR_ID,                                     00076200
076300             PRIM_EMAIL_ID  ,                                     00076300
076400             SECY_EMAIL_ID  ,                                     00076400
076500             MAIL_TO_EMAIL_ID,                                    00076500
076600             FAC_ID,                                              00076600
076700             ORG_ID,                                              00076700
076800             B2B_PRIM_RTNG_ID,                                    00076800
076900             PRIM_CNTY_TXT,                                       00076900
077000             SECY_CNTY_TXT,                                       00077000
077100             MAIL_TO_CNTY_TXT,                                    00077100
077200             DIR_SHP_LOC_SW,                                      00077200
077300             LOC_ORD_PROCNG_DD,                                   00077300
077400             ORD_PROCNG_CTOF_TM,                                  00077400
077500             SCH_SHP_DD_TXT,                                      00077500
077600             ORD_LEAD_TM_DD,                                      00077600
077700             ORD_BUFFER_TM_DD                                     00077700
077800         FROM     XXXATION                                        00077800
077900         WHERE   ( AP_NBR    >=                                   00077900
078000                  :DCLXXXATION.AP-NBR )                           00078000
078100         AND NOT                                                  00078100
078200                 ( AP_NBR    =                                    00078200
078300                  :DCLXXXATION.AP-NBR   AND                       00078300
078400                  AP_TYP_CD  < :DCLXXXATION.AP-TYP-CD )           00078400
078500         AND NOT                                                  00078500
078600                 ( AP_TYP_CD  =                                   00078600
078700                  :DCLXXXATION.AP-TYP-CD AND                      00078700
078800                  AP_NBR  = :DCLXXXATION.AP-NBR AND               00078800
078900                  LOC_TYP_CD <                                    00078900
079000                        :DCLXXXATION.LOC-TYP-CD )                 00079000
079100         AND NOT                                                  00079100
079200                 ( AP_TYP_CD  =                                   00079200
079300                  :DCLXXXATION.AP-TYP-CD AND                      00079300
079400                  AP_NBR  = :DCLXXXATION.AP-NBR AND               00079400
079500                  LOC_TYP_CD =                                    00079500
079600                  :DCLXXXATION.LOC-TYP-CD AND                     00079600
079700                  LOC_NBR    < :DCLXXXATION.LOC-NBR )             00079700
079800         ORDER BY                                                 00079800
079900                     AP_NBR,                                      00079900
080000                     AP_TYP_CD,                                   00080000
080100                     LOC_TYP_CD,                                  00080100
080200                     LOC_NBR                                      00080200
080300     END-EXEC.                                                    00080300
080400* --------------------------------------------------              00080400
080500* DDDXLO07 cursor declaration.                                    00080500
080600* --------------------------------------------------              00080600
080700     EXEC SQL                                                     00080700
080800         DECLARE DDDXLO07 CURSOR WITH HOLD FOR SELECT             00080800
080900             LO.LOC_TYP_CD,                                       00080900
081000             LO.LOC_NBR,                                          00081000
081100             LO.LOC_NM,                                           00081100
081200             LO.LOC_ABB,                                          00081200
081300             LO.LGL_LOC_NAM,                                      00081300
081400             LO.PRIM_CONTACT_NM,                                  00081400
081500             LO.PRIM_ADR_1,                                       00081500
081600             LO.PRIM_ADR_2,                                       00081600
081700             LO.PRIM_ADR_3,                                       00081700
081800             LO.PRIM_ADR_4,                                       00081800
081900             LO.PRIM_CITY,                                        00081900
082000             LO.PRIM_CITY_ID,                                     00082000
082100             LO.PRIM_STATE_CD,                                    00082100
082200             LO.PRIM_ZIP5_CD,                                     00082200
082300             LO.PRIM_ZIP4_CD,                                     00082300
082400             LO.PRIM_PHN_CNTRY_CD,                                00082400
082500             LO.PRIM_AREA_CD,                                     00082500
082600             LO.PRIM_PHONE_NBR,                                   00082600
082700             LO.PRIM_CNTRY_NM,                                    00082700
082800             LO.PRIM_CNTRY_ABB,                                   00082800
082900             LO.SEC_LOC_NM,                                       00082900
083000             LO.SEC_CONTACT_NM,                                   00083000
083100             LO.SEC_ADR_1,                                        00083100
083200             LO.SEC_ADR_2,                                        00083200
083300             LO.SEC_ADR_3,                                        00083300
083400             LO.SEC_ADR_4,                                        00083400
083500             LO.SEC_CITY,                                         00083500
083600             LO.SEC_STATE_CD,                                     00083600
083700             LO.SEC_ZIP5_CD,                                      00083700
083800             LO.SEC_ZIP4_CD,                                      00083800
083900             LO.SEC_PHN_CNTRY_CD,                                 00083900
084000             LO.SEC_AREA_CD,                                      00084000
084100             LO.SEC_PHONE_NBR,                                    00084100
084200             LO.SEC_CNTRY_NM,                                     00084200
084300             LO.SEC_CNTRY_ABB,                                    00084300
084400             LO.MAIL_TO_LOC_NM,                                   00084400
084500             LO.MAIL_TO_CNTCT_NM,                                 00084500
084600             LO.MAIL_TO_ADR_1,                                    00084600
084700             LO.MAIL_TO_ADR_2,                                    00084700
084800             LO.MAIL_TO_ADR_3,                                    00084800
084900             LO.MAIL_TO_ADR_4,                                    00084900
085000             LO.MAIL_TO_CITY,                                     00085000
085100             LO.MAIL_TO_STATE_CD,                                 00085100
085200             LO.MAIL_TO_ZIP5_CD,                                  00085200
085300             LO.MAIL_TO_ZIP4_CD,                                  00085300
085400             LO.MAIL_PHN_CNTRY_CD,                                00085400
085500             LO.MAIL_TO_AREA_CD,                                  00085500
085600             LO.MAIL_TO_PHONE_NBR,                                00085600
085700             LO.MAIL_TO_CNTRY_NM,                                 00085700
085800             LO.MAIL_TO_CNTRY_AB,                                 00085800
085900             LO.CURR_FAX_ID,                                      00085900
086000             LO.ADDED_DT,                                         00086000
086100             LO.DELETE_DT,                                        00086100
086200             LO.OPENED_DT,                                        00086200
086300             LO.CLOSED_DT,                                        00086300
086400             LO.INACTIVE_SW,                                      00086400
086500             LO.INACTIVE_DT,                                      00086500
086600             LO.AP_NBR,                                           00086600
086700             LO.AP_TYP_CD,                                        00086700
086800             LO.LST_UPDT_TS,                                      00086800
086900             LO.LST_UPDT_USR_ID,                                  00086900
087000             LO.PRIM_EMAIL_ID  ,                                  00087000
087100             LO.SECY_EMAIL_ID  ,                                  00087100
087200             LO.MAIL_TO_EMAIL_ID,                                 00087200
087300             LO.FAC_ID,                                           00087300
087400             LO.ORG_ID,                                           00087400
087500             LO.B2B_PRIM_RTNG_ID,                                 00087500
087600             LO.PRIM_CNTY_TXT,                                    00087600
087700             LO.SECY_CNTY_TXT,                                    00087700
087800             LO.MAIL_TO_CNTY_TXT,                                 00087800
087900             LO.DIR_SHP_LOC_SW,                                   00087900
088000             LO.LOC_ORD_PROCNG_DD,                                00088000
088100             LO.ORD_PROCNG_CTOF_TM,                               00088100
088200             LO.SCH_SHP_DD_TXT,                                   00088200
088300             LO.ORD_LEAD_TM_DD,                                   00088300
088400             LO.ORD_BUFFER_TM_DD                                  00088400
088500         FROM     XXXATION LO                                     00088500
088600             , AP_XXXATION AP                                     00088600
088700             , AP_XXX_SUB_TYP LST                                 00088700
088800         WHERE (LO.LOC_NBR >= :DCLXXXATION.LOC-NBR)               00088800
088900           AND AP.AP_NBR          = LST.AP_NBR                    00088900
089000           AND AP.AP_TYP_CD       = LST.AP_TYP_CD                 00089000
089100           AND LO.AP_NBR          = AP.AP_NBR                     00089100
089200           AND LO.AP_TYP_CD       = AP.AP_TYP_CD                  00089200
089300           AND LO.LOC_TYP_CD      = 'D'                           00089300
089400           AND AP.AP_TYP_CD       = 'DS'                          00089400
089500           AND LST.AP_SUB_TYP_CD  = 'DSV'                         00089500
089600         ORDER BY                                                 00089600
089700             LO.LOC_NBR                                           00089700
089800     END-EXEC.                                                    00089800
089900* ----------------------------------------------------------      00089900
090000* CURSOR USED TO DELETE ITEM FOR THE  FAX ID FROM XXX-NUMBERS     00090000
090100* -----------------------------------------------------------     00090100
090200     EXEC SQL                                                     00090200
090300         DECLARE DEL-CSR CURSOR WITH HOLD FOR SELECT              00090300
090400             FAX_ID                                               00090400
090500         FROM   XXX_NUMBERS                                       00090500
090600         WHERE  LOC_TYP_CD = :DCLXXX-NUMBERS.LOC-TYP-CD           00090600
090700           AND  LOC_NBR    = :DCLXXX-NUMBERS.LOC-NBR              00090700
090800     END-EXEC.                                                    00090800
090900                                                                  00090900
091000* ----------------------------------------------------------      00091000
091100* Cursor used to delete item for the  prod id from prod-comments  00091100
091200* -----------------------------------------------------------     00091200
091300     EXEC SQL                                                     00091300
091400         DECLARE DEL-CSR1 CURSOR WITH HOLD FOR SELECT             00091400
091500             ITM_CLS_CD                                           00091500
091600         FROM   XXXL_LOC_CLS_AD_ZN                                00091600
091700         WHERE  LOC_TYP_CD = :DCLXXXL-LOC-CLS-AD-ZN.LOC-TYP-CD    00091700
091800           AND  LOC_NBR    = :DCLXXXL-LOC-CLS-AD-ZN.LOC-NBR       00091800
091900     END-EXEC.                                                    00091900
092000                                                                  00092000
092100 LINKAGE SECTION.                                                 00092100
092200 COPY XXXN001A.                                                   00092200
092300     EXEC SQL                                                     00092300
092400         INCLUDE SQLCA                                            00092400
092500     END-EXEC.                                                    00092500
092600 COPY YYYN005A.                                                   00092600
092700 COPY NNNN0000.                                                   00092700
092800 COPY PPPTLO01.                                                   00092800
092900                                                                  00092900
093000 PROCEDURE DIVISION USING                                         00093000
093100     XXXN001A                                                     00093100
093200     SQLCA                                                        00093200
093300     YYYN005A                                                     00093300
093400     NNNN0000-PARMS                                               00093400
093500     P-DDDTLO01                                                   00093500
093600     .                                                            00093600
093700                                                                  00093700
093800************************************************************      00093800
093900* MAIN PROGRAM LINE.                                              00093900
094000************************************************************      00094000
094100 0000-EXIT-DISPATCHER.                                            00094100
094200     PERFORM 100-INITIALIZATION                                   00094200
094300     EVALUATE TRUE                                                00094300
094400       WHEN NOT SUCCESS                                           00094400
094500          CONTINUE                                                00094500
094600       WHEN EXIT-OPEN-CURSOR                                      00094600
094700          PERFORM 1000-EXIT-OPEN-CURSOR                           00094700
094800       WHEN EXIT-CLOSE-CURSOR                                     00094800
094900          PERFORM 1100-EXIT-CLOSE-CURSOR                          00094900
095000       WHEN EXIT-GET-UNIQUE-ROW                                   00095000
095100          PERFORM 1200-EXIT-GET-UNIQUE-ROW                        00095100
095200       WHEN EXIT-GET-NEXT-ROW                                     00095200
095300          PERFORM 1300-EXIT-GET-NEXT-ROW                          00095300
095400       WHEN EXIT-PUT-MODIFY-ROW                                   00095400
095500          PERFORM 1400-EXIT-PUT-MODIFY-ROW                        00095500
095600       WHEN EXIT-PUT-INSERT-ROW                                   00095600
095700          PERFORM 1500-EXIT-PUT-INSERT-ROW                        00095700
095800       WHEN EXIT-PUT-PURGE-ROW                                    00095800
095900          PERFORM 1600-EXIT-PUT-PURGE-ROW                         00095900
096000       WHEN EXIT-DO-SPECIAL-IO-FUNCS                              00096000
096100          PERFORM 10000-DO-SPECIAL-IO-FUNCS                       00096100
096200     END-EVALUATE                                                 00096200
096300                                                                  00096300
096400     PERFORM 120-EXIT-STUFF                                       00096400
096500     GOBACK                                                       00096500
096600     .                                                            00096600
096700                                                                  00096700
096800* ================================================================00096800
096900* Initialize data areas needed to call the i/o subroutine         00096900
097000* ================================================================00097000
097100 100-INITIALIZATION.                                              00097100
097200     INITIALIZE XXXN001A                                          00097200
097300                DAO-STATUS                                        00097300
097400                MMMC0474                                          00097400
097500                ZZZC0550                                          00097500
097600     MOVE NNNN0000-INDEX-HANDLE TO DDDTLO01-INDEX-HANDLE          00097600
097700     MOVE 0 TO WS-CHECKPOINT-INC                                  00097700
097800     MOVE 0 TO SQLCODE                                            00097800
097900     MOVE 0 TO SQL-INIT-FLAG                                      00097900
098000     IF NOT EXIT-CLOSE-CURSOR                                     00098000
098100       PERFORM 110-MOVE-PDA-FIELDS-2-DCL                          00098100
098200     END-IF                                                       00098200
098300     IF (YYYN005A-ORACLE       OR EXIT-PUT-INSERT-ROW             00098300
098400         OR EXIT-PUT-PURGE-ROW OR EXIT-PUT-MODIFY-ROW)            00098400
098500       PERFORM 115-CONNECT-TO-ORACLE                              00098500
098600     END-IF                                                       00098600
098700     .                                                            00098700
098800                                                                  00098800
098900                                                                  00098900
099000                                                                  00099000
099100                                                                  00099100
099200* ================================================================00099200
099300* Move the elementary fields in the parameter data area to the DCL00099300
099400* ================================================================00099400
099500 110-MOVE-PDA-FIELDS-2-DCL.                                       00099500
099600     MOVE LENGTH OF PRIM-CONTACT-NM OF P-DDDTLO01 TO WS-STR-LEN   00099600
099700     CALL  YYYS0134-STRING-CRUNCH USING                           00099700
099800                            XXXN001A                              00099800
099900                            PRIM-CONTACT-NM OF P-DDDTLO01         00099900
100000                            WS-STR-LEN                            00100000
100100     INITIALIZE XXXN001A                                          00100100
100200     MOVE LOC-TYP-CD OF P-DDDTLO01 TO LOC-TYP-CD OF DCLXXXATION   00100200
100300     MOVE LOC-NBR OF P-DDDTLO01 TO LOC-NBR OF DCLXXXATION         00100300
100400     MOVE LOC-NM OF P-DDDTLO01 TO LOC-NM OF DCLXXXATION           00100400
100500     MOVE LOC-ABB OF P-DDDTLO01 TO LOC-ABB OF DCLXXXATION         00100500
100600     MOVE LGL-LOC-NAM OF P-DDDTLO01 TO LGL-LOC-NAM OF DCLXXXATION 00100600
100700     MOVE PRIM-CONTACT-NM OF P-DDDTLO01                           00100700
100800       TO PRIM-CONTACT-NM OF DCLXXXATION                          00100800
100900     MOVE PRIM-ADR-1 OF P-DDDTLO01 TO PRIM-ADR-1 OF DCLXXXATION   00100900
101000     MOVE PRIM-ADR-2 OF P-DDDTLO01 TO PRIM-ADR-2 OF DCLXXXATION   00101000
101100     MOVE PRIM-ADR-3 OF P-DDDTLO01 TO PRIM-ADR-3 OF DCLXXXATION   00101100
101200     MOVE PRIM-ADR-4 OF P-DDDTLO01 TO PRIM-ADR-4 OF DCLXXXATION   00101200
101300     MOVE PRIM-CITY OF P-DDDTLO01 TO PRIM-CITY OF DCLXXXATION     00101300
101400     MOVE PRIM-CITY-ID OF P-DDDTLO01                              00101400
101500       TO PRIM-CITY-ID OF DCLXXXATION                             00101500
101600     MOVE PRIM-STATE-CD OF P-DDDTLO01                             00101600
101700       TO PRIM-STATE-CD OF DCLXXXATION                            00101700
101800     MOVE PRIM-ZIP5-CD OF P-DDDTLO01                              00101800
101900       TO PRIM-ZIP5-CD OF DCLXXXATION                             00101900
102000     MOVE PRIM-ZIP4-CD OF P-DDDTLO01                              00102000
102100       TO PRIM-ZIP4-CD OF DCLXXXATION                             00102100
102200     MOVE PRIM-PHN-CNTRY-CD OF P-DDDTLO01                         00102200
102300       TO PRIM-PHN-CNTRY-CD OF DCLXXXATION                        00102300
102400     MOVE PRIM-AREA-CD OF P-DDDTLO01                              00102400
102500       TO PRIM-AREA-CD OF DCLXXXATION                             00102500
102600     MOVE PRIM-PHONE-NBR OF P-DDDTLO01                            00102600
102700       TO PRIM-PHONE-NBR OF DCLXXXATION                           00102700
102800     MOVE PRIM-CNTRY-NM OF P-DDDTLO01                             00102800
102900       TO PRIM-CNTRY-NM OF DCLXXXATION                            00102900
103000     MOVE PRIM-CNTRY-ABB OF P-DDDTLO01                            00103000
103100       TO PRIM-CNTRY-ABB OF DCLXXXATION                           00103100
103200     MOVE SEC-LOC-NM OF P-DDDTLO01 TO SEC-LOC-NM OF DCLXXXATION   00103200
103300     MOVE SEC-CONTACT-NM OF P-DDDTLO01                            00103300
103400       TO SEC-CONTACT-NM OF DCLXXXATION                           00103400
103500     MOVE SEC-ADR-1 OF P-DDDTLO01 TO SEC-ADR-1 OF DCLXXXATION     00103500
103600     MOVE SEC-ADR-2 OF P-DDDTLO01 TO SEC-ADR-2 OF DCLXXXATION     00103600
103700     MOVE SEC-ADR-3 OF P-DDDTLO01 TO SEC-ADR-3 OF DCLXXXATION     00103700
103800     MOVE SEC-ADR-4 OF P-DDDTLO01 TO SEC-ADR-4 OF DCLXXXATION     00103800
103900     MOVE SEC-CITY OF P-DDDTLO01 TO SEC-CITY OF DCLXXXATION       00103900
104000     MOVE SEC-STATE-CD OF P-DDDTLO01                              00104000
104100       TO SEC-STATE-CD OF DCLXXXATION                             00104100
104200     MOVE SEC-ZIP5-CD OF P-DDDTLO01 TO SEC-ZIP5-CD OF DCLXXXATION 00104200
104300     MOVE SEC-ZIP4-CD OF P-DDDTLO01 TO SEC-ZIP4-CD OF DCLXXXATION 00104300
104400     MOVE SEC-PHN-CNTRY-CD OF P-DDDTLO01                          00104400
104500       TO SEC-PHN-CNTRY-CD OF DCLXXXATION                         00104500
104600     MOVE SEC-AREA-CD OF P-DDDTLO01 TO SEC-AREA-CD OF DCLXXXATION 00104600
104700     MOVE SEC-PHONE-NBR OF P-DDDTLO01                             00104700
104800       TO SEC-PHONE-NBR OF DCLXXXATION                            00104800
104900     MOVE SEC-CNTRY-NM OF P-DDDTLO01                              00104900
105000       TO SEC-CNTRY-NM OF DCLXXXATION                             00105000
105100     MOVE SEC-CNTRY-ABB OF P-DDDTLO01                             00105100
105200       TO SEC-CNTRY-ABB OF DCLXXXATION                            00105200
105300     MOVE MAIL-TO-LOC-NM OF P-DDDTLO01                            00105300
105400       TO MAIL-TO-LOC-NM OF DCLXXXATION                           00105400
105500     MOVE MAIL-TO-CNTCT-NM OF P-DDDTLO01                          00105500
105600       TO MAIL-TO-CNTCT-NM OF DCLXXXATION                         00105600
105700     MOVE MAIL-TO-ADR-1 OF P-DDDTLO01                             00105700
105800       TO MAIL-TO-ADR-1 OF DCLXXXATION                            00105800
105900     MOVE MAIL-TO-ADR-2 OF P-DDDTLO01                             00105900
106000       TO MAIL-TO-ADR-2 OF DCLXXXATION                            00106000
106100     MOVE MAIL-TO-ADR-3 OF P-DDDTLO01                             00106100
106200       TO MAIL-TO-ADR-3 OF DCLXXXATION                            00106200
106300     MOVE MAIL-TO-ADR-4 OF P-DDDTLO01                             00106300
106400       TO MAIL-TO-ADR-4 OF DCLXXXATION                            00106400
106500     MOVE MAIL-TO-CITY OF P-DDDTLO01                              00106500
106600       TO MAIL-TO-CITY OF DCLXXXATION                             00106600
106700     MOVE MAIL-TO-STATE-CD OF P-DDDTLO01                          00106700
106800       TO MAIL-TO-STATE-CD OF DCLXXXATION                         00106800
106900     MOVE MAIL-TO-ZIP5-CD OF P-DDDTLO01                           00106900
107000       TO MAIL-TO-ZIP5-CD OF DCLXXXATION                          00107000
107100     MOVE MAIL-TO-ZIP4-CD OF P-DDDTLO01                           00107100
107200       TO MAIL-TO-ZIP4-CD OF DCLXXXATION                          00107200
107300     MOVE MAIL-PHN-CNTRY-CD OF P-DDDTLO01                         00107300
107400       TO MAIL-PHN-CNTRY-CD OF DCLXXXATION                        00107400
107500     MOVE MAIL-TO-AREA-CD OF P-DDDTLO01                           00107500
107600       TO MAIL-TO-AREA-CD OF DCLXXXATION                          00107600
107700     MOVE MAIL-TO-PHONE-NBR OF P-DDDTLO01                         00107700
107800       TO MAIL-TO-PHONE-NBR OF DCLXXXATION                        00107800
107900     MOVE MAIL-TO-CNTRY-NM OF P-DDDTLO01                          00107900
108000       TO MAIL-TO-CNTRY-NM OF DCLXXXATION                         00108000
108100     MOVE MAIL-TO-CNTRY-AB OF P-DDDTLO01                          00108100
108200       TO MAIL-TO-CNTRY-AB OF DCLXXXATION                         00108200
108300     MOVE CURR-FAX-ID OF P-DDDTLO01 TO CURR-FAX-ID OF DCLXXXATION 00108300
108400                                                                  00108400
108500     IF ADDED-DT OF P-DDDTLO01 = SPACES                           00108500
108600     OR ADDED-DT OF P-DDDTLO01 = K-ZERO-DT                        00108600
108700       MOVE K-DEF-DT TO ADDED-DT OF P-DDDTLO01                    00108700
108800     END-IF                                                       00108800
108900     MOVE ADDED-DT OF P-DDDTLO01 TO ADDED-DT OF DCLXXXATION       00108900
109000                                                                  00109000
109100     IF DELETE-DT OF P-DDDTLO01 = SPACES                          00109100
109200     OR DELETE-DT OF P-DDDTLO01 = K-ZERO-DT                       00109200
109300       MOVE K-DEF-DT TO DELETE-DT OF P-DDDTLO01                   00109300
109400     END-IF                                                       00109400
109500     MOVE DELETE-DT OF P-DDDTLO01 TO DELETE-DT OF DCLXXXATION     00109500
109600                                                                  00109600
109700     IF INACTIVE-DT OF P-DDDTLO01 = SPACES                        00109700
109800     OR INACTIVE-DT OF P-DDDTLO01 = K-ZERO-DT                     00109800
109900       MOVE K-DEF-DT TO INACTIVE-DT OF P-DDDTLO01                 00109900
110000     END-IF                                                       00110000
110100     MOVE INACTIVE-DT OF P-DDDTLO01 TO INACTIVE-DT OF DCLXXXATION 00110100
110200                                                                  00110200
110300     IF OPENED-DT OF P-DDDTLO01 = SPACES                          00110300
110400     OR OPENED-DT OF P-DDDTLO01 = K-ZERO-DT                       00110400
110500       MOVE K-DEF-DT TO OPENED-DT OF P-DDDTLO01                   00110500
110600     END-IF                                                       00110600
110700     MOVE OPENED-DT OF P-DDDTLO01 TO OPENED-DT OF DCLXXXATION     00110700
110800                                                                  00110800
110900     IF CLOSED-DT OF P-DDDTLO01 = SPACES                          00110900
111000     OR CLOSED-DT OF P-DDDTLO01 = K-ZERO-DT                       00111000
111100       MOVE K-DEF-DT TO CLOSED-DT OF P-DDDTLO01                   00111100
111200     END-IF                                                       00111200
111300     MOVE CLOSED-DT OF P-DDDTLO01 TO CLOSED-DT OF DCLXXXATION     00111300
111400                                                                  00111400
111500     IF NOT LO-INACTIVE AND NOT LO-DELETED                        00111500
111600       SET LO-ACTIVE TO TRUE                                      00111600
111700     END-IF                                                       00111700
111800                                                                  00111800
111900     EVALUATE TRUE                                                00111900
112000       WHEN LOC-TYP-CD OF DCLXXXATION = K-VEND-LOC-TYPE           00112000
112100         MOVE K-AP-TYPE-CD TO AP-TYP-CD OF P-DDDTLO01             00112100
112200                                                                  00112200
112300       WHEN LOC-TYP-CD OF DCLXXXATION = K-STORE-LOC-TYPE          00112300
112400         MOVE 0      TO AP-NBR    OF P-DDDTLO01                   00112400
112500         MOVE SPACES TO AP-TYP-CD OF P-DDDTLO01                   00112500
112600                                                                  00112600
112700       WHEN LOC-TYP-CD OF DCLXXXATION = K-DSD-VEND-LOC-TYPE       00112700
112800         MOVE K-DSD-AP-TYPE-CD TO AP-TYP-CD OF P-DDDTLO01         00112800
112900         IF INACTIVE-DT OF P-DDDTLO01 = K-DEF-DT                  00112900
113000           SET LO-ACTIVE TO TRUE                                  00113000
113100         ELSE                                                     00113100
113200           SET LO-INACTIVE TO TRUE                                00113200
113300         END-IF                                                   00113300
113400     END-EVALUATE                                                 00113400
113500                                                                  00113500
113600     MOVE INACTIVE-SW OF P-DDDTLO01 TO INACTIVE-SW OF DCLXXXATION 00113600
113700                                                                  00113700
113800     MOVE AP-NBR OF P-DDDTLO01 TO AP-NBR OF DCLXXXATION           00113800
113900     MOVE AP-TYP-CD OF P-DDDTLO01   TO AP-TYP-CD OF DCLXXXATION   00113900
114000                                                                  00114000
114100     MOVE LST-UPDT-TS OF P-DDDTLO01 TO LST-UPDT-TS OF DCLXXXATION 00114100
114200     MOVE LST-UPDT-USR-ID OF P-DDDTLO01                           00114200
114300       TO LST-UPDT-USR-ID OF DCLXXXATION                          00114300
114400     MOVE PRIM-EMAIL-ID    OF P-DDDTLO01                          00114400
114500       TO PRIM-EMAIL-ID    OF DCLXXXATION                         00114500
114600     MOVE SECY-EMAIL-ID    OF P-DDDTLO01                          00114600
114700       TO SECY-EMAIL-ID    OF DCLXXXATION                         00114700
114800     MOVE MAIL-TO-EMAIL-ID OF P-DDDTLO01                          00114800
114900       TO MAIL-TO-EMAIL-ID OF DCLXXXATION                         00114900
115000     IF FAC-ID-X = SPACES                                         00115000
115100       MOVE 0 TO FAC-ID OF P-DDDTLO01                             00115100
115200     END-IF                                                       00115200
115300     MOVE FAC-ID           OF P-DDDTLO01                          00115300
115400       TO FAC-ID           OF DCLXXXATION                         00115400
115500     IF ORG-ID-X = SPACES                                         00115500
115600       MOVE 0 TO ORG-ID OF P-DDDTLO01                             00115600
115700     END-IF                                                       00115700
115800     MOVE ORG-ID           OF P-DDDTLO01                          00115800
115900       TO ORG-ID           OF DCLXXXATION                         00115900
116000     MOVE B2B-PRIM-RTNG-ID OF P-DDDTLO01                          00116000
116100       TO B2B-PRIM-RTNG-ID OF DCLXXXATION                         00116100
116200     MOVE PRIM-CNTY-TXT    OF P-DDDTLO01                          00116200
116300       TO PRIM-CNTY-TXT    OF DCLXXXATION                         00116300
116400     MOVE SECY-CNTY-TXT    OF P-DDDTLO01                          00116400
116500       TO SECY-CNTY-TXT    OF DCLXXXATION                         00116500
116600     MOVE MAIL-TO-CNTY-TXT OF P-DDDTLO01                          00116600
116700       TO MAIL-TO-CNTY-TXT OF DCLXXXATION                         00116700
116800                                                                  00116800
116900     IF NOT LOC-IS-DIRECT-SHIP OF P-DDDTLO01                      00116900
117000       SET LOC-IS-NOT-DIRECT-SHIP  OF P-DDDTLO01 TO TRUE          00117000
117100     END-IF                                                       00117100
117200     MOVE DIR-SHP-LOC-SW     OF P-DDDTLO01                        00117200
117300       TO DIR-SHP-LOC-SW     OF DCLXXXATION                       00117300
117400                                                                  00117400
117500     IF LOC-ORD-PROCNG-DD    OF P-DDDTLO01 NOT NUMERIC            00117500
117600        MOVE 0 TO LOC-ORD-PROCNG-DD OF P-DDDTLO01                 00117600
117700     END-IF                                                       00117700
117800     MOVE LOC-ORD-PROCNG-DD  OF P-DDDTLO01                        00117800
117900       TO LOC-ORD-PROCNG-DD  OF DCLXXXATION                       00117900
118000                                                                  00118000
118100     PERFORM 116-EDIT-SHIP-DAYS                                   00118100
118200     MOVE SCH-SHP-DD-TXT     OF P-DDDTLO01                        00118200
118300       TO SCH-SHP-DD-TXT     OF DCLXXXATION                       00118300
118400                                                                  00118400
118500                                                                  00118500
118600     IF ORD-LEAD-TM-DD OF P-DDDTLO01 IS NOT NUMERIC               00118600
118700        MOVE 0 TO ORD-LEAD-TM-DD OF P-DDDTLO01                    00118700
118800     END-IF                                                       00118800
118900     MOVE ORD-LEAD-TM-DD   OF P-DDDTLO01                          00118900
119000       TO ORD-LEAD-TM-DD   OF DCLXXXATION                         00119000
119100                                                                  00119100
119200     IF ORD-BUFFER-TM-DD OF P-DDDTLO01 IS NOT NUMERIC             00119200
119300        MOVE 0 TO ORD-BUFFER-TM-DD OF P-DDDTLO01                  00119300
119400     END-IF                                                       00119400
119500     MOVE ORD-BUFFER-TM-DD   OF P-DDDTLO01                        00119500
119600       TO ORD-BUFFER-TM-DD   OF DCLXXXATION                       00119600
119700                                                                  00119700
119800** Obsolete fields - Order Lead time and Buffer time renamed      00119800
119900** to FILLER1-TM and FILLER2-TM respectively.                     00119900
120000     MOVE WS-NULL-TM TO FILLER1-TM  OF DCLXXXATION                00120000
120100                        FILLER2-TM  OF DCLXXXATION                00120100
120200** Obsolete fields                                                00120200
120300                                                                  00120300
120400     IF ORD-PROCNG-CTOF-TM OF P-DDDTLO01 = SPACES                 00120400
120500       MOVE WS-NULL-TM TO ORD-PROCNG-CTOF-TM OF P-DDDTLO01        00120500
120600     END-IF                                                       00120600
120700     MOVE ORD-PROCNG-CTOF-TM OF P-DDDTLO01                        00120700
120800       TO ORD-PROCNG-CTOF-TM OF DCLXXXATION                       00120800
120900                                                                  00120900
121000     PERFORM 112-CONVERT-TM-TO-TS                                 00121000
121100     .                                                            00121100
121200                                                                  00121200
121300** ===========================================================    00121300
121400** CONVERTING TIME X(8) TO TIME STAMP X(26)                       00121400
121500** ===========================================================    00121500
121600 112-CONVERT-TM-TO-TS.                                            00121600
121700     IF (YYYN005A-ORACLE OR EXIT-PUT-MODIFY-ROW                   00121700
121800         OR EXIT-PUT-INSERT-ROW)                                  00121800
121900       INITIALIZE MMMC0291-INPUT-TM                               00121900
122000                  MMMC0291-INPUT-TS                               00122000
122100                                                                  00122100
122200       MOVE ORD-PROCNG-CTOF-TM OF DCLXXXATION                     00122200
122300         TO WS-TIME-INOUT-CONV(1)                                 00122300
122400       MOVE FILLER1-TM  OF DCLXXXATION                            00122400
122500         TO WS-TIME-INOUT-CONV(2)                                 00122500
122600       MOVE FILLER2-TM  OF DCLXXXATION                            00122600
122700         TO WS-TIME-INOUT-CONV(3)                                 00122700
122800                                                                  00122800
122900       SET  MMMC0291-CVT-TM-TO-TS  TO TRUE                        00122900
123000       CALL WS-MMMS0291-PGM USING                                 00123000
123100                          XXXN001A                                00123100
123200                          MMMC0291                                00123200
123300                                                                  00123300
123400       IF SUCCESS                                                 00123400
123500         MOVE WS-TIMSTAMP-INOUT-CONV(1)                           00123500
123600           TO WS-ORD-PROCNG-CTOF-TM                               00123600
123700         MOVE WS-TIMSTAMP-INOUT-CONV(2)                           00123700
123800           TO WS-FILLER1-TS                                       00123800
123900         MOVE WS-TIMSTAMP-INOUT-CONV(3)                           00123900
124000           TO WS-FILLER2-TS                                       00124000
124100       END-IF                                                     00124100
124200     ELSE                                                         00124200
124300       MOVE ORD-PROCNG-CTOF-TM OF DCLXXXATION                     00124300
124400         TO WS-ORD-PROCNG-CTOF-TM                                 00124400
124500       MOVE FILLER1-TM  OF DCLXXXATION TO WS-FILLER1-TS           00124500
124600       MOVE FILLER2-TM  OF DCLXXXATION TO WS-FILLER2-TS           00124600
124700     END-IF                                                       00124700
124800     .                                                            00124800
124900                                                                  00124900
125000                                                                  00125000
125100** ===========================================================    00125100
125200** CONNECTING TO ORACLE DATABASE                                  00125200
125300** ===========================================================    00125300
125400 115-CONNECT-TO-ORACLE.                                           00125400
125500     CALL Z-ORA-CONNECT USING XXXN001A                            00125500
125600                              SQLCA                               00125600
125700     IF NOT SUCCESS                                               00125700
125800       MOVE SQLCODE TO WS-SQLCODE                                 00125800
125900       MOVE SPACES  TO IS-RTRN-MSG-TXT                            00125900
126000       STRING 'NNNS0487 - Error connecting to Oracle. Sqlcode ='  00126000
126100               WS-SQLCODE                                         00126100
126200               DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT             00126200
126300     END-IF                                                       00126300
126400     .                                                            00126400
126500                                                                  00126500
126600                                                                  00126600
126700 116-EDIT-SHIP-DAYS.                                              00126700
126800     IF NOT SHIPS-MON                                             00126800
126900       SET DOES-NOT-SHIP-MON TO TRUE                              00126900
127000     END-IF                                                       00127000
127100     IF NOT SHIPS-TUE                                             00127100
127200       SET DOES-NOT-SHIP-TUE TO TRUE                              00127200
127300     END-IF                                                       00127300
127400     IF NOT SHIPS-WED                                             00127400
127500       SET DOES-NOT-SHIP-WED TO TRUE                              00127500
127600     END-IF                                                       00127600
127700     IF NOT SHIPS-THU                                             00127700
127800       SET DOES-NOT-SHIP-THU TO TRUE                              00127800
127900     END-IF                                                       00127900
128000     IF NOT SHIPS-FRI                                             00128000
128100       SET DOES-NOT-SHIP-FRI TO TRUE                              00128100
128200     END-IF                                                       00128200
128300     IF NOT SHIPS-SAT                                             00128300
128400       SET DOES-NOT-SHIP-SAT TO TRUE                              00128400
128500     END-IF                                                       00128500
128600     IF NOT SHIPS-SUN                                             00128600
128700       SET DOES-NOT-SHIP-SUN TO TRUE                              00128700
128800     END-IF                                                       00128800
128900     .                                                            00128900
129000                                                                  00129000
129100                                                                  00129100
129200* ================================================================00129200
129300* Stuff to do on exit.                                            00129300
129400* ================================================================00129400
129500 120-EXIT-STUFF.                                                  00129500
129600     IF SUCCESS                                                   00129600
129700       IF NOT EXIT-CLOSE-CURSOR                                   00129700
129800         PERFORM 130-MOVE-DCL-2-PDA-FIELDS                        00129800
129900       END-IF                                                     00129900
130000       ADD WS-CHECKPOINT-INC TO YYYN005A-CHKPT-CNT                00130000
130100     END-IF                                                       00130100
130200     IF (YYYN005A-ORACLE       OR EXIT-PUT-INSERT-ROW             00130200
130300         OR EXIT-PUT-PURGE-ROW OR EXIT-PUT-MODIFY-ROW)            00130300
130400       PERFORM 125-CONNECT-TO-DB2                                 00130400
130500     END-IF                                                       00130500
130600     MOVE SQLCODE TO DB2-SQL-CODE                                 00130600
130700     .                                                            00130700
130800                                                                  00130800
130900                                                                  00130900
131000** ===========================================================    00131000
131100** CONNECTING TO DB2 DATABASE                                     00131100
131200** ===========================================================    00131200
131300 125-CONNECT-TO-DB2.                                              00131300
131400     CALL Z-DB2-CONNECT         USING XXXN001A                    00131400
131500                                      SQLCA                       00131500
131600     .                                                            00131600
131700                                                                  00131700
131800                                                                  00131800
131900* ================================================================00131900
132000* Move the elementary fields of the DCL to parameter data area.   00132000
132100* ================================================================00132100
132200 130-MOVE-DCL-2-PDA-FIELDS.                                       00132200
132300     MOVE LOC-TYP-CD OF DCLXXXATION TO LOC-TYP-CD OF P-DDDTLO01   00132300
132400     MOVE LOC-NBR OF DCLXXXATION TO LOC-NBR OF P-DDDTLO01         00132400
132500     MOVE LOC-NM OF DCLXXXATION TO LOC-NM OF P-DDDTLO01           00132500
132600     MOVE LOC-ABB OF DCLXXXATION TO LOC-ABB OF P-DDDTLO01         00132600
132700     MOVE LGL-LOC-NAM OF DCLXXXATION TO LGL-LOC-NAM OF P-DDDTLO01 00132700
132800     MOVE PRIM-CONTACT-NM OF DCLXXXATION                          00132800
132900       TO PRIM-CONTACT-NM OF P-DDDTLO01                           00132900
133000     MOVE PRIM-ADR-1 OF DCLXXXATION TO PRIM-ADR-1 OF P-DDDTLO01   00133000
133100     MOVE PRIM-ADR-2 OF DCLXXXATION TO PRIM-ADR-2 OF P-DDDTLO01   00133100
133200     MOVE PRIM-ADR-3 OF DCLXXXATION TO PRIM-ADR-3 OF P-DDDTLO01   00133200
133300     MOVE PRIM-ADR-4 OF DCLXXXATION TO PRIM-ADR-4 OF P-DDDTLO01   00133300
133400     MOVE PRIM-CITY OF DCLXXXATION TO PRIM-CITY OF P-DDDTLO01     00133400
133500     MOVE PRIM-CITY-ID OF DCLXXXATION                             00133500
133600       TO PRIM-CITY-ID OF P-DDDTLO01                              00133600
133700     MOVE PRIM-STATE-CD OF DCLXXXATION                            00133700
133800       TO PRIM-STATE-CD OF P-DDDTLO01                             00133800
133900     MOVE PRIM-ZIP5-CD OF DCLXXXATION                             00133900
134000       TO PRIM-ZIP5-CD OF P-DDDTLO01                              00134000
134100     MOVE PRIM-ZIP4-CD OF DCLXXXATION                             00134100
134200       TO PRIM-ZIP4-CD OF P-DDDTLO01                              00134200
134300     MOVE PRIM-PHN-CNTRY-CD OF DCLXXXATION                        00134300
134400       TO PRIM-PHN-CNTRY-CD OF P-DDDTLO01                         00134400
134500     MOVE PRIM-AREA-CD OF DCLXXXATION                             00134500
134600       TO PRIM-AREA-CD OF P-DDDTLO01                              00134600
134700     MOVE PRIM-PHONE-NBR OF DCLXXXATION                           00134700
134800       TO PRIM-PHONE-NBR OF P-DDDTLO01                            00134800
134900     MOVE PRIM-CNTRY-NM OF DCLXXXATION                            00134900
135000       TO PRIM-CNTRY-NM OF P-DDDTLO01                             00135000
135100     MOVE PRIM-CNTRY-ABB OF DCLXXXATION                           00135100
135200       TO PRIM-CNTRY-ABB OF P-DDDTLO01                            00135200
135300     MOVE SEC-LOC-NM OF DCLXXXATION TO SEC-LOC-NM OF P-DDDTLO01   00135300
135400     MOVE SEC-CONTACT-NM OF DCLXXXATION                           00135400
135500       TO SEC-CONTACT-NM OF P-DDDTLO01                            00135500
135600     MOVE SEC-ADR-1 OF DCLXXXATION TO SEC-ADR-1 OF P-DDDTLO01     00135600
135700     MOVE SEC-ADR-2 OF DCLXXXATION TO SEC-ADR-2 OF P-DDDTLO01     00135700
135800     MOVE SEC-ADR-3 OF DCLXXXATION TO SEC-ADR-3 OF P-DDDTLO01     00135800
135900     MOVE SEC-ADR-4 OF DCLXXXATION TO SEC-ADR-4 OF P-DDDTLO01     00135900
136000     MOVE SEC-CITY OF DCLXXXATION TO SEC-CITY OF P-DDDTLO01       00136000
136100     MOVE SEC-STATE-CD OF DCLXXXATION                             00136100
136200       TO SEC-STATE-CD OF P-DDDTLO01                              00136200
136300     MOVE SEC-ZIP5-CD OF DCLXXXATION TO SEC-ZIP5-CD OF P-DDDTLO01 00136300
136400     MOVE SEC-ZIP4-CD OF DCLXXXATION TO SEC-ZIP4-CD OF P-DDDTLO01 00136400
136500     MOVE SEC-PHN-CNTRY-CD OF DCLXXXATION                         00136500
136600       TO SEC-PHN-CNTRY-CD OF P-DDDTLO01                          00136600
136700     MOVE SEC-AREA-CD OF DCLXXXATION TO SEC-AREA-CD OF P-DDDTLO01 00136700
136800     MOVE SEC-PHONE-NBR OF DCLXXXATION                            00136800
136900       TO SEC-PHONE-NBR OF P-DDDTLO01                             00136900
137000     MOVE SEC-CNTRY-NM OF DCLXXXATION                             00137000
137100       TO SEC-CNTRY-NM OF P-DDDTLO01                              00137100
137200     MOVE SEC-CNTRY-ABB OF DCLXXXATION                            00137200
137300       TO SEC-CNTRY-ABB OF P-DDDTLO01                             00137300
137400     MOVE MAIL-TO-LOC-NM OF DCLXXXATION                           00137400
137500       TO MAIL-TO-LOC-NM OF P-DDDTLO01                            00137500
137600     MOVE MAIL-TO-CNTCT-NM OF DCLXXXATION                         00137600
137700       TO MAIL-TO-CNTCT-NM OF P-DDDTLO01                          00137700
137800     MOVE MAIL-TO-ADR-1 OF DCLXXXATION                            00137800
137900       TO MAIL-TO-ADR-1 OF P-DDDTLO01                             00137900
138000     MOVE MAIL-TO-ADR-2 OF DCLXXXATION                            00138000
138100       TO MAIL-TO-ADR-2 OF P-DDDTLO01                             00138100
138200     MOVE MAIL-TO-ADR-3 OF DCLXXXATION                            00138200
138300       TO MAIL-TO-ADR-3 OF P-DDDTLO01                             00138300
138400     MOVE MAIL-TO-ADR-4 OF DCLXXXATION                            00138400
138500       TO MAIL-TO-ADR-4 OF P-DDDTLO01                             00138500
138600     MOVE MAIL-TO-CITY OF DCLXXXATION                             00138600
138700       TO MAIL-TO-CITY OF P-DDDTLO01                              00138700
138800     MOVE MAIL-TO-STATE-CD OF DCLXXXATION                         00138800
138900       TO MAIL-TO-STATE-CD OF P-DDDTLO01                          00138900
139000     MOVE MAIL-TO-ZIP5-CD OF DCLXXXATION                          00139000
139100       TO MAIL-TO-ZIP5-CD OF P-DDDTLO01                           00139100
139200     MOVE MAIL-TO-ZIP4-CD OF DCLXXXATION                          00139200
139300       TO MAIL-TO-ZIP4-CD OF P-DDDTLO01                           00139300
139400     MOVE MAIL-PHN-CNTRY-CD OF DCLXXXATION                        00139400
139500       TO MAIL-PHN-CNTRY-CD OF P-DDDTLO01                         00139500
139600     MOVE MAIL-TO-AREA-CD OF DCLXXXATION                          00139600
139700       TO MAIL-TO-AREA-CD OF P-DDDTLO01                           00139700
139800     MOVE MAIL-TO-PHONE-NBR OF DCLXXXATION                        00139800
139900       TO MAIL-TO-PHONE-NBR OF P-DDDTLO01                         00139900
140000     MOVE MAIL-TO-CNTRY-NM OF DCLXXXATION                         00140000
140100       TO MAIL-TO-CNTRY-NM OF P-DDDTLO01                          00140100
140200     MOVE MAIL-TO-CNTRY-AB OF DCLXXXATION                         00140200
140300       TO MAIL-TO-CNTRY-AB OF P-DDDTLO01                          00140300
140400     MOVE CURR-FAX-ID OF DCLXXXATION TO CURR-FAX-ID OF P-DDDTLO01 00140400
140500                                                                  00140500
140600     MOVE ADDED-DT OF DCLXXXATION TO ADDED-DT OF P-DDDTLO01       00140600
140700     MOVE DELETE-DT OF DCLXXXATION TO DELETE-DT OF P-DDDTLO01     00140700
140800     MOVE OPENED-DT OF DCLXXXATION TO OPENED-DT OF P-DDDTLO01     00140800
140900     MOVE CLOSED-DT OF DCLXXXATION TO CLOSED-DT OF P-DDDTLO01     00140900
141000     MOVE INACTIVE-DT OF DCLXXXATION TO INACTIVE-DT OF P-DDDTLO01 00141000
141100     IF ADDED-DT OF P-DDDTLO01 = K-DEF-DT                         00141100
141200       MOVE SPACES TO ADDED-DT OF P-DDDTLO01                      00141200
141300     END-IF                                                       00141300
141400     IF DELETE-DT OF P-DDDTLO01 = K-DEF-DT                        00141400
141500       MOVE SPACES TO DELETE-DT OF P-DDDTLO01                     00141500
141600     END-IF                                                       00141600
141700     IF OPENED-DT OF P-DDDTLO01 = K-DEF-DT                        00141700
141800       MOVE SPACES TO OPENED-DT OF P-DDDTLO01                     00141800
141900     END-IF                                                       00141900
142000     IF CLOSED-DT OF P-DDDTLO01 = K-DEF-DT                        00142000
142100       MOVE SPACES TO CLOSED-DT OF P-DDDTLO01                     00142100
142200     END-IF                                                       00142200
142300     IF INACTIVE-DT OF P-DDDTLO01 = K-DEF-DT                      00142300
142400       MOVE SPACES TO INACTIVE-DT OF P-DDDTLO01                   00142400
142500     END-IF                                                       00142500
142600                                                                  00142600
142700     MOVE INACTIVE-SW OF DCLXXXATION TO INACTIVE-SW OF P-DDDTLO01 00142700
142800     MOVE AP-NBR OF DCLXXXATION TO AP-NBR OF P-DDDTLO01           00142800
142900     MOVE AP-TYP-CD OF DCLXXXATION TO AP-TYP-CD OF P-DDDTLO01     00142900
143000                                                                  00143000
143100     MOVE LST-UPDT-TS OF DCLXXXATION TO LST-UPDT-TS OF P-DDDTLO01 00143100
143200     MOVE LST-UPDT-USR-ID OF DCLXXXATION                          00143200
143300       TO LST-UPDT-USR-ID OF P-DDDTLO01                           00143300
143400     MOVE PRIM-EMAIL-ID    OF DCLXXXATION                         00143400
143500       TO PRIM-EMAIL-ID    OF P-DDDTLO01                          00143500
143600     MOVE SECY-EMAIL-ID    OF DCLXXXATION                         00143600
143700       TO SECY-EMAIL-ID    OF P-DDDTLO01                          00143700
143800     MOVE MAIL-TO-EMAIL-ID OF DCLXXXATION                         00143800
143900       TO MAIL-TO-EMAIL-ID OF P-DDDTLO01                          00143900
144000     MOVE FAC-ID           OF DCLXXXATION                         00144000
144100       TO FAC-ID           OF P-DDDTLO01                          00144100
144200     MOVE ORG-ID           OF DCLXXXATION                         00144200
144300       TO ORG-ID           OF P-DDDTLO01                          00144300
144400     MOVE B2B-PRIM-RTNG-ID OF DCLXXXATION                         00144400
144500       TO B2B-PRIM-RTNG-ID OF P-DDDTLO01                          00144500
144600     MOVE PRIM-CNTY-TXT    OF DCLXXXATION                         00144600
144700       TO PRIM-CNTY-TXT    OF P-DDDTLO01                          00144700
144800     MOVE SECY-CNTY-TXT    OF DCLXXXATION                         00144800
144900       TO SECY-CNTY-TXT    OF P-DDDTLO01                          00144900
145000     MOVE MAIL-TO-CNTY-TXT OF DCLXXXATION                         00145000
145100       TO MAIL-TO-CNTY-TXT OF P-DDDTLO01                          00145100
145200                                                                  00145200
145300     MOVE DIR-SHP-LOC-SW   OF DCLXXXATION                         00145300
145400       TO DIR-SHP-LOC-SW   OF P-DDDTLO01                          00145400
145500     IF NOT LOC-IS-DIRECT-SHIP OF P-DDDTLO01                      00145500
145600       SET LOC-IS-NOT-DIRECT-SHIP OF P-DDDTLO01 TO TRUE           00145600
145700     END-IF                                                       00145700
145800                                                                  00145800
145900     MOVE LOC-ORD-PROCNG-DD  OF DCLXXXATION                       00145900
146000       TO LOC-ORD-PROCNG-DD  OF P-DDDTLO01                        00146000
146100                                                                  00146100
146200*    MOVE WS-CURRENT-DATE-DATA                                    00146200
146300*      TO WS-CURRENT-DATE-DATA1                                   00146300
146400*    MOVE WS-CURRENT-TIME  OF   WS-CURRENT-DATE-DATA1             00146400
146500*      TO ORD-PROCNG-CTOF-TM OF P-DDDTLO01                        00146500
146600*     MOVE ORD-PROCNG-CTOF-TM OF DCLXXXATION                      00146600
146700*       TO ORD-PROCNG-CTOF-TM OF P-DDDTLO01                       00146700
146800                                                                  00146800
146900     IF ORD-PROCNG-CTOF-TM OF P-DDDTLO01 = WS-NULL-TM             00146900
147000       MOVE SPACES TO ORD-PROCNG-CTOF-TM OF P-DDDTLO01            00147000
147100     END-IF                                                       00147100
147200                                                                  00147200
147300     MOVE SCH-SHP-DD-TXT     OF DCLXXXATION                       00147300
147400       TO SCH-SHP-DD-TXT     OF P-DDDTLO01                        00147400
147500     PERFORM 116-EDIT-SHIP-DAYS                                   00147500
147600     MOVE ORD-LEAD-TM-DD     OF DCLXXXATION                       00147600
147700       TO ORD-LEAD-TM-DD     OF P-DDDTLO01                        00147700
147800                                                                  00147800
147900     MOVE ORD-BUFFER-TM-DD   OF DCLXXXATION                       00147900
148000       TO ORD-BUFFER-TM-DD   OF P-DDDTLO01                        00148000
148100                                                                  00148100
148200     PERFORM 132-CONVERT-TS-TO-TM                                 00148200
148300     IF SUCCESS                                                   00148300
148400       MOVE ORD-PROCNG-CTOF-TM OF DCLXXXATION                     00148400
148500         TO ORD-PROCNG-CTOF-TM OF P-DDDTLO01                      00148500
148600     END-IF                                                       00148600
148700                                                                  00148700
148800     IF LOC-TYP-CD OF DCLXXXATION = K-DSD-VEND-LOC-TYPE           00148800
148900     AND SUCCESS AND YYYN005A-CICS-ENV                            00148900
149000       PERFORM 125-CONNECT-TO-DB2                                 00149000
149100       IF SUCCESS                                                 00149100
149200          PERFORM 10200-CHECK-DSV-LOC-SUB-TYP                     00149200
149300       END-IF                                                     00149300
149400     END-IF                                                       00149400
149500     .                                                            00149500
149600                                                                  00149600
149700                                                                  00149700
149800 132-CONVERT-TS-TO-TM.                                            00149800
149900     IF (YYYN005A-ORACLE OR EXIT-PUT-MODIFY-ROW                   00149900
150000         OR EXIT-PUT-INSERT-ROW)                                  00150000
150100       INITIALIZE MMMC0291-INPUT-TM                               00150100
150200                  MMMC0291-INPUT-TS                               00150200
150300                                                                  00150300
150400       MOVE WS-ORD-PROCNG-CTOF-TM                                 00150400
150500         TO WS-TIMSTAMP-INOUT-CONV(1)                             00150500
150600       MOVE WS-FILLER1-TS                                         00150600
150700         TO WS-TIMSTAMP-INOUT-CONV(2)                             00150700
150800       MOVE WS-FILLER2-TS                                         00150800
150900         TO WS-TIMSTAMP-INOUT-CONV(3)                             00150900
151000                                                                  00151000
151100       SET MMMC0291-CVT-TS-TO-TM  TO TRUE                         00151100
151200       CALL WS-MMMS0291-PGM USING                                 00151200
151300                          XXXN001A                                00151300
151400                          MMMC0291                                00151400
151500       IF SUCCESS                                                 00151500
151600         MOVE WS-TIME-INOUT-CONV(1)                               00151600
151700           TO ORD-PROCNG-CTOF-TM OF DCLXXXATION                   00151700
151800         MOVE WS-TIMSTAMP-INOUT-CONV(2)                           00151800
151900           TO FILLER1-TM OF DCLXXXATION                           00151900
152000         MOVE WS-TIMSTAMP-INOUT-CONV(3)                           00152000
152100           TO FILLER2-TM OF DCLXXXATION                           00152100
152200       END-IF                                                     00152200
152300     END-IF                                                       00152300
152400     .                                                            00152400
152500                                                                  00152500
152600                                                                  00152600
152700* ================================================================00152700
152800* Code required to do static sql including cursor, select, fetch, 00152800
152900* update, insert, and delete operations.                          00152900
153000* ================================================================00153000
153100 1000-EXIT-OPEN-CURSOR.                                           00153100
153200     EVALUATE TRUE                                                00153200
153300       WHEN DDDXLO01                                              00153300
153400         EXEC SQL                                                 00153400
153500           OPEN DDDXLO01                                          00153500
153600         END-EXEC                                                 00153600
153700       WHEN DDDXLO02                                              00153700
153800         EXEC SQL                                                 00153800
153900           OPEN DDDXLO02                                          00153900
154000         END-EXEC                                                 00154000
154100       WHEN DDDXLO03                                              00154100
154200         EXEC SQL                                                 00154200
154300           OPEN DDDXLO03                                          00154300
154400         END-EXEC                                                 00154400
154500       WHEN DDDXLO04                                              00154500
154600         EXEC SQL                                                 00154600
154700           OPEN DDDXLO04                                          00154700
154800         END-EXEC                                                 00154800
154900       WHEN DDDXLO05                                              00154900
155000         EXEC SQL                                                 00155000
155100           OPEN DDDXLO05                                          00155100
155200         END-EXEC                                                 00155200
155300       WHEN DDDXLO06                                              00155300
155400         EXEC SQL                                                 00155400
155500           OPEN DDDXLO06                                          00155500
155600         END-EXEC                                                 00155600
155700       WHEN DDDXLO07                                              00155700
155800         EXEC SQL                                                 00155800
155900           OPEN DDDXLO07                                          00155900
156000         END-EXEC                                                 00156000
156100       WHEN OTHER                                                 00156100
156200         SET FAILURE TO TRUE                                      00156200
156300         MOVE 'NNNS0487 - Invalid open cursor ID.'                00156300
156400           TO IS-RTRN-MSG-TXT OF XXXN001A                         00156400
156500     END-EVALUATE                                                 00156500
156600     .                                                            00156600
156700                                                                  00156700
156800                                                                  00156800
156900 1100-EXIT-CLOSE-CURSOR.                                          00156900
157000     EVALUATE TRUE                                                00157000
157100       WHEN DDDXLO01                                              00157100
157200         EXEC SQL                                                 00157200
157300           CLOSE DDDXLO01                                         00157300
157400         END-EXEC                                                 00157400
157500       WHEN DDDXLO02                                              00157500
157600         EXEC SQL                                                 00157600
157700           CLOSE DDDXLO02                                         00157700
157800         END-EXEC                                                 00157800
157900       WHEN DDDXLO03                                              00157900
158000         EXEC SQL                                                 00158000
158100           CLOSE DDDXLO03                                         00158100
158200         END-EXEC                                                 00158200
158300       WHEN DDDXLO04                                              00158300
158400         EXEC SQL                                                 00158400
158500           CLOSE DDDXLO04                                         00158500
158600         END-EXEC                                                 00158600
158700       WHEN DDDXLO05                                              00158700
158800         EXEC SQL                                                 00158800
158900           CLOSE DDDXLO05                                         00158900
159000         END-EXEC                                                 00159000
159100       WHEN DDDXLO06                                              00159100
159200         EXEC SQL                                                 00159200
159300           CLOSE DDDXLO06                                         00159300
159400         END-EXEC                                                 00159400
159500       WHEN DDDXLO07                                              00159500
159600         EXEC SQL                                                 00159600
159700           CLOSE DDDXLO07                                         00159700
159800         END-EXEC                                                 00159800
159900       WHEN OTHER                                                 00159900
160000         SET FAILURE TO TRUE                                      00160000
160100         MOVE 'NNNS0487 - Invalid close cursor ID.'               00160100
160200           TO IS-RTRN-MSG-TXT OF XXXN001A                         00160200
160300     END-EVALUATE                                                 00160300
160400     .                                                            00160400
160500                                                                  00160500
160600                                                                  00160600
160700 1200-EXIT-GET-UNIQUE-ROW.                                        00160700
160800     EXEC SQL                                                     00160800
160900         SELECT LOC_TYP_CD,                                       00160900
161000                LOC_NBR,                                          00161000
161100                LOC_NM,                                           00161100
161200                LOC_ABB,                                          00161200
161300                LGL_LOC_NAM,                                      00161300
161400                PRIM_CONTACT_NM,                                  00161400
161500                PRIM_ADR_1,                                       00161500
161600                PRIM_ADR_2,                                       00161600
161700                PRIM_ADR_3,                                       00161700
161800                PRIM_ADR_4,                                       00161800
161900                PRIM_CITY,                                        00161900
162000                PRIM_CITY_ID,                                     00162000
162100                PRIM_STATE_CD,                                    00162100
162200                PRIM_ZIP5_CD,                                     00162200
162300                PRIM_ZIP4_CD,                                     00162300
162400                PRIM_PHN_CNTRY_CD,                                00162400
162500                PRIM_AREA_CD,                                     00162500
162600                PRIM_PHONE_NBR,                                   00162600
162700                PRIM_CNTRY_NM,                                    00162700
162800                PRIM_CNTRY_ABB,                                   00162800
162900                SEC_LOC_NM,                                       00162900
163000                SEC_CONTACT_NM,                                   00163000
163100                SEC_ADR_1,                                        00163100
163200                SEC_ADR_2,                                        00163200
163300                SEC_ADR_3,                                        00163300
163400                SEC_ADR_4,                                        00163400
163500                SEC_CITY,                                         00163500
163600                SEC_STATE_CD,                                     00163600
163700                SEC_ZIP5_CD,                                      00163700
163800                SEC_ZIP4_CD,                                      00163800
163900                SEC_PHN_CNTRY_CD,                                 00163900
164000                SEC_AREA_CD,                                      00164000
164100                SEC_PHONE_NBR,                                    00164100
164200                SEC_CNTRY_NM,                                     00164200
164300                SEC_CNTRY_ABB,                                    00164300
164400                MAIL_TO_LOC_NM,                                   00164400
164500                MAIL_TO_CNTCT_NM,                                 00164500
164600                MAIL_TO_ADR_1,                                    00164600
164700                MAIL_TO_ADR_2,                                    00164700
164800                MAIL_TO_ADR_3,                                    00164800
164900                MAIL_TO_ADR_4,                                    00164900
165000                MAIL_TO_CITY,                                     00165000
165100                MAIL_TO_STATE_CD,                                 00165100
165200                MAIL_TO_ZIP5_CD,                                  00165200
165300                MAIL_TO_ZIP4_CD,                                  00165300
165400                MAIL_PHN_CNTRY_CD,                                00165400
165500                MAIL_TO_AREA_CD,                                  00165500
165600                MAIL_TO_PHONE_NBR,                                00165600
165700                MAIL_TO_CNTRY_NM,                                 00165700
165800                MAIL_TO_CNTRY_AB,                                 00165800
165900                CURR_FAX_ID,                                      00165900
166000                ADDED_DT,                                         00166000
166100                DELETE_DT,                                        00166100
166200                OPENED_DT,                                        00166200
166300                CLOSED_DT,                                        00166300
166400                INACTIVE_SW,                                      00166400
166500                INACTIVE_DT,                                      00166500
166600                AP_NBR,                                           00166600
166700                AP_TYP_CD,                                        00166700
166800                LST_UPDT_TS,                                      00166800
166900                LST_UPDT_USR_ID,                                  00166900
167000                PRIM_EMAIL_ID  ,                                  00167000
167100                SECY_EMAIL_ID  ,                                  00167100
167200                MAIL_TO_EMAIL_ID,                                 00167200
167300                FAC_ID,                                           00167300
167400                ORG_ID,                                           00167400
167500                B2B_PRIM_RTNG_ID,                                 00167500
167600                PRIM_CNTY_TXT,                                    00167600
167700                SECY_CNTY_TXT,                                    00167700
167800                MAIL_TO_CNTY_TXT,                                 00167800
167900                DIR_SHP_LOC_SW,                                   00167900
168000                LOC_ORD_PROCNG_DD,                                00168000
168100                ORD_PROCNG_CTOF_TM,                               00168100
168200                SCH_SHP_DD_TXT,                                   00168200
168300                FILLER1_TM,                                       00168300
168400                FILLER2_TM,                                       00168400
168500                ORD_LEAD_TM_DD,                                   00168500
168600                ORD_BUFFER_TM_DD                                  00168600
168700         INTO   :DCLXXXATION.LOC-TYP-CD,                          00168700
168800                :DCLXXXATION.LOC-NBR,                             00168800
168900                :DCLXXXATION.LOC-NM,                              00168900
169000                :DCLXXXATION.LOC-ABB,                             00169000
169100                :DCLXXXATION.LGL-LOC-NAM,                         00169100
169200                :DCLXXXATION.PRIM-CONTACT-NM,                     00169200
169300                :DCLXXXATION.PRIM-ADR-1,                          00169300
169400                :DCLXXXATION.PRIM-ADR-2,                          00169400
169500                :DCLXXXATION.PRIM-ADR-3,                          00169500
169600                :DCLXXXATION.PRIM-ADR-4,                          00169600
169700                :DCLXXXATION.PRIM-CITY,                           00169700
169800                :DCLXXXATION.PRIM-CITY-ID,                        00169800
169900                :DCLXXXATION.PRIM-STATE-CD,                       00169900
170000                :DCLXXXATION.PRIM-ZIP5-CD,                        00170000
170100                :DCLXXXATION.PRIM-ZIP4-CD,                        00170100
170200                :DCLXXXATION.PRIM-PHN-CNTRY-CD,                   00170200
170300                :DCLXXXATION.PRIM-AREA-CD,                        00170300
170400                :DCLXXXATION.PRIM-PHONE-NBR,                      00170400
170500                :DCLXXXATION.PRIM-CNTRY-NM,                       00170500
170600                :DCLXXXATION.PRIM-CNTRY-ABB,                      00170600
170700                :DCLXXXATION.SEC-LOC-NM,                          00170700
170800                :DCLXXXATION.SEC-CONTACT-NM,                      00170800
170900                :DCLXXXATION.SEC-ADR-1,                           00170900
171000                :DCLXXXATION.SEC-ADR-2,                           00171000
171100                :DCLXXXATION.SEC-ADR-3,                           00171100
171200                :DCLXXXATION.SEC-ADR-4,                           00171200
171300                :DCLXXXATION.SEC-CITY,                            00171300
171400                :DCLXXXATION.SEC-STATE-CD,                        00171400
171500                :DCLXXXATION.SEC-ZIP5-CD,                         00171500
171600                :DCLXXXATION.SEC-ZIP4-CD,                         00171600
171700                :DCLXXXATION.SEC-PHN-CNTRY-CD,                    00171700
171800                :DCLXXXATION.SEC-AREA-CD,                         00171800
171900                :DCLXXXATION.SEC-PHONE-NBR,                       00171900
172000                :DCLXXXATION.SEC-CNTRY-NM,                        00172000
172100                :DCLXXXATION.SEC-CNTRY-ABB,                       00172100
172200                :DCLXXXATION.MAIL-TO-LOC-NM,                      00172200
172300                :DCLXXXATION.MAIL-TO-CNTCT-NM,                    00172300
172400                :DCLXXXATION.MAIL-TO-ADR-1,                       00172400
172500                :DCLXXXATION.MAIL-TO-ADR-2,                       00172500
172600                :DCLXXXATION.MAIL-TO-ADR-3,                       00172600
172700                :DCLXXXATION.MAIL-TO-ADR-4,                       00172700
172800                :DCLXXXATION.MAIL-TO-CITY,                        00172800
172900                :DCLXXXATION.MAIL-TO-STATE-CD,                    00172900
173000                :DCLXXXATION.MAIL-TO-ZIP5-CD,                     00173000
173100                :DCLXXXATION.MAIL-TO-ZIP4-CD,                     00173100
173200                :DCLXXXATION.MAIL-PHN-CNTRY-CD,                   00173200
173300                :DCLXXXATION.MAIL-TO-AREA-CD,                     00173300
173400                :DCLXXXATION.MAIL-TO-PHONE-NBR,                   00173400
173500                :DCLXXXATION.MAIL-TO-CNTRY-NM,                    00173500
173600                :DCLXXXATION.MAIL-TO-CNTRY-AB,                    00173600
173700                :DCLXXXATION.CURR-FAX-ID,                         00173700
173800                :DCLXXXATION.ADDED-DT,                            00173800
173900                :DCLXXXATION.DELETE-DT,                           00173900
174000                :DCLXXXATION.OPENED-DT,                           00174000
174100                :DCLXXXATION.CLOSED-DT,                           00174100
174200                :DCLXXXATION.INACTIVE-SW,                         00174200
174300                :DCLXXXATION.INACTIVE-DT,                         00174300
174400                :DCLXXXATION.AP-NBR:WS-AP-NBR-IND,                00174400
174500                :DCLXXXATION.AP-TYP-CD:WS-AP-TYP-CD-IND,          00174500
174600                :DCLXXXATION.LST-UPDT-TS,                         00174600
174700                :DCLXXXATION.LST-UPDT-USR-ID,                     00174700
174800                :DCLXXXATION.PRIM-EMAIL-ID,                       00174800
174900                :DCLXXXATION.SECY-EMAIL-ID,                       00174900
175000                :DCLXXXATION.MAIL-TO-EMAIL-ID,                    00175000
175100                :DCLXXXATION.FAC-ID,                              00175100
175200                :DCLXXXATION.ORG-ID,                              00175200
175300                :DCLXXXATION.B2B-PRIM-RTNG-ID,                    00175300
175400                :DCLXXXATION.PRIM-CNTY-TXT,                       00175400
175500                :DCLXXXATION.SECY-CNTY-TXT,                       00175500
175600                :DCLXXXATION.MAIL-TO-CNTY-TXT,                    00175600
175700                :DCLXXXATION.DIR-SHP-LOC-SW,                      00175700
175800                :DCLXXXATION.LOC-ORD-PROCNG-DD,                   00175800
175900                :WS-ORD-PROCNG-CTOF-TM,                           00175900
176000                :DCLXXXATION.SCH-SHP-DD-TXT,                      00176000
176100                :WS-FILLER1-TS,                                   00176100
176200                :WS-FILLER2-TS,                                   00176200
176300                :DCLXXXATION.ORD-LEAD-TM-DD,                      00176300
176400                :DCLXXXATION.ORD-BUFFER-TM-DD                     00176400
176500         FROM   XXXATION                                          00176500
176600         WHERE  LOC_TYP_CD = :DCLXXXATION.LOC-TYP-CD              00176600
176700         AND    LOC_NBR = :DCLXXXATION.LOC-NBR                    00176700
176800     END-EXEC                                                     00176800
176900                                                                  00176900
177000     PERFORM 1700-CHECK-NULL-COLUMNS                              00177000
177100     .                                                            00177100
177200                                                                  00177200
177300                                                                  00177300
177400 1300-EXIT-GET-NEXT-ROW.                                          00177400
177500     EVALUATE TRUE                                                00177500
177600       WHEN DDDXLO01                                              00177600
177700         PERFORM 1301-FETCH-DDDXLO01                              00177700
177800       WHEN DDDXLO02                                              00177800
177900         PERFORM 1302-FETCH-DDDXLO02                              00177900
178000       WHEN DDDXLO03                                              00178000
178100         PERFORM 1303-FETCH-DDDXLO03                              00178100
178200       WHEN DDDXLO04                                              00178200
178300         PERFORM 1304-FETCH-DDDXLO04                              00178300
178400       WHEN DDDXLO05                                              00178400
178500         PERFORM 1305-FETCH-DDDXLO05                              00178500
178600       WHEN DDDXLO06                                              00178600
178700         PERFORM 1306-FETCH-DDDXLO06                              00178700
178800       WHEN DDDXLO07                                              00178800
178900         PERFORM 1307-FETCH-DDDXLO07                              00178900
179000       WHEN OTHER                                                 00179000
179100         SET FAILURE TO TRUE                                      00179100
179200         MOVE 'NNNS0487 - Invalid fetch cursor ID.'               00179200
179300           TO IS-RTRN-MSG-TXT OF XXXN001A                         00179300
179400     END-EVALUATE                                                 00179400
179500                                                                  00179500
179600     PERFORM 1700-CHECK-NULL-COLUMNS                              00179600
179700     .                                                            00179700
179800                                                                  00179800
179900                                                                  00179900
180000 1301-FETCH-DDDXLO01.                                             00180000
180100     EXEC SQL                                                     00180100
180200         FETCH DDDXLO01                                           00180200
180300         INTO  :DCLXXXATION.LOC-TYP-CD,                           00180300
180400               :DCLXXXATION.LOC-NBR,                              00180400
180500               :DCLXXXATION.LOC-NM,                               00180500
180600               :DCLXXXATION.LOC-ABB,                              00180600
180700               :DCLXXXATION.LGL-LOC-NAM,                          00180700
180800               :DCLXXXATION.PRIM-CONTACT-NM,                      00180800
180900               :DCLXXXATION.PRIM-ADR-1,                           00180900
181000               :DCLXXXATION.PRIM-ADR-2,                           00181000
181100               :DCLXXXATION.PRIM-ADR-3,                           00181100
181200               :DCLXXXATION.PRIM-ADR-4,                           00181200
181300               :DCLXXXATION.PRIM-CITY,                            00181300
181400               :DCLXXXATION.PRIM-CITY-ID,                         00181400
181500               :DCLXXXATION.PRIM-STATE-CD,                        00181500
181600               :DCLXXXATION.PRIM-ZIP5-CD,                         00181600
181700               :DCLXXXATION.PRIM-ZIP4-CD,                         00181700
181800               :DCLXXXATION.PRIM-PHN-CNTRY-CD,                    00181800
181900               :DCLXXXATION.PRIM-AREA-CD,                         00181900
182000               :DCLXXXATION.PRIM-PHONE-NBR,                       00182000
182100               :DCLXXXATION.PRIM-CNTRY-NM,                        00182100
182200               :DCLXXXATION.PRIM-CNTRY-ABB,                       00182200
182300               :DCLXXXATION.SEC-LOC-NM,                           00182300
182400               :DCLXXXATION.SEC-CONTACT-NM,                       00182400
182500               :DCLXXXATION.SEC-ADR-1,                            00182500
182600               :DCLXXXATION.SEC-ADR-2,                            00182600
182700               :DCLXXXATION.SEC-ADR-3,                            00182700
182800               :DCLXXXATION.SEC-ADR-4,                            00182800
182900               :DCLXXXATION.SEC-CITY,                             00182900
183000               :DCLXXXATION.SEC-STATE-CD,                         00183000
183100               :DCLXXXATION.SEC-ZIP5-CD,                          00183100
183200               :DCLXXXATION.SEC-ZIP4-CD,                          00183200
183300               :DCLXXXATION.SEC-PHN-CNTRY-CD,                     00183300
183400               :DCLXXXATION.SEC-AREA-CD,                          00183400
183500               :DCLXXXATION.SEC-PHONE-NBR,                        00183500
183600               :DCLXXXATION.SEC-CNTRY-NM,                         00183600
183700               :DCLXXXATION.SEC-CNTRY-ABB,                        00183700
183800               :DCLXXXATION.MAIL-TO-LOC-NM,                       00183800
183900               :DCLXXXATION.MAIL-TO-CNTCT-NM,                     00183900
184000               :DCLXXXATION.MAIL-TO-ADR-1,                        00184000
184100               :DCLXXXATION.MAIL-TO-ADR-2,                        00184100
184200               :DCLXXXATION.MAIL-TO-ADR-3,                        00184200
184300               :DCLXXXATION.MAIL-TO-ADR-4,                        00184300
184400               :DCLXXXATION.MAIL-TO-CITY,                         00184400
184500               :DCLXXXATION.MAIL-TO-STATE-CD,                     00184500
184600               :DCLXXXATION.MAIL-TO-ZIP5-CD,                      00184600
184700               :DCLXXXATION.MAIL-TO-ZIP4-CD,                      00184700
184800               :DCLXXXATION.MAIL-PHN-CNTRY-CD,                    00184800
184900               :DCLXXXATION.MAIL-TO-AREA-CD,                      00184900
185000               :DCLXXXATION.MAIL-TO-PHONE-NBR,                    00185000
185100               :DCLXXXATION.MAIL-TO-CNTRY-NM,                     00185100
185200               :DCLXXXATION.MAIL-TO-CNTRY-AB,                     00185200
185300               :DCLXXXATION.CURR-FAX-ID,                          00185300
185400               :DCLXXXATION.ADDED-DT,                             00185400
185500               :DCLXXXATION.DELETE-DT,                            00185500
185600               :DCLXXXATION.OPENED-DT,                            00185600
185700               :DCLXXXATION.CLOSED-DT,                            00185700
185800               :DCLXXXATION.INACTIVE-SW,                          00185800
185900               :DCLXXXATION.INACTIVE-DT,                          00185900
186000               :DCLXXXATION.AP-NBR:WS-AP-NBR-IND,                 00186000
186100               :DCLXXXATION.AP-TYP-CD:WS-AP-TYP-CD-IND,           00186100
186200               :DCLXXXATION.LST-UPDT-TS,                          00186200
186300               :DCLXXXATION.LST-UPDT-USR-ID,                      00186300
186400               :DCLXXXATION.PRIM-EMAIL-ID,                        00186400
186500               :DCLXXXATION.SECY-EMAIL-ID,                        00186500
186600               :DCLXXXATION.MAIL-TO-EMAIL-ID,                     00186600
186700               :DCLXXXATION.FAC-ID,                               00186700
186800               :DCLXXXATION.ORG-ID,                               00186800
186900               :DCLXXXATION.B2B-PRIM-RTNG-ID,                     00186900
187000               :DCLXXXATION.PRIM-CNTY-TXT,                        00187000
187100               :DCLXXXATION.SECY-CNTY-TXT,                        00187100
187200               :DCLXXXATION.MAIL-TO-CNTY-TXT,                     00187200
187300               :DCLXXXATION.DIR-SHP-LOC-SW,                       00187300
187400               :DCLXXXATION.LOC-ORD-PROCNG-DD,                    00187400
187500               :WS-ORD-PROCNG-CTOF-TM,                            00187500
187600               :DCLXXXATION.SCH-SHP-DD-TXT,                       00187600
187700               :DCLXXXATION.ORD-LEAD-TM-DD,                       00187700
187800               :DCLXXXATION.ORD-BUFFER-TM-DD                      00187800
187900     END-EXEC                                                     00187900
188000     .                                                            00188000
188100                                                                  00188100
188200                                                                  00188200
188300 1302-FETCH-DDDXLO02.                                             00188300
188400     EXEC SQL                                                     00188400
188500         FETCH DDDXLO02                                           00188500
188600         INTO  :DCLXXXATION.LOC-TYP-CD,                           00188600
188700               :DCLXXXATION.LOC-NBR,                              00188700
188800               :DCLXXXATION.LOC-NM,                               00188800
188900               :DCLXXXATION.LOC-ABB,                              00188900
189000               :DCLXXXATION.LGL-LOC-NAM,                          00189000
189100               :DCLXXXATION.PRIM-CONTACT-NM,                      00189100
189200               :DCLXXXATION.PRIM-ADR-1,                           00189200
189300               :DCLXXXATION.PRIM-ADR-2,                           00189300
189400               :DCLXXXATION.PRIM-ADR-3,                           00189400
189500               :DCLXXXATION.PRIM-ADR-4,                           00189500
189600               :DCLXXXATION.PRIM-CITY,                            00189600
189700               :DCLXXXATION.PRIM-CITY-ID,                         00189700
189800               :DCLXXXATION.PRIM-STATE-CD,                        00189800
189900               :DCLXXXATION.PRIM-ZIP5-CD,                         00189900
190000               :DCLXXXATION.PRIM-ZIP4-CD,                         00190000
190100               :DCLXXXATION.PRIM-PHN-CNTRY-CD,                    00190100
190200               :DCLXXXATION.PRIM-AREA-CD,                         00190200
190300               :DCLXXXATION.PRIM-PHONE-NBR,                       00190300
190400               :DCLXXXATION.PRIM-CNTRY-NM,                        00190400
190500               :DCLXXXATION.PRIM-CNTRY-ABB,                       00190500
190600               :DCLXXXATION.SEC-LOC-NM,                           00190600
190700               :DCLXXXATION.SEC-CONTACT-NM,                       00190700
190800               :DCLXXXATION.SEC-ADR-1,                            00190800
190900               :DCLXXXATION.SEC-ADR-2,                            00190900
191000               :DCLXXXATION.SEC-ADR-3,                            00191000
191100               :DCLXXXATION.SEC-ADR-4,                            00191100
191200               :DCLXXXATION.SEC-CITY,                             00191200
191300               :DCLXXXATION.SEC-STATE-CD,                         00191300
191400               :DCLXXXATION.SEC-ZIP5-CD,                          00191400
191500               :DCLXXXATION.SEC-ZIP4-CD,                          00191500
191600               :DCLXXXATION.SEC-PHN-CNTRY-CD,                     00191600
191700               :DCLXXXATION.SEC-AREA-CD,                          00191700
191800               :DCLXXXATION.SEC-PHONE-NBR,                        00191800
191900               :DCLXXXATION.SEC-CNTRY-NM,                         00191900
192000               :DCLXXXATION.SEC-CNTRY-ABB,                        00192000
192100               :DCLXXXATION.MAIL-TO-LOC-NM,                       00192100
192200               :DCLXXXATION.MAIL-TO-CNTCT-NM,                     00192200
192300               :DCLXXXATION.MAIL-TO-ADR-1,                        00192300
192400               :DCLXXXATION.MAIL-TO-ADR-2,                        00192400
192500               :DCLXXXATION.MAIL-TO-ADR-3,                        00192500
192600               :DCLXXXATION.MAIL-TO-ADR-4,                        00192600
192700               :DCLXXXATION.MAIL-TO-CITY,                         00192700
192800               :DCLXXXATION.MAIL-TO-STATE-CD,                     00192800
192900               :DCLXXXATION.MAIL-TO-ZIP5-CD,                      00192900
193000               :DCLXXXATION.MAIL-TO-ZIP4-CD,                      00193000
193100               :DCLXXXATION.MAIL-PHN-CNTRY-CD,                    00193100
193200               :DCLXXXATION.MAIL-TO-AREA-CD,                      00193200
193300               :DCLXXXATION.MAIL-TO-PHONE-NBR,                    00193300
193400               :DCLXXXATION.MAIL-TO-CNTRY-NM,                     00193400
193500               :DCLXXXATION.MAIL-TO-CNTRY-AB,                     00193500
193600               :DCLXXXATION.CURR-FAX-ID,                          00193600
193700               :DCLXXXATION.ADDED-DT,                             00193700
193800               :DCLXXXATION.DELETE-DT,                            00193800
193900               :DCLXXXATION.OPENED-DT,                            00193900
194000               :DCLXXXATION.CLOSED-DT,                            00194000
194100               :DCLXXXATION.INACTIVE-SW,                          00194100
194200               :DCLXXXATION.INACTIVE-DT,                          00194200
194300               :DCLXXXATION.AP-NBR:WS-AP-NBR-IND,                 00194300
194400               :DCLXXXATION.AP-TYP-CD:WS-AP-TYP-CD-IND,           00194400
194500               :DCLXXXATION.LST-UPDT-TS,                          00194500
194600               :DCLXXXATION.LST-UPDT-USR-ID,                      00194600
194700               :DCLXXXATION.PRIM-EMAIL-ID,                        00194700
194800               :DCLXXXATION.SECY-EMAIL-ID,                        00194800
194900               :DCLXXXATION.MAIL-TO-EMAIL-ID,                     00194900
195000               :DCLXXXATION.FAC-ID,                               00195000
195100               :DCLXXXATION.ORG-ID,                               00195100
195200               :DCLXXXATION.B2B-PRIM-RTNG-ID,                     00195200
195300               :DCLXXXATION.PRIM-CNTY-TXT,                        00195300
195400               :DCLXXXATION.SECY-CNTY-TXT,                        00195400
195500               :DCLXXXATION.MAIL-TO-CNTY-TXT,                     00195500
195600               :DCLXXXATION.DIR-SHP-LOC-SW,                       00195600
195700               :DCLXXXATION.LOC-ORD-PROCNG-DD,                    00195700
195800               :WS-ORD-PROCNG-CTOF-TM,                            00195800
195900               :DCLXXXATION.SCH-SHP-DD-TXT,                       00195900
196000               :DCLXXXATION.ORD-LEAD-TM-DD,                       00196000
196100               :DCLXXXATION.ORD-BUFFER-TM-DD                      00196100
196200     END-EXEC                                                     00196200
196300     .                                                            00196300
196400                                                                  00196400
196500                                                                  00196500
196600 1303-FETCH-DDDXLO03.                                             00196600
196700     EXEC SQL                                                     00196700
196800         FETCH DDDXLO03                                           00196800
196900         INTO  :DCLXXXATION.LOC-TYP-CD,                           00196900
197000               :DCLXXXATION.LOC-NBR,                              00197000
197100               :DCLXXXATION.LOC-NM,                               00197100
197200               :DCLXXXATION.LOC-ABB,                              00197200
197300               :DCLXXXATION.LGL-LOC-NAM,                          00197300
197400               :DCLXXXATION.PRIM-CONTACT-NM,                      00197400
197500               :DCLXXXATION.PRIM-ADR-1,                           00197500
197600               :DCLXXXATION.PRIM-ADR-2,                           00197600
197700               :DCLXXXATION.PRIM-ADR-3,                           00197700
197800               :DCLXXXATION.PRIM-ADR-4,                           00197800
197900               :DCLXXXATION.PRIM-CITY,                            00197900
198000               :DCLXXXATION.PRIM-CITY-ID,                         00198000
198100               :DCLXXXATION.PRIM-STATE-CD,                        00198100
198200               :DCLXXXATION.PRIM-ZIP5-CD,                         00198200
198300               :DCLXXXATION.PRIM-ZIP4-CD,                         00198300
198400               :DCLXXXATION.PRIM-PHN-CNTRY-CD,                    00198400
198500               :DCLXXXATION.PRIM-AREA-CD,                         00198500
198600               :DCLXXXATION.PRIM-PHONE-NBR,                       00198600
198700               :DCLXXXATION.PRIM-CNTRY-NM,                        00198700
198800               :DCLXXXATION.PRIM-CNTRY-ABB,                       00198800
198900               :DCLXXXATION.SEC-LOC-NM,                           00198900
199000               :DCLXXXATION.SEC-CONTACT-NM,                       00199000
199100               :DCLXXXATION.SEC-ADR-1,                            00199100
199200               :DCLXXXATION.SEC-ADR-2,                            00199200
199300               :DCLXXXATION.SEC-ADR-3,                            00199300
199400               :DCLXXXATION.SEC-ADR-4,                            00199400
199500               :DCLXXXATION.SEC-CITY,                             00199500
199600               :DCLXXXATION.SEC-STATE-CD,                         00199600
199700               :DCLXXXATION.SEC-ZIP5-CD,                          00199700
199800               :DCLXXXATION.SEC-ZIP4-CD,                          00199800
199900               :DCLXXXATION.SEC-PHN-CNTRY-CD,                     00199900
200000               :DCLXXXATION.SEC-AREA-CD,                          00200000
200100               :DCLXXXATION.SEC-PHONE-NBR,                        00200100
200200               :DCLXXXATION.SEC-CNTRY-NM,                         00200200
200300               :DCLXXXATION.SEC-CNTRY-ABB,                        00200300
200400               :DCLXXXATION.MAIL-TO-LOC-NM,                       00200400
200500               :DCLXXXATION.MAIL-TO-CNTCT-NM,                     00200500
200600               :DCLXXXATION.MAIL-TO-ADR-1,                        00200600
200700               :DCLXXXATION.MAIL-TO-ADR-2,                        00200700
200800               :DCLXXXATION.MAIL-TO-ADR-3,                        00200800
200900               :DCLXXXATION.MAIL-TO-ADR-4,                        00200900
201000               :DCLXXXATION.MAIL-TO-CITY,                         00201000
201100               :DCLXXXATION.MAIL-TO-STATE-CD,                     00201100
201200               :DCLXXXATION.MAIL-TO-ZIP5-CD,                      00201200
201300               :DCLXXXATION.MAIL-TO-ZIP4-CD,                      00201300
201400               :DCLXXXATION.MAIL-PHN-CNTRY-CD,                    00201400
201500               :DCLXXXATION.MAIL-TO-AREA-CD,                      00201500
201600               :DCLXXXATION.MAIL-TO-PHONE-NBR,                    00201600
201700               :DCLXXXATION.MAIL-TO-CNTRY-NM,                     00201700
201800               :DCLXXXATION.MAIL-TO-CNTRY-AB,                     00201800
201900               :DCLXXXATION.CURR-FAX-ID,                          00201900
202000               :DCLXXXATION.ADDED-DT,                             00202000
202100               :DCLXXXATION.DELETE-DT,                            00202100
202200               :DCLXXXATION.OPENED-DT,                            00202200
202300               :DCLXXXATION.CLOSED-DT,                            00202300
202400               :DCLXXXATION.INACTIVE-SW,                          00202400
202500               :DCLXXXATION.INACTIVE-DT,                          00202500
202600               :DCLXXXATION.AP-NBR:WS-AP-NBR-IND,                 00202600
202700               :DCLXXXATION.AP-TYP-CD:WS-AP-TYP-CD-IND,           00202700
202800               :DCLXXXATION.LST-UPDT-TS,                          00202800
202900               :DCLXXXATION.LST-UPDT-USR-ID,                      00202900
203000               :DCLXXXATION.PRIM-EMAIL-ID,                        00203000
203100               :DCLXXXATION.SECY-EMAIL-ID,                        00203100
203200               :DCLXXXATION.MAIL-TO-EMAIL-ID,                     00203200
203300               :DCLXXXATION.FAC-ID,                               00203300
203400               :DCLXXXATION.ORG-ID,                               00203400
203500               :DCLXXXATION.B2B-PRIM-RTNG-ID,                     00203500
203600               :DCLXXXATION.PRIM-CNTY-TXT,                        00203600
203700               :DCLXXXATION.SECY-CNTY-TXT,                        00203700
203800               :DCLXXXATION.MAIL-TO-CNTY-TXT,                     00203800
203900               :DCLXXXATION.DIR-SHP-LOC-SW,                       00203900
204000               :DCLXXXATION.LOC-ORD-PROCNG-DD,                    00204000
204100               :WS-ORD-PROCNG-CTOF-TM,                            00204100
204200               :DCLXXXATION.SCH-SHP-DD-TXT,                       00204200
204300               :DCLXXXATION.ORD-LEAD-TM-DD,                       00204300
204400               :DCLXXXATION.ORD-BUFFER-TM-DD                      00204400
204500     END-EXEC                                                     00204500
204600     .                                                            00204600
204700                                                                  00204700
204800                                                                  00204800
204900 1304-FETCH-DDDXLO04.                                             00204900
205000     EXEC SQL                                                     00205000
205100         FETCH DDDXLO04                                           00205100
205200         INTO  :DCLXXXATION.LOC-TYP-CD,                           00205200
205300               :DCLXXXATION.LOC-NBR,                              00205300
205400               :DCLXXXATION.LOC-NM,                               00205400
205500               :DCLXXXATION.LOC-ABB,                              00205500
205600               :DCLXXXATION.LGL-LOC-NAM,                          00205600
205700               :DCLXXXATION.PRIM-CONTACT-NM,                      00205700
205800               :DCLXXXATION.PRIM-ADR-1,                           00205800
205900               :DCLXXXATION.PRIM-ADR-2,                           00205900
206000               :DCLXXXATION.PRIM-ADR-3,                           00206000
206100               :DCLXXXATION.PRIM-ADR-4,                           00206100
206200               :DCLXXXATION.PRIM-CITY,                            00206200
206300               :DCLXXXATION.PRIM-CITY-ID,                         00206300
206400               :DCLXXXATION.PRIM-STATE-CD,                        00206400
206500               :DCLXXXATION.PRIM-ZIP5-CD,                         00206500
206600               :DCLXXXATION.PRIM-ZIP4-CD,                         00206600
206700               :DCLXXXATION.PRIM-PHN-CNTRY-CD,                    00206700
206800               :DCLXXXATION.PRIM-AREA-CD,                         00206800
206900               :DCLXXXATION.PRIM-PHONE-NBR,                       00206900
207000               :DCLXXXATION.PRIM-CNTRY-NM,                        00207000
207100               :DCLXXXATION.PRIM-CNTRY-ABB,                       00207100
207200               :DCLXXXATION.SEC-LOC-NM,                           00207200
207300               :DCLXXXATION.SEC-CONTACT-NM,                       00207300
207400               :DCLXXXATION.SEC-ADR-1,                            00207400
207500               :DCLXXXATION.SEC-ADR-2,                            00207500
207600               :DCLXXXATION.SEC-ADR-3,                            00207600
207700               :DCLXXXATION.SEC-ADR-4,                            00207700
207800               :DCLXXXATION.SEC-CITY,                             00207800
207900               :DCLXXXATION.SEC-STATE-CD,                         00207900
208000               :DCLXXXATION.SEC-ZIP5-CD,                          00208000
208100               :DCLXXXATION.SEC-ZIP4-CD,                          00208100
208200               :DCLXXXATION.SEC-PHN-CNTRY-CD,                     00208200
208300               :DCLXXXATION.SEC-AREA-CD,                          00208300
208400               :DCLXXXATION.SEC-PHONE-NBR,                        00208400
208500               :DCLXXXATION.SEC-CNTRY-NM,                         00208500
208600               :DCLXXXATION.SEC-CNTRY-ABB,                        00208600
208700               :DCLXXXATION.MAIL-TO-LOC-NM,                       00208700
208800               :DCLXXXATION.MAIL-TO-CNTCT-NM,                     00208800
208900               :DCLXXXATION.MAIL-TO-ADR-1,                        00208900
209000               :DCLXXXATION.MAIL-TO-ADR-2,                        00209000
209100               :DCLXXXATION.MAIL-TO-ADR-3,                        00209100
209200               :DCLXXXATION.MAIL-TO-ADR-4,                        00209200
209300               :DCLXXXATION.MAIL-TO-CITY,                         00209300
209400               :DCLXXXATION.MAIL-TO-STATE-CD,                     00209400
209500               :DCLXXXATION.MAIL-TO-ZIP5-CD,                      00209500
209600               :DCLXXXATION.MAIL-TO-ZIP4-CD,                      00209600
209700               :DCLXXXATION.MAIL-PHN-CNTRY-CD,                    00209700
209800               :DCLXXXATION.MAIL-TO-AREA-CD,                      00209800
209900               :DCLXXXATION.MAIL-TO-PHONE-NBR,                    00209900
210000               :DCLXXXATION.MAIL-TO-CNTRY-NM,                     00210000
210100               :DCLXXXATION.MAIL-TO-CNTRY-AB,                     00210100
210200               :DCLXXXATION.CURR-FAX-ID,                          00210200
210300               :DCLXXXATION.ADDED-DT,                             00210300
210400               :DCLXXXATION.DELETE-DT,                            00210400
210500               :DCLXXXATION.OPENED-DT,                            00210500
210600               :DCLXXXATION.CLOSED-DT,                            00210600
210700               :DCLXXXATION.INACTIVE-SW,                          00210700
210800               :DCLXXXATION.INACTIVE-DT,                          00210800
210900               :DCLXXXATION.AP-NBR:WS-AP-NBR-IND,                 00210900
211000               :DCLXXXATION.AP-TYP-CD:WS-AP-TYP-CD-IND,           00211000
211100               :DCLXXXATION.LST-UPDT-TS,                          00211100
211200               :DCLXXXATION.LST-UPDT-USR-ID,                      00211200
211300               :DCLXXXATION.PRIM-EMAIL-ID,                        00211300
211400               :DCLXXXATION.SECY-EMAIL-ID,                        00211400
211500               :DCLXXXATION.MAIL-TO-EMAIL-ID,                     00211500
211600               :DCLXXXATION.FAC-ID,                               00211600
211700               :DCLXXXATION.ORG-ID,                               00211700
211800               :DCLXXXATION.B2B-PRIM-RTNG-ID,                     00211800
211900               :DCLXXXATION.PRIM-CNTY-TXT,                        00211900
212000               :DCLXXXATION.SECY-CNTY-TXT,                        00212000
212100               :DCLXXXATION.MAIL-TO-CNTY-TXT,                     00212100
212200               :DCLXXXATION.DIR-SHP-LOC-SW,                       00212200
212300               :DCLXXXATION.LOC-ORD-PROCNG-DD,                    00212300
212400               :WS-ORD-PROCNG-CTOF-TM,                            00212400
212500               :DCLXXXATION.SCH-SHP-DD-TXT,                       00212500
212600               :DCLXXXATION.ORD-LEAD-TM-DD,                       00212600
212700               :DCLXXXATION.ORD-BUFFER-TM-DD                      00212700
212800     END-EXEC                                                     00212800
212900     .                                                            00212900
213000                                                                  00213000
213100                                                                  00213100
213200 1305-FETCH-DDDXLO05.                                             00213200
213300     EXEC SQL                                                     00213300
213400         FETCH DDDXLO05                                           00213400
213500         INTO  :DCLXXXATION.LOC-TYP-CD,                           00213500
213600               :DCLXXXATION.LOC-NBR,                              00213600
213700               :DCLXXXATION.LOC-NM,                               00213700
213800               :DCLXXXATION.LOC-ABB,                              00213800
213900               :DCLXXXATION.LGL-LOC-NAM,                          00213900
214000               :DCLXXXATION.PRIM-CONTACT-NM,                      00214000
214100               :DCLXXXATION.PRIM-ADR-1,                           00214100
214200               :DCLXXXATION.PRIM-ADR-2,                           00214200
214300               :DCLXXXATION.PRIM-ADR-3,                           00214300
214400               :DCLXXXATION.PRIM-ADR-4,                           00214400
214500               :DCLXXXATION.PRIM-CITY,                            00214500
214600               :DCLXXXATION.PRIM-CITY-ID,                         00214600
214700               :DCLXXXATION.PRIM-STATE-CD,                        00214700
214800               :DCLXXXATION.PRIM-ZIP5-CD,                         00214800
214900               :DCLXXXATION.PRIM-ZIP4-CD,                         00214900
215000               :DCLXXXATION.PRIM-PHN-CNTRY-CD,                    00215000
215100               :DCLXXXATION.PRIM-AREA-CD,                         00215100
215200               :DCLXXXATION.PRIM-PHONE-NBR,                       00215200
215300               :DCLXXXATION.PRIM-CNTRY-NM,                        00215300
215400               :DCLXXXATION.PRIM-CNTRY-ABB,                       00215400
215500               :DCLXXXATION.SEC-LOC-NM,                           00215500
215600               :DCLXXXATION.SEC-CONTACT-NM,                       00215600
215700               :DCLXXXATION.SEC-ADR-1,                            00215700
215800               :DCLXXXATION.SEC-ADR-2,                            00215800
215900               :DCLXXXATION.SEC-ADR-3,                            00215900
216000               :DCLXXXATION.SEC-ADR-4,                            00216000
216100               :DCLXXXATION.SEC-CITY,                             00216100
216200               :DCLXXXATION.SEC-STATE-CD,                         00216200
216300               :DCLXXXATION.SEC-ZIP5-CD,                          00216300
216400               :DCLXXXATION.SEC-ZIP4-CD,                          00216400
216500               :DCLXXXATION.SEC-PHN-CNTRY-CD,                     00216500
216600               :DCLXXXATION.SEC-AREA-CD,                          00216600
216700               :DCLXXXATION.SEC-PHONE-NBR,                        00216700
216800               :DCLXXXATION.SEC-CNTRY-NM,                         00216800
216900               :DCLXXXATION.SEC-CNTRY-ABB,                        00216900
217000               :DCLXXXATION.MAIL-TO-LOC-NM,                       00217000
217100               :DCLXXXATION.MAIL-TO-CNTCT-NM,                     00217100
217200               :DCLXXXATION.MAIL-TO-ADR-1,                        00217200
217300               :DCLXXXATION.MAIL-TO-ADR-2,                        00217300
217400               :DCLXXXATION.MAIL-TO-ADR-3,                        00217400
217500               :DCLXXXATION.MAIL-TO-ADR-4,                        00217500
217600               :DCLXXXATION.MAIL-TO-CITY,                         00217600
217700               :DCLXXXATION.MAIL-TO-STATE-CD,                     00217700
217800               :DCLXXXATION.MAIL-TO-ZIP5-CD,                      00217800
217900               :DCLXXXATION.MAIL-TO-ZIP4-CD,                      00217900
218000               :DCLXXXATION.MAIL-PHN-CNTRY-CD,                    00218000
218100               :DCLXXXATION.MAIL-TO-AREA-CD,                      00218100
218200               :DCLXXXATION.MAIL-TO-PHONE-NBR,                    00218200
218300               :DCLXXXATION.MAIL-TO-CNTRY-NM,                     00218300
218400               :DCLXXXATION.MAIL-TO-CNTRY-AB,                     00218400
218500               :DCLXXXATION.CURR-FAX-ID,                          00218500
218600               :DCLXXXATION.ADDED-DT,                             00218600
218700               :DCLXXXATION.DELETE-DT,                            00218700
218800               :DCLXXXATION.OPENED-DT,                            00218800
218900               :DCLXXXATION.CLOSED-DT,                            00218900
219000               :DCLXXXATION.INACTIVE-SW,                          00219000
219100               :DCLXXXATION.INACTIVE-DT,                          00219100
219200               :DCLXXXATION.AP-NBR:WS-AP-NBR-IND,                 00219200
219300               :DCLXXXATION.AP-TYP-CD:WS-AP-TYP-CD-IND,           00219300
219400               :DCLXXXATION.LST-UPDT-TS,                          00219400
219500               :DCLXXXATION.LST-UPDT-USR-ID,                      00219500
219600               :DCLXXXATION.PRIM-EMAIL-ID,                        00219600
219700               :DCLXXXATION.SECY-EMAIL-ID,                        00219700
219800               :DCLXXXATION.MAIL-TO-EMAIL-ID,                     00219800
219900               :DCLXXXATION.FAC-ID,                               00219900
220000               :DCLXXXATION.ORG-ID,                               00220000
220100               :DCLXXXATION.B2B-PRIM-RTNG-ID,                     00220100
220200               :DCLXXXATION.PRIM-CNTY-TXT,                        00220200
220300               :DCLXXXATION.SECY-CNTY-TXT,                        00220300
220400               :DCLXXXATION.MAIL-TO-CNTY-TXT,                     00220400
220500               :DCLXXXATION.DIR-SHP-LOC-SW,                       00220500
220600               :DCLXXXATION.LOC-ORD-PROCNG-DD,                    00220600
220700               :WS-ORD-PROCNG-CTOF-TM,                            00220700
220800               :DCLXXXATION.SCH-SHP-DD-TXT,                       00220800
220900               :DCLXXXATION.ORD-LEAD-TM-DD,                       00220900
221000               :DCLXXXATION.ORD-BUFFER-TM-DD                      00221000
221100     END-EXEC                                                     00221100
221200     .                                                            00221200
221300                                                                  00221300
221400                                                                  00221400
221500 1306-FETCH-DDDXLO06.                                             00221500
221600     EXEC SQL                                                     00221600
221700         FETCH DDDXLO06                                           00221700
221800         INTO  :DCLXXXATION.LOC-TYP-CD,                           00221800
221900               :DCLXXXATION.LOC-NBR,                              00221900
222000               :DCLXXXATION.LOC-NM,                               00222000
222100               :DCLXXXATION.LOC-ABB,                              00222100
222200               :DCLXXXATION.LGL-LOC-NAM,                          00222200
222300               :DCLXXXATION.PRIM-CONTACT-NM,                      00222300
222400               :DCLXXXATION.PRIM-ADR-1,                           00222400
222500               :DCLXXXATION.PRIM-ADR-2,                           00222500
222600               :DCLXXXATION.PRIM-ADR-3,                           00222600
222700               :DCLXXXATION.PRIM-ADR-4,                           00222700
222800               :DCLXXXATION.PRIM-CITY,                            00222800
222900               :DCLXXXATION.PRIM-CITY-ID,                         00222900
223000               :DCLXXXATION.PRIM-STATE-CD,                        00223000
223100               :DCLXXXATION.PRIM-ZIP5-CD,                         00223100
223200               :DCLXXXATION.PRIM-ZIP4-CD,                         00223200
223300               :DCLXXXATION.PRIM-PHN-CNTRY-CD,                    00223300
223400               :DCLXXXATION.PRIM-AREA-CD,                         00223400
223500               :DCLXXXATION.PRIM-PHONE-NBR,                       00223500
223600               :DCLXXXATION.PRIM-CNTRY-NM,                        00223600
223700               :DCLXXXATION.PRIM-CNTRY-ABB,                       00223700
223800               :DCLXXXATION.SEC-LOC-NM,                           00223800
223900               :DCLXXXATION.SEC-CONTACT-NM,                       00223900
224000               :DCLXXXATION.SEC-ADR-1,                            00224000
224100               :DCLXXXATION.SEC-ADR-2,                            00224100
224200               :DCLXXXATION.SEC-ADR-3,                            00224200
224300               :DCLXXXATION.SEC-ADR-4,                            00224300
224400               :DCLXXXATION.SEC-CITY,                             00224400
224500               :DCLXXXATION.SEC-STATE-CD,                         00224500
224600               :DCLXXXATION.SEC-ZIP5-CD,                          00224600
224700               :DCLXXXATION.SEC-ZIP4-CD,                          00224700
224800               :DCLXXXATION.SEC-PHN-CNTRY-CD,                     00224800
224900               :DCLXXXATION.SEC-AREA-CD,                          00224900
225000               :DCLXXXATION.SEC-PHONE-NBR,                        00225000
225100               :DCLXXXATION.SEC-CNTRY-NM,                         00225100
225200               :DCLXXXATION.SEC-CNTRY-ABB,                        00225200
225300               :DCLXXXATION.MAIL-TO-LOC-NM,                       00225300
225400               :DCLXXXATION.MAIL-TO-CNTCT-NM,                     00225400
225500               :DCLXXXATION.MAIL-TO-ADR-1,                        00225500
225600               :DCLXXXATION.MAIL-TO-ADR-2,                        00225600
225700               :DCLXXXATION.MAIL-TO-ADR-3,                        00225700
225800               :DCLXXXATION.MAIL-TO-ADR-4,                        00225800
225900               :DCLXXXATION.MAIL-TO-CITY,                         00225900
226000               :DCLXXXATION.MAIL-TO-STATE-CD,                     00226000
226100               :DCLXXXATION.MAIL-TO-ZIP5-CD,                      00226100
226200               :DCLXXXATION.MAIL-TO-ZIP4-CD,                      00226200
226300               :DCLXXXATION.MAIL-PHN-CNTRY-CD,                    00226300
226400               :DCLXXXATION.MAIL-TO-AREA-CD,                      00226400
226500               :DCLXXXATION.MAIL-TO-PHONE-NBR,                    00226500
226600               :DCLXXXATION.MAIL-TO-CNTRY-NM,                     00226600
226700               :DCLXXXATION.MAIL-TO-CNTRY-AB,                     00226700
226800               :DCLXXXATION.CURR-FAX-ID,                          00226800
226900               :DCLXXXATION.ADDED-DT,                             00226900
227000               :DCLXXXATION.DELETE-DT,                            00227000
227100               :DCLXXXATION.OPENED-DT,                            00227100
227200               :DCLXXXATION.CLOSED-DT,                            00227200
227300               :DCLXXXATION.INACTIVE-SW,                          00227300
227400               :DCLXXXATION.INACTIVE-DT,                          00227400
227500               :DCLXXXATION.AP-NBR:WS-AP-NBR-IND,                 00227500
227600               :DCLXXXATION.AP-TYP-CD:WS-AP-TYP-CD-IND,           00227600
227700               :DCLXXXATION.LST-UPDT-TS,                          00227700
227800               :DCLXXXATION.LST-UPDT-USR-ID,                      00227800
227900               :DCLXXXATION.PRIM-EMAIL-ID,                        00227900
228000               :DCLXXXATION.SECY-EMAIL-ID,                        00228000
228100               :DCLXXXATION.MAIL-TO-EMAIL-ID,                     00228100
228200               :DCLXXXATION.FAC-ID,                               00228200
228300               :DCLXXXATION.ORG-ID,                               00228300
228400               :DCLXXXATION.B2B-PRIM-RTNG-ID,                     00228400
228500               :DCLXXXATION.PRIM-CNTY-TXT,                        00228500
228600               :DCLXXXATION.SECY-CNTY-TXT,                        00228600
228700               :DCLXXXATION.MAIL-TO-CNTY-TXT,                     00228700
228800               :DCLXXXATION.DIR-SHP-LOC-SW,                       00228800
228900               :DCLXXXATION.LOC-ORD-PROCNG-DD,                    00228900
229000               :WS-ORD-PROCNG-CTOF-TM,                            00229000
229100               :DCLXXXATION.SCH-SHP-DD-TXT,                       00229100
229200               :DCLXXXATION.ORD-LEAD-TM-DD,                       00229200
229300               :DCLXXXATION.ORD-BUFFER-TM-DD                      00229300
229400     END-EXEC                                                     00229400
229500     .                                                            00229500
229600                                                                  00229600
229700                                                                  00229700
229800 1307-FETCH-DDDXLO07.                                             00229800
229900     EXEC SQL                                                     00229900
230000         FETCH DDDXLO07                                           00230000
230100         INTO  :DCLXXXATION.LOC-TYP-CD,                           00230100
230200               :DCLXXXATION.LOC-NBR,                              00230200
230300               :DCLXXXATION.LOC-NM,                               00230300
230400               :DCLXXXATION.LOC-ABB,                              00230400
230500               :DCLXXXATION.LGL-LOC-NAM,                          00230500
230600               :DCLXXXATION.PRIM-CONTACT-NM,                      00230600
230700               :DCLXXXATION.PRIM-ADR-1,                           00230700
230800               :DCLXXXATION.PRIM-ADR-2,                           00230800
230900               :DCLXXXATION.PRIM-ADR-3,                           00230900
231000               :DCLXXXATION.PRIM-ADR-4,                           00231000
231100               :DCLXXXATION.PRIM-CITY,                            00231100
231200               :DCLXXXATION.PRIM-CITY-ID,                         00231200
231300               :DCLXXXATION.PRIM-STATE-CD,                        00231300
231400               :DCLXXXATION.PRIM-ZIP5-CD,                         00231400
231500               :DCLXXXATION.PRIM-ZIP4-CD,                         00231500
231600               :DCLXXXATION.PRIM-PHN-CNTRY-CD,                    00231600
231700               :DCLXXXATION.PRIM-AREA-CD,                         00231700
231800               :DCLXXXATION.PRIM-PHONE-NBR,                       00231800
231900               :DCLXXXATION.PRIM-CNTRY-NM,                        00231900
232000               :DCLXXXATION.PRIM-CNTRY-ABB,                       00232000
232100               :DCLXXXATION.SEC-LOC-NM,                           00232100
232200               :DCLXXXATION.SEC-CONTACT-NM,                       00232200
232300               :DCLXXXATION.SEC-ADR-1,                            00232300
232400               :DCLXXXATION.SEC-ADR-2,                            00232400
232500               :DCLXXXATION.SEC-ADR-3,                            00232500
232600               :DCLXXXATION.SEC-ADR-4,                            00232600
232700               :DCLXXXATION.SEC-CITY,                             00232700
232800               :DCLXXXATION.SEC-STATE-CD,                         00232800
232900               :DCLXXXATION.SEC-ZIP5-CD,                          00232900
233000               :DCLXXXATION.SEC-ZIP4-CD,                          00233000
233100               :DCLXXXATION.SEC-PHN-CNTRY-CD,                     00233100
233200               :DCLXXXATION.SEC-AREA-CD,                          00233200
233300               :DCLXXXATION.SEC-PHONE-NBR,                        00233300
233400               :DCLXXXATION.SEC-CNTRY-NM,                         00233400
233500               :DCLXXXATION.SEC-CNTRY-ABB,                        00233500
233600               :DCLXXXATION.MAIL-TO-LOC-NM,                       00233600
233700               :DCLXXXATION.MAIL-TO-CNTCT-NM,                     00233700
233800               :DCLXXXATION.MAIL-TO-ADR-1,                        00233800
233900               :DCLXXXATION.MAIL-TO-ADR-2,                        00233900
234000               :DCLXXXATION.MAIL-TO-ADR-3,                        00234000
234100               :DCLXXXATION.MAIL-TO-ADR-4,                        00234100
234200               :DCLXXXATION.MAIL-TO-CITY,                         00234200
234300               :DCLXXXATION.MAIL-TO-STATE-CD,                     00234300
234400               :DCLXXXATION.MAIL-TO-ZIP5-CD,                      00234400
234500               :DCLXXXATION.MAIL-TO-ZIP4-CD,                      00234500
234600               :DCLXXXATION.MAIL-PHN-CNTRY-CD,                    00234600
234700               :DCLXXXATION.MAIL-TO-AREA-CD,                      00234700
234800               :DCLXXXATION.MAIL-TO-PHONE-NBR,                    00234800
234900               :DCLXXXATION.MAIL-TO-CNTRY-NM,                     00234900
235000               :DCLXXXATION.MAIL-TO-CNTRY-AB,                     00235000
235100               :DCLXXXATION.CURR-FAX-ID,                          00235100
235200               :DCLXXXATION.ADDED-DT,                             00235200
235300               :DCLXXXATION.DELETE-DT,                            00235300
235400               :DCLXXXATION.OPENED-DT,                            00235400
235500               :DCLXXXATION.CLOSED-DT,                            00235500
235600               :DCLXXXATION.INACTIVE-SW,                          00235600
235700               :DCLXXXATION.INACTIVE-DT,                          00235700
235800               :DCLXXXATION.AP-NBR:WS-AP-NBR-IND,                 00235800
235900               :DCLXXXATION.AP-TYP-CD:WS-AP-TYP-CD-IND,           00235900
236000               :DCLXXXATION.LST-UPDT-TS,                          00236000
236100               :DCLXXXATION.LST-UPDT-USR-ID,                      00236100
236200               :DCLXXXATION.PRIM-EMAIL-ID,                        00236200
236300               :DCLXXXATION.SECY-EMAIL-ID,                        00236300
236400               :DCLXXXATION.MAIL-TO-EMAIL-ID,                     00236400
236500               :DCLXXXATION.FAC-ID,                               00236500
236600               :DCLXXXATION.ORG-ID,                               00236600
236700               :DCLXXXATION.B2B-PRIM-RTNG-ID,                     00236700
236800               :DCLXXXATION.PRIM-CNTY-TXT,                        00236800
236900               :DCLXXXATION.SECY-CNTY-TXT,                        00236900
237000               :DCLXXXATION.MAIL-TO-CNTY-TXT,                     00237000
237100               :DCLXXXATION.DIR-SHP-LOC-SW,                       00237100
237200               :DCLXXXATION.LOC-ORD-PROCNG-DD,                    00237200
237300               :WS-ORD-PROCNG-CTOF-TM,                            00237300
237400               :DCLXXXATION.SCH-SHP-DD-TXT,                       00237400
237500               :DCLXXXATION.ORD-LEAD-TM-DD,                       00237500
237600               :DCLXXXATION.ORD-BUFFER-TM-DD                      00237600
237700     END-EXEC                                                     00237700
237800     .                                                            00237800
237900                                                                  00237900
238000                                                                  00238000
238100 1400-EXIT-PUT-MODIFY-ROW.                                        00238100
238200     PERFORM 1800-EDIT-NULL-INDICATORS                            00238200
238300     PERFORM 2040-GET-DATE-AND-USER                               00238300
238400                                                                  00238400
238500     IF SUCCESS                                                   00238500
238600       PERFORM 1411-CHECK-FOR-EVENTS                              00238600
238700       IF SUCCESS                                                 00238700
238800         PERFORM 1420-CHECK-AP-INFO                               00238800
238900         IF SUCCESS                                               00238900
239000*          PERFORM 10300-CHECK-FOR-VALID-COUNTY                   00239000
239100           SET EXIT-PUT-MODIFY-ROW  TO TRUE                       00239100
239200           IF SUCCESS                                             00239200
239300             PERFORM 1430-GET-CURRENT-VALUES                      00239300
239400             PERFORM 1800-EDIT-NULL-INDICATORS                    00239400
239500             PERFORM 1440-D0-MODIFY-ROW                           00239500
239600           END-IF                                                 00239600
239700         END-IF                                                   00239700
239800       END-IF                                                     00239800
239900     END-IF                                                       00239900
240000     .                                                            00240000
240100                                                                  00240100
240200                                                                  00240200
240300 1411-CHECK-FOR-EVENTS.                                           00240300
240400     PERFORM 1412-GET-AP-NBR                                      00240400
240500     IF SUCCESS                                                   00240500
240600       PERFORM 1414-VALIDATE-ACTIV-SW                             00240600
240700     END-IF                                                       00240700
240800     .                                                            00240800
240900                                                                  00240900
241000                                                                  00241000
241100 1412-GET-AP-NBR.                                                 00241100
241200     IF LOC-TYP-CD OF DCLXXXATION = 'V' OR 'D'                    00241200
241300       EXEC SQL                                                   00241300
241400           SELECT AP_NBR,                                         00241400
241500                  AP_TYP_CD                                       00241500
241600           INTO :WS-AP-NUM:WS-AP-NBR-IND,                         00241600
241700                :WS-AP-TYPE:WS-AP-TYP-CD-IND                      00241700
241800           FROM XXXATION                                          00241800
241900           WHERE LOC_TYP_CD = :DCLXXXATION.LOC-TYP-CD             00241900
242000           AND  LOC_NBR = :DCLXXXATION.LOC-NBR                    00242000
242100       END-EXEC                                                   00242100
242200                                                                  00242200
242300       EVALUATE TRUE                                              00242300
242400         WHEN SQLCODE = 0                                         00242400
242500           IF WS-AP-NBR-IND < 0                                   00242500
242600           OR WS-AP-TYP-CD-IND < 0                                00242600
242700             INITIALIZE WS-AP-NUM                                 00242700
242800                        WS-AP-TYPE                                00242800
242900           END-IF                                                 00242900
243000         WHEN SQLCODE = 100                                       00243000
243100           INITIALIZE WS-AP-NUM                                   00243100
243200                      WS-AP-TYPE                                  00243200
243300           MOVE 0 TO SQLCODE                                      00243300
243400        WHEN OTHER                                                00243400
243500         SET FAILURE  TO TRUE                                     00243500
243600         MOVE SPACES  TO IS-RTRN-MSG-TXT                          00243600
243700         MOVE SQLCODE TO WS-SQLCODE                               00243700
243800         STRING 'NNNS0487 - Error in gathering events. SQL '      00243800
243900                WS-SQLCODE '.'                                    00243900
244000         DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                   00244000
244100       END-EVALUATE                                               00244100
244200       MOVE WS-AP-NUM   TO MMMC0474-OLD-AP-NBR                    00244200
244300       MOVE WS-AP-TYPE  TO MMMC0474-OLD-AP-TYP                    00244300
244400     END-IF                                                       00244400
244500     .                                                            00244500
244600                                                                  00244600
244700                                                                  00244700
244800 1414-VALIDATE-ACTIV-SW.                                          00244800
244900     EVALUATE TRUE                                                00244900
245000       WHEN INACTIVE-SW OF DCLXXXATION = K-LOC-IN-ACTIVE          00245000
245100         IF INACTIVE-DT OF DCLXXXATION = K-DEF-DT                 00245100
245200           MOVE DTA10-MM-DD-YYYY TO INACTIVE-DT OF DCLXXXATION    00245200
245300         END-IF                                                   00245300
245400                                                                  00245400
245500       WHEN INACTIVE-SW OF DCLXXXATION = K-LOC-ACTIVE             00245500
245600         MOVE K-DEF-DT TO INACTIVE-DT OF DCLXXXATION              00245600
245700         IF DELETE-DT     OF P-DDDTLO01 = SPACES                  00245700
245800           MOVE K-DEF-DT TO DELETE-DT     OF DCLXXXATION          00245800
245900         ELSE                                                     00245900
246000           MOVE DELETE-DT     OF P-DDDTLO01                       00246000
246100             TO DELETE-DT     OF DCLXXXATION                      00246100
246200         END-IF                                                   00246200
246300                                                                  00246300
246400                                                                  00246400
246500       WHEN INACTIVE-SW OF DCLXXXATION = K-LOC-DELETED            00246500
246600         IF INACTIVE-DT OF DCLXXXATION = K-DEF-DT                 00246600
246700           MOVE DTA10-MM-DD-YYYY TO INACTIVE-DT OF DCLXXXATION    00246700
246800         END-IF                                                   00246800
246900         IF DELETE-DT OF DCLXXXATION = K-DEF-DT                   00246900
247000           MOVE DTA10-MM-DD-YYYY TO DELETE-DT   OF DCLXXXATION    00247000
247100         END-IF                                                   00247100
247200                                                                  00247200
247300       WHEN OTHER                                                 00247300
247400         SET  FAILURE TO TRUE                                     00247400
247500         MOVE 'NNNS0487 - Invalid active-sw - must be A,I,or D!'  00247500
247600           TO IS-RTRN-MSG-TXT                                     00247600
247700     END-EVALUATE                                                 00247700
247800     .                                                            00247800
247900                                                                  00247900
248000                                                                  00248000
248100 1420-CHECK-AP-INFO.                                              00248100
248200     IF LOC-TYP-CD OF DCLXXXATION = 'V'                           00248200
248300       IF AP-NBR OF DCLXXXATION > 0                               00248300
248400         IF AP-NBR OF DCLXXXATION NOT EQUAL TO WS-AP-NUM          00248400
248500           PERFORM 1505-GET-FAC-ORG-ID                            00248500
248600         END-IF                                                   00248600
248700       END-IF                                                     00248700
248800     END-IF                                                       00248800
248900     .                                                            00248900
249000                                                                  00249000
249100                                                                  00249100
249200 1430-GET-CURRENT-VALUES.                                         00249200
249300                                                                  00249300
249400     EXEC SQL                                                     00249400
249500         SELECT MAIL_TO_LOC_NM,                                   00249500
249600                MAIL_TO_ADR_1,                                    00249600
249700                MAIL_TO_ADR_2,                                    00249700
249800                MAIL_TO_CITY,                                     00249800
249900                MAIL_TO_STATE_CD,                                 00249900
250000                SEC_CONTACT_NM,                                   00250000
250100                MAIL_TO_ZIP5_CD,                                  00250100
250200                MAIL_TO_ZIP4_CD,                                  00250200
250300                MAIL_TO_PHONE_NBR                                 00250300
250400          INTO  :WS-MAIL-TO-LOC-NM,                               00250400
250500                :WS-MAIL-TO-ADR-1,                                00250500
250600                :WS-MAIL-TO-ADR-2,                                00250600
250700                :WS-MAIL-TO-CITY,                                 00250700
250800                :WS-MAIL-TO-STATE-CD,                             00250800
250900                :WS-SEC-CONTACT-NM,                               00250900
251000                :WS-MAIL-TO-ZIP5-CD,                              00251000
251100                :WS-MAIL-TO-ZIP4-CD,                              00251100
251200                :WS-MAIL-TO-PHONE-NBR                             00251200
251300         FROM   XXXATION                                          00251300
251400         WHERE  LOC_TYP_CD = :DCLXXXATION.LOC-TYP-CD              00251400
251500         AND    LOC_NBR = :DCLXXXATION.LOC-NBR                    00251500
251600     END-EXEC                                                     00251600
251700                                                                  00251700
251800     EVALUATE TRUE                                                00251800
251900       WHEN SQLCODE = 0                                           00251900
252000         CONTINUE                                                 00252000
252100       WHEN SQLCODE = 100                                         00252100
252200         CONTINUE                                                 00252200
252300       WHEN SQLCODE NOT = 0                                       00252300
252400         SET  FAILURE TO TRUE                                     00252400
252500         MOVE SPACES  TO IS-RTRN-MSG-TXT                          00252500
252600         MOVE SQLCODE TO WS-SQLCODE                               00252600
252700         STRING 'NNNS0487 - Error in getting curr values, '       00252700
252800                'RC=' WS-SQLCODE '.'                              00252800
252900                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT            00252900
253000     END-EVALUATE                                                 00253000
253100     .                                                            00253100
253200                                                                  00253200
253300                                                                  00253300
253400 1440-D0-MODIFY-ROW.                                              00253400
253500     MOVE YYYC0127-TS   TO LST-UPDT-TS     OF DCLXXXATION         00253500
253600     MOVE YYYC0107-USER TO LST-UPDT-USR-ID OF DCLXXXATION         00253600
253700                                                                  00253700
253800     PERFORM 4010-REP-LOWVALUE-WITH-SPACES                        00253800
253900     PERFORM 5000-CALL-NNNS0487-CUD-ROUTINE                       00253900
254000                                                                  00254000
254100     IF SQLCODE = 0                                               00254100
254200       SET YYYN110A-UPD TO TRUE                                   00254200
254300       SET MMMC0265-MOD TO TRUE                                   00254300
254400       SET LOC-UPD      TO TRUE                                   00254400
254500       SET DSD-UPD      TO TRUE                                   00254500
254600       SET WHS-UPD      TO TRUE                                   00254600
254700       SET VEN-UPD      TO TRUE                                   00254700
254800       SET MODIFY-OPERATION TO TRUE                               00254800
254900       PERFORM 10100-CHECK-FOR-VNDR-EVENTS                        00254900
255000       PERFORM 2000-DENORM-PROCESS                                00255000
255100       IF SUCCESS                                                 00255100
255200         IF LOC-TYP-CD OF DCLXXXATION = 'V' OR 'D'                00255200
255300           IF AP-NBR OF DCLXXXATION NOT EQUAL MMMC0474-OLD-AP-NBR 00255300
255400             SET MMMC0474-UPD TO TRUE                             00255400
255500             PERFORM 125-CONNECT-TO-DB2                           00255500
255600             PERFORM 2050-DO-AVP-MAINTENANCE                      00255600
255700           END-IF                                                 00255700
255800         END-IF                                                   00255800
255900       END-IF                                                     00255900
256000     END-IF                                                       00256000
256100     .                                                            00256100
256200                                                                  00256200
256300                                                                  00256300
256400 1500-EXIT-PUT-INSERT-ROW.                                        00256400
256500     PERFORM 1800-EDIT-NULL-INDICATORS                            00256500
256600     PERFORM 2040-GET-DATE-AND-USER                               00256600
256700     IF SUCCESS                                                   00256700
256800*      PERFORM 10300-CHECK-FOR-VALID-COUNTY                       00256800
256900       SET EXIT-PUT-INSERT-ROW TO TRUE                            00256900
257000       IF SUCCESS                                                 00257000
257100         PERFORM 1505-GET-FAC-ORG-ID                              00257100
257200         IF SUCCESS                                               00257200
257300           PERFORM 1510-D0-INSERT-ROW                             00257300
257400         END-IF                                                   00257400
257500       END-IF                                                     00257500
257600     END-IF                                                       00257600
257700     .                                                            00257700
257800                                                                  00257800
257900                                                                  00257900
258000 1505-GET-FAC-ORG-ID.                                             00258000
258100     EVALUATE TRUE                                                00258100
258200       WHEN LOC-TYP-CD OF DCLXXXATION = 'V'                       00258200
258300        AND AP-NBR OF DCLXXXATION > 0                             00258300
258400         PERFORM 1515-CHECK-AP-NBR                                00258400
258500       WHEN (LOC-TYP-CD OF DCLXXXATION = 'A' OR 'D' OR 'S' OR 'W')00258500
258600         OR (LOC-TYP-CD OF DCLXXXATION = 'V'                      00258600
258700        AND AP-NBR OF DCLXXXATION = 0)                            00258700
258800         PERFORM 1525-EXIT-GET-FAC-ID                             00258800
258900         IF SUCCESS                                               00258900
259000           PERFORM 1530-EXIT-GET-ORG-ID                           00259000
259100         END-IF                                                   00259100
259200       WHEN OTHER                                                 00259200
259300         CONTINUE                                                 00259300
259400     END-EVALUATE                                                 00259400
259500     .                                                            00259500
259600                                                                  00259600
259700                                                                  00259700
259800 1510-D0-INSERT-ROW.                                              00259800
259900     MOVE DTA10-MM-DD-YYYY TO ADDED-DT        OF DCLXXXATION      00259900
260000     MOVE YYYC0127-TS      TO LST-UPDT-TS     OF DCLXXXATION      00260000
260100     MOVE YYYC0107-USER    TO LST-UPDT-USR-ID OF DCLXXXATION      00260100
260200     PERFORM 4010-REP-LOWVALUE-WITH-SPACES                        00260200
260300     PERFORM 5000-CALL-NNNS0487-CUD-ROUTINE                       00260300
260400                                                                  00260400
260500     IF SQLCODE = 0                                               00260500
260600       SET YYYN110A-ADD TO TRUE                                   00260600
260700       SET MMMC0265-ADD TO TRUE                                   00260700
260800       SET LOC-ADD      TO TRUE                                   00260800
260900       SET DSD-ADD      TO TRUE                                   00260900
261000       SET WHS-ADD      TO TRUE                                   00261000
261100       SET VEN-ADD      TO TRUE                                   00261100
261200       PERFORM 2000-DENORM-PROCESS                                00261200
261300     END-IF                                                       00261300
261400     .                                                            00261400
261500                                                                  00261500
261600                                                                  00261600
261700 1515-CHECK-AP-NBR.                                               00261700
261800     MOVE AP-NBR    OF DCLXXXATION TO WS-AP-NUM                   00261800
261900     MOVE AP-TYP-CD OF DCLXXXATION TO WS-AP-TYPE                  00261900
262000                                                                  00262000
262100     EXEC SQL                                                     00262100
262200         SELECT COUNT(*)                                          00262200
262300         INTO   :WS-AP-NBR-CNT                                    00262300
262400         FROM XXXATION                                            00262400
262500         WHERE AP_TYP_CD = :WS-AP-TYPE                            00262500
262600           AND AP_NBR = :WS-AP-NUM                                00262600
262700     END-EXEC                                                     00262700
262800                                                                  00262800
262900     EVALUATE TRUE                                                00262900
263000       WHEN SQLCODE = 0                                           00263000
263100         IF WS-AP-NBR-CNT = 0                                     00263100
263200           PERFORM 1525-EXIT-GET-FAC-ID                           00263200
263300           IF SUCCESS                                             00263300
263400             PERFORM 1530-EXIT-GET-ORG-ID                         00263400
263500           END-IF                                                 00263500
263600         ELSE                                                     00263600
263700           PERFORM 1520-GET-AP-INFO                               00263700
263800         END-IF                                                   00263800
263900       WHEN OTHER                                                 00263900
264000         SET  FAILURE        TO TRUE                              00264000
264100         MOVE 'NNNS0487 - Error getting AP count!'                00264100
264200           TO IS-RTRN-MSG-TXT                                     00264200
264300     END-EVALUATE                                                 00264300
264400     .                                                            00264400
264500                                                                  00264500
264600                                                                  00264600
264700 1520-GET-AP-INFO.                                                00264700
264800     EXEC SQL                                                     00264800
264900       SELECT FAC_ID,                                             00264900
265000              ORG_ID                                              00265000
265100         INTO :WS-FAC-ID,                                         00265100
265200              :WS-ORG-ID                                          00265200
265300         FROM XXXATION                                            00265300
265400        WHERE AP_TYP_CD = :WS-AP-TYPE                             00265400
265500          AND AP_NBR = :WS-AP-NUM                                 00265500
265600       FETCH FIRST 1 ROWS ONLY                                    00265600
265700     END-EXEC                                                     00265700
265800                                                                  00265800
265900     EVALUATE TRUE                                                00265900
266000       WHEN SQLCODE = 0                                           00266000
266100         MOVE WS-FAC-ID TO FAC-ID OF DCLXXXATION                  00266100
266200         MOVE WS-ORG-ID TO ORG-ID OF DCLXXXATION                  00266200
266300       WHEN OTHER                                                 00266300
266400         SET  FAILURE TO TRUE                                     00266400
266500         MOVE 'NNNS0487 - Error getting AP info!'                 00266500
266600           TO IS-RTRN-MSG-TXT                                     00266600
266700     END-EVALUATE                                                 00266700
266800     .                                                            00266800
266900                                                                  00266900
267000                                                                  00267000
267100                                                                  00267100
267200 1525-EXIT-GET-FAC-ID.                                            00267200
267300     EXEC SQL                                                     00267300
267400         SELECT MAX (FAC_ID)                                      00267400
267500         INTO   :DCLXXXATION.FAC-ID                               00267500
267600         FROM   XXXATION                                          00267600
267700     END-EXEC                                                     00267700
267800                                                                  00267800
267900     EVALUATE TRUE                                                00267900
268000       WHEN SQLCODE = 0                                           00268000
268100         COMPUTE FAC-ID OF DCLXXXATION =                          00268100
268200                 FAC-ID OF DCLXXXATION + 1                        00268200
268300       WHEN OTHER                                                 00268300
268400         SET  FAILURE TO TRUE                                     00268400
268500         MOVE 'NNNS0487 - Error getting FAC_ID!'                  00268500
268600           TO IS-RTRN-MSG-TXT                                     00268600
268700     END-EVALUATE                                                 00268700
268800     .                                                            00268800
268900                                                                  00268900
269000                                                                  00269000
269100 1530-EXIT-GET-ORG-ID.                                            00269100
269200     EXEC SQL                                                     00269200
269300         SELECT GREATEST (MAX (LOC.ORG_ID), MAX (DEPT.ORG_ID))    00269300
269400         INTO   :DCLXXXATION.ORG-ID                               00269400
269500         FROM   XXXATION LOC, STR_DEPT DEPT                       00269500
269600     END-EXEC                                                     00269600
269700                                                                  00269700
269800     EVALUATE TRUE                                                00269800
269900       WHEN SQLCODE = 0                                           00269900
270000         COMPUTE ORG-ID OF DCLXXXATION =                          00270000
270100                 ORG-ID OF DCLXXXATION + 1                        00270100
270200       WHEN OTHER                                                 00270200
270300         SET  FAILURE TO TRUE                                     00270300
270400         MOVE 'NNNS0487 - Error getting ORG_ID!'                  00270400
270500           TO IS-RTRN-MSG-TXT                                     00270500
270600     END-EVALUATE                                                 00270600
270700     .                                                            00270700
270800                                                                  00270800
270900                                                                  00270900
271000 1600-EXIT-PUT-PURGE-ROW.                                         00271000
271100     EVALUATE TRUE                                                00271100
271200       WHEN LOC-TYP-CD OF DCLXXXATION = K-STORE-LOC-TYPE          00271200
271300       OR   LOC-TYP-CD OF DCLXXXATION = K-ACCT-LOC-TYPE           00271300
271400         PERFORM 1610-DELETE-STORE                                00271400
271500       WHEN LOC-TYP-CD OF DCLXXXATION = K-DSD-VEND-LOC-TYPE       00271500
271600         PERFORM 1620-DELETE-DSD-VENDOR                           00271600
271700       WHEN LOC-TYP-CD OF DCLXXXATION = 'B'                       00271700
271800         PERFORM 1640-DELETE-BKHAUL                               00271800
271900       WHEN LOC-TYP-CD OF DCLXXXATION = 'T'                       00271900
272000         PERFORM 1650-DELETE-TERMINAL                             00272000
272100       WHEN LOC-TYP-CD OF DCLXXXATION = 'W'                       00272100
272200         PERFORM 1660-DELETE-WHSE                                 00272200
272300     END-EVALUATE                                                 00272300
272400                                                                  00272400
272500     PERFORM 115-CONNECT-TO-ORACLE                                00272500
272600     IF SUCCESS AND SQLCODE = 0                                   00272600
272700       PERFORM 1690-DELETE-LO                                     00272700
272800     END-IF                                                       00272800
272900                                                                  00272900
273000     EVALUATE TRUE                                                00273000
273100       WHEN SQLCODE = 0                                           00273100
273200         SET YYYN110A-DEL  TO TRUE                                00273200
273300         SET MMMC0265-DEL  TO TRUE                                00273300
273400         SET LOC-DEL       TO TRUE                                00273400
273500         SET DSD-DEL       TO TRUE                                00273500
273600         SET WHS-DEL       TO TRUE                                00273600
273700         SET VEN-DEL       TO TRUE                                00273700
273800     IF SUCCESS AND SQLCODE = 0                                   00273800
273900         SET DELETE-OPERATION  TO TRUE                            00273900
274000         SET STAGE-EVENT       TO TRUE                            00274000
274100     END-IF                                                       00274100
274200         PERFORM 2000-DENORM-PROCESS                              00274200
274300       WHEN SQLCODE = -532                                        00274300
274400       WHEN SQLCODE = -84                                         00274400
274500         SET  FAILURE TO TRUE                                     00274500
274600         MOVE 'NNNS0487 - XXXATION in use - it cannot be deleted!'00274600
274700           TO IS-RTRN-MSG-TXT                                     00274700
274800       WHEN SQLCODE NOT = 0                                       00274800
274900         MOVE SQLCODE                 TO WS-SQLCODE               00274900
275000         SET  FAILURE                 TO TRUE                     00275000
275100         MOVE SPACES                  TO IS-RTRN-MSG-TXT          00275100
275200         STRING 'NNNS0514 - Error deleting XXXATION, SQL='        00275200
275300                 WS-SQLCODE                                       00275300
275400                 DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT           00275400
275500     END-EVALUATE                                                 00275500
275600     .                                                            00275600
275700                                                                  00275700
275800                                                                  00275800
275900 1610-DELETE-STORE.                                               00275900
276000     PERFORM 1610-EXIT-PUT-PURGE-RETL                             00276000
276100     IF SQLCODE = 0                                               00276100
276200       MOVE  LOC-TYP-CD       OF DCLXXXATION                      00276200
276300         TO  LOC-TYP-CD       OF P-DDDTLR01                       00276300
276400       MOVE  LOC-NBR          OF DCLXXXATION                      00276400
276500         TO  LOC-NBR          OF P-DDDTLR01                       00276500
276600       SET EXIT-PUT-PURGE-ROW TO TRUE                             00276600
276700       PERFORM 2060-CALL-RETAIL-LOC-DAO                           00276700
276800       IF SQLCODE = 100                                           00276800
276900         MOVE 0 TO SQLCODE                                        00276900
277000       END-IF                                                     00277000
277100     END-IF                                                       00277100
277200                                                                  00277200
277300     IF SQLCODE = 0                                               00277300
277400       MOVE  LOC-TYP-CD       OF DCLXXXATION                      00277400
277500         TO  LOC-TYP-CD       OF P-DDDTDT01                       00277500
277600       MOVE  LOC-NBR          OF DCLXXXATION                      00277600
277700         TO  LOC-NBR          OF P-DDDTDT01                       00277700
277800       SET EXIT-PUT-PURGE-ROW TO TRUE                             00277800
277900       PERFORM 2070-CALL-STR-LOC-DEPT-DAO                         00277900
278000       IF SQLCODE = 100                                           00278000
278100         MOVE 0 TO SQLCODE                                        00278100
278200       END-IF                                                     00278200
278300     END-IF                                                       00278300
278400                                                                  00278400
278500     IF SQLCODE = 0                                               00278500
278600       EXEC SQL                                                   00278600
278700         DELETE FROM LST_BILED_CST                                00278700
278800         WHERE  STR_LOC_NUM = :DCLXXXATION.LOC-NBR                00278800
278900         AND    STR_LOC_TYP = :DCLXXXATION.LOC-TYP-CD             00278900
279000       END-EXEC                                                   00279000
279100       IF SQLCODE = 100                                           00279100
279200         MOVE 0 TO SQLCODE                                        00279200
279300       END-IF                                                     00279300
279400     END-IF                                                       00279400
279500                                                                  00279500
279600     .                                                            00279600
279700                                                                  00279700
279800                                                                  00279800
279900 1610-EXIT-PUT-PURGE-RETL.                                        00279900
280000     SET NOT-END-OF-DEL-CSR1 TO TRUE                              00280000
280100     PERFORM 1611-OPEN-DEL-CSR1                                   00280100
280200     IF SUCCESS                                                   00280200
280300       PERFORM UNTIL END-OF-DEL-CSR1 OR NOT SUCCESS               00280300
280400         PERFORM 1612-FETCH-DEL-CSR1                              00280400
280500         IF SUCCESS AND NOT-END-OF-DEL-CSR1                       00280500
280600           PERFORM 1613-EXIT-PURGE-RETL                           00280600
280700           PERFORM 115-CONNECT-TO-ORACLE                          00280700
280800         END-IF                                                   00280800
280900       END-PERFORM                                                00280900
281000     END-IF                                                       00281000
281100     PERFORM 1614-CLOSE-DEL-CSR1                                  00281100
281200     .                                                            00281200
281300                                                                  00281300
281400                                                                  00281400
281500 1611-OPEN-DEL-CSR1.                                              00281500
281600     MOVE LOC-TYP-CD                 OF DCLXXXATION               00281600
281700       TO LOC-TYP-CD                 OF DCLXXXL-LOC-CLS-AD-ZN     00281700
281800     MOVE LOC-NBR                    OF DCLXXXATION               00281800
281900       TO LOC-NBR                    OF DCLXXXL-LOC-CLS-AD-ZN     00281900
282000                                                                  00282000
282100     EXEC SQL                                                     00282100
282200       OPEN DEL-CSR1                                              00282200
282300     END-EXEC                                                     00282300
282400                                                                  00282400
282500     IF SQLCODE NOT = 0                                           00282500
282600       MOVE SQLCODE TO WS-SQLCODE                                 00282600
282700       SET  FAILURE TO TRUE                                       00282700
282800       MOVE SPACES  TO IS-RTRN-MSG-TXT                            00282800
282900       STRING 'NNNS0487 - ERROR OPENING DEL-CSR1, '               00282900
283000              'SQL=' WS-SQLCODE '.'                               00283000
283100              DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT              00283100
283200     END-IF                                                       00283200
283300     .                                                            00283300
283400                                                                  00283400
283500                                                                  00283500
283600 1612-FETCH-DEL-CSR1.                                             00283600
283700     EXEC SQL                                                     00283700
283800         FETCH DEL-CSR1                                           00283800
283900         INTO  :DCLXXXL-LOC-CLS-AD-ZN.ITM-CLS-CD                  00283900
284000     END-EXEC                                                     00284000
284100                                                                  00284100
284200     EVALUATE TRUE                                                00284200
284300       WHEN SQLCODE = 0                                           00284300
284400         CONTINUE                                                 00284400
284500       WHEN SQLCODE = 100                                         00284500
284600         SET  END-OF-DEL-CSR1 TO TRUE                             00284600
284700         MOVE 0 TO SQLCODE                                        00284700
284800       WHEN SQLCODE NOT = 0                                       00284800
284900         MOVE SQLCODE TO WS-SQLCODE                               00284900
285000         SET FAILURE TO TRUE                                      00285000
285100         MOVE SPACES  TO IS-RTRN-MSG-TXT                          00285100
285200         STRING 'NNNS0487 - ERROR FETCHING DEL-CSR1, '            00285200
285300                'SQLCODE=' WS-SQLCODE '.'                         00285300
285400                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT            00285400
285500     END-EVALUATE                                                 00285500
285600     .                                                            00285600
285700                                                                  00285700
285800                                                                  00285800
285900 1613-EXIT-PURGE-RETL.                                            00285900
286000     MOVE ITM-CLS-CD         OF DCLXXXL-LOC-CLS-AD-ZN             00286000
286100       TO ITM-CLS-CD         OF P-DDDTCZ01                        00286100
286200     MOVE LOC-TYP-CD         OF DCLXXXL-LOC-CLS-AD-ZN             00286200
286300       TO LOC-TYP-CD         OF P-DDDTCZ01                        00286300
286400     MOVE LOC-NBR            OF DCLXXXL-LOC-CLS-AD-ZN             00286400
286500       TO LOC-NBR            OF P-DDDTCZ01                        00286500
286600                                                                  00286600
286700     SET EXIT-PUT-PURGE-ROW TO TRUE                               00286700
286800     PERFORM 4000-NNNS0473-RETL-DAO                               00286800
286900                                                                  00286900
287000     EVALUATE TRUE                                                00287000
287100       WHEN SQLCODE = 0 OR 100                                    00287100
287200         MOVE 0 TO SQLCODE                                        00287200
287300       WHEN OTHER                                                 00287300
287400         MOVE SQLCODE TO WS-SQLCODE                               00287400
287500         SET FAILURE TO TRUE                                      00287500
287600         MOVE SPACES  TO IS-RTRN-MSG-TXT                          00287600
287700         MOVE SQLCODE TO WS-SQLCODE                               00287700
287800         STRING 'NNNS0487 - Error in delete of RETL'              00287800
287900                'LOC ,RC=' WS-SQLCODE '.'                         00287900
288000                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT            00288000
288100     END-EVALUATE                                                 00288100
288200     .                                                            00288200
288300                                                                  00288300
288400                                                                  00288400
288500 1614-CLOSE-DEL-CSR1.                                             00288500
288600     MOVE LOC-TYP-CD                 OF DCLXXXATION               00288600
288700       TO LOC-TYP-CD                 OF DCLXXXL-LOC-CLS-AD-ZN     00288700
288800     MOVE LOC-NBR                    OF DCLXXXATION               00288800
288900       TO LOC-NBR                    OF DCLXXXL-LOC-CLS-AD-ZN     00288900
289000                                                                  00289000
289100     EXEC SQL                                                     00289100
289200       CLOSE DEL-CSR1                                             00289200
289300     END-EXEC                                                     00289300
289400                                                                  00289400
289500     IF SQLCODE NOT = 0                                           00289500
289600       MOVE SQLCODE TO WS-SQLCODE                                 00289600
289700       SET  FAILURE TO TRUE                                       00289700
289800       MOVE SPACES  TO IS-RTRN-MSG-TXT                            00289800
289900       STRING 'NNNS0487 - Error closing DEL-CSR1, '               00289900
290000              'SQL=' WS-SQLCODE '.'                               00290000
290100              DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT              00290100
290200     END-IF                                                       00290200
290300     .                                                            00290300
290400                                                                  00290400
290500                                                                  00290500
290600 1620-DELETE-DSD-VENDOR.                                          00290600
290700     PERFORM 1630-GET-AP-KEY                                      00290700
290800                                                                  00290800
290900     IF SUCCESS                                                   00290900
291000       EXEC SQL                                                   00291000
291100         DELETE FROM LST_BILED_CST                                00291100
291200         WHERE  SPLR_LOC_NBR    = :DCLXXXATION.LOC-NBR            00291200
291300         AND    SPLR_LOC_TYP_CD = :DCLXXXATION.LOC-TYP-CD         00291300
291400       END-EXEC                                                   00291400
291500     END-IF                                                       00291500
291600     IF SQLCODE = 100                                             00291600
291700       MOVE 0 TO SQLCODE                                          00291700
291800     END-IF                                                       00291800
291900                                                                  00291900
292000     IF SUCCESS                                                   00292000
292100       SET EXIT-PUT-PURGE-ROW TO TRUE                             00292100
292200       MOVE LOC-NBR    OF DCLXXXATION                             00292200
292300         TO LOC-NBR    OF P-RMDTLD01                              00292300
292400       MOVE LOC-TYP-CD OF DCLXXXATION                             00292400
292500         TO LOC-TYP-CD OF P-RMDTLD01                              00292500
292600       PERFORM 1680-NNNS0486-DSD-LOC-DAO                          00292600
292700     END-IF                                                       00292700
292800     IF SQLCODE = 100                                             00292800
292900       MOVE 0 TO SQLCODE                                          00292900
293000     END-IF                                                       00293000
293100                                                                  00293100
293200     IF  SUCCESS                                                  00293200
293300     AND SQLCODE = 0                                              00293300
293400     AND WS-AP-NUM > 0                                            00293400
293500     MOVE WS-AP-NUM                                               00293500
293600       TO AP-NBR    OF P-DDDTAP01                                 00293600
293700     MOVE WS-AP-TYPE                                              00293700
293800       TO AP-TYP-CD OF P-DDDTAP01                                 00293800
293900     SET EXIT-PUT-PURGE-ROW TO TRUE                               00293900
294000     PERFORM 1670-NNNS0457-AP-LOC-DAO                             00294000
294100       IF SQLCODE NOT = 0                                         00294100
294200         SET FAILURE TO TRUE                                      00294200
294300         MOVE 'NNNS0487 - Could not delete AP entry for vendor!'  00294300
294400           TO IS-RTRN-MSG-TXT                                     00294400
294500       END-IF                                                     00294500
294600     END-IF                                                       00294600
294700                                                                  00294700
294800     .                                                            00294800
294900                                                                  00294900
295000                                                                  00295000
295100 1630-GET-AP-KEY.                                                 00295100
295200     EXEC SQL                                                     00295200
295300       SELECT AP_NBR,                                             00295300
295400              AP_TYP_CD                                           00295400
295500         INTO :WS-AP-NUM  :WS-AP-NBR-IND,                         00295500
295600              :WS-AP-TYPE :WS-AP-TYP-CD-IND                       00295600
295700       FROM   XXXATION                                            00295700
295800       WHERE  LOC_TYP_CD = :DCLXXXATION.LOC-TYP-CD                00295800
295900       AND    LOC_NBR    = :DCLXXXATION.LOC-NBR                   00295900
296000     END-EXEC                                                     00296000
296100                                                                  00296100
296200     EVALUATE TRUE                                                00296200
296300       WHEN SQLCODE = 0                                           00296300
296400         IF WS-AP-NBR-IND < 0                                     00296400
296500         OR WS-AP-TYP-CD-IND < 0                                  00296500
296600           INITIALIZE WS-AP-NUM                                   00296600
296700                      WS-AP-TYPE                                  00296700
296800         END-IF                                                   00296800
296900       WHEN SQLCODE = 100                                         00296900
297000         INITIALIZE WS-AP-NUM                                     00297000
297100                    WS-AP-TYPE                                    00297100
297200         MOVE 0 TO SQLCODE                                        00297200
297300       WHEN OTHER                                                 00297300
297400         SET FAILURE           TO TRUE                            00297400
297500         MOVE SQLCODE          TO WS-SQLCODE                      00297500
297600         STRING 'NNNS0487 - Error getting XXXATION AP Key,'       00297600
297700                ',SQL=' WS-SQLCODE                                00297700
297800                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT            00297800
297900     END-EVALUATE                                                 00297900
298000     .                                                            00298000
298100                                                                  00298100
298200                                                                  00298200
298300 1640-DELETE-BKHAUL.                                              00298300
298400                                                                  00298400
298500     PERFORM 5000-CALL-MMMU0487-CUD-ROUTINE                       00298500
298600                                                                  00298600
298700     IF SQLCODE = 100                                             00298700
298800       MOVE 0 TO SQLCODE                                          00298800
298900     END-IF                                                       00298900
299000     .                                                            00299000
299100                                                                  00299100
299200                                                                  00299200
299300 1650-DELETE-TERMINAL.                                            00299300
299400                                                                  00299400
299500     PERFORM 5000-CALL-MMMU0487-CUD-ROUTINE                       00299500
299600                                                                  00299600
299700     IF SQLCODE = 100                                             00299700
299800       MOVE 0 TO SQLCODE                                          00299800
299900     END-IF                                                       00299900
300000     .                                                            00300000
300100                                                                  00300100
300200                                                                  00300200
300300 1660-DELETE-WHSE.                                                00300300
300400                                                                  00300400
300500     PERFORM 5000-CALL-MMMU0487-CUD-ROUTINE                       00300500
300600                                                                  00300600
300700     IF SQLCODE = 100                                             00300700
300800       MOVE 0 TO SQLCODE                                          00300800
300900     END-IF                                                       00300900
301000     .                                                            00301000
301100                                                                  00301100
301200 1670-NNNS0457-AP-LOC-DAO.                                        00301200
301300     CALL NNNS0457-AP-LOC-DAO USING                               00301300
301400          XXXN001A                                                00301400
301500          SQLCA                                                   00301500
301600          YYYN005A                                                00301600
301700          NNNN0000-PARMS                                          00301700
301800          P-DDDTAP01                                              00301800
301900     .                                                            00301900
302000                                                                  00302000
302100 1680-NNNS0486-DSD-LOC-DAO.                                       00302100
302200     CALL NNNS0486-DSD-LOC-DAO USING                              00302200
302300          XXXN001A                                                00302300
302400          SQLCA                                                   00302400
302500          YYYN005A                                                00302500
302600          NNNN0000-PARMS                                          00302600
302700          P-RMDTLD01                                              00302700
302800     .                                                            00302800
302900                                                                  00302900
303000                                                                  00303000
303100 1690-DELETE-LO.                                                  00303100
303200     PERFORM 1691-EXIT-PUT-PURGE-FAX-NUM                          00303200
303300                                                                  00303300
303400     PERFORM 4500-CALL-MMMS0304-RI-DEL-CHK                        00303400
303500     IF SUCCESS                                                   00303500
303600        PERFORM 5000-CALL-NNNS0487-CUD-ROUTINE                    00303600
303700                                                                  00303700
303800        IF SQLCODE = 100                                          00303800
303900           MOVE 0 TO SQLCODE                                      00303900
304000        END-IF                                                    00304000
304100     END-IF                                                       00304100
304200     .                                                            00304200
304300                                                                  00304300
304400                                                                  00304400
304500 1691-EXIT-PUT-PURGE-FAX-NUM.                                     00304500
304600     SET NOT-END-OF-DEL-CSR TO TRUE                               00304600
304700     PERFORM 1692-OPEN-DEL-CSR                                    00304700
304800     IF SUCCESS                                                   00304800
304900       PERFORM UNTIL END-OF-DEL-CSR OR NOT SUCCESS                00304900
305000         PERFORM 1693-FETCH-DEL-CSR                               00305000
305100         IF SUCCESS AND NOT-END-OF-DEL-CSR                        00305100
305200           PERFORM 1694-EXIT-PURGE-FAX-NUM                        00305200
305300           PERFORM 115-CONNECT-TO-ORACLE                          00305300
305400         END-IF                                                   00305400
305500       END-PERFORM                                                00305500
305600     END-IF                                                       00305600
305700     PERFORM 1695-CLOSE-DEL-CSR                                   00305700
305800     .                                                            00305800
305900                                                                  00305900
306000                                                                  00306000
306100 1692-OPEN-DEL-CSR.                                               00306100
306200     MOVE LOC-TYP-CD                 OF DCLXXXATION               00306200
306300       TO LOC-TYP-CD                 OF DCLXXX-NUMBERS            00306300
306400     MOVE LOC-NBR                    OF DCLXXXATION               00306400
306500       TO LOC-NBR                    OF DCLXXX-NUMBERS            00306500
306600                                                                  00306600
306700     EXEC SQL                                                     00306700
306800       OPEN DEL-CSR                                               00306800
306900     END-EXEC                                                     00306900
307000                                                                  00307000
307100     IF SQLCODE NOT = 0                                           00307100
307200       MOVE SQLCODE TO WS-SQLCODE                                 00307200
307300       SET  FAILURE TO TRUE                                       00307300
307400       MOVE SPACES  TO IS-RTRN-MSG-TXT                            00307400
307500       STRING 'NNNS0487 - ERROR OPENING DEL-CSR, '                00307500
307600              'SQL=' WS-SQLCODE '.'                               00307600
307700              DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT              00307700
307800     END-IF                                                       00307800
307900     .                                                            00307900
308000                                                                  00308000
308100                                                                  00308100
308200 1693-FETCH-DEL-CSR.                                              00308200
308300     EXEC SQL                                                     00308300
308400         FETCH DEL-CSR                                            00308400
308500         INTO  :DCLXXX-NUMBERS.FAX-ID                             00308500
308600     END-EXEC                                                     00308600
308700                                                                  00308700
308800     EVALUATE TRUE                                                00308800
308900       WHEN SQLCODE = 0                                           00308900
309000         CONTINUE                                                 00309000
309100       WHEN SQLCODE = 100                                         00309100
309200         SET  END-OF-DEL-CSR TO TRUE                              00309200
309300         MOVE 0 TO SQLCODE                                        00309300
309400       WHEN SQLCODE NOT = 0                                       00309400
309500         MOVE SQLCODE TO WS-SQLCODE                               00309500
309600         SET FAILURE TO TRUE                                      00309600
309700         MOVE SPACES  TO IS-RTRN-MSG-TXT                          00309700
309800         STRING 'NNNS0487 - ERROR FETCHING DEL-CSR, '             00309800
309900                'SQLCODE=' WS-SQLCODE '.'                         00309900
310000                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT            00310000
310100     END-EVALUATE                                                 00310100
310200     .                                                            00310200
310300                                                                  00310300
310400                                                                  00310400
310500 1694-EXIT-PURGE-FAX-NUM.                                         00310500
310600                                                                  00310600
310700     MOVE FAX-ID             OF DCLXXX-NUMBERS                    00310700
310800       TO FAX-ID             OF P-DDDTFX01                        00310800
310900     MOVE LOC-TYP-CD         OF DCLXXX-NUMBERS                    00310900
311000       TO LOC-TYP-CD         OF P-DDDTFX01                        00311000
311100     MOVE LOC-NBR            OF DCLXXX-NUMBERS                    00311100
311200       TO LOC-NBR            OF P-DDDTFX01                        00311200
311300     SET EXIT-PUT-PURGE-ROW TO TRUE                               00311300
311400     PERFORM 3000-NNNS0483-FAX-DAO                                00311400
311500                                                                  00311500
311600     EVALUATE TRUE                                                00311600
311700       WHEN SQLCODE = 0 OR 100                                    00311700
311800         MOVE 0 TO SQLCODE                                        00311800
311900       WHEN OTHER                                                 00311900
312000         MOVE SQLCODE TO WS-SQLCODE                               00312000
312100         SET FAILURE TO TRUE                                      00312100
312200         MOVE SPACES  TO IS-RTRN-MSG-TXT                          00312200
312300         MOVE SQLCODE TO WS-SQLCODE                               00312300
312400         STRING 'NNNS0487 - ERROR IN DELETE OF FAX'               00312400
312500                'PROD ,RC=' WS-SQLCODE '.'                        00312500
312600                DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT            00312600
312700     END-EVALUATE                                                 00312700
312800     .                                                            00312800
312900                                                                  00312900
313000                                                                  00313000
313100 1695-CLOSE-DEL-CSR.                                              00313100
313200     MOVE LOC-TYP-CD                 OF DCLXXXATION               00313200
313300       TO LOC-TYP-CD                 OF DCLXXX-NUMBERS            00313300
313400     MOVE LOC-NBR                    OF DCLXXXATION               00313400
313500       TO LOC-NBR                    OF DCLXXX-NUMBERS            00313500
313600                                                                  00313600
313700     EXEC SQL                                                     00313700
313800       CLOSE DEL-CSR                                              00313800
313900     END-EXEC                                                     00313900
314000                                                                  00314000
314100     IF SQLCODE NOT = 0                                           00314100
314200       MOVE SQLCODE TO WS-SQLCODE                                 00314200
314300       SET  FAILURE TO TRUE                                       00314300
314400       MOVE SPACES  TO IS-RTRN-MSG-TXT                            00314400
314500       STRING 'NNNS0487 - ERROR CLOSING DEL-CSR, '                00314500
314600              'SQL=' WS-SQLCODE '.'                               00314600
314700              DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT              00314700
314800     END-IF                                                       00314800
314900     .                                                            00314900
315000                                                                  00315000
315100                                                                  00315100
315200                                                                  00315200
315300* ================================================================00315300
315400* Initialize NULL variables if the column is set to NULL.         00315400
315500* ================================================================00315500
315600 1700-CHECK-NULL-COLUMNS.                                         00315600
315700     IF WS-AP-NBR-IND    < 0                                      00315700
315800     OR WS-AP-TYP-CD-IND < 0                                      00315800
315900       MOVE SPACES TO AP-TYP-CD OF DCLXXXATION                    00315900
316000       MOVE 0      TO AP-NBR    OF DCLXXXATION                    00316000
316100     END-IF                                                       00316100
316200     .                                                            00316200
316300                                                                  00316300
316400                                                                  00316400
316500* ================================================================00316500
316600* Make sure the null indicators are valid.                        00316600
316700* ================================================================00316700
316800 1800-EDIT-NULL-INDICATORS.                                       00316800
316900     INITIALIZE WS-AP-NBR-IND                                     00316900
317000                WS-AP-TYP-CD-IND                                  00317000
317100                                                                  00317100
317200     IF AP-TYP-CD OF DCLXXXATION   = SPACES                       00317200
317300     OR AP-NBR    OF DCLXXXATION   = 0                            00317300
317400       MOVE -1 TO WS-AP-NBR-IND                                   00317400
317500       MOVE -1 TO WS-AP-TYP-CD-IND                                00317500
317600     END-IF                                                       00317600
317700     .                                                            00317700
317800                                                                  00317800
317900                                                                  00317900
318000* ================================================================00318000
318100* Misc functions...                                               00318100
318200* ================================================================00318200
318300 2000-DENORM-PROCESS.                                             00318300
318400     MOVE 1                TO WS-CHECKPOINT-INC                   00318400
318500     MOVE YYYN005A-SYS-ENV TO YYYN110A-SYS-ENV                    00318500
318600     PERFORM 2010-CALL-CONTROL-SUBR                               00318600
318700     IF  SUCCESS                                                  00318700
318800     AND WWWC0100-NORM-TASK                                       00318800
318900       PERFORM 2020-CALL-SYNC-SUBR                                00318900
319000       IF SUCCESS                                                 00319000
319100          PERFORM 2030-ISSUE-EVENTS                               00319100
319200       END-IF                                                     00319200
319300     END-IF                                                       00319300
319400     .                                                            00319400
319500                                                                  00319500
319600                                                                  00319600
319700                                                                  00319700
319800 2010-CALL-CONTROL-SUBR.                                          00319800
319900     SET WWWC0100-GET-TASK  TO TRUE                               00319900
320000     CALL WWWS0100-CONTROL-SUBR USING                             00320000
320100         XXXN001A                                                 00320100
320200         WWWC0100                                                 00320200
320300     .                                                            00320300
320400                                                                  00320400
320500                                                                  00320500
320600 2020-CALL-SYNC-SUBR.                                             00320600
320700     SET YYYN110A-ORACLE        TO TRUE                           00320700
320800     EVALUATE TRUE                                                00320800
320900       WHEN LOC-TYP-CD OF DCLXXXATION = K-VEND-LOC-TYPE           00320900
321000         SET MMMC0135-LO-IS-CURRENT TO TRUE                       00321000
321100         CALL MMMS0135-SYNC-LO USING                              00321100
321200           XXXN001A                                               00321200
321300           YYYN110A                                               00321300
321400           MMMC0135                                               00321400
321500           P-DDDTLO01                                             00321500
321600       WHEN LOC-TYP-CD OF DCLXXXATION = K-STORE-LOC-TYPE          00321600
321700       OR   LOC-TYP-CD OF DCLXXXATION = K-ACCT-LOC-TYPE           00321700
321800         SET MMMC0157-LO-IS-CURRENT TO TRUE                       00321800
321900         CALL MMMS0157-SYNC-LO USING                              00321900
322000           XXXN001A                                               00322000
322100           YYYN110A                                               00322100
322200           MMMC0157                                               00322200
322300           P-DDDTLO01                                             00322300
322400       WHEN LOC-TYP-CD OF DCLXXXATION = K-DSD-VEND-LOC-TYPE       00322400
322500         CONTINUE                                                 00322500
322600*        SET MMMC0153-LO-IS-CURRENT TO TRUE                       00322600
322700*        CALL MMMS0153-SYNC-LO USING                              00322700
322800*          XXXN001A                                               00322800
322900*          YYYN110A                                               00322900
323000*          MMMC0153                                               00323000
323100*          P-DDDTLO01                                             00323100
323200       WHEN LOC-TYP-CD OF DCLXXXATION = 'B'                       00323200
323300       OR   LOC-TYP-CD OF DCLXXXATION = 'T'                       00323300
323400       OR   LOC-TYP-CD OF DCLXXXATION = 'W'                       00323400
323500       OR   LOC-TYP-CD OF DCLXXXATION = 'O'                       00323500
323600         SET  MMMC0265-LO-LEVEL TO TRUE                           00323600
323700         MOVE LOC-TYP-CD OF DCLXXXATION TO LOC-TYP-CD OF MMMC0265 00323700
323800         MOVE LOC-NBR    OF DCLXXXATION TO LOC-NBR    OF MMMC0265 00323800
323900         CALL WS-MMMS0265-SYNC USING                              00323900
324000             XXXN001A                                             00324000
324100             MMMC0265                                             00324100
324200     END-EVALUATE                                                 00324200
324300     .                                                            00324300
324400                                                                  00324400
324500                                                                  00324500
324600 2030-ISSUE-EVENTS.                                               00324600
324700     INITIALIZE ZZZC0550-IN-DATA                                  00324700
324800     SET  YYYN110A-ORACLE       TO TRUE                           00324800
324900     IF DELETE-OPERATION                                          00324900
325000       SET VENDOR-DELETE-EVENT  TO TRUE                           00325000
325100     ELSE                                                         00325100
325200       SET VENDOR-EVENT TO TRUE                                   00325200
325300     END-IF                                                       00325300
325400     MOVE LOC-TYP-CD       OF DCLXXXATION                         00325400
325500       TO VEND-TYP-CD      OF ZZZC0550-VENDOR-DATA                00325500
325600     MOVE LOC-NBR          OF DCLXXXATION                         00325600
325700       TO VEND-NBR         OF ZZZC0550-VENDOR-DATA                00325700
325800     MOVE 'NNNS0487'            TO ZZZC0197-PROGRAM               00325800
325900     MOVE YYYC0107-USER         TO ZZZC0197-USER                  00325900
326000     MOVE YYYN005A-SYS-ENV      TO YYYN110A-SYS-ENV               00326000
326100     MOVE LOC-NBR OF P-DDDTLO01 TO WS-OLD-KEY                     00326100
326200                                                                  00326200
326300     EVALUATE TRUE                                                00326300
326400       WHEN LOC-TYP-CD OF DCLXXXATION = K-VEND-LOC-TYPE           00326400
326500         MOVE WS-VENDOR-FACILITY                                  00326500
326600           TO VM-VENDOR-FACILITY OF ZZZC0020                      00326600
326700         MOVE WS-VENDOR-NUMBER                                    00326700
326800           TO VM-VENDOR-NUMBER   OF ZZZC0020                      00326800
326900         MOVE ZZZC0020              TO ZZZC0197-TRX-REC           00326900
327000         MOVE 'VSHP'                TO ZZZC0197-TRX-ID            00327000
327100         CALL ZZZS0197-EVENT-MGR USING                            00327100
327200             XXXN001A                                             00327200
327300             YYYN110A                                             00327300
327400             ZZZC0197                                             00327400
327500                                                                  00327500
327600         MOVE LOC-TYP-CD OF DCLXXXATION                           00327600
327700           TO LOC-VEN-TYP-CD OF ZZZC0124                          00327700
327800         MOVE LOC-NBR OF DCLXXXATION                              00327800
327900           TO LOC-VEN-NBR OF ZZZC0124                             00327900
328000         MOVE ZZZC0124              TO ZZZC0197-TRX-REC           00328000
328100         MOVE 'VENM'                TO ZZZC0197-TRX-ID            00328100
328200         CALL ZZZS0197-EVENT-MGR USING                            00328200
328300             XXXN001A                                             00328300
328400             YYYN110A                                             00328400
328500             ZZZC0197                                             00328500
328600                                                                  00328600
328700       WHEN LOC-TYP-CD OF DCLXXXATION = K-STORE-LOC-TYPE          00328700
328800         MOVE LOC-NBR OF DCLXXXATION                              00328800
328900           TO ST-STORE-NUMBER OF ZZZC0032                         00328900
329000              LOC-NBR OF ZZZC0094                                 00329000
329100         SET  ZZZC0032-UPD-FXXX TO TRUE                           00329100
329200         MOVE ZZZC0032          TO ZZZC0197-TRX-REC               00329200
329300         MOVE 'CUST'            TO ZZZC0197-TRX-ID                00329300
329400         CALL ZZZS0197-EVENT-MGR USING                            00329400
329500             XXXN001A                                             00329500
329600             YYYN110A                                             00329600
329700             ZZZC0197                                             00329700
329800                                                                  00329800
329900         MOVE LOC-TYP-CD OF DCLXXXATION TO                        00329900
330000                                 LOC-TYP-CD OF ZZZC0094           00330000
330100         MOVE ZZZC0094              TO ZZZC0197-TRX-REC           00330100
330200         MOVE 'STRM'                TO ZZZC0197-TRX-ID            00330200
330300         CALL ZZZS0197-EVENT-MGR USING                            00330300
330400              XXXN001A                                            00330400
330500              YYYN110A                                            00330500
330600              ZZZC0197                                            00330600
330700                                                                  00330700
330800       WHEN LOC-TYP-CD OF DCLXXXATION = K-DSD-VEND-LOC-TYPE       00330800
330900         MOVE LOC-TYP-CD OF DCLXXXATION TO                        00330900
331000                                 LOC-DSD-TYP-CD OF ZZZC0122       00331000
331100         MOVE ZZZC0122              TO ZZZC0197-TRX-REC           00331100
331200         MOVE 'DSDM'                TO ZZZC0197-TRX-ID            00331200
331300         CALL ZZZS0197-EVENT-MGR USING                            00331300
331400              XXXN001A                                            00331400
331500              YYYN110A                                            00331500
331600              ZZZC0197                                            00331600
331700                                                                  00331700
331800       WHEN LOC-TYP-CD OF DCLXXXATION = K-WHSE-LOC-TYPE           00331800
331900         MOVE LOC-NBR OF DCLXXXATION                              00331900
332000           TO LOC-WHS-NBR OF ZZZC0123                             00332000
332100         MOVE LOC-TYP-CD OF DCLXXXATION TO                        00332100
332200              LOC-WHS-TYP-CD OF ZZZC0123                          00332200
332300*        SET  ZZZC0044-UPD-FXXX TO TRUE                           00332300
332400         MOVE ZZZC0123          TO ZZZC0197-TRX-REC               00332400
332500         MOVE 'WHSM'            TO ZZZC0197-TRX-ID                00332500
332600         CALL ZZZS0197-EVENT-MGR USING                            00332600
332700             XXXN001A                                             00332700
332800             YYYN110A                                             00332800
332900             ZZZC0197                                             00332900
333000                                                                  00333000
333100     END-EVALUATE                                                 00333100
333200     IF STAGE-EVENT AND WWWC0100-CREATE-SCAN-EVENT                00333200
333300       AND LOC-TYP-CD OF DCLXXXATION = 'D'                        00333300
333400         MOVE ZZZC0550              TO ZZZC0197-TRX-REC           00333400
333500         MOVE ZZZC0550-TRX          TO ZZZC0197-TRX-ID            00333500
333600                                       ZZZC0197-TRX-CD            00333600
333700         CALL ZZZS0197-EVENT-MGR USING                            00333700
333800              XXXN001A                                            00333800
333900              YYYN110A                                            00333900
334000              ZZZC0197                                            00334000
334100     END-IF                                                       00334100
334200     .                                                            00334200
334300                                                                  00334300
334400                                                                  00334400
334500 2040-GET-DATE-AND-USER.                                          00334500
334600     CALL Z-DATE-FUNCTIONS USING                                  00334600
334700         XXXN001A                                                 00334700
334800         YYYC0127                                                 00334800
334900                                                                  00334900
335000     IF  SUCCESS                                                  00335000
335100     AND YYYN005A-CICS-ENV                                        00335100
335200       CALL Z-GET-CICS-USER-ID USING                              00335200
335300           EIBLK    WS-DUMMY                                      00335300
335400           XXXN001A YYYC0107                                      00335400
335500     ELSE                                                         00335500
335600       MOVE 'BATCH' TO YYYC0107-USER                              00335600
335700     END-IF                                                       00335700
335800     .                                                            00335800
335900                                                                  00335900
336000                                                                  00336000
336100 2050-DO-AVP-MAINTENANCE.                                         00336100
336200     MOVE LOC-TYP-CD               OF DCLXXXATION                 00336200
336300       TO MMMC0474-LOC-TYP-CD                                     00336300
336400     MOVE LOC-NBR                  OF DCLXXXATION                 00336400
336500       TO MMMC0474-LOC-NBR                                        00336500
336600     MOVE AP-TYP-CD                OF DCLXXXATION                 00336600
336700       TO MMMC0474-NEW-AP-TYP                                     00336700
336800     MOVE AP-NBR                   OF DCLXXXATION                 00336800
336900       TO MMMC0474-NEW-AP-NBR                                     00336900
337000     SET MMMC0474-LO               TO TRUE                        00337000
337100     MOVE 'NNNS0487'                                              00337100
337200       TO MMMC0474-PROGRAM                                        00337200
337300     MOVE YYYN005A-SYS-ENV                                        00337300
337400       TO MMMC0474-SYS-ENV                                        00337400
337500                                                                  00337500
337600     CALL MMMS0474-DO-AVP-MAIN USING                              00337600
337700          XXXN001A                                                00337700
337800          MMMC0474                                                00337800
337900     .                                                            00337900
338000                                                                  00338000
338100                                                                  00338100
338200 2060-CALL-RETAIL-LOC-DAO.                                        00338200
338300     CALL NNNS0488-RETAIL-LOC-DAO USING                           00338300
338400          XXXN001A                                                00338400
338500          SQLCA                                                   00338500
338600          YYYN005A                                                00338600
338700          NNNN0000-PARMS                                          00338700
338800          P-DDDTLR01                                              00338800
338900     .                                                            00338900
339000                                                                  00339000
339100                                                                  00339100
339200 2070-CALL-STR-LOC-DEPT-DAO.                                      00339200
339300     CALL NNNS0575-STR-LOC-DEPT-DAO USING                         00339300
339400          XXXN001A                                                00339400
339500          SQLCA                                                   00339500
339600          YYYN005A                                                00339600
339700          NNNN0000-PARMS                                          00339700
339800          P-DDDTDT01                                              00339800
339900     .                                                            00339900
340000                                                                  00340000
340100                                                                  00340100
340200 3000-NNNS0483-FAX-DAO.                                           00340200
340300     CALL NNNS0483-FAX-DAO USING                                  00340300
340400          XXXN001A                                                00340400
340500          SQLCA                                                   00340500
340600          YYYN005A                                                00340600
340700          NNNN0000-PARMS                                          00340700
340800          P-DDDTFX01                                              00340800
340900     .                                                            00340900
341000 4000-NNNS0473-RETL-DAO.                                          00341000
341100     CALL NNNS0473-RETL-DAO USING                                 00341100
341200          XXXN001A                                                00341200
341300          SQLCA                                                   00341300
341400          YYYN005A                                                00341400
341500          NNNN0000-PARMS                                          00341500
341600          P-DDDTCZ01                                              00341600
341700     .                                                            00341700
341800                                                                  00341800
341900                                                                  00341900
342000 4100-CALL-MECY-DAO.                                              00342000
342100     CALL NNNS2294-MECY-DAO USING                                 00342100
342200          XXXN001A                                                00342200
342300          SQLCA                                                   00342300
342400          YYYN005A                                                00342400
342500          NNNN0000-PARMS                                          00342500
342600          P-DDDTCY01                                              00342600
342700     .                                                            00342700
342800                                                                  00342800
342900                                                                  00342900
343000 4500-CALL-MMMS0304-RI-DEL-CHK.                                   00343000
343100     INITIALIZE MMMC0304                                          00343100
343200     MOVE LOC-TYP-CD OF DCLXXXATION    TO                         00343200
343300                                      MMMC0304-LOC-TYP-CD         00343300
343400     MOVE LOC-NBR OF DCLXXXATION      TO                          00343400
343500                                      MMMC0304-LOC-NBR            00343500
343600     SET MMMC0304-DELETE-CHECK TO TRUE                            00343600
343700     SET MMMC0304-XXXATION     TO TRUE                            00343700
343800     SET MMMC0304-ORACLE       TO TRUE                            00343800
343900     CALL MMMS0304-RI-DEL-CHK USING                               00343900
344000          XXXN001A                                                00344000
344100          MMMC0304                                                00344100
344200     .                                                            00344200
344300                                                                  00344300
344400                                                                  00344400
344500 5000-CALL-NNNS0487-CUD-ROUTINE.                                  00344500
344600     CALL NNNU0487-ORACLE-UPDATE USING                            00344600
344700          XXXN001A                                                00344700
344800          SQLCA                                                   00344800
344900          YYYN005A                                                00344900
345000          NNNN0000-PARMS                                          00345000
345100          DDDTLO01                                                00345100
345200          WS-ORD-PROCNG-CTOF-TM                                   00345200
345300          WS-FILLER1-TS                                           00345300
345400          WS-FILLER2-TS                                           00345400
345500          WS-AP-NBR-IND                                           00345500
345600          WS-AP-TYP-CD-IND                                        00345600
345700     .                                                            00345700
345800                                                                  00345800
345900                                                                  00345900
346000 4010-REP-LOWVALUE-WITH-SPACES.                                   00346000
346100     IF PRIM-CONTACT-NM   OF DCLXXXATION EQUAL LOW-VALUES         00346100
346200        MOVE SPACES       TO PRIM-CONTACT-NM   OF DCLXXXATION     00346200
346300     END-IF                                                       00346300
346400     IF PRIM-ADR-2        OF DCLXXXATION EQUAL LOW-VALUES         00346400
346500        MOVE SPACES       TO PRIM-ADR-2        OF DCLXXXATION     00346500
346600     END-IF                                                       00346600
346700     .                                                            00346700
346800                                                                  00346800
346900                                                                  00346900
347000 5000-CALL-MMMU0487-CUD-ROUTINE.                                  00347000
347100     CALL MMMU0487-ORACLE-UPDATE USING                            00347100
347200          XXXN001A                                                00347200
347300          SQLCA                                                   00347300
347400          YYYN005A                                                00347400
347500          NNNN0000-PARMS                                          00347500
347600          DDDTLO01                                                00347600
347700     .                                                            00347700
347800                                                                  00347800
347900                                                                  00347900
348000* ================================================================00348000
348100* Special sql or functions to be performed by this subroutine.    00348100
348200* ================================================================00348200
348300 10000-DO-SPECIAL-IO-FUNCS.                                       00348300
348400     EXIT                                                         00348400
348500     .                                                            00348500
348600                                                                  00348600
348700                                                                  00348700
348800 10100-CHECK-FOR-VNDR-EVENTS.                                     00348800
348900     IF ((WS-MAIL-TO-LOC-NM  NOT = MAIL-TO-LOC-NM                 00348900
349000                                         OF DCLXXXATION ) OR      00349000
349100         (WS-MAIL-TO-ADR-1   NOT = MAIL-TO-ADR-1                  00349100
349200                                         OF DCLXXXATION ) OR      00349200
349300         (WS-MAIL-TO-ADR-2   NOT = MAIL-TO-ADR-2                  00349300
349400                                         OF DCLXXXATION ) OR      00349400
349500         (WS-MAIL-TO-CITY    NOT = MAIL-TO-CITY                   00349500
349600                                         OF DCLXXXATION ) OR      00349600
349700         (WS-MAIL-TO-STATE-CD NOT = MAIL-TO-STATE-CD              00349700
349800                                         OF DCLXXXATION ) OR      00349800
349900         (WS-SEC-CONTACT-NM   NOT = SEC-CONTACT-NM                00349900
350000                                         OF DCLXXXATION ) OR      00350000
350100         (WS-MAIL-TO-ZIP5-CD  NOT = MAIL-TO-ZIP5-CD               00350100
350200                                         OF DCLXXXATION ) OR      00350200
350300         (WS-MAIL-TO-ZIP4-CD  NOT = MAIL-TO-ZIP4-CD               00350300
350400                                         OF DCLXXXATION ) OR      00350400
350500         (WS-MAIL-TO-PHONE-NBR NOT = MAIL-TO-PHONE-NBR            00350500
350600                                         OF DCLXXXATION ))        00350600
350700         SET STAGE-EVENT TO TRUE                                  00350700
350800     ELSE                                                         00350800
350900         SET DONT-STAGE-EVENT TO TRUE                             00350900
351000     END-IF                                                       00351000
351100     .                                                            00351100
351200                                                                  00351200
351300                                                                  00351300
351400 10200-CHECK-DSV-LOC-SUB-TYP.                                     00351400
351500     MOVE LOC-NBR    OF DCLXXXATION TO MMMC0711-I-VEND-NBR        00351500
351600     MOVE LOC-TYP-CD OF DCLXXXATION TO MMMC0711-I-VEND-TYP-CD     00351600
351700     SET VEND-IS-NOT-DSV            TO TRUE                       00351700
351800     SET MMMC0711-IS-DSV-FUNC       TO TRUE                       00351800
351900                                                                  00351900
352000     CALL MMMS0711-DSV-CHECK USING                                00352000
352100          XXXN001A                                                00352100
352200          MMMC0711                                                00352200
352300     IF SUCCESS AND VEND-IS-DSV                                   00352300
352400        SET IS-DSV-VEND TO TRUE                                   00352400
352500     END-IF                                                       00352500
352600     .                                                            00352600
352700                                                                  00352700
352800                                                                  00352800
352900 10300-CHECK-FOR-VALID-COUNTY.                                    00352900
353000     IF PRIM-CNTY-TXT OF DCLXXXATION NOT = SPACES                 00353000
353100       MOVE PRIM-CNTY-TXT OF DCLXXXATION TO CNTY-TXT OF P-DDDTCY0100353100
353200                                                                  00353200
353300       SET EXIT-GET-UNIQUE-ROW TO TRUE                            00353300
353400       PERFORM 4100-CALL-MECY-DAO                                 00353400
353500                                                                  00353500
353600       EVALUATE TRUE                                              00353600
353700         WHEN NOT SUCCESS                                         00353700
353800           CONTINUE                                               00353800
353900         WHEN SQLCODE = 0                                         00353900
354000           CONTINUE                                               00354000
354100         WHEN SQLCODE = 100                                       00354100
354200           SET  FAILURE TO TRUE                                   00354200
354300           MOVE SPACES  TO IS-RTRN-MSG-TXT                        00354300
354400           STRING 'NNNS0487 - County Not Found! '                 00354400
354500               DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT             00354500
354600         WHEN OTHER                                               00354600
354700           SET  FAILURE TO TRUE                                   00354700
354800           MOVE SPACES  TO IS-RTRN-MSG-TXT                        00354800
354900           MOVE SQLCODE TO WS-SQLCODE                             00354900
355000           STRING 'NNNS0487 - Error Getting COUNTY! SQL '         00355000
355100                  WS-SQLCODE '.'                                  00355100
355200               DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT             00355200
355300       END-EVALUATE                                               00355300
355400     END-IF                                                       00355400
355500     .                                                            00355500
355600                                                                  00355600
355700                                                                  00355700
