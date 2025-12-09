
           EXEC SQL DECLARE FC_XXXAIL_STORES TABLE
           ( FC_STORE_NO                    DECIMAL(5, 0) NOT NULL,
             FC_RL_STORE_NM                 CHAR(30) NOT NULL,
             FC_RL_STORE_CD                 CHAR(1) NOT NULL,
             FC_RL_STORE_DIR_NM             CHAR(30) NOT NULL,
             FC_RL_STORE_LOC_NM             CHAR(30) NOT NULL,
             FC_RL_OPENING_DT               DATE NOT NULL,
             FC_RL_CLOSING_DT               DATE NOT NULL,
             FC_RL_REMODEL_DT               DATE NOT NULL,
             FC_RL_DELETED_DT               DATE NOT NULL,
             FC_RL_DISTRICT_NO              DECIMAL(3, 0) NOT NULL,
             FC_RL_MARKET_AR_NO             DECIMAL(3, 0) NOT NULL,
             FC_RL_PAYROL_AR_NO             DECIMAL(3, 0) NOT NULL,
             FC_RL_PAY_GROUP_NO             DECIMAL(3, 0) NOT NULL,
             FC_RL_COMPANY_NO               DECIMAL(3, 0) NOT NULL,
             FC_RL_GEO_ZONE_CD              CHAR(1) NOT NULL,
             FC_RL_GEO_ZONE_NO              DECIMAL(3, 0) NOT NULL,
             FC_RL_SCAN_MAIN_CD             CHAR(1) NOT NULL,
             FC_RL_FRONT_END_CD             CHAR(1) NOT NULL,
             FC_RL_PRICE_BUL_CD             CHAR(1) NOT NULL,
             FC_RL_UPC_ON_PB_CD             CHAR(1) NOT NULL,
             FC_RL_COMPETITR_CD             DECIMAL(3, 0) NOT NULL,
             FC_RL_ASSOC_STR_NO             DECIMAL(5, 0) NOT NULL,
             FC_RL_RPRT_SEQ_NO              CHAR(4) NOT NULL,
             FC_RL_SORT_SOS_NO              CHAR(4) NOT NULL,
             FC_RL_VID_PRZN_NO              DECIMAL(5, 0) NOT NULL,
             FC_RL_CITY_ID_CD               DECIMAL(3, 0) NOT NULL,
             FC_RL_ADZONE_ABB               CHAR(1) NOT NULL,
             FC_RL_ADZONE_DES               CHAR(6) NOT NULL,
             FC_RL_UNLOAD_SW                CHAR(1) NOT NULL,
             FN_ROLLUP_REPT_CD              CHAR(20) NOT NULL,
             FC_RL_STATUS_CD                CHAR(1) NOT NULL,
             FC_RL_NEW_STORE_CD             CHAR(1) NOT NULL,
             FC_RL_TYPE_CD                  DECIMAL(3, 0) NOT NULL,
             FC_RL_GROUP_CD                 CHAR(2) NOT NULL,
             FC_RL_SELECTCIR_CD             CHAR(2) NOT NULL,
             FC_RL_AREA_CODE_NO             DECIMAL(3, 0) NOT NULL,
             FC_RL_TELEPHONE_NO             DECIMAL(7, 0) NOT NULL,
             FC_RL_STORE_ABB                CHAR(5) NOT NULL,
             FC_RL_BCKRM_FT_QTY             DECIMAL(5, 0) NOT NULL,
             FC_RL_LFT_FOOD_QTY             DECIMAL(5, 0) NOT NULL,
             FC_RL_LFT_NONF_QTY             DECIMAL(5, 0) NOT NULL,
             FC_RL_SETOFF_CD                CHAR(1) NOT NULL,
             FC_RL_CL12_ZONE_NO             DECIMAL(3, 0) NOT NULL,
             FC_RL_CL12_ADZN_NO             DECIMAL(3, 0) NOT NULL,
             FC_RL_CL13_ZONE_NO             DECIMAL(3, 0) NOT NULL,
             FC_RL_CL13_ADZN_NO             DECIMAL(3, 0) NOT NULL,
             FC_RL_CL14_ZONE_NO             DECIMAL(3, 0) NOT NULL,
             FC_RL_CL14_ADZN_NO             DECIMAL(3, 0) NOT NULL,
             FC_RL_CL36_ADZN_NO             DECIMAL(3, 0) NOT NULL,
             FC_RL_CL37_ADZN_NO             DECIMAL(3, 0) NOT NULL,
             FC_RL_STORE_DEA_NO             CHAR(9) NOT NULL,
             FC_RL_RETL_ZONE_NO             DECIMAL(3, 0) NOT NULL,
             FC_RL_STOR2_LOC_NM             CHAR(30) NOT NULL,
             FC_CITY_ADR                    CHAR(30) NOT NULL,
             FC_STATE_ADR                   CHAR(2) NOT NULL,
             FC_ZIP_CODE5_ADR               CHAR(5) NOT NULL,
             FC_ZIP_CODE4_ADR               CHAR(4) NOT NULL,
             FC_RL_SOS_TYPE_CD              CHAR(2) NOT NULL,
             FC_RL_NOPROCESS_CD             CHAR(1) NOT NULL,
             FC_RL_SOSHDRTYP_CD             DECIMAL(3, 0) NOT NULL,
             FC_RL_CAT_CLASS_TB             CHAR(74) NOT NULL,
             FC_RL_LATITUDE_K               DECIMAL(5, 2) NOT NULL,
             FC_RL_LONGITUDE_K              DECIMAL(5, 2) NOT NULL,
             FN_DIVISION_CD                 DECIMAL(3, 0) NOT NULL,
             FN_LINE_OF_BUS_CD              DECIMAL(3, 0) NOT NULL,
             FN_ROLLUP_REPT_01_NBR          DECIMAL(3, 0) NOT NULL,
             FN_ROLLUP_REPT_02_NBR          DECIMAL(3, 0) NOT NULL,
             FN_ROLLUP_REPT_03_NBR          DECIMAL(3, 0) NOT NULL,
             FN_ROLLUP_REPT_04_NBR          DECIMAL(3, 0) NOT NULL,
             FN_ROLLUP_REPT_05_NBR          DECIMAL(3, 0) NOT NULL,
             FN_ROLLUP_REPT_06_NBR          DECIMAL(3, 0) NOT NULL,
             FN_ROLLUP_REPT_07_NBR          DECIMAL(3, 0) NOT NULL,
             FN_ROLLUP_REPT_08_NBR          DECIMAL(3, 0) NOT NULL,
             FN_ROLLUP_REPT_09_NBR          DECIMAL(3, 0) NOT NULL,
             FN_ROLLUP_REPT_10_NBR          DECIMAL(3, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PROD.FC_XXXAIL_STORES           *
      ******************************************************************
       01  DCLFC-XXXAIL-STORES.
           10 FC-STORE-NO          PIC S9(5)V USAGE COMP-3.
           10 FC-RL-STORE-NM       PIC X(30).
           10 FC-RL-STORE-CD       PIC X(1).
           10 FC-RL-STORE-DIR-NM   PIC X(30).
           10 FC-RL-STORE-LOC-NM   PIC X(30).
           10 FC-RL-OPENING-DT     PIC X(10).
           10 FC-RL-CLOSING-DT     PIC X(10).
           10 FC-RL-REMODEL-DT     PIC X(10).
           10 FC-RL-DELETED-DT     PIC X(10).
           10 FC-RL-DISTRICT-NO    PIC S9(3)V USAGE COMP-3.
           10 FC-RL-MARKET-AR-NO   PIC S9(3)V USAGE COMP-3.
           10 FC-RL-PAYROL-AR-NO   PIC S9(3)V USAGE COMP-3.
           10 FC-RL-PAY-GROUP-NO   PIC S9(3)V USAGE COMP-3.
           10 FC-RL-COMPANY-NO     PIC S9(3)V USAGE COMP-3.
           10 FC-RL-GEO-ZONE-CD    PIC X(1).
           10 FC-RL-GEO-ZONE-NO    PIC S9(3)V USAGE COMP-3.
           10 FC-RL-SCAN-MAIN-CD   PIC X(1).
           10 FC-RL-FRONT-END-CD   PIC X(1).
           10 FC-RL-PRICE-BUL-CD   PIC X(1).
           10 FC-RL-UPC-ON-PB-CD   PIC X(1).
           10 FC-RL-COMPETITR-CD   PIC S9(3)V USAGE COMP-3.
           10 FC-RL-ASSOC-STR-NO   PIC S9(5)V USAGE COMP-3.
           10 FC-RL-RPRT-SEQ-NO    PIC X(4).
           10 FC-RL-SORT-SOS-NO    PIC X(4).
           10 FC-RL-VID-PRZN-NO    PIC S9(5)V USAGE COMP-3.
           10 FC-RL-CITY-ID-CD     PIC S9(3)V USAGE COMP-3.
           10 FC-RL-ADZONE-ABB     PIC X(1).
           10 FC-RL-ADZONE-DES     PIC X(6).
           10 FC-RL-UNLOAD-SW      PIC X(1).
           10 FN-ROLLUP-REPT-CD    PIC X(20).
           10 FC-RL-STATUS-CD      PIC X(1).
           10 FC-RL-NEW-STORE-CD   PIC X(1).
           10 FC-RL-TYPE-CD        PIC S9(3)V USAGE COMP-3.
           10 FC-RL-GROUP-CD       PIC X(2).
           10 FC-RL-SELECTCIR-CD   PIC X(2).
           10 FC-RL-AREA-CODE-NO   PIC S9(3)V USAGE COMP-3.
           10 FC-RL-TELEPHONE-NO   PIC S9(7)V USAGE COMP-3.
           10 FC-RL-STORE-ABB      PIC X(5).
           10 FC-RL-BCKRM-FT-QTY   PIC S9(5)V USAGE COMP-3.
           10 FC-RL-LFT-FOOD-QTY   PIC S9(5)V USAGE COMP-3.
           10 FC-RL-LFT-NONF-QTY   PIC S9(5)V USAGE COMP-3.
           10 FC-RL-SETOFF-CD      PIC X(1).
           10 FC-RL-CL12-ZONE-NO   PIC S9(3)V USAGE COMP-3.
           10 FC-RL-CL12-ADZN-NO   PIC S9(3)V USAGE COMP-3.
           10 FC-RL-CL13-ZONE-NO   PIC S9(3)V USAGE COMP-3.
           10 FC-RL-CL13-ADZN-NO   PIC S9(3)V USAGE COMP-3.
           10 FC-RL-CL14-ZONE-NO   PIC S9(3)V USAGE COMP-3.
           10 FC-RL-CL14-ADZN-NO   PIC S9(3)V USAGE COMP-3.
           10 FC-RL-CL36-ADZN-NO   PIC S9(3)V USAGE COMP-3.
           10 FC-RL-CL37-ADZN-NO   PIC S9(3)V USAGE COMP-3.
           10 FC-RL-STORE-DEA-NO   PIC X(9).
           10 FC-RL-RETL-ZONE-NO   PIC S9(3)V USAGE COMP-3.
           10 FC-RL-STOR2-LOC-NM   PIC X(30).
           10 FC-CITY-ADR          PIC X(30).
           10 FC-STATE-ADR         PIC X(2).
           10 FC-ZIP-CODE5-ADR     PIC X(5).
           10 FC-ZIP-CODE4-ADR     PIC X(4).
           10 FC-RL-SOS-TYPE-CD    PIC X(2).
           10 FC-RL-NOPROCESS-CD   PIC X(1).
           10 FC-RL-SOSHDRTYP-CD   PIC S9(3)V USAGE COMP-3.
           10 FC-RL-CAT-CLASS-TB   PIC X(74).
           10 FC-RL-LATITUDE-K     PIC S9(3)V9(2) USAGE COMP-3.
           10 FC-RL-LONGITUDE-K    PIC S9(3)V9(2) USAGE COMP-3.
           10 FN-DIVISION-CD       PIC S9(3)V USAGE COMP-3.
           10 FN-LINE-OF-BUS-CD    PIC S9(3)V USAGE COMP-3.
           10 FN-ROLLUP-REPT-01-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FN-ROLLUP-REPT-02-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FN-ROLLUP-REPT-03-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FN-ROLLUP-REPT-04-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FN-ROLLUP-REPT-05-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FN-ROLLUP-REPT-06-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FN-ROLLUP-REPT-07-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FN-ROLLUP-REPT-08-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FN-ROLLUP-REPT-09-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FN-ROLLUP-REPT-10-NBR
              PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 75      *
      ******************************************************************
       01  DDDTRL01
           REDEFINES
           DCLFC-XXXAIL-STORES.
           10 FC-FC-STORE-NO       PIC S9(5)V USAGE COMP-3.
           10 FC-FC-RL-STORE-NM    PIC X(30).
           10 FC-FC-RL-STORE-CD    PIC X(1).
           10 FC-FC-RL-STORE-DIR-NM PIC X(30).
           10 FC-FC-RL-STORE-LOC-NM PIC X(30).
           10 FC-FC-RL-OPENING-DT  PIC X(10).
           10 FC-FC-RL-CLOSING-DT  PIC X(10).
           10 FC-FC-RL-REMODEL-DT  PIC X(10).
           10 FC-FC-RL-DELETED-DT  PIC X(10).
           10 FC-FC-RL-DISTRICT-NO PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-MARKET-AR-NO PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-PAYROL-AR-NO PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-PAY-GROUP-NO PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-COMPANY-NO  PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-GEO-ZONE-CD PIC X(1).
           10 FC-FC-RL-GEO-ZONE-NO PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-SCAN-MAIN-CD PIC X(1).
           10 FC-FC-RL-FRONT-END-CD PIC X(1).
           10 FC-FC-RL-PRICE-BUL-CD PIC X(1).
           10 FC-FC-RL-UPC-ON-PB-CD PIC X(1).
           10 FC-FC-RL-COMPETITR-CD PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-ASSOC-STR-NO PIC S9(5)V USAGE COMP-3.
           10 FC-FC-RL-RPRT-SEQ-NO PIC X(4).
           10 FC-FC-RL-SORT-SOS-NO PIC X(4).
           10 FC-FC-RL-VID-PRZN-NO PIC S9(5)V USAGE COMP-3.
           10 FC-FC-RL-CITY-ID-CD  PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-ADZONE-ABB  PIC X(1).
           10 FC-FC-RL-ADZONE-DES  PIC X(6).
           10 FC-FC-RL-UNLOAD-SW   PIC X(1).
           10 FC-FN-ROLLUP-REPT-CD PIC X(20).
           10 FC-FC-RL-STATUS-CD   PIC X(1).
           10 FC-FC-RL-NEW-STORE-CD PIC X(1).
           10 FC-FC-RL-TYPE-CD     PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-GROUP-CD    PIC X(2).
           10 FC-FC-RL-SELECTCIR-CD PIC X(2).
           10 FC-FC-RL-AREA-CODE-NO PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-TELEPHONE-NO PIC S9(7)V USAGE COMP-3.
           10 FC-FC-RL-STORE-ABB   PIC X(5).
           10 FC-FC-RL-BCKRM-FT-QTY PIC S9(5)V USAGE COMP-3.
           10 FC-FC-RL-LFT-FOOD-QTY PIC S9(5)V USAGE COMP-3.
           10 FC-FC-RL-LFT-NONF-QTY PIC S9(5)V USAGE COMP-3.
           10 FC-FC-RL-SETOFF-CD   PIC X(1).
           10 FC-FC-RL-CL12-ZONE-NO PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-CL12-ADZN-NO PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-CL13-ZONE-NO PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-CL13-ADZN-NO PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-CL14-ZONE-NO PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-CL14-ADZN-NO PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-CL36-ADZN-NO PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-CL37-ADZN-NO PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-STORE-DEA-NO PIC X(9).
           10 FC-FC-RL-RETL-ZONE-NO PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-STOR2-LOC-NM PIC X(30).
           10 FC-FC-CITY-ADR       PIC X(30).
           10 FC-FC-STATE-ADR      PIC X(2).
           10 FC-FC-ZIP-CODE5-ADR  PIC X(5).
           10 FC-FC-ZIP-CODE4-ADR  PIC X(4).
           10 FC-FC-RL-SOS-TYPE-CD PIC X(2).
           10 FC-FC-RL-NOPROCESS-CD PIC X(1).
           10 FC-FC-RL-SOSHDRTYP-CD PIC S9(3)V USAGE COMP-3.
           10 FC-FC-RL-CAT-CLASS-TB PIC X(74).
           10 FC-FC-RL-LATITUDE-K  PIC S9(3)V9(2) USAGE COMP-3.
           10 FC-FC-RL-LONGITUDE-K PIC S9(3)V9(2) USAGE COMP-3.
           10 FC-FN-DIVISION-CD    PIC S9(3)V USAGE COMP-3.
           10 FC-FN-LINE-OF-BUS-CD PIC S9(3)V USAGE COMP-3.
           10 FC-FN-ROLLUP-REPT-01-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FC-FN-ROLLUP-REPT-02-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FC-FN-ROLLUP-REPT-03-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FC-FN-ROLLUP-REPT-04-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FC-FN-ROLLUP-REPT-05-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FC-FN-ROLLUP-REPT-06-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FC-FN-ROLLUP-REPT-07-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FC-FN-ROLLUP-REPT-08-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FC-FN-ROLLUP-REPT-09-NBR
              PIC S9(3)V USAGE COMP-3.
           10 FC-FN-ROLLUP-REPT-10-NBR
              PIC S9(3)V USAGE COMP-3.
      ******************************************************************
