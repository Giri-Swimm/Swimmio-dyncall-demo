      ******************************************************************
      * DCLGEN TABLE(DB2PROD.XXX_DEPT)                                 *
      *        LIBRARY(SYS2.DBCLIB(DDDTDP01))                          *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        APOST                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE XXX_DEPT TABLE
           ( STR_DEPT_NBR                   CHAR(5) NOT NULL,
             STR_SUB_DEPT_ID                CHAR(5) NOT NULL,
             DEPT_NM                        CHAR(30) NOT NULL,
             DEPT_ABB                       CHAR(6) NOT NULL,
             REPT_GRP_CD                    INTEGER NOT NULL,
             GRPRFT_LO_PCT                  DECIMAL(7, 4) NOT NULL,
             GRPRFT_HI_PCT                  DECIMAL(7, 4) NOT NULL,
             SHRNK_LO_PCT                   DECIMAL(7, 4) NOT NULL,
             SHRNK_HI_PCT                   DECIMAL(7, 4) NOT NULL,
             LST_UPDT_USR_ID                CHAR(8) NOT NULL,
             LST_UPDT_TS                    TIMESTAMP NOT NULL,
             ORG_ID                         INTEGER NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PROD.STR_DEPT                   *
      ******************************************************************
       01  DCLXXX-DEPT.
           10 STR-DEPT-NBR         PIC X(5).
           10 STR-SUB-DEPT-ID      PIC X(5).
           10 DEPT-NM              PIC X(30).
           10 DEPT-ABB             PIC X(6).
           10 REPT-GRP-CD          PIC S9(9) USAGE COMP.
           10 GRPRFT-LO-PCT        PIC S9(3)V9(4) USAGE COMP-3.
           10 GRPRFT-HI-PCT        PIC S9(3)V9(4) USAGE COMP-3.
           10 SHRNK-LO-PCT         PIC S9(3)V9(4) USAGE COMP-3.
           10 SHRNK-HI-PCT         PIC S9(3)V9(4) USAGE COMP-3.
           10 LST-UPDT-USR-ID      PIC X(8).
           10 LST-UPDT-TS          PIC X(26).
           10 ORG-ID               PIC S9(9) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
       01  DDDDTDP01
           REDEFINES
           DCLXXX-DEPT.
           10 MD-STR-DEPT-NBR      PIC X(5).
           10 MD-STR-SUB-DEPT-ID   PIC X(5).
           10 MD-DEPT-NM           PIC X(30).
           10 MD-DEPT-ABB          PIC X(6).
           10 MD-REPT-GRP-CD       PIC S9(9) USAGE COMP.
           10 MD-GRPRFT-LO-PCT     PIC S9(3)V9(4) USAGE COMP-3.
           10 MD-GRPRFT-HI-PCT     PIC S9(3)V9(4) USAGE COMP-3.
           10 MD-SHRNK-LO-PCT      PIC S9(3)V9(4) USAGE COMP-3.
           10 MD-SHRNK-HI-PCT      PIC S9(3)V9(4) USAGE COMP-3.
           10 MD-LST-UPDT-USR-ID   PIC X(8).
           10 MD-LST-UPDT-TS       PIC X(26).
           10 MD-ORG-ID            PIC S9(9) USAGE COMP.
      ******************************************************************
