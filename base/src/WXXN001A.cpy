00001 *BEGINCL WXXN001A                                                 12/03/93
00002  01  DA-DAILY-AUDIT-RECORD.                                       WXXN001A
00003    03  DA-LOG-01.                                                    LV001
00004      05  DA-LOG-KEY.                                              WXXN001A
00005          10  DA-LOG-ACCOUNT-NO.                                   WXXN001A
00006              15  DA-XXX-ACCOUNT-IN.                               WXXN001A
00007                  20  DA-COMPANY-NUMBER PIC X(2).                  WXXN001A
00008                  20  DA-MAJOR-ACCT.                               WXXN001A
00009                      25  DA-PRE-MAJ    PIC X(3).                  WXXN001A
00010                      25  DA-FILL-MAJ   PIC XX.                    WXXN001A
00011                  20  DA-MINOR-ACCT.                               WXXN001A
00012                      25  DA-PRE-MIN    PIC XX.                    WXXN001A
00013                      25  DA-WEEK       PIC X.                     WXXN001A
00014                  20  DA-STORE-NUMBER   PIC 9(5).                  WXXN001A
00015                  20  DA-DEPT-NUMBER.                              WXXN001A
00016                      25  DA-DEPT-SUB-DEPT.                        WXXN001A
00017                          30  DA-DEPT       PIC XX.                WXXN001A
00018                          30  DA-SUB-DEPT   PIC X.                 WXXN001A
00019          10  FILLER              PIC X(4).                        WXXN001A
00020          10  DA-MAJOR-ACCOUNT-TYPE PIC X.                         WXXN001A
00021              88  DA-ADJUSTMENTS       VALUE 'A'.                  WXXN001A
00022              88  DA-BUYING-ALLOWANCES VALUE 'B'.                  WXXN001A
00023              88  DA-CASH-PURCHASES    VALUE 'C'.                  WXXN001A
00024              88  DA-DSD               VALUE 'D'.                  WXXN001A
00025              88  DA-JOURNAL-ENTRIES   VALUE 'J'.                  WXXN001A
00026              88  DA-SHRINK-ADJ        VALUE 'K'.                  WXXN001A
00027              88  DA-WHSE-DSD-ADLOSS   VALUE 'L'.                  WXXN001A
00028              88  DA-MISC-PURCHASES    VALUE 'M'.                  WXXN001A
00029              88  DA-CONTROLLABLES     VALUE 'N'.                  WXXN001A
00030              88  DA-PURCH-VAR-JES     VALUE 'P'.                  WXXN001A
00031              88  DA-RGC-JES           VALUE 'R'.                  WXXN001A
00032              88  DA-SALES             VALUE 'S'.                  WXXN001A
00033              88  DA-TRANSFERS         VALUE 'T'.                  WXXN001A
00034              88  DA-WHSE-VARIANCE     VALUE 'V'.                  WXXN001A
00035              88  DA-WHSE-PURCHASE     VALUE 'W'.                  WXXN001A
00036              88  DA-PHARM-CHGBACK-JES VALUE 'X'.                  WXXN001A
00037          10  DA-ACCOUNT-TYPE     PIC X.                           WXXN001A
00038              88  DA-GL-ACCOUNT     VALUE '1'.                     WXXN001A
00039              88  DA-NOT-GL-ACCOUNT VALUE '0'.                     WXXN001A
00040      05  DA-DATE                 PIC 9(7)            COMP-3.      WXXN001A
00041      05  DA-TIME                 PIC 9(7)            COMP-3.      WXXN001A
00042      05  DA-SOURCE               PIC X(8).                        WXXN001A
00043      05  DA-ACCT-TYP-AUD-CD      PIC X.                           WXXN001A
00044          88 STORE-ACCOUNT        VALUE 'S'.                       WXXN001A
00045          88 OTHER-TYPE           VALUE 'O'.                       WXXN001A
00046      05  FILLER                  PIC X(09).                       WXXN001A
00047    03  DA-LOG-10.                                                 WXXN001A
00048      05  DA-COMMON-TRANS-CODE    PIC X(8).                        WXXN001A
00049      05  DA-COMMON-DATE          PIC 9(7)            COMP-3.      WXXN001A
00050      05  DA-COMMON-TIME          PIC 9(7)            COMP-3.      WXXN001A
00051      05  DA-SOURCE-CODE          PIC X(8).                        WXXN001A
00052      05  DA-TRANSACTION-TYPE     PIC X.                           WXXN001A
00053          88  DA-ADD-CODE           VALUE 'A'.                     WXXN001A
00054          88  DA-CHANGE-CODE        VALUE 'C'.                     WXXN001A
00055          88  DA-DELETE-CODE        VALUE 'D'.                     WXXN001A
00056          88  DA-REPLACE-CODE       VALUE 'R'.                     WXXN001A
00057      05  DA-SIGNON-CODE          PIC X(8).                        WXXN001A
00058      05  DA-STATUS-CODE.                                          WXXN001A
00059          10  DA-AUDIT-STATUS     PIC X.                           WXXN001A
00060              88  DA-NOT-AUDITED     VALUE '0'.                    WXXN001A
00061              88  DA-AUDITED         VALUE '1'.                    WXXN001A
00062          10  DA-GL-INTERFACE-STATUS PIC X.                        WXXN001A
00063              88  DA-GL-NOT-PASSED   VALUE '0'.                    WXXN001A
00064              88  DA-GL-PASSED       VALUE '1'.                    WXXN001A
00065          10  DA-REPORT-STATUS    PIC X.                           WXXN001A
00066              88  DA-NOT-REPORTED    VALUE '0'.                    WXXN001A
00067              88  DA-REPORTED        VALUE '1'.                    WXXN001A
00068      05  DA-AUDIT-DES            PIC X(21).                       WXXN001A
00069      05  DA-NEW-VALUE            PIC S9(10)V9(3)    COMP-3.       WXXN001A
00070      05  DA-NEW-VALUE-X REDEFINES DA-NEW-VALUE PIC X(7).          WXXN001A
00071      05  DA-CONTROL-NUMBER       PIC X(5).                        WXXN001A
00072      05  DA-DISTRICT-NUMBER      PIC 9(3)           COMP-3.       WXXN001A
00073      05  DA-AUDIT-REF-NO         PIC X(6).                        WXXN001A
00074      05  DA-CONTROL-DATE         PIC 9(7)           COMP-3.       WXXN001A
00075      05  DA-RETAIL-COST-SWITCH   PIC X.                           WXXN001A
00076          88  DA-RETAIL              VALUE 'R'.                    WXXN001A
00077          88  DA-COST                VALUE 'C'.                    WXXN001A
00078      05  DA-PERIOD-NUMBER.                                        WXXN001A
00079         10  DA-YEAR              PIC 9(2).                        WXXN001A
00080         10  DA-PERIOD            PIC 9(2).                        WXXN001A
00081      05  DA-WEEK-NUMBER          PIC 9.                           WXXN001A
00082      05  DA-CONTROL-NAME         PIC X(10).                       WXXN001A
00083      05  FILLER                  PIC X(3).                        WXXN001A
00084    03  DA-STORE-REPORTING-SEQ-NO PIC X(4).                        WXXN001A
00085    03  DA-STORE-NAME             PIC X(30).                       WXXN001A
00086    03  DA-DEPT-NAME              PIC X(30).                       WXXN001A
00087    03  DA-DISTRICT-NAME          PIC X(30).                       WXXN001A
00088    03  DA-DIVISION-NUMBER        PIC 9(2).                        WXXN001A
00089    03  DA-DIVISION-NAME          PIC X(30).                       WXXN001A
00090 *ENDINCL WXXN001A                                                 WXXN001A
