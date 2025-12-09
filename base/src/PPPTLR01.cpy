000100 01 P-DDDTLR01.                                                   00000100
000200     05 LOC-NBR                        PIC S9(9) COMP VALUE 0.    00000200
000300     05 LOC-TYP-CD                     PIC X(2)  VALUE SPACES.    00000300
000400     05 ASSOC-STR-TYP-CD               PIC X(2)  VALUE SPACES.    00000400
000500     05 ASSOC-STR-NBR                  PIC S9(9) COMP VALUE 0.    00000500
000600     05 STR-REMODL-DT                  PIC X(10) VALUE SPACES.    00000600
000700     05 RETL-LOC-STAT-CD               PIC X(1)  VALUE SPACES.    00000700
000800     05 RETL-LOC-STAT-DT               PIC X(10) VALUE SPACES.    00000800
000900     05 COMPANY-ID                     PIC S9(9) COMP VALUE 0.    00000900
001000     05 FINANCIAL-DIV-ID               PIC S9(3) COMP-3 VALUE 0.  00001000
001100     05 LIN-OF-BUS-ID                  PIC S9(5) COMP-3 VALUE 0.  00001100
001200     05 DIST-ID                        PIC S9(5) COMP-3 VALUE 0.  00001200
001300     05 MKT-RGN-ID                     PIC S9(5) COMP-3 VALUE 0.  00001300
001400     05 GEO-ZN-CD                      PIC X(1)  VALUE SPACES.    00001400
001500     05 RETL-GEO-ZN-ID                 PIC S9(3) COMP-3 VALUE 0.  00001500
001600     05 SCN-MAINT-SW                   PIC X(1)  VALUE SPACES.    00001600
001700     05 FRNT-END-CD                    PIC X(1)  VALUE SPACES.    00001700
001800     05 PRC-BUL-SW                     PIC X(1)  VALUE SPACES.    00001800
001900     05 UPC-ON-PRC-BUL-SW              PIC X(1)  VALUE SPACES.    00001900
002000     05 CMPTR-TYP-CD                   PIC S9(5) COMP-3 VALUE 0.  00002000
002100     05 RETL-VID-ZN-NBR                PIC S9(7) COMP-3 VALUE 0.  00002100
002200     05 RETL-UNLD-CD                   PIC X(1)  VALUE SPACES.    00002200
002300     05 ROLUP-REPT-TBL-TXT             PIC X(20) VALUE SPACES.    00002300
002400     05 REDEFINES ROLUP-REPT-TBL-TXT.                             00002400
002500         10 ROLUP-REPT-TBL OCCURS 10 TIMES.                       00002500
002600             15 ROLUP-REPT-NBR         PIC S9(3) COMP-3.          00002600
002700     05 NEW-STR-SW                     PIC X(1)  VALUE SPACES.    00002700
002800     05 SEL-CIR-SW                     PIC X(1)  VALUE SPACES.    00002800
002900     05 BKRM-SQ-FT                     PIC S9(7) COMP-3 VALUE 0.  00002900
003000     05 FD-LINER-FT                    PIC S9(7) COMP-3 VALUE 0.  00003000
003100     05 NON-FD-LINER-FT                PIC S9(7) COMP-3 VALUE 0.  00003100
003200     05 SETOFF-ROOM-SW                 PIC X(1)  VALUE SPACES.    00003200
003300     05 CAT-CLS-TBL-TXT                PIC X(100) VALUE SPACES.   00003300
003400     05 REDEFINES CAT-CLS-TBL-TXT.                                00003400
003500         10 CAT-CLASSES OCCURS 74 TIMES.                          00003500
003600             15 CAT-CLASS              PIC X(1).                  00003600
003700     05 DONTUSE1                       PIC S9(3)V9(2) COMP-3      00003700
003800                                       VALUE +0.                  00003800
003900     05 DONTUSE2                       PIC S9(3)V9(2) COMP-3      00003900
004000                                       VALUE +0.                  00004000
004100     05 CK-COLL-REPT-SW                PIC X(1)  VALUE SPACES.    00004100
004200     05 CK-COLL-CNTL-CD                PIC X(1)  VALUE SPACES.    00004200
004300     05 CK-COLL-ADD-DEL-SW             PIC X(1)  VALUE SPACES.    00004300
004400     05 CK-ALT-STR-ID                  PIC S9(9) COMP VALUE 0.    00004400
004500     05 CK-COLL-FEE-AMT                PIC S9(5)V9(2) COMP-3      00004500
004600                                       VALUE +0.                  00004600
004700     05 SALS-TAX-PCT                   PIC S9(2)V9(3) COMP-3      00004700
004800                                       VALUE +0.                  00004800
004900     05 SOAP-SALE-VAR-PCT              PIC S9(3) COMP-3 VALUE 0.  00004900
005000     05 ON-SRS-CD                      PIC X(1)  VALUE SPACES.    00005000
005100     05 SRS-DSD-ORD-SW                 PIC X(1)  VALUE SPACES.    00005100
005200     05 RETL-LOC-TYP-CD                PIC X(2)  VALUE SPACES.    00005200
005300     05 DEA-NBR                        PIC X(9)  VALUE SPACES.    00005300
005400     05 STR-OPSTMT-SRT-CD              PIC X(4)  VALUE SPACES.    00005400
005500     05 STR-OPSTMT-TYP-CD              PIC X(2)  VALUE SPACES.    00005500
005600     05 STR-OPSTMT-HDR-CD              PIC S9(3) COMP-3 VALUE 0.  00005600
005700         88 ST-BODEGA                            VALUE 6.         00005700
005800         88 ST-CONVENTIONAL                      VALUE 3.         00005800
005900         88 ST-CORE-STORE                        VALUE 1 2 3 7.   00005900
006000         88 ST-MARKETPLACE                       VALUE 7.         00006000
006100         88 ST-MEXICO                            VALUE 9.         00006100
006200         88 ST-MEXICO-BORDER                     VALUE 8.         00006200
006300         88 ST-PANTRY                            VALUE 5.         00006300
006400         88 ST-SUPER-FOOD                        VALUE 2.         00006400
006500         88 ST-SUPER-STORE                       VALUE 1.         00006500
006600         88 ST-VIDEO                             VALUE 4.         00006600
006700     05 DPS-NBR                        PIC X(8)  VALUE SPACES.    00006700
006800     05 MEDICARE-ID                    PIC X(12) VALUE SPACES.    00006800
006900     05 NABP-NBR                       PIC X(7)  VALUE SPACES.    00006900
007000     05 NATL-PROV-ID                   PIC X(30) VALUE SPACES.    00007000
007100     05 CURR-AD-ZN-NBR                 PIC S9(7) COMP-3 VALUE 0.  00007100
007200     05 PD-ZONE-NO                     PIC S9(5) COMP-3 VALUE 0.  00007200
007300     05 SOS-PROC-SW                    PIC X(1)  VALUE SPACES.    00007300
007400     05 RPRT-SEQ-NBR                   PIC X(4)  VALUE SPACES.    00007400
007500     05 GRP-CD                         PIC X(2)  VALUE SPACES.    00007500
007600     05 PRIM-GRP-CD-1                  PIC X(10) VALUE SPACES.    00007600
007700     05 PRIM-GRP-CD-2                  PIC X(10) VALUE SPACES.    00007700
007800     05 SECY-GRP-CD-1                  PIC X(10) VALUE SPACES.    00007800
007900     05 SECY-GRP-CD-2                  PIC X(10) VALUE SPACES.    00007900
008000     05 PRIM-CLS-NBR-1                 PIC S9(9) COMP VALUE 0.    00008000
008100     05 PRIM-CLS-NBR-2                 PIC S9(9) COMP VALUE 0.    00008100
008200     05 SECY-CLS-NBR-1                 PIC S9(9) COMP VALUE 0.    00008200
008300     05 SECY-CLS-NBR-2                 PIC S9(9) COMP VALUE 0.    00008300
008400     05 VAL-STR-SW                     PIC X(1)  VALUE SPACES.    00008400
008500     05 SLS-CLOSED-DT                  PIC X(10) VALUE SPACES.    00008500
008600     05 TBCO-PRMT-NBR             PIC S9(9)V COMP-3 VALUE 0.      00008600
008700     05 LAT-K                     PIC S9(3)V9(5) COMP-3 VALUE +0. 00008700
008800     05 LON-K                     PIC S9(3)V9(5) COMP-3 VALUE +0. 00008800
008900     05 SUB-UNLIKE-PROD-CD             PIC X(1)  VALUE SPACES.    00008900
009000         88 NO-UNLIKE-SUB-STORE-PREF             VALUE ' '.       00009000
009100         88 OK-TO-SUB-UNLIKE-PRODS               VALUE 'Y'.       00009100
009200         88 DONT-SUB-UNLIKE-PRODS                VALUE 'N'.       00009200
009300     05 SUB-DSPLY-PAL-CD               PIC X(1)  VALUE SPACES.    00009300
009400         88 NO-DISP-PAL-SUB-STORE-PREF           VALUE ' '.       00009400
009500         88 OK-TO-SUB-DISP-PALS                  VALUE 'Y'.       00009500
009600         88 DONT-SUB-DISP-PALS                   VALUE 'N'.       00009600
009700     05 RLTM-SCN-MAINT-SW              PIC X(1)  VALUE SPACES.    00009700
009800         88 SEND-REAL-TIME-G3                    VALUE 'Y'.       00009800
009900         88 DONT-SEND-REAL-TIME-G3               VALUE 'N'.       00009900
010000     05 TOP-LEADER-NM                  PIC X(50) VALUE SPACES.    00010000
010100     05 CUST-FRNDLY-NM                 PIC X(100) VALUE SPACES.   00010100
010200     05 SLS-OPEN-DT                    PIC X(10) VALUE SPACES.    00010200
010300     05 MON-OPEN-TM                    PIC X(8)  VALUE SPACES.    00010300
010400     05 MON-CLOS-TM                    PIC X(8)  VALUE SPACES.    00010400
010500     05 TUE-OPEN-TM                    PIC X(8)  VALUE SPACES.    00010500
010600     05 TUE-CLOS-TM                    PIC X(8)  VALUE SPACES.    00010600
010700     05 WED-OPEN-TM                    PIC X(8)  VALUE SPACES.    00010700
010800     05 WED-CLOS-TM                    PIC X(8)  VALUE SPACES.    00010800
010900     05 THUR-OPEN-TM                   PIC X(8)  VALUE SPACES.    00010900
011000     05 THUR-CLOS-TM                   PIC X(8)  VALUE SPACES.    00011000
011100     05 FRI-OPEN-TM                    PIC X(8)  VALUE SPACES.    00011100
011200     05 FRI-CLOS-TM                    PIC X(8)  VALUE SPACES.    00011200
011300     05 SAT-OPEN-TM                    PIC X(8)  VALUE SPACES.    00011300
011400     05 SUN-OPEN-TM                    PIC X(8)  VALUE SPACES.    00011400
011500     05 SAT-CLOS-TM                    PIC X(8)  VALUE SPACES.    00011500
011600     05 SUN-CLOS-TM                    PIC X(8)  VALUE SPACES.    00011600
011700     05 RETL-LOC-FRMAT-CD              PIC X(5)  VALUE SPACES.    00011700
011800     05 RETL-LOC-SEGM-CD               PIC X(5)  VALUE SPACES.    00011800
011900     05 ECOMM-MKT-AREA-CD              PIC X(5)  VALUE SPACES.    00011900
012000     05 ECOMM-STRT-DT                  PIC X(10) VALUE SPACES.    00012000
           05 ECOMM-STRT-DT-IND              PIC S9(4) COMP VALUE 0.    00012010
             88 ECOMM-STRT-DT-NOT-NULL                      VALUE 0.    00012020
             88 ECOMM-STRT-DT-NULL                          VALUE -1.   00012030
012100     05 ECOMM-END-DT                   PIC X(10) VALUE SPACES.    00012100
           05 ECOMM-END-DT-IND               PIC S9(4) COMP VALUE 0.    00012110
             88 ECOMM-END-DT-NOT-NULL                       VALUE 0.    00012120
             88 ECOMM-END-DT-NULL                           VALUE -1.   00012130
AT1386     05 ONLIN-SSON-SW                  PIC X(1)   VALUE SPACES.   00012140
AT1386     05 RPLACD-BY-STR-NBR              PIC S9(9)  COMP VALUE 0.   00012150
012200     05 FILLER                         PIC X(470) VALUE SPACES.   00012200
