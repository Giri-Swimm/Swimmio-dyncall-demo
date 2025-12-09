000100* ================================================================00000100
000200* WORK AREA COPYBOOK FOR DDDTLO01.                                00000200
000600 01 P-DDDTLO01.                                                   00000600
000700     05 LOC-TYP-CD                     PIC X(2)  VALUE SPACES.    00000700
000800     05 LOC-NBR                        PIC S9(9) COMP VALUE 0.    00000800
000900     05 LOC-NM                         PIC X(30)  VALUE SPACES.   00000900
001000     05 LOC-ABB                        PIC X(6)  VALUE SPACES.    00001000
001100     05 LGL-LOC-NAM                    PIC X(30)  VALUE SPACES.   00001100
001200     05 PRIM-CONTACT-NM                PIC X(50)  VALUE SPACES.   00001200
001300     05 PRIM-ADR-1                     PIC X(30)  VALUE SPACES.   00001300
001400     05 PRIM-ADR-2                     PIC X(30)  VALUE SPACES.   00001400
001500     05 PRIM-ADR-3                     PIC X(30)  VALUE SPACES.   00001500
001600     05 PRIM-ADR-4                     PIC X(30)  VALUE SPACES.   00001600
001700     05 PRIM-CITY                      PIC X(30)  VALUE SPACES.   00001700
001800     05 PRIM-CITY-ID                   PIC S9(9) COMP VALUE 0.    00001800
001900     05 PRIM-STATE-CD                  PIC X(2)  VALUE SPACES.    00001900
002000     05 PRIM-ZIP5-CD                   PIC S9(5) COMP-3 VALUE 0.  00002000
002100     05 PRIM-ZIP4-CD                   PIC S9(4) COMP-3 VALUE 0.  00002100
002200     05 PRIM-PHN-CNTRY-CD              PIC S9(9) COMP VALUE 0.    00002200
002300     05 PRIM-AREA-CD                   PIC S9(3) COMP-3 VALUE 0.  00002300
002400     05 PRIM-PHONE-NBR                 PIC S9(7) COMP-3 VALUE 0.  00002400
002500     05 PRIM-CNTRY-NM                  PIC X(30)  VALUE SPACES.   00002500
002600     05 REDEFINES PRIM-CNTRY-NM.                                  00002600
002700       10 PRIM-ZIP5-EXCEP              PIC X(5).                  00002700
002800       10 PRIM-ZIP4-EXCEP              PIC X(4).                  00002800
002900       10 FILLER                       PIC X(21).                 00002900
003000     05 PRIM-CNTRY-ABB                 PIC X(6)  VALUE SPACES.    00003000
003100     05 SEC-LOC-NM                     PIC X(30)  VALUE SPACES.   00003100
003200     05 SEC-CONTACT-NM                 PIC X(30)  VALUE SPACES.   00003200
003300     05 SEC-ADR-1                      PIC X(30)  VALUE SPACES.   00003300
003400     05 SEC-ADR-2                      PIC X(30)  VALUE SPACES.   00003400
003500     05 SEC-ADR-3                      PIC X(30)  VALUE SPACES.   00003500
003600     05 SEC-ADR-4                      PIC X(30)  VALUE SPACES.   00003600
003700     05 SEC-CITY                       PIC X(30)  VALUE SPACES.   00003700
003800     05 SEC-STATE-CD                   PIC X(2)  VALUE SPACES.    00003800
003900     05 SEC-ZIP5-CD                    PIC S9(5) COMP-3 VALUE 0.  00003900
004000     05 SEC-ZIP4-CD                    PIC S9(4) COMP-3 VALUE 0.  00004000
004100     05 SEC-PHN-CNTRY-CD               PIC S9(9) COMP VALUE 0.    00004100
004200     05 SEC-AREA-CD                    PIC S9(3) COMP-3 VALUE 0.  00004200
004300     05 SEC-PHONE-NBR                  PIC S9(7) COMP-3 VALUE 0.  00004300
004400     05 SEC-CNTRY-NM                   PIC X(30)  VALUE SPACES.   00004400
004500     05 REDEFINES SEC-CNTRY-NM.                                   00004500
004600       10 SEC-ZIP5-EXCEP               PIC X(5).                  00004600
004700       10 SEC-ZIP4-EXCEP               PIC X(4).                  00004700
004800       10 FILLER                       PIC X(21).                 00004800
004900     05 SEC-CNTRY-ABB                  PIC X(6)  VALUE SPACES.    00004900
005000     05 MAIL-TO-LOC-NM                 PIC X(30) VALUE SPACES.    00005000
005100     05 MAIL-TO-CNTCT-NM               PIC X(30) VALUE SPACES.    00005100
005200     05 MAIL-TO-ADR-1                  PIC X(30) VALUE SPACES.    00005200
005300     05 MAIL-TO-ADR-2                  PIC X(30) VALUE SPACES.    00005300
005400     05 MAIL-TO-ADR-3                  PIC X(30) VALUE SPACES.    00005400
005500     05 MAIL-TO-ADR-4                  PIC X(30) VALUE SPACES.    00005500
005600     05 MAIL-TO-CITY                   PIC X(30) VALUE SPACES.    00005600
005700     05 MAIL-TO-STATE-CD               PIC X(2)  VALUE SPACES.    00005700
005800     05 MAIL-TO-ZIP5-CD                PIC S9(5) COMP-3 VALUE 0.  00005800
005900     05 MAIL-TO-ZIP4-CD                PIC S9(4) COMP-3 VALUE 0.  00005900
006000     05 MAIL-PHN-CNTRY-CD              PIC S9(9) COMP VALUE 0.    00006000
006100     05 MAIL-TO-AREA-CD                PIC S9(3) COMP-3 VALUE 0.  00006100
006200     05 MAIL-TO-PHONE-NBR              PIC S9(7) COMP-3 VALUE 0.  00006200
006300     05 MAIL-TO-CNTRY-NM               PIC X(30) VALUE SPACES.    00006300
006400     05 REDEFINES MAIL-TO-CNTRY-NM.                               00006400
006500       10 MAIL-TO-ZIP5-EXCEP           PIC X(5).                  00006500
006600       10 MAIL-TO-ZIP4-EXCEP           PIC X(4).                  00006600
006700       10 FILLER                       PIC X(21).                 00006700
006800     05 MAIL-TO-CNTRY-AB               PIC X(6)  VALUE SPACES.    00006800
006900     05 CURR-FAX-ID                    PIC S9(9) COMP VALUE 0.    00006900
007000     05 ADDED-DT                       PIC X(10) VALUE SPACES.    00007000
007100     05 DELETE-DT                      PIC X(10) VALUE SPACES.    00007100
007200     05 OPENED-DT                      PIC X(10) VALUE SPACES.    00007200
007300     05 CLOSED-DT                      PIC X(10) VALUE SPACES.    00007300
007400     05 INACTIVE-SW                    PIC X(1)  VALUE SPACES.    00007400
007500         88 LO-ACTIVE                            VALUE 'A'.       00007500
007600         88 LO-INACTIVE                          VALUE 'I'.       00007600
007700         88 LO-DELETED                           VALUE 'D'.       00007700
007800     05 INACTIVE-DT                    PIC X(10) VALUE SPACES.    00007800
007900     05 AP-NBR                         PIC S9(9) COMP VALUE 0.    00007900
008000     05 AP-TYP-CD                      PIC X(2)  VALUE SPACES.    00008000
008100     05 LST-UPDT-TS                    PIC X(26) VALUE SPACES.    00008100
008200     05 LST-UPDT-USR-ID                PIC X(8)  VALUE SPACES.    00008200
008300     05 PRIM-EMAIL-ID                  PIC X(50) VALUE SPACES.    00008300
008400     05 SECY-EMAIL-ID                  PIC X(50) VALUE SPACES.    00008400
008500     05 MAIL-TO-EMAIL-ID               PIC X(50) VALUE SPACES.    00008500
008600     05 FAC-ID                         PIC S9(9) COMP VALUE 0.    00008600
008700     05 FAC-ID-X REDEFINES FAC-ID      PIC X(4).                  00008700
008800     05 ORG-ID                         PIC S9(9) COMP VALUE 0.    00008800
008900     05 ORG-ID-X REDEFINES ORG-ID      PIC X(4).                  00008900
009000     05 B2B-PRIM-RTNG-ID               PIC X(30) VALUE SPACES.    00009000
009100     05 PRIM-CNTY-TXT                  PIC X(30) VALUE SPACES.    00009100
009200     05 SECY-CNTY-TXT                  PIC X(30) VALUE SPACES.    00009200
009300     05 MAIL-TO-CNTY-TXT               PIC X(30) VALUE SPACES.    00009300
009400     05 DIR-SHP-LOC-SW                 PIC X(1)  VALUE SPACES.    00009400
009500         88 LOC-IS-NOT-DIRECT-SHIP               VALUE 'N'.       00009500
009600         88 LOC-IS-DIRECT-SHIP                   VALUE 'Y'.       00009600
009700     05 LOC-ORD-PROCNG-DD          PIC S9(3)V9(2) COMP-3 VALUE 0. 00009700
009800     05 ORD-PROCNG-CTOF-TM             PIC X(8)  VALUE SPACES.    00009800
009900     05 SCH-SHP-DD-TXT                 PIC X(7)  VALUE SPACES.    00009900
010000         88 SHIPS-EVERY-DAY                      VALUE 'YYYYYYY'. 00010000
010100         88 SHIPS-MON-THRU-FRI                   VALUE 'YYYYYNN'. 00010100
010200     05 REDEFINES SCH-SHP-DD-TXT.                                 00010200
010300         10 SCH-SHP-DD-TXT-MON         PIC X(1).                  00010300
010400             88 SHIPS-MON                        VALUE 'Y'.       00010400
010500             88 DOES-NOT-SHIP-MON                VALUE 'N'.       00010500
010600         10 SCH-SHP-DD-TXT-TUE         PIC X(1).                  00010600
010700             88 SHIPS-TUE                        VALUE 'Y'.       00010700
010800             88 DOES-NOT-SHIP-TUE                VALUE 'N'.       00010800
010900         10 SCH-SHP-DD-TXT-WED         PIC X(1).                  00010900
011000             88 SHIPS-WED                        VALUE 'Y'.       00011000
011100             88 DOES-NOT-SHIP-WED                VALUE 'N'.       00011100
011200         10 SCH-SHP-DD-TXT-THU         PIC X(1).                  00011200
011300             88 SHIPS-THU                        VALUE 'Y'.       00011300
011400             88 DOES-NOT-SHIP-THU                VALUE 'N'.       00011400
011500         10 SCH-SHP-DD-TXT-FRI         PIC X(1).                  00011500
011600             88 SHIPS-FRI                        VALUE 'Y'.       00011600
011700             88 DOES-NOT-SHIP-FRI                VALUE 'N'.       00011700
011800         10 SCH-SHP-DD-TXT-SAT         PIC X(1).                  00011800
011900             88 SHIPS-SAT                        VALUE 'Y'.       00011900
012000             88 DOES-NOT-SHIP-SAT                VALUE 'N'.       00012000
012100         10 SCH-SHP-DD-TXT-SUN         PIC X(1).                  00012100
012200             88 SHIPS-SUN                        VALUE 'Y'.       00012200
012300             88 DOES-NOT-SHIP-SUN                VALUE 'N'.       00012300
012400     05 ORD-LEAD-TM-DD             PIC S9(3)V9(2) COMP-3 VALUE 0. 00012400
012500     05 ORD-BUFFER-TM-DD           PIC S9(3)V9(2) COMP-3 VALUE 0. 00012500
012500     05 DSV-VEND-SW                    PIC X(1) VALUE 'N'.        00012510
012200        88 IS-DSV-VEND                      VALUE 'Y'.            00012520
012300        88 IS-NOT-DSV-VEND                  VALUE 'N'.            00012530
012600     05 FILLER                         PIC X(96)  VALUE SPACES.   00012600
