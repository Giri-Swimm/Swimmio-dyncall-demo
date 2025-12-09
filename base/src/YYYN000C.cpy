000010*                                                                 00000100
000100* ================================================================00000200
000200* THIS COPY BOOK CONTAINS THE WS VARIABLES FOR DYNAMIC SUBROUTINE 00000300
000300* RELATED TO MIDDLEWARE CLIENT ROUTINES.                          00000400
000400* ================================================================00000500
000600*                                                                 00000600
000700* ----------------------------------------------------------------00000700
000800* THE ONE AND ONLY MIDDLWARE INTERFACE.                           00000800
001000* ----------------------------------------------------------------00000900
001100*                                                                 00001000
009100 01 Z-MIDDLEWARE-INTERFACE  PIC X(8) VALUE 'YYYS0160'.            00001100
009101                                                                  00001200
009810*                                                                 00001300
009820* ----------------------------------------------------------------00001400
009830* THESE ARE THE ENTRY LEVEL MANAGERS FOR MIDDLEWARES SUPPORTED.   00001500
009840* ----------------------------------------------------------------00001600
009850*                                                                 00001700
009900 01 Z-SOC-MIDDLEWARE-MGR    PIC X(8) VALUE 'YYYS0170'.            00001800
010000 01 Z-MQC-MIDDLEWARE-MGR    PIC X(8) VALUE 'YYYS0171'.            00001900
010001 01 Z-MQC-INTERFACE-MGR     PIC X(8) VALUE 'YYYS0172'.            00002000
010002 01 Z-TRIGGER-ASYNCH-TASK   PIC X(8) VALUE 'YYYS0173'.            00002100
010003 01 Z-TRIGGER-INT-EVENT     PIC X(8) VALUE 'YYYS0174'.            00002200
010004 01 Z-EVENT-STAGER          PIC X(8) VALUE 'YYYS0175'.            00002300
010004 01 Z-MQC-GET-CLIENT-MSG    PIC X(8) VALUE 'YYYS0176'.            00002400
010004 01 Z-MQC-SEND-CLIENT-REPLY PIC X(8) VALUE 'YYYS0177'.            00002500
010004 01 Z-CICS-START-TRAN       PIC X(8) VALUE 'YYYS0178'.            00002600
010004 01 Z-CICS-TEST-OR-PROD     PIC X(8) VALUE 'YYYS0179'.            00002700
010010                                                                  00002800
010100*                                                                 00002900
010200* ----------------------------------------------------------------00003000
010300* THESE ARE SYBASE SUPPORT MIDDLEWARE SUBROUTINES.                00003100
010400* ----------------------------------------------------------------00003200
010500*                                                                 00003300
010600 01 Z-SOC-CONNECT-MGR       PIC X(8) VALUE 'YYYS0161'.            00003400
010700 01 Z-SOC-CONNECT           PIC X(8) VALUE 'YYYS0162'.            00003500
010800 01 Z-SOC-DISCONNECT        PIC X(8) VALUE 'YYYS0163'.            00003600
010900 01 Z-SOC-SEND-PARMS        PIC X(8) VALUE 'YYYS0164'.            00003700
011000 01 Z-SOC-GET-RESULTS       PIC X(8) VALUE 'YYYS0165'.            00003800
011100 01 Z-SOC-GET-STATUS-RESULT PIC X(8) VALUE 'YYYS0166'.            00003900
011200 01 Z-SOC-GET-ROW-RESULT    PIC X(8) VALUE 'YYYS0167'.            00004000
011300 01 Z-SOC-GET-MESSAGES      PIC X(8) VALUE 'YYYS0168'.            00004100
011400*                                                                 00004200
011500* ----------------------------------------------------------------00004300
011600* THESE ARE MQ SUPPORT MIDDLEWARE SUBROUTINES FOR CICS            00004400
011700* ----------------------------------------------------------------00004500
011800*                                                                 00004600
011900 01 Z-MQC-CONNECT           PIC X(8) VALUE 'YYYS0180'.            00004700
011910 01 Z-MQC-OPEN-Q            PIC X(8) VALUE 'YYYS0181'.            00004800
012000 01 Z-MQC-SEND-REQUEST      PIC X(8) VALUE 'YYYS0182'.            00004900
012100 01 Z-MQC-GET-REPLY         PIC X(8) VALUE 'YYYS0183'.            00005000
012200 01 Z-MQC-CLOSE-Q           PIC X(8) VALUE 'YYYS0184'.            00005100
012300 01 Z-MQC-DISCONNECT        PIC X(8) VALUE 'YYYS0185'.            00005200
012400 01 Z-MQC-BEG-TRX           PIC X(8) VALUE 'YYYS0186'.            00005300
012410 01 Z-MQC-END-TRX           PIC X(8) VALUE 'YYYS0187'.            00005400
012510*                                                                 00005500
012520* ----------------------------------------------------------------00005600
012530* THESE ARE MQ SUPPORT MIDDLEWARE SUBROUTINES BATCH (NONBMP).     00005700
012540* ----------------------------------------------------------------00005800
012550*                                                                 00005900
012600 01 Z-MQB-CONNECT           PIC X(8) VALUE 'YYYS0190'.            00006000
012700 01 Z-MQB-OPEN-Q            PIC X(8) VALUE 'YYYS0191'.            00006100
012800 01 Z-MQB-SEND-REQUEST      PIC X(8) VALUE 'YYYS0192'.            00006200
012900 01 Z-MQB-GET-REPLY         PIC X(8) VALUE 'YYYS0193'.            00006300
013000 01 Z-MQB-CLOSE-Q           PIC X(8) VALUE 'YYYS0194'.            00006400
013100 01 Z-MQB-DISCONNECT        PIC X(8) VALUE 'YYYS0195'.            00006500
013200 01 Z-MQB-BEG-TRX           PIC X(8) VALUE 'YYYS0196'.            00006600
013300 01 Z-MQB-END-TRX           PIC X(8) VALUE 'YYYS0197'.            00006700
013301 01 Z-MQB-BACKOUT           PIC X(8) VALUE 'YYYS0198'.            00006800
012510*                                                                 00006900
012520* ----------------------------------------------------------------00007000
012530* THESE ARE MQ SUPPORT MIDDLEWARE SUBROUTINES IMS (TP/BATCH).     00007100
012540* ----------------------------------------------------------------00007200
012550*                                                                 00007300
012600 01 Z-MQI-CONNECT           PIC X(8) VALUE 'YYYS0200'.            00007400
012700 01 Z-MQI-OPEN-Q            PIC X(8) VALUE 'YYYS0201'.            00007500
012800 01 Z-MQI-SEND-REQUEST      PIC X(8) VALUE 'YYYS0202'.            00007600
012900 01 Z-MQI-GET-REPLY         PIC X(8) VALUE 'YYYS0203'.            00007700
013000 01 Z-MQI-CLOSE-Q           PIC X(8) VALUE 'YYYS0204'.            00007800
013100 01 Z-MQI-DISCONNECT        PIC X(8) VALUE 'YYYS0205'.            00007900
013200 01 Z-MQI-BEG-TRX           PIC X(8) VALUE 'YYYS0206'.            00008000
013300 01 Z-MQI-END-TRX           PIC X(8) VALUE 'YYYS0207'.            00008100
013301 01 Z-MQI-BACKOUT           PIC X(8) VALUE 'YYYS0208'.            00008200
013310*                                                                 00008300
013320* ----------------------------------------------------------------00008400
013330* THESE ARE COMMON MQ SUPPORT MIDDLEWARE SUBROUTINES              00008500
013340* ----------------------------------------------------------------00008600
013350*                                                                 00008700
013400 01 Z-MQC-GET-TRX-INFO      PIC X(8) VALUE 'YYYS0188'.            00008800
013500 01 Z-MQC-ERROR             PIC X(8) VALUE 'YYYS0189'.            00008900
