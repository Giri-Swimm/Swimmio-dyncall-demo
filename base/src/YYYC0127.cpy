000100 01 YYYC0127.                                                     00000100
000200     05 YYYC0127-TS                  PIC X(26) VALUE SPACES.      00000200
000300     05 REDEFINES YYYC0127-TS.                                    00000300
000400         10 TS-YYYY                  PIC 9(4).                    00000400
000500         10 REDEFINES TS-YYYY.                                    00000500
000600             15 TS-CC                PIC 9(2).                    00000600
000700             15 TS-YY                PIC 9(2).                    00000700
000800         10 TS-D1                    PIC X(1).                    00000800
000900         10 TS-MM                    PIC 9(2).                    00000900
001000         10 TS-D2                    PIC X(1).                    00001000
001100         10 TS-DD                    PIC 9(2).                    00001100
001200         10 TS-D3                    PIC X(1).                    00001200
001300         10 TS-HH                    PIC 9(2).                    00001300
001400         10 TS-P1                    PIC X(1).                    00001400
001500         10 TS-II                    PIC 9(2).                    00001500
001600         10 TS-P2                    PIC X(1).                    00001600
001700         10 TS-SS                    PIC 9(2).                    00001700
001800         10 TS-P3                    PIC X(1).                    00001800
001900         10 TS-NNNNNN                PIC 9(6).                    00001900
002000                                                                  00002000
002100     05 YYYC0127-DATES.                                           00002100
002200         10 YYYC0127-DT-MASK         PIC X(1) VALUE SPACES.       00002200
002300                                                                  00002300
002400         10 DTA4-MMDD                PIC X(4) VALUE SPACES.       00002400
002500         10 REDEFINES DTA4-MMDD.                                  00002500
002600             15 DTA4-MMDD-MM          PIC 9(2).                   00002600
002700             15 DTA4-MMDD-DD          PIC 9(2).                   00002700
002800                                                                  00002800
002900         10 DTA5-MM-DD                PIC X(5) VALUE SPACES.      00002900
003000         10 REDEFINES DTA5-MM-DD.                                 00003000
003100             15 DTA5-MM-DD-MM         PIC 9(2).                   00003100
003200             15 DTA5-MM-DD-S1         PIC X(1).                   00003200
003300             15 DTA5-MM-DD-DD         PIC 9(2).                   00003300
003400                                                                  00003400
003500         10 DTA6-MMDDYY               PIC X(6) VALUE SPACES.      00003500
003600         10 REDEFINES DTA6-MMDDYY.                                00003600
003700             15 DTA6-MMDDYY-MM        PIC 9(2).                   00003700
003800             15 DTA6-MMDDYY-DD        PIC 9(2).                   00003800
003900             15 DTA6-MMDDYY-YY        PIC 9(2).                   00003900
004000         10 DTA8-MM-DD-YY             PIC X(8) VALUE SPACES.      00004000
004100         10 REDEFINES DTA8-MM-DD-YY.                              00004100
004200             15 DTA8-MM-DD-YY-MM      PIC 9(2).                   00004200
004300             15 DTA8-MM-DD-YY-S1      PIC X(1).                   00004300
004400             15 DTA8-MM-DD-YY-DD      PIC 9(2).                   00004400
004500             15 DTA8-MM-DD-YY-S2      PIC X(1).                   00004500
004600             15 DTA8-MM-DD-YY-YY      PIC 9(2).                   00004600
004700                                                                  00004700
004800         10 DTA8-MMDDYYYY             PIC X(8) VALUE SPACES.      00004800
004900         10 REDEFINES DTA8-MMDDYYYY.                              00004900
005000             15 DTA8-MMDDYYYY-MM      PIC 9(2).                   00005000
005100             15 DTA8-MMDDYYYY-DD      PIC 9(2).                   00005100
005200             15 DTA8-MMDDYYYY-YYYY    PIC 9(4).                   00005200
005300                                                                  00005300
005400         10 DTA10-MM-DD-YYYY          PIC X(10) VALUE SPACES.     00005400
005500         10 REDEFINES DTA10-MM-DD-YYYY.                           00005500
005600             15 DTA10-MM-DD-YYYY-MM   PIC 9(2).                   00005600
005700             15 DTA10-MM-DD-YYYY-S1   PIC X(1).                   00005700
005800             15 DTA10-MM-DD-YYYY-DD   PIC 9(2).                   00005800
005900             15 DTA10-MM-DD-YYYY-S2   PIC X(1).                   00005900
006000             15 DTA10-MM-DD-YYYY-YYYY PIC 9(4).                   00006000
006010             15 REDEFINES DTA10-MM-DD-YYYY-YYYY.                  00006010
006020                 20 DTA10-MM-DD-YYYY-CC PIC 9(2).                 00006020
006030                 20 DTA10-MM-DD-YYYY-YY PIC 9(2).                 00006030
006100                                                                  00006100
006200         10 DTA-YYDDD                 PIC X(5)  VALUE SPACES.     00006200
006201         10 REDEFINES DTA-YYDDD.                                  00006201
006202             15 DTA-YYDDD-YY          PIC 9(2).                   00006202
006203             15 DTA-YYDDD-DDD         PIC 9(3).                   00006203
006204         10 REDEFINES DTA-YYDDD.                                  00006204
006205             15 DTN-YYDDD             PIC 9(5).                   00006205
006207                                                                  00006207
006208         10 DTA-DOW                   PIC X(1)  VALUE SPACES.     00006208
006209         10 REDEFINES DTA-DOW.                                    00006209
006210             15 DTN-DOW               PIC 9(1).                   00006210
006211                                                                  00006211
006212         10 DTA-YYYYDDD               PIC X(7)  VALUE SPACES.     00006212
006213         10 REDEFINES DTA-YYYYDDD.                                00006213
006214             15 DTA-YYYYDDD-CC        PIC 9(2).                   00006214
006215             15 DTA-YYYYDDD-YY        PIC 9(2).                   00006215
006216             15 DTA-YYYYDDD-DDD       PIC 9(3).                   00006216
006217         10 REDEFINES DTA-YYYYDDD.                                00006217
006218             15 DTN-YYYYDDD           PIC 9(7).                   00006218
006220         10 FILLER                    PIC X(67) VALUE SPACES.     00006220
006300                                                                  00006300
006400     05 YYYC0127-TIMES.                                           00006400
006500         10 YYYC0127-TM-MASK         PIC X(1) VALUE SPACES.       00006500
006600                                                                  00006600
006700         10 TMA4-HHII                PIC X(4) VALUE SPACES.       00006700
006800         10 REDEFINES TMA4-HHII.                                  00006800
006900             15 TMA4-HHII-HH         PIC 9(2).                    00006900
007000             15 TMA4-HHII-II         PIC 9(2).                    00007000
007010         10 REDEFINES TMA4-HHII.                                  00007010
007020             15 TMN4-HHII            PIC 9(4).                    00007020
007100                                                                  00007100
007200         10 TMA5-HH-II               PIC X(5) VALUE SPACES.       00007200
007300         10 REDEFINES TMA5-HH-II.                                 00007300
007400             15 TMA5-HH-II-HH        PIC 9(2).                    00007400
007500             15 TMA5-HH-II-S1        PIC X(1).                    00007500
007600             15 TMA5-HH-II-II        PIC 9(2).                    00007600
007700                                                                  00007700
007800         10 TMA6-HHIISS              PIC X(6) VALUE SPACES.       00007800
007900         10 REDEFINES TMA6-HHIISS.                                00007900
008000             15 TMA6-HHIISS-HH       PIC 9(2).                    00008000
008100             15 TMA6-HHIISS-II       PIC 9(2).                    00008100
008200             15 TMA6-HHIISS-SS       PIC 9(2).                    00008200
008210         10 REDEFINES TMA6-HHIISS.                                00008210
008220             15 TMN6-HHIISS          PIC 9(6).                    00008220
008300                                                                  00008300
008400         10 TMA8-HH-II-SS            PIC X(8) VALUE SPACES.       00008400
008500         10 REDEFINES TMA8-HH-II-SS.                              00008500
008600             15 TMA8-HH-II-SS-HH     PIC 9(2).                    00008600
008700             15 TMA8-HH-II-SS-S1     PIC X(1).                    00008700
008800             15 TMA8-HH-II-SS-II     PIC 9(2).                    00008800
008900             15 TMA8-HH-II-SS-S2     PIC X(1).                    00008900
009000             15 TMA8-HH-II-SS-SS     PIC 9(2).                    00009000
009100                                                                  00009100
009200         10 FILLER                   PIC X(80) VALUE SPACES.      00009200
