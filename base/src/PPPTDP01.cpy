000100 01 P-DDDTDP01.                                                   00000100
000200     05 STR-DEPT-NBR                   PIC X(5)  VALUE SPACES.    00000200
000210     05 REDEFINES STR-DEPT-NBR.                                   00000210
000220         10 STR-DEPT-NBR-N2            PIC 9(2).                  00000220
000230         10 STR-DEPT-NBR-F1            PIC X(3).                  00000230
000300     05 STR-SUB-DEPT-ID                PIC X(5)  VALUE SPACES.    00000300
000400     05 DEPT-NM                        PIC X(30)  VALUE SPACES.   00000400
000500     05 DEPT-ABB                       PIC X(6)  VALUE SPACES.    00000500
000600     05 REPT-GRP-CD                    PIC S9(9) COMP VALUE 0.    00000600
000700     05 GRPRFT-LO-PCT                  PIC S9(3)V9(4) COMP-3      00000700
000800                                       VALUE +0.                  00000800
000900     05 GRPRFT-HI-PCT                  PIC S9(3)V9(4) COMP-3      00000900
001000                                       VALUE +0.                  00001000
001100     05 SHRNK-LO-PCT                   PIC S9(3)V9(4) COMP-3      00001100
001200                                       VALUE +0.                  00001200
001300     05 SHRNK-HI-PCT                   PIC S9(3)V9(4) COMP-3      00001300
001400                                       VALUE +0.                  00001400
001500     05 LST-UPDT-USR-ID                PIC X(8)  VALUE SPACES.    00001500
001600     05 LST-UPDT-TS                    PIC X(26)  VALUE SPACES.   00001600
001700     05 ORG-ID                         PIC S9(9) COMP VALUE 0.    00001700
001800     05 ORG-ID-X REDEFINES                                        00001800
001900                 ORG-ID                PIC X(4).                  00001900
002000     05 FILLER                         PIC X(246) VALUE SPACES.   00002000