000100 01 MMMC0304.                                                     00000100
000110     05 MMMC0304-CONNECT              PIC X(1)  VALUE SPACES.     00000110
000120         88 MMMC0304-DB2                        VALUE SPACES.     00000120
000130         88 MMMC0304-ORACLE                     VALUE 'O'.        00000130
000200     05 MMMC0304-FUNC                 PIC X(02) VALUE SPACES.     00000200
000300         88 MMMC0304-DELETE-CHECK               VALUE 'D'.        00000300
000400         88 MMMC0304-UPDATE-CHECK               VALUE 'U'.        00000400
000500         88 MMMC0304-INSERT-CHECK               VALUE 'I'.        00000500
000600     05 MMMC0304-TABLE                PIC 9(3) VALUE ZEROES.      00000600
000700         88 MMMC0304-AP-LOCATION               VALUE 001.         00000700
000800         88 MMMC0304-AP-TYPE                   VALUE 002.         00000800
000900         88 MMMC0304-BKHAUL-LOC                VALUE 003.         00000900
001000         88 MMMC0304-BRACKET-UNIT-CODE         VALUE 004.         00001000
001100         88 MMMC0304-FC-FACILITY               VALUE 005.         00001100
001200         88 MMMC0304-FC-RETAIL-DEPTS           VALUE 006.         00001200
001300         88 MMMC0304-FC-TYPE-CODE              VALUE 007.         00001300
001400         88 MMMC0304-FC-WHSE-FACILITY          VALUE 008.         00001400
001500         88 MMMC0304-FC-RETAIL-STORES          VALUE 009.         00001500
001600         88 MMMC0304-LOCATION                  VALUE 010.         00001600
001700         88 MMMC0304-LOCATION-TYPE             VALUE 011.         00001700
001800         88 MMMC0304-PMM-VENDOR                VALUE 012.         00001800
001900         88 MMMC0304-RETAIL-LOC                VALUE 013.         00001900
002000         88 MMMC0304-RETL-LOC-SEGM             VALUE 014.         00002000
002100         88 MMMC0304-STR-DEPT                  VALUE 015.         00002100
002200         88 MMMC0304-VEND-SOURCING             VALUE 016.         00002200
002300         88 MMMC0304-VENDOR-LOCATION           VALUE 017.         00002300
002400     05 MMMC0304-STATUS               PIC X(01)  VALUE SPACES.    00002400
002500         88 MMMC0304-NO-CHILD                    VALUE ' '.       00002500
002600         88 MMMC0304-CHILD                       VALUE 'X'.       00002600
002700     05 MMMC0304-DATA                 PIC X(250) VALUE SPACES.    00002700
002800     05 REDEFINES MMMC0304-DATA.                                  00002800
002900        10 MMMC0304-AP-TYP-CD         PIC X(2).                   00002900
003000        10 MMMC0304-AP-NBR            PIC S9(9) USAGE COMP.       00003000
003100        10 FILLER                     PIC X(239).                 00003100
003200     05 REDEFINES MMMC0304-DATA.                                  00003200
003300        10 MMMC0304-LOC-TYP-CD        PIC X(2).                   00003300
003400        10 MMMC0304-LOC-NBR           PIC S9(9) USAGE COMP.       00003400
003500        10 FILLER                     PIC X(239).                 00003500
003600     05 REDEFINES MMMC0304-DATA.                                  00003600
003700        10 MMMC0304-BRKT-UNT-TYP      PIC X(1).                   00003700
003800        10 FILLER                     PIC X(249).                 00003800
003900     05 REDEFINES MMMC0304-DATA.                                  00003900
004000        10 MMMC0304-FC-FAC-NO         PIC S9(5)V USAGE COMP-3.    00004000
004100        10 MMMC0304-FC-OFFSITE-FAC-NO PIC S9(5)V USAGE COMP-3.    00004100
004200        10 MMMC0304-FC-TYP-CD         PIC X(2).                   00004200
004300        10 FILLER                     PIC X(238).                 00004300
004400     05 REDEFINES MMMC0304-DATA.                                  00004400
004500        10 MMMC0304-FC-DP-DEPT-NO     PIC S999V USAGE COMP-3.     00004500
004600        10 MMMC0304-FC-DP-SUB-DEPT-CD PIC X(1).                   00004600
004700        10 MMMC0304-FC-STORE-NO       PIC S99999V USAGE COMP-3.   00004700
004800        10 FILLER                     PIC X(241).                 00004800
004900     05 REDEFINES MMMC0304-DATA.                                  00004900
005000        10 MMMC0304-LIN-OF-BUS-ID     PIC S9(5)V USAGE COMP-3.    00005000
005100        10 FILLER                     PIC X(245).                 00005100
005200     05 REDEFINES MMMC0304-DATA.                                  00005200
005300        10 MMMC0304-RETL-LOC-SEGM-CD  PIC X(5).                   00005300
005400        10 FILLER                     PIC X(245).                 00005400
005500     05 REDEFINES MMMC0304-DATA.                                  00005500
005600        10 MMMC0304-STR-DEPT-NBR      PIC X(5).                   00005600
005700        10 MMMC0304-STR-SUB-DEPT-ID   PIC X(5).                   00005700
005800        10 FILLER                     PIC X(240).                 00005800
005900     05 REDEFINES MMMC0304-DATA.                                  00005900
006000        10 MMMC0304-FAC-ID            PIC S9(2)V USAGE COMP-3.    00006000
006100        10 MMMC0304-OMI-VEND-NBR      PIC S9(9) USAGE COMP.       00006100
006200        10 FILLER                     PIC X(239).                 00006200
006300     05 REDEFINES MMMC0304-DATA.                                  00006300
006400        10 MMMC0304-PMM-VENDOR-NUMBER PIC X(8).                   00006400
006500        10 FILLER                     PIC X(242).                 00006500
006600     05 REDEFINES MMMC0304-DATA.                                  00006600
006700        10 MMMC0304-WHSE-LOC-TYP-CD   PIC X(2).                   00006700
006800        10 FILLER                     PIC X(248).                 00006800
006900     05 REDEFINES MMMC0304-DATA.                                  00006900
007000        10 MMMC0304-ERR-TBL           PIC X(25).                  00007000
007100        10 FILLER                     PIC X(225).                 00007100
