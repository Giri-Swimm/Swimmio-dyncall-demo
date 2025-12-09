000100 01 MMMC0335.                                                     00000100
000200     05 MMMC0335-CONNECT              PIC X(1)  VALUE SPACES.     00000200
000300         88 MMMC0335-DB2                        VALUE SPACES.     00000300
000400         88 MMMC0335-ORACLE                     VALUE 'O'.        00000400
000500     05 MMMC0335-FUNC                    PIC X(02) VALUE SPACES.  00000500
000600         88 MMMC0335-INSERT-CHECK                  VALUE 'I'.     00000600
000700     05 MMMC0335-TABLE                   PIC 9(03) VALUE ZEROES.  00000700
000800         88 MMMC0335-BASE-CST-ALLOW                VALUE 001.     00000800
000900         88 MMMC0335-RETL-LOC-EVENT                VALUE 002.     00000900
001000         88 MMMC0335-RETL-LOC-QTY                  VALUE 003.     00001000
001100         88 MMMC0335-RETL-LOC-TRAIT                VALUE 004.     00001100
001200         88 MMMC0335-BDM-VEND-LOC                  VALUE 005.     00001200
001300         88 MMMC0335-LOC-SHPNG-OPT                 VALUE 006.     00001300
001400         88 MMMC0335-RETAIL-LOC                    VALUE 007.     00001400
001500         88 MMMC0335-RETL-LOC-CLS-AD-ZN          VALUE 008.       00001500
001600         88 MMMC0335-SCA-VEND-LOC                  VALUE 009.     00001600
001700         88 MMMC0335-VEND-TRXAL-CNTL               VALUE 010.     00001700
001800         88 MMMC0335-VENDOR-COMMENTS               VALUE 011.     00001800
001900     05 MMMC0335-STATUS                  PIC X(01) VALUE SPACES.  00001900
002000         88 MMMC0335-NO-PARENT                     VALUE ' '.     00002000
002100         88 MMMC0335-PARENT                        VALUE 'X'.     00002100
002200     05 MMMC0335-DATA                    PIC X(250) VALUE SPACES. 00002200
002300     05 REDEFINES MMMC0335-DATA.                                  00002300
002400        10 MMMC0335-BAS-ALLO-CD          PIC X(5).                00002400
002520        10 MMMC0335-BDM-CD               PIC X(5).                00002520
002530        10 FILLER                        PIC X(240).              00002530
002600     05 REDEFINES MMMC0335-DATA.                                  00002600
002700        10 MMMC0335-STR-EVENT-NBR        PIC S9(9) USAGE COMP.    00002700
002800        10 FILLER                        PIC X(241).              00002800
002900     05 REDEFINES MMMC0335-DATA.                                  00002900
003000        10 MMMC0335-STR-ATTRB-QTY-ID     PIC S9(9) USAGE COMP.    00003000
003100        10 FILLER                        PIC X(241).              00003100
003200     05 REDEFINES MMMC0335-DATA.                                  00003200
003300        10 MMMC0335-STR-TRAIT-NBR        PIC S9(9) USAGE COMP.    00003300
003420        10 MMMC0335-TRAIT-VAL-CD         PIC X(5).                00003420
003430        10 FILLER                        PIC X(236).              00003430
003800     05 REDEFINES MMMC0335-DATA.                                  00003800
003900        10 MMMC0335-CUST-SHPNG-METH-CD   PIC X(5).                00003900
004000        10 FILLER                        PIC X(245).              00004000
004100     05 REDEFINES MMMC0335-DATA.                                  00004100
004200        10 MMMC0335-ECOMM-MKT-AREA-CD    PIC X(5).                00004200
004210        10 MMMC0335-CMPTR-TYP-CD         PIC S9(5)V USAGE COMP-3. 00004210
004300        10 FILLER                        PIC X(240).              00004300
004400     05 REDEFINES MMMC0335-DATA.                                  00004400
004500        10 MMMC0335-ITM-CLS-CD           PIC S9(3)V USAGE COMP-3. 00004500
004620        10 MMMC0335-AD-ZONE              PIC S9(7)V USAGE COMP-3. 00004620
004621        10 MMMC0335-AD-ZONE-EXCP         PIC S9(7)V USAGE COMP-3. 00004621
004630        10 FILLER                        PIC X(233).              00004630
005000     05 REDEFINES MMMC0335-DATA.                                  00005000
005100        10 MMMC0335-SCA-CD               PIC X(5).                00005100
005200        10 FILLER                        PIC X(245).              00005200
005300     05 REDEFINES MMMC0335-DATA.                                  00005300
005400        10 MMMC0335-BUS-FUNC-ID          PIC S9(9) USAGE COMP.    00005400
005500        10 FILLER                        PIC X(241).              00005500
006200     05 REDEFINES MMMC0335-DATA.                                  00006200
006300        10 MMMC0335-CMT-TYP-CD           PIC X(5).                00006300
006400        10 FILLER                        PIC X(245).              00006400
006500     05 REDEFINES MMMC0335-DATA.                                  00006500
006600        10 MMMC0335-ERR-TBL              PIC X(25).               00006600
006700        10 FILLER                        PIC X(225).              00006700
