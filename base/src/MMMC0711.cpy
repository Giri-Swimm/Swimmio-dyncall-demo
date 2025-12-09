000100 01 MMMC0711.                                                     00000100
000110     05 MMMC0711-INPUTS.                                          00000110
000111         10 MMMC0711-FUNC           PIC X(4)   VALUE 'DSVI'.      00000111
000112             88 MMMC0711-IS-DSV-FUNC           VALUE 'DSV '.      00000113
000112             88 MMMC0711-IS-DSV-ITEM-FUNC      VALUE 'DSVI'.      00000114
000113         10 MMMC0711-I-ENTY-TYP     PIC X(5)   VALUE SPACES.      00000115
000112             88 XXX-ITEM-KEY-CD                VALUE 'ITMCD'.     00000116
000112             88 DSD-ITEM-KEY-CD                VALUE 'DSD  '.     00000117
000112             88 UPC-ITEM-KEY-CD                VALUE 'UPC  '.     00000118
000112             88 PRD-ITEM-KEY-CD                VALUE 'PROD '.     00000119
000114         10 MMMC0711-I-ENTY-ID      PIC S9(17) COMP-3 VALUE 0.    00000120
000114         10 MMMC0711-I-VEND-NBR     PIC S9(9)  COMP VALUE 0.      00000121
000114         10 MMMC0711-I-VEND-TYP-CD  PIC X(2)   VALUE SPACES.      00000122
000114         10 MMMC0711-I-USE-VEND     PIC X(1)   VALUE 'Y'.         00000123
000112             88 CHECK-WITH-VEND                VALUE 'Y'.         00000124
000112             88 DO-NOT-CHECK-WITH-VEND         VALUE 'N'.         00000125
000115         10 FILLER                  PIC X(49)  VALUE SPACES.      00000126
000120     05 MMMC0711-OUTPUTS.                                         00000130
000310         10 MMMC0711-DSV-SW         PIC X(1)   VALUE SPACES.      00000310
000320             88 VEND-IS-DSV                    VALUE 'Y'.         00000320
000330             88 VEND-IS-NOT-DSV                VALUE 'N'.         00000330
000310         10 MMMC0711-DSVI-SW        PIC X(1)   VALUE SPACES.      00000340
000320             88 ENTY-IS-DSV                    VALUE 'Y'.         00000350
000330             88 ENTY-IS-NOT-DSV                VALUE 'N'.         00000351
000310         10 MMMC0711-ENTY-EXISTS    PIC X(1)   VALUE SPACES.      00000352
000320             88 ENTY-EXISTS                    VALUE 'Y'.         00000353
000330             88 ENTY-DOES-NOT-EXIST            VALUE 'N'.         00000354
000360         10 FILLER                  PIC X(49)  VALUE SPACES.      00000360
