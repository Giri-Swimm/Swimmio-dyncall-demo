000100 01 MMMC0291.                                                     00000100
000200     05 MMMC0291-INPUTS.                                          00000200
000300         10 MMMC0291-FUNC           PIC X(5)   VALUE 'TM2TS'.     00000300
000400             88 MMMC0291-CVT-TS-TO-TM          VALUE 'TS2TM'.     00000400
000500             88 MMMC0291-CVT-TM-TO-TS          VALUE 'TM2TS'.     00000500
005200     05 MMMC0291-INPUT-TM.                                        00005200
005300         10 WS-TIME-INOUT-CONV      PIC X(8)   OCCURS 14 TIMES.   00005300
005800     05 MMMC0291-INPUT-TS.                                        00005800
005900         10 WS-TIMSTAMP-INOUT-CONV  PIC X(26)  OCCURS 14 TIMES.   00005900
006000     05 FILLER                      PIC X(250) VALUE  SPACES.     00006000
