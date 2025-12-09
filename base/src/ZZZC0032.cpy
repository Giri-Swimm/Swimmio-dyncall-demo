000100 01 ZZZC0032.                                                     00000100
000200     05 ST-STORE-KEY.                                             00000200
000300         10 ST-STORE-NUMBER          PIC 9(5)   VALUE 0.          00000300
000310     05 ZZZC0032-FXXX-FUNC           PIC X(1)   VALUE SPACES.     00000310
000320         88 ZZZC0032-DONT-UPD-FXXX              VALUE 'X'.        00000320
000330         88 ZZZC0032-UPD-FXXX                   VALUE ' '.        00000330
000400     05 FILLER                       PIC X(254) VALUE SPACES.     00000400
