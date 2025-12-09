000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    YYYS0212.                                         00000200
000300 AUTHOR.        NAME.                                             00000300
000400 DATE-WRITTEN.  OCT, 1600.                                        00000400
001400 ENVIRONMENT DIVISION.                                            00001400
001500 CONFIGURATION SECTION.                                           00001500
001600 DATA DIVISION.                                                   00001600
001700 WORKING-STORAGE SECTION.                                         00001700
001800                                                                  00001800
001900 01 WS-ERR-MSG1                        PIC X(20) VALUE SPACES.    00001900
002000 01 WS-ERR-MSG2                        PIC X(20) VALUE SPACES.    00002000
002100 01 WS-ERR-MSG3                        PIC X(10) VALUE SPACES.    00002100
002200 01 WS-ERR-ORA-CODE                    PIC X(05) VALUE SPACES.    00002200
002300                                                                  00002300
002400                                                                  00002400
002500 LINKAGE SECTION.                                                 00002500
002600     COPY XXXN001A.                                               00002600
002700     EXEC SQL                                                     00002700
002800         INCLUDE SQLCA                                            00002800
002900     END-EXEC.                                                    00002900
003000                                                                  00003000
003100 PROCEDURE DIVISION USING XXXN001A                                00003100
003200                          SQLCA                                   00003200
003300     .                                                            00003300
003400                                                                  00003400
003500************************************************************      00003500
003600* MAIN PROGRAM LINE.                                              00003600
003700************************************************************      00003700
003800 000-MAIN-PROCESS.                                                00003800
003900     PERFORM 100-INITIALIZE                                       00003900
004000     PERFORM 200-FORMAT-USER-MSG-TXT                              00004000
004100     GOBACK                                                       00004100
004200     .                                                            00004200
004300                                                                  00004300
004400                                                                  00004400
004500*=================================================================00004500
004600* Initialize of course...                                         00004600
004700*=================================================================00004700
004800 100-INITIALIZE.                                                  00004800
004900     INITIALIZE  WS-ERR-MSG1                                      00004900
005000                 WS-ERR-MSG2                                      00005000
005100                 WS-ERR-MSG3                                      00005100
005200                 WS-ERR-ORA-CODE                                  00005200
005300     .                                                            00005300
005400                                                                  00005400
005500                                                                  00005500
005600*=================================================================00005600
005700* CONVERTING ORACLE ERROR MESSAGE TO CORRESPONDING DB2 SQL CODE   00005700
005800*=================================================================00005800
005900 200-FORMAT-USER-MSG-TXT.                                         00005900
006000     UNSTRING SQLERRMC  DELIMITED BY SPACE INTO                   00006000
006100                        WS-ERR-MSG1                               00006100
006200                        WS-ERR-MSG2                               00006200
006300                        WS-ERR-MSG3                               00006300
006400                        WS-ERR-ORA-CODE                           00006400
006500     EVALUATE WS-ERR-ORA-CODE                                     00006500
006510       WHEN  '60   '                                              00006510
006520         MOVE  -911                             TO SQLCODE        00006520
006600       WHEN  '904  '                                              00006600
006700       WHEN  '310  '                                              00006700
006800         MOVE  -206                             TO SQLCODE        00006800
006900       WHEN  '615  '                                              00006900
007000       WHEN  '616  '                                              00007000
007100         MOVE  -420                             TO SQLCODE        00007100
007200       WHEN  '942  '                                              00007200
007300         MOVE  -204                             TO SQLCODE        00007300
007400       WHEN  '1403 '                                              00007400
007500         MOVE  -100                             TO SQLCODE        00007500
007600       WHEN  '1001 '                                              00007600
007700         MOVE  -501                             TO SQLCODE        00007700
007800       WHEN  '1438 '                                              00007800
007900         MOVE  -413                             TO SQLCODE        00007900
008000       WHEN  '2112 '                                              00008000
008100       WHEN  '1422 '                                              00008100
008200         MOVE  -811                             TO SQLCODE        00008200
008300       WHEN  '2049 '                                              00008300
008400         MOVE  -913                             TO SQLCODE        00008400
008500       WHEN  '2291 '                                              00008500
008600         MOVE  -530                             TO SQLCODE        00008600
008700       WHEN  '2292 '                                              00008700
008800         MOVE  -532                             TO SQLCODE        00008800
008900       WHEN  '6502 '                                              00008900
009000         MOVE  -304                             TO SQLCODE        00009000
009100       WHEN  '6508 '                                              00009100
009200         MOVE  -440                             TO SQLCODE        00009200
009300       WHEN  '6511 '                                              00009300
009400         MOVE  -502                             TO SQLCODE        00009400
009500       WHEN  '6550 '                                              00009500
009600       WHEN  '6553 '                                              00009600
009700         MOVE  -440                             TO SQLCODE        00009700
009800       WHEN  '14028'                                              00009800
009900         MOVE  -538                             TO SQLCODE        00009900
010000       WHEN  '30006'                                              00010000
010100         MOVE  -904                             TO SQLCODE        00010100
010200       WHEN OTHER                                                 00010200
010300         STRING 'Error in YYYS0212. Oracle code:'                 00010300
010300                 WS-ERR-ORA-CODE                                  00010320
010300         DELIMITED BY SIZE INTO IS-RTRN-MSG2-TXT                  00010330
010500     END-EVALUATE                                                 00010500
010600     MOVE SPACES                                TO SQLERRMC       00010600
010700     .                                                            00010700
010800                                                                  00010800
010900                                                                  00010900
