000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    MMMS0258.                                         00000200
000300 AUTHOR.        NAME.                                             00000300
000400 DATE-WRITTEN.  mar 1600.                                         00000400
000500*---------------------------------------------------------------- 00000500
002100 ENVIRONMENT DIVISION.                                            00002100
002200 DATA DIVISION.                                                   00002200
002300 WORKING-STORAGE SECTION.                                         00002300
002400* --------------------------------------------------------------- 00002400
002500* Misc working storage...                                         00002500
002600* --------------------------------------------------------------- 00002600
002700 01 WS-CT-DEPARTMENT-SEQUENCE         PIC X(3) VALUE '200'.       00002700
002800                                                                  00002800
002900 01 WS-STR-DEPT-NBR-VALUE             PIC 9(5)V VALUE ZEROES.     00002900
003000 01 WS-STR-DEPT-NBR.                                              00003000
003100     05 WS-STR-DEPT-NBR-N             PIC X(5) VALUE SPACES.      00003100
003200     05 WS-STR-DEPT-NBR-X REDEFINES WS-STR-DEPT-NBR-N.            00003200
003300        10 WS-STR-DEPT-NBR-X-L2       PIC X(2).                   00003300
003400        10 WS-STR-DEPT-NBR-X-L3       PIC X(3).                   00003400
003500                                                                  00003500
003600 01 WS-STR-DEPT.                                                  00003600
003700     05 WS-STR-DEPT-X2                PIC X(2) VALUE SPACES.      00003700
003800     05 WS-STR-DEPT-N2 REDEFINES WS-STR-DEPT-X2                   00003800
003900                                      PIC 9(2).                   00003900
004000                                                                  00004000
004100 01 WS-STR-DEPT-X3                    PIC X(2) VALUE SPACES.      00004100
004200 01 WS-STR-DEPT-N3                    PIC 9(2) VALUE 0.           00004200
004300                                                                  00004300
004400* --------------------------------------------------------------- 00004400
004500* Miscellaneous copy books go here...                             00004500
004600* --------------------------------------------------------------- 00004600
004700 COPY MMMC9012.                                                   00004700
004800 COPY MMMK001B.                                                   00004800
004900 COPY YYYN000A.                                                   00004900
005000 COPY MMMN000A.                                                   00005000
005100                                                                  00005100
005200* ----------------------------------------------------------------00005200
005300* DB2 stuff...                                                    00005300
005400* ----------------------------------------------------------------00005400
005500     EXEC SQL                                                     00005500
005600       INCLUDE SQLCA                                              00005600
005700     END-EXEC                                                     00005700
005800                                                                  00005800
005900 LINKAGE SECTION.                                                 00005900
006000 COPY XXXN001A.                                                   00006000
006100 COPY YYYN111A.                                                   00006100
006200 COPY DDDTDP01.                                                   00006200
006300 COPY DDDLCT20.                                                   00006300
006400                                                                  00006400
006500 PROCEDURE DIVISION USING                                         00006500
006600     XXXN001A                                                     00006600
006700     YYYN111A                                                     00006700
006800     P-DDDTDP01                                                   00006800
006900     DDDLCT20                                                     00006900
007000     .                                                            00007000
007100                                                                  00007100
007200                                                                  00007200
007300***************************************************************** 00007300
007400* Start of program main line.                                     00007400
007500***************************************************************** 00007500
007600 000-MAIN.                                                        00007600
007700     PERFORM 100-INITIALIZE                                       00007700
007800                                                                  00007800
007900     EVALUATE TRUE                                                00007900
008000       WHEN YYYN111A-NEW-2-OLD                                    00008000
008100         PERFORM 200-NEW-2-OLD                                    00008100
008200                                                                  00008200
008300       WHEN YYYN111A-OLD-2-NEW                                    00008300
008400         PERFORM 500-OLD-2-NEW                                    00008400
008500                                                                  00008500
008600       WHEN OTHER                                                 00008600
008700         SET FAILURE TO TRUE                                      00008700
008800         MOVE 'MMMS0258 - Invalid translation function.'          00008800
008900           TO IS-RTRN-MSG-TXT                                     00008900
009000     END-EVALUATE                                                 00009000
009100                                                                  00009100
009200     GOBACK                                                       00009200
009300     .                                                            00009300
009400                                                                  00009400
009500                                                                  00009500
009600*================================================================ 00009600
009700* Initialization...                                               00009700
009800*================================================================ 00009800
009900 100-INITIALIZE.                                                  00009900
010000     INITIALIZE XXXN001A                                          00010000
010100     .                                                            00010100
010200                                                                  00010200
010300                                                                  00010300
010400*================================================================ 00010400
010500* Transalate from the new to the old...                           00010500
010600*================================================================ 00010600
010700 200-NEW-2-OLD.                                                   00010700
010800     MOVE WS-CT-DEPARTMENT-SEQUENCE                               00010800
010900       TO CT-DEPARTMENT-SEQUENCE      OF DDDLCT20                 00010900
011000     MOVE DEPT-NM                     OF P-DDDTDP01               00011000
011100       TO CT-DEPARTMENT-NAME          OF DDDLCT20                 00011100
011200     MOVE DEPT-ABB                    OF P-DDDTDP01               00011200
011300       TO ST-DEPARTMENT-ABBREVIATION  OF DDDLCT20                 00011300
011400     MOVE REPT-GRP-CD                 OF P-DDDTDP01               00011400
011500       TO OA-REPT-GRP-CD              OF DDDLCT20                 00011500
011600     MOVE GRPRFT-LO-PCT               OF P-DDDTDP01               00011600
011700       TO OA-GRS-PRFT-LO-PCT          OF DDDLCT20                 00011700
011800     MOVE GRPRFT-HI-PCT               OF P-DDDTDP01               00011800
011900       TO OA-GRS-PRFT-HI-PCT          OF DDDLCT20                 00011900
012000     MOVE SHRNK-LO-PCT                OF P-DDDTDP01               00012000
012100       TO OA-SHRINK-LO-PCT            OF DDDLCT20                 00012100
012200     MOVE SHRNK-HI-PCT                OF P-DDDTDP01               00012200
012300       TO OA-SHRINK-HI-PCT            OF DDDLCT20                 00012300
012400                                                                  00012400
012500     PERFORM 210-DEPT-CONV-NUM-2-ALPHA                            00012500
012600     .                                                            00012600
012700                                                                  00012700
012800                                                                  00012800
012900*================================================================ 00012900
013000* Format str-dept-nbr....                                         00013000
013100*================================================================ 00013100
013200 210-DEPT-CONV-NUM-2-ALPHA.                                       00013200
013300     MOVE STR-DEPT-NBR     (1:2)                                  00013300
013400       TO ST-DEPARTMENT-KEY(1:2)                                  00013400
013500                                                                  00013500
013600     MOVE STR-SUB-DEPT-ID  (1:1)                                  00013600
013700       TO ST-DEPARTMENT-KEY(3:1)                                  00013700
013800     .                                                            00013800
013900                                                                  00013900
014000                                                                  00014000
014100*================================================================ 00014100
014200* Transalate from the old to the new...                           00014200
014300*================================================================ 00014300
014400 500-OLD-2-NEW.                                                   00014400
014500     MOVE ST-DEPARTMENT-KEY                                       00014500
014600       TO WS-STR-DEPT-NBR                                         00014600
014700     MOVE CT-DEPARTMENT-NAME                                      00014700
014800       TO DEPT-NM                     OF P-DDDTDP01               00014800
014900     MOVE ST-DEPARTMENT-ABBREVIATION                              00014900
015000       TO DEPT-ABB                    OF P-DDDTDP01               00015000
015100                                                                  00015100
015200     IF OA-REPT-GRP-CD        NOT NUMERIC                         00015200
015300        MOVE ZERO                                                 00015300
015400          TO REPT-GRP-CD              OF P-DDDTDP01               00015400
015500     ELSE                                                         00015500
015600        MOVE OA-REPT-GRP-CD                                       00015600
015700          TO REPT-GRP-CD              OF P-DDDTDP01               00015700
015800     END-IF                                                       00015800
015900                                                                  00015900
016000     IF OA-GRS-PRFT-LO-PCT    NOT NUMERIC                         00016000
016100        MOVE ZERO                                                 00016100
016200          TO GRPRFT-LO-PCT            OF P-DDDTDP01               00016200
016300     ELSE                                                         00016300
016400        MOVE OA-GRS-PRFT-LO-PCT                                   00016400
016500          TO GRPRFT-LO-PCT            OF P-DDDTDP01               00016500
016600     END-IF                                                       00016600
016700                                                                  00016700
016800     IF OA-GRS-PRFT-HI-PCT    NOT NUMERIC                         00016800
016900        MOVE ZERO                                                 00016900
017000          TO GRPRFT-HI-PCT            OF P-DDDTDP01               00017000
017100     ELSE                                                         00017100
017200        MOVE OA-GRS-PRFT-HI-PCT                                   00017200
017300          TO GRPRFT-HI-PCT            OF P-DDDTDP01               00017300
017400     END-IF                                                       00017400
017500                                                                  00017500
017600     IF OA-SHRINK-LO-PCT      NOT NUMERIC                         00017600
017700        MOVE ZERO                                                 00017700
017800          TO SHRNK-LO-PCT             OF P-DDDTDP01               00017800
017900     ELSE                                                         00017900
018000        MOVE OA-SHRINK-LO-PCT                                     00018000
018100          TO SHRNK-LO-PCT             OF P-DDDTDP01               00018100
018200     END-IF                                                       00018200
018300                                                                  00018300
018400     IF OA-SHRINK-HI-PCT      NOT NUMERIC                         00018400
018500        MOVE ZERO                                                 00018500
018600          TO SHRNK-HI-PCT             OF P-DDDTDP01               00018600
018700     ELSE                                                         00018700
018800        MOVE OA-SHRINK-HI-PCT                                     00018800
018900          TO SHRNK-HI-PCT             OF P-DDDTDP01               00018900
019000     END-IF                                                       00019000
019100                                                                  00019100
019200     PERFORM 510-POPULATE-STR-DEPT                                00019200
019300     .                                                            00019300
019400                                                                  00019400
019500                                                                  00019500
019600*================================================================ 00019600
019700* Format str-dept-nbr....                                         00019700
019800*================================================================ 00019800
019900 510-POPULATE-STR-DEPT.                                           00019900
020000     MOVE WS-STR-DEPT-NBR-X-L3(1:1)                               00020000
020100       TO STR-SUB-DEPT-ID OF P-DDDTDP01                           00020100
020200     MOVE WS-STR-DEPT-NBR-X-L2(1:2)                               00020200
020300       TO WS-STR-DEPT                                             00020300
020400                                                                  00020400
020500     IF WS-STR-DEPT-N2 IS NUMERIC                                 00020500
020600       MOVE WS-STR-DEPT-N2                                        00020600
020700         TO STR-DEPT-NBR            OF P-DDDTDP01                 00020700
020800     ELSE                                                         00020800
020900       MOVE WS-STR-DEPT-X2(2:1)                                   00020900
021000         TO WS-STR-DEPT-X3(1:1)                                   00021000
021100       IF WS-STR-DEPT-X3(1:1) IS NUMERIC                          00021100
021200          MOVE WS-STR-DEPT-X3(1:1)                                00021200
021300            TO WS-STR-DEPT-N3                                     00021300
021400          MOVE WS-STR-DEPT-N3                                     00021400
021500            TO STR-DEPT-NBR            OF P-DDDTDP01              00021500
021600       ELSE                                                       00021600
021700          MOVE WS-STR-DEPT-X3(1:1)                                00021700
021800            TO STR-DEPT-NBR            OF P-DDDTDP01              00021800
021900       END-IF                                                     00021900
022000     END-IF                                                       00022000
022100     .                                                            00022100
