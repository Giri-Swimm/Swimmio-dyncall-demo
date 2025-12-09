000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    MMMS0162.                                         00000200
000300 AUTHOR.        NAME                                              00000300
000400 DATE-WRITTEN.  Circa 1600.                                       00000400
000500*---------------------------------------------------------------- 00000500
000600* Translate the 'old' database structure to the "new" database.   00000600
000700*                                                                 00000700
000800* Note that at some time in the future, after I retire, this      00000800
000900* will no longer be necessary.                                    00000900
001000*                                                                 00001000
001100* Databases translated...                                         00001100
001200*    OLD - XXXPST01(Item class - 12,13,14,36,37)                  00001200
001300*    NEW - XXXTCZ01                                               00001300
001400* Note that when converting from the old to the new               00001400
001500* you must pass the class in XXX-CLS-CD of XXXTCZ01               00001500
001600* For inbound (old to new) -                                      00001600
001700* The transition routine must call this module as many times as   00001700
001800* the number of XXX-CLASS details it has in the XXXPST01.         00001800
001900* --------------------------------------------------------------- 00001900
002400 ENVIRONMENT DIVISION.                                            00002400
002500 DATA DIVISION.                                                   00002500
002600 WORKING-STORAGE SECTION.                                         00002600
002700* --------------------------------------------------------------- 00002700
002800* Misc working storage...                                         00002800
002900* --------------------------------------------------------------- 00002900
003000                                                                  00003000
003100* --------------------------------------------------------------- 00003100
003200* Miscellaneous copy books go here...                             00003200
003300* --------------------------------------------------------------- 00003300
003400                                                                  00003400
003500* ----------------------------------------------------------------00003500
003600* DB2 stuff...                                                    00003600
003700* ----------------------------------------------------------------00003700
003800                                                                  00003800
003900 LINKAGE SECTION.                                                 00003900
004000 COPY XXXN001A.                                                   00004000
004100 COPY YYYN111A.                                                   00004100
004200 COPY PPPTCZ01.                                                   00004200
004300 COPY DDDPST01.                                                   00004300
004400 COPY PPPTRL01.                                                   00004400
004500                                                                  00004500
004600 PROCEDURE DIVISION USING                                         00004600
004700     XXXN001A                                                     00004700
004800     YYYN111A                                                     00004800
004900     P-DDDTCZ01                                                   00004900
005000     DDDPST01                                                     00005000
005100     P-DDDTRL01                                                   00005100
005200     .                                                            00005200
005300                                                                  00005300
005400***************************************************************** 00005400
005500* Start of program main line.                                     00005500
005600***************************************************************** 00005600
005700 000-MAIN.                                                        00005700
005800     PERFORM 100-INITIALIZE                                       00005800
005900                                                                  00005900
006000     EVALUATE TRUE                                                00006000
006100       WHEN YYYN111A-NEW-2-OLD                                    00006100
006200         PERFORM 200-NEW-2-OLD                                    00006200
006300                                                                  00006300
006400       WHEN YYYN111A-OLD-2-NEW                                    00006400
006500         PERFORM 500-OLD-2-NEW                                    00006500
006600                                                                  00006600
006700       WHEN OTHER                                                 00006700
006800         SET FAILURE TO TRUE                                      00006800
006900         MOVE 'MMMS0162 - Invalid translation function.'          00006900
007000           TO IS-RTRN-MSG-TXT                                     00007000
007100     END-EVALUATE                                                 00007100
007200                                                                  00007200
007300     GOBACK                                                       00007300
007400     .                                                            00007400
007500                                                                  00007500
007600                                                                  00007600
007700*================================================================ 00007700
007800* Initialization...                                               00007800
007900*================================================================ 00007900
008000 100-INITIALIZE.                                                  00008000
008100     INITIALIZE XXXN001A                                          00008100
008200     .                                                            00008200
008300                                                                  00008300
008400                                                                  00008400
008500*================================================================ 00008500
008600* Transalate from the new to the old...                           00008600
008700*================================================================ 00008700
008800 200-NEW-2-OLD.                                                   00008800
008900     PERFORM 210-POPULATE-DDDPST01                                00008900
009000     IF SUCCESS                                                   00009000
009100       PERFORM 220-POPULATE-DDDTRL01                              00009100
009200     END-IF                                                       00009200
009300     .                                                            00009300
009400                                                                  00009400
009500                                                                  00009500
009600*================================================================ 00009600
009700* Populate the DDDPST01 from the NEW database                     00009700
009800*================================================================ 00009800
009900 210-POPULATE-DDDPST01.                                           00009900
010000     MOVE LOC-TYP-CD                   OF P-DDDTCZ01              00010000
010100       TO ST-STORE-TYPE                                           00010100
010200                                                                  00010200
010300     MOVE LOC-NBR                      OF P-DDDTCZ01              00010300
010400       TO ST-STORE-NUMBER                                         00010400
010500                                                                  00010500
010600     EVALUATE ITM-CLS-CD                                          00010600
010700      WHEN 12                                                     00010700
010800                                                                  00010800
010900       MOVE AD-ZONE                    OF P-DDDTCZ01              00010900
011000         TO ST-CLASS12-ZONE                                       00011000
011100       MOVE AD-ZONE-EXCP               OF P-DDDTCZ01              00011100
011200         TO ST-CLASS12-EXCEPTION-AD-ZONE                          00011200
011300                                                                  00011300
011400      WHEN 13                                                     00011400
011500                                                                  00011500
011600       MOVE AD-ZONE                    OF P-DDDTCZ01              00011600
011700         TO ST-CLASS13-ZONE                                       00011700
011800       MOVE AD-ZONE-EXCP               OF P-DDDTCZ01              00011800
011900         TO ST-CLASS13-EXCEPTION-AD-ZONE                          00011900
012000                                                                  00012000
012100      WHEN 14                                                     00012100
012200                                                                  00012200
012300       MOVE AD-ZONE                    OF P-DDDTCZ01              00012300
012400         TO ST-CLASS14-ZONE                                       00012400
012500       MOVE AD-ZONE-EXCP               OF P-DDDTCZ01              00012500
012600         TO ST-CLASS14-EXCEPTION-AD-ZONE                          00012600
012700                                                                  00012700
012800      WHEN 36                                                     00012800
012900                                                                  00012900
013000       MOVE AD-ZONE-EXCP               OF P-DDDTCZ01              00013000
013100         TO ST-CLASS36-EXCEPTION-AD-ZONE                          00013100
013200                                                                  00013200
013300      WHEN 37                                                     00013300
013400                                                                  00013400
013500       MOVE AD-ZONE-EXCP               OF P-DDDTCZ01              00013500
013600         TO ST-CLASS37-EXCEPTION-AD-ZONE                          00013600
013700     END-EVALUATE                                                 00013700
013800     .                                                            00013800
013900                                                                  00013900
014000                                                                  00014000
014100*================================================================ 00014100
014200* Populate the DDDTRL01 from the NEW database                     00014200
014300*================================================================ 00014300
014400 220-POPULATE-DDDTRL01.                                           00014400
014500     MOVE LOC-TYP-CD                   OF P-DDDTCZ01              00014500
014600       TO FC-RL-STORE-CD                                          00014600
014700                                                                  00014700
014800     MOVE LOC-NBR                      OF P-DDDTCZ01              00014800
014900       TO FC-STORE-NO                                             00014900
015000                                                                  00015000
015100     EVALUATE ITM-CLS-CD                                          00015100
015200      WHEN 12                                                     00015200
015300       MOVE AD-ZONE                    OF P-DDDTCZ01              00015300
015400         TO FC-RL-CL12-ZONE-NO                                    00015400
015500       MOVE AD-ZONE-EXCP               OF P-DDDTCZ01              00015500
015600         TO FC-RL-CL12-ADZN-NO                                    00015600
015700                                                                  00015700
015800      WHEN 13                                                     00015800
015900                                                                  00015900
016000       MOVE AD-ZONE                    OF P-DDDTCZ01              00016000
016100         TO FC-RL-CL13-ZONE-NO                                    00016100
016200       MOVE AD-ZONE-EXCP               OF P-DDDTCZ01              00016200
016300         TO FC-RL-CL13-ADZN-NO                                    00016300
016400                                                                  00016400
016500      WHEN 14                                                     00016500
016600                                                                  00016600
016700       MOVE AD-ZONE                    OF P-DDDTCZ01              00016700
016800         TO FC-RL-CL14-ZONE-NO                                    00016800
016900       MOVE AD-ZONE-EXCP               OF P-DDDTCZ01              00016900
017000         TO FC-RL-CL14-ADZN-NO                                    00017000
017100                                                                  00017100
017200      WHEN 36                                                     00017200
017300                                                                  00017300
017400       MOVE AD-ZONE-EXCP               OF P-DDDTCZ01              00017400
017500         TO FC-RL-CL36-ADZN-NO                                    00017500
017600                                                                  00017600
017700      WHEN 37                                                     00017700
017800                                                                  00017800
017900       MOVE AD-ZONE-EXCP               OF P-DDDTCZ01              00017900
018000         TO FC-RL-CL37-ADZN-NO                                    00018000
018100     END-EVALUATE                                                 00018100
018200     .                                                            00018200
018300                                                                  00018300
018400                                                                  00018400
018500*================================================================ 00018500
018600* Transalate from the old to the new...                           00018600
018700*================================================================ 00018700
018800 500-OLD-2-NEW.                                                   00018800
018900     MOVE ST-STORE-TYPE                                           00018900
019000       TO LOC-TYP-CD                   OF P-DDDTCZ01              00019000
019100                                                                  00019100
019200     MOVE ST-STORE-NUMBER                                         00019200
019300       TO LOC-NBR                      OF P-DDDTCZ01              00019300
019400                                                                  00019400
019500     EVALUATE ITM-CLS-CD                                          00019500
019600      WHEN 12                                                     00019600
019900       MOVE ST-CLASS12-ZONE                                       00019900
020000         TO AD-ZONE                    OF P-DDDTCZ01              00020000
020100       MOVE ST-CLASS12-EXCEPTION-AD-ZONE                          00020100
020200         TO AD-ZONE-EXCP               OF P-DDDTCZ01              00020200
020300                                                                  00020300
020400      WHEN 13                                                     00020400
020700       MOVE ST-CLASS13-ZONE                                       00020700
020800         TO AD-ZONE                    OF P-DDDTCZ01              00020800
020900       MOVE ST-CLASS13-EXCEPTION-AD-ZONE                          00020900
021000         TO AD-ZONE-EXCP               OF P-DDDTCZ01              00021000
021100                                                                  00021100
021200      WHEN 14                                                     00021200
021500       MOVE ST-CLASS14-ZONE                                       00021500
021600         TO AD-ZONE                    OF P-DDDTCZ01              00021600
021700       MOVE ST-CLASS14-EXCEPTION-AD-ZONE                          00021700
021800         TO AD-ZONE-EXCP               OF P-DDDTCZ01              00021800
021900                                                                  00021900
022000      WHEN 36                                                     00022000
022300       MOVE ST-CLASS36-EXCEPTION-AD-ZONE                          00022300
022400         TO AD-ZONE                    OF P-DDDTCZ01              00022400
022500       MOVE ST-CLASS36-EXCEPTION-AD-ZONE                          00022500
022600         TO AD-ZONE-EXCP               OF P-DDDTCZ01              00022600
022700                                                                  00022700
022800      WHEN 37                                                     00022800
023000       MOVE 37                                                    00023000
023100         TO ITM-CLS-CD                 OF P-DDDTCZ01              00023100
023200       MOVE ST-CLASS37-EXCEPTION-AD-ZONE                          00023200
023300         TO AD-ZONE                    OF P-DDDTCZ01              00023300
023400       MOVE ST-CLASS37-EXCEPTION-AD-ZONE                          00023400
023500         TO AD-ZONE-EXCP               OF P-DDDTCZ01              00023500
023600     END-EVALUATE                                                 00023600
023700     .                                                            00023700
