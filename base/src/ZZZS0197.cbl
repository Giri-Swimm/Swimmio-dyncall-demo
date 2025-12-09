000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    ZZZS0197.                                         00000200
000300 AUTHOR.        NAME.                                             00000300
000400 DATE-WRITTEN.  Circa 1600.                                       00000400
000500 DATE-COMPILED.                                                   00000500
000600*-----------------------------------------------------------------00000600
000700* Generic event issuer for master data entities and relationships.00000700
000800* ----------------------------------------------------------------00000800
003600 ENVIRONMENT DIVISION.                                            00003600
003700 CONFIGURATION SECTION.                                           00003700
003800 DATA DIVISION.                                                   00003800
003900 WORKING-STORAGE SECTION.                                         00003900
004000*=================================================================00004000
004100* Misc working storage.                                           00004100
004200*=================================================================00004200
004300 01 WS-EVENT-STAGER                 PIC X(8) VALUE 'YYYS0175'.    00004300
004400                                                                  00004400
004500 01 WS-CONTROLS.                                                  00004500
004600     05 WS-FEET-PRINT               PIC X(01) VALUE SPACES.       00004600
004700         88 NOT-BEEN-HERE-BEFORE              VALUE ' '.          00004700
004800         88 BEEN-HERE-BEFORE                  VALUE 'X'.          00004800
004900     05 WS-FEET-PRINT               PIC X(01) VALUE SPACES.       00004900
005000         88 WEED-EVENT                        VALUE ' '.          00005000
005100         88 PROCESS-EVENT                     VALUE 'X'.          00005100
005200                                                                  00005200
005300 01 WS-PREV-COMPARE-DATA            PIC X(100) VALUE SPACES.      00005300
005400 01 WS-PREV-EVENT-TRX               PIC X(4)   VALUE SPACES.      00005400
005500                                                                  00005500
005600 01 I                               PIC S9(9) COMP VALUE 0.       00005600
005700 01 WS-MAX-EVENTS                   PIC S9(9) COMP VALUE 100.     00005700
005800 01 WS-EVENT-TABLE.                                               00005800
005900     05 WS-NUM-EVENTS               PIC S9(9) COMP VALUE 0.       00005900
006000     05 WS-EVENTS OCCURS 100 TIMES.                               00006000
006100         10 WS-EVENT-TRX            PIC X(4).                     00006100
006200         10 WS-COMPARE-DATA         PIC X(100).                   00006200
006300                                                                  00006300
006400*=================================================================00006400
006500* Misc copy books.                                                00006500
006600*=================================================================00006600
006700 COPY YYYN000A.                                                   00006700
006800 COPY YYYC0175.                                                   00006800
006900                                                                  00006900
007000 LINKAGE SECTION.                                                 00007000
007100     COPY XXXN001A.                                               00007100
007200     COPY YYYN110A.                                               00007200
007300     COPY ZZZC0197.                                               00007300
007400                                                                  00007400
007500 PROCEDURE DIVISION USING                                         00007500
007600     XXXN001A                                                     00007600
007700     YYYN110A                                                     00007700
007800     ZZZC0197                                                     00007800
007900     .                                                            00007900
008000                                                                  00008000
008100*=================================================================00008100
008200* Main program logic...                                           00008200
008300*=================================================================00008300
008400 000-MAINLINE.                                                    00008400
008500     PERFORM 100-INITIALIZE                                       00008500
008600                                                                  00008600
008700*    IF  ZZZC0197-TRX-ID NOT = 'CFIP'                             00008700
008800       PERFORM 200-WEED-EVENT                                     00008800
008900*    END-IF                                                       00008900
009000                                                                  00009000
009100     IF PROCESS-EVENT                                             00009100
009200       PERFORM 300-ISSUE-EVENT                                    00009200
009300     END-IF                                                       00009300
009400                                                                  00009400
009500     GOBACK                                                       00009500
009600     .                                                            00009600
009700                                                                  00009700
009800                                                                  00009800
009900*=================================================================00009900
010000* Initialization stuff...                                         00010000
010100*=================================================================00010100
010200 100-INITIALIZE.                                                  00010200
010300     INITIALIZE XXXN001A                                          00010300
010400     SET PROCESS-EVENT TO TRUE                                    00010400
010500                                                                  00010500
010600     IF NOT-BEEN-HERE-BEFORE                                      00010600
010700       INITIALIZE WS-EVENT-TABLE                                  00010700
010800       SET BEEN-HERE-BEFORE TO TRUE                               00010800
010900     END-IF                                                       00010900
011000     .                                                            00011000
011100                                                                  00011100
011200                                                                  00011200
011300*=================================================================00011300
011400* Weed prior events we may have sent...                           00011400
011500*=================================================================00011500
011600 200-WEED-EVENT.                                                  00011600
011700     SET PROCESS-EVENT TO TRUE                                    00011700
011800     PERFORM 210-WEED-BY-HARD-CODE                                00011800
011900     IF PROCESS-EVENT                                             00011900
012000       PERFORM 220-WEED-DUP-EVENTS                                00012000
012100     END-IF                                                       00012100
012200     .                                                            00012200
012300                                                                  00012300
012400                                                                  00012400
012500 210-WEED-BY-HARD-CODE.                                           00012500
012600     IF  (ZZZC0197-TRX-ID = 'BVLM'                                00012600
012700*      OR ZZZC0197-TRX-ID = 'APLM'                                00012700
012800*      OR ZZZC0197-TRX-ID = 'BDMM'                                00012800
012900*      OR ZZZC0197-TRX-ID = 'BCAM'                                00012900
013000*      OR ZZZC0197-TRX-ID = 'CCSM'                                00013000
013100*      OR ZZZC0197-TRX-ID = 'CEMM'                                00013100
013200       OR ZZZC0197-TRX-ID = 'CNCM'                                00013200
013300*      OR ZZZC0197-TRX-ID = 'COMM'                                00013300
013400*      OR ZZZC0197-TRX-ID = 'CRCM'                                00013400
013500*      OR ZZZC0197-TRX-ID = 'CSCM'                                00013500
013600*      OR ZZZC0197-TRX-ID = 'CTOM'                                00013600
013700*      OR ZZZC0197-TRX-ID = 'DIRM'                                00013700
013800*      OR ZZZC0197-TRX-ID = 'DISM'                                00013800
013900*      OR ZZZC0197-TRX-ID = 'DSDM'                                00013900
014000*      OR ZZZC0197-TRX-ID = 'FINM'                                00014000
014100*      OR ZZZC0197-TRX-ID = 'ICCM'                                00014100
014200*      OR ZZZC0197-TRX-ID = 'ITMM'                                00014200
014300       OR ZZZC0197-TRX-ID = 'IWVM'                                00014300
014400*      OR ZZZC0197-TRX-ID = 'LOBM'                                00014400
014500*      OR ZZZC0197-TRX-ID = 'MCEM'                                00014500
014600*      OR ZZZC0197-TRX-ID = 'MRGM'                                00014600
014700       OR ZZZC0197-TRX-ID = 'OBSM'                                00014700
014800*      OR ZZZC0197-TRX-ID = 'ORBM'                                00014800
014900       OR ZZZC0197-TRX-ID = 'PBCM'                                00014900
015000*      OR ZZZC0197-TRX-ID = 'PBNM'                                00015000
015100       OR ZZZC0197-TRX-ID = 'PBTM'                                00015100
015200*      OR ZZZC0197-TRX-ID = 'PCCM'                                00015200
015300*      OR ZZZC0197-TRX-ID = 'PCTM'                                00015300
015400*      OR ZZZC0197-TRX-ID = 'PDSH'                                00015400
015500*      OR ZZZC0197-TRX-ID = 'PDUA'                                00015500
015600*      OR ZZZC0197-TRX-ID = 'PDUP'                                00015600
015700       OR ZZZC0197-TRX-ID = 'PIPM'                                00015700
015800*      OR ZZZC0197-TRX-ID = 'PRIM'                                00015800
015900*      OR ZZZC0197-TRX-ID = 'PRMM'                                00015900
016000*      OR ZZZC0197-TRX-ID = 'PRRM'                                00016000
016100       OR ZZZC0197-TRX-ID = 'PSBM'                                00016100
016200*      OR ZZZC0197-TRX-ID = 'PSCM'                                00016200
016300       OR ZZZC0197-TRX-ID = 'RARM'                                00016300
016400       OR ZZZC0197-TRX-ID = 'RFTM'                                00016400
016500       OR ZZZC0197-TRX-ID = 'RITM'                                00016500
016600       OR ZZZC0197-TRX-ID = 'RRFM'                                00016600
016700       OR ZZZC0197-TRX-ID = 'RTDM'                                00016700
016800*      OR ZZZC0197-TRX-ID = 'RTTM'                                00016800
016900       OR ZZZC0197-TRX-ID = 'SCAM'                                00016900
017000*      OR ZZZC0197-TRX-ID = 'SDPM'                                00017000
017100*      OR ZZZC0197-TRX-ID = 'SLDM'                                00017100
017200       OR ZZZC0197-TRX-ID = 'STAM'                                00017200
017300*      OR ZZZC0197-TRX-ID = 'STPM'                                00017300
017400*      OR ZZZC0197-TRX-ID = 'STRM'                                00017400
017500       OR ZZZC0197-TRX-ID = 'STTM'                                00017500
017600*      OR ZZZC0197-TRX-ID = 'T2TM'                                00017600
017700       OR ZZZC0197-TRX-ID = 'TRDM'                                00017700
017800*      OR ZZZC0197-TRX-ID = 'VCMM'                                00017800
017900*      OR ZZZC0197-TRX-ID = 'VENM'                                00017900
018000*      OR ZZZC0197-TRX-ID = 'VISM'                                00018000
018100*      OR ZZZC0197-TRX-ID = 'VLIM'                                00018100
018200*      OR ZZZC0197-TRX-ID = 'WHSM'                                00018200
018300       OR ZZZC0197-TRX-ID = 'WLIM')                               00018300
018400       AND YYYN110A-BATCH-ENV                                     00018400
018500         SET WEED-EVENT TO TRUE                                   00018500
018600     END-IF                                                       00018600
018700     .                                                            00018700
018800                                                                  00018800
018900                                                                  00018900
019000 220-WEED-DUP-EVENTS.                                             00019000
019100     IF  WS-PREV-COMPARE-DATA NOT = SPACES                        00019100
019200     AND WS-PREV-EVENT-TRX    NOT = SPACES                        00019200
019300     AND WS-PREV-COMPARE-DATA = ZZZC0197-COMPARE-DATA             00019300
019400     AND WS-PREV-EVENT-TRX    = ZZZC0197-TRX-ID                   00019400
019500       SET WEED-EVENT TO TRUE                                     00019500
019600     END-IF                                                       00019600
019700                                                                  00019700
019800     MOVE ZZZC0197-COMPARE-DATA TO WS-PREV-COMPARE-DATA           00019800
019900     MOVE ZZZC0197-TRX-ID       TO WS-PREV-EVENT-TRX              00019900
020000                                                                  00020000
020100     IF PROCESS-EVENT                                             00020100
020200       PERFORM VARYING I FROM 1 BY 1                              00020200
020300           UNTIL I > WS-NUM-EVENTS                                00020300
020400           OR    WEED-EVENT                                       00020400
020500         IF WS-EVENT-TRX (I) = ZZZC0197-TRX-ID                    00020500
020600           IF WS-COMPARE-DATA (I) = ZZZC0197-COMPARE-DATA         00020600
020700             SET WEED-EVENT TO TRUE                               00020700
020800           END-IF                                                 00020800
020900         END-IF                                                   00020900
021000       END-PERFORM                                                00021000
021100                                                                  00021100
021200       IF  PROCESS-EVENT                                          00021200
021300       AND WS-NUM-EVENTS < WS-MAX-EVENTS                          00021300
021400         ADD  1                     TO WS-NUM-EVENTS              00021400
021500         MOVE WS-NUM-EVENTS         TO I                          00021500
021600         MOVE ZZZC0197-TRX-ID       TO WS-EVENT-TRX (I)           00021600
021700         MOVE ZZZC0197-COMPARE-DATA TO WS-COMPARE-DATA (I)        00021700
021800       END-IF                                                     00021800
021900     END-IF                                                       00021900
022000     .                                                            00022000
022100                                                                  00022100
022200                                                                  00022200
022300*=================================================================00022300
022400* Issue an event...                                               00022400
022500*=================================================================00022500
022600 300-ISSUE-EVENT.                                                 00022600
022700     INITIALIZE YYYC0175                                          00022700
022800                                                                  00022800
022900     IF YYYN110A-BATCH-ENV                                        00022900
023000       SET YYYC0175-BATCH-ENV TO TRUE                             00023000
023100     ELSE                                                         00023100
023200       SET YYYC0175-CICS-ENV  TO TRUE                             00023200
023300     END-IF                                                       00023300
023301                                                                  00023301
023310     IF YYYN110A-ORACLE                                           00023310
023320       SET YYYC0175-ORACLE    TO TRUE                             00023320
023330     ELSE                                                         00023330
023340       SET YYYC0175-DB2       TO TRUE                             00023340
023350     END-IF                                                       00023350
023400                                                                  00023400
023500     MOVE ZZZC0197-TRX-ID  TO YYYC0175-TRX-CD                     00023500
023600     MOVE ZZZC0197-TRX-REC TO YYYC0175-DATA                       00023600
023700     EVALUATE TRUE                                                00023700
023800       WHEN YYYN110A-ADD                                          00023800
023900         MOVE 'A' TO YYYC0175-ACTION-CD                           00023900
024000       WHEN YYYN110A-DEL                                          00024000
024100         MOVE 'P' TO YYYC0175-ACTION-CD                           00024100
024200       WHEN OTHER                                                 00024200
024300         MOVE 'M' TO YYYC0175-ACTION-CD                           00024300
024400     END-EVALUATE                                                 00024400
024500                                                                  00024500
024600     MOVE ZZZC0197-PROGRAM TO YYYC0175-CALLING-PROG               00024600
024700     MOVE ZZZC0197-USER    TO YYYC0175-CALLING-USER               00024700
024800                                                                  00024800
024900     SET  YYYC0175-SOURCE-WMS       TO TRUE                       00024900
025000     SET  YYYC0175-TARGET-MAINFRAME TO TRUE                       00025000
025100                                                                  00025100
025200     CALL WS-EVENT-STAGER USING                                   00025200
025300         XXXN001A                                                 00025300
025400         YYYC0175                                                 00025400
025500     .                                                            00025500
