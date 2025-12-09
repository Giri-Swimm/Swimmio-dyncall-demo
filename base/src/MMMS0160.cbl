000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    MMMS0160.                                         00000200
000300 AUTHOR.        NAME                                              00000300
000400 DATE-WRITTEN.  Circa 1600.                                       00000400
000500*---------------------------------------------------------------- 00000500
000600* Translate the "old" database structure to the "new" database.   00000600
000700*                                                                 00000700
000800* Note that at some time in the future, after I retire, this      00000800
000900* will no longer be necessary.                                    00000900
001000*                                                                 00001000
001100* Databases translated...                                         00001100
001200*    OLD - XXXPST01,DDDTRL01                                      00001200
001300*    NEW - XXXTLR01                                               00001300
001400* --------------------------------------------------------------- 00001400
002400 ENVIRONMENT DIVISION.                                            00002400
002500 DATA DIVISION.                                                   00002500
002600 WORKING-STORAGE SECTION.                                         00002600
002700* --------------------------------------------------------------- 00002700
002800* Misc working storage...                                         00002800
002900* --------------------------------------------------------------- 00002900
003000 01 WS-SQLCODE                          PIC ----9.                00003000
003100                                                                  00003100
003200 01 WS-SUBRS.                                                     00003200
003300     05 MMMC9012-DATE-CONV              PIC X(8) VALUE 'MMMS9012'.00003300
003400                                                                  00003400
003500 01 WS-X2                               PIC X(2) VALUE SPACES.    00003500
003600 01 REDEFINES WS-X2.                                              00003600
003700     05 WS-N2                           PIC 9(2).                 00003700
003800 01 REDEFINES WS-X2.                                              00003800
003900     05 WS-B1                           PIC X(1).                 00003900
004000     05 WS-B2                           PIC X(1).                 00004000
004100                                                                  00004100
004200 01 FILLER.                                                       00004200
004300     05 WS-CNT                          PIC S9(2) COMP.           00004300
004400     05 WS-K                            PIC S9(2) COMP VALUE 10.  00004400
004500     05 WS-REPT-TBL-TXT                 PIC X(20).                00004500
004600     05 WS-REPT-TBL-NUMERIC REDEFINES  WS-REPT-TBL-TXT            00004600
004700                                        PIC S9(3) COMP-3 OCCURS   00004700
004800                                        10 TIMES.                 00004800
004900     05 WS-DUMMY-DATE                   PIC S9(7) COMP-3          00004900
005000                                                  VALUE +0.       00005000
005100     05 WS-CURR-AD-ZN-NBR               PIC 9(3) VALUE 0.         00005100
005200                                                                  00005200
005300* --------------------------------------------------------------- 00005300
005400* Miscellaneous copy books go here...                             00005400
005500* --------------------------------------------------------------- 00005500
005600  COPY MMMC9012.                                                  00005600
005700  COPY MMMK001B.                                                  00005700
005800                                                                  00005800
005900* ----------------------------------------------------------------00005900
006000* DB2 stuff...                                                    00006000
006100* ----------------------------------------------------------------00006100
006200     EXEC SQL                                                     00006200
006300       INCLUDE SQLCA                                              00006300
006400     END-EXEC                                                     00006400
006500                                                                  00006500
006600 LINKAGE SECTION.                                                 00006600
006700 COPY XXXN001A.                                                   00006700
006800 COPY YYYN111A.                                                   00006800
006900 COPY XXXTLR01.                                                   00006900
007000 COPY XXXPST01.                                                   00007000
007100 COPY PPPTRL01.                                                   00007100
007200                                                                  00007200
007300 PROCEDURE DIVISION USING                                         00007300
007400     XXXN001A                                                     00007400
007500     YYYN111A                                                     00007500
007600     P-DDDTLR01                                                   00007600
007700     DDDPST01                                                     00007700
007800     P-DDDTRL01                                                   00007800
007900     .                                                            00007900
008000                                                                  00008000
008100***************************************************************** 00008100
008200* Start of program main line.                                     00008200
008300***************************************************************** 00008300
008400 000-MAIN.                                                        00008400
008500     PERFORM 100-INITIALIZE                                       00008500
008600                                                                  00008600
008700     EVALUATE TRUE                                                00008700
008800       WHEN YYYN111A-NEW-2-OLD                                    00008800
008900         PERFORM 200-NEW-2-OLD                                    00008900
009000                                                                  00009000
009100       WHEN YYYN111A-OLD-2-NEW                                    00009100
009200         PERFORM 500-OLD-2-NEW                                    00009200
009300                                                                  00009300
009400       WHEN OTHER                                                 00009400
009500         SET FAILURE TO TRUE                                      00009500
009600         MOVE 'XXXS0136 - Invalid translation function.'          00009600
009700           TO IS-RTRN-MSG-TXT                                     00009700
009800     END-EVALUATE                                                 00009800
009900                                                                  00009900
010000     GOBACK                                                       00010000
010100     .                                                            00010100
010200                                                                  00010200
010300                                                                  00010300
010400*================================================================ 00010400
010500* Initialization...                                               00010500
010600*================================================================ 00010600
010700 100-INITIALIZE.                                                  00010700
010800     INITIALIZE XXXN001A                                          00010800
010900     .                                                            00010900
011000                                                                  00011000
011100                                                                  00011100
011200*================================================================ 00011200
011300* Transalate from the new to the old...                           00011300
011400*================================================================ 00011400
011500 200-NEW-2-OLD.                                                   00011500
011600     PERFORM 210-POPULATE-XXXPST01                                00011600
011700     IF SUCCESS                                                   00011700
011800        PERFORM 240-POPULATE-DDDTRL01                             00011800
011900     END-IF                                                       00011900
012000     .                                                            00012000
012100                                                                  00012100
012200                                                                  00012200
012300*================================================================ 00012300
012400* Prepare XXXPST01                                                00012400
012500*================================================================ 00012500
012600 210-POPULATE-XXXPST01.                                           00012600
012700     MOVE LOC-TYP-CD                 OF P-XXXTLR01                00012700
012800       TO ST-STORE-TYPE                                           00012800
012900     MOVE LOC-NBR                    OF P-XXXTLR01                00012900
013000       TO ST-STORE-NUMBER                                         00013000
013100     MOVE ASSOC-STR-NBR              OF P-XXXTLR01                00013100
013200       TO ST-ASSOCIATED-STORE                                     00013200
013300     MOVE COMPANY-ID                 OF P-XXXTLR01                00013300
013400       TO ST-COMPANY-NUMBER                                       00013400
013500     MOVE FINANCIAL-DIV-ID           OF P-XXXTLR01                00013500
013600       TO FN-DIVISION-CD             OF XXXPST01                  00013600
013700     MOVE LIN-OF-BUS-ID              OF P-XXXTLR01                00013700
013800       TO FN-LINE-OF-BUS-CD          OF XXXPST01                  00013800
013900     MOVE DIST-ID                    OF P-XXXTLR01                00013900
014000       TO ST-DISTRICT-CODE                                        00014000
014100     MOVE DIST-ID                    OF P-XXXTLR01                00014100
014200       TO ST-PAYROLL-AREA                                         00014200
014300     MOVE DIST-ID                    OF P-XXXTLR01                00014300
014400       TO ST-PAY-GROUP                                            00014400
014500     MOVE MKT-RGN-ID                 OF P-XXXTLR01                00014500
014600       TO ST-MARKET-AREA                                          00014600
014700     MOVE GEO-ZN-CD                  OF P-XXXTLR01                00014700
014800       TO ST-GEO-ZONE-ALPHA                                       00014800
014900     MOVE RETL-GEO-ZN-ID             OF P-XXXTLR01                00014900
015000       TO ST-GEO-ZONE-NUMERIC                                     00015000
015100     MOVE SCN-MAINT-SW               OF P-XXXTLR01                00015100
015200       TO ST-SCAN-ITEM-MAINTENANCE                                00015200
015300     MOVE FRNT-END-CD                OF P-XXXTLR01                00015300
015400       TO ST-SCAN-FRONT-END-TYPE                                  00015400
015500     MOVE PRC-BUL-SW                 OF P-XXXTLR01                00015500
015600       TO ST-PRICE-BULLETINS                                      00015600
015700     MOVE UPC-ON-PRC-BUL-SW          OF P-XXXTLR01                00015700
015800       TO ST-UPCS-ON-PRICE-BLTNS                                  00015800
015900     MOVE CMPTR-TYP-CD               OF P-XXXTLR01                00015900
016000       TO ST-COMPETITOR-CODE                                      00016000
016100     MOVE RETL-VID-ZN-NBR            OF P-XXXTLR01                00016100
016200       TO ST-VIDEO-PRICE-ZONE                                     00016200
016300     MOVE RETL-UNLD-CD               OF P-XXXTLR01                00016300
016400       TO ST-UNLOAD-FACILITY-FLAG                                 00016400
016500     MOVE NEW-STR-SW                 OF P-XXXTLR01                00016500
016600       TO ST-NEW-STORE-FLAG                                       00016600
016700     MOVE SEL-CIR-SW                 OF P-XXXTLR01                00016700
016800       TO ST-SELECT-CIRCLE-FLAG                                   00016800
016900     MOVE BKRM-SQ-FT                 OF P-XXXTLR01                00016900
017000       TO ST-BACKROOM-SQ-FT                                       00017000
017100     MOVE FD-LINER-FT                OF P-XXXTLR01                00017100
017200       TO ST-LINEAR-FT-FOOD                                       00017200
017300     MOVE NON-FD-LINER-FT            OF P-XXXTLR01                00017300
017400       TO ST-LINEAR-FT-NON-FOOD                                   00017400
017500     MOVE SETOFF-ROOM-SW             OF P-XXXTLR01                00017500
017600       TO ST-SET-OFF-ROOM-FLAG                                    00017600
017700     MOVE CAT-CLS-TBL-TXT            OF P-XXXTLR01                00017700
017800       TO ST-ITEM-CAT-CLASS-DATA                                  00017800
017900     MOVE CK-COLL-REPT-SW            OF P-XXXTLR01                00017900
018000       TO FC-CK-COLL-REPT-SW                                      00018000
018100     MOVE CK-COLL-CNTL-CD            OF P-XXXTLR01                00018100
018200       TO FC-CK-COLL-CNTR-CD                                      00018200
018300     MOVE CK-COLL-ADD-DEL-SW         OF P-XXXTLR01                00018300
018400       TO FC-CK-ADD-DEL-SW                                        00018400
018500     MOVE CK-ALT-STR-ID              OF P-XXXTLR01                00018500
018600       TO FC-CK-ALT-STR-NO                                        00018600
018700     MOVE CK-COLL-FEE-AMT            OF P-XXXTLR01                00018700
018800       TO FC-CK-COLL-FEE-AMT                                      00018800
018900     MOVE SALS-TAX-PCT               OF P-XXXTLR01                00018900
019000       TO SALE-TAX-PCT                                            00019000
019100     MOVE SOAP-SALE-VAR-PCT          OF P-XXXTLR01                00019100
019200       TO SOAP-SALE-VAR-PCT          OF XXXPST01                  00019200
019300     MOVE ON-SRS-CD                  OF P-XXXTLR01                00019300
019400       TO ST-SRS-SYS-CD                                           00019400
019500     MOVE SRS-DSD-ORD-SW             OF P-XXXTLR01                00019500
019600       TO SRS-DSD-ORDR-SW                                         00019600
019700     MOVE RETL-LOC-TYP-CD OF P-XXXTLR01 TO WS-X2                  00019700
019800     IF WS-B1 = SPACES                                            00019800
019900       MOVE '0' TO WS-B1                                          00019900
020000     END-IF                                                       00020000
020100     IF WS-B2 = SPACES                                            00020100
020200       MOVE '0' TO WS-B2                                          00020200
020300     END-IF                                                       00020300
020400                                                                  00020400
020500     MOVE WS-N2                                                   00020500
020600       TO ST-FACILITY-TYPE                                        00020600
020700     MOVE DEA-NBR                    OF P-XXXTLR01                00020700
020800       TO ST-DEA-NUMBER                                           00020800
020900     MOVE STR-OPSTMT-SRT-CD          OF P-XXXTLR01                00020900
021000       TO ST-SORT-SEQUENCE-SOS                                    00021000
021100     MOVE STR-OPSTMT-TYP-CD          OF P-XXXTLR01                00021100
021200       TO ST-SOS-STORE-TYPE                                       00021200
021300     MOVE STR-OPSTMT-HDR-CD          OF P-XXXTLR01                00021300
021400       TO ST-SOS-HDR-STORE-TYPE                                   00021400
021500     MOVE PD-ZONE-NO                 OF P-XXXTLR01                00021500
021600       TO ST-RETAIL-ZONE                                          00021600
021700     MOVE SOS-PROC-SW                OF P-XXXTLR01                00021700
021800       TO ST-SOS-HDR-DONT-PROCESS-FLAG                            00021800
021900     MOVE RPRT-SEQ-NBR               OF P-XXXTLR01                00021900
022000       TO ST-REPORTING-SEQUENCE-NO                                00022000
022100     MOVE GRP-CD                     OF P-XXXTLR01                00022100
022200       TO ST-FACILITY-GROUP                                       00022200
022300     MOVE SPACES                                                  00022300
022400       TO ST-ADZONE-ABBREV                                        00022400
022500     MOVE CURR-AD-ZN-NBR             OF P-XXXTLR01                00022500
022600       TO WS-CURR-AD-ZN-NBR                                       00022600
022700     MOVE RETL-LOC-STAT-CD             OF P-XXXTLR01              00022700
022800       TO ST-STATUS-FLAG                                          00022800
022900                                                                  00022900
023000     STRING 'ZN '             DELIMITED BY SIZE                   00023000
023100            WS-CURR-AD-ZN-NBR DELIMITED BY SIZE                   00023100
023200            INTO ST-ADZONE-DESC                                   00023200
023300     END-STRING                                                   00023300
023400                                                                  00023400
023500     PERFORM 220-NEW-2-OLD-DATE-CONV                              00023500
023600                                                                  00023600
023700     PERFORM 230-CONVERT-ROLLUP-TXT-OLD                           00023700
023800     .                                                            00023800
023900                                                                  00023900
024000                                                                  00024000
024100*===============================================================  00024100
024200*  DATE conversion from DB2 format to old @YYMMDD format          00024200
024300*===============================================================  00024300
024400 220-NEW-2-OLD-DATE-CONV.                                         00024400
024500     SET MMMC9012-CONV-FROM-DB2      TO TRUE                      00024500
024600     SET MMMC9012-PIC-P7-YYMMDD      TO TRUE                      00024600
024700                                                                  00024700
024800     PERFORM 900-PERFORM-DATE-CONV                                00024800
024900                                                                  00024900
025000     IF NOT SUCCESS                                               00025000
025100        STRING IS-RTRN-MSG-TXT, ' N2O' DELIMITED BY '-'           00025100
025200           INTO IS-RTRN-MSG-TXT                                   00025200
025300        END-STRING                                                00025300
025400     END-IF                                                       00025400
025500     .                                                            00025500
025600                                                                  00025600
025700                                                                  00025700
025800*===============================================================  00025800
025900*  Copy ROLUP-REPT-TBL-TXT to FN-ROLLUP-REPT-CD(10)               00025900
026000*===============================================================  00026000
026100 230-CONVERT-ROLLUP-TXT-OLD.                                      00026100
026200     SET FNRC TO +1                                               00026200
026300                                                                  00026300
026400     MOVE ROLUP-REPT-TBL-TXT OF P-XXXTLR01 TO WS-REPT-TBL-TXT     00026400
026500                                                                  00026500
026600     PERFORM VARYING WS-CNT FROM 1 BY 1 UNTIL WS-CNT > WS-K       00026600
026700       IF WS-REPT-TBL-NUMERIC(WS-CNT) IS NUMERIC                  00026700
026800         MOVE WS-REPT-TBL-NUMERIC (WS-CNT)                        00026800
026900           TO FN-ROLLUP-REPT-CD OF XXXPST01 (WS-CNT)              00026900
027000       ELSE                                                       00027000
027100         MOVE 0 TO FN-ROLLUP-REPT-CD OF XXXPST01 (WS-CNT)         00027100
027200       END-IF                                                     00027200
027300     END-PERFORM                                                  00027300
027400     .                                                            00027400
027500                                                                  00027500
027600                                                                  00027600
027700*================================================================ 00027700
027800* Prepare DDDTRL01                                                00027800
027900*================================================================ 00027900
028000 240-POPULATE-DDDTRL01.                                           00028000
028100     MOVE LOC-TYP-CD                 OF P-XXXTLR01                00028100
028200       TO FC-RL-STORE-CD                                          00028200
028300     MOVE LOC-NBR                    OF P-XXXTLR01                00028300
028400       TO FC-STORE-NO                                             00028400
028500     MOVE ASSOC-STR-NBR              OF P-XXXTLR01                00028500
028600       TO FC-RL-ASSOC-STR-NO                                      00028600
028700     MOVE COMPANY-ID                 OF P-XXXTLR01                00028700
028800       TO FC-RL-COMPANY-NO                                        00028800
028900     MOVE FINANCIAL-DIV-ID           OF P-XXXTLR01                00028900
029000       TO FN-DIVISION-CD             OF P-DDDTRL01                00029000
029100     MOVE LIN-OF-BUS-ID              OF P-XXXTLR01                00029100
029200       TO FN-LINE-OF-BUS-CD          OF P-DDDTRL01                00029200
029300     MOVE DIST-ID                    OF P-XXXTLR01                00029300
029400       TO FC-RL-DISTRICT-NO                                       00029400
029500     MOVE DIST-ID                    OF P-XXXTLR01                00029500
029600       TO FC-RL-PAYROL-AR-NO                                      00029600
029700     MOVE DIST-ID                    OF P-XXXTLR01                00029700
029800       TO FC-RL-PAY-GROUP-NO                                      00029800
029900     MOVE MKT-RGN-ID                 OF P-XXXTLR01                00029900
030000       TO FC-RL-MARKET-AR-NO                                      00030000
030100     MOVE GEO-ZN-CD                  OF P-XXXTLR01                00030100
030200       TO FC-RL-GEO-ZONE-CD                                       00030200
030300     MOVE RETL-GEO-ZN-ID             OF P-XXXTLR01                00030300
030400       TO FC-RL-GEO-ZONE-NO                                       00030400
030500     MOVE SCN-MAINT-SW               OF P-XXXTLR01                00030500
030600       TO FC-RL-SCAN-MAIN-CD                                      00030600
030700     MOVE FRNT-END-CD                OF P-XXXTLR01                00030700
030800       TO FC-RL-FRONT-END-CD                                      00030800
030900     MOVE PRC-BUL-SW                 OF P-XXXTLR01                00030900
031000       TO FC-RL-PRICE-BUL-CD                                      00031000
031100     MOVE UPC-ON-PRC-BUL-SW          OF P-XXXTLR01                00031100
031200       TO FC-RL-UPC-ON-PB-CD                                      00031200
031300     MOVE CMPTR-TYP-CD               OF P-XXXTLR01                00031300
031400       TO FC-RL-COMPETITR-CD                                      00031400
031500     MOVE RETL-VID-ZN-NBR            OF P-XXXTLR01                00031500
031600       TO FC-RL-VID-PRZN-NO                                       00031600
031700     MOVE RETL-UNLD-CD               OF P-XXXTLR01                00031700
031800       TO FC-RL-UNLOAD-SW                                         00031800
031900     MOVE NEW-STR-SW                 OF P-XXXTLR01                00031900
032000       TO FC-RL-NEW-STORE-CD                                      00032000
032100     MOVE SEL-CIR-SW                 OF P-XXXTLR01                00032100
032200       TO FC-RL-SELECTCIR-CD                                      00032200
032300     MOVE BKRM-SQ-FT                 OF P-XXXTLR01                00032300
032400       TO FC-RL-BCKRM-FT-QTY                                      00032400
032500     MOVE FD-LINER-FT                OF P-XXXTLR01                00032500
032600       TO FC-RL-LFT-FOOD-QTY                                      00032600
032700     MOVE NON-FD-LINER-FT            OF P-XXXTLR01                00032700
032800       TO FC-RL-LFT-NONF-QTY                                      00032800
032900     MOVE SETOFF-ROOM-SW             OF P-XXXTLR01                00032900
033000       TO FC-RL-SETOFF-CD                                         00033000
033100     MOVE CAT-CLS-TBL-TXT            OF P-XXXTLR01                00033100
033200       TO FC-RL-CAT-CLASS-TB                                      00033200
033300     MOVE LAT-K                      OF P-XXXTLR01                00033300
033400       TO FC-RL-LATITUDE-K                                        00033400
033500     MOVE LON-K                      OF P-XXXTLR01                00033500
033600       TO FC-RL-LONGITUDE-K                                       00033600
033700     MOVE RETL-LOC-TYP-CD            OF P-XXXTLR01                00033700
033800       TO FC-RL-TYPE-CD                                           00033800
033900     MOVE DEA-NBR                    OF P-XXXTLR01                00033900
034000       TO FC-RL-STORE-DEA-NO                                      00034000
034100     MOVE STR-OPSTMT-SRT-CD          OF P-XXXTLR01                00034100
034200       TO FC-RL-SORT-SOS-NO                                       00034200
034300     MOVE STR-OPSTMT-TYP-CD          OF P-XXXTLR01                00034300
034400       TO FC-RL-SOS-TYPE-CD                                       00034400
034500     MOVE STR-OPSTMT-HDR-CD          OF P-XXXTLR01                00034500
034600       TO FC-RL-SOSHDRTYP-CD                                      00034600
034700     MOVE PD-ZONE-NO                 OF P-XXXTLR01                00034700
034800       TO FC-RL-RETL-ZONE-NO                                      00034800
034900     MOVE SOS-PROC-SW                OF P-XXXTLR01                00034900
035000       TO FC-RL-NOPROCESS-CD                                      00035000
035100     MOVE RPRT-SEQ-NBR               OF P-XXXTLR01                00035100
035200       TO FC-RL-RPRT-SEQ-NO                                       00035200
035300     MOVE GRP-CD                     OF P-XXXTLR01                00035300
035400       TO FC-RL-GROUP-CD                                          00035400
035500     MOVE ROLUP-REPT-TBL-TXT         OF P-XXXTLR01                00035500
035600       TO FN-ROLLUP-REPT-CD          OF P-DDDTRL01                00035600
035700     MOVE STR-REMODL-DT              OF P-XXXTLR01                00035700
035800       TO FC-RL-REMODEL-DT           OF P-DDDTRL01                00035800
035900     IF FC-RL-REMODEL-DT             EQUAL SPACE OR K-ZERO-DT     00035900
036000       MOVE K-DEF-DT                 TO FC-RL-REMODEL-DT          00036000
036100     END-IF                                                       00036100
036200     MOVE RETL-LOC-STAT-CD           OF P-XXXTLR01                00036200
036300       TO FC-RL-STATUS-CD                                         00036300
036400                                                                  00036400
036500     MOVE SPACES                                                  00036500
036600       TO FC-RL-ADZONE-ABB                                        00036600
036700     MOVE CURR-AD-ZN-NBR             OF P-XXXTLR01                00036700
036800       TO WS-CURR-AD-ZN-NBR                                       00036800
036900                                                                  00036900
037000     STRING 'ZN '             DELIMITED BY SIZE                   00037000
037100            WS-CURR-AD-ZN-NBR DELIMITED BY SIZE                   00037100
037200            INTO FC-RL-ADZONE-DES                                 00037200
037300     END-STRING                                                   00037300
037400     .                                                            00037400
037500                                                                  00037500
037600                                                                  00037600
037700*================================================================ 00037700
037800* Transalate from the old to the new...                           00037800
037900*================================================================ 00037900
038000 500-OLD-2-NEW.                                                   00038000
038100     INITIALIZE P-XXXTLR01                                        00038100
038200                                                                  00038200
038300     MOVE ST-STORE-NUMBER                                         00038300
038400       TO LOC-NBR                    OF P-XXXTLR01                00038400
038500     MOVE ST-STORE-TYPE                                           00038500
038600       TO LOC-TYP-CD                 OF P-XXXTLR01                00038600
038700     MOVE ST-DISTRICT-CODE                                        00038700
038800       TO DIST-ID                    OF P-XXXTLR01                00038800
038900     MOVE ST-MARKET-AREA                                          00038900
039000       TO MKT-RGN-ID                 OF P-XXXTLR01                00039000
039100     MOVE ST-COMPANY-NUMBER                                       00039100
039200       TO COMPANY-ID                 OF P-XXXTLR01                00039200
039300     MOVE ST-GEO-ZONE-ALPHA                                       00039300
039400       TO GEO-ZN-CD                  OF P-XXXTLR01                00039400
039500     MOVE ST-GEO-ZONE-NUMERIC                                     00039500
039600       TO RETL-GEO-ZN-ID             OF P-XXXTLR01                00039600
039700     MOVE ST-SCAN-ITEM-MAINTENANCE                                00039700
039800       TO SCN-MAINT-SW               OF P-XXXTLR01                00039800
039900     MOVE ST-SCAN-FRONT-END-TYPE                                  00039900
040000       TO FRNT-END-CD                OF P-XXXTLR01                00040000
040100     MOVE ST-PRICE-BULLETINS                                      00040100
040200       TO PRC-BUL-SW                 OF P-XXXTLR01                00040200
040300     MOVE ST-UPCS-ON-PRICE-BLTNS                                  00040300
040400       TO UPC-ON-PRC-BUL-SW          OF P-XXXTLR01                00040400
040500     MOVE ST-COMPETITOR-CODE                                      00040500
040600       TO CMPTR-TYP-CD               OF P-XXXTLR01                00040600
040700     MOVE ST-ASSOCIATED-STORE                                     00040700
040800       TO ASSOC-STR-NBR              OF P-XXXTLR01                00040800
040900     MOVE ST-STORE-TYPE                                           00040900
041000       TO ASSOC-STR-TYP-CD           OF P-XXXTLR01                00041000
041100     MOVE ST-REPORTING-SEQUENCE-NO                                00041100
041200       TO RPRT-SEQ-NBR               OF P-XXXTLR01                00041200
041300     MOVE ST-SORT-SEQUENCE-SOS                                    00041300
041400       TO STR-OPSTMT-SRT-CD          OF P-XXXTLR01                00041400
041500     MOVE ST-VIDEO-PRICE-ZONE                                     00041500
041600       TO RETL-VID-ZN-NBR            OF P-XXXTLR01                00041600
041700     MOVE ST-UNLOAD-FACILITY-FLAG                                 00041700
041800       TO RETL-UNLD-CD               OF P-XXXTLR01                00041800
041900     MOVE ST-NEW-STORE-FLAG                                       00041900
042000       TO NEW-STR-SW                 OF P-XXXTLR01                00042000
042100     MOVE ST-FACILITY-TYPE                                        00042100
042200       TO WS-N2                                                   00042200
042300     IF WS-B1 = SPACES                                            00042300
042400       MOVE '0' TO WS-B1                                          00042400
042500     END-IF                                                       00042500
042600     IF WS-B2 = SPACES                                            00042600
042700       MOVE '0' TO WS-B2                                          00042700
042800     END-IF                                                       00042800
042900     MOVE WS-X2                                                   00042900
043000       TO RETL-LOC-TYP-CD            OF P-XXXTLR01                00043000
043100     MOVE ST-FACILITY-GROUP                                       00043100
043200       TO GRP-CD                     OF P-XXXTLR01                00043200
043300     MOVE ST-SELECT-CIRCLE-FLAG                                   00043300
043400       TO SEL-CIR-SW                 OF P-XXXTLR01                00043400
043500     MOVE ST-BACKROOM-SQ-FT                                       00043500
043600       TO BKRM-SQ-FT                 OF P-XXXTLR01                00043600
043700     MOVE ST-LINEAR-FT-FOOD                                       00043700
043800       TO FD-LINER-FT                OF P-XXXTLR01                00043800
043900     MOVE ST-LINEAR-FT-NON-FOOD                                   00043900
044000       TO NON-FD-LINER-FT            OF P-XXXTLR01                00044000
044100     MOVE ST-SET-OFF-ROOM-FLAG                                    00044100
044200       TO SETOFF-ROOM-SW             OF P-XXXTLR01                00044200
044300     MOVE ST-DEA-NUMBER                                           00044300
044400       TO DEA-NBR                    OF P-XXXTLR01                00044400
044500                                                                  00044500
044600     EVALUATE TRUE                                                00044600
044700       WHEN ST-STORE-TYPE NOT = 'S'                               00044700
044800         MOVE 99999 TO PD-ZONE-NO  OF P-XXXTLR01                  00044800
044900       WHEN ST-RETAIL-ZONE = 888                                  00044900
045000         MOVE 88888 TO PD-ZONE-NO  OF P-XXXTLR01                  00045000
045100       WHEN ST-RETAIL-ZONE = 999                                  00045100
045200         MOVE 99999 TO PD-ZONE-NO  OF P-XXXTLR01                  00045200
045300       WHEN NOT ST-ACTIVE OF XXXPST01                             00045300
045400         MOVE 99999 TO PD-ZONE-NO  OF P-XXXTLR01                  00045400
045500       WHEN ST-RETAIL-ZONE < 100                                  00045500
045600         MOVE 88888 TO PD-ZONE-NO  OF P-XXXTLR01                  00045600
045700       WHEN OTHER                                                 00045700
045800         MOVE ST-RETAIL-ZONE                                      00045800
045900           TO PD-ZONE-NO           OF P-XXXTLR01                  00045900
046000     END-EVALUATE                                                 00046000
046100                                                                  00046100
046200     MOVE ST-SOS-STORE-TYPE                                       00046200
046300       TO STR-OPSTMT-TYP-CD          OF P-XXXTLR01                00046300
046400     MOVE ST-SOS-HDR-DONT-PROCESS-FLAG                            00046400
046500       TO SOS-PROC-SW                OF P-XXXTLR01                00046500
046600     MOVE ST-SOS-HDR-STORE-TYPE                                   00046600
046700       TO STR-OPSTMT-HDR-CD          OF P-XXXTLR01                00046700
046800     MOVE ST-ITEM-CAT-CLASS-DATA                                  00046800
046900       TO CAT-CLS-TBL-TXT            OF P-XXXTLR01                00046900
047000     MOVE FC-CK-COLL-REPT-SW                                      00047000
047100       TO CK-COLL-REPT-SW            OF P-XXXTLR01                00047100
047200     MOVE FC-CK-ADD-DEL-SW                                        00047200
047300       TO CK-COLL-ADD-DEL-SW         OF P-XXXTLR01                00047300
047400     MOVE FC-CK-COLL-CNTR-CD                                      00047400
047500       TO CK-COLL-CNTL-CD            OF P-XXXTLR01                00047500
047600     MOVE FC-CK-ALT-STR-NO                                        00047600
047700       TO CK-ALT-STR-ID              OF P-XXXTLR01                00047700
047800     MOVE FC-CK-COLL-FEE-AMT                                      00047800
047900       TO CK-COLL-FEE-AMT            OF P-XXXTLR01                00047900
048000     MOVE FN-DIVISION-CD             OF XXXPST01                  00048000
048100       TO FINANCIAL-DIV-ID           OF P-XXXTLR01                00048100
048200     MOVE FN-LINE-OF-BUS-CD          OF XXXPST01                  00048200
048300       TO LIN-OF-BUS-ID              OF P-XXXTLR01                00048300
048400     MOVE SALE-TAX-PCT                                            00048400
048500       TO SALS-TAX-PCT               OF P-XXXTLR01                00048500
048600     MOVE SOAP-SALE-VAR-PCT          OF XXXPST01                  00048600
048700       TO SOAP-SALE-VAR-PCT          OF P-XXXTLR01                00048700
048800     MOVE ST-SRS-SYS-CD                                           00048800
048900       TO ON-SRS-CD                  OF P-XXXTLR01                00048900
049000     MOVE SRS-DSD-ORDR-SW                                         00049000
049100       TO SRS-DSD-ORD-SW             OF P-XXXTLR01                00049100
049200                                                                  00049200
049300                                                                  00049300
049400     PERFORM 510-OLD-2-NEW-DATE-CONV                              00049400
049500                                                                  00049500
049600     PERFORM 520-CONVERT-ROLLUP-TXT-NEW                           00049600
049700                                                                  00049700
049800     MOVE WS-REPT-TBL-TXT                                         00049800
049900       TO ROLUP-REPT-TBL-TXT         OF P-XXXTLR01                00049900
050000                                                                  00050000
050100     MOVE ST-STATUS-FLAG                                          00050100
050200       TO RETL-LOC-STAT-CD           OF P-XXXTLR01                00050200
050300     .                                                            00050300
050400                                                                  00050400
050500                                                                  00050500
050600*===============================================================  00050600
050700*  DATE conversion from old @YYMMDD format to new DB2 format      00050700
050800*===============================================================  00050800
050900 510-OLD-2-NEW-DATE-CONV.                                         00050900
051000     SET MMMC9012-CONV-TO-DB2        TO TRUE                      00051000
051100     SET MMMC9012-PIC-P7-YYMMDD      TO TRUE                      00051100
051200                                                                  00051200
051300     PERFORM 900-PERFORM-DATE-CONV                                00051300
051400                                                                  00051400
051500     IF NOT SUCCESS                                               00051500
051600        STRING IS-RTRN-MSG-TXT, ' O2N' DELIMITED BY '-'           00051600
051700           INTO IS-RTRN-MSG-TXT                                   00051700
051800        END-STRING                                                00051800
051900     END-IF                                                       00051900
052000     .                                                            00052000
052100                                                                  00052100
052200                                                                  00052200
052300*===============================================================  00052300
052400*  Copy FN-ROLLUP-REPT-CD(10)    TO        ROLUP-REPT-TBL-TXT     00052400
052500*===============================================================  00052500
052600 520-CONVERT-ROLLUP-TXT-NEW.                                      00052600
052700     PERFORM VARYING WS-CNT FROM 1 BY 1 UNTIL WS-CNT > WS-K       00052700
052800       MOVE FN-ROLLUP-REPT-CD OF XXXPST01 (WS-CNT)                00052800
052900         TO WS-REPT-TBL-NUMERIC (WS-CNT)                          00052900
053000     END-PERFORM                                                  00053000
053100     .                                                            00053100
053200                                                                  00053200
053300                                                                  00053300
053400*===============================================================  00053400
053500*  Common date conversion. Call rountine                          00053500
053600*===============================================================  00053600
053700 900-PERFORM-DATE-CONV.                                           00053700
053800     CALL  MMMC9012-DATE-CONV USING XXXN001A                      00053800
053900                                    MMMC9012                      00053900
054000                                    ST-REMODEL-DATE               00054000
054100                                    STR-REMODL-DT                 00054100
054200                                                                  00054200
054300     IF NOT SUCCESS                                               00054300
054400       MOVE 'MMMS0160 - Invalid remodel date!'                    00054400
054500         TO IS-RTRN-MSG-TXT                                       00054500
054600     END-IF                                                       00054600
054700                                                                  00054700
054800     IF SUCCESS                                                   00054800
054900       CALL MMMC9012-DATE-CONV USING XXXN001A                     00054900
055000                                     MMMC9012                     00055000
055100                                     WS-DUMMY-DATE                00055100
055200                                     RETL-LOC-STAT-DT             00055200
055300                                                                  00055300
055400       IF NOT SUCCESS                                             00055400
055500         MOVE 'MMMS0160 - Invalid location status date!'          00055500
055600          TO IS-RTRN-MSG-TXT                                      00055600
055700       END-IF                                                     00055700
055800     END-IF                                                       00055800
055900     .                                                            00055900
