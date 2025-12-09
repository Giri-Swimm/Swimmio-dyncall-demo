000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    MMMS0158.                                         00000200
000300 AUTHOR.        NAME                                              00000300
000400 DATE-WRITTEN.  Circa 1600.                                       00000400
000500*---------------------------------------------------------------- 00000500
002000* --------------------------------------------------------------- 00002000
002100 ENVIRONMENT DIVISION.                                            00002100
002200 DATA DIVISION.                                                   00002200
002300 WORKING-STORAGE SECTION.                                         00002300
002400* --------------------------------------------------------------- 00002400
002500* Misc working storage...                                         00002500
002600* --------------------------------------------------------------- 00002600
002700 01 WS-SQLCODE                          PIC ----9.                00002700
002800                                                                  00002800
002900 01 WS-SUBRS.                                                     00002900
003000     05 MMMS9012-DATE-CONV              PIC X(8) VALUE 'MMMS9012'.00003000
003100                                                                  00003100
003200 01 FILLER.                                                       00003200
003300     05 WS-ZIP5-CD                      PIC X(5).                 00003300
003400     05 REDEFINES WS-ZIP5-CD.                                     00003400
003500         10 WS-ZIP5-CD-NUMERIC          PIC 9(5).                 00003500
003600     05 WS-ZIP4-CD                      PIC X(4).                 00003600
003700     05 REDEFINES WS-ZIP4-CD.                                     00003700
003800         10 WS-ZIP4-CD-NUMERIC          PIC 9(4).                 00003800
003900                                                                  00003900
004000 01 WS-CONTACT-LEN                      PIC S9(2) COMP VALUE 0.   00004000
004100                                                                  00004100
004200 01 WS-NAME-STUFF.                                                00004200
004300     05 WS-FIRST-NAME                   PIC X(15) VALUE SPACE.    00004300
004400     05 WS-MIDLE-NAME                   PIC X(15) VALUE SPACE.    00004400
004500     05 WS-LAST-NAME                    PIC X(15) VALUE SPACE.    00004500
004600     05 WS-JR-SR-NAME                   PIC X(15) VALUE SPACE.    00004600
004700                                                                  00004700
004800* --------------------------------------------------------------- 00004800
004900* Miscellaneous copy books go here...                             00004900
005000* --------------------------------------------------------------- 00005000
005100  COPY MMMC9012.                                                  00005100
005200  COPY MMMK001B.                                                  00005200
005300                                                                  00005300
005400* ----------------------------------------------------------------00005400
005500* DB2 stuff...                                                    00005500
005600* ----------------------------------------------------------------00005600
005700     EXEC SQL                                                     00005700
005800       INCLUDE SQLCA                                              00005800
005900     END-EXEC                                                     00005900
006000                                                                  00006000
006100 LINKAGE SECTION.                                                 00006100
006200 COPY XXXN001A.                                                   00006200
006300 COPY YYYN111A.                                                   00006300
006400 COPY PPPTLO01.                                                   00006400
006500 COPY XXXPST01.                                                   00006500
006600 COPY PPPTRL01.                                                   00006600
006700                                                                  00006700
006800 PROCEDURE DIVISION USING                                         00006800
006900     XXXN001A                                                     00006900
007000     YYYN111A                                                     00007000
007100     P-DDDTLO01                                                   00007100
007200     XXXPST01                                                     00007200
007300     P-DDDTRL01                                                   00007300
007400     .                                                            00007400
007500                                                                  00007500
007600***************************************************************** 00007600
007700* Start of program main line.                                     00007700
007800***************************************************************** 00007800
007900 000-MAIN.                                                        00007900
008000     PERFORM 100-INITIALIZE                                       00008000
008100                                                                  00008100
008200     EVALUATE TRUE                                                00008200
008300       WHEN YYYN111A-NEW-2-OLD                                    00008300
008400         PERFORM 200-NEW-2-OLD                                    00008400
008500                                                                  00008500
008600       WHEN YYYN111A-OLD-2-NEW                                    00008600
008700         PERFORM 500-OLD-2-NEW                                    00008700
008800                                                                  00008800
008900       WHEN OTHER                                                 00008900
009000         SET FAILURE TO TRUE                                      00009000
009100         MOVE 'MMMS0158 - Invalid translation function.'          00009100
009200           TO IS-RTRN-MSG-TXT                                     00009200
009300     END-EVALUATE                                                 00009300
009400                                                                  00009400
009500     GOBACK                                                       00009500
009600     .                                                            00009600
009700                                                                  00009700
009800                                                                  00009800
009900*================================================================ 00009900
010000* Initialization...                                               00010000
010100*================================================================ 00010100
010200 100-INITIALIZE.                                                  00010200
010300     INITIALIZE XXXN001A                                          00010300
010400     .                                                            00010400
010500                                                                  00010500
010600                                                                  00010600
010700*================================================================ 00010700
010800* Transalate from the new to the old...                           00010800
010900*================================================================ 00010900
011000 200-NEW-2-OLD.                                                   00011000
011100     PERFORM 210-POPULATE-XXXPST01                                00011100
011200     IF SUCCESS                                                   00011200
011300        PERFORM 250-POPULATE-DDDTRL01                             00011300
011400     END-IF                                                       00011400
011500     .                                                            00011500
011600                                                                  00011600
011700                                                                  00011700
011800*================================================================ 00011800
011900* Prepare XXXPST01                                                00011900
012000*================================================================ 00012000
012100 210-POPULATE-XXXPST01.                                           00012100
012200     MOVE LOC-TYP-CD                   OF P-DDDTLO01              00012200
012300       TO ST-STORE-TYPE                                           00012300
012400     MOVE LOC-NBR                      OF P-DDDTLO01              00012400
012500       TO ST-STORE-NUMBER                                         00012500
012600                                                                  00012600
012700     MOVE LOC-NM                       OF P-DDDTLO01              00012700
012800       TO ST-STORE-NAME                                           00012800
012900     MOVE LOC-ABB                      OF P-DDDTLO01              00012900
013000       TO ST-STORE-NAME-ABBREV                                    00013000
013100     MOVE PRIM-ADR-1                   OF P-DDDTLO01              00013100
013200       TO ST-STORE-LOCATION                                       00013200
013300     MOVE PRIM-ADR-2                   OF P-DDDTLO01              00013300
013400       TO ST-STORE-LOCATION-2                                     00013400
013500     MOVE PRIM-CITY                    OF P-DDDTLO01              00013500
013600       TO ST-CITY                                                 00013600
013700     MOVE PRIM-CITY-ID                 OF P-DDDTLO01              00013700
013800       TO ST-CITY-IDENTIFIER                                      00013800
013900     MOVE PRIM-STATE-CD                OF P-DDDTLO01              00013900
014000       TO ST-STATE                                                00014000
014100     MOVE PRIM-AREA-CD                 OF P-DDDTLO01              00014100
014200       TO ST-AREA-CODE                                            00014200
014300     MOVE PRIM-PHONE-NBR               OF P-DDDTLO01              00014300
014400       TO ST-PHONE-NUMBER                                         00014400
014500                                                                  00014500
014600     PERFORM 220-UNPK-NAME                                        00014600
014700                                                                  00014700
014800     PERFORM 230-NEW-2-OLD-DATE-CONV                              00014800
014900                                                                  00014900
015000     PERFORM 240-NEW-2-OLD-ZIPC-CONV                              00015000
015100     .                                                            00015100
015200                                                                  00015200
015300                                                                  00015300
015400*================================================================ 00015400
015500*  Unpack Name to First, Middle and Last name                     00015500
015600*=================================================================00015600
015700 220-UNPK-NAME.                                                   00015700
015800     INITIALIZE WS-NAME-STUFF                                     00015800
015900                                                                  00015900
016000     UNSTRING PRIM-CONTACT-NM DELIMITED BY ALL SPACES             00016000
016100         INTO WS-FIRST-NAME,                                      00016100
016200              WS-MIDLE-NAME,                                      00016200
016300              WS-LAST-NAME,                                       00016300
016400              WS-JR-SR-NAME                                       00016400
016500     END-UNSTRING                                                 00016500
016600                                                                  00016600
016700     MOVE WS-FIRST-NAME               TO ST-FIRST-NAME            00016700
016800                                                                  00016800
016900     IF WS-LAST-NAME = SPACES                                     00016900
017000        MOVE WS-MIDLE-NAME            TO ST-LAST-NAME             00017000
017100        MOVE SPACES                   TO ST-INIT                  00017100
017200     ELSE                                                         00017200
017300       MOVE WS-MIDLE-NAME             TO ST-INIT                  00017300
017400       IF WS-JR-SR-NAME = SPACES                                  00017400
017500         MOVE WS-LAST-NAME            TO ST-LAST-NAME             00017500
017600       ELSE                                                       00017600
017700         STRING WS-LAST-NAME  DELIMITED BY SPACE                  00017700
017800                ' '           DELIMITED BY SIZE                   00017800
017900                WS-JR-SR-NAME DELIMITED BY SPACE                  00017900
018000         INTO   ST-LAST-NAME                                      00018000
018100       END-IF                                                     00018100
018200     END-IF                                                       00018200
018300     .                                                            00018300
018400                                                                  00018400
018500                                                                  00018500
018600*================================================================ 00018600
018700*  DATE conversion from Db2 format to old @YYMMDD format          00018700
018800*=================================================================00018800
018900 230-NEW-2-OLD-DATE-CONV.                                         00018900
019000     SET MMMC9012-CONV-FROM-DB2        TO TRUE                    00019000
019100     SET MMMC9012-PIC-P7-YYMMDD        TO TRUE                    00019100
019200                                                                  00019200
019300     IF SUCCESS                                                   00019300
019400       PERFORM 900-DATE-CONVERSION-PARA                           00019400
019500       IF NOT SUCCESS                                             00019500
019600         STRING IS-RTRN-MSG-TXT, '-N2O' DELIMITED BY '.'          00019600
019700            INTO IS-RTRN-MSG-TXT                                  00019700
019800         END-STRING                                               00019800
019900       END-IF                                                     00019900
020000     END-IF                                                       00020000
020100     .                                                            00020100
020200                                                                  00020200
020300                                                                  00020300
020400*================================================================ 00020400
020500*  ZIP conversion from DB2 format to old FORMAT                   00020500
020600*================================================================ 00020600
020700 240-NEW-2-OLD-ZIPC-CONV.                                         00020700
020800     IF PRIM-ZIP5-CD OF P-DDDTLO01 = 0                            00020800
020900       MOVE '00000'                    TO ST-ZIP-FIRST-5          00020900
021000     ELSE                                                         00021000
021100       MOVE PRIM-ZIP5-CD OF P-DDDTLO01 TO ST-ZIP-FIRST-5          00021100
021200     END-IF                                                       00021200
021300                                                                  00021300
021400     IF PRIM-ZIP4-CD OF P-DDDTLO01 = 0                            00021400
021500       MOVE '0000'                     TO ST-ZIP-LAST-4           00021500
021600     ELSE                                                         00021600
021700       MOVE PRIM-ZIP4-CD OF P-DDDTLO01 TO ST-ZIP-LAST-4           00021700
021800     END-IF                                                       00021800
021900     .                                                            00021900
022000                                                                  00022000
022100                                                                  00022100
022200*================================================================ 00022200
022300* Prepare DDDTRL01                                                00022300
022400*================================================================ 00022400
022500 250-POPULATE-DDDTRL01.                                           00022500
022600     MOVE LOC-TYP-CD                   OF P-DDDTLO01              00022600
022700       TO FC-RL-STORE-CD                                          00022700
022800     MOVE LOC-NBR                      OF P-DDDTLO01              00022800
022900       TO FC-STORE-NO                                             00022900
023000     MOVE LOC-NM                       OF P-DDDTLO01              00023000
023100       TO FC-RL-STORE-NM                                          00023100
023200     MOVE LOC-ABB                      OF P-DDDTLO01              00023200
023300       TO FC-RL-STORE-ABB                                         00023300
023400     MOVE PRIM-CONTACT-NM              OF P-DDDTLO01              00023400
023500       TO FC-RL-STORE-DIR-NM                                      00023500
023600     MOVE PRIM-ADR-1                   OF P-DDDTLO01              00023600
023700       TO FC-RL-STORE-LOC-NM                                      00023700
023800     MOVE PRIM-ADR-2                   OF P-DDDTLO01              00023800
023900       TO FC-RL-STOR2-LOC-NM                                      00023900
024000     MOVE PRIM-CITY                    OF P-DDDTLO01              00024000
024100       TO FC-CITY-ADR                                             00024100
024200     MOVE PRIM-CITY-ID                 OF P-DDDTLO01              00024200
024300       TO FC-RL-CITY-ID-CD                                        00024300
024400     MOVE PRIM-STATE-CD                OF P-DDDTLO01              00024400
024500       TO FC-STATE-ADR                                            00024500
024600     MOVE PRIM-AREA-CD                 OF P-DDDTLO01              00024600
024700       TO FC-RL-AREA-CODE-NO                                      00024700
024800     MOVE PRIM-PHONE-NBR               OF P-DDDTLO01              00024800
024900       TO FC-RL-TELEPHONE-NO                                      00024900
025000     MOVE DELETE-DT                    OF P-DDDTLO01              00025000
025100       TO FC-RL-DELETED-DT                                        00025100
025200     MOVE OPENED-DT                    OF P-DDDTLO01              00025200
025300       TO FC-RL-OPENING-DT                                        00025300
025400     MOVE CLOSED-DT                    OF P-DDDTLO01              00025400
025500       TO FC-RL-CLOSING-DT                                        00025500
025600                                                                  00025600
025700     IF FC-RL-DELETED-DT = SPACES                                 00025700
025800     OR FC-RL-DELETED-DT = K-ZERO-DT                              00025800
025900       MOVE K-DEF-DT                   TO FC-RL-DELETED-DT        00025900
026000     END-IF                                                       00026000
026100     IF FC-RL-OPENING-DT = SPACES                                 00026100
026200     OR FC-RL-OPENING-DT = K-ZERO-DT                              00026200
026300       MOVE K-DEF-DT                   TO FC-RL-OPENING-DT        00026300
026400     END-IF                                                       00026400
026500     IF FC-RL-CLOSING-DT = SPACES                                 00026500
026600     OR FC-RL-CLOSING-DT = K-ZERO-DT                              00026600
026700       MOVE K-DEF-DT                   TO FC-RL-CLOSING-DT        00026700
026800     END-IF                                                       00026800
026900                                                                  00026900
027000     IF PRIM-ZIP5-CD OF P-DDDTLO01 = 0                            00027000
027100       MOVE '00000'                    TO FC-ZIP-CODE5-ADR        00027100
027200     ELSE                                                         00027200
027300       MOVE PRIM-ZIP5-CD OF P-DDDTLO01 TO FC-ZIP-CODE5-ADR        00027300
027400     END-IF                                                       00027400
027500     IF PRIM-ZIP4-CD OF P-DDDTLO01 = 0                            00027500
027600       MOVE '0000'                     TO FC-ZIP-CODE4-ADR        00027600
027700     ELSE                                                         00027700
027800       MOVE PRIM-ZIP4-CD OF P-DDDTLO01 TO FC-ZIP-CODE4-ADR        00027800
027900     END-IF                                                       00027900
028000     .                                                            00028000
028100                                                                  00028100
028200                                                                  00028200
028300*================================================================ 00028300
028400* Transalate from the old to the new...                           00028400
028500*================================================================ 00028500
028600 500-OLD-2-NEW.                                                   00028600
028700     MOVE ST-STORE-NUMBER                                         00028700
028800       TO LOC-NBR                      OF P-DDDTLO01              00028800
028900     MOVE ST-STORE-TYPE                                           00028900
029000       TO LOC-TYP-CD                   OF P-DDDTLO01              00029000
029100     MOVE ST-STORE-NAME                                           00029100
029200       TO LOC-NM                       OF P-DDDTLO01              00029200
029300     MOVE ST-STORE-NAME                                           00029300
029400       TO LGL-LOC-NAM                  OF P-DDDTLO01              00029400
029500     MOVE ST-STORE-LOCATION                                       00029500
029600       TO PRIM-ADR-1                   OF P-DDDTLO01              00029600
029700     MOVE ST-CITY-IDENTIFIER                                      00029700
029800       TO PRIM-CITY-ID                 OF P-DDDTLO01              00029800
029900     MOVE ST-AREA-CODE                                            00029900
030000       TO PRIM-AREA-CD                 OF P-DDDTLO01              00030000
030100     MOVE ST-PHONE-NUMBER                                         00030100
030200       TO PRIM-PHONE-NBR               OF P-DDDTLO01              00030200
030300     MOVE ST-STORE-NAME-ABBREV                                    00030300
030400       TO LOC-ABB                      OF P-DDDTLO01              00030400
030500     MOVE ST-STORE-LOCATION-2                                     00030500
030600       TO PRIM-ADR-2                   OF P-DDDTLO01              00030600
030700     MOVE ST-CITY                                                 00030700
030800       TO PRIM-CITY                    OF P-DDDTLO01              00030800
030900     MOVE ST-STATE                                                00030900
031000       TO PRIM-STATE-CD                OF P-DDDTLO01              00031000
031100                                                                  00031100
031200     IF ST-INACTIVE                                               00031200
031300     OR ST-NO-DEPTS                                               00031300
031400       SET LO-INACTIVE TO TRUE                                    00031400
031500     ELSE                                                         00031500
031600       SET LO-ACTIVE   TO TRUE                                    00031600
031700     END-IF                                                       00031700
031800                                                                  00031800
031900     PERFORM 510-PACK-NAME                                        00031900
032000                                                                  00032000
032100     PERFORM 520-OLD-2-NEW-DATE-CONV                              00032100
032200                                                                  00032200
032300     PERFORM 530-OLD-2-NEW-ZIPC-CONV                              00032300
032400     .                                                            00032400
032500                                                                  00032500
032600                                                                  00032600
032700*================================================================ 00032700
032800*  Remove white spaces in the text                                00032800
032900*================================================================ 00032900
033000 510-PACK-NAME.                                                   00033000
033100     MOVE SPACES        TO WS-NAME-STUFF                          00033100
033200     MOVE ST-FIRST-NAME TO WS-FIRST-NAME                          00033200
033300     MOVE ST-INIT       TO WS-MIDLE-NAME                          00033300
033400     MOVE ST-LAST-NAME  TO WS-LAST-NAME                           00033400
033500                                                                  00033500
033600     MOVE LENGTH OF WS-NAME-STUFF TO WS-CONTACT-LEN               00033600
033700     CALL  YYYS0134-STRING-CRUNCH USING                           00033700
033800                            XXXN001A                              00033800
033900                            WS-NAME-STUFF                         00033900
034000                            WS-CONTACT-LEN                        00034000
034100                                                                  00034100
034200     MOVE WS-NAME-STUFF TO PRIM-CONTACT-NM OF P-DDDTLO01          00034200
034300                                                                  00034300
034400     IF NOT SUCCESS                                               00034400
034500       INITIALIZE XXXN001A                                        00034500
034600     END-IF                                                       00034600
034700     .                                                            00034700
034800                                                                  00034800
034900                                                                  00034900
035000*================================================================ 00035000
035100*  DATE Conversion from old @YYMMDD format to DB2 format          00035100
035200*================================================================ 00035200
035300 520-OLD-2-NEW-DATE-CONV.                                         00035300
035400     SET MMMC9012-CONV-TO-DB2          TO TRUE                    00035400
035500     SET MMMC9012-PIC-P7-YYMMDD        TO TRUE                    00035500
035600                                                                  00035600
035700     IF SUCCESS                                                   00035700
035800       PERFORM 900-DATE-CONVERSION-PARA                           00035800
035900       IF NOT SUCCESS                                             00035900
036000         STRING IS-RTRN-MSG-TXT, '-O2N' DELIMITED BY '.'          00036000
036100            INTO IS-RTRN-MSG-TXT                                  00036100
036200         END-STRING                                               00036200
036300       END-IF                                                     00036300
036400     END-IF                                                       00036400
036500     .                                                            00036500
036600                                                                  00036600
036700                                                                  00036700
036800*================================================================ 00036800
036900*  ZIP conversion from DB2 format to old format                   00036900
037000*================================================================ 00037000
037100 530-OLD-2-NEW-ZIPC-CONV.                                         00037100
037200     IF ST-ZIP-FIRST-5 IS NOT EQUAL (SPACES OR LOW-VALUES)        00037200
037300        MOVE ST-ZIP-FIRST-5     TO WS-ZIP5-CD                     00037300
037400        MOVE WS-ZIP5-CD-NUMERIC TO PRIM-ZIP5-CD OF P-DDDTLO01     00037400
037500     ELSE                                                         00037500
037600        MOVE 0                  TO PRIM-ZIP5-CD OF P-DDDTLO01     00037600
037700     END-IF                                                       00037700
037800                                                                  00037800
037900     IF ST-ZIP-LAST-4 IS NOT EQUAL (SPACES OR LOW-VALUES)         00037900
038000        MOVE ST-ZIP-LAST-4      TO WS-ZIP4-CD                     00038000
038100        MOVE WS-ZIP4-CD-NUMERIC TO PRIM-ZIP4-CD OF P-DDDTLO01     00038100
038200     ELSE                                                         00038200
038300        MOVE 0                  TO PRIM-ZIP4-CD OF P-DDDTLO01     00038300
038400     END-IF                                                       00038400
038500     .                                                            00038500
038600                                                                  00038600
038700                                                                  00038700
038800*================================================================ 00038800
038900*  Common lines for data conversion.call routine                  00038900
039000*================================================================ 00039000
039100 900-DATE-CONVERSION-PARA.                                        00039100
039200     IF SUCCESS                                                   00039200
039300       CALL MMMS9012-DATE-CONV USING XXXN001A                     00039300
039400                                     MMMC9012                     00039400
039500                                     ST-DELETE-DATE               00039500
039600                                     DELETE-DT OF P-DDDTLO01      00039600
039700                                                                  00039700
039800       IF NOT SUCCESS                                             00039800
039900         MOVE 'MMMS0158 - Invalid delete date conversion.'        00039900
040000           TO IS-RTRN-MSG-TXT                                     00040000
040100       END-IF                                                     00040100
040200     END-IF                                                       00040200
040300                                                                  00040300
040400     IF SUCCESS                                                   00040400
040500       CALL MMMS9012-DATE-CONV USING XXXN001A                     00040500
040600                                     MMMC9012                     00040600
040700                                     ST-OPENING-DATE              00040700
040800                                     OPENED-DT OF P-DDDTLO01      00040800
040900                                                                  00040900
041000       IF NOT SUCCESS                                             00041000
041100         MOVE 'MMMS0158 - Invalid open date conversion.'          00041100
041200           TO IS-RTRN-MSG-TXT                                     00041200
041300       END-IF                                                     00041300
041400     END-IF                                                       00041400
041500                                                                  00041500
041600     IF SUCCESS                                                   00041600
041700       CALL MMMS9012-DATE-CONV USING XXXN001A                     00041700
041800                                     MMMC9012                     00041800
041900                                     ST-CLOSING-DATE              00041900
042000                                     CLOSED-DT OF P-DDDTLO01      00042000
042100                                                                  00042100
042200       IF NOT SUCCESS                                             00042200
042300         MOVE 'MMMS0158 - Invalid close date conversion.'         00042300
042400           TO IS-RTRN-MSG-TXT                                     00042400
042500       END-IF                                                     00042500
042600     END-IF                                                       00042600
042700     .                                                            00042700