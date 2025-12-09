000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    YYYS0134.                                         00000200
000300 AUTHOR.        NAME.                                             00000300
000400 DATE-WRITTEN.  1600-Jan.                                         00000400
000500* ----------------------------------------------------------------00000500
000600* Description: It works as space cruncher.Given a text, this      00000600
000700* module parses for more than 1 space in between characters and   00000700
000710* reshuffle the charcters to have maximum 1 space between them.   00000710
000720* This program can be used in cases like concatenation of         00000720
000730* First-Middle-Last name.                                         00000730
000740* Calling program just concats the string and calls this module.  00000740
000800* ----------------------------------------------------------------00000800
001700                                                                  00001700
001800 ENVIRONMENT DIVISION.                                            00001800
001900 CONFIGURATION SECTION.                                           00001900
002000 DATA DIVISION.                                                   00002000
002100                                                                  00002100
002200 WORKING-STORAGE SECTION.                                         00002200
004200 01 WS-MAX-STR-LEN                    PIC S9(4) COMP VALUE 80.    00004200
004210 01 I                                 PIC S9(2) COMP VALUE 0.     00004210
004211 01 J                                 PIC S9(2) COMP VALUE 0.     00004211
004212 01 WS-OT-STR.                                                    00004212
004230    05 WS-STR-BYTES                   OCCURS 80 TIMES.            00004230
004240         10 WS-STR-B                  PIC X VALUE SPACE.          00004240
004300                                                                  00004300
004400 01 WS-BKUP-BYTE                      PIC X(1) VALUE SPACES.      00004400
006900 COPY YYYN000A.                                                   00006900
007000                                                                  00007000
007200 LINKAGE SECTION.                                                 00007200
007300 COPY XXXN001A.                                                   00007300
007500 01 STR                               PIC X(80).                  00007500
007600 01 REDEFINES STR.                                                00007600
007700     05 STR-BYTES                     OCCURS 80 TIMES.            00007700
007800         10 STR-B                     PIC X.                      00007800
007900 01 STR-LEN                           PIC S9(2) COMP.             00007900
008000                                                                  00008000
008100 PROCEDURE DIVISION USING                                         00008100
008200     XXXN001A                                                     00008200
008400     STR                                                          00008400
008500     STR-LEN                                                      00008500
008600     .                                                            00008600
008610                                                                  00008610
008700                                                                  00008700
008800******************************************************************00008800
008900* MAIN PROGRAM LINE.                                              00008900
009000******************************************************************00009000
009100 000-MAIN.                                                        00009100
009200     PERFORM 100-INITIALIZATION                                   00009200
009300     IF SUCCESS                                                   00009300
009400       PERFORM 200-CRUNCH-STRING                                  00009400
009500     END-IF                                                       00009500
009600     IF SUCCESS                                                   00009600
009700       PERFORM 300-SEND-FINAL-STRING                              00009700
009800     END-IF                                                       00009800
010300     GOBACK                                                       00010300
010400     .                                                            00010400
010500                                                                  00010500
010600                                                                  00010600
010700* ================================================================00010700
010800* THE INITIALIZATION SECTION.                                     00010800
010900* ================================================================00010900
011000 100-INITIALIZATION.                                              00011000
011100     INITIALIZE XXXN001A                                          00011100
011101                WS-OT-STR                                         00011101
011102                WS-BKUP-BYTE                                      00011102
011103                J                                                 00011103
011104                                                                  00011104
011105     IF STR-LEN          > WS-MAX-STR-LEN                         00011105
011106       MOVE WS-MAX-STR-LEN            TO STR-LEN                  00011106
011107     END-IF                                                       00011107
011108                                                                  00011108
011110     IF STR EQUAL SPACES OR LOW-VALUES                            00011110
011111         SET FAILURE                  TO TRUE                     00011111
011120         MOVE 'YYYS0134 - Text not passed for parsing.'           00011120
011130           TO IS-RTRN-MSG-TXT                                     00011130
011160     END-IF                                                       00011160
011800     .                                                            00011800
011900                                                                  00011900
012000                                                                  00012000
012100* ================================================================00012100
012200* Substitutes  more than 1 Consecutive Spaces btw charcters with  00012200
012300* a single Space and re aligns the text                           00012300
012400* ================================================================00012400
012500 200-CRUNCH-STRING.                                               00012500
012600                                                                  00012600
013500     PERFORM VARYING I FROM +1 BY +1 UNTIL I > STR-LEN            00013500
013501                                                                  00013501
013502       IF STR-B(I) EQUAL SPACE AND WS-BKUP-BYTE EQUAL SPACE       00013502
013503         CONTINUE                                                 00013503
013504       ELSE                                                       00013504
013505         ADD +1                       TO J                        00013505
013506                                                                  00013506
013507         MOVE STR-B(I)                TO WS-STR-B(J)              00013507
013508       END-IF                                                     00013508
013509                                                                  00013509
013510       MOVE STR-B(I)                  TO WS-BKUP-BYTE             00013510
013520                                                                  00013520
013600     END-PERFORM                                                  00013600
036500     .                                                            00036500
036510                                                                  00036510
036520                                                                  00036520
036600 300-SEND-FINAL-STRING.                                           00036600
036700     MOVE SPACES                      TO STR(1:STR-LEN)           00036700
036800     MOVE WS-OT-STR                   TO STR(1:STR-LEN)           00036800
037000     .                                                            00037000
037100                                                                  00037100
037200                                                                  00037200