000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID.    NNNU0473.                                         00020000
000300 AUTHOR.        NAME.                                             00030000
000400 DATE-WRITTEN.  NOV, 1600.                                        00040000
000500 DATE-COMPILED.                                                   00050000
000600* ----------------------------------------------------------------00060000
000700* Description : Z-DAO Table IO object.                            00070000
000800*                                                                 00080000
000900* Source      : Generated from DCL Gen on 02/03/1999.             00090000
001000* Gen Version : Z-DAO 1.0b                                        00100000
001100* Template    : COBOL IO Subr - DB2.cob, Version 1.01b            00110000
001200*                                                                 00120000
001300* Table DBMS  : ORACLE                                            00130000
001400* Table Name  : XXXX_LOC_CLS_AD_ZN , DDDTCZ01                     00140000
001500* Table Handle: 00473                                             00150000
001600*                                                                 00160000
001700* Special Func: None at time of generation.                       00170000
001800* ----------------------------------------------------------------00180000
002300 ENVIRONMENT DIVISION.                                            00230000
002400 CONFIGURATION SECTION.                                           00240000
002500 DATA DIVISION.                                                   00250000
002600 WORKING-STORAGE SECTION.                                         00260000
002700                                                                  00270000
002800 LINKAGE SECTION.                                                 00280000
002900 COPY XXXN001A.                                                   00290000
003000     EXEC SQL                                                     00300000
003100         INCLUDE SQLCA                                            00310000
003200     END-EXEC.                                                    00320000
003300 COPY YYYN005A.                                                   00330000
003400 COPY NNNN0000.                                                   00340000
003500 COPY DDDTCZ01.                                                   00350000
003600                                                                  00360000
003700 PROCEDURE DIVISION USING                                         00370000
003800     XXXN001A                                                     00380000
003900     SQLCA                                                        00390000
004000     YYYN005A                                                     00400000
004100     NNNN0000-PARMS                                               00410000
004200     DDDTCZ01                                                     00420000
004300     .                                                            00430000
004400                                                                  00440000
004500************************************************************      00450000
004600* MAIN PROGRAM LINE.                                              00460000
004700************************************************************      00470000
004800 0000-EXIT-DISPATCHER.                                            00480000
004900      EVALUATE TRUE                                               00490000
005000       WHEN EXIT-PUT-MODIFY-ROW                                   00500000
005100          PERFORM 1400-EXIT-PUT-MODIFY-ROW                        00510000
005200       WHEN EXIT-PUT-INSERT-ROW                                   00520000
005300          PERFORM 1500-EXIT-PUT-INSERT-ROW                        00530000
005400       WHEN EXIT-PUT-PURGE-ROW                                    00540000
005500          PERFORM 1600-EXIT-PUT-PURGE-ROW                         00550000
005600      END-EVALUATE                                                00560000
005700     GOBACK                                                       00570000
005800     .                                                            00580000
005900                                                                  00590000
006000 1400-EXIT-PUT-MODIFY-ROW.                                        00600000
006100      PERFORM 1405-DO-UPDATE                                      00610000
006200      .                                                           00620000
006300                                                                  00630000
006400                                                                  00640000
006500 1405-DO-UPDATE.                                                  00650000
006600     EXEC SQL                                                     00660000
046000         UPDATE XXXX_LOC_CLS_AD_ZN                                00046000
046100         SET    LOC_TYP_CD         =                              00046100
046200               :DCLXXXX-LOC-CLS-AD-ZN.LOC-TYP-CD,                 00046200
046300                LOC_NBR            =                              00046300
046400                      :DCLXXXX-LOC-CLS-AD-ZN.LOC-NBR,             00046400
046500                ITM_CLS_CD         =                              00046500
046600                      :DCLXXXX-LOC-CLS-AD-ZN.ITM-CLS-CD,          00046600
046700                AD_ZONE            =                              00046700
046800                      :DCLXXXX-LOC-CLS-AD-ZN.AD-ZONE,             00046800
046900                AD_ZONE_EXCP       =                              00046900
047000                      :DCLXXXX-LOC-CLS-AD-ZN.AD-ZONE-EXCP         00047000
047100         WHERE  LOC_TYP_CD = :DCLXXXX-LOC-CLS-AD-ZN.LOC-TYP-CD    00047100
047200         AND    LOC_NBR = :DCLXXXX-LOC-CLS-AD-ZN.LOC-NBR          00047200
047300         AND    ITM_CLS_CD = :DCLXXXX-LOC-CLS-AD-ZN.ITM-CLS-CD    00047300
006700     END-EXEC                                                     00670000
006800     .                                                            00680000
006900                                                                  00690000
007000                                                                  00700000
007100 1500-EXIT-PUT-INSERT-ROW.                                        00710000
007200      PERFORM 1505-DO-INSERT                                      00720000
007300     .                                                            00730000
007400                                                                  00740000
007500                                                                  00750000
007600 1505-DO-INSERT.                                                  00760000
007700     EXEC SQL                                                     00770000
048900         INSERT INTO XXXX_LOC_CLS_AD_ZN (                         00048900
049000             LOC_TYP_CD,                                          00049000
049100             LOC_NBR,                                             00049100
049200             ITM_CLS_CD,                                          00049200
049300             AD_ZONE,                                             00049300
049400             AD_ZONE_EXCP )                                       00049400
049500         VALUES (                                                 00049500
049600             :DCLXXXX-LOC-CLS-AD-ZN.LOC-TYP-CD,                   00049600
049700             :DCLXXXX-LOC-CLS-AD-ZN.LOC-NBR,                      00049700
049800             :DCLXXXX-LOC-CLS-AD-ZN.ITM-CLS-CD,                   00049800
049900             :DCLXXXX-LOC-CLS-AD-ZN.AD-ZONE,                      00049900
050000             :DCLXXXX-LOC-CLS-AD-ZN.AD-ZONE-EXCP )                00050000
007800     END-EXEC                                                     00780000
007900     .                                                            00790000
008000                                                                  00800000
008100                                                                  00810000
008200 1600-EXIT-PUT-PURGE-ROW.                                         00820000
008300       EXEC SQL                                                   00830000
051400         DELETE FROM XXXX_LOC_CLS_AD_ZN                           00051400
051500         WHERE  LOC_TYP_CD = :DCLXXXX-LOC-CLS-AD-ZN.LOC-TYP-CD    00051500
051600         AND    LOC_NBR = :DCLXXXX-LOC-CLS-AD-ZN.LOC-NBR          00051600
051700         AND    ITM_CLS_CD = :DCLXXXX-LOC-CLS-AD-ZN.ITM-CLS-CD    00051700
008400       END-EXEC                                                   00840000
008500     .                                                            00850000
008600                                                                  00860000
008700                                                                  00870000
