//XXXW012D JOB P1004,'XXX',MSGCLASS=Q,COND=(0,NE),       			00010000
//         PRTY=15,CLASS=H,REGION=6M                                    00011002
//********************************************************************  00015000
//ST01OF04 EXEC SORT3MB                                                 00016000
//SORT3MBS.SORTIN DD DSN=XXXP.AUDIT.STRIP(0),DISP=SHR                   00017000
//SORT3MBS.SORTOUT DD DSN=&&XXXPSORT,DISP=(,PASS),                      00018000
//            UNIT=SYSDA,                                               00019000
//            DCB=(RECFM=FB,BLKSIZE=6150,LRECL=150),                    00020000
//            SPACE=(6150,(300,30),RLSE)                                00021000
//SORT3MBS.SYSIN DD *                                                   00022000
  SORT FIELDS=(6,3,A),FORMAT=BI                                         00023000
/*                                                                      00024000
//********************************************************************  00025000
//* ACCESSES STORE AND CONTROL DATABASES TO PROVIDE LOOKUP INFO         00026000
//********************************************************************  00027000
//ST02OF04 EXEC DB2BATCH,MBR=XXXP12DK                                   00028000
//G.SYSOUT DD SYSOUT=*                                                  00029000
//G.SYSUDUMP DD SYSOUT=H,HOLD=YES                                       00030000
//G.ERRORS DD SYSOUT=*                                                  00031000
//G.SYSIN  DD DSN=GL.CONTROL.CARDS(XXXC1001),DISP=SHR                   00032000
//G.A20F001A DD DSN=&&XXXPSORT,DISP=(OLD,DELETE)                        00033000
//G.A20F001B DD DSN=XXXP.DIST.AUDIT.STRIP,DISP=(,CATLG,DELETE),         00034000
//            UNIT=SYSDA,                                               00035000
//            DCB=(RECFM=FB,BLKSIZE=0,LRECL=276,DSORG=PS),              00036000
//            SPACE=(276,(35,3),RLSE),AVGREC=K                          00037000
//********************************************************************  00038000
//***  CREATES BACKUP TAPE OF DIST STRIP FILE                      ***  00039000
//********************************************************************  00040000
//ST03OF04 EXEC PGM=IEBGENER,COND=(0,LT)                                00041000
//SYSPRINT DD SYSOUT=*,FCB=T001                                         00042000
//SYSIN    DD DUMMY                                                     00043000
//SYSUT1   DD DSN=XXXP.DIST.AUDIT.STRIP,DISP=SHR                        00044000
//SYSUT2   DD DSN=XXXP.DAILY.DIST.BKUP(+1),DISP=(,CATLG,DELETE),        00045000
//            UNIT=TAPE,                                                00046000
//            DCB=(MODDSCB,RECFM=FB,BLKSIZE=0,LRECL=276,DSORG=PS)       00047000
