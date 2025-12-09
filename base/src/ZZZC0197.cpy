000100 01 ZZZC0197.                                                     00000100
000200     05 ZZZC0197-TRX-ID              PIC X(4)    VALUE SPACES.    00000200
000300     05 ZZZC0197-TRX-REC             PIC X(4096) VALUE SPACES.    00000300
000400     05 REDEFINES ZZZC0197-TRX-REC.                               00000400
000500         10 ZZZC0197-TRX-CD          PIC X(4).                    00000500
000600         10 ZZZC0197-TRX-DATA        PIC X(4092).                 00000600
000700     05 REDEFINES ZZZC0197-TRX-REC.                               00000700
000800         10 ZZZC0197-COMPARE-DATA    PIC X(100).                  00000800
000900         10 FILLER                   PIC X(3996).                 00000900
001000     05 ZZZC0197-USER                PIC X(8)    VALUE SPACES.    00001000
001100     05 ZZZC0197-PROGRAM             PIC X(8)    VALUE SPACES.    00001100
001200     05 FILLER                       PIC X(100)  VALUE SPACES.    00001200
