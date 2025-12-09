       01 DDDTDP01-FIELD-CONSTANTS.
           05 DDDTDP01-NUM-FLDS              PIC S9(4) COMP VALUE 012.
           05 DDDTDP01-KEY-FLDS              PIC S9(4) COMP VALUE 002.
           05 DDDTDP01-SPECIAL-FUNC          PIC X(4) VALUE SPACES.
               88 DDDTDP01-NO-SPECIAL-FUNC   VALUE SPACES.

       01 DDDTDP01-INDEX-INFO.
           05 DDDTDP01-TABLE-HANDLE          PIC S9(9) COMP VALUE 00573.
           05 DDDTDP01-INDEX-HANDLE          PIC S9(4) COMP VALUE 01.
               88 DDDXDP01                   VALUE 01.
               88 DDDXDP02                   VALUE 02.
               88 DDDXDP03-NO-IDX            VALUE 999.
           05 DDDTDP01-MAX-INDICIES          PIC S9(4) COMP VALUE 002.
           05 DDDTDP01-MAX-INDEX-FLDS        PIC S9(4) COMP VALUE 003.
           05 DDDTDP01-INDEX-DATA.
               10 DDDXDP01-IDX-FLDS          PIC X(009)
                    VALUE '001002000'.
               10 DDDXDP02-IDX-FLDS          PIC X(009)
                    VALUE '003001002'.
           05 REDEFINES DDDTDP01-INDEX-DATA.
               10 INDEX-NUMBER OCCURS 002 TIMES.
                   15 INDEX-FIELDS OCCURS 003 TIMES.
                       20 INDEX-FIELD        PIC 9(3).

       01 DDDTDP01-FIELD-HANDLES.
           05 STR-DEPT-NBR-H                 PIC S9(4) COMP VALUE 001.
           05 STR-SUB-DEPT-ID-H              PIC S9(4) COMP VALUE 002.
           05 DEPT-NM-H                      PIC S9(4) COMP VALUE 003.
           05 DEPT-ABB-H                     PIC S9(4) COMP VALUE 004.
           05 REPT-GRP-CD-H                  PIC S9(4) COMP VALUE 005.
           05 GRPRFT-LO-PCT-H                PIC S9(4) COMP VALUE 006.
           05 GRPRFT-HI-PCT-H                PIC S9(4) COMP VALUE 007.
           05 SHRNK-LO-PCT-H                 PIC S9(4) COMP VALUE 008.
           05 SHRNK-HI-PCT-H                 PIC S9(4) COMP VALUE 009.
           05 LST-UPDT-USR-ID-H              PIC S9(4) COMP VALUE 010.
           05 LST-UPDT-TS-H                  PIC S9(4) COMP VALUE 011.
           05 ORG-ID-H                       PIC S9(4) COMP VALUE 012.
