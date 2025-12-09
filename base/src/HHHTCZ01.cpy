       01 DDDTCZ01-FIELD-CONSTANTS.
           05 DDDTCZ01-NUM-FLDS              PIC S9(4) COMP VALUE 005.
           05 DDDTCZ01-KEY-FLDS              PIC S9(4) COMP VALUE 003.
           05 DDDTCZ01-SPECIAL-FUNC          PIC X(4) VALUE SPACES.
               88 DDDTCZ01-NO-SPECIAL-FUNC   VALUE SPACES.

       01 DDDTCZ01-INDEX-INFO.
           05 DDDTCZ01-TABLE-HANDLE          PIC S9(9) COMP VALUE 00473.
           05 DDDTCZ01-INDEX-HANDLE          PIC S9(4) COMP VALUE 01.
               88 DDDXCZ01                   VALUE 01.
               88 DDDXCZ02                   VALUE 02.
               88 DDDXCZ03                   VALUE 03.
               88 DDDXCZ04                   VALUE 04.
               88 DDDXCZ05-NO-IDX            VALUE 999.
           05 DDDTCZ01-MAX-INDICIES          PIC S9(4) COMP VALUE 004.
           05 DDDTCZ01-MAX-INDEX-FLDS        PIC S9(4) COMP VALUE 004.
           05 DDDTCZ01-INDEX-DATA.
               10 DDDXCZ01-IDX-FLDS          PIC X(012)
                    VALUE '001002003000'.
               10 DDDXCZ02-IDX-FLDS          PIC X(012)
                    VALUE '003001002000'.
               10 DDDXCZ03-IDX-FLDS          PIC X(012)
                    VALUE '004003001002'.
               10 DDDXCZ04-IDX-FLDS          PIC X(012)
                    VALUE '003004001002'.
           05 REDEFINES DDDTCZ01-INDEX-DATA.
               10 INDEX-NUMBER OCCURS 004 TIMES.
                   15 INDEX-FIELDS OCCURS 004 TIMES.
                       20 INDEX-FIELD        PIC 9(3).

       01 DDDTCZ01-FIELD-HANDLES.
           05 LOC-TYP-CD-H                   PIC S9(4) COMP VALUE 001.
           05 LOC-NBR-H                      PIC S9(4) COMP VALUE 002.
           05 ITM-CLS-CD-H                   PIC S9(4) COMP VALUE 003.
           05 AD-ZONE-H                      PIC S9(4) COMP VALUE 004.
           05 AD-ZONE-EXCP-H                 PIC S9(4) COMP VALUE 005.
