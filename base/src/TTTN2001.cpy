       01  DB2-MESSAGE-AREA.
           05  DB2-OPTION-BYTE             PIC 9(1)  VALUE 1.
               88  DB2-PRINT-DUMP-ABEND              VALUE 1.
               88  DB2-PRINT-DUMP-RETURN             VALUE 4.
               88  DB2-PRINT-RETURN                  VALUE 2.
               88  DB2-RETURN                        VALUE 3.
           05  DB2-ERROR-IDENTIFIER.
               10 DB2-ERROR-PROGNAME       PIC X(8)  VALUE SPACES.
               10 FILLER                   PIC X     VALUE SPACES.
               10 DB2-ERROR-PARAGRAPH      PIC X(57) VALUE SPACES.
           05  DB2-LINE-LENGTH             PIC S9(9) COMP VALUE +79.
           05  DB2-MESSAGE-LENGTH          PIC S9(4) COMP VALUE +1210.
           05  DB2-MESSAGE-TEXT            PIC X(1185) VALUE SPACES.
           05  DB2-MESSAGE-LINES REDEFINES DB2-MESSAGE-TEXT.
               10  DB2-MESSAGE-LINE
                   OCCURS 15 TIMES             PIC X(79).
           05  FILLER                      PIC X(25) VALUE SPACES.

       01  DB279-MESSAGE-AREA REDEFINES DB2-MESSAGE-AREA.
           05  DB279-OPTION-BYTE           PIC 9(1).
               88  DB279-PRINT-AND-DUMP              VALUE 1.
               88  DB279-PRINT-AND-RETURN            VALUE 2.
               88  DB279-DONT-PRINT-AND-RETURN       VALUE 3.
           05  DB279-ERROR-IDENTIFIER      PIC X(66).
           05  DB279-LINE-LENGTH           PIC S9(9).
           05  DB279-MESSAGE-LENGTH        PIC S9(4).
           05  DB279-MESSAGE-LINE
               OCCURS 15 TIMES             PIC X(79).
           05  FILLER                      PIC X(25).

       01  DB2121-MESSAGE-AREA REDEFINES DB279-MESSAGE-AREA.
           05  DB2121-OPTION-BYTE          PIC 9(1).
               88  DB2121-PRINT-AND-DUMP              VALUE 1.
               88  DB2121-PRINT-AND-RETURN            VALUE 2.
               88  DB2121-DONT-PRINT-AND-RETURN       VALUE 3.
           05  DB2121-ERROR-IDENTIFIER     PIC X(66).
           05  DB2121-LINE-LENGTH          PIC S9(9) COMP.
           05  DB2121-MESSAGE-LENGTH       PIC S9(4) COMP.
           05  DB2121-MESSAGE-LINE
               OCCURS 10 TIMES             PIC X(121).