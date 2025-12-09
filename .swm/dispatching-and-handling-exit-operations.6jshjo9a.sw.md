---
title: Dispatching and Handling Exit Operations
---
This document describes how exit operation requests are dispatched and routed to the correct handler. The flow covers cursor management, row operations, and special I/O functions. For each request, transaction and location data are prepared and validated, and a database connection is established if needed before performing the requested operation.

```mermaid
flowchart TD
  node1["Dispatching exit operations and routing to handlers"]:::HeadingStyle
  click node1 goToHeading "Dispatching exit operations and routing to handlers"
  node1 --> node2["Preparing transaction state and location data"]:::HeadingStyle
  click node2 goToHeading "Preparing transaction state and location data"
  node2 --> node3{"Is database connection required?"}
  node3 -->|"Yes"| node4["Establishing Oracle database connection"]:::HeadingStyle
  click node4 goToHeading "Establishing Oracle database connection"
  node3 -->|"No"| node5["Managing SQL cursors for data access"]:::HeadingStyle
  click node5 goToHeading "Managing SQL cursors for data access"
  node4 --> node5
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Spec

## Detailed View of the Program's Functionality

# Detailed Explanation of the Flow in base/src/NNNS0488.cbl

---

## a. Main Dispatcher and Operation Routing

The program begins execution in a main dispatcher section. This dispatcher is responsible for:

- Calling an initialization routine to prepare the environment and data structures.
- Examining the requested operation (such as opening or closing a cursor, fetching or updating a row, etc.).
- Routing control to the appropriate handler for the requested operation.
- Performing any necessary cleanup or finalization before returning control to the caller.

The dispatcher uses a decision structure to determine which operation is being requested and then invokes the corresponding handler. If an error or failure is detected at any point, it ensures that the error is handled gracefully.

---

## b. Preparing Transaction State and Location Data

### Initialization

The initialization routine is responsible for:

- Resetting and initializing various working storage areas, status flags, and counters.
- Copying an index handle from input parameters to a local structure for later use.
- Resetting SQL-related flags and counters.
- If the operation is not a cursor close, invoking a routine to copy and validate location and transaction data from the input area to a working area.
- If the environment requires an Oracle database connection (either because the environment is Oracle or the operation is an insert, purge, or modify), invoking a routine to establish that connection.

### Copying and Validating Location Fields

The routine that copies location fields performs the following actions:

- Transfers all relevant location and transaction fields from the input area to the working area.
- Applies business rules to ensure data integrity:
  - Sets default values for missing or invalid fields (e.g., default dates, times, and indicator flags).
  - Ensures that numeric fields are valid, setting them to zero if not.
  - Sets compound flags based on combinations of other fields (such as store preferences).
  - Handles special cases for certain fields, such as setting a default store type if missing.
- Invokes a subroutine to handle rollup fields, ensuring that these are also validated and transferred.
- Invokes a subroutine to normalize time fields, converting them as needed for the target environment (e.g., Oracle vs. DB2).
- If running in a specific environment (such as CICS), updates certain sequence numbers for reporting.

### Rollup Data Handling

The rollup data handler:

- Copies a text field containing rollup data to a working area.
- For each of up to 10 numeric slots in the rollup data, checks if the value is numeric.
- If a value is not numeric, sets the corresponding field to zero in the destination record.
- Ensures that all rollup data is clean and ready for downstream processing.

### Time Field Normalization

The time normalization routine:

- Checks if the environment or operation requires Oracle-compatible time formats.
- If so, collects all open and close times for each day of the week, calls an external utility to convert these times to timestamps, and stores the results.
- If not, simply copies the time fields directly.
- Handles errors in time conversion by building an error message with the SQL code.

---

## c. Establishing Oracle Database Connection

The Oracle connection routine:

- Calls an external subroutine to establish a connection to the Oracle database, passing necessary context and control structures.
- If the connection fails, builds an error message including the SQL code and stores it for error handling.
- Delegates the actual connection logic to an external module, keeping the main program decoupled from low-level database details.

---

## d. Managing SQL Cursors for Data Access

### Opening Cursors

The cursor open handler:

- Receives a request to open a specific SQL cursor, identified by a hardcoded list of valid cursor IDs.
- If the requested cursor ID matches a known value, issues the SQL command to open that cursor.
- If the cursor ID is invalid, sets a failure flag and returns an error message indicating the problem.

### Closing Cursors

The cursor close handler:

- Receives a request to close a specific SQL cursor, again identified by a hardcoded list of valid cursor IDs.
- If the requested cursor ID matches a known value, issues the SQL command to close that cursor.
- If the cursor ID is invalid, sets a failure flag and returns an error message.

---

## e. Fetching and Updating Data Rows

Although not detailed in the initial flow document, the code includes routines for:

- Fetching unique or next rows from the database using the currently open cursor.
- Updating, inserting, or purging rows in the database, including validation and event handling.
- Ensuring that all data transferred between the input area and the working area is validated, normalized, and consistent with business rules.

---

## f. Finalization and Cleanup

Before returning control to the caller, the program:

- If the operation was successful and not a cursor close, copies data from the working area back to the output area.
- Updates checkpoint counters and status flags as needed.
- If the environment or operation requires, re-establishes a connection to DB2 (if previously switched to Oracle).
- Ensures that the final SQL code is stored for downstream error handling or reporting.

---

## g. Special and Utility Functions

The program includes additional routines for:

- Handling special I/O functions as requested.
- Managing null indicators and ensuring that columns set to NULL are handled correctly.
- Calling external subroutines for event management, date handling, and other utility functions.

---

# Summary

This program acts as a robust transaction and data handler for location records, supporting multiple database environments (Oracle and DB2), enforcing business rules, and providing clear error handling and routing for a variety of data operations. It is modular, with each major function (initialization, data validation, cursor management, database connection, etc.) encapsulated in its own routine, making it easier to maintain and extend.

# Rule Definition

| Paragraph Name            | Rule ID | Category          | Description                                                                                                                                                                          | Conditions                                                                                                             | Remarks                                                                                                                                                |
| ------------------------- | ------- | ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| 0000-EXIT-DISPATCHER      | RL-001  | Conditional Logic | The dispatcher must route the request to the correct handler based on which operation flag is set to true in the input structure. Only one operation flag should be set per request. | One and only one operation flag (e.g., EXIT-OPEN-CURSOR, EXIT-CLOSE-CURSOR, EXIT-GET-UNIQUE-ROW, etc.) is set to true. | Operation flags are boolean indicators in the input structure. Only one should be true per request.                                                    |
| 100-INITIALIZATION        | RL-002  | Data Assignment   | The dispatcher must initialize the transaction environment and prepare all required fields before routing to the handler.                                                            | Always performed at the start of processing.                                                                           | Fields such as SQLCODE, checkpoint increment, and working storage areas are initialized. Input/output structures are cleared or set to initial values. |
| 110-MOVE-PDA-FIELDS-2-DCL | RL-003  | Conditional Logic | If EXIT-CLOSE-CURSOR is not set, the dispatcher must copy and validate all PDA fields, applying default values and business rules for dates, times, numerics, indicators, and flags. | EXIT-CLOSE-CURSOR is not set.                                                                                          | \- Default date: '01/01/1600' (K-DEF-DT)                                                                                                               |

- Default time: '00.00.00' (K-DEF-TM)
- DB2 max time: '24.00.00' (K-DB2-MAX-TM)
- Oracle max time: '23.59.59' (K-ORA-MAX-TM)
- Numeric fields set to zero if not numeric.
- Indicator fields set to 0 (valid) or -1 (invalid/missing).
- Single-character flags default to 'Y', 'N', or 'X' as appropriate.
- ASSOC-STR-TYP-CD defaults to 'S'.
- Output structure mirrors input with updated/validated fields. | | 117-MOVE-ROLLUP-DATA | RL-004 | Computation | After PDA field validation, rollup normalization must copy the rollup text and validate all 10 numeric slots, setting any non-numeric slot to zero. | Always performed after PDA field validation. | - Rollup text is a 20-character string.
- Each numeric slot is a number (typically 3 digits, COMP-3).
- Non-numeric slots are set to zero. | | 112-MOVE-TIME-FIELDS, 132-MOVE-TIME-FIELDS | RL-005 | Computation | Time normalization must convert all open/close times for the week to 26-character timestamp strings if the environment is Oracle or if certain operation flags are set. Otherwise, times are copied as 8-character strings. If normalization fails, set error message and flag error. | Environment is Oracle or EXIT-PUT-INSERT-ROW or EXIT-PUT-MODIFY-ROW is set. | - Oracle: 26-character timestamp strings.
- DB2: 8-character time strings.
- Error message format: 'NNNS0488 - INVALID TIME.PLS VERIFY Sqlcode =<code>'
- IS-RTRN-MSG-TXT is truncated to its maximum length if needed. | | 115-CONNECT-TO-ORACLE | RL-006 | Conditional Logic | If the operation requires an Oracle connection, attempt to connect. If the connection fails, set error message in IS-RTRN-MSG-TXT with SQLCODE, truncated as needed. | YYYN005A-ORACLE is true, or EXIT-PUT-INSERT-ROW, EXIT-PUT-PURGE-ROW, or EXIT-PUT-MODIFY-ROW is set. | - Error message: 'NNNS0488 - Error connecting to Oracle. Sqlcode =<code>'
- IS-RTRN-MSG-TXT is truncated to its maximum length if needed. | | 1000-EXIT-OPEN-CURSOR, 1100-EXIT-CLOSE-CURSOR | RL-007 | Conditional Logic | For cursor operations, open/close the specified cursor if the cursor ID is valid. If not valid, set FAILURE and IS-RTRN-MSG-TXT to appropriate error message. | EXIT-OPEN-CURSOR or EXIT-CLOSE-CURSOR is set. | - Valid open cursor IDs: DDDXLR01-DDDXLR09 (except DDDXLR08 for close)
- Valid close cursor IDs: DDDXLR01-DDDXLR07, DDDXLR09
- Error messages: 'NNNS0488 - Invalid open cursor ID.' or 'NNNS0488 - Invalid close cursor ID.'
- IS-RTRN-MSG-TXT is truncated if needed. | | 120-EXIT-STUFF, throughout error handling | RL-008 | Data Assignment | The dispatcher must update SUCCESS or FAILURE flags and IS-RTRN-MSG-TXT in the output structure to reflect the result of the operation. If an error message exceeds the maximum length, it must be truncated. | After each operation or error. | - SUCCESS/FAILURE are boolean flags in the output structure.
- IS-RTRN-MSG-TXT is a string field with a defined maximum length. | | 130-MOVE-DCL-2-PDA-FIELDS, 120-EXIT-STUFF | RL-009 | Data Assignment | All output must be returned in the same structure as the input, with updated fields as described by the business rules. | Always at the end of processing. | - Output structure is identical to input, but with updated/validated fields, status flags, and messages. |

# User Stories

## User Story 1: Dispatcher routes and initializes requests

---

### Story Description:

As a system, I want the dispatcher to route each request to the correct handler and initialize the transaction environment so that operations are processed accurately and reliably.

---

### Business Rule Mapping:

| Rule ID | Paragraph Name       | Rule Description                                                                                                                                                                     |
| ------- | -------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| RL-001  | 0000-EXIT-DISPATCHER | The dispatcher must route the request to the correct handler based on which operation flag is set to true in the input structure. Only one operation flag should be set per request. |
| RL-002  | 100-INITIALIZATION   | The dispatcher must initialize the transaction environment and prepare all required fields before routing to the handler.                                                            |

---

### Relevant Functionality:

- **0000-EXIT-DISPATCHER**
  1. **RL-001:**
     - On entry, check which operation flag is set to true.
     - Route to the corresponding handler (PERFORM block) for that operation.
     - If more than one or none are set, handle as an error or continue as appropriate.
- **100-INITIALIZATION**
  1. **RL-002:**
     - INITIALIZE all relevant working storage and input/output areas.
     - Set SQLCODE and checkpoint increment to 0.
     - Prepare index handles and other required fields.

## User Story 2: Validate and normalize input fields

---

### Story Description:

As a system, I want all PDA fields to be validated and normalized according to business rules so that data integrity is maintained and defaults are applied where necessary.

---

### Business Rule Mapping:

| Rule ID | Paragraph Name                             | Rule Description                                                                                                                                                                                                                                                                      |
| ------- | ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| RL-003  | 110-MOVE-PDA-FIELDS-2-DCL                  | If EXIT-CLOSE-CURSOR is not set, the dispatcher must copy and validate all PDA fields, applying default values and business rules for dates, times, numerics, indicators, and flags.                                                                                                  |
| RL-004  | 117-MOVE-ROLLUP-DATA                       | After PDA field validation, rollup normalization must copy the rollup text and validate all 10 numeric slots, setting any non-numeric slot to zero.                                                                                                                                   |
| RL-005  | 112-MOVE-TIME-FIELDS, 132-MOVE-TIME-FIELDS | Time normalization must convert all open/close times for the week to 26-character timestamp strings if the environment is Oracle or if certain operation flags are set. Otherwise, times are copied as 8-character strings. If normalization fails, set error message and flag error. |

---

### Relevant Functionality:

- **110-MOVE-PDA-FIELDS-2-DCL**
  1. **RL-003:**
     - For each date field: if blank or zero, set to default date.
     - For each time field: if blank or zero, set to default time; if DB2 max time, set to Oracle max time.
     - For each rollup numeric field: if not numeric, set to zero.
     - For each indicator field: set to 0 if associated field is valid, -1 if invalid/missing.
     - For each non-numeric field: if not numeric, set to zero.
     - For each single-character flag: if blank, set to business rule default.
     - For ASSOC-STR-TYP-CD: if blank, set to 'S'.
     - For ASSOC-STR-NBR and RPLACD-BY-STR-NBR: if zero, set indicator to -1.
     - For store preference flags: set NO-UNLIKE-SUB-STORE-PREF or NO-DISP-PAL-SUB-STORE-PREF to true if both related flags are false; set DONT-SEND-REAL-TIME-G3 to true if SEND-REAL-TIME-G3 is false.
- **117-MOVE-ROLLUP-DATA**
  1. **RL-004:**
     - Copy rollup text from input to working storage.
     - For each of the 10 numeric slots:
       - If slot is numeric, copy value.
       - If not numeric, set to zero.
     - Copy validated values to output structure.
- **112-MOVE-TIME-FIELDS**
  1. **RL-005:**
     - If Oracle or relevant flags set:
       - Convert each open/close time to timestamp string using external routine.
       - If conversion fails, set error message and flag error.
       - Otherwise, store converted timestamps in output.
     - Else:
       - Copy 8-character time strings as-is.

## User Story 3: Handle database operations and return results

---

### Story Description:

As a user, I want the system to manage Oracle connections and cursor operations, validate IDs, and update the output structure with success or failure flags and error messages so that I am informed of the outcome of my request.

---

### Business Rule Mapping:

| Rule ID | Paragraph Name                                | Rule Description                                                                                                                                                                                              |
| ------- | --------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| RL-006  | 115-CONNECT-TO-ORACLE                         | If the operation requires an Oracle connection, attempt to connect. If the connection fails, set error message in IS-RTRN-MSG-TXT with SQLCODE, truncated as needed.                                          |
| RL-007  | 1000-EXIT-OPEN-CURSOR, 1100-EXIT-CLOSE-CURSOR | For cursor operations, open/close the specified cursor if the cursor ID is valid. If not valid, set FAILURE and IS-RTRN-MSG-TXT to appropriate error message.                                                 |
| RL-008  | 120-EXIT-STUFF, throughout error handling     | The dispatcher must update SUCCESS or FAILURE flags and IS-RTRN-MSG-TXT in the output structure to reflect the result of the operation. If an error message exceeds the maximum length, it must be truncated. |
| RL-009  | 130-MOVE-DCL-2-PDA-FIELDS, 120-EXIT-STUFF     | All output must be returned in the same structure as the input, with updated fields as described by the business rules.                                                                                       |

---

### Relevant Functionality:

- **115-CONNECT-TO-ORACLE**
  1. **RL-006:**
     - Attempt to connect to Oracle.
     - If connection fails:
       - Set error message with SQLCODE in IS-RTRN-MSG-TXT.
       - Truncate message if necessary.
- **1000-EXIT-OPEN-CURSOR**
  1. **RL-007:**
     - On open/close cursor request:
       - If cursor ID is valid, perform SQL OPEN/CLOSE.
       - Else, set FAILURE and error message in IS-RTRN-MSG-TXT.
- **120-EXIT-STUFF**
  1. **RL-008:**
     - After each operation, set SUCCESS or FAILURE as appropriate.
     - If an error message is set, ensure it does not exceed the maximum length; truncate if necessary.
     - Copy updated fields to output structure.
- **130-MOVE-DCL-2-PDA-FIELDS**
  1. **RL-009:**
     - Copy all relevant fields from working storage/output areas to the output structure.
     - Ensure all business rule updates are reflected in the output.

# Code Walkthrough

## Dispatching exit operations and routing to handlers

<SwmSnippet path="/base/src/NNNS0488.cbl" line="1209">

---

0000-EXIT-DISPATCHER starts the flow, calls 100-INITIALIZATION to prep the environment, then routes to the correct handler for the requested operation.

```cobol
125400 0000-EXIT-DISPATCHER.                                            00125400
125500     PERFORM 100-INITIALIZATION                                   00125500
125600     EVALUATE TRUE                                                00125600
125700       WHEN NOT SUCCESS                                           00125700
125800          CONTINUE                                                00125800
125900       WHEN EXIT-OPEN-CURSOR                                      00125900
126000          PERFORM 1000-EXIT-OPEN-CURSOR                           00126000
126100       WHEN EXIT-CLOSE-CURSOR                                     00126100
126200          PERFORM 1100-EXIT-CLOSE-CURSOR                          00126200
126300       WHEN EXIT-GET-UNIQUE-ROW                                   00126300
126400          PERFORM 1200-EXIT-GET-UNIQUE-ROW                        00126400
126500       WHEN EXIT-GET-NEXT-ROW                                     00126500
126600          PERFORM 1300-EXIT-GET-NEXT-ROW                          00126600
126700       WHEN EXIT-PUT-MODIFY-ROW                                   00126700
126800          PERFORM 1400-EXIT-PUT-MODIFY-ROW                        00126800
126900       WHEN EXIT-PUT-INSERT-ROW                                   00126900
127000          PERFORM 1500-EXIT-PUT-INSERT-ROW                        00127000
127100       WHEN EXIT-PUT-PURGE-ROW                                    00127100
127200          PERFORM 1600-EXIT-PUT-PURGE-ROW                         00127200
127300       WHEN EXIT-DO-SPECIAL-IO-FUNCS                              00127300
127400          PERFORM 10000-DO-SPECIAL-IO-FUNCS                       00127400
127500     END-EVALUATE                                                 00127500
127600     PERFORM 120-EXIT-STUFF                                       00127600
127700     GOBACK                                                       00127700
127800     .                                                            00127800
```

---

</SwmSnippet>

### Preparing transaction state and location data

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Prepare transaction environment"]
    click node1 openCode "base/src/NNNS0488.cbl:1239:1247"
    node1 --> node2{"EXIT-CLOSE-CURSOR not set?"}
    
    node2 -->|"Yes"| node3["Copy and validate PDA fields"]
    click node3 openCode "base/src/NNNS0488.cbl:1249:1250"
    node2 -->|"No"| node4{"Need Oracle connection?"}
    click node4 openCode "base/src/NNNS0488.cbl:1251:1254"
    node3 --> node4
    node4 -->|"Yes"| node5["Connect to Oracle"]
    click node5 openCode "base/src/NNNS0488.cbl:1253:1254"
    node4 -->|"No"| node6["Ready for processing"]
    click node6 openCode "base/src/NNNS0488.cbl:1255:1255"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
click node2 goToHeading "Copying and validating location fields, handling rollup and time data"
node2:::HeadingStyle
```

<SwmSnippet path="/base/src/NNNS0488.cbl" line="1239">

---

100-INITIALIZATION sets up the transaction and calls 110-MOVE-PDA-FIELDS-2-DCL to copy and clean up location data for downstream use.

```cobol
128400 100-INITIALIZATION.                                              00128400
128500     INITIALIZE XXXN001A                                          00128500
128600                DAO-STATUS                                        00128600
128700                WS-LOC-STAT-SW                                    00128700
128800                WS-CURR-VALUES                                    00128800
128900     MOVE NNNN0000-INDEX-HANDLE TO DDDTLR01-INDEX-HANDLE          00128900
129000     MOVE 0 TO WS-CHECKPOINT-INC                                  00129000
129100     MOVE 0 TO SQLCODE                                            00129100
129200     MOVE 0 TO SQL-INIT-FLAG                                      00129200
129300     IF NOT EXIT-CLOSE-CURSOR                                     00129300
129400       PERFORM 110-MOVE-PDA-FIELDS-2-DCL                          00129400
129500     END-IF                                                       00129500
129600     IF (YYYN005A-ORACLE       OR EXIT-PUT-INSERT-ROW             00129600
129700         OR EXIT-PUT-PURGE-ROW OR EXIT-PUT-MODIFY-ROW)            00129700
129800       PERFORM 115-CONNECT-TO-ORACLE                              00129800
129900     END-IF                                                       00129900
130000     .                                                            00130000
```

---

</SwmSnippet>

#### Copying and validating location fields, handling rollup and time data

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Transfer location fields from source to destination"] --> node2{"Are any key fields missing or invalid? (dates, times, indicators)"}
    click node1 openCode "base/src/NNNS0488.cbl:1261:1328"
    node2 -->|"Yes"| node3["Set defaults for missing fields (e.g., date to 01/01/1600, time to 00.00.00, indicators to 0/-1)"]
    click node2 openCode "base/src/NNNS0488.cbl:1265:1597"
    click node3 openCode "base/src/NNNS0488.cbl:1265:1597"
    node2 -->|"No"| node4["Proceed to rollup normalization"]
    node3 --> node4
    node4["Invoke rollup normalization"]
    click node4 openCode "base/src/NNNS0488.cbl:1329:1330"
    node4 --> node5["Invoke time normalization"]
    click node5 openCode "base/src/NNNS0488.cbl:1526:1527"
    node5 --> node6{"Is environment CICS?"}
    click node6 openCode "base/src/NNNS0488.cbl:1372:1375"
    node6 -->|"Yes"| node7["Update report sequence number"]
    click node7 openCode "base/src/NNNS0488.cbl:1373:1374"
    node6 -->|"No"| node8["Done"]
    node7 --> node8
    click node8 openCode "base/src/NNNS0488.cbl:1597:1597"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

<SwmSnippet path="/base/src/NNNS0488.cbl" line="1261">

---

110-MOVE-PDA-FIELDS-2-DCL copies location fields from the source to the target record, but also applies business rules: it defaults dates/times if they're blank or invalid, sets indicator flags based on field values, zeros out non-numeric fields, and sets compound flags for store preferences. It then calls 117-MOVE-ROLLUP-DATA to handle rollup fields, making sure those are validated and transferred as well.

```cobol
130600 110-MOVE-PDA-FIELDS-2-DCL.                                       00130600
130700     MOVE LOC-NBR OF P-DDDTLR01 TO LOC-NBR OF DCLXXXAIL-LOC       00130700
130800     MOVE LOC-TYP-CD OF P-DDDTLR01 TO LOC-TYP-CD OF DCLXXXAIL-LOC 00130800
130900                                                                  00130900
131000     IF ASSOC-STR-TYP-CD OF P-DDDTLR01 = SPACES                   00131000
131100        MOVE 'S' TO ASSOC-STR-TYP-CD OF P-DDDTLR01                00131100
131200     END-IF                                                       00131200
131300     MOVE ASSOC-STR-TYP-CD OF P-DDDTLR01                          00131300
131400       TO ASSOC-STR-TYP-CD OF DCLXXXAIL-LOC                       00131400
131500     MOVE 0 TO  WS-ASSOC-ST-TYPE-IND                              00131500
131600     MOVE ASSOC-STR-NBR OF P-DDDTLR01                             00131600
131700       TO ASSOC-STR-NBR OF DCLXXXAIL-LOC                          00131700
131800     IF ASSOC-STR-NBR OF P-DDDTLR01  =  ZERO                      00131800
131900        MOVE -1 TO WS-ASSOC-ST-NO-IND                             00131900
132000     ELSE                                                         00132000
132100        MOVE 0 TO  WS-ASSOC-ST-NO-IND                             00132100
132200     END-IF                                                       00132200
132300                                                                  00132300
132400     IF SLS-CLOSED-DT OF P-DDDTLR01 = SPACES                      00132400
132500     OR SLS-CLOSED-DT OF P-DDDTLR01 = K-ZERO-DT                   00132500
132600       MOVE K-DEF-DT TO SLS-CLOSED-DT OF P-DDDTLR01               00132600
132700     END-IF                                                       00132700
132800     MOVE SLS-CLOSED-DT OF P-DDDTLR01                             00132800
132900       TO SLS-CLOSED-DT OF DCLXXXAIL-LOC                          00132900
133000     IF STR-REMODL-DT OF P-DDDTLR01 = SPACES                      00133000
133100     OR STR-REMODL-DT OF P-DDDTLR01 = K-ZERO-DT                   00133100
133200       MOVE K-DEF-DT TO STR-REMODL-DT OF P-DDDTLR01               00133200
133300     END-IF                                                       00133300
133400     MOVE STR-REMODL-DT OF P-DDDTLR01                             00133400
133500       TO STR-REMODL-DT OF DCLXXXAIL-LOC                          00133500
133600                                                                  00133600
133700     MOVE RETL-LOC-STAT-CD OF P-DDDTLR01                          00133700
133800       TO RETL-LOC-STAT-CD OF DCLXXXAIL-LOC                       00133800
133900                                                                  00133900
134000     IF RETL-LOC-STAT-DT OF P-DDDTLR01 = SPACES                   00134000
134100     OR RETL-LOC-STAT-DT OF P-DDDTLR01 = K-ZERO-DT                00134100
134200       MOVE K-DEF-DT TO RETL-LOC-STAT-DT OF P-DDDTLR01            00134200
134300     END-IF                                                       00134300
134400     MOVE RETL-LOC-STAT-DT OF P-DDDTLR01                          00134400
134500       TO RETL-LOC-STAT-DT OF DCLXXXAIL-LOC                       00134500
134600                                                                  00134600
134700     MOVE COMPANY-ID OF P-DDDTLR01 TO COMPANY-ID OF DCLXXXAIL-LOC 00134700
134800     MOVE FINANCIAL-DIV-ID OF P-DDDTLR01                          00134800
134900       TO FINANCIAL-DIV-ID OF DCLXXXAIL-LOC                       00134900
135000     MOVE LIN-OF-BUS-ID OF P-DDDTLR01                             00135000
135100       TO LIN-OF-BUS-ID OF DCLXXXAIL-LOC                          00135100
135200     MOVE DIST-ID OF P-DDDTLR01 TO DIST-ID OF DCLXXXAIL-LOC       00135200
135300                                   MKT-RGN-ID OF P-DDDTLR01       00135300
135400     MOVE MKT-RGN-ID OF P-DDDTLR01 TO MKT-RGN-ID OF DCLXXXAIL-LOC 00135400
135500     MOVE GEO-ZN-CD OF P-DDDTLR01 TO GEO-ZN-CD OF DCLXXXAIL-LOC   00135500
135600     MOVE RETL-GEO-ZN-ID OF P-DDDTLR01                            00135600
135700       TO RETL-GEO-ZN-ID OF DCLXXXAIL-LOC                         00135700
135800     MOVE SCN-MAINT-SW OF P-DDDTLR01                              00135800
135900       TO SCN-MAINT-SW OF DCLXXXAIL-LOC                           00135900
136000     MOVE FRNT-END-CD OF P-DDDTLR01                               00136000
136100       TO FRNT-END-CD OF DCLXXXAIL-LOC                            00136100
136200     MOVE PRC-BUL-SW OF P-DDDTLR01 TO PRC-BUL-SW OF DCLXXXAIL-LOC 00136200
136300     MOVE UPC-ON-PRC-BUL-SW OF P-DDDTLR01                         00136300
136400       TO UPC-ON-PRC-BUL-SW OF DCLXXXAIL-LOC                      00136400
136500     MOVE CMPTR-TYP-CD OF P-DDDTLR01                              00136500
136600       TO CMPTR-TYP-CD OF DCLXXXAIL-LOC                           00136600
136700     MOVE RETL-VID-ZN-NBR OF P-DDDTLR01                           00136700
136800       TO RETL-VID-ZN-NBR OF DCLXXXAIL-LOC                        00136800
136900     MOVE RETL-UNLD-CD OF P-DDDTLR01                              00136900
137000       TO RETL-UNLD-CD OF DCLXXXAIL-LOC                           00137000
137100*    MOVE ROLUP-REPT-TBL-TXT OF P-DDDTLR01                        00137100
137200     MOVE SPACES                                                  00137200
137300       TO ROLUP-REPT-TBL-TXT OF DCLXXXAIL-LOC                     00137300
137400     PERFORM 117-MOVE-ROLLUP-DATA                                 00137400
137500     MOVE NEW-STR-SW OF P-DDDTLR01 TO NEW-STR-SW OF DCLXXXAIL-LOC 00137500
137600     MOVE SEL-CIR-SW OF P-DDDTLR01 TO SEL-CIR-SW OF DCLXXXAIL-LOC 00137600
137700     MOVE BKRM-SQ-FT OF P-DDDTLR01 TO BKRM-SQ-FT OF DCLXXXAIL-LOC 00137700
137800     MOVE FD-LINER-FT OF P-DDDTLR01                               00137800
137900       TO FD-LINER-FT OF DCLXXXAIL-LOC                            00137900
138000     MOVE NON-FD-LINER-FT OF P-DDDTLR01                           00138000
138100       TO NON-FD-LINER-FT OF DCLXXXAIL-LOC                        00138100
138200     MOVE SETOFF-ROOM-SW OF P-DDDTLR01                            00138200
138300       TO SETOFF-ROOM-SW OF DCLXXXAIL-LOC                         00138300
138400     MOVE CAT-CLS-TBL-TXT OF P-DDDTLR01                           00138400
138500       TO CAT-CLS-TBL-TXT OF DCLXXXAIL-LOC                        00138500
138600                                                                  00138600
138700     IF LAT-K OF P-DDDTLR01 IS NOT NUMERIC                        00138700
138800       MOVE 0 TO LAT-K OF P-DDDTLR01                              00138800
138900     END-IF                                                       00138900
139000     MOVE LAT-K OF P-DDDTLR01 TO LAT-K OF DCLXXXAIL-LOC           00139000
139100                                                                  00139100
139200     IF LON-K OF P-DDDTLR01 IS NOT NUMERIC                        00139200
139300       MOVE 0 TO LON-K OF P-DDDTLR01                              00139300
139400     END-IF                                                       00139400
139500     MOVE LON-K OF P-DDDTLR01 TO LON-K OF DCLXXXAIL-LOC           00139500
139600                                                                  00139600
139700     MOVE CK-COLL-REPT-SW OF P-DDDTLR01                           00139700
139800       TO CK-COLL-REPT-SW OF DCLXXXAIL-LOC                        00139800
139900     MOVE CK-COLL-CNTL-CD OF P-DDDTLR01                           00139900
140000       TO CK-COLL-CNTL-CD OF DCLXXXAIL-LOC                        00140000
140100     MOVE CK-COLL-ADD-DEL-SW OF P-DDDTLR01                        00140100
140200       TO CK-COLL-ADD-DEL-SW OF DCLXXXAIL-LOC                     00140200
140300     MOVE CK-ALT-STR-ID OF P-DDDTLR01                             00140300
140400       TO CK-ALT-STR-ID OF DCLXXXAIL-LOC                          00140400
140500     MOVE CK-COLL-FEE-AMT OF P-DDDTLR01                           00140500
140600       TO CK-COLL-FEE-AMT OF DCLXXXAIL-LOC                        00140600
140700     MOVE SALS-TAX-PCT OF P-DDDTLR01                              00140700
140800       TO SALS-TAX-PCT OF DCLXXXAIL-LOC                           00140800
140900     MOVE SOAP-SALE-VAR-PCT OF P-DDDTLR01                         00140900
141000       TO SOAP-SALE-VAR-PCT OF DCLXXXAIL-LOC                      00141000
141100     MOVE ON-SRS-CD OF P-DDDTLR01 TO ON-SRS-CD OF DCLXXXAIL-LOC   00141100
141200     MOVE SRS-DSD-ORD-SW OF P-DDDTLR01                            00141200
141300       TO SRS-DSD-ORD-SW OF DCLXXXAIL-LOC                         00141300
141400     MOVE RETL-LOC-TYP-CD OF P-DDDTLR01                           00141400
141500       TO RETL-LOC-TYP-CD OF DCLXXXAIL-LOC                        00141500
141600     MOVE DEA-NBR OF P-DDDTLR01 TO DEA-NBR OF DCLXXXAIL-LOC       00141600
141700     IF YYYN005A-CICS-ENV                                         00141700
141800       MOVE STR-OPSTMT-SRT-CD OF P-DDDTLR01                       00141800
141900         TO RPRT-SEQ-NBR      OF P-DDDTLR01                       00141900
142000     END-IF                                                       00142000
142100     MOVE RPRT-SEQ-NBR OF P-DDDTLR01                              00142100
142200       TO RPRT-SEQ-NBR OF DCLXXXAIL-LOC                           00142200
142300     MOVE STR-OPSTMT-SRT-CD OF P-DDDTLR01                         00142300
142400       TO STR-OPSTMT-SRT-CD OF DCLXXXAIL-LOC                      00142400
142500     MOVE STR-OPSTMT-TYP-CD OF P-DDDTLR01                         00142500
142600       TO STR-OPSTMT-TYP-CD OF DCLXXXAIL-LOC                      00142600
142700     MOVE STR-OPSTMT-HDR-CD OF P-DDDTLR01                         00142700
142800       TO STR-OPSTMT-HDR-CD OF DCLXXXAIL-LOC                      00142800
142900     MOVE DPS-NBR OF P-DDDTLR01 TO DPS-NBR OF DCLXXXAIL-LOC       00142900
143000     MOVE MEDICARE-ID OF P-DDDTLR01                               00143000
143100       TO MEDICARE-ID OF DCLXXXAIL-LOC                            00143100
143200     MOVE NABP-NBR OF P-DDDTLR01 TO NABP-NBR OF DCLXXXAIL-LOC     00143200
143300     MOVE NATL-PROV-ID OF P-DDDTLR01                              00143300
143400       TO NATL-PROV-ID OF DCLXXXAIL-LOC                           00143400
143500     MOVE CURR-AD-ZN-NBR OF P-DDDTLR01                            00143500
143600       TO CURR-AD-ZN-NBR OF DCLXXXAIL-LOC                         00143600
143700     MOVE PD-ZONE-NO OF P-DDDTLR01 TO PD-ZONE-NO OF DCLXXXAIL-LOC 00143700
143800     MOVE SOS-PROC-SW OF P-DDDTLR01                               00143800
143900       TO SOS-PROC-SW OF DCLXXXAIL-LOC                            00143900
144000     MOVE GRP-CD OF P-DDDTLR01 TO GRP-CD OF DCLXXXAIL-LOC         00144000
144100     MOVE PRIM-GRP-CD-1 OF P-DDDTLR01                             00144100
144200       TO PRIM-GRP-CD-1 OF DCLXXXAIL-LOC                          00144200
144300     MOVE PRIM-GRP-CD-2 OF P-DDDTLR01                             00144300
144400       TO PRIM-GRP-CD-2 OF DCLXXXAIL-LOC                          00144400
144500     MOVE SECY-GRP-CD-1 OF P-DDDTLR01                             00144500
144600       TO SECY-GRP-CD-1 OF DCLXXXAIL-LOC                          00144600
144700     MOVE SECY-GRP-CD-2 OF P-DDDTLR01                             00144700
144800       TO SECY-GRP-CD-2 OF DCLXXXAIL-LOC                          00144800
144900     MOVE PRIM-CLS-NBR-1 OF P-DDDTLR01                            00144900
145000       TO PRIM-CLS-NBR-1 OF DCLXXXAIL-LOC                         00145000
145100     MOVE PRIM-CLS-NBR-2 OF P-DDDTLR01                            00145100
145200       TO PRIM-CLS-NBR-2 OF DCLXXXAIL-LOC                         00145200
145300     MOVE SECY-CLS-NBR-1 OF P-DDDTLR01                            00145300
145400       TO SECY-CLS-NBR-1 OF DCLXXXAIL-LOC                         00145400
145500     MOVE SECY-CLS-NBR-2 OF P-DDDTLR01                            00145500
145600       TO SECY-CLS-NBR-2 OF DCLXXXAIL-LOC                         00145600
145700     MOVE VAL-STR-SW OF P-DDDTLR01 TO VAL-STR-SW OF DCLXXXAIL-LOC 00145700
145800     IF TBCO-PRMT-NBR OF P-DDDTLR01 NOT NUMERIC                   00145800
145900       MOVE ZEROES TO TBCO-PRMT-NBR OF P-DDDTLR01                 00145900
146000     END-IF                                                       00146000
146100                                                                  00146100
146200     MOVE TBCO-PRMT-NBR OF P-DDDTLR01                             00146200
146300       TO TBCO-PRMT-NBR OF DCLXXXAIL-LOC                          00146300
146400                                                                  00146400
146500     IF  NOT OK-TO-SUB-UNLIKE-PRODS   OF P-DDDTLR01               00146500
146600     AND NOT DONT-SUB-UNLIKE-PRODS    OF P-DDDTLR01               00146600
146700       SET NO-UNLIKE-SUB-STORE-PREF   OF P-DDDTLR01 TO TRUE       00146700
146800     END-IF                                                       00146800
146900     MOVE SUB-UNLIKE-PROD-CD OF P-DDDTLR01                        00146900
147000       TO SUB-UNLIKE-PROD-CD OF DCLXXXAIL-LOC                     00147000
147100                                                                  00147100
147200     IF  NOT OK-TO-SUB-DISP-PALS      OF P-DDDTLR01               00147200
147300     AND NOT DONT-SUB-DISP-PALS       OF P-DDDTLR01               00147300
147400       SET NO-DISP-PAL-SUB-STORE-PREF OF P-DDDTLR01 TO TRUE       00147400
147500     END-IF                                                       00147500
147600     MOVE SUB-DSPLY-PAL-CD   OF P-DDDTLR01                        00147600
147700       TO SUB-DSPLY-PAL-CD   OF DCLXXXAIL-LOC                     00147700
147800                                                                  00147800
147900     IF  NOT SEND-REAL-TIME-G3        OF P-DDDTLR01               00147900
148000       SET DONT-SEND-REAL-TIME-G3     OF P-DDDTLR01 TO TRUE       00148000
148100     END-IF                                                       00148100
148200     MOVE RLTM-SCN-MAINT-SW  OF P-DDDTLR01                        00148200
148300       TO RLTM-SCN-MAINT-SW  OF DCLXXXAIL-LOC                     00148300
148400     MOVE TOP-LEADER-NM  OF P-DDDTLR01                            00148400
148500       TO TOP-LEADER-NM  OF DCLXXXAIL-LOC                         00148500
148600     MOVE CUST-FRNDLY-NM OF P-DDDTLR01                            00148600
148700       TO CUST-FRNDLY-NM OF DCLXXXAIL-LOC                         00148700
148800     IF SLS-OPEN-DT       OF P-DDDTLR01 = SPACES                  00148800
148900     OR SLS-OPEN-DT       OF P-DDDTLR01 = K-ZERO-DT               00148900
149000        MOVE K-DEF-DT     TO SLS-OPEN-DT OF P-DDDTLR01            00149000
149100     END-IF                                                       00149100
149200     MOVE SLS-OPEN-DT    OF P-DDDTLR01                            00149200
149300       TO SLS-OPEN-DT    OF DCLXXXAIL-LOC                         00149300
149400     IF MON-OPEN-TM       OF P-DDDTLR01 = SPACES                  00149400
149500     OR MON-OPEN-TM       OF P-DDDTLR01 = K-DEF-TM                00149500
149600        MOVE K-DEF-TM     TO MON-OPEN-TM OF P-DDDTLR01            00149600
149700     END-IF                                                       00149700
149800     IF MON-CLOS-TM       OF P-DDDTLR01 = SPACES                  00149800
149900     OR MON-CLOS-TM       OF P-DDDTLR01 = K-DEF-TM                00149900
150000        MOVE K-DEF-TM     TO MON-CLOS-TM OF P-DDDTLR01            00150000
150100     END-IF                                                       00150100
150200     IF MON-CLOS-TM       OF P-DDDTLR01 = K-DB2-MAX-TM            00150200
150300        MOVE K-ORA-MAX-TM TO MON-CLOS-TM OF P-DDDTLR01            00150300
150400     END-IF                                                       00150400
150500     IF TUE-OPEN-TM       OF P-DDDTLR01 = SPACES                  00150500
150600     OR TUE-OPEN-TM       OF P-DDDTLR01 = K-DEF-TM                00150600
150700        MOVE K-DEF-TM     TO TUE-OPEN-TM OF P-DDDTLR01            00150700
150800     END-IF                                                       00150800
150900     IF TUE-CLOS-TM       OF P-DDDTLR01 = SPACES                  00150900
151000     OR TUE-CLOS-TM       OF P-DDDTLR01 = K-DEF-TM                00151000
151100        MOVE K-DEF-TM     TO TUE-CLOS-TM OF P-DDDTLR01            00151100
151200     END-IF                                                       00151200
151300     IF TUE-CLOS-TM       OF P-DDDTLR01 = K-DB2-MAX-TM            00151300
151400        MOVE K-ORA-MAX-TM TO TUE-CLOS-TM OF P-DDDTLR01            00151400
151500     END-IF                                                       00151500
151600     IF WED-OPEN-TM       OF P-DDDTLR01 = SPACES                  00151600
151700     OR WED-OPEN-TM       OF P-DDDTLR01 = K-DEF-TM                00151700
151800        MOVE K-DEF-TM     TO WED-OPEN-TM OF P-DDDTLR01            00151800
151900     END-IF                                                       00151900
152000     IF WED-CLOS-TM       OF P-DDDTLR01 = SPACES                  00152000
152100     OR WED-CLOS-TM       OF P-DDDTLR01 = K-DEF-TM                00152100
152200        MOVE K-DEF-TM     TO WED-CLOS-TM OF P-DDDTLR01            00152200
152300     END-IF                                                       00152300
152400     IF WED-CLOS-TM       OF P-DDDTLR01 = K-DB2-MAX-TM            00152400
152500        MOVE K-ORA-MAX-TM TO WED-CLOS-TM OF P-DDDTLR01            00152500
152600     END-IF                                                       00152600
152700     IF THUR-OPEN-TM      OF P-DDDTLR01 = SPACES                  00152700
152800     OR THUR-OPEN-TM      OF P-DDDTLR01 = K-DEF-TM                00152800
152900        MOVE K-DEF-TM     TO THUR-OPEN-TM  OF P-DDDTLR01          00152900
153000     END-IF                                                       00153000
153100     IF THUR-CLOS-TM      OF P-DDDTLR01 = SPACES                  00153100
153200     OR THUR-CLOS-TM      OF P-DDDTLR01 = K-DEF-TM                00153200
153300        MOVE K-DEF-TM     TO THUR-CLOS-TM OF P-DDDTLR01           00153300
153400     END-IF                                                       00153400
153500     IF THUR-CLOS-TM      OF P-DDDTLR01 = K-DB2-MAX-TM            00153500
153600        MOVE K-ORA-MAX-TM TO THUR-CLOS-TM OF P-DDDTLR01           00153600
153700     END-IF                                                       00153700
153800     IF FRI-OPEN-TM       OF P-DDDTLR01 = SPACES                  00153800
153900     OR FRI-OPEN-TM       OF P-DDDTLR01 = K-DEF-TM                00153900
154000        MOVE K-DEF-TM     TO FRI-OPEN-TM OF P-DDDTLR01            00154000
154100     END-IF                                                       00154100
154200     IF FRI-CLOS-TM       OF P-DDDTLR01 = SPACES                  00154200
154300     OR FRI-CLOS-TM       OF P-DDDTLR01 = K-DEF-TM                00154300
154400        MOVE K-DEF-TM     TO FRI-CLOS-TM OF P-DDDTLR01            00154400
154500     END-IF                                                       00154500
154600     IF FRI-CLOS-TM       OF P-DDDTLR01 = K-DB2-MAX-TM            00154600
154700        MOVE K-ORA-MAX-TM TO FRI-CLOS-TM OF P-DDDTLR01            00154700
154800     END-IF                                                       00154800
154900     IF SAT-OPEN-TM       OF P-DDDTLR01 = SPACES                  00154900
155000     OR SAT-OPEN-TM       OF P-DDDTLR01 = K-DEF-TM                00155000
155100        MOVE K-DEF-TM     TO SAT-OPEN-TM OF P-DDDTLR01            00155100
155200     END-IF                                                       00155200
155300     IF SAT-CLOS-TM       OF P-DDDTLR01 = SPACES                  00155300
155400     OR SAT-CLOS-TM       OF P-DDDTLR01 = K-DEF-TM                00155400
155500        MOVE K-DEF-TM     TO SAT-CLOS-TM OF P-DDDTLR01            00155500
155600     END-IF                                                       00155600
155700     IF SAT-CLOS-TM       OF P-DDDTLR01 = K-DB2-MAX-TM            00155700
155800        MOVE K-ORA-MAX-TM TO SAT-CLOS-TM OF P-DDDTLR01            00155800
155900     END-IF                                                       00155900
156000     IF SUN-OPEN-TM       OF P-DDDTLR01 = SPACES                  00156000
156100     OR SUN-OPEN-TM       OF P-DDDTLR01 = K-DEF-TM                00156100
156200        MOVE K-DEF-TM     TO SUN-OPEN-TM OF P-DDDTLR01            00156200
156300     END-IF                                                       00156300
156400     IF SUN-CLOS-TM       OF P-DDDTLR01 = SPACES                  00156400
156500     OR SUN-CLOS-TM       OF P-DDDTLR01 = K-DEF-TM                00156500
156600        MOVE K-DEF-TM     TO SUN-CLOS-TM OF P-DDDTLR01            00156600
156700     END-IF                                                       00156700
156800     IF SUN-CLOS-TM       OF P-DDDTLR01 = K-DB2-MAX-TM            00156800
156900        MOVE K-ORA-MAX-TM TO SUN-CLOS-TM OF P-DDDTLR01            00156900
157000     END-IF                                                       00157000
157100     PERFORM 112-MOVE-TIME-FIELDS                                 00157100
157200     MOVE RETL-LOC-FRMAT-CD OF P-DDDTLR01                         00157200
157300       TO RETL-LOC-FRMAT-CD OF DCLXXXAIL-LOC                      00157300
157400     MOVE RETL-LOC-SEGM-CD OF P-DDDTLR01                          00157400
157500       TO RETL-LOC-SEGM-CD OF DCLXXXAIL-LOC                       00157500
157600     MOVE ECOMM-MKT-AREA-CD OF P-DDDTLR01                         00157600
157700       TO ECOMM-MKT-AREA-CD OF DCLXXXAIL-LOC                      00157700
157800     IF ECOMM-STRT-DT OF P-DDDTLR01 = SPACES                      00157800
157900     OR ECOMM-STRT-DT OF P-DDDTLR01 = K-ZERO-DT                   00157900
158000       MOVE K-DEF-DT TO ECOMM-STRT-DT OF P-DDDTLR01               00158000
158100     END-IF                                                       00158100
158200     MOVE ECOMM-STRT-DT OF P-DDDTLR01                             00158200
158300       TO ECOMM-STRT-DT OF DCLXXXAIL-LOC                          00158300
158400     MOVE 0 TO ECOMM-STRT-DT-IND OF P-DDDTLR01                    00158400
158500     MOVE 0 TO ECOMM-STRT-DT-IND OF DCLXXXAIL-LOC-IND             00158500
158600     IF ECOMM-END-DT OF P-DDDTLR01 = SPACES                       00158600
158700     OR ECOMM-END-DT OF P-DDDTLR01 = K-ZERO-DT                    00158700
158800       MOVE K-DEF-DT TO ECOMM-END-DT OF P-DDDTLR01                00158800
158900     END-IF                                                       00158900
159000     MOVE ECOMM-END-DT OF P-DDDTLR01                              00159000
159100       TO ECOMM-END-DT OF DCLXXXAIL-LOC                           00159100
159200     MOVE 0 TO ECOMM-END-DT-IND OF P-DDDTLR01                     00159200
159300     MOVE 0 TO ECOMM-END-DT-IND OF DCLXXXAIL-LOC-IND              00159300
159400     MOVE ONLIN-SSON-SW OF P-DDDTLR01                             00159400
159500                        TO ONLIN-SSON-SW  OF DCLXXXAIL-LOC        00159500
159510     MOVE 0 TO  RPLACD-BY-STR-NBR-IND                             00159510
159600     MOVE RPLACD-BY-STR-NBR OF P-DDDTLR01                         00159600
159700                     TO RPLACD-BY-STR-NBR OF DCLXXXAIL-LOC        00159700
159710     IF RPLACD-BY-STR-NBR  OF P-DDDTLR01  =  ZERO                 00159710
159720        MOVE -1 TO RPLACD-BY-STR-NBR-IND  OF DCLXXXAIL-LOC-IND    00159720
159730     ELSE                                                         00159730
159740        MOVE 0 TO  RPLACD-BY-STR-NBR-IND OF DCLXXXAIL-LOC-IND     00159740
159750     END-IF                                                       00159750
159760     .                                                            00159760
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/NNNS0488.cbl" line="1687">

---

117-MOVE-ROLLUP-DATA copies the rollup text field, then checks each of the 10 numeric slots for valid numbers. If a slot isn't numeric, it sets the corresponding field to zero in the destination record. This keeps rollup data clean for later reporting.

```cobol
172400 117-MOVE-ROLLUP-DATA.                                            00172400
172500     MOVE ROLUP-REPT-TBL-TXT OF P-DDDTLR01                        00172500
172600       TO WS-REPT-TBL-TXT                                         00172600
172700     IF WS-REPT-TBL-NUMERIC(1) IS NUMERIC                         00172700
172800        MOVE WS-REPT-TBL-NUMERIC(1)                               00172800
172900          TO ROLUP-REPT-TBL-01-NBR  OF DCLXXXAIL-LOC              00172900
173000     ELSE                                                         00173000
173100        MOVE ZERO TO ROLUP-REPT-TBL-01-NBR OF DCLXXXAIL-LOC       00173100
173200     END-IF                                                       00173200
173300     IF WS-REPT-TBL-NUMERIC(2) IS NUMERIC                         00173300
173400        MOVE WS-REPT-TBL-NUMERIC(2)                               00173400
173500          TO ROLUP-REPT-TBL-02-NBR  OF DCLXXXAIL-LOC              00173500
173600     ELSE                                                         00173600
173700        MOVE ZERO TO ROLUP-REPT-TBL-02-NBR OF DCLXXXAIL-LOC       00173700
173800     END-IF                                                       00173800
173900     IF WS-REPT-TBL-NUMERIC(3) IS NUMERIC                         00173900
174000        MOVE WS-REPT-TBL-NUMERIC(3)                               00174000
174100          TO ROLUP-REPT-TBL-03-NBR  OF DCLXXXAIL-LOC              00174100
174200     ELSE                                                         00174200
174300        MOVE ZERO TO ROLUP-REPT-TBL-03-NBR OF DCLXXXAIL-LOC       00174300
174400     END-IF                                                       00174400
174500     IF WS-REPT-TBL-NUMERIC(4) IS NUMERIC                         00174500
174600        MOVE WS-REPT-TBL-NUMERIC(4)                               00174600
174700          TO ROLUP-REPT-TBL-04-NBR  OF DCLXXXAIL-LOC              00174700
174800     ELSE                                                         00174800
174900        MOVE ZERO TO ROLUP-REPT-TBL-04-NBR OF DCLXXXAIL-LOC       00174900
175000     END-IF                                                       00175000
175100     IF WS-REPT-TBL-NUMERIC(5) IS NUMERIC                         00175100
175200        MOVE WS-REPT-TBL-NUMERIC(5)                               00175200
175300          TO ROLUP-REPT-TBL-05-NBR  OF DCLXXXAIL-LOC              00175300
175400     ELSE                                                         00175400
175500        MOVE ZERO TO ROLUP-REPT-TBL-05-NBR OF DCLXXXAIL-LOC       00175500
175600     END-IF                                                       00175600
175700     IF WS-REPT-TBL-NUMERIC(6) IS NUMERIC                         00175700
175800        MOVE WS-REPT-TBL-NUMERIC(6)                               00175800
175900          TO ROLUP-REPT-TBL-06-NBR  OF DCLXXXAIL-LOC              00175900
176000     ELSE                                                         00176000
176100        MOVE ZERO TO ROLUP-REPT-TBL-06-NBR OF DCLXXXAIL-LOC       00176100
176200     END-IF                                                       00176200
176300     IF WS-REPT-TBL-NUMERIC(7) IS NUMERIC                         00176300
176400        MOVE WS-REPT-TBL-NUMERIC(7)                               00176400
176500          TO ROLUP-REPT-TBL-07-NBR  OF DCLXXXAIL-LOC              00176500
176600     ELSE                                                         00176600
176700        MOVE ZERO TO ROLUP-REPT-TBL-07-NBR OF DCLXXXAIL-LOC       00176700
176800     END-IF                                                       00176800
176900     IF WS-REPT-TBL-NUMERIC(8) IS NUMERIC                         00176900
177000        MOVE WS-REPT-TBL-NUMERIC(8)                               00177000
177100          TO ROLUP-REPT-TBL-08-NBR  OF DCLXXXAIL-LOC              00177100
177200     ELSE                                                         00177200
177300        MOVE ZERO TO ROLUP-REPT-TBL-08-NBR OF DCLXXXAIL-LOC       00177300
177400     END-IF                                                       00177400
177500     IF WS-REPT-TBL-NUMERIC(9) IS NUMERIC                         00177500
177600        MOVE WS-REPT-TBL-NUMERIC(9)                               00177600
177700          TO ROLUP-REPT-TBL-09-NBR  OF DCLXXXAIL-LOC              00177700
177800     ELSE                                                         00177800
177900        MOVE ZERO TO ROLUP-REPT-TBL-09-NBR OF DCLXXXAIL-LOC       00177900
178000     END-IF                                                       00178000
178100     IF WS-REPT-TBL-NUMERIC(10) IS NUMERIC                        00178100
178200        MOVE WS-REPT-TBL-NUMERIC(10)                              00178200
178300          TO ROLUP-REPT-TBL-10-NBR  OF DCLXXXAIL-LOC              00178300
178400     ELSE                                                         00178400
178500        MOVE ZERO TO ROLUP-REPT-TBL-10-NBR OF DCLXXXAIL-LOC       00178500
178600     END-IF                                                       00178600
178700     .                                                            00178700
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/NNNS0488.cbl" line="1565">

---

112-MOVE-TIME-FIELDS checks if we're working with Oracle or certain exit codes. If so, it collects all open/close times for the week, calls MMMS0291 to convert them to timestamps, and moves the results to timestamp fields. If not, it just copies the times directly. The MMMS0291 utility does the actual conversion.

```cobol
160200 112-MOVE-TIME-FIELDS.                                            00160200
160300     IF (YYYN005A-ORACLE OR EXIT-PUT-INSERT-ROW                   00160300
160400         OR EXIT-PUT-MODIFY-ROW)                                  00160400
160500       INITIALIZE MMMC0291-INPUT-TM                               00160500
160600                  MMMC0291-INPUT-TS                               00160600
160700       MOVE MON-OPEN-TM OF P-DDDTLR01                             00160700
160800         TO WS-TIME-INOUT-CONV(1)                                 00160800
160900       MOVE MON-CLOS-TM OF P-DDDTLR01                             00160900
161000         TO WS-TIME-INOUT-CONV(2)                                 00161000
161100       MOVE TUE-OPEN-TM OF P-DDDTLR01                             00161100
161200         TO WS-TIME-INOUT-CONV(3)                                 00161200
161300       MOVE TUE-CLOS-TM OF P-DDDTLR01                             00161300
161400         TO WS-TIME-INOUT-CONV(4)                                 00161400
161500       MOVE WED-OPEN-TM OF P-DDDTLR01                             00161500
161600         TO WS-TIME-INOUT-CONV(5)                                 00161600
161700       MOVE WED-CLOS-TM OF P-DDDTLR01                             00161700
161800         TO WS-TIME-INOUT-CONV(6)                                 00161800
161900       MOVE THUR-OPEN-TM OF P-DDDTLR01                            00161900
162000         TO WS-TIME-INOUT-CONV(7)                                 00162000
162100       MOVE THUR-CLOS-TM OF P-DDDTLR01                            00162100
162200         TO WS-TIME-INOUT-CONV(8)                                 00162200
162300       MOVE FRI-OPEN-TM OF P-DDDTLR01                             00162300
162400         TO WS-TIME-INOUT-CONV(9)                                 00162400
162500       MOVE FRI-CLOS-TM OF P-DDDTLR01                             00162500
162600         TO WS-TIME-INOUT-CONV(10)                                00162600
162700       MOVE SAT-OPEN-TM OF P-DDDTLR01                             00162700
162800         TO WS-TIME-INOUT-CONV(11)                                00162800
162900       MOVE SAT-CLOS-TM OF P-DDDTLR01                             00162900
163000         TO WS-TIME-INOUT-CONV(12)                                00163000
163100       MOVE SUN-OPEN-TM OF P-DDDTLR01                             00163100
163200         TO WS-TIME-INOUT-CONV(13)                                00163200
163300       MOVE SUN-CLOS-TM OF P-DDDTLR01                             00163300
163400         TO WS-TIME-INOUT-CONV(14)                                00163400
163500                                                                  00163500
163600       SET  MMMC0291-CVT-TM-TO-TS  TO TRUE                        00163600
163700       CALL WS-MMMS0291-PGM USING                                 00163700
163800                          XXXN001A                                00163800
163900                          MMMC0291                                00163900
164000                                                                  00164000
164100       IF NOT SUCCESS                                             00164100
164200         STRING 'NNNS0488 - INVALID TIME.PLS VERIFY Sqlcode ='    00164200
164300             WS-SQLCODE                                           00164300
164400             DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT               00164400
164500       ELSE                                                       00164500
164600         MOVE WS-TIMSTAMP-INOUT-CONV(1)                           00164600
164700           TO WS-MON-OPEN-TS                                      00164700
164800         MOVE WS-TIMSTAMP-INOUT-CONV(2)                           00164800
164900           TO WS-MON-CLOS-TS                                      00164900
165000         MOVE WS-TIMSTAMP-INOUT-CONV(3)                           00165000
165100           TO WS-TUE-OPEN-TS                                      00165100
165200         MOVE WS-TIMSTAMP-INOUT-CONV(4)                           00165200
165300           TO WS-TUE-CLOS-TS                                      00165300
165400         MOVE WS-TIMSTAMP-INOUT-CONV(5)                           00165400
165500           TO WS-WED-OPEN-TS                                      00165500
165600         MOVE WS-TIMSTAMP-INOUT-CONV(6)                           00165600
165700           TO WS-WED-CLOS-TS                                      00165700
165800         MOVE WS-TIMSTAMP-INOUT-CONV(7)                           00165800
165900           TO WS-THUR-OPEN-TS                                     00165900
166000         MOVE WS-TIMSTAMP-INOUT-CONV(8)                           00166000
166100           TO WS-THUR-CLOS-TS                                     00166100
166200         MOVE WS-TIMSTAMP-INOUT-CONV(9)                           00166200
166300           TO WS-FRI-OPEN-TS                                      00166300
166400         MOVE WS-TIMSTAMP-INOUT-CONV(10)                          00166400
166500           TO WS-FRI-CLOS-TS                                      00166500
166600         MOVE WS-TIMSTAMP-INOUT-CONV(11)                          00166600
166700           TO WS-SAT-OPEN-TS                                      00166700
166800         MOVE WS-TIMSTAMP-INOUT-CONV(12)                          00166800
166900           TO WS-SAT-CLOS-TS                                      00166900
167000         MOVE WS-TIMSTAMP-INOUT-CONV(13)                          00167000
167100           TO WS-SUN-OPEN-TS                                      00167100
167200         MOVE WS-TIMSTAMP-INOUT-CONV(14)                          00167200
167300           TO WS-SUN-CLOS-TS                                      00167300
167400       END-IF                                                     00167400
167500     ELSE                                                         00167500
167600       MOVE MON-OPEN-TM OF P-DDDTLR01                             00167600
167700         TO MON-OPEN-TM OF DCLXXXAIL-LOC                          00167700
167800       MOVE MON-CLOS-TM OF P-DDDTLR01                             00167800
167900         TO MON-CLOS-TM OF DCLXXXAIL-LOC                          00167900
168000       MOVE TUE-OPEN-TM OF P-DDDTLR01                             00168000
168100         TO TUE-OPEN-TM OF DCLXXXAIL-LOC                          00168100
168200       MOVE TUE-CLOS-TM OF P-DDDTLR01                             00168200
168300         TO TUE-CLOS-TM OF DCLXXXAIL-LOC                          00168300
168400       MOVE WED-OPEN-TM OF P-DDDTLR01                             00168400
168500         TO WED-OPEN-TM OF DCLXXXAIL-LOC                          00168500
168600       MOVE WED-CLOS-TM OF P-DDDTLR01                             00168600
168700         TO WED-CLOS-TM OF DCLXXXAIL-LOC                          00168700
168800       MOVE THUR-OPEN-TM OF P-DDDTLR01                            00168800
168900         TO THUR-OPEN-TM OF DCLXXXAIL-LOC                         00168900
169000       MOVE THUR-CLOS-TM OF P-DDDTLR01                            00169000
169100         TO THUR-CLOS-TM OF DCLXXXAIL-LOC                         00169100
169200       MOVE FRI-OPEN-TM OF P-DDDTLR01                             00169200
169300         TO FRI-OPEN-TM OF DCLXXXAIL-LOC                          00169300
169400       MOVE FRI-CLOS-TM OF P-DDDTLR01                             00169400
169500         TO FRI-CLOS-TM OF DCLXXXAIL-LOC                          00169500
169600       MOVE SAT-OPEN-TM OF P-DDDTLR01                             00169600
169700         TO SAT-OPEN-TM OF DCLXXXAIL-LOC                          00169700
169800       MOVE SAT-CLOS-TM OF P-DDDTLR01                             00169800
169900         TO SAT-CLOS-TM OF DCLXXXAIL-LOC                          00169900
170000       MOVE SUN-OPEN-TM OF P-DDDTLR01                             00170000
170100         TO SUN-OPEN-TM OF DCLXXXAIL-LOC                          00170100
170200       MOVE SUN-CLOS-TM OF P-DDDTLR01                             00170200
170300         TO SUN-CLOS-TM OF DCLXXXAIL-LOC                          00170300
170400     END-IF                                                       00170400
170500     .                                                            00170500
```

---

</SwmSnippet>

#### Establishing Oracle database connection

<SwmSnippet path="/base/src/NNNS0488.cbl" line="1674">

---

115-CONNECT-TO-ORACLE calls Z-ORA-CONNECT (which runs XXXS0210) to set up the Oracle database connection. If the connection fails, it builds an error message with the SQLCODE and stores it for downstream error handling. The connection setup is delegated to XXXS0210 so this module doesn't have to manage low-level DB details.

```cobol
171100 115-CONNECT-TO-ORACLE.                                           00171100
171200     CALL Z-ORA-CONNECT USING XXXN001A                            00171200
171300                              SQLCA                               00171300
171400     IF NOT SUCCESS                                               00171400
171500       MOVE SQLCODE TO WS-SQLCODE                                 00171500
171600       MOVE SPACES  TO IS-RTRN-MSG-TXT                            00171600
171700       STRING 'NNNS0488 - Error connecting to Oracle. Sqlcode ='  00171700
171800               WS-SQLCODE                                         00171800
171900               DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT             00171900
172000     END-IF                                                       00172000
172100     .                                                            00172100
```

---

</SwmSnippet>

### Managing SQL cursors for data access

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Dispatch cursor operation request (open/close, cursor ID)"] --> node2{"Is operation Open or Close?"}
    click node1 openCode "base/src/NNNS0488.cbl:2243:2328"
    node2 -->|"Open"| node3{"Is cursor ID valid for open?"}
    click node2 openCode "base/src/NNNS0488.cbl:2243:2286"
    node2 -->|"Close"| node4{"Is cursor ID valid for close?"}
    click node2 openCode "base/src/NNNS0488.cbl:2289:2328"
    node3 -->|"Yes"| node5["Open the specified cursor"]
    click node3 openCode "base/src/NNNS0488.cbl:2243:2286"
    node3 -->|"No"| node6["Set failure and return 'Invalid open cursor ID'"]
    click node6 openCode "base/src/NNNS0488.cbl:2282:2284"
    node4 -->|"Yes"| node7["Close the specified cursor"]
    click node4 openCode "base/src/NNNS0488.cbl:2289:2328"
    node4 -->|"No"| node8["Set failure and return 'Invalid close cursor ID'"]
    click node8 openCode "base/src/NNNS0488.cbl:2324:2326"

classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

<SwmSnippet path="/base/src/NNNS0488.cbl" line="2206">

---

1000-EXIT-OPEN-CURSOR opens the requested SQL cursor based on a hardcoded list of IDs. If the input doesn't match any known cursor, it sets a failure flag and returns an error message. This keeps cursor management predictable and error handling clear.

```cobol
224300 1000-EXIT-OPEN-CURSOR.                                           00224300
224400     EVALUATE TRUE                                                00224400
224500       WHEN DDDXLR01                                              00224500
224600         EXEC SQL                                                 00224600
224700           OPEN DDDXLR01                                          00224700
224800         END-EXEC                                                 00224800
224900       WHEN DDDXLR02                                              00224900
225000         EXEC SQL                                                 00225000
225100           OPEN DDDXLR02                                          00225100
225200         END-EXEC                                                 00225200
225300       WHEN DDDXLR03                                              00225300
225400         EXEC SQL                                                 00225400
225500           OPEN DDDXLR03                                          00225500
225600         END-EXEC                                                 00225600
225700       WHEN DDDXLR04                                              00225700
225800         EXEC SQL                                                 00225800
225900           OPEN DDDXLR04                                          00225900
226000         END-EXEC                                                 00226000
226100       WHEN DDDXLR05                                              00226100
226200         EXEC SQL                                                 00226200
226300           OPEN DDDXLR05                                          00226300
226400         END-EXEC                                                 00226400
226500       WHEN DDDXLR06                                              00226500
226600         EXEC SQL                                                 00226600
226700           OPEN DDDXLR06                                          00226700
226800         END-EXEC                                                 00226800
226900       WHEN DDDXLR07                                              00226900
227000         EXEC SQL                                                 00227000
227100           OPEN DDDXLR07                                          00227100
227200         END-EXEC                                                 00227200
227300       WHEN DDDXLR08                                              00227300
227400         EXEC SQL                                                 00227400
227500           OPEN DDDXLR08                                          00227500
227600         END-EXEC                                                 00227600
227700       WHEN DDDXLR09                                              00227700
227800         EXEC SQL                                                 00227800
227900           OPEN DDDXLR09                                          00227900
228000         END-EXEC                                                 00228000
228100       WHEN OTHER                                                 00228100
228200         SET FAILURE TO TRUE                                      00228200
228300         MOVE 'NNNS0488 - Invalid open cursor ID.'                00228300
228400           TO IS-RTRN-MSG-TXT OF XXXN001A                         00228400
228500     END-EVALUATE                                                 00228500
228600     .                                                            00228600
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/NNNS0488.cbl" line="2252">

---

1100-EXIT-CLOSE-CURSOR closes the requested SQL cursor if the ID matches a known value. If not, it sets a failure flag and returns an error message. This makes sure only valid cursors are closed and errors are clearly reported.

```cobol
228900 1100-EXIT-CLOSE-CURSOR.                                          00228900
229000     EVALUATE TRUE                                                00229000
229100       WHEN DDDXLR01                                              00229100
229200         EXEC SQL                                                 00229200
229300           CLOSE DDDXLR01                                         00229300
229400         END-EXEC                                                 00229400
229500       WHEN DDDXLR02                                              00229500
229600         EXEC SQL                                                 00229600
229700           CLOSE DDDXLR02                                         00229700
229800         END-EXEC                                                 00229800
229900       WHEN DDDXLR03                                              00229900
230000         EXEC SQL                                                 00230000
230100           CLOSE DDDXLR03                                         00230100
230200         END-EXEC                                                 00230200
230300       WHEN DDDXLR04                                              00230300
230400         EXEC SQL                                                 00230400
230500           CLOSE DDDXLR04                                         00230500
230600         END-EXEC                                                 00230600
230700       WHEN DDDXLR05                                              00230700
230800         EXEC SQL                                                 00230800
230900           CLOSE DDDXLR05                                         00230900
231000         END-EXEC                                                 00231000
231100       WHEN DDDXLR06                                              00231100
231200         EXEC SQL                                                 00231200
231300           CLOSE DDDXLR06                                         00231300
231400         END-EXEC                                                 00231400
231500       WHEN DDDXLR07                                              00231500
231600         EXEC SQL                                                 00231600
231700           CLOSE DDDXLR07                                         00231700
231800         END-EXEC                                                 00231800
231900       WHEN DDDXLR09                                              00231900
232000         EXEC SQL                                                 00232000
232100           CLOSE DDDXLR09                                         00232100
232200         END-EXEC                                                 00232200
232300       WHEN OTHER                                                 00232300
232400         SET FAILURE TO TRUE                                      00232400
232500         MOVE 'NNNS0488 - Invalid close cursor ID.'               00232500
232600           TO IS-RTRN-MSG-TXT OF XXXN001A                         00232600
232700     END-EVALUATE                                                 00232700
232800     .                                                            00232800
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
