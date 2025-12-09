---
title: Preparing and Normalizing Location Data
---
This document describes the flow for preparing and normalizing location data. The process receives raw location and environment data, cleans and normalizes it, marks non-shipping days, converts times to timestamps if needed, and establishes a database connection when required. The output is location data that is ready for further business operations.

```mermaid
flowchart TD
  node1["Starting Up and Preparing Location Data"]:::HeadingStyle
  click node1 goToHeading "Starting Up and Preparing Location Data"
  node1 --> node2["Transferring and Normalizing Location Fields"]:::HeadingStyle
  click node2 goToHeading "Transferring and Normalizing Location Fields"
  node2 --> node3["Marking Non-Shipping Days"]:::HeadingStyle
  click node3 goToHeading "Marking Non-Shipping Days"
  node3 --> node4["Converting Times to Timestamps"]:::HeadingStyle
  click node4 goToHeading "Converting Times to Timestamps"
  node4 --> node5["Connecting to Oracle"]:::HeadingStyle
  click node5 goToHeading "Connecting to Oracle"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Spec

## Detailed View of the Program's Functionality

# Detailed Explanation of the Swimmio-dyncall-demo Flow

---

## a. Program Startup and Initialization

When the main program starts, it performs several setup steps to ensure all data structures and environment flags are in a known state:

- It clears (initializes) all working storage fields, status flags, and any temporary data areas that will be used during processing.
- It copies an index handle from the input parameters to the main working structure, ensuring the correct context for subsequent operations.
- It resets checkpoint counters and SQL status codes to zero, preparing for any database operations.
- It checks if the current operation is a cursor close. If not, it proceeds to transfer and normalize location data.
- If the environment requires an Oracle connection (for example, if the system is running in Oracle mode or if the operation is an insert, modify, or purge), it attempts to establish a connection to the Oracle database.

---

## b. Transferring and Normalizing Location Fields

If the operation is not a cursor close, the program proceeds to prepare the location data for further processing:

- It calculates the length of the primary contact name field and calls a string cleaning routine to remove extra spaces from this field.
- It copies all relevant fields (location type, number, name, abbreviation, legal name, contact info, addresses, phone numbers, country info, etc.) from the input parameter area to the main working structure.
- It ensures that all date fields (such as added, deleted, inactive, opened, closed) are set to default values if they are empty or contain a special "zero" value.
- It sets flags to indicate whether the location is active, inactive, or deleted, based on the values of the inactive and deleted date fields.
- It applies business rules for certain location types (such as vendor, store, or DSD vendor), setting additional fields or flags as needed.
- It validates and normalizes numeric fields, ensuring that non-numeric values are replaced with zero.
- It initializes obsolete fields to a default value.
- It marks non-shipping days by checking which days of the week are not available for shipping and setting flags accordingly.
- It prepares time fields for conversion to timestamps, ensuring that any missing or blank times are set to a default value.
- It calls routines to convert time fields to timestamps, preparing the data for use in environments that require timestamp formats.

---

## c. Cleaning Up and Outputting the String

The string cleaning routine is responsible for removing extra spaces from text fields, such as names:

- It validates the input string and its length, ensuring it is not empty or filled with only spaces or low-value characters.
- If the input is invalid, it sets an error flag and returns an error message.
- If the input is valid, it loops through each character in the string, copying characters to the output unless both the current and previous characters are spaces (in which case it skips the extra space).
- After processing, it clears the original string area and copies the cleaned output back, ensuring no leftover data remains.

---

## d. Marking Non-Shipping Days

The program checks each day of the week (Monday through Sunday) to determine if shipping is available:

- For each day, if shipping is not scheduled, it sets a flag to indicate that shipping does not occur on that day.
- This information is used later to quickly determine which days are valid for shipping operations.

---

## e. Converting Times to Timestamps

When working in an Oracle environment or performing row modifications/inserts, the program needs to convert time fields to timestamp format:

- It prepares the input time fields and clears the output timestamp fields.
- It calls a conversion routine that attempts to convert each time value to a timestamp.
- If the conversion is successful, it updates the output fields with the new timestamp values.
- If the conversion fails, it falls back to using the original time values.

---

## f. Time Conversion Dispatcher

The time conversion routine acts as a dispatcher, determining which type of conversion to perform:

- If the request is to convert time to timestamp, it loops through each input value, validates the time components (hours, minutes, seconds), fills in missing values with '00', and constructs a timestamp string. If a value is invalid, it sets an error and message.
- If the request is to convert timestamp to time, it parses the timestamp, extracts the time components, validates them, and fills in missing values as needed. Again, errors are flagged if values are out of range.
- If the function code is not recognized, it sets an error and returns an appropriate message.

---

## g. Connecting to Oracle

When a database connection is required, the program attempts to connect to Oracle:

- It calls a dedicated routine to establish the connection.
- If the connection fails, it retrieves the SQL error code and constructs a detailed error message for troubleshooting.
- If the connection succeeds, processing continues as normal.

---

## h. Main Program Dispatcher

The main dispatcher routine determines which operation to perform based on the current context:

- It calls the initialization routine.
- It checks the operation type (open cursor, close cursor, get unique row, get next row, modify row, insert row, purge row, or special functions) and dispatches control to the appropriate routine.
- After the main operation, it performs any necessary cleanup, such as moving data back to the parameter area, updating checkpoint counters, and resetting SQL codes.

---

## i. Data Movement Back to Parameter Area

After processing, if the operation was not a cursor close and was successful:

- The program copies all relevant fields from the main working structure back to the parameter area, ensuring that the caller receives the updated data.
- It also converts timestamps back to time fields if needed, depending on the environment and operation type.

---

## j. Handling Database Operations

The program contains routines for opening and closing database cursors, fetching rows, modifying, inserting, and deleting records:

- Each operation is handled by a dedicated routine that performs the necessary SQL operations and updates the working structures accordingly.
- Error handling is included to set failure flags and return detailed messages if any database operation fails.

---

## k. Miscellaneous Utility Routines

Several utility routines support the main flow:

- Routines to check and initialize null indicators for database columns.
- Routines to fetch additional data (such as AP numbers, facility IDs, organization IDs) as needed for certain operations.
- Routines to handle event generation, denormalization, and synchronization with other systems.

---

## l. Summary

The overall flow ensures that location data is:

- Properly initialized and validated.
- Cleaned and normalized for consistent processing.
- Correctly transferred between input/output structures and the main working area.
- Enhanced with business logic (such as shipping day flags and timestamp conversions).
- Safely and robustly handled with respect to database operations, with detailed error handling and messaging throughout.

This design allows the program to serve as a robust data access and transformation layer for location-related operations, supporting both legacy and modern database environments.

# Rule Definition

| Paragraph Name                                          | Rule ID | Category          | Description                                                                                                                                                                                                                                               | Conditions                                                         | Remarks                                                                                                                                                                                                    |
| ------------------------------------------------------- | ------- | ----------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 100-INITIALIZATION                                      | RL-001  | Data Assignment   | All location, contact, and operational fields must be initialized to their default values before any processing begins.                                                                                                                                   | Program start or before any processing of input data.              | Default values are typically spaces for alphanumeric fields and zero or a specified constant for numeric fields. For time fields, the default is '00:00:01'. For date fields, the default is '2024-06-01'. |
| 100-INITIALIZATION                                      | RL-002  | Data Assignment   | If the input indicates that the cursor is not being closed, all location, contact, and operational fields must be transferred from the input to the output structure.                                                                                     | EXIT-CLOSE-CURSOR = false                                          | All fields are copied as-is unless further normalization/validation is required by other rules.                                                                                                            |
| 110-MOVE-PDA-FIELDS-2-DCL (CALL YYYS0134-STRING-CRUNCH) | RL-003  | Computation       | The primary contact name must be normalized so that only single spaces separate words.                                                                                                                                                                    | Whenever the primary contact name is processed.                    | Normalization is performed by a subroutine that collapses multiple spaces into a single space. The field is alphanumeric, fixed length.                                                                    |
| 110-MOVE-PDA-FIELDS-2-DCL                               | RL-004  | Conditional Logic | All date fields must be checked. If any field is blank, contains only spaces, or contains a zero-date (e.g., '0000-00-00'), it must be replaced with the default date value.                                                                              | Date field is blank, spaces, or zero-date.                         | Default date value is '2024-06-01'. Applies to fields like ADDED-DT, DELETE-DT, OPENED-DT, CLOSED-DT, INACTIVE-DT.                                                                                         |
| 110-MOVE-PDA-FIELDS-2-DCL                               | RL-005  | Conditional Logic | All time fields must be checked. If any field is blank or contains only spaces, it must be replaced with the default time value.                                                                                                                          | Time field is blank or spaces.                                     | Default time value is '00:00:01'. Applies to fields like ORD-PROCNG-CTOF-TM, FILLER1-TM, FILLER2-TM.                                                                                                       |
| 110-MOVE-PDA-FIELDS-2-DCL                               | RL-006  | Data Assignment   | All obsolete time fields must be initialized to the default time value.                                                                                                                                                                                   | Always, during initialization or field transfer.                   | Default time value is '00:00:01'. Applies to FILLER1-TM, FILLER2-TM.                                                                                                                                       |
| 115-CONNECT-TO-ORACLE                                   | RL-007  | Conditional Logic | If the environment requires Oracle connection, the program must attempt to connect and, if unsuccessful, store the SQL error code and prepare an error message.                                                                                           | YYYN005A-ORACLE = 'O' or exit code 8/9/10.                         | Error message format: 'NNNS0487 - Error connecting to Oracle. Sqlcode = <code>'.                                                                                                                           |
| 116-EDIT-SHIP-DAYS                                      | RL-008  | Conditional Logic | For each weekday, if the corresponding character in SCH-SHP-DD-TXT is not present, the output flag DOES-NOT-SHIP-<DAY> must be set to 'Y'; otherwise, set to 'N'.                                                                                         | For each weekday, check corresponding character in SCH-SHP-DD-TXT. | Output flags are single-character fields, 'Y' or 'N'.                                                                                                                                                      |
| 112-CONVERT-TM-TO-TS, 132-CONVERT-TS-TO-TM              | RL-009  | Computation       | If the environment is Oracle or a row modification/insertion is being performed, all time fields must be converted to timestamp format using the provided date and time values. If conversion fails, the original time values must be used in the output. | YYYN005A-ORACLE or EXIT-PUT-MODIFY-ROW or EXIT-PUT-INSERT-ROW.     | Timestamp format: 'YYYY-MM-DD-HH.MM.SS.SSSSSS'. If conversion fails, use original time values.                                                                                                             |
| 110-MOVE-PDA-FIELDS-2-DCL                               | RL-010  | Conditional Logic | All numeric fields must be validated to ensure they contain only valid numeric values; if not, they must be replaced with their default value.                                                                                                            | Numeric field is not valid numeric.                                | Default value is 0 for integers. Applies to fields like LOC-ORD-PROCNG-DD, ORD-LEAD-TM-DD, ORD-BUFFER-TM-DD.                                                                                               |
| 130-MOVE-DCL-2-PDA-FIELDS, output preparation           | RL-011  | Computation       | All string fields must be space-padded to their fixed length in the output.                                                                                                                                                                               | When preparing output fields.                                      | String fields are padded with spaces to their defined length. No truncation unless specified.                                                                                                              |
| 130-MOVE-DCL-2-PDA-FIELDS, output preparation           | RL-012  | Data Assignment   | All output fields must be written to the output file in the specified format and length.                                                                                                                                                                  | When writing output.                                               | Output format and length are defined by the output structure. Fields must match these exactly.                                                                                                             |
| Timestamp conversion and validation routines            | RL-013  | Conditional Logic | All timestamp fields used for tracking updates or time-based events must be checked. If any such field is blank or contains only spaces, it must be replaced with the default timestamp value.                                                            | A timestamp field is blank or contains only spaces.                | Default timestamp value is '2024-06-01-00.00.01.000000'. Applies to all fields in the output structure that represent timestamps for updates or events.                                                    |

# User Stories

## User Story 1: Initialize and validate all input fields

---

### Story Description:

As a system, I want all location, contact, operational, date, time, numeric, and timestamp fields to be initialized and validated so that data integrity is ensured before processing begins.

---

### Business Rule Mapping:

| Rule ID | Paragraph Name                               | Rule Description                                                                                                                                                                                                                                          |
| ------- | -------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| RL-001  | 100-INITIALIZATION                           | All location, contact, and operational fields must be initialized to their default values before any processing begins.                                                                                                                                   |
| RL-004  | 110-MOVE-PDA-FIELDS-2-DCL                    | All date fields must be checked. If any field is blank, contains only spaces, or contains a zero-date (e.g., '0000-00-00'), it must be replaced with the default date value.                                                                              |
| RL-005  | 110-MOVE-PDA-FIELDS-2-DCL                    | All time fields must be checked. If any field is blank or contains only spaces, it must be replaced with the default time value.                                                                                                                          |
| RL-006  | 110-MOVE-PDA-FIELDS-2-DCL                    | All obsolete time fields must be initialized to the default time value.                                                                                                                                                                                   |
| RL-010  | 110-MOVE-PDA-FIELDS-2-DCL                    | All numeric fields must be validated to ensure they contain only valid numeric values; if not, they must be replaced with their default value.                                                                                                            |
| RL-009  | 112-CONVERT-TM-TO-TS, 132-CONVERT-TS-TO-TM   | If the environment is Oracle or a row modification/insertion is being performed, all time fields must be converted to timestamp format using the provided date and time values. If conversion fails, the original time values must be used in the output. |
| RL-013  | Timestamp conversion and validation routines | All timestamp fields used for tracking updates or time-based events must be checked. If any such field is blank or contains only spaces, it must be replaced with the default timestamp value.                                                            |

---

### Relevant Functionality:

- **100-INITIALIZATION**
  1. **RL-001:**
     - On program start:
       - Set all location, contact, and operational fields to their default values.
       - Use INITIALIZE statement for structures.
       - Set specific fields to their documented default values.
- **110-MOVE-PDA-FIELDS-2-DCL**
  1. **RL-004:**
     - For each date field:
       - If field is blank, spaces, or zero-date:
         - Set field to '2024-06-01'.
  2. **RL-005:**
     - For each time field:
       - If field is blank or spaces:
         - Set field to '00:00:01'.
  3. **RL-006:**
     - Set FILLER1-TM and FILLER2-TM to '00:00:01'.
  4. **RL-010:**
     - For each numeric field:
       - If not numeric:
         - Set field to 0.
- **112-CONVERT-TM-TO-TS**
  1. **RL-009:**
     - If Oracle or row modification/insertion:
       - Convert time fields to timestamp format.
       - If conversion fails:
         - Use original time values in output.
- **Timestamp conversion and validation routines**
  1. **RL-013:**
     - For each timestamp field used for tracking updates or events:
       - If the field is blank or contains only spaces:
         - Set the field to '2024-06-01-00.00.01.000000'.

## User Story 2: Transfer and normalize input fields

---

### Story Description:

As a system, I want to transfer all location, contact, and operational fields from input to output and normalize the primary contact name so that the output structure contains accurate and standardized data.

---

### Business Rule Mapping:

| Rule ID | Paragraph Name                                          | Rule Description                                                                                                                                                      |
| ------- | ------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| RL-002  | 100-INITIALIZATION                                      | If the input indicates that the cursor is not being closed, all location, contact, and operational fields must be transferred from the input to the output structure. |
| RL-003  | 110-MOVE-PDA-FIELDS-2-DCL (CALL YYYS0134-STRING-CRUNCH) | The primary contact name must be normalized so that only single spaces separate words.                                                                                |

---

### Relevant Functionality:

- **100-INITIALIZATION**
  1. **RL-002:**
     - If EXIT-CLOSE-CURSOR is false:
       - Move all relevant fields from input structure to output structure.
- **110-MOVE-PDA-FIELDS-2-DCL (CALL YYYS0134-STRING-CRUNCH)**
  1. **RL-003:**
     - Determine length of primary contact name.
     - Call space cruncher subroutine with the name and its length.
     - Replace the original field with the normalized result.

## User Story 3: Prepare and write output fields, including Oracle error handling and shipping days

---

### Story Description:

As a user, I want all output fields to be written in the specified format and length, including space-padding string fields, handling Oracle connection errors, and determining shipping days for each weekday so that the output file meets required specifications and contains all necessary operational information.

---

### Business Rule Mapping:

| Rule ID | Paragraph Name                                | Rule Description                                                                                                                                                  |
| ------- | --------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| RL-008  | 116-EDIT-SHIP-DAYS                            | For each weekday, if the corresponding character in SCH-SHP-DD-TXT is not present, the output flag DOES-NOT-SHIP-<DAY> must be set to 'Y'; otherwise, set to 'N'. |
| RL-007  | 115-CONNECT-TO-ORACLE                         | If the environment requires Oracle connection, the program must attempt to connect and, if unsuccessful, store the SQL error code and prepare an error message.   |
| RL-011  | 130-MOVE-DCL-2-PDA-FIELDS, output preparation | All string fields must be space-padded to their fixed length in the output.                                                                                       |
| RL-012  | 130-MOVE-DCL-2-PDA-FIELDS, output preparation | All output fields must be written to the output file in the specified format and length.                                                                          |

---

### Relevant Functionality:

- **116-EDIT-SHIP-DAYS**
  1. **RL-008:**
     - For each weekday:
       - If not present in SCH-SHP-DD-TXT:
         - Set DOES-NOT-SHIP-<DAY> to 'Y'.
       - Else:
         - Set DOES-NOT-SHIP-<DAY> to 'N'.
- **115-CONNECT-TO-ORACLE**
  1. **RL-007:**
     - If Oracle connection required:
       - Attempt to connect.
       - If unsuccessful:
         - Store SQL error code.
         - Prepare error message.
- **130-MOVE-DCL-2-PDA-FIELDS**
  1. **RL-011:**
     - For each string field in output:
       - Pad with spaces to fixed length.
  2. **RL-012:**
     - For each output field:
       - Write field in specified format and length.

# Code Walkthrough

## Starting Up and Preparing Location Data

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Initialize all location and environment fields"] --> node2{"EXIT-CLOSE-CURSOR = false?"}
    click node1 openCode "base/src/NNNS0487.cbl:894:903"
    node2 -->|"Yes"| node3["Transferring and Normalizing Location Fields"]
    click node2 openCode "base/src/NNNS0487.cbl:903:905"
    
    node2 -->|"No"| node4{"Oracle connection needed? (YYYN005A-ORACLE = 'O' or exit code 8/9/10)"}
    node3 --> node4
    node4 -->|"Yes"| node5["Connecting to Oracle"]
    click node4 openCode "base/src/NNNS0487.cbl:906:909"
    
    node4 -->|"No"| node5
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
click node3 goToHeading "Transferring and Normalizing Location Fields"
node3:::HeadingStyle
click node5 goToHeading "Connecting to Oracle"
node5:::HeadingStyle
```

<SwmSnippet path="/base/src/NNNS0487.cbl" line="894">

---

100-INITIALIZATION resets all the working fields and status flags, then calls 110-MOVE-PDA-FIELDS-2-DCL to prep and normalize location data if we're not just closing the cursor.

```cobol
097100 100-INITIALIZATION.                                              00097100
097200     INITIALIZE XXXN001A                                          00097200
097300                DAO-STATUS                                        00097300
097400                MMMC0474                                          00097400
097500                ZZZC0550                                          00097500
097600     MOVE NNNN0000-INDEX-HANDLE TO DDDTLO01-INDEX-HANDLE          00097600
097700     MOVE 0 TO WS-CHECKPOINT-INC                                  00097700
097800     MOVE 0 TO SQLCODE                                            00097800
097900     MOVE 0 TO SQL-INIT-FLAG                                      00097900
098000     IF NOT EXIT-CLOSE-CURSOR                                     00098000
098100       PERFORM 110-MOVE-PDA-FIELDS-2-DCL                          00098100
098200     END-IF                                                       00098200
098300     IF (YYYN005A-ORACLE       OR EXIT-PUT-INSERT-ROW             00098300
098400         OR EXIT-PUT-PURGE-ROW OR EXIT-PUT-MODIFY-ROW)            00098400
098500       PERFORM 115-CONNECT-TO-ORACLE                              00098500
098600     END-IF                                                       00098600
098700     .                                                            00098700
```

---

</SwmSnippet>

### Transferring and Normalizing Location Fields

<SwmSnippet path="/base/src/NNNS0487.cbl" line="918">

---

110-MOVE-PDA-FIELDS-2-DCL handles copying and normalizing all location, contact, and operational fields from the source to the destination structure. It starts by calling YYYS0134-STRING-CRUNCH to clean up extra spaces in PRIM-CONTACT-NM, then moves all fields, defaults dates, sets flags based on business rules, validates numeric fields, and initializes obsolete fields. It wraps up by calling 116-EDIT-SHIP-DAYS to set shipping day flags and 112-CONVERT-TM-TO-TS to handle time-to-timestamp conversion, prepping the data for further use.

```cobol
099500 110-MOVE-PDA-FIELDS-2-DCL.                                       00099500
099600     MOVE LENGTH OF PRIM-CONTACT-NM OF P-DDDTLO01 TO WS-STR-LEN   00099600
099700     CALL  YYYS0134-STRING-CRUNCH USING                           00099700
099800                            XXXN001A                              00099800
099900                            PRIM-CONTACT-NM OF P-DDDTLO01         00099900
100000                            WS-STR-LEN                            00100000
100100     INITIALIZE XXXN001A                                          00100100
100200     MOVE LOC-TYP-CD OF P-DDDTLO01 TO LOC-TYP-CD OF DCLXXXATION   00100200
100300     MOVE LOC-NBR OF P-DDDTLO01 TO LOC-NBR OF DCLXXXATION         00100300
100400     MOVE LOC-NM OF P-DDDTLO01 TO LOC-NM OF DCLXXXATION           00100400
100500     MOVE LOC-ABB OF P-DDDTLO01 TO LOC-ABB OF DCLXXXATION         00100500
100600     MOVE LGL-LOC-NAM OF P-DDDTLO01 TO LGL-LOC-NAM OF DCLXXXATION 00100600
100700     MOVE PRIM-CONTACT-NM OF P-DDDTLO01                           00100700
100800       TO PRIM-CONTACT-NM OF DCLXXXATION                          00100800
100900     MOVE PRIM-ADR-1 OF P-DDDTLO01 TO PRIM-ADR-1 OF DCLXXXATION   00100900
101000     MOVE PRIM-ADR-2 OF P-DDDTLO01 TO PRIM-ADR-2 OF DCLXXXATION   00101000
101100     MOVE PRIM-ADR-3 OF P-DDDTLO01 TO PRIM-ADR-3 OF DCLXXXATION   00101100
101200     MOVE PRIM-ADR-4 OF P-DDDTLO01 TO PRIM-ADR-4 OF DCLXXXATION   00101200
101300     MOVE PRIM-CITY OF P-DDDTLO01 TO PRIM-CITY OF DCLXXXATION     00101300
101400     MOVE PRIM-CITY-ID OF P-DDDTLO01                              00101400
101500       TO PRIM-CITY-ID OF DCLXXXATION                             00101500
101600     MOVE PRIM-STATE-CD OF P-DDDTLO01                             00101600
101700       TO PRIM-STATE-CD OF DCLXXXATION                            00101700
101800     MOVE PRIM-ZIP5-CD OF P-DDDTLO01                              00101800
101900       TO PRIM-ZIP5-CD OF DCLXXXATION                             00101900
102000     MOVE PRIM-ZIP4-CD OF P-DDDTLO01                              00102000
102100       TO PRIM-ZIP4-CD OF DCLXXXATION                             00102100
102200     MOVE PRIM-PHN-CNTRY-CD OF P-DDDTLO01                         00102200
102300       TO PRIM-PHN-CNTRY-CD OF DCLXXXATION                        00102300
102400     MOVE PRIM-AREA-CD OF P-DDDTLO01                              00102400
102500       TO PRIM-AREA-CD OF DCLXXXATION                             00102500
102600     MOVE PRIM-PHONE-NBR OF P-DDDTLO01                            00102600
102700       TO PRIM-PHONE-NBR OF DCLXXXATION                           00102700
102800     MOVE PRIM-CNTRY-NM OF P-DDDTLO01                             00102800
102900       TO PRIM-CNTRY-NM OF DCLXXXATION                            00102900
103000     MOVE PRIM-CNTRY-ABB OF P-DDDTLO01                            00103000
103100       TO PRIM-CNTRY-ABB OF DCLXXXATION                           00103100
103200     MOVE SEC-LOC-NM OF P-DDDTLO01 TO SEC-LOC-NM OF DCLXXXATION   00103200
103300     MOVE SEC-CONTACT-NM OF P-DDDTLO01                            00103300
103400       TO SEC-CONTACT-NM OF DCLXXXATION                           00103400
103500     MOVE SEC-ADR-1 OF P-DDDTLO01 TO SEC-ADR-1 OF DCLXXXATION     00103500
103600     MOVE SEC-ADR-2 OF P-DDDTLO01 TO SEC-ADR-2 OF DCLXXXATION     00103600
103700     MOVE SEC-ADR-3 OF P-DDDTLO01 TO SEC-ADR-3 OF DCLXXXATION     00103700
103800     MOVE SEC-ADR-4 OF P-DDDTLO01 TO SEC-ADR-4 OF DCLXXXATION     00103800
103900     MOVE SEC-CITY OF P-DDDTLO01 TO SEC-CITY OF DCLXXXATION       00103900
104000     MOVE SEC-STATE-CD OF P-DDDTLO01                              00104000
104100       TO SEC-STATE-CD OF DCLXXXATION                             00104100
104200     MOVE SEC-ZIP5-CD OF P-DDDTLO01 TO SEC-ZIP5-CD OF DCLXXXATION 00104200
104300     MOVE SEC-ZIP4-CD OF P-DDDTLO01 TO SEC-ZIP4-CD OF DCLXXXATION 00104300
104400     MOVE SEC-PHN-CNTRY-CD OF P-DDDTLO01                          00104400
104500       TO SEC-PHN-CNTRY-CD OF DCLXXXATION                         00104500
104600     MOVE SEC-AREA-CD OF P-DDDTLO01 TO SEC-AREA-CD OF DCLXXXATION 00104600
104700     MOVE SEC-PHONE-NBR OF P-DDDTLO01                             00104700
104800       TO SEC-PHONE-NBR OF DCLXXXATION                            00104800
104900     MOVE SEC-CNTRY-NM OF P-DDDTLO01                              00104900
105000       TO SEC-CNTRY-NM OF DCLXXXATION                             00105000
105100     MOVE SEC-CNTRY-ABB OF P-DDDTLO01                             00105100
105200       TO SEC-CNTRY-ABB OF DCLXXXATION                            00105200
105300     MOVE MAIL-TO-LOC-NM OF P-DDDTLO01                            00105300
105400       TO MAIL-TO-LOC-NM OF DCLXXXATION                           00105400
105500     MOVE MAIL-TO-CNTCT-NM OF P-DDDTLO01                          00105500
105600       TO MAIL-TO-CNTCT-NM OF DCLXXXATION                         00105600
105700     MOVE MAIL-TO-ADR-1 OF P-DDDTLO01                             00105700
105800       TO MAIL-TO-ADR-1 OF DCLXXXATION                            00105800
105900     MOVE MAIL-TO-ADR-2 OF P-DDDTLO01                             00105900
106000       TO MAIL-TO-ADR-2 OF DCLXXXATION                            00106000
106100     MOVE MAIL-TO-ADR-3 OF P-DDDTLO01                             00106100
106200       TO MAIL-TO-ADR-3 OF DCLXXXATION                            00106200
106300     MOVE MAIL-TO-ADR-4 OF P-DDDTLO01                             00106300
106400       TO MAIL-TO-ADR-4 OF DCLXXXATION                            00106400
106500     MOVE MAIL-TO-CITY OF P-DDDTLO01                              00106500
106600       TO MAIL-TO-CITY OF DCLXXXATION                             00106600
106700     MOVE MAIL-TO-STATE-CD OF P-DDDTLO01                          00106700
106800       TO MAIL-TO-STATE-CD OF DCLXXXATION                         00106800
106900     MOVE MAIL-TO-ZIP5-CD OF P-DDDTLO01                           00106900
107000       TO MAIL-TO-ZIP5-CD OF DCLXXXATION                          00107000
107100     MOVE MAIL-TO-ZIP4-CD OF P-DDDTLO01                           00107100
107200       TO MAIL-TO-ZIP4-CD OF DCLXXXATION                          00107200
107300     MOVE MAIL-PHN-CNTRY-CD OF P-DDDTLO01                         00107300
107400       TO MAIL-PHN-CNTRY-CD OF DCLXXXATION                        00107400
107500     MOVE MAIL-TO-AREA-CD OF P-DDDTLO01                           00107500
107600       TO MAIL-TO-AREA-CD OF DCLXXXATION                          00107600
107700     MOVE MAIL-TO-PHONE-NBR OF P-DDDTLO01                         00107700
107800       TO MAIL-TO-PHONE-NBR OF DCLXXXATION                        00107800
107900     MOVE MAIL-TO-CNTRY-NM OF P-DDDTLO01                          00107900
108000       TO MAIL-TO-CNTRY-NM OF DCLXXXATION                         00108000
108100     MOVE MAIL-TO-CNTRY-AB OF P-DDDTLO01                          00108100
108200       TO MAIL-TO-CNTRY-AB OF DCLXXXATION                         00108200
108300     MOVE CURR-FAX-ID OF P-DDDTLO01 TO CURR-FAX-ID OF DCLXXXATION 00108300
108400                                                                  00108400
108500     IF ADDED-DT OF P-DDDTLO01 = SPACES                           00108500
108600     OR ADDED-DT OF P-DDDTLO01 = K-ZERO-DT                        00108600
108700       MOVE K-DEF-DT TO ADDED-DT OF P-DDDTLO01                    00108700
108800     END-IF                                                       00108800
108900     MOVE ADDED-DT OF P-DDDTLO01 TO ADDED-DT OF DCLXXXATION       00108900
109000                                                                  00109000
109100     IF DELETE-DT OF P-DDDTLO01 = SPACES                          00109100
109200     OR DELETE-DT OF P-DDDTLO01 = K-ZERO-DT                       00109200
109300       MOVE K-DEF-DT TO DELETE-DT OF P-DDDTLO01                   00109300
109400     END-IF                                                       00109400
109500     MOVE DELETE-DT OF P-DDDTLO01 TO DELETE-DT OF DCLXXXATION     00109500
109600                                                                  00109600
109700     IF INACTIVE-DT OF P-DDDTLO01 = SPACES                        00109700
109800     OR INACTIVE-DT OF P-DDDTLO01 = K-ZERO-DT                     00109800
109900       MOVE K-DEF-DT TO INACTIVE-DT OF P-DDDTLO01                 00109900
110000     END-IF                                                       00110000
110100     MOVE INACTIVE-DT OF P-DDDTLO01 TO INACTIVE-DT OF DCLXXXATION 00110100
110200                                                                  00110200
110300     IF OPENED-DT OF P-DDDTLO01 = SPACES                          00110300
110400     OR OPENED-DT OF P-DDDTLO01 = K-ZERO-DT                       00110400
110500       MOVE K-DEF-DT TO OPENED-DT OF P-DDDTLO01                   00110500
110600     END-IF                                                       00110600
110700     MOVE OPENED-DT OF P-DDDTLO01 TO OPENED-DT OF DCLXXXATION     00110700
110800                                                                  00110800
110900     IF CLOSED-DT OF P-DDDTLO01 = SPACES                          00110900
111000     OR CLOSED-DT OF P-DDDTLO01 = K-ZERO-DT                       00111000
111100       MOVE K-DEF-DT TO CLOSED-DT OF P-DDDTLO01                   00111100
111200     END-IF                                                       00111200
111300     MOVE CLOSED-DT OF P-DDDTLO01 TO CLOSED-DT OF DCLXXXATION     00111300
111400                                                                  00111400
111500     IF NOT LO-INACTIVE AND NOT LO-DELETED                        00111500
111600       SET LO-ACTIVE TO TRUE                                      00111600
111700     END-IF                                                       00111700
111800                                                                  00111800
111900     EVALUATE TRUE                                                00111900
112000       WHEN LOC-TYP-CD OF DCLXXXATION = K-VEND-LOC-TYPE           00112000
112100         MOVE K-AP-TYPE-CD TO AP-TYP-CD OF P-DDDTLO01             00112100
112200                                                                  00112200
112300       WHEN LOC-TYP-CD OF DCLXXXATION = K-STORE-LOC-TYPE          00112300
112400         MOVE 0      TO AP-NBR    OF P-DDDTLO01                   00112400
112500         MOVE SPACES TO AP-TYP-CD OF P-DDDTLO01                   00112500
112600                                                                  00112600
112700       WHEN LOC-TYP-CD OF DCLXXXATION = K-DSD-VEND-LOC-TYPE       00112700
112800         MOVE K-DSD-AP-TYPE-CD TO AP-TYP-CD OF P-DDDTLO01         00112800
112900         IF INACTIVE-DT OF P-DDDTLO01 = K-DEF-DT                  00112900
113000           SET LO-ACTIVE TO TRUE                                  00113000
113100         ELSE                                                     00113100
113200           SET LO-INACTIVE TO TRUE                                00113200
113300         END-IF                                                   00113300
113400     END-EVALUATE                                                 00113400
113500                                                                  00113500
113600     MOVE INACTIVE-SW OF P-DDDTLO01 TO INACTIVE-SW OF DCLXXXATION 00113600
113700                                                                  00113700
113800     MOVE AP-NBR OF P-DDDTLO01 TO AP-NBR OF DCLXXXATION           00113800
113900     MOVE AP-TYP-CD OF P-DDDTLO01   TO AP-TYP-CD OF DCLXXXATION   00113900
114000                                                                  00114000
114100     MOVE LST-UPDT-TS OF P-DDDTLO01 TO LST-UPDT-TS OF DCLXXXATION 00114100
114200     MOVE LST-UPDT-USR-ID OF P-DDDTLO01                           00114200
114300       TO LST-UPDT-USR-ID OF DCLXXXATION                          00114300
114400     MOVE PRIM-EMAIL-ID    OF P-DDDTLO01                          00114400
114500       TO PRIM-EMAIL-ID    OF DCLXXXATION                         00114500
114600     MOVE SECY-EMAIL-ID    OF P-DDDTLO01                          00114600
114700       TO SECY-EMAIL-ID    OF DCLXXXATION                         00114700
114800     MOVE MAIL-TO-EMAIL-ID OF P-DDDTLO01                          00114800
114900       TO MAIL-TO-EMAIL-ID OF DCLXXXATION                         00114900
115000     IF FAC-ID-X = SPACES                                         00115000
115100       MOVE 0 TO FAC-ID OF P-DDDTLO01                             00115100
115200     END-IF                                                       00115200
115300     MOVE FAC-ID           OF P-DDDTLO01                          00115300
115400       TO FAC-ID           OF DCLXXXATION                         00115400
115500     IF ORG-ID-X = SPACES                                         00115500
115600       MOVE 0 TO ORG-ID OF P-DDDTLO01                             00115600
115700     END-IF                                                       00115700
115800     MOVE ORG-ID           OF P-DDDTLO01                          00115800
115900       TO ORG-ID           OF DCLXXXATION                         00115900
116000     MOVE B2B-PRIM-RTNG-ID OF P-DDDTLO01                          00116000
116100       TO B2B-PRIM-RTNG-ID OF DCLXXXATION                         00116100
116200     MOVE PRIM-CNTY-TXT    OF P-DDDTLO01                          00116200
116300       TO PRIM-CNTY-TXT    OF DCLXXXATION                         00116300
116400     MOVE SECY-CNTY-TXT    OF P-DDDTLO01                          00116400
116500       TO SECY-CNTY-TXT    OF DCLXXXATION                         00116500
116600     MOVE MAIL-TO-CNTY-TXT OF P-DDDTLO01                          00116600
116700       TO MAIL-TO-CNTY-TXT OF DCLXXXATION                         00116700
116800                                                                  00116800
116900     IF NOT LOC-IS-DIRECT-SHIP OF P-DDDTLO01                      00116900
117000       SET LOC-IS-NOT-DIRECT-SHIP  OF P-DDDTLO01 TO TRUE          00117000
117100     END-IF                                                       00117100
117200     MOVE DIR-SHP-LOC-SW     OF P-DDDTLO01                        00117200
117300       TO DIR-SHP-LOC-SW     OF DCLXXXATION                       00117300
117400                                                                  00117400
117500     IF LOC-ORD-PROCNG-DD    OF P-DDDTLO01 NOT NUMERIC            00117500
117600        MOVE 0 TO LOC-ORD-PROCNG-DD OF P-DDDTLO01                 00117600
117700     END-IF                                                       00117700
117800     MOVE LOC-ORD-PROCNG-DD  OF P-DDDTLO01                        00117800
117900       TO LOC-ORD-PROCNG-DD  OF DCLXXXATION                       00117900
118000                                                                  00118000
118100     PERFORM 116-EDIT-SHIP-DAYS                                   00118100
118200     MOVE SCH-SHP-DD-TXT     OF P-DDDTLO01                        00118200
118300       TO SCH-SHP-DD-TXT     OF DCLXXXATION                       00118300
118400                                                                  00118400
118500                                                                  00118500
118600     IF ORD-LEAD-TM-DD OF P-DDDTLO01 IS NOT NUMERIC               00118600
118700        MOVE 0 TO ORD-LEAD-TM-DD OF P-DDDTLO01                    00118700
118800     END-IF                                                       00118800
118900     MOVE ORD-LEAD-TM-DD   OF P-DDDTLO01                          00118900
119000       TO ORD-LEAD-TM-DD   OF DCLXXXATION                         00119000
119100                                                                  00119100
119200     IF ORD-BUFFER-TM-DD OF P-DDDTLO01 IS NOT NUMERIC             00119200
119300        MOVE 0 TO ORD-BUFFER-TM-DD OF P-DDDTLO01                  00119300
119400     END-IF                                                       00119400
119500     MOVE ORD-BUFFER-TM-DD   OF P-DDDTLO01                        00119500
119600       TO ORD-BUFFER-TM-DD   OF DCLXXXATION                       00119600
119700                                                                  00119700
119800** Obsolete fields - Order Lead time and Buffer time renamed      00119800
119900** to FILLER1-TM and FILLER2-TM respectively.                     00119900
120000     MOVE WS-NULL-TM TO FILLER1-TM  OF DCLXXXATION                00120000
120100                        FILLER2-TM  OF DCLXXXATION                00120100
120200** Obsolete fields                                                00120200
120300                                                                  00120300
120400     IF ORD-PROCNG-CTOF-TM OF P-DDDTLO01 = SPACES                 00120400
120500       MOVE WS-NULL-TM TO ORD-PROCNG-CTOF-TM OF P-DDDTLO01        00120500
120600     END-IF                                                       00120600
120700     MOVE ORD-PROCNG-CTOF-TM OF P-DDDTLO01                        00120700
120800       TO ORD-PROCNG-CTOF-TM OF DCLXXXATION                       00120800
120900                                                                  00120900
121000     PERFORM 112-CONVERT-TM-TO-TS                                 00121000
121100     .                                                            00121100
```

---

</SwmSnippet>

#### Cleaning Up and Outputting the String

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Start: Initialize and validate input string"] --> node2{"Is input string valid (not empty or only spaces/low-values)?"}
    click node1 openCode "base/src/YYYS0134.cbl:62:77"
    node2 -->|"Yes"| node3{"Initialization successful (SUCCESS=0)?"}
    click node2 openCode "base/src/YYYS0134.cbl:68:76"
    node2 -->|"No"| node6["Return error: No text to process"]
    click node6 openCode "base/src/YYYS0134.cbl:72:76"
    node3 -->|"Yes"| node4["Clean string: Remove extra spaces"]
    click node3 openCode "base/src/YYYS0134.cbl:49:51"
    node3 -->|"No"| node11["End"]
    click node4 openCode "base/src/YYYS0134.cbl:84:99"
    subgraph loop1["For each character in string (1 to STR-LEN)"]
        node4 --> node5{"Are current and previous characters both spaces?"}
        click node5 openCode "base/src/YYYS0134.cbl:88:96"
        node5 -->|"Yes"| node7["Skip character"]
        click node7 openCode "base/src/YYYS0134.cbl:89:89"
        node5 -->|"No"| node8["Copy character to output"]
        click node8 openCode "base/src/YYYS0134.cbl:91:93"
        node7 --> node4
        node8 --> node4
    end
    node4 --> node9{"Cleaning successful (SUCCESS=0)?"}
    click node9 openCode "base/src/YYYS0134.cbl:52:54"
    node9 -->|"Yes"| node10["Send cleaned string"]
    click node10 openCode "base/src/YYYS0134.cbl:102:105"
    node9 -->|"No"| node11["End"]
    node10 --> node11["End"]
    node6 --> node11
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

<SwmSnippet path="/base/src/YYYS0134.cbl" line="47">

---

000-MAIN validates the input, crunches spaces, and outputs the cleaned string, stopping if any step fails.

```cobol
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
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/YYYS0134.cbl" line="62">

---

100-INITIALIZATION validates the input string and length, and stops the flow with an error if the input is empty or invalid.

```cobol
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
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/YYYS0134.cbl" line="84">

---

200-CRUNCH-STRING loops through the input string, copying each character to the output unless it's a consecutive space. It tracks the previous character to skip extra spaces, so the result is a string with single spaces between words.

```cobol
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
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/YYYS0134.cbl" line="102">

---

300-SEND-FINAL-STRING clears the target substring in STR, then overwrites it with the processed output string. This guarantees the final output is clean and doesn't contain any old data.

```cobol
036600 300-SEND-FINAL-STRING.                                           00036600
036700     MOVE SPACES                      TO STR(1:STR-LEN)           00036700
036800     MOVE WS-OT-STR                   TO STR(1:STR-LEN)           00036800
037000     .                                                            00037000
```

---

</SwmSnippet>

#### Marking Non-Shipping Days

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Start: Check shipping days"]
    click node1 openCode "base/src/NNNS0487.cbl:1190:1212"
    subgraph loop1["For each day: Monday to Sunday"]
      node1 --> node2{"Is shipping available on Monday?"}
      click node2 openCode "base/src/NNNS0487.cbl:1191:1193"
      node2 -->|"No"| node3["Mark Monday as 'does not ship'"]
      click node3 openCode "base/src/NNNS0487.cbl:1192:1193"
      node2 -->|"Yes"| node4
      node4 --> node5{"Is shipping available on Tuesday?"}
      click node5 openCode "base/src/NNNS0487.cbl:1194:1196"
      node5 -->|"No"| node6["Mark Tuesday as 'does not ship'"]
      click node6 openCode "base/src/NNNS0487.cbl:1195:1196"
      node5 -->|"Yes"| node7
      node7 --> node8{"Is shipping available on Wednesday?"}
      click node8 openCode "base/src/NNNS0487.cbl:1197:1199"
      node8 -->|"No"| node9["Mark Wednesday as 'does not ship'"]
      click node9 openCode "base/src/NNNS0487.cbl:1198:1199"
      node8 -->|"Yes"| node10
      node10 --> node11{"Is shipping available on Thursday?"}
      click node11 openCode "base/src/NNNS0487.cbl:1200:1202"
      node11 -->|"No"| node12["Mark Thursday as 'does not ship'"]
      click node12 openCode "base/src/NNNS0487.cbl:1201:1202"
      node11 -->|"Yes"| node13
      node13 --> node14{"Is shipping available on Friday?"}
      click node14 openCode "base/src/NNNS0487.cbl:1203:1205"
      node14 -->|"No"| node15["Mark Friday as 'does not ship'"]
      click node15 openCode "base/src/NNNS0487.cbl:1204:1205"
      node14 -->|"Yes"| node16
      node16 --> node17{"Is shipping available on Saturday?"}
      click node17 openCode "base/src/NNNS0487.cbl:1206:1208"
      node17 -->|"No"| node18["Mark Saturday as 'does not ship'"]
      click node18 openCode "base/src/NNNS0487.cbl:1207:1208"
      node17 -->|"Yes"| node19
      node19 --> node20{"Is shipping available on Sunday?"}
      click node20 openCode "base/src/NNNS0487.cbl:1209:1211"
      node20 -->|"No"| node21["Mark Sunday as 'does not ship'"]
      click node21 openCode "base/src/NNNS0487.cbl:1210:1211"
      node20 -->|"Yes"| node22["End"]
      click node22 openCode "base/src/NNNS0487.cbl:1212:1212"
    end
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

<SwmSnippet path="/base/src/NNNS0487.cbl" line="1190">

---

116-EDIT-SHIP-DAYS loops through each weekday, and if shipping isn't scheduled, sets a flag to mark that day as non-shipping. This makes it clear which days are off for shipments and simplifies later checks.

```cobol
126700 116-EDIT-SHIP-DAYS.                                              00126700
126800     IF NOT SHIPS-MON                                             00126800
126900       SET DOES-NOT-SHIP-MON TO TRUE                              00126900
127000     END-IF                                                       00127000
127100     IF NOT SHIPS-TUE                                             00127100
127200       SET DOES-NOT-SHIP-TUE TO TRUE                              00127200
127300     END-IF                                                       00127300
127400     IF NOT SHIPS-WED                                             00127400
127500       SET DOES-NOT-SHIP-WED TO TRUE                              00127500
127600     END-IF                                                       00127600
127700     IF NOT SHIPS-THU                                             00127700
127800       SET DOES-NOT-SHIP-THU TO TRUE                              00127800
127900     END-IF                                                       00127900
128000     IF NOT SHIPS-FRI                                             00128000
128100       SET DOES-NOT-SHIP-FRI TO TRUE                              00128100
128200     END-IF                                                       00128200
128300     IF NOT SHIPS-SAT                                             00128300
128400       SET DOES-NOT-SHIP-SAT TO TRUE                              00128400
128500     END-IF                                                       00128500
128600     IF NOT SHIPS-SUN                                             00128600
128700       SET DOES-NOT-SHIP-SUN TO TRUE                              00128700
128800     END-IF                                                       00128800
128900     .                                                            00128900
```

---

</SwmSnippet>

### Converting Times to Timestamps

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1{"Oracle environment or row modification/insertion?"}
    click node1 openCode "base/src/NNNS0487.cbl:1217:1218"
    node1 -->|"Yes"| node2["Convert time to timestamp"]
    click node2 openCode "base/src/NNNS0487.cbl:1219:1232"
    node2 --> node3{"Conversion succeeded?"}
    click node3 openCode "base/src/NNNS0487.cbl:1234:1234"
    node3 -->|"Yes"| node4["Use converted timestamp values"]
    click node4 openCode "base/src/NNNS0487.cbl:1235:1240"
    node3 -->|"No"| node5["Fallback: Use original time values from DCLXXXATION"]
    click node5 openCode "base/src/NNNS0487.cbl:1243:1246"
    node1 -->|"No"| node5
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

<SwmSnippet path="/base/src/NNNS0487.cbl" line="1139">

---

112-CONVERT-TM-TO-TS checks if we're working with Oracle or doing row modifications/inserts, then sets up the time fields and calls MMMS0291 to convert them to timestamps. If conversion succeeds, it updates the output fields; if not, it just copies the original times.

```cobol
121600 112-CONVERT-TM-TO-TS.                                            00121600
121700     IF (YYYN005A-ORACLE OR EXIT-PUT-MODIFY-ROW                   00121700
121800         OR EXIT-PUT-INSERT-ROW)                                  00121800
121900       INITIALIZE MMMC0291-INPUT-TM                               00121900
122000                  MMMC0291-INPUT-TS                               00122000
122100                                                                  00122100
122200       MOVE ORD-PROCNG-CTOF-TM OF DCLXXXATION                     00122200
122300         TO WS-TIME-INOUT-CONV(1)                                 00122300
122400       MOVE FILLER1-TM  OF DCLXXXATION                            00122400
122500         TO WS-TIME-INOUT-CONV(2)                                 00122500
122600       MOVE FILLER2-TM  OF DCLXXXATION                            00122600
122700         TO WS-TIME-INOUT-CONV(3)                                 00122700
122800                                                                  00122800
122900       SET  MMMC0291-CVT-TM-TO-TS  TO TRUE                        00122900
123000       CALL WS-MMMS0291-PGM USING                                 00123000
123100                          XXXN001A                                00123100
123200                          MMMC0291                                00123200
123300                                                                  00123300
123400       IF SUCCESS                                                 00123400
123500         MOVE WS-TIMSTAMP-INOUT-CONV(1)                           00123500
123600           TO WS-ORD-PROCNG-CTOF-TM                               00123600
123700         MOVE WS-TIMSTAMP-INOUT-CONV(2)                           00123700
123800           TO WS-FILLER1-TS                                       00123800
123900         MOVE WS-TIMSTAMP-INOUT-CONV(3)                           00123900
124000           TO WS-FILLER2-TS                                       00124000
124100       END-IF                                                     00124100
124200     ELSE                                                         00124200
124300       MOVE ORD-PROCNG-CTOF-TM OF DCLXXXATION                     00124300
124400         TO WS-ORD-PROCNG-CTOF-TM                                 00124400
124500       MOVE FILLER1-TM  OF DCLXXXATION TO WS-FILLER1-TS           00124500
124600       MOVE FILLER2-TM  OF DCLXXXATION TO WS-FILLER2-TS           00124600
124700     END-IF                                                       00124700
124800     .                                                            00124800
```

---

</SwmSnippet>

### Time Conversion Dispatcher

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Start: Initialize process"]
    click node1 openCode "base/src/MMMS0291.cbl:59:60"
    node1 --> node2{"Requested conversion?"}
    click node2 openCode "base/src/MMMS0291.cbl:61:66"
    node2 -->|"TM2TS"| node3["Convert time values to timestamps"]
    node2 -->|"TS2TM"| node4["Convert timestamp values to times"]
    node2 -->|"Other"| node5["Set FAILURE, return error message"]
    click node5 openCode "base/src/MMMS0291.cbl:67:69"
    subgraph loop1["For each value in batch"]
      node3 --> node7{"Is value valid?"}
      click node3 openCode "base/src/MMMS0291.cbl:105:137"
      click node7 openCode "base/src/MMMS0291.cbl:106:126"
      node7 -->|"Yes"| node8["Convert and store result"]
      click node8 openCode "base/src/MMMS0291.cbl:122:125"
      node7 -->|"No"| node9["Set FAILURE, store error message"]
      click node9 openCode "base/src/MMMS0291.cbl:127:132"
    end
    subgraph loop2["For each value in batch"]
      node4 --> node10{"Is value valid?"}
      click node4 openCode "base/src/MMMS0291.cbl:145:173"
      click node10 openCode "base/src/MMMS0291.cbl:146:162"
      node10 -->|"Yes"| node11["Convert and store result"]
      click node11 openCode "base/src/MMMS0291.cbl:161:161"
      node10 -->|"No"| node12["Set FAILURE, store error message"]
      click node12 openCode "base/src/MMMS0291.cbl:163:168"
    end
    node3 --> node6["Finish"]
    node4 --> node6
    node5 --> node6
    click node6 openCode "base/src/MMMS0291.cbl:73:74"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

<SwmSnippet path="/base/src/MMMS0291.cbl" line="51">

---

000-MAIN in MMMS0291 looks at the function code and runs the right conversion routine—either time to timestamp or the reverse. If the code isn't recognized, it sets an error and message.

```cobol
005800 000-MAIN.                                                        00005800
005900     PERFORM 100-INITIALIZE                                       00005900
006000                                                                  00006000
006100     EVALUATE TRUE                                                00006100
006200       WHEN MMMC0291-CVT-TM-TO-TS                                 00006200
006300         PERFORM 201-CONVERT-TM-TO-TS                             00006300
006400       WHEN MMMC0291-CVT-TS-TO-TM                                 00006400
006500         PERFORM 301-CONVERT-TS-TO-TM                             00006500
006600       WHEN OTHER                                                 00006600
006700         SET FAILURE TO TRUE                                      00006700
006800         MOVE 'MMMS0291 - Invalid MMMC0291-FUNC passed.'          00006800
006900           TO IS-RTRN-MSG-TXT                                     00006900
007000     END-EVALUATE                                                 00007000
007100                                                                  00007100
007200                                                                  00007200
007300     GOBACK                                                       00007300
007400     .                                                            00007400
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/MMMS0291.cbl" line="96">

---

201-CONVERT-TM-TO-TS loops through each time string, validates hours/minutes/seconds, defaults blanks to '00', and converts valid times to timestamps. If a value is out of range, it sets an error and message; if the input is spaces, it clears the output entry.

```cobol
010300 201-CONVERT-TM-TO-TS.                                            00010300
010400                                                                  00010400
010500     PERFORM VARYING WS-CNT FROM 1 BY 1 UNTIL WS-CNT > WS-K       00010500
010600       IF WS-TIME-INOUT-CONV(WS-CNT) NOT EQUAL TO SPACES          00010600
010700         MOVE WS-TIME-INOUT-CONV(WS-CNT)                          00010700
010800                                     TO  WS-CURRENT-TIME          00010800
010900         PERFORM 110-INITIALIZE-TM-DELIM                          00010900
011000         IF WS-CURRENT-HOURS <= '23' AND WS-CURRENT-MINUTE <= '59'00011000
011100                                     AND WS-CURRENT-SECOND <= '59'00011100
011200           IF WS-CURRENT-HOURS   = '  '                           00011200
011300              MOVE '00'         TO WS-CURRENT-HOURS               00011300
011400           END-IF                                                 00011400
011500           IF WS-CURRENT-MINUTE  = '  '                           00011500
011600              MOVE '00'         TO WS-CURRENT-MINUTE              00011600
011700           END-IF                                                 00011700
011800           IF WS-CURRENT-SECOND  = '  '                           00011800
011900              MOVE '00'         TO WS-CURRENT-SECOND              00011900
012000           END-IF                                                 00012000
012100                                                                  00012100
012200           MOVE WS-CURRENT-TIME-STAMP                             00012200
012300                                TO WS-CURRENT-DATE-DATA           00012300
012400           MOVE WS-CURRENT-DATE-DATA                              00012400
012500                                TO WS-TIMSTAMP-INOUT-CONV(WS-CNT) 00012500
012600         ELSE                                                     00012600
012700           SET  FAILURE TO TRUE                                   00012700
012800           MOVE 'MMMS0291 - Invalid MMMC0291-FUNC passed.'        00012800
012900             TO IS-RTRN-MSG-TXT                                   00012900
013000           STRING 'MMMS0291 - INVALID TIME. PLEASE ENTER CORRECT' 00013000
013100                  'TIME VALUE'                                    00013100
013200           DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                 00013200
013300         END-IF                                                   00013300
013400       ELSE                                                       00013400
013500         MOVE SPACES TO WS-TIMSTAMP-INOUT-CONV(WS-CNT)            00013500
013600       END-IF                                                     00013600
013700     END-PERFORM                                                  00013700
013800     .                                                            00013800
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/MMMS0291.cbl" line="137">

---

301-CONVERT-TS-TO-TM loops through each timestamp, parses and validates time components, fills blanks with '00', and updates the output. If a value is out of range, it sets an error and message; if the input is spaces, it clears the output entry.

```cobol
014400 301-CONVERT-TS-TO-TM.                                            00014400
014500     PERFORM VARYING WS-CNT FROM 1 BY 1 UNTIL WS-CNT > WS-K       00014500
014600       IF WS-TIMSTAMP-INOUT-CONV(WS-CNT) NOT EQUAL TO SPACES      00014600
014700         MOVE WS-TIMSTAMP-INOUT-CONV(WS-CNT)                      00014700
014800                                     TO  WS-CURRENT-TIME-STAMP    00014800
014900         PERFORM 110-INITIALIZE-TM-DELIM                          00014900
015000         IF WS-CURRENT-HOURS <= '23' AND WS-CURRENT-MINUTE <= '59'00015000
015100                                     AND WS-CURRENT-SECOND <= '59'00015100
015200           IF WS-CURRENT-HOURS   = '  '                           00015200
015300              MOVE '00'         TO WS-CURRENT-HOURS               00015300
015400           END-IF                                                 00015400
015500           IF WS-CURRENT-MINUTE  = '  '                           00015500
015600              MOVE '00'         TO WS-CURRENT-MINUTE              00015600
015700           END-IF                                                 00015700
015800           IF WS-CURRENT-SECOND  = '  '                           00015800
015900              MOVE '00'         TO WS-CURRENT-SECOND              00015900
016000           END-IF                                                 00016000
016100           MOVE WS-CURRENT-TIME TO WS-TIME-INOUT-CONV(WS-CNT)     00016100
016200         ELSE                                                     00016200
016300           SET FAILURE TO TRUE                                    00016300
016400           MOVE 'MMMS0291 - Invalid MMMC0291-FUNC passed.'        00016400
016500             TO IS-RTRN-MSG-TXT                                   00016500
016600           STRING 'MMMS0291 - INVALID TIME. PLEASE ENTER CORRECT' 00016600
016700                  'TIMESTAMP VALUE'                               00016700
016800            DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT                00016800
016900         END-IF                                                   00016900
017000       ELSE                                                       00017000
017100         MOVE SPACES TO WS-TIME-INOUT-CONV(WS-CNT)                00017100
017200       END-IF                                                     00017200
017300     END-PERFORM                                                  00017300
017400     .                                                            00017400
```

---

</SwmSnippet>

### Connecting to Oracle

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Attempt to connect to Oracle database"]
    click node1 openCode "base/src/NNNS0487.cbl:1177:1179"
    node1 --> node2{"Was connection successful?"}
    click node2 openCode "base/src/NNNS0487.cbl:1180:1186"
    node2 -->|"Yes"| node5["Initialization complete"]
    click node5 openCode "base/src/NNNS0487.cbl:1187:1187"
    node2 -->|"No"| node3["Store SQL error code"]
    click node3 openCode "base/src/NNNS0487.cbl:1181:1181"
    node3 --> node4["Prepare and store error message"]
    click node4 openCode "base/src/NNNS0487.cbl:1182:1185"
    node4 --> node5

classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

<SwmSnippet path="/base/src/NNNS0487.cbl" line="1177">

---

115-CONNECT-TO-ORACLE calls Z-ORA-CONNECT to set up the Oracle database connection. If it fails, it grabs the SQLCODE and builds an error message for troubleshooting. The actual connection logic is handled by XXXS0210, which sets up the system state and connection.

```cobol
125400 115-CONNECT-TO-ORACLE.                                           00125400
125500     CALL Z-ORA-CONNECT USING XXXN001A                            00125500
125600                              SQLCA                               00125600
125700     IF NOT SUCCESS                                               00125700
125800       MOVE SQLCODE TO WS-SQLCODE                                 00125800
125900       MOVE SPACES  TO IS-RTRN-MSG-TXT                            00125900
126000       STRING 'NNNS0487 - Error connecting to Oracle. Sqlcode ='  00126000
126100               WS-SQLCODE                                         00126100
126200               DELIMITED BY SIZE INTO IS-RTRN-MSG-TXT             00126200
126300     END-IF                                                       00126300
126400     .                                                            00126400
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
