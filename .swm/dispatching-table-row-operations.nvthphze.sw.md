---
title: Dispatching Table Row Operations
---
This document explains how the system selects and performs the appropriate database operation—modify, insert, or purge—on a table row. The flow receives a request indicating the desired action and dispatches it to the relevant process, resulting in the row being updated, added, or removed as needed.

# Spec

## Detailed View of the Program's Functionality

# Program Initialization and Structure

The program begins by declaring its identity, author, and metadata such as the date written and compiled. It describes itself as a data access object (DAO) for table input/output operations, specifically for a table named XXXX_LOC_CLS_AD_ZN in an Oracle database. The program uses several external data structures and SQL communication areas, which are included via copy statements. These structures provide the necessary fields and parameters for database operations.

# Main Entry Point and Operation Dispatch

The main logic starts in the procedure division, which receives several data structures as input. The first routine executed is the dispatcher. This dispatcher evaluates which database operation should be performed based on an input code. The possible operations are:

- Modify an existing row
- Insert a new row
- Purge (delete) a row

Depending on the input code, the dispatcher calls the corresponding routine for the required operation. This separation ensures that each type of database action is handled independently, making the code easier to maintain and extend.

# Modifying an Existing Table Row

If the dispatcher determines that a row should be modified, it calls the routine responsible for updates. This routine, in turn, performs an embedded SQL UPDATE statement. The update sets several fields in the table to new values provided by the input data structure. The update only affects rows that match specific criteria, namely the values of three key fields. This ensures that only the intended row is updated.

# Inserting a New Table Row

If the dispatcher decides to insert a new row, it calls the insert routine. This routine delegates the actual insertion to a dedicated subroutine. The subroutine executes an embedded SQL INSERT statement, adding a new row to the table. The values for the new row are taken from the input data structure. This design keeps the insert logic isolated, so changes to how inserts are performed do not affect other parts of the program.

# Purging (Deleting) a Table Row

If the dispatcher determines that a row should be purged, it calls the purge routine. This routine executes an embedded SQL DELETE statement, removing a row from the table. The deletion is based on matching three key fields from the input data structure. Only rows that match all three criteria are deleted, ensuring precise control over which data is removed.

# Program Termination

After the requested operation is performed, the program returns control to the caller. This is done using a standard COBOL statement for ending a program.

# Summary

- The program acts as a database access object for a specific table.
- It receives input specifying which operation to perform: modify, insert, or purge.
- The dispatcher directs control to the appropriate routine for the requested operation.
- Each operation (update, insert, delete) is handled by a dedicated routine using embedded SQL statements.
- The program uses input data structures to supply values for database operations.
- The design keeps logic for each operation separate, making the code maintainable and clear.

# Rule Definition

| Paragraph Name                                                                                    | Rule ID | Category          | Description                                                                                                                                                                                                    | Conditions                                                                                                                                                      | Remarks                                                                                                                                                                                                                                                                                                                     |
| ------------------------------------------------------------------------------------------------- | ------- | ----------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 0000-EXIT-DISPATCHER                                                                              | RL-001  | Conditional Logic | The system evaluates the requested database operation (insert, modify, purge) based on an input code and dispatches to the corresponding routine.                                                              | An input code indicating the desired operation (insert, modify, purge) must be provided.                                                                        | The input code is compared against constants representing each operation. The dispatcher uses a conditional structure to select the appropriate routine. The output format is not affected by this rule.                                                                                                                    |
| 1505-DO-INSERT                                                                                    | RL-002  | Data Assignment   | For an insert operation, the system adds a new row to the XXXX_LOC_CLS_AD_ZN table using all five fields from the DCLXXXX-LOC-CLS-AD-ZN structure.                                                             | The operation code must indicate an insert operation. All five fields must be present in the input structure.                                                   | The fields are: LOC-TYP-CD, LOC-NBR, ITM-CLS-CD, AD-ZONE, AD-ZONE-EXCP. These are mapped to the table columns LOC_TYP_CD, LOC_NBR, ITM_CLS_CD, AD_ZONE, AD_ZONE_EXCP. All values are passed as parameters to the SQL INSERT statement. Field types are assumed to match between structure and table (e.g., string, number). |
| 1600-EXIT-PUT-PURGE-ROW                                                                           | RL-003  | Computation       | For a purge operation, the system deletes a row from the XXXX_LOC_CLS_AD_ZN table where the LOC_TYP_CD, LOC_NBR, and ITM_CLS_CD columns match the corresponding values in the DCLXXXX-LOC-CLS-AD-ZN structure. | The operation code must indicate a purge operation. The three key fields must be present in the input structure.                                                | Only LOC-TYP-CD, LOC-NBR, and ITM-CLS-CD are used for matching. The SQL DELETE statement uses these as WHERE clause parameters. Field types must match between structure and table (e.g., string, number).                                                                                                                  |
| 1405-DO-UPDATE                                                                                    | RL-004  | Computation       | For a modify operation, the system updates all five columns in the XXXX_LOC_CLS_AD_ZN table for the row where LOC_TYP_CD, LOC_NBR, and ITM_CLS_CD match the input structure.                                   | The operation code must indicate a modify operation. All five fields must be present in the input structure. The row to update must match the three key fields. | The SQL UPDATE statement sets all five columns to the values from the input structure, but only for the row matching the three key fields. Field types must match between structure and table (e.g., string, number).                                                                                                       |
| 0000-EXIT-DISPATCHER, 1400-EXIT-PUT-MODIFY-ROW, 1500-EXIT-PUT-INSERT-ROW, 1600-EXIT-PUT-PURGE-ROW | RL-005  | Conditional Logic | The logic for each operation (insert, purge, modify) is kept separate and isolated from each other, as required by the specification.                                                                          | Each operation must be handled in its own routine, with no shared logic between them.                                                                           | Each operation is implemented in a distinct paragraph/routine. There is no overlap in logic or data manipulation between these routines.                                                                                                                                                                                    |

# User Stories

## User Story 1: Dispatch and perform insert operation

---

### Story Description:

As a system, I want to evaluate the input code and, if it indicates an insert operation, add a new row to the XXXX_LOC_CLS_AD_ZN table using all five fields from the DCLXXXX-LOC-CLS-AD-ZN structure so that new location-class-ad-zone data can be recorded accurately.

---

### Business Rule Mapping:

| Rule ID | Paragraph Name                                                                                    | Rule Description                                                                                                                                   |
| ------- | ------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| RL-001  | 0000-EXIT-DISPATCHER                                                                              | The system evaluates the requested database operation (insert, modify, purge) based on an input code and dispatches to the corresponding routine.  |
| RL-005  | 0000-EXIT-DISPATCHER, 1400-EXIT-PUT-MODIFY-ROW, 1500-EXIT-PUT-INSERT-ROW, 1600-EXIT-PUT-PURGE-ROW | The logic for each operation (insert, purge, modify) is kept separate and isolated from each other, as required by the specification.              |
| RL-002  | 1505-DO-INSERT                                                                                    | For an insert operation, the system adds a new row to the XXXX_LOC_CLS_AD_ZN table using all five fields from the DCLXXXX-LOC-CLS-AD-ZN structure. |

---

### Relevant Functionality:

- **0000-EXIT-DISPATCHER**
  1. **RL-001:**
     - Evaluate the input operation code.
       - If the code is for modify, perform the modify routine.
       - If the code is for insert, perform the insert routine.
       - If the code is for purge, perform the purge routine.
     - Each routine is isolated and only invoked for its corresponding code.
  2. **RL-005:**
     - The dispatcher selects the routine based on the operation code.
     - Each routine (insert, modify, purge) is implemented in a separate paragraph.
     - No routine shares logic or data manipulation with another.
- **1505-DO-INSERT**
  1. **RL-002:**
     - On insert operation:
       - Map each of the five fields from the input structure to the corresponding table column.
       - Execute an SQL INSERT statement with these values.

## User Story 2: Dispatch and perform purge operation

---

### Story Description:

As a system, I want to evaluate the input code and, if it indicates a purge operation, delete the row from the XXXX_LOC_CLS_AD_ZN table where the LOC_TYP_CD, LOC_NBR, and ITM_CLS_CD columns match the input structure so that obsolete location-class-ad-zone data can be removed.

---

### Business Rule Mapping:

| Rule ID | Paragraph Name                                                                                    | Rule Description                                                                                                                                                                                               |
| ------- | ------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| RL-001  | 0000-EXIT-DISPATCHER                                                                              | The system evaluates the requested database operation (insert, modify, purge) based on an input code and dispatches to the corresponding routine.                                                              |
| RL-005  | 0000-EXIT-DISPATCHER, 1400-EXIT-PUT-MODIFY-ROW, 1500-EXIT-PUT-INSERT-ROW, 1600-EXIT-PUT-PURGE-ROW | The logic for each operation (insert, purge, modify) is kept separate and isolated from each other, as required by the specification.                                                                          |
| RL-003  | 1600-EXIT-PUT-PURGE-ROW                                                                           | For a purge operation, the system deletes a row from the XXXX_LOC_CLS_AD_ZN table where the LOC_TYP_CD, LOC_NBR, and ITM_CLS_CD columns match the corresponding values in the DCLXXXX-LOC-CLS-AD-ZN structure. |

---

### Relevant Functionality:

- **0000-EXIT-DISPATCHER**
  1. **RL-001:**
     - Evaluate the input operation code.
       - If the code is for modify, perform the modify routine.
       - If the code is for insert, perform the insert routine.
       - If the code is for purge, perform the purge routine.
     - Each routine is isolated and only invoked for its corresponding code.
  2. **RL-005:**
     - The dispatcher selects the routine based on the operation code.
     - Each routine (insert, modify, purge) is implemented in a separate paragraph.
     - No routine shares logic or data manipulation with another.
- **1600-EXIT-PUT-PURGE-ROW**
  1. **RL-003:**
     - On purge operation:
       - Use the three key fields from the input structure to match the row in the table.
       - Execute an SQL DELETE statement with these fields in the WHERE clause.

## User Story 3: Dispatch and perform modify operation

---

### Story Description:

As a system, I want to evaluate the input code and, if it indicates a modify operation, update all five columns in the XXXX_LOC_CLS_AD_ZN table for the row matching the three key fields so that existing location-class-ad-zone data can be updated as needed.

---

### Business Rule Mapping:

| Rule ID | Paragraph Name                                                                                    | Rule Description                                                                                                                                                             |
| ------- | ------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| RL-001  | 0000-EXIT-DISPATCHER                                                                              | The system evaluates the requested database operation (insert, modify, purge) based on an input code and dispatches to the corresponding routine.                            |
| RL-005  | 0000-EXIT-DISPATCHER, 1400-EXIT-PUT-MODIFY-ROW, 1500-EXIT-PUT-INSERT-ROW, 1600-EXIT-PUT-PURGE-ROW | The logic for each operation (insert, purge, modify) is kept separate and isolated from each other, as required by the specification.                                        |
| RL-004  | 1405-DO-UPDATE                                                                                    | For a modify operation, the system updates all five columns in the XXXX_LOC_CLS_AD_ZN table for the row where LOC_TYP_CD, LOC_NBR, and ITM_CLS_CD match the input structure. |

---

### Relevant Functionality:

- **0000-EXIT-DISPATCHER**
  1. **RL-001:**
     - Evaluate the input operation code.
       - If the code is for modify, perform the modify routine.
       - If the code is for insert, perform the insert routine.
       - If the code is for purge, perform the purge routine.
     - Each routine is isolated and only invoked for its corresponding code.
  2. **RL-005:**
     - The dispatcher selects the routine based on the operation code.
     - Each routine (insert, modify, purge) is implemented in a separate paragraph.
     - No routine shares logic or data manipulation with another.
- **1405-DO-UPDATE**
  1. **RL-004:**
     - On modify operation:
       - Use the three key fields from the input structure to match the row in the table.
       - Update all five columns in the matched row with the values from the input structure.

# Code Walkthrough

## Dispatching Table Row Operations

<SwmSnippet path="/base/src/NNNU0473.cbl" line="44">

---

`0000-EXIT-DISPATCHER` kicks off the flow by evaluating which database operation to run—modify, insert, or purge—based on the input code. It then calls the relevant routine, like `1500-EXIT-PUT-INSERT-ROW` for inserts. Calling the insert routine here lets us add new records when the dispatcher determines that's needed, keeping the logic for each operation separate and clear.

```cobol
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
```

---

</SwmSnippet>

### Inserting a New Table Row

<SwmSnippet path="/base/src/NNNU0473.cbl" line="81">

---

`1500-EXIT-PUT-INSERT-ROW` just hands off control to `1505-DO-INSERT`, which actually performs the database insert. This separation keeps the insert logic isolated, so changes to how inserts work don't mess with the rest of the flow.

```cobol
007100 1500-EXIT-PUT-INSERT-ROW.                                        00710000
007200      PERFORM 1505-DO-INSERT                                      00720000
007300     .                                                            00730000
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/NNNU0473.cbl" line="86">

---

`1505-DO-INSERT` runs an embedded SQL insert to add a new row to the XXXX_LOC_CLS_AD_ZN table, using values from the DCLXXXX-LOC-CLS-AD-ZN structure. This is standard COBOL/SQL integration, but assumes the data structure is already set up with the right values.

```cobol
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
```

---

</SwmSnippet>

### Purging a Table Row

<SwmSnippet path="/base/src/NNNU0473.cbl" line="104">

---

`1600-EXIT-PUT-PURGE-ROW` deletes a row from the table using an embedded SQL DELETE, matching on fields from the DCLXXXX-LOC-CLS-AD-ZN structure. It assumes those fields are correctly set up before this routine runs, otherwise the delete might not work as expected.

```cobol
008200 1600-EXIT-PUT-PURGE-ROW.                                         00820000
008300       EXEC SQL                                                   00830000
051400         DELETE FROM XXXX_LOC_CLS_AD_ZN                           00051400
051500         WHERE  LOC_TYP_CD = :DCLXXXX-LOC-CLS-AD-ZN.LOC-TYP-CD    00051500
051600         AND    LOC_NBR = :DCLXXXX-LOC-CLS-AD-ZN.LOC-NBR          00051600
051700         AND    ITM_CLS_CD = :DCLXXXX-LOC-CLS-AD-ZN.ITM-CLS-CD    00051700
008400       END-EXEC                                                   00840000
008500     .                                                            00850000
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
