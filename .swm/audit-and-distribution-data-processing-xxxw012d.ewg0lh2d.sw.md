---
title: Audit and Distribution Data Processing (XXXW012D)
---
The XXXW012D job processes audit and distribution records by sorting raw audit data, updating distribution records with control rules, and backing up the finalized data to tape. It organizes and validates audit data to support reporting and archival needs.

For example, it sorts unsorted audit records by transaction key, updates distribution status, and outputs updated records along with a backup copy.

# Dependencies

```mermaid
graph TD
  
  kqz5j("XXXW012D"):::currentEntity --> 5m5q6("SORT3MB")
  
  
kqz5j("XXXW012D"):::currentEntity --> g0ph3("DB2BATCH")
  
  
  
click kqz5j openCode "base/cntl/XXXW012D.jcl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   
%%   kqz5j("XXXW012D"):::currentEntity --> 5m5q6("SORT3MB")
%%   
%%   
%% kqz5j("XXXW012D"):::currentEntity --> g0ph3("DB2BATCH")
%%   
%%   
%%   
%% click kqz5j openCode "<SwmPath>[base/cntl/XXXW012D.jcl](base/cntl/XXXW012D.jcl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

Here is a high level diagram of the file:

```mermaid
graph TD
ac5fb783f("Sort Audit Data")
  click ac5fb783f goToHeading "Sort Audit Data"
  

ae1f44b46("Update Distribution Records")
  click ae1f44b46 goToHeading "Update Distribution Records"
  

a17eb6ce9("Backup Distribution Records")
  click a17eb6ce9 goToHeading "Backup Distribution Records"
  




ac5fb783f --> ae1f44b46
ae1f44b46 --> a17eb6ce9
style ac5fb783f color:#000000,fill:#7CB9F4
style ae1f44b46 color:#000000,fill:#7CB9F4
style a17eb6ce9 color:#000000,fill:#7CB9F4
```

## Sort Audit Data

Step in this section: `ST01OF04`.

The section prepares audit data for downstream distribution by sorting records in a specific key order, ensuring that related information is grouped together for subsequent handling.

1. The audit records from the input dataset XXXP.AUDIT.STRIP are read into the sorting utility.
2. Each record is examined for its transaction key (TXN_KEY), defined as starting at position 6 for 3 bytes.
3. Records are ordered in ascending key order using a binary format comparison, grouping related transactions together.
4. The sorted results are written to the temporary dataset &&XXXPSORT in exactly the same structure as the input, but reorganized so transaction keys appear in order.
5. This sorted output enables efficient downstream distribution and lookup operations.

### Input

**XXXP.AUDIT.STRIP - XXXP.AUDIT.STRIP(0)**

Raw audit records prior to any sorting or organization.

Sample:

| Column Name | Sample               |
| ----------- | -------------------- |
| TXN_KEY     | 001                  |
| USER_ID     | U12345               |
| AMOUNT      | 1000                 |
| AUDIT_DATE  | 20240609             |
| DETAILS     | Transaction approved |

### Output

**&&XXXPSORT**

Audit records sorted by transaction key, stored as a temporary dataset for use in later processing and distribution steps.

Sample:

| Column Name | Sample               |
| ----------- | -------------------- |
| TXN_KEY     | 001                  |
| USER_ID     | U12345               |
| AMOUNT      | 1000                 |
| AUDIT_DATE  | 20240609             |
| DETAILS     | Transaction approved |

## Update Distribution Records

Step in this section: `ST02OF04`.

This section performs distribution updates by transforming sorted audit records into finalized distribution records based on business rules and lookup data.

- The section starts by reading sorted audit records from the temporary dataset (&&XXXPSORT).
- Control card rules (GL.CONTROL.CARDS(XXXC1001)) are referenced to determine how each audit record is processed according to distribution needs and lookup validations.
- For each audit transaction:
  1. Lookup data and rules are applied to verify transaction legitimacy and define distribution status.
  2. Transaction fields are examined and selectively transformed (such as adding or updating DIST_STATUS).
  3. Processed records are written to the destination dataset (XXXP.DIST.AUDIT.STRIP), which serves as the finalized distribution record set for backup and reporting.
- Output follows the structure required for downstream processes, such as backup tape creation.

### Input

**&&XXXPSORT**

Sorted audit records from previous step, prepared for distribution processing.

Sample:

| Column Name | Sample               |
| ----------- | -------------------- |
| TXN_KEY     | 001                  |
| USER_ID     | U12345               |
| AMOUNT      | 1000                 |
| AUDIT_DATE  | 20240609             |
| DETAILS     | Transaction approved |

**GL.CONTROL.CARDS(XXXC1001)**

Control cards defining distribution and lookup rules for processing audit records.

### Output

**XXXP.DIST.AUDIT.STRIP**

Distributed audit records updated according to control requirements; forms basis for backup and further reporting.

Sample:

| Column Name | Sample      |
| ----------- | ----------- |
| TXN_KEY     | 001         |
| USER_ID     | U12345      |
| DIST_STATUS | Distributed |
| AUDIT_DATE  | 20240609    |
| AMOUNT      | 1000        |

## Backup Distribution Records

Step in this section: `ST03OF04`.

This section ensures that each day's processed distribution records are safely duplicated to a tape backup to prevent data loss and support rollback or audit activities.

- Reads finalized distribution records from the dataset XXXP.DIST.AUDIT.STRIP.
- Duplicates each record exactly as-is, including all fields (TXN_KEY, USER_ID, DIST_STATUS, AUDIT_DATE, AMOUNT), without transformation or filtering.
- Writes the copied records to a new sequential version of the backup dataset (XXXP.DAILY.DIST.BKUP), preserving the daily state of distribution data for archival and recovery.

### Input

**XXXP.DIST.AUDIT.STRIP**

Finalized and updated distribution audit records prepared in the prior step.

Sample:

| Column Name | Sample      |
| ----------- | ----------- |
| TXN_KEY     | 001         |
| USER_ID     | U12345      |
| DIST_STATUS | Distributed |
| AUDIT_DATE  | 20240609    |
| AMOUNT      | 1000        |

### Output

**XXXP.DAILY.DIST.BKUP(+1)**

A new backup version of distribution audit records, stored for daily archival and recovery.

Sample:

| Column Name | Sample      |
| ----------- | ----------- |
| TXN_KEY     | 001         |
| USER_ID     | U12345      |
| DIST_STATUS | Distributed |
| AUDIT_DATE  | 20240609    |
| AMOUNT      | 1000        |

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
