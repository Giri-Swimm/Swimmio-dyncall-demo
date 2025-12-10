---
title: Oracle to DB2 SQL Error Code Converter (YYYS0212) - Overview
---
# Overview

This document explains the flow of handling Oracle SQL errors. Oracle error information is converted into standardized internal codes and user-friendly messages, ensuring consistent and understandable error reporting for users and downstream systems.

```mermaid
flowchart TD
    node1["Starting the error handling process"]:::HeadingStyle --> node2["Extracting and mapping Oracle error codes"]:::HeadingStyle
    node2 --> node3{"Is Oracle error code recognized?"}
    node3 -->|"Yes"| node4["Map to standardized internal SQL error code"]
    node3 -->|"No"| node5["Generate user message with Oracle code"]
    click node1 goToHeading "Starting the error handling process"
    click node2 goToHeading "Extracting and mapping Oracle error codes"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- YYYS0212 (<SwmPath>[base/src/YYYS0212.cbl](base/src/YYYS0212.cbl)</SwmPath>)

### Copybooks

- XXXN001A (<SwmPath>[base/src/XXXN001A.cpy](base/src/XXXN001A.cpy)</SwmPath>)
- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  dl55a("Database Connection and Oracle SQL Error Handling (YYYS0211)") --> 5klke("Oracle to DB2 SQL Error Code Converter (YYYS0212)"):::currentEntity
click dl55a openCode "base/src/YYYS0211.cbl:1"
  
  
click 5klke openCode "base/src/YYYS0212.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   dl55a("Database Connection and Oracle SQL Error Handling (YYYS0211)") --> 5klke("Oracle to DB2 SQL Error Code Converter (YYYS0212)"):::currentEntity
%% click dl55a openCode "<SwmPath>[base/src/YYYS0211.cbl](base/src/YYYS0211.cbl)</SwmPath>:1"
%%   
%%   
%% click 5klke openCode "<SwmPath>[base/src/YYYS0212.cbl](base/src/YYYS0212.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
