---
title: Database Connection Manager (YYYS0220) - Overview
---
# Overview

This document describes how the system processes database connection requests, selects the appropriate database (DB2 or Oracle) based on the environment, and maintains connection statistics. The flow receives a connection request and returns the updated connection state, statistics, or information as needed.

```mermaid
flowchart TD
    node1["Dispatching the Connection Request
(Dispatching the Connection Request)"]:::HeadingStyle --> node2{"Which operation is requested?
(Dispatching the Connection Request)"}:::HeadingStyle
    click node1 goToHeading "Dispatching the Connection Request"
    click node2 goToHeading "Dispatching the Connection Request"
    node2 -->|"DB2"| node3["Handling DB2 Connection Requests"]:::HeadingStyle
    click node3 goToHeading "Handling DB2 Connection Requests"
    node3 --> node4["Switching to DB2 Database"]:::HeadingStyle
    click node4 goToHeading "Switching to DB2 Database"
    node2 -->|"Oracle"| node5["Handling Oracle Connection Requests"]:::HeadingStyle
    click node5 goToHeading "Handling Oracle Connection Requests"
    node5 --> node6["Switching to Oracle Database"]:::HeadingStyle
    click node6 goToHeading "Switching to Oracle Database"
    node2 -->|"Reset Stats"| node7["Resetting Connection Statistics"]:::HeadingStyle
    click node7 goToHeading "Resetting Connection Statistics"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- YYYS0220 (<SwmPath>[base/src/YYYS0220.cbl](base/src/YYYS0220.cbl)</SwmPath>)

### Copybooks

- SQLCA
- XXXN001A (<SwmPath>[base/src/XXXN001A.cpy](base/src/XXXN001A.cpy)</SwmPath>)
- YYYC0220 (<SwmPath>[base/src/YYYC0220.cpy](base/src/YYYC0220.cpy)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  0s62b("Database Connection and Oracle SQL Error Handling (YYYS0211)") --> i9uee("Database Connection Manager (YYYS0220)"):::currentEntity
click 0s62b openCode "base/src/YYYS0211.cbl:1"
7gnq9("Database Connection Initialization (XXXS0210)") --> i9uee("Database Connection Manager (YYYS0220)"):::currentEntity
click 7gnq9 openCode "base/src/XXXS0210.cbl:1"
  
  
click i9uee openCode "base/src/YYYS0220.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   0s62b("Database Connection and Oracle SQL Error Handling (YYYS0211)") --> i9uee("Database Connection Manager (YYYS0220)"):::currentEntity
%% click 0s62b openCode "<SwmPath>[base/src/YYYS0211.cbl](base/src/YYYS0211.cbl)</SwmPath>:1"
%% 7gnq9("Database Connection Initialization (XXXS0210)") --> i9uee("Database Connection Manager (YYYS0220)"):::currentEntity
%% click 7gnq9 openCode "<SwmPath>[base/src/XXXS0210.cbl](base/src/XXXS0210.cbl)</SwmPath>:1"
%%   
%%   
%% click i9uee openCode "<SwmPath>[base/src/YYYS0220.cbl](base/src/YYYS0220.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
