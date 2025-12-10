---
title: Retrieving Store Details (NNNS0120) - Overview
---
# Overview

This document describes the flow for processing store operation requests. The flow initializes the environment, manages database connections, and executes store operations such as opening cursors, fetching records, and updating data, ensuring accurate processing and connection management.

```mermaid
flowchart TD
    node1["Preparing store records and environment"]:::HeadingStyle
    click node1 goToHeading "Preparing store records and environment"
    node1 --> node2["Dispatching store operations"]:::HeadingStyle
    click node2 goToHeading "Dispatching store operations"
    node2 --> node3{"Which store operation is requested?"}
    node3 -->|"Open Cursor"| node4["Opening database cursors"]:::HeadingStyle
    click node4 goToHeading "Opening database cursors"
    node3 -->|"Fetch Next Row"| node5["Fetching next store record"]:::HeadingStyle
    click node5 goToHeading "Fetching next store record"
    node3 -->|"Insert/Modify/Purge Row"| node6["Finalizing insert operations"]:::HeadingStyle
    click node6 goToHeading "Finalizing insert operations"
    node6 -->|"If connection switch required"| node7["Switching to DB2 and restoring state"]:::HeadingStyle
    click node7 goToHeading "Switching to DB2 and restoring state"
    node3 -->|"Special I/O"| node7

classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- NNNS0120 (<SwmPath>[base/src/NNNS0120.cbl](base/src/NNNS0120.cbl)</SwmPath>)
- YYYS0210 (<SwmPath>[base/src/XXXS0210.cbl](base/src/XXXS0210.cbl)</SwmPath>)
- YYYS0220 (<SwmPath>[base/src/YYYS0220.cbl](base/src/YYYS0220.cbl)</SwmPath>)
- YYYS0211 (<SwmPath>[base/src/YYYS0211.cbl](base/src/YYYS0211.cbl)</SwmPath>)
- YYYS0212 (<SwmPath>[base/src/YYYS0212.cbl](base/src/YYYS0212.cbl)</SwmPath>)
- MMMS0304
- NNNU0120

### Copybooks

- SQLCA
- XXXN001A (<SwmPath>[base/src/XXXN001A.cpy](base/src/XXXN001A.cpy)</SwmPath>)
- YYYC0220 (<SwmPath>[base/src/YYYC0220.cpy](base/src/YYYC0220.cpy)</SwmPath>)
- YYYN000A (<SwmPath>[base/src/YYYN000A.cpy](base/src/YYYN000A.cpy)</SwmPath>)
- NNNN000U (<SwmPath>[base/src/NNNN000U.cpy](base/src/NNNN000U.cpy)</SwmPath>)
- HHHTRL01 (<SwmPath>[base/src/HHHTRL01.cpy](base/src/HHHTRL01.cpy)</SwmPath>)
- MMMC0304 (<SwmPath>[base/src/MMMC0304.cpy](base/src/MMMC0304.cpy)</SwmPath>)
- DDDTRL01 (<SwmPath>[base/src/DDDTRL01.cpy](base/src/DDDTRL01.cpy)</SwmPath>)
- W00N001A
- YYYN005A (<SwmPath>[base/src/YYYN005A.cpy](base/src/YYYN005A.cpy)</SwmPath>)
- NNNN0000 (<SwmPath>[base/src/NNNN0000.cpy](base/src/NNNN0000.cpy)</SwmPath>)
- PPPTRL01 (<SwmPath>[base/src/PPPTRL01.cpy](base/src/PPPTRL01.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  0yplz("Synchronizing Store Class Zones (MMMS0161)") --> t1j5y("Retrieving Store Details (NNNS0120)"):::currentEntity
click 0yplz openCode "base/src/MMMS0161.cbl:1"
  
  
click t1j5y openCode "base/src/NNNS0120.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   0yplz("Synchronizing Store Class Zones (MMMS0161)") --> t1j5y("Retrieving Store Details (NNNS0120)"):::currentEntity
%% click 0yplz openCode "<SwmPath>[base/src/MMMS0161.cbl](base/src/MMMS0161.cbl)</SwmPath>:1"
%%   
%%   
%% click t1j5y openCode "<SwmPath>[base/src/NNNS0120.cbl](base/src/NNNS0120.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
