---
title: Department Data Translation (MMMS0258) - Overview
---
# Overview

This document describes the flow for translating department data between new and old formats. The process determines the translation direction, validates and maps department fields, and ensures compatibility for downstream systems.

```mermaid
flowchart TD
    node1["Entry and Dispatch Logic
(Determine translation direction)
(Entry and Dispatch Logic)"]:::HeadingStyle --> node2{"Translation direction?
(Entry and Dispatch Logic)"}:::HeadingStyle
    click node1 goToHeading "Entry and Dispatch Logic"
    node2 -->|"New-to-old"| node3["New-to-Old Data Mapping"]:::HeadingStyle
    click node2 goToHeading "Entry and Dispatch Logic"
    click node3 goToHeading "New-to-Old Data Mapping"
    node2 -->|"Old-to-new"| node4["Old-to-New Data Mapping"]:::HeadingStyle
    click node4 goToHeading "Old-to-New Data Mapping"
    node3 --> node5["Department Identifier Extraction"]:::HeadingStyle
    click node5 goToHeading "Department Identifier Extraction"
    node4 --> node5
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- MMMS0258 (<SwmPath>[base/src/MMMS0258.cbl](base/src/MMMS0258.cbl)</SwmPath>)

### Copybooks

- MMMC9012 (<SwmPath>[base/src/MMMC9012.cpy](base/src/MMMC9012.cpy)</SwmPath>)
- MMMK001B (<SwmPath>[base/src/MMMK001B.cpy](base/src/MMMK001B.cpy)</SwmPath>)
- YYYN000A (<SwmPath>[base/src/YYYN000A.cpy](base/src/YYYN000A.cpy)</SwmPath>)
- MMMN000A (<SwmPath>[base/src/MMMN000A.cpy](base/src/MMMN000A.cpy)</SwmPath>)
- XXXN001A (<SwmPath>[base/src/XXXN001A.cpy](base/src/XXXN001A.cpy)</SwmPath>)
- YYYN111A (<SwmPath>[base/src/YYYN111A.cpy](base/src/YYYN111A.cpy)</SwmPath>)
- DDDTDP01 (<SwmPath>[base/src/DDDTDP01.cpy](base/src/DDDTDP01.cpy)</SwmPath>)
- DDDLCT20

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  4zv38("Database Cursor Operations Handler (WWWS0040)") --> 05iur("Department Data Translation (MMMS0258)"):::currentEntity
click 4zv38 openCode "base/src/WWWS0040.cbl:1"
  
  
click 05iur openCode "base/src/MMMS0258.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   4zv38("Database Cursor Operations Handler (WWWS0040)") --> 05iur("Department Data Translation (MMMS0258)"):::currentEntity
%% click 4zv38 openCode "<SwmPath>[base/src/WWWS0040.cbl](base/src/WWWS0040.cbl)</SwmPath>:1"
%%   
%%   
%% click 05iur openCode "<SwmPath>[base/src/MMMS0258.cbl](base/src/MMMS0258.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
