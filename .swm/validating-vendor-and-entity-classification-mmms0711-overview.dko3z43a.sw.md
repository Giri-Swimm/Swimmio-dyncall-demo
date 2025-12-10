---
title: Validating Vendor and Entity Classification (MMMS0711) - Overview
---
# Overview

This document describes the flow for determining whether a vendor or entity qualifies as a Direct Store Vendor (DSV). The process validates requests, applies business rules, and queries the database to classify the vendor or entity, returning DSV status, entity existence, and error messages as needed.

```mermaid
flowchart TD
    node1["Starting the validation and routing logic"]:::HeadingStyle --> node2{"Function type?"}
    click node1 goToHeading "Starting the validation and routing logic"
    node2 -->|"Vendor DSV"| node3["Checking vendor DSV status"]:::HeadingStyle
    click node3 goToHeading "Checking vendor DSV status"
    node2 -->|"Entity DSV"| node4{"Entity type?"}
    node4 -->|"UPC/DSD"| node5["Checking UPC/vendor DSV association"]:::HeadingStyle
    click node5 goToHeading "Checking UPC/vendor DSV association"
    click node5 goToHeading "Determining DSV status for a UPC"
    node4 -->|"PROD"| node6["Checking entity DSV status"]:::HeadingStyle
    click node6 goToHeading "Checking entity DSV status"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- MMMS0711 (<SwmPath>[base/src/MMMS0711.cbl](base/src/MMMS0711.cbl)</SwmPath>)
- YYYS0210 (<SwmPath>[base/src/XXXS0210.cbl](base/src/XXXS0210.cbl)</SwmPath>)
- YYYS0220 (<SwmPath>[base/src/YYYS0220.cbl](base/src/YYYS0220.cbl)</SwmPath>)
- YYYS0211 (<SwmPath>[base/src/YYYS0211.cbl](base/src/YYYS0211.cbl)</SwmPath>)
- YYYS0212 (<SwmPath>[base/src/YYYS0212.cbl](base/src/YYYS0212.cbl)</SwmPath>)
- WWWS0099

### Copybooks

- YYYN000A (<SwmPath>[base/src/YYYN000A.cpy](base/src/YYYN000A.cpy)</SwmPath>)
- YYYC0220 (<SwmPath>[base/src/YYYC0220.cpy](base/src/YYYC0220.cpy)</SwmPath>)
- XXXN001A (<SwmPath>[base/src/XXXN001A.cpy](base/src/XXXN001A.cpy)</SwmPath>)
- SQLCA
- WWWC0099
- YYYN005A (<SwmPath>[base/src/YYYN005A.cpy](base/src/YYYN005A.cpy)</SwmPath>)
- DDDTLS01
- DDDTPT01
- DDDTVI01
- DDDTSI01
- DDDTLO01 (<SwmPath>[base/src/DDDTLO01.cpy](base/src/DDDTLO01.cpy)</SwmPath>)
- MMMC0711 (<SwmPath>[base/src/MMMC0711.cpy](base/src/MMMC0711.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  5d2gl("Managing Location Data (NNNS0487)") --> k3wuc("Validating Vendor and Entity Classification (MMMS0711)"):::currentEntity
click 5d2gl openCode "base/src/NNNS0487.cbl:1"
  
  
click k3wuc openCode "base/src/MMMS0711.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   5d2gl("Managing Location Data (NNNS0487)") --> k3wuc("Validating Vendor and Entity Classification (MMMS0711)"):::currentEntity
%% click 5d2gl openCode "<SwmPath>[base/src/NNNS0487.cbl](base/src/NNNS0487.cbl)</SwmPath>:1"
%%   
%%   
%% click k3wuc openCode "<SwmPath>[base/src/MMMS0711.cbl](base/src/MMMS0711.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
