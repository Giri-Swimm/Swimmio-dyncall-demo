---
title: >-
  Table IO Operations for Location Class Advertisement Zone (NNNU0473) -
  Overview
---
# Overview

This document describes the flow for handling table row operations in the XXXX_LOC_CLS_AD_ZN table. The flow receives requests to modify, insert, or purge rows and ensures each request is routed to the correct process. When updating a row, only the targeted record matching the key fields is changed.

## Dependencies

### Program

- NNNU0473 (<SwmPath>[base/src/NNNU0473.cbl](base/src/NNNU0473.cbl)</SwmPath>)

### Copybooks

- XXXN001A (<SwmPath>[base/src/XXXN001A.cpy](base/src/XXXN001A.cpy)</SwmPath>)
- SQLCA
- YYYN005A (<SwmPath>[base/src/YYYN005A.cpy](base/src/YYYN005A.cpy)</SwmPath>)
- NNNN0000 (<SwmPath>[base/src/NNNN0000.cpy](base/src/NNNN0000.cpy)</SwmPath>)
- DDDTCZ01 (<SwmPath>[base/src/DDDTCZ01.cpy](base/src/DDDTCZ01.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  87yl6("Table IO Object for Location Class Advertisement Zones (NNNS0473)") --> min5q("Table IO Operations for Location Class Advertisement Zone (NNNU0473)"):::currentEntity
click 87yl6 openCode "base/src/NNNS0473.cbl:1"
  
  
click min5q openCode "base/src/NNNU0473.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   87yl6("Table IO Object for Location Class Advertisement Zones (NNNS0473)") --> min5q("Table IO Operations for Location Class Advertisement Zone (NNNU0473)"):::currentEntity
%% click 87yl6 openCode "<SwmPath>[base/src/NNNS0473.cbl](base/src/NNNS0473.cbl)</SwmPath>:1"
%%   
%%   
%% click min5q openCode "<SwmPath>[base/src/NNNU0473.cbl](base/src/NNNU0473.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
