---
title: Generic Event Issuer for Master Data Entities (ZZZS0197) - Overview
---
# Overview

This document describes the flow for filtering and issuing master data entity events. The process receives a list of events, filters them by transaction type and environment, removes duplicates, and issues only eligible events to the event staging system.

## Dependencies

### Programs

- ZZZS0197 (<SwmPath>[base/src/ZZZS0197.cbl](base/src/ZZZS0197.cbl)</SwmPath>)
- YYYS0175

### Copybooks

- YYYN000A (<SwmPath>[base/src/YYYN000A.cpy](base/src/YYYN000A.cpy)</SwmPath>)
- YYYC0175
- XXXN001A (<SwmPath>[base/src/XXXN001A.cpy](base/src/XXXN001A.cpy)</SwmPath>)
- YYYN110A (<SwmPath>[base/src/YYYN110A.cpy](base/src/YYYN110A.cpy)</SwmPath>)
- ZZZC0197 (<SwmPath>[base/src/ZZZC0197.cpy](base/src/ZZZC0197.cpy)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  q09u2("Table IO Object for Location Class Advertisement Zones (NNNS0473)") --> 24670("Generic Event Issuer for Master Data Entities (ZZZS0197)"):::currentEntity
click q09u2 openCode "base/src/NNNS0473.cbl:1"
vwryz("Table IO Object for Department Data (NNNS0573)") --> 24670("Generic Event Issuer for Master Data Entities (ZZZS0197)"):::currentEntity
click vwryz openCode "base/src/NNNS0573.cbl:1"
gneh8("Managing Location Data (NNNS0487)") --> 24670("Generic Event Issuer for Master Data Entities (ZZZS0197)"):::currentEntity
click gneh8 openCode "base/src/NNNS0487.cbl:1"
f18tz("Retail Location Data Processing (NNNS0488)") --> 24670("Generic Event Issuer for Master Data Entities (ZZZS0197)"):::currentEntity
click f18tz openCode "base/src/NNNS0488.cbl:1"
  
  
click 24670 openCode "base/src/ZZZS0197.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   q09u2("Table IO Object for Location Class Advertisement Zones (NNNS0473)") --> 24670("Generic Event Issuer for Master Data Entities (ZZZS0197)"):::currentEntity
%% click q09u2 openCode "<SwmPath>[base/src/NNNS0473.cbl](base/src/NNNS0473.cbl)</SwmPath>:1"
%% vwryz("Table IO Object for Department Data (NNNS0573)") --> 24670("Generic Event Issuer for Master Data Entities (ZZZS0197)"):::currentEntity
%% click vwryz openCode "<SwmPath>[base/src/NNNS0573.cbl](base/src/NNNS0573.cbl)</SwmPath>:1"
%% gneh8("Managing Location Data (NNNS0487)") --> 24670("Generic Event Issuer for Master Data Entities (ZZZS0197)"):::currentEntity
%% click gneh8 openCode "<SwmPath>[base/src/NNNS0487.cbl](base/src/NNNS0487.cbl)</SwmPath>:1"
%% f18tz("Retail Location Data Processing (NNNS0488)") --> 24670("Generic Event Issuer for Master Data Entities (ZZZS0197)"):::currentEntity
%% click f18tz openCode "<SwmPath>[base/src/NNNS0488.cbl](base/src/NNNS0488.cbl)</SwmPath>:1"
%%   
%%   
%% click 24670 openCode "<SwmPath>[base/src/ZZZS0197.cbl](base/src/ZZZS0197.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
