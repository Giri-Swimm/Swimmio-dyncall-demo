---
title: Master Data Transitional Control (WWWS0100) - Overview
---
# Overview

This document describes the flow for managing the transitional state of master data tasks. The program allows other modules to save or retrieve task state information, such as task, subtask, update status, and event flags, based on the requested operation.

## Dependencies

### Program

- WWWS0100 (<SwmPath>[base/src/WWWS0100.cbl](base/src/WWWS0100.cbl)</SwmPath>)

### Copybooks

- XXXN001A (<SwmPath>[base/src/XXXN001A.cpy](base/src/XXXN001A.cpy)</SwmPath>)
- WWWC0100 (<SwmPath>[base/src/WWWC0100.cpy](base/src/WWWC0100.cpy)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  jlgaj("Table IO Object for Location Class Advertisement Zones (NNNS0473)") --> yxmoe("Master Data Transitional Control (WWWS0100)"):::currentEntity
click jlgaj openCode "base/src/NNNS0473.cbl:1"
1e0l3("Table IO Object for Department Data (NNNS0573)") --> yxmoe("Master Data Transitional Control (WWWS0100)"):::currentEntity
click 1e0l3 openCode "base/src/NNNS0573.cbl:1"
a95p5("Managing Location Data (NNNS0487)") --> yxmoe("Master Data Transitional Control (WWWS0100)"):::currentEntity
click a95p5 openCode "base/src/NNNS0487.cbl:1"
p6oaq("Retail Location Data Processing (NNNS0488)") --> yxmoe("Master Data Transitional Control (WWWS0100)"):::currentEntity
click p6oaq openCode "base/src/NNNS0488.cbl:1"
  
  
click yxmoe openCode "base/src/WWWS0100.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   jlgaj("Table IO Object for Location Class Advertisement Zones (NNNS0473)") --> yxmoe("Master Data Transitional Control (WWWS0100)"):::currentEntity
%% click jlgaj openCode "<SwmPath>[base/src/NNNS0473.cbl](base/src/NNNS0473.cbl)</SwmPath>:1"
%% 1e0l3("Table IO Object for Department Data (NNNS0573)") --> yxmoe("Master Data Transitional Control (WWWS0100)"):::currentEntity
%% click 1e0l3 openCode "<SwmPath>[base/src/NNNS0573.cbl](base/src/NNNS0573.cbl)</SwmPath>:1"
%% a95p5("Managing Location Data (NNNS0487)") --> yxmoe("Master Data Transitional Control (WWWS0100)"):::currentEntity
%% click a95p5 openCode "<SwmPath>[base/src/NNNS0487.cbl](base/src/NNNS0487.cbl)</SwmPath>:1"
%% p6oaq("Retail Location Data Processing (NNNS0488)") --> yxmoe("Master Data Transitional Control (WWWS0100)"):::currentEntity
%% click p6oaq openCode "<SwmPath>[base/src/NNNS0488.cbl](base/src/NNNS0488.cbl)</SwmPath>:1"
%%   
%%   
%% click yxmoe openCode "<SwmPath>[base/src/WWWS0100.cbl](base/src/WWWS0100.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
