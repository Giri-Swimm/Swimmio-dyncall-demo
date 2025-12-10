---
title: Time and Timestamp Conversion Utility (MMMS0291) - Overview
---
# Overview

This document explains the flow for converting between time and timestamp formats for up to 14 entries, ensuring each entry is validated and formatted according to business rules. The conversion utility supports consistent handling of time and timestamp data across the system.

## Dependencies

### Program

- MMMS0291 (<SwmPath>[base/src/MMMS0291.cbl](base/src/MMMS0291.cbl)</SwmPath>)

### Copybooks

- SQLCA
- XXXN001A (<SwmPath>[base/src/XXXN001A.cpy](base/src/XXXN001A.cpy)</SwmPath>)
- MMMC0291 (<SwmPath>[base/src/MMMC0291.cpy](base/src/MMMC0291.cpy)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  7tp64("Managing Location Data (NNNS0487)") --> bdib5("Time and Timestamp Conversion Utility (MMMS0291)"):::currentEntity
click 7tp64 openCode "base/src/NNNS0487.cbl:1"
kbu92("Retail Location Data Processing (NNNS0488)") --> bdib5("Time and Timestamp Conversion Utility (MMMS0291)"):::currentEntity
click kbu92 openCode "base/src/NNNS0488.cbl:1"
  
  
click bdib5 openCode "base/src/MMMS0291.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   7tp64("Managing Location Data (NNNS0487)") --> bdib5("Time and Timestamp Conversion Utility (MMMS0291)"):::currentEntity
%% click 7tp64 openCode "<SwmPath>[base/src/NNNS0487.cbl](base/src/NNNS0487.cbl)</SwmPath>:1"
%% kbu92("Retail Location Data Processing (NNNS0488)") --> bdib5("Time and Timestamp Conversion Utility (MMMS0291)"):::currentEntity
%% click kbu92 openCode "<SwmPath>[base/src/NNNS0488.cbl](base/src/NNNS0488.cbl)</SwmPath>:1"
%%   
%%   
%% click bdib5 openCode "<SwmPath>[base/src/MMMS0291.cbl](base/src/MMMS0291.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
