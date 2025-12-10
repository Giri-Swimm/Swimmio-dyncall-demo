---
title: Space Cruncher (YYYS0134) - Overview
---
# Overview

This document describes the process of validating and transforming a text string to ensure only single spaces remain between words. The flow is used to standardize concatenated text fields, such as names, for clean output.

## Dependencies

### Program

- YYYS0134 (<SwmPath>[base/src/YYYS0134.cbl](base/src/YYYS0134.cbl)</SwmPath>)

### Copybooks

- YYYN000A (<SwmPath>[base/src/YYYN000A.cpy](base/src/YYYN000A.cpy)</SwmPath>)
- XXXN001A (<SwmPath>[base/src/XXXN001A.cpy](base/src/XXXN001A.cpy)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  wzsko("Translating Store Location Data (MMMS0158)") --> idrdt("Space Cruncher (YYYS0134)"):::currentEntity
click wzsko openCode "base/src/MMMS0158.cbl:1"
roylc("Managing Location Data (NNNS0487)") --> idrdt("Space Cruncher (YYYS0134)"):::currentEntity
click roylc openCode "base/src/NNNS0487.cbl:1"
  
  
click idrdt openCode "base/src/YYYS0134.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   wzsko("Translating Store Location Data (MMMS0158)") --> idrdt("Space Cruncher (YYYS0134)"):::currentEntity
%% click wzsko openCode "<SwmPath>[base/src/MMMS0158.cbl](base/src/MMMS0158.cbl)</SwmPath>:1"
%% roylc("Managing Location Data (NNNS0487)") --> idrdt("Space Cruncher (YYYS0134)"):::currentEntity
%% click roylc openCode "<SwmPath>[base/src/NNNS0487.cbl](base/src/NNNS0487.cbl)</SwmPath>:1"
%%   
%%   
%% click idrdt openCode "<SwmPath>[base/src/YYYS0134.cbl](base/src/YYYS0134.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
