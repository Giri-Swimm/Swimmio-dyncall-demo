---
title: DB2 Error Output Processing (TTTP2002) - Overview
---
# Overview

This document describes the flow for handling DB2 errors. When an error occurs, the system prepares for error processing, determines the appropriate actions, formats and logs error messages, and communicates the outcome through a return code.

## Dependencies

### Programs

- TTTP2002 (<SwmPath>[base/src/TTTP2002.cbl](base/src/TTTP2002.cbl)</SwmPath>)
- DSNTIAR

### Copybooks

- SQLCA
- T01N2001

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  fa9s7("Error Diagnostic Handler (TTTS2001)") --> i99bi("DB2 Error Output Processing (TTTP2002)"):::currentEntity
click fa9s7 openCode "base/src/TTTS2001.cbl:1"
  
  
click i99bi openCode "base/src/TTTP2002.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   fa9s7("Error Diagnostic Handler (TTTS2001)") --> i99bi("DB2 Error Output Processing (TTTP2002)"):::currentEntity
%% click fa9s7 openCode "<SwmPath>[base/src/TTTS2001.cbl](base/src/TTTS2001.cbl)</SwmPath>:1"
%%   
%%   
%% click i99bi openCode "<SwmPath>[base/src/TTTP2002.cbl](base/src/TTTP2002.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
