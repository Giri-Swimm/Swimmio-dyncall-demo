---
title: >-
  Validating Department, District, and Division Lookup Tables (XXXP12DK) -
  Dependencies
---
# Dependencies

```mermaid
graph TD
  
  pn74h("Validating Department, District, and Division Lookup Tables (XXXP12DK)"):::currentEntity --> wn52q("TTTS2001")
click wn52q openCode "base/src/TTTS2001.cbl:1"
  wn52q("TTTS2001") --> fnxyn("TTTP2002")
click fnxyn openCode "base/src/TTTP2002.cbl:1"
  fnxyn("TTTP2002") --> u661c("DSNTIAR")
  
  
  
wn52q("TTTS2001") --> a99be("SSSABEND")
  
  
  
  
click pn74h openCode "base/src/XXXP12DK.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   
%%   pn74h("Validating Department, District, and Division Lookup Tables (XXXP12DK)"):::currentEntity --> wn52q("TTTS2001")
%% click wn52q openCode "<SwmPath>[base/src/TTTS2001.cbl](base/src/TTTS2001.cbl)</SwmPath>:1"
%%   wn52q("TTTS2001") --> fnxyn("TTTP2002")
%% click fnxyn openCode "<SwmPath>[base/src/TTTP2002.cbl](base/src/TTTP2002.cbl)</SwmPath>:1"
%%   fnxyn("TTTP2002") --> u661c("DSNTIAR")
%%   
%%   
%%   
%% wn52q("TTTS2001") --> a99be("SSSABEND")
%%   
%%   
%%   
%%   
%% click pn74h openCode "<SwmPath>[base/src/XXXP12DK.cbl](base/src/XXXP12DK.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/TTTS2001.cbl](base/src/TTTS2001.cbl)</SwmPath>

<SwmPath>[base/src/TTTP2002.cbl](base/src/TTTP2002.cbl)</SwmPath>

<SwmPath>[base/src/TTTN2001.cpy](base/src/TTTN2001.cpy)</SwmPath>

<SwmPath>[base/src/WXXN001A.cpy](base/src/WXXN001A.cpy)</SwmPath>

<SwmPath>[base/src/NNNN0000.cpy](base/src/NNNN0000.cpy)</SwmPath>

<SwmPath>[base/src/YYYN005A.cpy](base/src/YYYN005A.cpy)</SwmPath>

<SwmPath>[base/src/XXXN001A.cpy](base/src/XXXN001A.cpy)</SwmPath>

<SwmPath>[base/src/WWWC0003.cpy](base/src/WWWC0003.cpy)</SwmPath>

<SwmPath>[base/src/WWWC0040.cpy](base/src/WWWC0040.cpy)</SwmPath>

<SwmPath>[base/src/WWWC0100.cpy](base/src/WWWC0100.cpy)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1keW5jYWxsLWRlbW8lM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-dyncall-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
