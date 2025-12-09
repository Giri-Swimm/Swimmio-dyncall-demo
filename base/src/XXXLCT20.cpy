        01  XXXLCT20.                                      
          03  CT-DEPARTMENT-NAME-TABLE.                    
            05  CT-DEPARTMENT-SEQUENCE         PIC X(3).   
              88  CT-DEPARTMENT-SEQUENCE-VALUE VALUE '200'.
            05  ST-DEPARTMENT-KEY              PIC X(3).   
            05  FILLER                         PIC X(04).  
            05  CT-DEPARTMENT-NAME             PIC X(30).  
            05  ST-DEPARTMENT-ABBREVIATION     PIC X(2).   
              88  ST-BAKERY                    VALUE 'BK'. 
              88  ST-DELI                      VALUE 'DL'. 
              88  ST-GM                        VALUE 'GM'. 
              88  ST-GROCERY                   VALUE 'GR'. 
              88  ST-HEALTH-BEAUTY-AIDS        VALUE 'HB'. 
              88  ST-HOUSEWARES                VALUE 'HW'. 
              88  ST-MARKET                    VALUE 'MK'. 
              88  ST-PHARMACY                  VALUE 'PH'. 
              88  ST-PRODUCE                   VALUE 'PR'. 
              88  ST-SHOES                     VALUE 'SH'. 
              88  ST-WEARABLES                 VALUE 'WR'. 
            05  OA-REPT-GRP-CD                 PIC 9(2).               
            05  OA-GRS-PRFT-LO-PCT             PIC S9(03)V9(02) COMP-3.
            05  OA-GRS-PRFT-HI-PCT             PIC S9(03)V9(02) COMP-3.
            05  OA-SHRINK-LO-PCT               PIC S9(03)V9(02) COMP-3.
            05  OA-SHRINK-HI-PCT               PIC S9(03)V9(02) COMP-3.
            05  FILLER                         PIC X(34).              