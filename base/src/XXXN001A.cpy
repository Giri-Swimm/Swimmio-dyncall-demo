01 XXXN001A.                                      
   05 IS-RTRN-CD              PIC 9.              
      88 SUCCESS                      VALUE 0.    
      88 BUENO                        VALUE 0.    
      88 COOL                         VALUE 0.    
      88 FAILURE                      VALUE 1.    
      88 NO-BUENO                     VALUE 1.    
      88 NOT-COOL                     VALUE 1.    
   05 IS-RTRN-MSG-TXT         PIC X(80).          
   05 IS-RTRN-MSG2-TXT        PIC X(176).         
   05 IS-ERR-RSN-CD           PIC S9(9) COMP SYNC.
   05 IS-MIDDLEWARE-SW        PIC X VALUE 'N'.    
      88 MIDDLEWARE-USED            VALUE 'Y'.    
      88 NO-MIDDLEWARE              VALUE 'N'.    