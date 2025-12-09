000300*----------------------------------------------------------------
000200* This copybook is unique to WWWS0003.                           
000300* ---------------------------------------------------------------
000400 01 WWWC0003.                                                    
000500     05 WWWC0003-LR-CURRENT-SW        PIC X(1)   VALUE SPACES.   
000700         88 WWWC0003-RFCTRL01-NOT-CURRENT        VALUE ' '.      
000710         88 WWWC0003-RFCTRL01-IS-CURRENT         VALUE 'X'.      
000800     05 FILLER                        PIC X(100) VALUE SPACES.   