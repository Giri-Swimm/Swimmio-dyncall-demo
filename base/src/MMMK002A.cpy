000100 01 PRODUCT-HIERARCHY.                                            00000100
000200   05 DEPARTMENT1                      PIC X(2).                  00000200
000300     88 ALPHA-A-DEPARTMENT             VALUE 'A'.                 00000300
000400     88 ALPHA-B-DEPARTMENT             VALUE 'B'.                 00000400
000500     88 ALPHA-C-DEPARTMENT             VALUE 'C'.                 00000500
000600     88 ALPHA-D-DEPARTMENT             VALUE 'D'.                 00000600
000700     88 ALPHA-E-DEPARTMENT             VALUE 'E'.                 00000700
000800     88 ALPHA-F-DEPARTMENT             VALUE 'F'.                 00000800
000900     88 ALPHA-G-DEPARTMENT             VALUE 'G'.                 00000900
001000     88 ALPHA-H-DEPARTMENT             VALUE 'H'.                 00001000
001100     88 ALPHA-I-DEPARTMENT             VALUE 'I'.                 00001100
001200     88 ALPHA-J-DEPARTMENT             VALUE 'J'.                 00001200
001300     88 ALPHA-K-DEPARTMENT             VALUE 'K'.                 00001300
001400     88 ALPHA-L-DEPARTMENT             VALUE 'L'.                 00001400
001500     88 DEPARTMENT-ZERO                VALUE '0'.                 00001500
001600     88 DEPARTMENT-00                  VALUE '00'.                00001600
001700     88 TOTAL-STORE                    VALUE '01'.                00001700
001800     88 MARKET                         VALUE '02'.                00001800
001900     88 BAKERY                         VALUE '03'.                00001900
002000     88 DRUG-STORE                     VALUE '04'.                00002000
002100     88 PHARMACY                       VALUE '05'.                00002100
002200     88 DELI                           VALUE '06'.                00002200
002300     88 GROCERY                        VALUE '07'.                00002300
002400     88 SERVICES                       VALUE '08'.                00002400
002500     88 PRODUCE                        VALUE '09'.                00002500
002600     88 STORE-SERVICES                 VALUE '10'.                00002600
002700     88 SERVICES-CO                    VALUE '11'.                00002700
002800     88 GENERAL-MERCHAN                VALUE '12'.                00002800
002900     88 KOSHER-STORE                   VALUE '13'.                00002900
003000     88 GREATS                         VALUE '14'.                00003000
003100     88 CM-CHEF-PREPARED               VALUE '15'.                00003100
003200     88 HEALTHY-LIVING                 VALUE '16'.                00003200
003300     88 GIFT-BASKETS                   VALUE '17'.                00003300
003400     88 E-40-KITCHEN                   VALUE '18'.                00003400
003500     88 E-40-CHICKEN                   VALUE '19'.                00003500
003600     88 BARGAIN-BASEMENT               VALUE '20'.                00003600
003700     88 OPTICAL                        VALUE '21'.                00003700
003800     88 SEAFOOD                        VALUE '23'.                00003800
003810     88 BLOOMS                         VALUE '24'.                00003810
003900                                                                  00003900
004000   05 SUB-DEPARTMENT1                  PIC X(1).                  00004000
004100     88 SUB-DEPTA                      VALUE 'A'.                 00004100
004200     88 SUB-DEPTB                      VALUE 'B'.                 00004200
004300     88 SUB-DEPTC                      VALUE 'C'.                 00004300
004400     88 SUB-DEPTD                      VALUE 'D'.                 00004400
004500     88 SUB-DEPTE                      VALUE 'E'.                 00004500
004600     88 SUB-DEPTF                      VALUE 'F'.                 00004600
004700     88 SUB-DEPTG                      VALUE 'G'.                 00004700
004800     88 SUB-DEPTH                      VALUE 'H'.                 00004800
004900     88 SUB-DEPTI                      VALUE 'I'.                 00004900
005000     88 SUB-DEPTJ                      VALUE 'J'.                 00005000
005100     88 SUB-DEPTK                      VALUE 'K'.                 00005100
005200     88 SUB-DEPTL                      VALUE 'L'.                 00005200
005300     88 SUB-DEPTM                      VALUE 'M'.                 00005300
005400     88 SUB-DEPTN                      VALUE 'N'.                 00005400
005500     88 SUB-DEPTO                      VALUE 'O'.                 00005500
005600     88 SUB-DEPTP                      VALUE 'P'.                 00005600
005700     88 SUB-DEPTQ                      VALUE 'Q'.                 00005700
005800     88 SUB-DEPTR                      VALUE 'R'.                 00005800
005900     88 SUB-DEPTS                      VALUE 'S'.                 00005900
006000     88 SUB-DEPTT                      VALUE 'T'.                 00006000
006100     88 SUB-DEPTU                      VALUE 'U'.                 00006100
006200     88 SUB-DEPTV                      VALUE 'V'.                 00006200
006300     88 SUB-DEPTW                      VALUE 'W'.                 00006300
006400     88 SUB-DEPTX                      VALUE 'X'.                 00006400
006500     88 SUB-DEPTY                      VALUE 'Y'.                 00006500
006600     88 SUB-DEPTZ                      VALUE 'Z'.                 00006600
006700                                                                  00006700
006800   05 DEPARTMENT2              PIC X(3).                          00006800
006900   05 REDEFINES DEPARTMENT2.                                      00006900
007000               10 DEPT1        PIC 99.                            00007000
007100               10 SUB-DEPT1    PIC X.                             00007100
007200                                                                  00007200
007300   05 CLASS1                   PIC 9(3).                          00007300
007400     88 GROCERY-MDSE                   VALUE  1.                  00007400
007500     88 MEAT-MERCHADSE                 VALUE  2.                  00007500
007600     88 WINE                           VALUE  3.                  00007600
007700     88 BULK-FOODS                     VALUE  4.                  00007700
007800     88 GROCERY-WRAP                   VALUE  5.                  00007800
007900     88 HOUSEHOLD-CARE                 VALUE  6.                  00007900
008000     88 PET-CARE                       VALUE  7.                  00008000
008100     88 SNACKS                         VALUE  8.                  00008100
008200     88 DAIRY-MERCHANDISE              VALUE  9.                  00008200
008300     88 BEER                           VALUE 10.                  00008300
008400     88 PHOTO-PLANT                    VALUE 11.                  00008400
008500     88 BEEF-AND-LAMB                  VALUE 12.                  00008500
008600     88 FRESH-POULTRY                  VALUE 13.                  00008600
008700     88 FRESH-PORK                     VALUE 14.                  00008700
008810     88 TORTILLERIA-DRY                VALUE 15.                  00008810
008900     88 NON-FROZEN-BAKERY              VALUE 16.                  00008900
009000     88 BAKED-GOODS                    VALUE 17.                  00009000
009100     88 ICE-CREAM-PLANT                VALUE 18.                  00009100
009200     88 FRESH-MILK                     VALUE 19.                  00009200
009300     88 FRESH-SEAFOOD                  VALUE 20.                  00009300
009400     88 REFRIGERATED-SEAFOOD           VALUE 21.                  00009400
009500     88 FROZEN-SEAFOOD                 VALUE 22.                  00009500
009600     88 PREPARED-FOODS-FROZEN          VALUE 23.                  00009600
009700     88 BAKERY-CITY-KITCHEN            VALUE 24.                  00009700
009800     88 FROZEN-BAKERY                  VALUE 25.                  00009800
009900     88 FROZEN-DELI                    VALUE 26.                  00009900
010000     88 DELI-REFRIGERATED              VALUE 27.                  00010000
010100     88 NEW-VENTURES                   VALUE 28.                  00010100
010200     88 DELI-SCAN-BASE-TRADE           VALUE 29.                  00010200
010300     88 FLORAL-ACCESSORIES             VALUE 30.                  00010300
010400     88 PET-CARE-FROZEN                VALUE 31.                  00010400
010500     88 PHARMACY-CLS                   VALUE 32.                  00010500
010600     88 PHARMACY-WRAP                  VALUE 33.                  00010600
010700     88 CENTRAL-FILL                   VALUE 34.                  00010700
010800     88 DAIRY                          VALUE 35.                  00010800
010900     88 NON-FROZEN-MARKET              VALUE 36.                  00010900
011000     88 FROZEN-MARKET                  VALUE 37.                  00011000
011100     88 PLANTS                         VALUE 38.                  00011100
011200     88 FROZEN-GROCERY                 VALUE 39.                  00011200
011300     88 SNACKS-SELF-MAN                VALUE 40.                  00011300
011400     88 DELI-WRAP                      VALUE 41.                  00011400
011500     88 FRESH-PRODUCE                  VALUE 42.                  00011500
011600     88 PRODUCE-WRAP                   VALUE 43.                  00011600
011700     88 BEVERAGE                       VALUE 44.                  00011700
011800     88 CENTRAL-MARKET-PREP-FOOD       VALUE 45.                  00011800
011900     88 PREPARED-FOODS-WRAP            VALUE 46.                  00011900
012000     88 SEAFOOD-WRAP                   VALUE 47.                  00012000
012100     88 FRESH-FLOWERS                  VALUE 48.                  00012100
012200*    88 GM-4V-WRAP                     VALUE 49.                  00012200
012210     88 ESTORE-SUPPLY                  VALUE 49.                  00012210
012300     88 UNIFORMS                       VALUE 50.                  00012300
012400     88 MARKET-SUPPLY                  VALUE 51.                  00012400
012500     88 XXX-GIFT-CARDS                 VALUE 52.                  00012500
012610     88 RESTAURANT-WRAP                VALUE 53.                  00012610
012700     88 BAKE-SHOP-WRAP                 VALUE 54.                  00012700
012800     88 STORE-SUPPLY                   VALUE 55.                  00012800
012900     88 MARKET-WRAP                    VALUE 56.                  00012900
013000     88 SERVICES-CLS                   VALUE 57.                  00013000
013100*    88 GM-4P-WRAP                     VALUE 58.                  00013100
013110     88 ESTORE-WRAP                    VALUE 58.                  00013110
013200     88 GM-4A-WRAP                     VALUE 59.                  00013200
013300     88 CANDY                          VALUE 60.                  00013300
013400     88 OPTICAL-CLS                    VALUE 61.                  00013400
013500     88 TOBACCO                        VALUE 62.                  00013500
013600     88 CAMERAS-AND-ACCES              VALUE 63.                  00013600
013700     88 COSMETICS                      VALUE 64.                  00013700
013800     88 READY-TO-SELL-BAK              VALUE 65.                  00013800
013900     88 HOME                           VALUE 66.                  00013900
014000     88 HEALTHY-LIVING-CLS             VALUE 67.                  00014000
014100     88 HEALTH-AIDS                    VALUE 68.                  00014100
014200     88 TOILETRIES                     VALUE 69.                  00014200
014300     88 MADE-TO-ORDER-REF              VALUE 70.                  00014300
014400     88 TRUE-TEXAS-BBQ                 VALUE 71.                  00014400
014500     88 SOFTLINES                      VALUE 72.                  00014500
014600     88 FLAMING-BIRDS                  VALUE 73.                  00014600
014700     88 TEXAS-BACKYARD                 VALUE 74.                  00014700
014800     88 SCHOOL-OFFICE-SUPPLIES         VALUE 75.                  00014800
014900     88 SUSHI-FROZEN                   VALUE 76.                  00014900
015000     88 ENTERTAINMENT                  VALUE 77.                  00015000
015100     88 A-B-C-MOBILE                   VALUE 78.                  00015100
015200     88 BABY                           VALUE 79.                  00015200
015300     88 COOK-N-GRILL                   VALUE 80.                  00015300
015400     88 NON-FROZEN-TORTILLERIA         VALUE 81.                  00015400
015500     88 FROZEN-TORTILLERIA             VALUE 82.                  00015500
015600     88 SUSHI-FRESH                    VALUE 83.                  00015600
015700     88 AUTOMOTIVE                     VALUE 84.                  00015700
015800     88 HARDWARE                       VALUE 85.                  00015800
015900     88 SPORTING-GOODS                 VALUE 86.                  00015900
016000     88 TORTILLAS-CHIPS                VALUE 87.                  00016000
016100     88 BREAD-BUN-PLANT                VALUE 88.                  00016100
016200     88 PASTRY-PLANT                   VALUE 89.                  00016200
016300     88 TOYS                           VALUE 90.                  00016300
016400     88 MIAS-MIRROR                    VALUE 91.                  00016400
016500     88 MIAS-MIRROR-WRAP               VALUE 92.                  00016500
016600     88 ALCOHOLIC-BEVERAGES            VALUE 93.                  00016600
016700     88 NOT-USED10                     VALUE 94.                  00016700
016800     88 TEXAS-BACKYARD-HARD-GOODS      VALUE 95.                  00016800
016900     88 NOT-USED11                     VALUE 96.                  00016900
017000     88 BASIC-GM                       VALUE 97.                  00017000
017100     88 CARD-AND-PARTY                 VALUE 98.                  00017100
017200     88 DRUGSTORE                      VALUE 99.                  00017200
017300                                                                  00017300
017400******************************************************************00017400
017500*          ADDED AS PART OF J30P67WK PROGRAM CHANGES             *00017500
017600******************************************************************00017600
017700  01  DEPARTMENT-SEQUENCE-VALUES.                                 00017700
017800      03  FILLER           PIC 9(2)      VALUE   07.              00017800
017900      03  FILLER           PIC X(1)      VALUE  'A'.              00017900
018000      03  FILLER           PIC X(20)     VALUE                    00018000
018100          'GROCERY-A           '.                                 00018100
018200      03  FILLER           PIC 9(2)      VALUE   07.              00018200
018300      03  FILLER           PIC X(1)      VALUE  'B'.              00018300
018400      03  FILLER           PIC X(20)     VALUE                    00018400
018500          'GROCERY-B(BREAD/PAS)'.                                 00018500
018600      03  FILLER           PIC 9(2)      VALUE   07.              00018600
018700      03  FILLER           PIC X(1)      VALUE  'C'.              00018700
018800      03  FILLER           PIC X(20)     VALUE                    00018800
018900          'GROCERY-C(BEER/WINE)'.                                 00018900
019000      03  FILLER           PIC 9(2)      VALUE   07.              00019000
019100      03  FILLER           PIC X(1)      VALUE  'D'.              00019100
019200      03  FILLER           PIC X(20)     VALUE                    00019200
019300          'GROCERY-D(DAIRY)    '.                                 00019300
019400      03  FILLER           PIC 9(2)      VALUE   07.              00019400
019500      03  FILLER           PIC X(1)      VALUE  'E'.              00019500
019600      03  FILLER           PIC X(20)     VALUE                    00019600
019700          'NATURE"S HARVEST    '.                                 00019700
019800      03  FILLER           PIC 9(2)      VALUE   07.              00019800
019900      03  FILLER           PIC X(1)      VALUE  'F'.              00019900
020000      03  FILLER           PIC X(20)     VALUE                    00020000
020100          'GROCERY-F(FROZEN FD)'.                                 00020100
020200      03  FILLER           PIC 9(2)      VALUE   07.              00020200
020300      03  FILLER           PIC X(1)      VALUE  'H'.              00020300
020400      03  FILLER           PIC X(20)     VALUE                    00020400
020500          'BULK FOODS          '.                                 00020500
020600      03  FILLER           PIC 9(2)      VALUE   02.              00020600
020700      03  FILLER           PIC X(1)      VALUE  ' '.              00020700
020800      03  FILLER           PIC X(20)     VALUE                    00020800
020900          'MARKET              '.                                 00020900
021000      03  FILLER           PIC 9(2)      VALUE   02.              00021000
021100      03  FILLER           PIC X(1)      VALUE  'A'.              00021100
021200      03  FILLER           PIC X(20)     VALUE                    00021200
021300          'MARKET-DELI/FROZEN  '.                                 00021300
021400      03  FILLER           PIC 9(2)      VALUE   02.              00021400
021500      03  FILLER           PIC X(1)      VALUE  'G'.              00021500
021600      03  FILLER           PIC X(20)     VALUE                    00021600
021700          'MARKET-SMOKESHOP    '.                                 00021700
021800      03  FILLER           PIC 9(2)      VALUE   02.              00021800
021900      03  FILLER           PIC X(1)      VALUE  'M'.              00021900
022000      03  FILLER           PIC X(20)     VALUE                    00022000
022100          'MARKET-MEAT         '.                                 00022100
022200      03  FILLER           PIC 9(2)      VALUE   02.              00022200
022300      03  FILLER           PIC X(1)      VALUE  'S'.              00022300
022400      03  FILLER           PIC X(20)     VALUE                    00022400
022500          'MARKET-SEAFOOD      '.                                 00022500
022600      03  FILLER           PIC 9(2)      VALUE   09.              00022600
022700      03  FILLER           PIC X(1)      VALUE  'A'.              00022700
022800      03  FILLER           PIC X(20)     VALUE                    00022800
022900          'PRODUCE A(FRESH)    '.                                 00022900
023000      03  FILLER           PIC 9(2)      VALUE   24.              00023000
023100      03  FILLER           PIC X(1)      VALUE  'A'.              00023100
023200      03  FILLER           PIC X(20)     VALUE                    00023200
023300          'BLOOMS  A(BLOOMS)   '.                                 00023300
023400      03  FILLER           PIC 9(2)      VALUE   04.              00023400
023500      03  FILLER           PIC X(1)      VALUE  'A'.              00023500
023600      03  FILLER           PIC X(20)     VALUE                    00023600
023700          'DRUG STORE          '.                                 00023700
023800      03  FILLER           PIC 9(2)      VALUE   04.              00023800
023900      03  FILLER           PIC X(1)      VALUE  'B'.              00023900
024000      03  FILLER           PIC X(20)     VALUE                    00024000
024100          'DRUG STORE (BABY)   '.                                 00024100
024200      03  FILLER           PIC 9(2)      VALUE   04.              00024200
024300      03  FILLER           PIC X(1)      VALUE  'C'.              00024300
024400      03  FILLER           PIC X(20)     VALUE                    00024400
024500          'DRUG STORE (BEAUTY) '.                                 00024500
024600      03  FILLER           PIC 9(2)      VALUE   04.              00024600
024700      03  FILLER           PIC X(1)      VALUE  'F'.              00024700
024800      03  FILLER           PIC X(20)     VALUE                    00024800
024900          'GM-F (FILM)         '.                                 00024900
025000      03  FILLER           PIC 9(2)      VALUE   04.              00025000
025100      03  FILLER           PIC X(1)      VALUE  'P'.              00025100
025200      03  FILLER           PIC X(20)     VALUE                    00025200
025300          'DRUG STORE (PHOTO)  '.                                 00025300
025400      03  FILLER           PIC 9(2)      VALUE   04.              00025400
025500      03  FILLER           PIC X(1)      VALUE  'S'.              00025500
025600      03  FILLER           PIC X(20)     VALUE                    00025600
025700          'GM-S (SELL THRU)    '.                                 00025700
025800      03  FILLER           PIC 9(2)      VALUE   04.              00025800
025900      03  FILLER           PIC X(1)      VALUE  'T'.              00025900
026000      03  FILLER           PIC X(20)     VALUE                    00026000
026100          'GM-T (TOBACCO)      '.                                 00026100
026200      03  FILLER           PIC 9(2)      VALUE   04.              00026200
026300      03  FILLER           PIC X(1)      VALUE  'V'.              00026300
026400      03  FILLER           PIC X(20)     VALUE                    00026400
026500          'GM-V (VIDEO)        '.                                 00026500
026600      03  FILLER           PIC 9(2)      VALUE   03.              00026600
026700      03  FILLER           PIC X(1)      VALUE  'A'.              00026700
026800      03  FILLER           PIC X(20)     VALUE                    00026800
026900          'BAKERY-A            '.                                 00026900
027000      03  FILLER           PIC 9(2)      VALUE   03.              00027000
027100      03  FILLER           PIC X(1)      VALUE  'B'.              00027100
027200      03  FILLER           PIC X(20)     VALUE                    00027200
027300          'BAKERY-B            '.                                 00027300
027400      03  FILLER           PIC 9(2)      VALUE   03.              00027400
027500      03  FILLER           PIC X(1)      VALUE  'C'.              00027500
027600      03  FILLER           PIC X(20)     VALUE                    00027600
027700          'BAKERY-C (RTS)      '.                                 00027700
027800      03  FILLER           PIC 9(2)      VALUE   03.              00027800
027900      03  FILLER           PIC X(1)      VALUE  'T'.              00027900
028000      03  FILLER           PIC X(20)     VALUE                    00028000
028100          'BAKERY-T (TORTILLA) '.                                 00028100
028200      03  FILLER           PIC 9(2)      VALUE   06.              00028200
028300      03  FILLER           PIC X(1)      VALUE  'A'.              00028300
028400      03  FILLER           PIC X(20)     VALUE                    00028400
028500          'DELI                '.                                 00028500
028600      03  FILLER           PIC 9(2)      VALUE   06.              00028600
028700      03  FILLER           PIC X(1)      VALUE  'C'.              00028700
028800      03  FILLER           PIC X(20)     VALUE                    00028800
028900          'DELI-FOOD SERVICE   '.                                 00028900
029000      03  FILLER           PIC 9(2)      VALUE   06.              00029000
029100      03  FILLER           PIC X(1)      VALUE  'D'.              00029100
029200      03  FILLER           PIC X(20)     VALUE                    00029200
029300          'DELI-CHEESE         '.                                 00029300
029400      03  FILLER           PIC 9(2)      VALUE   06.              00029400
029500      03  FILLER           PIC X(1)      VALUE  'P'.              00029500
029600      03  FILLER           PIC X(20)     VALUE                    00029600
029700          'DELI-ITALIAN KITCHEN'.                                 00029700
029800      03  FILLER           PIC 9(2)      VALUE   05.              00029800
029900      03  FILLER           PIC X(1)      VALUE  'A'.              00029900
030000      03  FILLER           PIC X(20)     VALUE                    00030000
030100          'PHARMACY            '.                                 00030100
030200      03  FILLER           PIC 9(2)      VALUE   01.              00030200
030300      03  FILLER           PIC X(1)      VALUE  'B'.              00030300
030400      03  FILLER           PIC X(20)     VALUE                    00030400
030500          'GASOLINE            '.                                 00030500
030600      03  FILLER           PIC 9(2)      VALUE   12.              00030600
030700      03  FILLER           PIC X(1)      VALUE  'A'.              00030700
030800      03  FILLER           PIC X(20)     VALUE                    00030800
030900          'GM/SEASONAL         '.                                 00030900
031000      03  FILLER           PIC 9(2)      VALUE   12.              00031000
031100      03  FILLER           PIC X(1)      VALUE  'E'.              00031100
031200      03  FILLER           PIC X(20)     VALUE                    00031200
031300          'ENTERTAINMENT       '.                                 00031300
031400      03  FILLER           PIC 9(2)      VALUE   12.              00031400
031500      03  FILLER           PIC X(1)      VALUE  'F'.              00031500
031600      03  FILLER           PIC X(20)     VALUE                    00031600
031700          'FLORAL              '.                                 00031700
031800      03  FILLER           PIC 9(2)      VALUE   12.              00031800
031900      03  FILLER           PIC X(1)      VALUE  'H'.              00031900
032000      03  FILLER           PIC X(20)     VALUE                    00032000
032100          'TEXAS BACKYARD-HARD '.                                 00032100
032200      03  FILLER           PIC 9(2)      VALUE   12.              00032200
032300      03  FILLER           PIC X(1)      VALUE  'L'.              00032300
032400      03  FILLER           PIC X(20)     VALUE                    00032400
032500          'TEXAS BACKYARD-LIVE '.                                 00032500
032600      03  FILLER           PIC 9(2)      VALUE   12.              00032600
032700      03  FILLER           PIC X(1)      VALUE  'P'.              00032700
032800      03  FILLER           PIC X(20)     VALUE                    00032800
032900          'CARD AND PARTY      '.                                 00032900
033000      03  FILLER           PIC 9(2)      VALUE   12.              00033000
033100      03  FILLER           PIC X(1)      VALUE  'T'.              00033100
033200      03  FILLER           PIC X(20)     VALUE                    00033200
033300          'TOBACCO             '.                                 00033300
033400      03  FILLER           PIC 9(2)      VALUE   13.              00033400
033500      03  FILLER           PIC X(1)      VALUE  'A'.              00033500
033600      03  FILLER           PIC X(20)     VALUE                    00033600
033700          'KOSHER              '.                                 00033700
033800      03  FILLER           PIC 9(2)      VALUE   14.              00033800
033900      03  FILLER           PIC X(1)      VALUE  'A'.              00033900
034000      03  FILLER           PIC X(20)     VALUE                    00034000
034100          'GREAT"S!            '.                                 00034100
034200      03  FILLER           PIC 9(2)      VALUE   15.              00034200
034300      03  FILLER           PIC X(1)      VALUE  'A'.              00034300
034400      03  FILLER           PIC X(20)     VALUE                    00034400
034500          'CHEF PREPARED       '.                                 00034500
034600      03  FILLER           PIC 9(2)      VALUE   15.              00034600
034700      03  FILLER           PIC X(1)      VALUE  'B'.              00034700
034800      03  FILLER           PIC X(20)     VALUE                    00034800
034900          'COFFEE/SMOOTHIE BAR '.                                 00034900
035000      03  FILLER           PIC 9(2)      VALUE   15.              00035000
035100      03  FILLER           PIC X(1)      VALUE  'C'.              00035100
035200      03  FILLER           PIC X(20)     VALUE                    00035200
035300          '3RD PARTY COFFEE    '.                                 00035300
035400      03  FILLER           PIC 9(2)      VALUE   15.              00035400
035500      03  FILLER           PIC X(1)      VALUE  'D'.              00035500
035600      03  FILLER           PIC X(20)     VALUE                    00035600
035700          'COOKING SCHOOL      '.                                 00035700
035800      03  FILLER           PIC 9(2)      VALUE   15.              00035800
035900      03  FILLER           PIC X(1)      VALUE  'E'.              00035900
036000      03  FILLER           PIC X(20)     VALUE                    00036000
036100          'CAFE                '.                                 00036100
036200      03  FILLER           PIC 9(2)      VALUE   15.              00036200
036300      03  FILLER           PIC X(1)      VALUE  'F'.              00036300
036400      03  FILLER           PIC X(20)     VALUE                    00036400
036500          'BEER/WINE SERVICE   '.                                 00036500
036600      03  FILLER           PIC 9(2)      VALUE   15.              00036600
036700      03  FILLER           PIC X(1)      VALUE  'G'.              00036700
036800      03  FILLER           PIC X(20)     VALUE                    00036800
036900          'AGUAS               '.                                 00036900
037000      03  FILLER           PIC 9(2)      VALUE   15.              00037000
037100      03  FILLER           PIC X(1)      VALUE  'H'.              00037100
037200      03  FILLER           PIC X(20)     VALUE                    00037200
037300          'CEVICHERIA          '.                                 00037300
037400      03  FILLER           PIC 9(2)      VALUE   15.              00037400
037500      03  FILLER           PIC X(1)      VALUE  'I'.              00037500
037600      03  FILLER           PIC X(20)     VALUE                    00037600
037700          'COCINA              '.                                 00037700
037800      03  FILLER           PIC 9(2)      VALUE   16.              00037800
037900      03  FILLER           PIC X(1)      VALUE  'A'.              00037900
038000      03  FILLER           PIC X(20)     VALUE                    00038000
038100          'HEALTHY LIVING      '.                                 00038100
038200      03  FILLER           PIC 9(2)      VALUE   17.              00038200
038300      03  FILLER           PIC X(1)      VALUE  'A'.              00038300
038400      03  FILLER           PIC X(20)     VALUE                    00038400
038500          'GIFT BASKETS        '.                                 00038500
038600      03  FILLER           PIC 9(2)      VALUE   19.              00038600
038700      03  FILLER           PIC X(1)      VALUE  'A'.              00038700
038800      03  FILLER           PIC X(20)     VALUE                    00038800
038900          'THE ROOST           '.                                 00038900
039000      03  FILLER           PIC 9(2)      VALUE   12.              00039000
039100      03  FILLER           PIC X(1)      VALUE  'B'.              00039100
039200      03  FILLER           PIC X(20)     VALUE                    00039200
039300          'PREPAID PHONES      '.                                 00039300
039400      03  FILLER           PIC 9(2)      VALUE   09.              00039400
039500      03  FILLER           PIC X(1)      VALUE  'C'.              00039500
039600      03  FILLER           PIC X(20)     VALUE                    00039600
039700          'CUT FRUIT/FRUITDRINK'.                                 00039700
039800      03  FILLER           PIC 9(2)      VALUE   02.              00039800
039900      03  FILLER           PIC X(1)      VALUE  'H'.              00039900
040000      03  FILLER           PIC X(20)     VALUE                    00040000
040100          'SUSHI               '.                                 00040100
040200      03  FILLER           PIC 9(2)      VALUE   04.              00040200
040300      03  FILLER           PIC X(1)      VALUE  'G'.              00040300
040400      03  FILLER           PIC X(20)     VALUE                    00040400
040500          'HEALTHYLIVIN DRUGSTR'.                                 00040500
040600      03  FILLER           PIC 9(2)      VALUE   04.              00040600
040700      03  FILLER           PIC X(1)      VALUE  'H'.              00040700
040800      03  FILLER           PIC X(20)     VALUE                    00040800
040900          'HEALTHYLIVIN COSMETC'.                                 00040900
041000      03  FILLER           PIC 9(2)      VALUE   05.              00041000
041100      03  FILLER           PIC X(1)      VALUE  'B'.              00041100
041200      03  FILLER           PIC X(20)     VALUE                    00041200
041300          'RX PROF SERVICES'.                                     00041300
041400      03  FILLER           PIC 9(2)      VALUE   05.              00041400
041500      03  FILLER           PIC X(1)      VALUE  'C'.              00041500
041600      03  FILLER           PIC X(20)     VALUE                    00041600
041700          'RX OVER THE CNTR'.                                     00041700
041800      03  FILLER           PIC 9(2)      VALUE   07.              00041800
041900      03  FILLER           PIC X(1)      VALUE  'J'.              00041900
042000      03  FILLER           PIC X(20)     VALUE                    00042000
042100          'HEALTHY LIVING GROC'.                                  00042100
042200      03  FILLER           PIC 9(2)      VALUE   12.              00042200
042300      03  FILLER           PIC X(1)      VALUE  'C'.              00042300
042400      03  FILLER           PIC X(20)     VALUE                    00042400
042500          'CANDY'.                                                00042500
042600                                                                  00042600
042700 01  DEPARTMENT-SEQUENCE-TABLE          REDEFINES                 00042700
042800     DEPARTMENT-SEQUENCE-VALUES.                                  00042800
042900     03  DEPARTMENT-ENTRY               OCCURS 62.                00042900
043000         05  DST-DEPARTMENT-NUMBER      PIC 9(2).                 00043000
043100         05  DST-SUB-DEPARTMENT-NUMBER  PIC X(1).                 00043100
043200         05  DST-DEPARTMENT-NAME        PIC X(20).                00043200
043300                                                                  00043300
043400***************************************************************** 00043400
043500*          ADDED AS PART OF J30P59WK PROGRAM CHANGES             *00043500
043600******************************************************************00043600
043700  01  DEPARTMENT-SEQUENCE-VALUES1.                                00043700
043800      03  FILLER           PIC 9(2)      VALUE   07.              00043800
043900      03  FILLER           PIC X(1)      VALUE  'A'.              00043900
044000      03  FILLER           PIC X(20)     VALUE                    00044000
044100          'GROCERY-A           '.                                 00044100
044200      03  FILLER           PIC 9(2)      VALUE   07.              00044200
044300      03  FILLER           PIC X(1)      VALUE  'B'.              00044300
044400      03  FILLER           PIC X(20)     VALUE                    00044400
044500          'GROCERY-B(BREAD/PAS)'.                                 00044500
044600      03  FILLER           PIC 9(2)      VALUE   07.              00044600
044700      03  FILLER           PIC X(1)      VALUE  'C'.              00044700
044800      03  FILLER           PIC X(20)     VALUE                    00044800
044900          'GROCERY-C(BEER/WINE)'.                                 00044900
045000      03  FILLER           PIC 9(2)      VALUE   07.              00045000
045100      03  FILLER           PIC X(1)      VALUE  'D'.              00045100
045200      03  FILLER           PIC X(20)     VALUE                    00045200
045300          'GROCERY-D(DAIRY)    '.                                 00045300
045400      03  FILLER           PIC 9(2)      VALUE   07.              00045400
045500      03  FILLER           PIC X(1)      VALUE  'F'.              00045500
045600      03  FILLER           PIC X(20)     VALUE                    00045600
045700          'GROCERY-D(FROZEN FD)'.                                 00045700
045800      03  FILLER           PIC 9(2)      VALUE   02.              00045800
045900      03  FILLER           PIC X(1)      VALUE  ' '.              00045900
046000      03  FILLER           PIC X(20)     VALUE                    00046000
046100          'MARKET              '.                                 00046100
046200      03  FILLER           PIC 9(2)      VALUE   02.              00046200
046300      03  FILLER           PIC X(1)      VALUE  'A'.              00046300
046400      03  FILLER           PIC X(20)     VALUE                    00046400
046500          'MARKET-DELI/FROZEN  '.                                 00046500
046600      03  FILLER           PIC 9(2)      VALUE   02.              00046600
046700      03  FILLER           PIC X(1)      VALUE  'M'.              00046700
046800      03  FILLER           PIC X(20)     VALUE                    00046800
046900          'MARKET-MEAT         '.                                 00046900
047000      03  FILLER           PIC 9(2)      VALUE   02.              00047000
047100      03  FILLER           PIC X(1)      VALUE  'S'.              00047100
047200      03  FILLER           PIC X(20)     VALUE                    00047200
047300          'MARKET-SEAFOOD      '.                                 00047300
047400      03  FILLER           PIC 9(2)      VALUE   09.              00047400
047500      03  FILLER           PIC X(1)      VALUE  'A'.              00047500
047600      03  FILLER           PIC X(20)     VALUE                    00047600
047700          'PRODUCE A(FRESH)    '.                                 00047700
047800      03  FILLER           PIC 9(2)      VALUE   24.              00047800
047900      03  FILLER           PIC X(1)      VALUE  'A'.              00047900
048000      03  FILLER           PIC X(20)     VALUE                    00048000
048100          'BLOOMS  A(BLOOMS)   '.                                 00048100
048200      03  FILLER           PIC 9(2)      VALUE   04.              00048200
048300      03  FILLER           PIC X(1)      VALUE  'A'.              00048300
048400      03  FILLER           PIC X(20)     VALUE                    00048400
048500          'GM-A                '.                                 00048500
048600      03  FILLER           PIC 9(2)      VALUE   04.              00048600
048700      03  FILLER           PIC X(1)      VALUE  'C'.              00048700
048800      03  FILLER           PIC X(20)     VALUE                    00048800
048900          'GM-C (COSMETICS)    '.                                 00048900
049000      03  FILLER           PIC 9(2)      VALUE   04.              00049000
049100      03  FILLER           PIC X(1)      VALUE  'F'.              00049100
049200      03  FILLER           PIC X(20)     VALUE                    00049200
049300          'GM-C (FILM)         '.                                 00049300
049400      03  FILLER           PIC 9(2)      VALUE   04.              00049400
049500      03  FILLER           PIC X(1)      VALUE  'P'.              00049500
049600      03  FILLER           PIC X(20)     VALUE                    00049600
049700          'GM-P (PHOTO)        '.                                 00049700
049800      03  FILLER           PIC 9(2)      VALUE   04.              00049800
049900      03  FILLER           PIC X(1)      VALUE  'S'.              00049900
050000      03  FILLER           PIC X(20)     VALUE                    00050000
050100          'GM-S (SELL THRU)    '.                                 00050100
050200      03  FILLER           PIC 9(2)      VALUE   04.              00050200
050300      03  FILLER           PIC X(1)      VALUE  'T'.              00050300
050400      03  FILLER           PIC X(20)     VALUE                    00050400
050500          'GM-T (TOBACCO)      '.                                 00050500
050600      03  FILLER           PIC 9(2)      VALUE   04.              00050600
050700      03  FILLER           PIC X(1)      VALUE  'V'.              00050700
050800      03  FILLER           PIC X(20)     VALUE                    00050800
050900          'GM-V (VIDEO)        '.                                 00050900
051000      03  FILLER           PIC 9(2)      VALUE   03.              00051000
051100      03  FILLER           PIC X(1)      VALUE  'A'.              00051100
051200      03  FILLER           PIC X(20)     VALUE                    00051200
051300          'BAKERY-A            '.                                 00051300
051400      03  FILLER           PIC 9(2)      VALUE   06.              00051400
051500      03  FILLER           PIC X(1)      VALUE  'A'.              00051500
051600      03  FILLER           PIC X(20)     VALUE                    00051600
051700          'DELI                '.                                 00051700
051800      03  FILLER           PIC 9(2)      VALUE   06.              00051800
051900      03  FILLER           PIC X(1)      VALUE  'C'.              00051900
052000      03  FILLER           PIC X(20)     VALUE                    00052000
052100          'DELI-CHINESE KITCHEN'.                                 00052100
052200      03  FILLER           PIC 9(2)      VALUE   06.              00052200
052300      03  FILLER           PIC X(1)      VALUE  'P'.              00052300
052400      03  FILLER           PIC X(20)     VALUE                    00052400
052500          'DELI-ITALIAN KITCHEN'.                                 00052500
052600      03  FILLER           PIC 9(2)      VALUE   05.              00052600
052700      03  FILLER           PIC X(1)      VALUE  'A'.              00052700
052800      03  FILLER           PIC X(20)     VALUE                    00052800
052900          'PHARMACY            '.                                 00052900
053000      03  FILLER           PIC 9(2)      VALUE   08.              00053000
053100      03  FILLER           PIC X(1)      VALUE  ' '.              00053100
053200      03  FILLER           PIC X(20)     VALUE                    00053200
053300          'SERVICE             '.                                 00053300
053400      03  FILLER           PIC 9(2)      VALUE   01.              00053400
053500      03  FILLER           PIC X(1)      VALUE  'B'.              00053500
053600      03  FILLER           PIC X(20)     VALUE                    00053600
053700          'GASOLINE            '.                                 00053700
053800      03  FILLER           PIC 9(2)      VALUE   14.              00053800
053900      03  FILLER           PIC X(1)      VALUE  'A'.              00053900
054000      03  FILLER           PIC X(20)     VALUE                    00054000
054100          'GREAT"S!            '.                                 00054100
054200                                                                  00054200
054300 01  DEPARTMENT-SEQUENCE-TABLE1         REDEFINES                 00054300
054400     DEPARTMENT-SEQUENCE-VALUES1.                                 00054400
054500     03  DEPARTMENT-ENTRY1              OCCURS 26.                00054500
054600         05  DST-DEPARTMENT-NUMBER1     PIC 9(2).                 00054600
054700         05  DST-SUB-DEPARTMENT-NUMBER1 PIC X(1).                 00054700
054800         05  DST-DEPARTMENT-NAME1       PIC X(20).                00054800
054810                                                                  00054810
