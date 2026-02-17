# Calculating crosstabs

## Introduction

In this vignette we’ll show some examples how crosstabs can be
generated. First, a labelled data set is modified with
[datadaptor](https://codeberg.org/urswilke/datadaptor) to prepare the
data for the crosstabs. The latter are then calculated with the
crosstabser functionalities.
[Here](https://urswilke.github.io/datadaptor-crosstabser-table_charter-demo/)
you can find an interactive version of this vignette that will also
automatically generate an interactive
[table_charter](https://gitlab.com/urswilke/table_charter) app of the
crosstabs in the browser.

## Getting started

First, we’ll load the library.

``` r

library(crosstabser)
```

In order to use crosstabser, we need a dataset and a mapping file. The
latter defines the commands to modify the data (optional; using
datadaptor functionalities), and those to calculate crosstabs.

We’ll use the
[`datadaptor::fruit_survey`](https://urswilke.gitlab.io/datadaptor/reference/fruit_survey.html)
toy data set

``` r

df <- datadaptor::fruit_survey
```

together with the [example mapping
file](https://codeberg.org/urswilke/crosstabser/src/branch/main/inst/extdata/tabulate-fruits.xlsx)
for this data set in the package. If you install crosstabser on your
machine, this file is also on your computer:

``` r

mapping_file <- system.file("extdata", "tabulate-fruits.xlsx", package = "crosstabser")
```

We can apply the data modifications and calculate the crosstabs in one
command by defining an R6 object of class “Tabula”:

``` r

m <- crosstabser::Tabula$new(
  dat = df, 
  mapping_file = mapping_file
)
```

and then print the crosstabs defined in the mapping:

(*by passing a high value of the* `width` *parameter, we can make sure
that all headers are printed*)

``` r

m |> print(width = 1e4)
#> $`3`
#> $`3`[[1]]
#> # Q1. Do you like fruits?
#>                            TOTAL Likes ----- ----- Favo… ----- ----- ----- Frui… ----- -----
#>                                  Appl… Bana… Oran… Apple Bana… Oran… Othe… 3 or… 4 or… 6 an…
#> TOTAL                abs     100    57    54    54    27    12    19     9    18    22    27
#> Yes                  abs      75    57    54    54    27    12    19     9    18    22    27
#>                      in %     75   100   100   100   100   100   100   100   100   100   100
#> No                   abs      19     0     0     0     0     0     0     0     0     0     0
#>                      in %     19     0     0     0     0     0     0     0     0     0     0
#> No answer            abs       6     0     0     0     0     0     0     0     0     0     0
#>                      in %      6     0     0     0     0     0     0     0     0     0     0
#> VALID CASES          abs     100    57    54    54    27    12    19     9    18    22    27
#>                      in %    100   100   100   100   100   100   100   100   100   100   100
#> 
#> 
#> $`4`
#> $`4`[[1]]
#> # Q2. Which fruits do you like?
#>                            TOTAL Likes ----- ----- Favo… ----- ----- ----- Frui… ----- -----
#>                                  Appl… Bana… Oran… Apple Bana… Oran… Othe… 3 or… 4 or… 6 an…
#> TOTAL                abs   100    57    54    54    27    12    19     9    18    22    27  
#> Sum of valid answe…  abs   222   182   180   178    81    40    59    21    56    66    79  
#> Apple                abs    57    57    42    40    27     8    13     4    14    18    20  
#>                      in %   77.0 100    77.8  74.1 100    66.7  68.4  44.4  77.8  81.8  74.1
#> Banana               abs    54    42    54    42    21    12    13     3    16    16    17  
#>                      in %   73.0  73.7 100    77.8  77.8 100    68.4  33.3  88.9  72.7  63.0
#> Orange               abs    54    40    42    54    15     9    19     5    13    15    20  
#>                      in %   73.0  70.2  77.8 100    55.6  75   100    55.6  72.2  68.2  74.1
#> Others               abs    57    43    42    42    18    11    14     9    13    17    22  
#>                      in %   77.0  75.4  77.8  77.8  66.7  91.7  73.7 100    72.2  77.3  81.5
#> VALID CASES          abs    74    57    54    54    27    12    19     9    18    22    27  
#>                      in %   74   100   100   100   100   100   100   100   100   100   100  
#> No answer            abs     1     0     0     0     0     0     0     0     0     0     0  
#>                      in %    1     0     0     0     0     0     0     0     0     0     0  
#> FILTER               abs    25     0     0     0     0     0     0     0     0     0     0  
#>                      in %   25     0     0     0     0     0     0     0     0     0     0  
#> 
#> 
#> $`5`
#> $`5`[[1]]
#> # Q3. Please rate on a scale from 1 to 5 how much you like this fruit TOP2 overview
#>                            TOTAL Likes ----- ----- Favo… ----- ----- ----- Frui… ----- -----
#>                                  Appl… Bana… Oran… Apple Bana… Oran… Othe… 3 or… 4 or… 6 an…
#> VALID CASES (at le…  abs    74    57    54    54    27    12   19      9    18    22    27  
#> Apple                TOP2   40.4  40.4  47.6  35    81.5   0    7.69   0    50    50    35  
#>                      VALI…  57    57    42    40    27     8   13      4    14    18    20  
#> Banana               TOP2   31.5  35.7  31.5  28.6  33.3  58.3 15.4    0    43.8  37.5  17.6
#>                      VALI…  54    42    54    42    21    12   13      3    16    16    17  
#> Orange               TOP2   50    50    57.1  50    40    22.2 78.9    0    61.5  60    30  
#>                      VALI…  54    40    42    54    15     9   19      5    13    15    20  
#> Others               TOP2   31.6  32.6  28.6  31.0  22.2  18.2 28.6   66.7  38.5  47.1  13.6
#>                      VALI…  57    43    42    42    18    11   14      9    13    17    22  
#> 
#> $`5`[[2]]
#> # Q3. Please rate on a scale from 1 to 5 how much you like this fruit Summary of means
#>                            TOTAL Likes ----- ----- Favo… ----- ----- ----- Frui… ----- -----
#>                                  Appl… Bana… Oran… Apple Bana… Oran… Othe… 3 or… 4 or… 6 an…
#> VALID CASES (at le…  abs   74    57    54    54    27    12    19     9    18    22    27   
#> Apple                Mean   2.95  2.95  3.26  2.78  4.04  2     1.77  2.25  3.29  3     2.85
#>                      VALI… 57    57    42    40    27     8    13     4    14    18    20   
#> Banana               Mean   2.80  2.86  2.80  2.74  2.86  3.83  2.15  1.67  3.06  2.88  2.59
#>                      VALI… 54    42    54    42    21    12    13     3    16    16    17   
#> Orange               Mean   3.43  3.42  3.64  3.43  3.13  2.67  4.32  1.6   3.69  3.6   2.95
#>                      VALI… 54    40    42    54    15     9    19     5    13    15    20   
#> Others               Mean   2.79  2.74  2.76  2.69  2.56  2.91  2.29  3.89  3.15  3.29  2.18
#>                      VALI… 57    43    42    42    18    11    14     9    13    17    22   
#> 
#> $`5`[[3]]
#> # Q3. Please rate on a scale from 1 to 5 how much you like this fruit Apple
#>                                   TOTAL   Likes ------ ------ Favori… ------ ------ ------ Fruit… ------ ------
#>                                          Apples Banan… Orang…   Apple Banana Orange Others 3 or … 4 or 5 6 and…
#> TOTAL                     abs   100      57     54     54      27     12     19      9     18     22     27    
#> 5 = I love it             abs    11      11     10      7      11      0      0      0      4      5      2    
#>                           in %   19.3    19.3   23.8   17.5    40.7    0      0      0     28.6   27.8   10    
#> 4                         abs    12      12     10      7      11      0      1      0      3      4      5    
#>                           in %   21.1    21.1   23.8   17.5    40.7    0      7.69   0     21.4   22.2   25    
#> 3                         abs     9       9      8      6       2      2      2      2      2      1      5    
#>                           in %   15.8    15.8   19.0   15       7.41  25     15.4   50     14.3    5.56  25    
#> 2                         abs    13      13      9     10       1      4      3      1      3      2      4    
#>                           in %   22.8    22.8   21.4   25       3.70  50     23.1   25     21.4   11.1   20    
#> 1 = It's not bad          abs    12      12      5     10       2      2      7      1      2      6      4    
#>                           in %   21.1    21.1   11.9   25       7.41  25     53.8   25     14.3   33.3   20    
#> SUMMARY             TOP-2 abs    23      23     20     14      22      0      1      0      7      9      7    
#>                     TOP-2 in %   40.4    40.4   47.6   35      81.5    0      7.69   0     50     50     35    
#>                     MID-1 abs     9       9      8      6       2      2      2      2      2      1      5    
#>                     MID-1 in %   15.8    15.8   19.0   15       7.41  25     15.4   50     14.3    5.56  25    
#>                     LOW-2 abs    25      25     14     20       3      6     10      2      5      8      8    
#>                     LOW-2 in %   43.9    43.9   33.3   50      11.1   75     76.9   50     35.7   44.4   40    
#> Mean                              2.95    2.95   3.26   2.78    4.04   2      1.77   2.25   3.29   3      2.85 
#> Std.Err. Of Mean                  0.191   0.191  0.210  0.231   0.223  0.267  0.281  0.479  0.398  0.404  0.293
#> VALID CASES               abs    57      57     42     40      27      8     13      4     14     18     20    
#>                           in %   57     100     77.8   74.1   100     66.7   68.4   44.4   77.8   81.8   74.1  
#> FILTER                    abs    43       0     12     14       0      4      6      5      4      4      7    
#>                           in %   43       0     22.2   25.9     0     33.3   31.6   55.6   22.2   18.2   25.9  
#> 
#> $`5`[[4]]
#> # Q3. Please rate on a scale from 1 to 5 how much you like this fruit Banana
#>                                   TOTAL  Likes ------- ------ Favor… ------- ------ ------- Fruit… ------ ------
#>                                         Apples Bananas Orang…  Apple  Banana Orange  Others 3 or … 4 or 5 6 and…
#> TOTAL                     abs   100     57      54     54     27      12     19       9     18     22     27    
#> 5 = I love it             abs     7      6       7      5      3       4      0       0      4      3      0    
#>                           in %   13.0   14.3    13.0   11.9   14.3    33.3    0       0     25     18.8    0    
#> 4                         abs    10      9      10      7      4       3      2       0      3      3      3    
#>                           in %   18.5   21.4    18.5   16.7   19.0    25     15.4     0     18.8   18.8   17.6  
#> 3                         abs    10      7      10      9      4       4      1       0      2      1      6    
#>                           in %   18.5   16.7    18.5   21.4   19.0    33.3    7.69    0     12.5    6.25  35.3  
#> 2                         abs    19     13      19     14      7       1      7       2      4      7      6    
#>                           in %   35.2   31.0    35.2   33.3   33.3     8.33  53.8    66.7   25     43.8   35.3  
#> 1 = It's not bad          abs     8      7       8      7      3       0      3       1      3      2      2    
#>                           in %   14.8   16.7    14.8   16.7   14.3     0     23.1    33.3   18.8   12.5   11.8  
#> SUMMARY             TOP-2 abs    17     15      17     12      7       7      2       0      7      6      3    
#>                     TOP-2 in %   31.5   35.7    31.5   28.6   33.3    58.3   15.4     0     43.8   37.5   17.6  
#>                     MID-1 abs    10      7      10      9      4       4      1       0      2      1      6    
#>                     MID-1 in %   18.5   16.7    18.5   21.4   19.0    33.3    7.69    0     12.5    6.25  35.3  
#>                     LOW-2 abs    27     20      27     21     10       1     10       3      7      9      8    
#>                     LOW-2 in %   50     47.6    50     50     47.6     8.33  76.9   100     43.8   56.2   47.1  
#> Mean                              2.80   2.86    2.80   2.74   2.86    3.83   2.15    1.67   3.06   2.88   2.59 
#> Std.Err. Of Mean                  0.174  0.206   0.174  0.196  0.287   0.297  0.274   0.333  0.382  0.352  0.228
#> VALID CASES               abs    54     42      54     42     21      12     13       3     16     16     17    
#>                           in %   54     73.7   100     77.8   77.8   100     68.4    33.3   88.9   72.7   63.0  
#> FILTER                    abs    46     15       0     12      6       0      6       6      2      6     10    
#>                           in %   46     26.3     0     22.2   22.2     0     31.6    66.7   11.1   27.3   37.0  
#> 
#> $`5`[[5]]
#> # Q3. Please rate on a scale from 1 to 5 how much you like this fruit Orange
#>                                   TOTAL  Likes ------ ------- Favor… ------ ------- ----- Fruit… ----- ------
#>                                         Apples Banan… Oranges  Apple Banana  Orange Othe… 3 or … 4 or… 6 and…
#> TOTAL                     abs   100     57     54      54     27     12      19       9   18     22    27    
#> 5 = I love it             abs    18     13     17      18      4      0      11       0    4      7     4    
#>                           in %   33.3   32.5   40.5    33.3   26.7    0      57.9     0   30.8   46.7  20    
#> 4                         abs     9      7      7       9      2      2       4       0    4      2     2    
#>                           in %   16.7   17.5   16.7    16.7   13.3   22.2    21.1     0   30.8   13.3  10    
#> 3                         abs    12      9      8      12      4      3       3       1    3      0     8    
#>                           in %   22.2   22.5   19.0    22.2   26.7   33.3    15.8    20   23.1    0    40    
#> 2                         abs     8      6      6       8      2      3       1       1    1      5     1    
#>                           in %   14.8   15     14.3    14.8   13.3   33.3     5.26   20    7.69  33.3   5    
#> 1 = It's not bad          abs     7      5      4       7      3      1       0       3    1      1     5    
#>                           in %   13.0   12.5    9.52   13.0   20     11.1     0      60    7.69   6.67 25    
#> SUMMARY             TOP-2 abs    27     20     24      27      6      2      15       0    8      9     6    
#>                     TOP-2 in %   50     50     57.1    50     40     22.2    78.9     0   61.5   60    30    
#>                     MID-1 abs    12      9      8      12      4      3       3       1    3      0     8    
#>                     MID-1 in %   22.2   22.5   19.0    22.2   26.7   33.3    15.8    20   23.1    0    40    
#>                     LOW-2 abs    15     11     10      15      5      4       1       4    2      6     6    
#>                     LOW-2 in %   27.8   27.5   23.8    27.8   33.3   44.4     5.26   80   15.4   40    30    
#> Mean                              3.43   3.42   3.64    3.43   3.13   2.67    4.32    1.6  3.69   3.6   2.95 
#> Std.Err. Of Mean                  0.194  0.223  0.215   0.194  0.389  0.333   0.217   0.4  0.347  0.4   0.320
#> VALID CASES               abs    54     40     42      54     15      9      19       5   13     15    20    
#>                           in %   54     70.2   77.8   100     55.6   75     100      55.6 72.2   68.2  74.1  
#> FILTER                    abs    46     17     12       0     12      3       0       4    5      7     7    
#>                           in %   46     29.8   22.2     0     44.4   25       0      44.4 27.8   31.8  25.9  
#> 
#> $`5`[[6]]
#> # Q3. Please rate on a scale from 1 to 5 how much you like this fruit Others
#>                                   TOTAL  Likes ------ ------ Favor… ------ ------ ------- Fruit… ------ ------
#>                                         Apples Banan… Orang…  Apple Banana Orange  Others 3 or … 4 or 5 6 and…
#> TOTAL                     abs   100     57     54     54     27     12     19       9     18     22     27    
#> 5 = I love it             abs     8      6      7      4      2      2      1       2      1      5      1    
#>                           in %   14.0   14.0   16.7    9.52  11.1   18.2    7.14   22.2    7.69  29.4    4.55 
#> 4                         abs    10      8      5      9      2      0      3       4      4      3      2    
#>                           in %   17.5   18.6   11.9   21.4   11.1    0     21.4    44.4   30.8   17.6    9.09 
#> 3                         abs    14     10     11     10      5      5      1       3      5      4      5    
#>                           in %   24.6   23.3   26.2   23.8   27.8   45.5    7.14   33.3   38.5   23.5   22.7  
#> 2                         abs    12      7      9      8      4      3      3       0      2      2      6    
#>                           in %   21.1   16.3   21.4   19.0   22.2   27.3   21.4     0     15.4   11.8   27.3  
#> 1 = It's not bad          abs    13     12     10     11      5      1      6       0      1      3      8    
#>                           in %   22.8   27.9   23.8   26.2   27.8    9.09  42.9     0      7.69  17.6   36.4  
#> SUMMARY             TOP-2 abs    18     14     12     13      4      2      4       6      5      8      3    
#>                     TOP-2 in %   31.6   32.6   28.6   31.0   22.2   18.2   28.6    66.7   38.5   47.1   13.6  
#>                     MID-1 abs    14     10     11     10      5      5      1       3      5      4      5    
#>                     MID-1 in %   24.6   23.3   26.2   23.8   27.8   45.5    7.14   33.3   38.5   23.5   22.7  
#>                     LOW-2 abs    25     19     19     19      9      4      9       0      3      5     14    
#>                     LOW-2 in %   43.9   44.2   45.2   45.2   50     36.4   64.3     0     23.1   29.4   63.6  
#> Mean                              2.79   2.74   2.76   2.69   2.56   2.91   2.29    3.89   3.15   3.29   2.18 
#> Std.Err. Of Mean                  0.180  0.216  0.215  0.206  0.315  0.368  0.384   0.261  0.296  0.361  0.252
#> VALID CASES               abs    57     43     42     42     18     11     14       9     13     17     22    
#>                           in %   57     75.4   77.8   77.8   66.7   91.7   73.7   100     72.2   77.3   81.5  
#> FILTER                    abs    43     14     12     12      9      1      5       0      5      5      5    
#>                           in %   43     24.6   22.2   22.2   33.3    8.33  26.3     0     27.8   22.7   18.5  
#> 
#> 
#> $`6`
#> $`6`[[1]]
#> # Q4. What's your favorite fruit?
#>                            TOTAL Likes ----- ----- Favo… ----- ----- ----- Frui… ----- -----
#>                                  Appl… Bana… Oran… Apple Bana… Oran… Othe… 3 or… 4 or… 6 an…
#> TOTAL                abs   100   57    54     54      27    12    19     9  18    22    27  
#> Apple                abs    27   27    21     15      27     0     0     0   8     8    11  
#>                      in %   40.3 51.9  42.9   31.2   100     0     0     0  44.4  36.4  40.7
#> Banana               abs    12    8    12      9       0    12     0     0   2     5     5  
#>                      in %   17.9 15.4  24.5   18.8     0   100     0     0  11.1  22.7  18.5
#> Orange               abs    19   13    13     19       0     0    19     0   5     6     8  
#>                      in %   28.4 25    26.5   39.6     0     0   100     0  27.8  27.3  29.6
#> Others               abs     9    4     3      5       0     0     0     9   3     3     3  
#>                      in %   13.4  7.69  6.12  10.4     0     0     0   100  16.7  13.6  11.1
#> VALID CASES          abs    67   52    49     48      27    12    19     9  18    22    27  
#>                      in %   67   91.2  90.7   88.9   100   100   100   100 100   100   100  
#> No answer            abs     7    5     5      6       0     0     0     0   0     0     0  
#>                      in %    7    8.77  9.26  11.1     0     0     0     0   0     0     0  
#> FILTER               abs    26    0     0      0       0     0     0     0   0     0     0  
#>                      in %   26    0     0      0       0     0     0     0   0     0     0  
#> 
#> 
#> $`7`
#> $`7`[[1]]
#> # Q5. How many of your favorite fruit do you like to eat?
#>                                   TOTAL  Likes ------ ------ Favori… ------- ------- ------- Fruit … ------- -------
#>                                         Apples Banan… Orang…   Apple  Banana  Orange  Others 3 or l…  4 or 5 6 and …
#> TOTAL                     abs   100     57     54     54      27      12      19       9      18      22      27    
#> 1                         abs     1      0      0      1       0       0       0       1       1       0       0    
#>                           in %    1.49   0      0      2.08    0       0       0      11.1     5.56    0       0    
#> 2                         abs    12      9     11      7       6       1       3       2      12       0       0    
#>                           in %   17.9   17.3   22.4   14.6    22.2     8.33   15.8    22.2    66.7     0       0    
#> 3                         abs     5      5      5      5       2       1       2       0       5       0       0    
#>                           in %    7.46   9.62  10.2   10.4     7.41    8.33   10.5     0      27.8     0       0    
#> 4                         abs    15     12     11     10       7       3       2       3       0      15       0    
#>                           in %   22.4   23.1   22.4   20.8    25.9    25      10.5    33.3     0      68.2     0    
#> 5                         abs     7      6      5      5       1       2       4       0       0       7       0    
#>                           in %   10.4   11.5   10.2   10.4     3.70   16.7    21.1     0       0      31.8     0    
#> 6                         abs    13     11      8      8       6       3       1       3       0       0      13    
#>                           in %   19.4   21.2   16.3   16.7    22.2    25       5.26   33.3     0       0      48.1  
#> 7                         abs     5      3      1      5       1       0       4       0       0       0       5    
#>                           in %    7.46   5.77   2.04  10.4     3.70    0      21.1     0       0       0      18.5  
#> 8                         abs     6      3      5      5       2       2       2       0       0       0       6    
#>                           in %    8.96   5.77  10.2   10.4     7.41   16.7    10.5     0       0       0      22.2  
#> 9                         abs     1      1      1      1       1       0       0       0       0       0       1    
#>                           in %    1.49   1.92   2.04   2.08    3.70    0       0       0       0       0       3.70 
#> 10                        abs     2      2      2      1       1       0       1       0       0       0       2    
#>                           in %    2.99   3.85   4.08   2.08    3.70    0       5.26    0       0       0       7.41 
#> SUMMARY             Up t… abs    18     14     16     13       8       2       5       3      18       0       0    
#>                     Up t… in %   26.9   26.9   32.7   27.1    29.6    16.7    26.3    33.3   100       0       0    
#>                     4 or… abs    22     18     16     15       8       5       6       3       0      22       0    
#>                     4 or… in %   32.8   34.6   32.7   31.2    29.6    41.7    31.6    33.3     0     100       0    
#>                     More… abs    27     20     17     20      11       5       8       3       0       0      27    
#>                     More… in %   40.3   38.5   34.7   41.7    40.7    41.7    42.1    33.3     0       0     100    
#> Mean                              4.85   4.83   4.69   4.94    4.78    5.08    5.26    3.89    2.22    4.32    7.04 
#> Median                            5      4.5    4      5       4       5       5       4       2       4       7    
#> Std.Err. Of Mean                  0.265  0.293  0.325  0.314   0.441   0.529   0.529   0.633   0.129   0.102   0.242
#> VALID CASES               abs    67     52     49     48      27      12      19       9      18      22      27    
#>                           in %   67     91.2   90.7   88.9   100     100     100     100     100     100     100    
#> FILTER                    abs    33      5      5      6       0       0       0       0       0       0       0    
#>                           in %   33      8.77   9.26  11.1     0       0       0       0       0       0       0
```

## Dive-in

In the following, we’ll have a closer look, how this works.

### Create a mapping file

crosstabser’s example file `mapping_file` to generate crosstabs was
built by editing the empty template of an Excel mapping file. For every
data set `df`, this is how you can generate the empty mapping file
(named `"mapping.xlsx"` here) from the template:

``` r

create_tabula(df, "mapping.xlsx")
```

### Mapping file content

You can also see the relevant parts in the sheets of `mapping_file`
here, in the following 5 data.frames:

- Variables
- Label
- Free1
- Macro
- Questions

``` r

Variables
#> # A tibble: 1 × 6
#>   var   type   varlab                      new_label      new_name op   
#>   <chr> <chr>  <chr>                       <chr>          <chr>    <chr>
#> 1 q4    double What's your favorite fruit? Favorite fruit NA       NA
```

The variable label of `q4` is changed to `"Favorite fruit"`.

``` r

Label
#> # A tibble: 3 × 7
#>   var      nv vallab   new_label sum_var_label sum_var_value sum_var_vallab
#>   <chr> <dbl> <chr>    <chr>     <chr>                 <dbl> <chr>         
#> 1 q2_1      1 Selected NA        Likes                     1 Apples        
#> 2 q2_2      1 Selected NA        Likes                     1 Bananas       
#> 3 q2_3      1 Selected NA        Likes                     1 Oranges
```

This creates the 3 variables `kq2_1`, `kq2_2` & `kq2_3` from `q2_1`,
`q2_2` & `q2_3` with the variable label “Likes” and the respective value
labels “Apples”, “Bananas” & “Oranges”. The created variables are `NA`
if the respective fruits weren’t selected, and serve as the basis to
calculate and label the corresponding headers in the crosstabs (see
Macro sheet).

``` r

Free1
#> # A tibble: 15 × 6
#>      row X1     X2                                 X3                                                         X4                    X5        
#>    <int> <chr>  <chr>                              <chr>                                                      <chr>                 <chr>     
#>  1     1 #COMP  n_fruits                           ((dat_mod |> dplyr::select(q2_1:q2_97)) == 1) |> rowSums() NA                    NA        
#>  2     2 #IF    n_fruits == 0 & q1 == 1            q2_99 = 1                                                  NA                    NA        
#>  3     3 #VARL  q2_99                              No answer                                                  NA                    NA        
#>  4     5 #VARL  q{2 3}_1                           Apple                                                      NA                    NA        
#>  5     6 #VARL  q{2 3}_2                           Banana                                                     NA                    NA        
#>  6     7 #VARL  q{2 3}_3                           Orange                                                     NA                    NA        
#>  7     8 #VARL  q{2 3}_97                          Others                                                     NA                    NA        
#>  8    10 #IF    n_fruits == 1 & q2_{1 2 3 97} == 1 q4 = {1 2 3 97}                                            NA                    NA        
#>  9    12 #RMVAL q4                                 kq4                                                        NA                    NA        
#> 10    13 NA     -2                                 NA                                                         NA                    NA        
#> 11    14 .      99                                 NA                                                         NA                    NA        
#> 12    16 #REC   q5                                 kq5                                                        Fruit number category NA        
#> 13    17 NA     1                                  3                                                          1                     3 or less 
#> 14    18 NA     4                                  5                                                          2                     4 or 5    
#> 15    19 .      6                                  Inf                                                        3                     6 and more
```

The following calculations are done:

- `n_fruits`: The number of fruits selected in `Q2`.
- If no fruit was selected in `Q2`, but it was asked according to `Q1`,
  `Q2` is coded as “No answer”.
- If only 1 fruit is selected in `Q2`, `Q4` wasn’t asked and we set `Q4`
  to this fruit.
- The variable labels in `Q2` & `Q3` (which provides the row labels in
  the crosstabs) are changed to “Apple”, “Banana” & “Orange” & “Other”,
  respectively.
- Further header variables `kq4` / `kq5` are generated (also for use in
  the Macro sheet):
  - `kq4`: with a subset of values (`NA` otherwise)
  - `kq5`: labelled values of summarized intervals

``` r

Macro
#> # A tibble: 4 × 2
#>   A                E                        
#>   <chr>            <chr>                    
#> 1 Scenario         1                        
#> 2 ColVar           kq2_1 kq2_2 kq2_3 kq4 kq5
#> 3 Invalid          -1,-3,-2                 
#> 4 Gesamtstichprobe 1=1
```

We only need the column where `Scenario` is equal to the value
`"V_Scenario"` specified in the mapping.

(This allows to store varying parameter sets for different scenarios.
And by using this named region in Excel formulas, you can also enter
scenario-specific commands in the Excel file.)

The only relevant argument in this case is the value in the `"ColVar"`
row. The variable names are entered in a space separated list,
corresponding to this character vector:

    ColVar <-  c("kq2_1", "kq2_2", "kq2_3", "kq4", "kq5")

``` r

Questions
#> # A tibble: 5 × 11
#>   Title                                                               RowVar               Type  Unguelt CatRec                                   CatLab                                 MetrMac Fussnote                       Sort    MdgMissLab RepOV                           
#>   <chr>                                                               <chr>                <chr> <chr>   <chr>                                    <chr>                                  <chr>   <chr>                          <chr>   <chr>      <chr>                           
#> 1 Q1. Do you like fruits?                                             q1                   cat   NA      NA                                       NA                                     NA      NA                             NA      NA         NA                              
#> 2 Q2. Which fruits do you like?                                       q2_1 q2_2 q2_3 q2_97 mdg   q2_99   NA                                       NA                                     NA      FILTER: Q1 ≠ 'Yes'.            NA      FILTER     NA                              
#> 3 Q3. Please rate on a scale from 1 to 5 how much you like this fruit q3_1 q3_2 q3_3 q3_97 mw    NA      (1 THRU 2=3) (3=2) (4 THRU 5=1)          1 'TOP-2' 2 'MID-1' 3 'LOW-2'          S1E2    FILTER: Fruit selected in Q2.  ORDER=D NA         TOP2:(4 THRU 5=100) (1 THRU 5=0)
#> 4 Q4. What's your favorite fruit?                                     q4                   cat   99, -2  NA                                       NA                                     NA      FILTER: Fruits selected in Q2. NA      NA         NA                              
#> 5 Q5. How many of your favorite fruit do you like to eat?             q5                   cat   NA      (1 THRU 3 = 1) (4,5 = 2) (6 THRU HI = 3) 1 'Up to 3' 2 '4 or 5' 3 'More than 5' S1M0E2  FILTER: Q1 ≠ 'Yes'.            NA      NA         NA
```

Only the columns `RowVar` & `Type` are mandatory for every table. The
other options allow for further customization of the tables. For the
various options how to modify the crosstabs, please have a look at
[`vignette("questions-parameters")`](https://urswilke.codeberg.page/crosstabser/articles/questions-parameters.md).

The following 5 are tabulated:

- `Q1`: Single answer question
- `Q2`: Multiple answer question, “No answer” is invalid, and we label
  as “FILTER” if no variable is selected (also invalid).
- `Q3`: Item battery:
  - First comes a table with the TOP2 overview,
  - then the default “Summary of means” overview,
  - followed by tables of the answers for single items, ordered from top
    to bottom, and with additional rows for TOP2, MID1 & LOW2, as well
    as the mean & its standard error.
- `Q4`: Single choice question, “No answer” is set to invalid.
- `Q5`: Like the single items in `Q3` this variable is tabulated with
  additional rows for summarized intervals, as well as the mean & its
  standard error.

### Use the mapping

Next, we’ll show how we can apply the data modifications and calculate
the crosstabs in the mapping.

#### Modify data

The data is modified by applying the commands in the following sheets
(using
[`datadaptor::Mapping$modify_data()`](https://urswilke.codeberg.page/datadaptor/reference/Mapping.html#method-Mapping-modify_data)
under the hood):

- `Variables`: This modifies the data set’s variables and their labels.
- `Label`: This modifies the data set’s variables’ value labels and
  allows to generate summary variables.
- `Free1`: This allows to define additional commands modifying the data.
  All the modifications of the `Variables` & `Label` sheets could also
  be done there.

See
[here](https://urswilke.codeberg.page/datadaptor/articles/command_blocks.html)
for the documentation of the commands used in these sheets.

#### Calculate crosstabs

The following sheets control, which and how crosstabs are calculated:

- `Macro`: This allows to define the additional header variables for the
  x-axis of the crosstabs.
- `Questions`: The rows in this table define the crosstabs that are
  calculated.

#### Execute

We can apply the data modifications and calculate the crosstabs in one
command by constructing an R6 object of class “Tabula”:

``` r

m <- crosstabser::Tabula$new(
  dat = df, 
  mapping_file = mapping_file
)
```

### Alternative use without Excel

Instead of using an Excel mapping file, we can also define the mapping
entirely in R without the need of an Excel file by passing the relevant
data as a list:

``` r

mapping_file <- list(
  Variables = Variables,
  Label = Label,
  Free1 = Free1,
  Questions = Questions, 
  # You can pass the parameters of the data.frame 
  # in the `Macro` sheet as a named list:
  Macro = list(ColVar = ColVar)
)
m <- crosstabser::Tabula$new(
  dat = df, 
  mapping_file = mapping_file
)
```
