# 5 tables' prints are reproduced

    Code
      withr::with_options(list(pillar.print_max = Inf, width = 1000), print(tabsi$
        crosstabs$data))
    Output
      $tab_table
      # A tibble: 1 x 9
        TabNo TabName  TabType QuestNo TabTitle                                              TabCaption SelVal repov_name    BookNo
        <int> <chr>    <chr>   <chr>   <chr>                                                 <chr>      <chr>  <chr>          <dbl>
      1     1 CAT#Q1@1 CAT     Q1      1. Wie viele Mitarbeiter beschäftigt Ihr Unternehmen? <NA>       <NA>   <NA>       999999999
      
      $val_table
      # A tibble: 130 x 5
          QuestNo RowNo ColNo  value    BookNo
          <chr>   <int> <int>  <dbl>     <dbl>
        1 Q1          4     4 151    999999999
        2 Q1          4     5  19    999999999
        3 Q1          4     6  75    999999999
        4 Q1          4     7  43    999999999
        5 Q1          4     8  14    999999999
        6 Q1          5     4 151    999999999
        7 Q1          5     5  15    999999999
        8 Q1          5     6  71.5  999999999
        9 Q1          5     7  47.5  999999999
       10 Q1          5     8  17    999999999
       11 Q1          6     4   0    999999999
       12 Q1          6     5   0    999999999
       13 Q1          6     6   0    999999999
       14 Q1          6     7   0    999999999
       15 Q1          6     8   0    999999999
       16 Q1          7     4   0    999999999
       17 Q1          7     5   0    999999999
       18 Q1          7     6   0    999999999
       19 Q1          7     7   0    999999999
       20 Q1          7     8   0    999999999
       21 Q1          8     4   0    999999999
       22 Q1          8     5   0    999999999
       23 Q1          8     6   0    999999999
       24 Q1          8     7   0    999999999
       25 Q1          8     8   0    999999999
       26 Q1          9     4   0    999999999
       27 Q1          9     5   0    999999999
       28 Q1          9     6   0    999999999
       29 Q1          9     7   0    999999999
       30 Q1          9     8   0    999999999
       31 Q1         10     4  49    999999999
       32 Q1         10     5   5    999999999
       33 Q1         10     6  20    999999999
       34 Q1         10     7  19    999999999
       35 Q1         10     8   5    999999999
       36 Q1         11     4  54    999999999
       37 Q1         11     5   4.5  999999999
       38 Q1         11     6  18.5  999999999
       39 Q1         11     7  24    999999999
       40 Q1         11     8   7    999999999
       41 Q1         12     4  32.5  999999999
       42 Q1         12     5  26.3  999999999
       43 Q1         12     6  26.7  999999999
       44 Q1         12     7  44.2  999999999
       45 Q1         12     8  35.7  999999999
       46 Q1         13     4  35.8  999999999
       47 Q1         13     5  30    999999999
       48 Q1         13     6  25.9  999999999
       49 Q1         13     7  50.5  999999999
       50 Q1         13     8  41.2  999999999
       51 Q1         14     4  63    999999999
       52 Q1         14     5   6    999999999
       53 Q1         14     6  30    999999999
       54 Q1         14     7  21    999999999
       55 Q1         14     8   6    999999999
       56 Q1         15     4  61    999999999
       57 Q1         15     5   3.5  999999999
       58 Q1         15     6  28    999999999
       59 Q1         15     7  21.5  999999999
       60 Q1         15     8   8    999999999
       61 Q1         16     4  41.7  999999999
       62 Q1         16     5  31.6  999999999
       63 Q1         16     6  40    999999999
       64 Q1         16     7  48.8  999999999
       65 Q1         16     8  42.9  999999999
       66 Q1         17     4  40.4  999999999
       67 Q1         17     5  23.3  999999999
       68 Q1         17     6  39.2  999999999
       69 Q1         17     7  45.3  999999999
       70 Q1         17     8  47.1  999999999
       71 Q1         18     4  39    999999999
       72 Q1         18     5   8    999999999
       73 Q1         18     6  25    999999999
       74 Q1         18     7   3    999999999
       75 Q1         18     8   3    999999999
       76 Q1         19     4  36    999999999
       77 Q1         19     5   7    999999999
       78 Q1         19     6  25    999999999
       79 Q1         19     7   2    999999999
       80 Q1         19     8   2    999999999
       81 Q1         20     4  25.8  999999999
       82 Q1         20     5  42.1  999999999
       83 Q1         20     6  33.3  999999999
       84 Q1         20     7   6.98 999999999
       85 Q1         20     8  21.4  999999999
       86 Q1         21     4  23.8  999999999
       87 Q1         21     5  46.7  999999999
       88 Q1         21     6  35.0  999999999
       89 Q1         21     7   4.21 999999999
       90 Q1         21     8  11.8  999999999
       91 Q1         22     4   0    999999999
       92 Q1         22     5   0    999999999
       93 Q1         22     6   0    999999999
       94 Q1         22     7   0    999999999
       95 Q1         22     8   0    999999999
       96 Q1         23     4   0    999999999
       97 Q1         23     5   0    999999999
       98 Q1         23     6   0    999999999
       99 Q1         23     7   0    999999999
      100 Q1         23     8   0    999999999
      101 Q1         24     4   0    999999999
      102 Q1         24     5   0    999999999
      103 Q1         24     6   0    999999999
      104 Q1         24     7   0    999999999
      105 Q1         24     8   0    999999999
      106 Q1         25     4   0    999999999
      107 Q1         25     5   0    999999999
      108 Q1         25     6   0    999999999
      109 Q1         25     7   0    999999999
      110 Q1         25     8   0    999999999
      111 Q1         26     4 151    999999999
      112 Q1         26     5  19    999999999
      113 Q1         26     6  75    999999999
      114 Q1         26     7  43    999999999
      115 Q1         26     8  14    999999999
      116 Q1         27     4 151    999999999
      117 Q1         27     5  15    999999999
      118 Q1         27     6  71.5  999999999
      119 Q1         27     7  47.5  999999999
      120 Q1         27     8  17    999999999
      121 Q1         28     4 100    999999999
      122 Q1         28     5 100    999999999
      123 Q1         28     6 100    999999999
      124 Q1         28     7 100    999999999
      125 Q1         28     8 100    999999999
      126 Q1         29     4 100    999999999
      127 Q1         29     5 100    999999999
      128 Q1         29     6 100    999999999
      129 Q1         29     7 100    999999999
      130 Q1         29     8 100    999999999
      
      $row_table
      # A tibble: 30 x 14
         RowNo RowContent RowAbsPercent RowWeighted  TabNo RowTitle1                                               RowTitle2                  RowTitle3 RowFormat RowDecimals RowVariable RowValue QuestNo    BookNo
         <int> <chr>      <chr>         <chr>        <int> <chr>                                                   <chr>                      <chr>     <chr>           <int> <chr>          <dbl> <chr>       <dbl>
       1     1 Title      ""            ""               1 "1. Wie viele Mitarbeiter beschäftigt Ihr Unternehmen?" ""                         ""        <NA>               NA <NA>              NA Q1      999999999
       2     2 Header     ""            ""               1  <NA>                                                   ""                         ""        <NA>               NA <NA>              NA Q1      999999999
       3     3 Header     ""            ""               1  <NA>                                                   ""                         ""        <NA>               NA <NA>              NA Q1      999999999
       4     4 Total      "Abs"         "Unweighted"     1 "GESAMT"                                                "GESAMT"                   "abs"     <NA>                0 q1                 1 Q1      999999999
       5     5 Total      "Abs"         "Weighted"       1 "GESAMT"                                                "GESAMT"                   "abs ⚖"   <NA>                0 q1                 1 Q1      999999999
       6     6 Detail     "Abs"         "Unweighted"     1 "1 Beschäftigter"                                       "1 Beschäftigter"          "abs"     <NA>                0 q1                 1 Q1      999999999
       7     7 Detail     "Abs"         "Weighted"       1 "1 Beschäftigter"                                       "1 Beschäftigter"          "abs ⚖"   <NA>                0 q1                 1 Q1      999999999
       8     8 Detail     "Percent"     "Unweighted"     1 "1 Beschäftigter"                                       "1 Beschäftigter"          "in %"    <NA>                1 q1                 1 Q1      999999999
       9     9 Detail     "Percent"     "Weighted"       1 "1 Beschäftigter"                                       "1 Beschäftigter"          "in % ⚖"  <NA>                1 q1                 1 Q1      999999999
      10    10 Detail     "Abs"         "Unweighted"     1 "2 - 4 Beschäftigte"                                    "2 - 4 Beschäftigte"       "abs"     <NA>                0 q1                 2 Q1      999999999
      11    11 Detail     "Abs"         "Weighted"       1 "2 - 4 Beschäftigte"                                    "2 - 4 Beschäftigte"       "abs ⚖"   <NA>                0 q1                 2 Q1      999999999
      12    12 Detail     "Percent"     "Unweighted"     1 "2 - 4 Beschäftigte"                                    "2 - 4 Beschäftigte"       "in %"    <NA>                1 q1                 2 Q1      999999999
      13    13 Detail     "Percent"     "Weighted"       1 "2 - 4 Beschäftigte"                                    "2 - 4 Beschäftigte"       "in % ⚖"  <NA>                1 q1                 2 Q1      999999999
      14    14 Detail     "Abs"         "Unweighted"     1 "5 - 9 Beschäftigte"                                    "5 - 9 Beschäftigte"       "abs"     <NA>                0 q1                 3 Q1      999999999
      15    15 Detail     "Abs"         "Weighted"       1 "5 - 9 Beschäftigte"                                    "5 - 9 Beschäftigte"       "abs ⚖"   <NA>                0 q1                 3 Q1      999999999
      16    16 Detail     "Percent"     "Unweighted"     1 "5 - 9 Beschäftigte"                                    "5 - 9 Beschäftigte"       "in %"    <NA>                1 q1                 3 Q1      999999999
      17    17 Detail     "Percent"     "Weighted"       1 "5 - 9 Beschäftigte"                                    "5 - 9 Beschäftigte"       "in % ⚖"  <NA>                1 q1                 3 Q1      999999999
      18    18 Detail     "Abs"         "Unweighted"     1 "10 - 19 Beschäftigte"                                  "10 - 19 Beschäftigte"     "abs"     <NA>                0 q1                 4 Q1      999999999
      19    19 Detail     "Abs"         "Weighted"       1 "10 - 19 Beschäftigte"                                  "10 - 19 Beschäftigte"     "abs ⚖"   <NA>                0 q1                 4 Q1      999999999
      20    20 Detail     "Percent"     "Unweighted"     1 "10 - 19 Beschäftigte"                                  "10 - 19 Beschäftigte"     "in %"    <NA>                1 q1                 4 Q1      999999999
      21    21 Detail     "Percent"     "Weighted"       1 "10 - 19 Beschäftigte"                                  "10 - 19 Beschäftigte"     "in % ⚖"  <NA>                1 q1                 4 Q1      999999999
      22    22 Detail     "Abs"         "Unweighted"     1 "20 Beschäftigte und mehr"                              "20 Beschäftigte und mehr" "abs"     <NA>                0 q1                 5 Q1      999999999
      23    23 Detail     "Abs"         "Weighted"       1 "20 Beschäftigte und mehr"                              "20 Beschäftigte und mehr" "abs ⚖"   <NA>                0 q1                 5 Q1      999999999
      24    24 Detail     "Percent"     "Unweighted"     1 "20 Beschäftigte und mehr"                              "20 Beschäftigte und mehr" "in %"    <NA>                1 q1                 5 Q1      999999999
      25    25 Detail     "Percent"     "Weighted"       1 "20 Beschäftigte und mehr"                              "20 Beschäftigte und mehr" "in % ⚖"  <NA>                1 q1                 5 Q1      999999999
      26    26 Valid      "Abs"         "Unweighted"     1 "GÜLTIGE FÄLLE"                                         "GÜLTIGE FÄLLE"            "abs"     <NA>                0 q1                 1 Q1      999999999
      27    27 Valid      "Abs"         "Weighted"       1 "GÜLTIGE FÄLLE"                                         "GÜLTIGE FÄLLE"            "abs ⚖"   <NA>                0 q1                 1 Q1      999999999
      28    28 Valid      "Percent"     "Unweighted"     1 "GÜLTIGE FÄLLE"                                         "GÜLTIGE FÄLLE"            "in %"    <NA>                1 q1                 1 Q1      999999999
      29    29 Valid      "Percent"     "Weighted"       1 "GÜLTIGE FÄLLE"                                         "GÜLTIGE FÄLLE"            "in % ⚖"  <NA>                1 q1                 1 Q1      999999999
      30    30 Empty      ""            ""               1 ""                                                      ""                         ""        <NA>               NA <NA>              NA Q1      999999999
      
      $head_table
      # A tibble: 2 x 4
        HeadNo HeadName      HeadTitle    BookNo
         <dbl> <chr>         <chr>         <dbl>
      1      2 DC#STICHPROBE GESAMT    999999999
      2      3 kregio        Region    999999999
      
      $col_table_all
         HeadNo ColTitle1   ColTitle2 ColNo   ColVariable ColValue BookNo
      1       1                           1          <NA>       NA  1e+09
      2       1                           2          <NA>       NA  1e+09
      3       1                           3          <NA>       NA  1e+09
      4       2    GESAMT                 4 DC#STICHPROBE        1  1e+09
      5       3    Region       Asien     5        kregio        1  1e+09
      6       3    Region      Europa     6        kregio        2  1e+09
      7       3    Region Nordamerika     7        kregio        3  1e+09
      8       3    Region     Pazifik     8        kregio        4  1e+09
      9       4                           9          <NA>       NA  1e+09
      10      5                          10          <NA>       NA  1e+09
      

