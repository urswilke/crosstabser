# 5 tables' prints are reproduced

    Code
      withr::with_options(list(pillar.print_max = Inf, width = 1000), print(tabsi$
        crosstabs$data))
    Output
      $tab_table
      # A tibble: 1 x 15
           BookNo QuestNo TabName  QuestLine TabNo TabType TabTitle                                              TabTitle1                                             TabTitle2                                             TabTitle3                                             TabCaption SelVal repov_name TabCount TabRowTypes
            <dbl> <chr>   <chr>        <dbl> <int> <chr>   <chr>                                                 <chr>                                                 <chr>                                                 <chr>                                                 <chr>      <chr>  <chr>         <int>       <int>
      1 999999999 Q1      CAT#Q1@1         5     1 CAT     1. Wie viele Mitarbeiter beschäftigt Ihr Unternehmen? 1. Wie viele Mitarbeiter beschäftigt Ihr Unternehmen? 1. Wie viele Mitarbeiter beschäftigt Ihr Unternehmen? 1. Wie viele Mitarbeiter beschäftigt Ihr Unternehmen? <NA>       <NA>   <NA>             30          NA
      
      $val_table
      # A tibble: 130 x 6
             BookNo QuestNo TabNo RowNo ColNo  Value
              <dbl> <chr>   <int> <int> <int>  <dbl>
        1 999999999 Q1          1     4     4 151   
        2 999999999 Q1          1     4     5  19   
        3 999999999 Q1          1     4     6  75   
        4 999999999 Q1          1     4     7  43   
        5 999999999 Q1          1     4     8  14   
        6 999999999 Q1          1     5     4 151   
        7 999999999 Q1          1     5     5  15   
        8 999999999 Q1          1     5     6  71.5 
        9 999999999 Q1          1     5     7  47.5 
       10 999999999 Q1          1     5     8  17   
       11 999999999 Q1          1     6     4   0   
       12 999999999 Q1          1     6     5   0   
       13 999999999 Q1          1     6     6   0   
       14 999999999 Q1          1     6     7   0   
       15 999999999 Q1          1     6     8   0   
       16 999999999 Q1          1     7     4   0   
       17 999999999 Q1          1     7     5   0   
       18 999999999 Q1          1     7     6   0   
       19 999999999 Q1          1     7     7   0   
       20 999999999 Q1          1     7     8   0   
       21 999999999 Q1          1     8     4   0   
       22 999999999 Q1          1     8     5   0   
       23 999999999 Q1          1     8     6   0   
       24 999999999 Q1          1     8     7   0   
       25 999999999 Q1          1     8     8   0   
       26 999999999 Q1          1     9     4   0   
       27 999999999 Q1          1     9     5   0   
       28 999999999 Q1          1     9     6   0   
       29 999999999 Q1          1     9     7   0   
       30 999999999 Q1          1     9     8   0   
       31 999999999 Q1          1    10     4  49   
       32 999999999 Q1          1    10     5   5   
       33 999999999 Q1          1    10     6  20   
       34 999999999 Q1          1    10     7  19   
       35 999999999 Q1          1    10     8   5   
       36 999999999 Q1          1    11     4  54   
       37 999999999 Q1          1    11     5   4.5 
       38 999999999 Q1          1    11     6  18.5 
       39 999999999 Q1          1    11     7  24   
       40 999999999 Q1          1    11     8   7   
       41 999999999 Q1          1    12     4  32.5 
       42 999999999 Q1          1    12     5  26.3 
       43 999999999 Q1          1    12     6  26.7 
       44 999999999 Q1          1    12     7  44.2 
       45 999999999 Q1          1    12     8  35.7 
       46 999999999 Q1          1    13     4  35.8 
       47 999999999 Q1          1    13     5  30   
       48 999999999 Q1          1    13     6  25.9 
       49 999999999 Q1          1    13     7  50.5 
       50 999999999 Q1          1    13     8  41.2 
       51 999999999 Q1          1    14     4  63   
       52 999999999 Q1          1    14     5   6   
       53 999999999 Q1          1    14     6  30   
       54 999999999 Q1          1    14     7  21   
       55 999999999 Q1          1    14     8   6   
       56 999999999 Q1          1    15     4  61   
       57 999999999 Q1          1    15     5   3.5 
       58 999999999 Q1          1    15     6  28   
       59 999999999 Q1          1    15     7  21.5 
       60 999999999 Q1          1    15     8   8   
       61 999999999 Q1          1    16     4  41.7 
       62 999999999 Q1          1    16     5  31.6 
       63 999999999 Q1          1    16     6  40   
       64 999999999 Q1          1    16     7  48.8 
       65 999999999 Q1          1    16     8  42.9 
       66 999999999 Q1          1    17     4  40.4 
       67 999999999 Q1          1    17     5  23.3 
       68 999999999 Q1          1    17     6  39.2 
       69 999999999 Q1          1    17     7  45.3 
       70 999999999 Q1          1    17     8  47.1 
       71 999999999 Q1          1    18     4  39   
       72 999999999 Q1          1    18     5   8   
       73 999999999 Q1          1    18     6  25   
       74 999999999 Q1          1    18     7   3   
       75 999999999 Q1          1    18     8   3   
       76 999999999 Q1          1    19     4  36   
       77 999999999 Q1          1    19     5   7   
       78 999999999 Q1          1    19     6  25   
       79 999999999 Q1          1    19     7   2   
       80 999999999 Q1          1    19     8   2   
       81 999999999 Q1          1    20     4  25.8 
       82 999999999 Q1          1    20     5  42.1 
       83 999999999 Q1          1    20     6  33.3 
       84 999999999 Q1          1    20     7   6.98
       85 999999999 Q1          1    20     8  21.4 
       86 999999999 Q1          1    21     4  23.8 
       87 999999999 Q1          1    21     5  46.7 
       88 999999999 Q1          1    21     6  35.0 
       89 999999999 Q1          1    21     7   4.21
       90 999999999 Q1          1    21     8  11.8 
       91 999999999 Q1          1    22     4   0   
       92 999999999 Q1          1    22     5   0   
       93 999999999 Q1          1    22     6   0   
       94 999999999 Q1          1    22     7   0   
       95 999999999 Q1          1    22     8   0   
       96 999999999 Q1          1    23     4   0   
       97 999999999 Q1          1    23     5   0   
       98 999999999 Q1          1    23     6   0   
       99 999999999 Q1          1    23     7   0   
      100 999999999 Q1          1    23     8   0   
      101 999999999 Q1          1    24     4   0   
      102 999999999 Q1          1    24     5   0   
      103 999999999 Q1          1    24     6   0   
      104 999999999 Q1          1    24     7   0   
      105 999999999 Q1          1    24     8   0   
      106 999999999 Q1          1    25     4   0   
      107 999999999 Q1          1    25     5   0   
      108 999999999 Q1          1    25     6   0   
      109 999999999 Q1          1    25     7   0   
      110 999999999 Q1          1    25     8   0   
      111 999999999 Q1          1    26     4 151   
      112 999999999 Q1          1    26     5  19   
      113 999999999 Q1          1    26     6  75   
      114 999999999 Q1          1    26     7  43   
      115 999999999 Q1          1    26     8  14   
      116 999999999 Q1          1    27     4 151   
      117 999999999 Q1          1    27     5  15   
      118 999999999 Q1          1    27     6  71.5 
      119 999999999 Q1          1    27     7  47.5 
      120 999999999 Q1          1    27     8  17   
      121 999999999 Q1          1    28     4 100   
      122 999999999 Q1          1    28     5 100   
      123 999999999 Q1          1    28     6 100   
      124 999999999 Q1          1    28     7 100   
      125 999999999 Q1          1    28     8 100   
      126 999999999 Q1          1    29     4 100   
      127 999999999 Q1          1    29     5 100   
      128 999999999 Q1          1    29     6 100   
      129 999999999 Q1          1    29     7 100   
      130 999999999 Q1          1    29     8 100   
      
      $row_table
      # A tibble: 30 x 18
            BookNo RowNo RowContent RowAbsPercent RowWeighted  TabNo RowTitle1                                               RowTitle2                  RowTitle3 RowFormat RowDecimals RowVariable RowValue row_type           QuestNo RowTypeS                  RowType RowContentDetail
             <dbl> <int> <chr>      <chr>         <chr>        <int> <chr>                                                   <chr>                      <chr>     <chr>           <int> <chr>          <dbl> <chr>              <chr>   <chr>                       <int> <chr>           
       1 999999999     1 Title      ""            ""               1 "1. Wie viele Mitarbeiter beschäftigt Ihr Unternehmen?" ""                         ""        <NA>               NA <NA>              NA <NA>               Q1      Title                           1 ""              
       2 999999999     2 Header     ""            ""               1  <NA>                                                   ""                         ""        <NA>               NA <NA>              NA <NA>               Q1      Header                          2 ""              
       3 999999999     3 Header     ""            ""               1  <NA>                                                   ""                         ""        <NA>               NA <NA>              NA <NA>               Q1      Header                          2 ""              
       4 999999999     4 Total      "Abs"         "Unweighted"     1 "GESAMT"                                                "GESAMT"                   "abs"     <NA>                0 q1                 1 total              Q1      Total|AbsUnweighted       2097408 ""              
       5 999999999     5 Total      "Abs"         "Weighted"       1 "GESAMT"                                                "GESAMT"                   "abs⚖"    <NA>                0 q1                 1 total              Q1      Total|AbsWeighted         1048832 ""              
       6 999999999     6 Detail     "Abs"         "Unweighted"     1 "1 Beschäftigter"                                       "1 Beschäftigter"          "abs"     <NA>                0 q1                 1 detail_freqs_valid Q1      Detail|AbsUnweighted      2097168 ""              
       7 999999999     7 Detail     "Abs"         "Weighted"       1 "1 Beschäftigter"                                       "1 Beschäftigter"          "abs⚖"    <NA>                0 q1                 1 detail_freqs_valid Q1      Detail|AbsWeighted        1048592 ""              
       8 999999999     8 Detail     "Percent"     "Unweighted"     1 "1 Beschäftigter"                                       "1 Beschäftigter"          "in %"    <NA>                1 q1                 1 detail_perc_valid  Q1      Detail|PercentUnweighted 33554448 ""              
       9 999999999     9 Detail     "Percent"     "Weighted"       1 "1 Beschäftigter"                                       "1 Beschäftigter"          "in %⚖"   <NA>                1 q1                 1 detail_perc_valid  Q1      Detail|PercentWeighted   16777232 ""              
      10 999999999    10 Detail     "Abs"         "Unweighted"     1 "2 - 4 Beschäftigte"                                    "2 - 4 Beschäftigte"       "abs"     <NA>                0 q1                 2 detail_freqs_valid Q1      Detail|AbsUnweighted      2097168 ""              
      11 999999999    11 Detail     "Abs"         "Weighted"       1 "2 - 4 Beschäftigte"                                    "2 - 4 Beschäftigte"       "abs⚖"    <NA>                0 q1                 2 detail_freqs_valid Q1      Detail|AbsWeighted        1048592 ""              
      12 999999999    12 Detail     "Percent"     "Unweighted"     1 "2 - 4 Beschäftigte"                                    "2 - 4 Beschäftigte"       "in %"    <NA>                1 q1                 2 detail_perc_valid  Q1      Detail|PercentUnweighted 33554448 ""              
      13 999999999    13 Detail     "Percent"     "Weighted"       1 "2 - 4 Beschäftigte"                                    "2 - 4 Beschäftigte"       "in %⚖"   <NA>                1 q1                 2 detail_perc_valid  Q1      Detail|PercentWeighted   16777232 ""              
      14 999999999    14 Detail     "Abs"         "Unweighted"     1 "5 - 9 Beschäftigte"                                    "5 - 9 Beschäftigte"       "abs"     <NA>                0 q1                 3 detail_freqs_valid Q1      Detail|AbsUnweighted      2097168 ""              
      15 999999999    15 Detail     "Abs"         "Weighted"       1 "5 - 9 Beschäftigte"                                    "5 - 9 Beschäftigte"       "abs⚖"    <NA>                0 q1                 3 detail_freqs_valid Q1      Detail|AbsWeighted        1048592 ""              
      16 999999999    16 Detail     "Percent"     "Unweighted"     1 "5 - 9 Beschäftigte"                                    "5 - 9 Beschäftigte"       "in %"    <NA>                1 q1                 3 detail_perc_valid  Q1      Detail|PercentUnweighted 33554448 ""              
      17 999999999    17 Detail     "Percent"     "Weighted"       1 "5 - 9 Beschäftigte"                                    "5 - 9 Beschäftigte"       "in %⚖"   <NA>                1 q1                 3 detail_perc_valid  Q1      Detail|PercentWeighted   16777232 ""              
      18 999999999    18 Detail     "Abs"         "Unweighted"     1 "10 - 19 Beschäftigte"                                  "10 - 19 Beschäftigte"     "abs"     <NA>                0 q1                 4 detail_freqs_valid Q1      Detail|AbsUnweighted      2097168 ""              
      19 999999999    19 Detail     "Abs"         "Weighted"       1 "10 - 19 Beschäftigte"                                  "10 - 19 Beschäftigte"     "abs⚖"    <NA>                0 q1                 4 detail_freqs_valid Q1      Detail|AbsWeighted        1048592 ""              
      20 999999999    20 Detail     "Percent"     "Unweighted"     1 "10 - 19 Beschäftigte"                                  "10 - 19 Beschäftigte"     "in %"    <NA>                1 q1                 4 detail_perc_valid  Q1      Detail|PercentUnweighted 33554448 ""              
      21 999999999    21 Detail     "Percent"     "Weighted"       1 "10 - 19 Beschäftigte"                                  "10 - 19 Beschäftigte"     "in %⚖"   <NA>                1 q1                 4 detail_perc_valid  Q1      Detail|PercentWeighted   16777232 ""              
      22 999999999    22 Detail     "Abs"         "Unweighted"     1 "20 Beschäftigte und mehr"                              "20 Beschäftigte und mehr" "abs"     <NA>                0 q1                 5 detail_freqs_valid Q1      Detail|AbsUnweighted      2097168 ""              
      23 999999999    23 Detail     "Abs"         "Weighted"       1 "20 Beschäftigte und mehr"                              "20 Beschäftigte und mehr" "abs⚖"    <NA>                0 q1                 5 detail_freqs_valid Q1      Detail|AbsWeighted        1048592 ""              
      24 999999999    24 Detail     "Percent"     "Unweighted"     1 "20 Beschäftigte und mehr"                              "20 Beschäftigte und mehr" "in %"    <NA>                1 q1                 5 detail_perc_valid  Q1      Detail|PercentUnweighted 33554448 ""              
      25 999999999    25 Detail     "Percent"     "Weighted"       1 "20 Beschäftigte und mehr"                              "20 Beschäftigte und mehr" "in %⚖"   <NA>                1 q1                 5 detail_perc_valid  Q1      Detail|PercentWeighted   16777232 ""              
      26 999999999    26 Valid      "Abs"         "Unweighted"     1 "GÜLTIGE FÄLLE"                                         "GÜLTIGE FÄLLE"            "abs"     <NA>                0 q1                 1 n_valid_freqs      Q1      Valid|AbsUnweighted       2097664 ""              
      27 999999999    27 Valid      "Abs"         "Weighted"       1 "GÜLTIGE FÄLLE"                                         "GÜLTIGE FÄLLE"            "abs⚖"    <NA>                0 q1                 1 n_valid_freqs      Q1      Valid|AbsWeighted         1049088 ""              
      28 999999999    28 Valid      "Percent"     "Unweighted"     1 "GÜLTIGE FÄLLE"                                         "GÜLTIGE FÄLLE"            "in %"    <NA>                1 q1                 1 n_valid_perc       Q1      Valid|PercentUnweighted  33554944 ""              
      29 999999999    29 Valid      "Percent"     "Weighted"       1 "GÜLTIGE FÄLLE"                                         "GÜLTIGE FÄLLE"            "in %⚖"   <NA>                1 q1                 1 n_valid_perc       Q1      Valid|PercentWeighted    16777728 ""              
      30 999999999    30 Empty      ""            ""               1 ""                                                      ""                         ""        <NA>               NA <NA>              NA <NA>               Q1      Empty                           4 ""              
      
      $head_table
      # A tibble: 5 x 5
           BookNo HeadNo HeadName     HeadTitle HeadCount
            <dbl>  <int> <chr>        <chr>         <int>
      1 999999999      1 DC#ROWHEADER <NA>              3
      2 999999999      2 DC#TOTAL     GESAMT            1
      3 999999999      3 kregio@1     Region            4
      4 999999999      4 DC#EMPTY     <NA>              1
      5 999999999      5 DC#TITLE     <NA>              1
      
      $col_table_all
         BookNo ColNo HeadNo ColTitle1   ColTitle2  ColVariable ColValue
      1   1e+09     1      1                       DC#ROWHEADER       NA
      2   1e+09     2      1                       DC#ROWHEADER       NA
      3   1e+09     3      1                       DC#ROWHEADER       NA
      4   1e+09     4      2    GESAMT                 DC#TOTAL        1
      5   1e+09     5      3    Region       Asien       kregio        1
      6   1e+09     6      3    Region      Europa       kregio        2
      7   1e+09     7      3    Region Nordamerika       kregio        3
      8   1e+09     8      3    Region     Pazifik       kregio        4
      9   1e+09     9      4                           DC#EMPTY       NA
      10  1e+09    10      5                           DC#TITLE       NA
      

