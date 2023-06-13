# cross-tabulation works for cat

    Code
      df_crosstab_cat
    Output
      # A tibble: 4 x 13
        rowvar rowval `q2_-2`  q2_1  q2_2 q2_99 `q3_-2`  q3_1  q3_2  q3_3  q3_4  q3_5
        <chr>   <dbl>   <int> <int> <int> <int>   <int> <int> <int> <int> <int> <int>
      1 q2         -2       4    NA    NA    NA      NA     1     2    NA     1    NA
      2 q2          1      NA    28    NA    NA       1     2     7     7     4     7
      3 q2          2      NA    NA    36    NA       1     8     7     3     8     5
      4 q2         99      NA    NA    NA    31      NA     6     5     5     6     6
      # i 1 more variable: q3_99 <int>

