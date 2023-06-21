# cross-tabulation works for cat

    Code
      df_crosstab_cat
    Output
      # A tibble: 5 x 14
        rowvar rowval `DC#STICHPROBE_1` `q2_-2`  q2_1  q2_2 q2_99 `q3_-2`  q3_1  q3_2
        <chr>   <dbl>             <int>   <int> <int> <int> <int>   <int> <int> <int>
      1 q2_TC       1                99       4    28    36    31       2    17    21
      2 q2         -2                 4       4    NA    NA    NA      NA     1     2
      3 q2          1                28      NA    28    NA    NA       1     2     7
      4 q2          2                36      NA    NA    36    NA       1     8     7
      5 q2         99                31      NA    NA    NA    31      NA     6     5
      # i 4 more variables: q3_3 <int>, q3_4 <int>, q3_5 <int>, q3_99 <int>

