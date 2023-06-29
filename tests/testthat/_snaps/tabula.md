# cross-tabulation works for cat

    Code
      df_crosstab_cat
    Output
      # A tibble: 5 x 12
        rowvar rowval `DC#STICHPROBE_1`  q2_1  q2_2 q2_99  q3_1  q3_2  q3_3  q3_4
        <chr>   <dbl>             <int> <int> <int> <int> <int> <int> <int> <int>
      1 q2_TC       1                95    28    36    31    16    19    15    18
      2 q2          1                28    28    NA    NA     2     7     7     4
      3 q2          2                36    NA    36    NA     8     7     3     8
      4 q2         99                31    NA    NA    31     6     5     5     6
      5 q2_VC       1                64    28    36    NA    10    14    10    12
      # i 2 more variables: q3_5 <int>, q3_99 <int>

