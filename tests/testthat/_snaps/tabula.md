# cross-tabulation works for cat

    Code
      df_crosstab_cat
    Output
      # A tibble: 5 x 14
        rowvar rowval `DC#STICHPROBE_1`  q2_1  q2_2 q2_99 q2_NA  q3_1  q3_2  q3_3
        <chr>   <dbl>             <int> <int> <int> <int> <int> <int> <int> <int>
      1 q2_TC       1                99    28    36    31     4    17    21    15
      2 q2          1                28    28    NA    NA    NA     2     7     7
      3 q2          2                36    NA    36    NA    NA     8     7     3
      4 q2         99                31    NA    NA    31    NA     6     5     5
      5 q2         NA                 4    NA    NA    NA     4     1     2    NA
      # i 4 more variables: q3_4 <int>, q3_5 <int>, q3_99 <int>, q3_NA <int>

