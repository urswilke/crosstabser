# cross-tabulation works for cat

    Code
      df_crosstab_cat
    Output
      # A tibble: 5 x 14
        rowvar rowval RowContent RowAbsPercent `DC#STICHPROBE_1`  q2_1  q2_2 q2_99
        <chr>   <dbl> <chr>      <chr>                     <int> <int> <int> <int>
      1 q2          1 Total      Abs                          95    28    36    31
      2 q2          1 Detail     Abs                          28    28    NA    NA
      3 q2          2 Detail     Abs                          36    NA    36    NA
      4 q2         99 Detail     Abs                          31    NA    NA    31
      5 q2          1 Valid      Abs                          64    28    36    NA
      # i 6 more variables: q3_1 <int>, q3_2 <int>, q3_3 <int>, q3_4 <int>,
      #   q3_5 <int>, q3_99 <int>

