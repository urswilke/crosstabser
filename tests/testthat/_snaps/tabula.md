# cross-tabulation works for cat

    Code
      df_crosstab_cat
    Output
      # A tibble: 8 x 8
        rowval rowvar RowContent RowAbsPercent `DC#STICHPROBE_1`  q2_1  q2_2 q2_99
         <dbl> <chr>  <chr>      <chr>                     <dbl> <dbl> <dbl> <dbl>
      1      1 q2     Total      Abs                      95        28    36    31
      2      1 q2     Detail     Abs                      28        28    NA    NA
      3      2 q2     Detail     Abs                      36        NA    36    NA
      4     99 q2     Detail     Abs                      31        NA    NA    31
      5      1 q2     Detail     Percent                   0.412     1    NA    NA
      6      2 q2     Detail     Percent                   0.529    NA     1    NA
      7     99 q2     Detail     Percent                   0.456    NA    NA    NA
      8      1 q2     Valid      Abs                      68        28    36     0

# wide_tab is repoduced for cat

    Code
      wide_tab
    Output
      # A tibble: 6 x 5
        RowNo    `4`   `5`   `6`   `7`
        <dbl>  <dbl> <dbl> <dbl> <dbl>
      1     1 95        28    36    31
      2     2 28        28    NA    NA
      3     3  0.412     1    NA    NA
      4     4 36        NA    36    NA
      5     5  0.529    NA     1    NA
      6     6 68        28    36     0

