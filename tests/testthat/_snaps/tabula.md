# cross-tabulation works for cat

    Code
      df_crosstab_cat
    Output
      # A tibble: 8 x 8
        rowvar rowval RowContent RowAbsPercent `DC#STICHPROBE_1`  q2_1  q2_2 q2_99
        <chr>   <dbl> <chr>      <chr>                     <dbl> <dbl> <dbl> <dbl>
      1 q2          1 Total      Abs                      95        28    36    31
      2 q2          1 Detail     Abs                      28        28    NA    NA
      3 q2          2 Detail     Abs                      36        NA    36    NA
      4 q2         99 Detail     Abs                      31        NA    NA    31
      5 q2          1 Detail     Percent                   0.438     1    NA    NA
      6 q2          2 Detail     Percent                   0.562    NA     1    NA
      7 q2         99 Detail     Percent                   0.484    NA    NA    NA
      8 q2          1 Valid      Abs                      64        28    36    NA

# wide_tab repoduced for cat

    Code
      wide_tab
    Output
      # A tibble: 6 x 5
        RowNo    `4`   `5`   `6`   `7`
        <dbl>  <dbl> <dbl> <dbl> <dbl>
      1     1 95        28    36    31
      2     2 28        28    NA    NA
      3     3  0.438     1    NA    NA
      4     4 36        NA    36    NA
      5     5  0.562    NA     1    NA
      6     6 64        28    36    NA

