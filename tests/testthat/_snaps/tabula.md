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

# wide_tab is repoduced with new mapping

    Code
      wide_tabs
    Output
      [[1]]
      # A tibble: 8 x 6
        RowNo     `4`    `5`    `6`     `7`    `8`
        <dbl>   <dbl>  <dbl>  <dbl>   <dbl>  <dbl>
      1     1 151     19     75     43      14    
      2     4  49      5     20     19       5    
      3     5   0.325  0.263  0.267  0.442   0.357
      4     6  63      6     30     21       6    
      5     7   0.417  0.316  0.4    0.488   0.429
      6     8  39      8     25      3       3    
      7     9   0.258  0.421  0.333  0.0698  0.214
      8    12 151     19     75     43      14    
      
      [[2]]
      # A tibble: 16 x 6
         RowNo      `4`     `5`     `6`     `7`     `8`
         <dbl>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
       1     1 151      19      75      43      14     
       2     3 132      15      66      40      11     
       3     4   0.874   0.789   0.88    0.930   0.786 
       4     5  46       7      25       8       6     
       5     6   0.305   0.368   0.333   0.186   0.429 
       6     7  18       3      10       4       1     
       7     8   0.119   0.158   0.133   0.0930  0.0714
       8     9  14       3       4       4       3     
       9    10   0.0927  0.158   0.0533  0.0930  0.214 
      10    11  12       2       6       2       2     
      11    12   0.0795  0.105   0.08    0.0465  0.143 
      12    13   3      NA       1       1       1     
      13    14   0.0199 NA       0.0133  0.0233  0.0714
      14    15   5       1       1       2       1     
      15    16   0.0331  0.0526  0.0133  0.0465  0.0714
      16    17 151      19      75      43      14     
      
      [[3]]
      # A tibble: 0 x 1
      # i 1 variable: RowNo <dbl>
      
      [[4]]
      # A tibble: 0 x 1
      # i 1 variable: RowNo <dbl>
      

