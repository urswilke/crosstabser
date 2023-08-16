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
      1     4 95        28    36    31
      2     5 28        28    NA    NA
      3     6  0.412     1    NA    NA
      4     7 36        NA    36    NA
      5     8  0.529    NA     1    NA
      6     9 68        28    36     0

# wide_tab is repoduced with new mapping

    Code
      wide_tabs
    Output
      [[1]]
      # A tibble: 8 x 6
        RowNo     `4`    `5`    `6`     `7`    `8`
        <dbl>   <dbl>  <dbl>  <dbl>   <dbl>  <dbl>
      1     4 151     19     75     43      14    
      2     7  49      5     20     19       5    
      3     8   0.325  0.263  0.267  0.442   0.357
      4     9  63      6     30     21       6    
      5    10   0.417  0.316  0.4    0.488   0.429
      6    11  39      8     25      3       3    
      7    12   0.258  0.421  0.333  0.0698  0.214
      8    15 151     19     75     43      14    
      
      [[2]]
      # A tibble: 14 x 6
         RowNo      `4`     `5`     `6`     `7`     `8`
         <dbl>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
       1     4   0.874   0.789   0.88    0.930   0.786 
       2     5  46       7      25       8       6     
       3     6   0.305   0.368   0.333   0.186   0.429 
       4     7  18       3      10       4       1     
       5     8   0.119   0.158   0.133   0.0930  0.0714
       6     9  14       3       4       4       3     
       7    10   0.0927  0.158   0.0533  0.0930  0.214 
       8    11  12       2       6       2       2     
       9    12   0.0795  0.105   0.08    0.0465  0.143 
      10    13   3      NA       1       1       1     
      11    14   0.0199 NA       0.0133  0.0233  0.0714
      12    15   5       1       1       2       1     
      13    16   0.0331  0.0526  0.0133  0.0465  0.0714
      14    17 151      19      75      43      14     
      
      [[3]]
      # A tibble: 0 x 1
      # i 1 variable: RowNo <dbl>
      
      [[4]]
      # A tibble: 19 x 6
         RowNo `4`       `5`       `6`       `7`       `8`      
         <dbl> <list>    <list>    <list>    <list>    <list>   
       1     4 <dbl [3]> <dbl [3]> <dbl [3]> <dbl [3]> <dbl [3]>
       2     6 <dbl [3]> <dbl [3]> <dbl [3]> <dbl [3]> <dbl [3]>
       3     7 <dbl [3]> <dbl [3]> <dbl [3]> <dbl [3]> <dbl [3]>
       4     8 <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]>
       5     9 <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]>
       6    10 <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]>
       7    11 <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]>
       8    12 <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]>
       9    13 <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]>
      10    14 <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]>
      11    15 <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]>
      12    16 <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]>
      13    17 <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]>
      14    18 <dbl [3]> <dbl [3]> <dbl [3]> <dbl [3]> <dbl [3]>
      15    19 <dbl [3]> <dbl [3]> <dbl [3]> <dbl [3]> <dbl [3]>
      16    20 <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]>
      17    21 <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]>
      18    22 <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]>
      19    23 <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]> <dbl [1]>
      

