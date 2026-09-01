# mcg tables with invalids & Exclusive are reproduced

    Code
      m_rm_invalids
    Output
      $`2`
      $`2`[[1]]
      # mcg with invalids
                                 TOTAL age   -----
                                       18-39 40+  
      TOTAL                abs     3       2     1
      Sum of valid answe…  abs     3       1     2
      ch1                  abs     1       0     1
                           in %   50       0   100
      ch2                  abs     2       1     1
                           in %  100     100   100
      VALID CASES          abs     2       1     1
                           in %   66.7    50   100
      ch3                  abs     1       1     0
                           in %   33.3    50     0
      
      
      $`3`
      $`3`[[1]]
      # mcg with invalids & exclusive
                                 TOTAL age   -----
                                       18-39 40+  
      TOTAL                abs     3       2     1
      Sum of valid answe…  abs     2       1     1
      ch1                  abs     0       0     0
                           in %    0       0     0
      ch2                  abs     2       1     1
                           in %  100     100   100
      VALID CASES          abs     2       1     1
                           in %   66.7    50   100
      ch3                  abs     1       1     0
                           in %   33.3    50     0
      
      
      $`4`
      $`4`[[1]]
      # mcg with NA & exclusive
                                 TOTAL age   -----
                                       18-39 40+  
      TOTAL                abs     3       2     1
      Sum of valid answe…  abs     4       2     2
      0                    abs     1       0     1
                           in %   50       0   100
      ch1                  abs     0       0     0
                           in %    0       0     0
      ch2                  abs     0       0     0
                           in %    0       0     0
      ch3                  abs     2       1     1
                           in %  100     100   100
      ch4                  abs     1       1     0
                           in %   50     100     0
      VALID CASES          abs     2       1     1
                           in %   66.7    50   100
      
      
      $`5`
      $`5`[[1]]
      # mdg with invalids
                                 TOTAL age   -----
                                       18-39 40+  
      TOTAL                abs     3       2     1
      Sum of valid answe…  abs     3       1     2
      Choice 1             abs     1       0     1
                           in %   50       0   100
      Choice 2             abs     2       1     1
                           in %  100     100   100
      VALID CASES          abs     2       1     1
                           in %   66.7    50   100
      Choice 3             abs     1       1     0
                           in %   33.3    50     0
      
      
      $`6`
      $`6`[[1]]
      # mdg with invalids & exclusive
                                 TOTAL age   -----
                                       18-39 40+  
      TOTAL                abs     3       2     1
      Sum of valid answe…  abs     2       1     1
      Choice 1             abs     0       0     0
                           in %    0       0     0
      Choice 2             abs     2       1     1
                           in %  100     100   100
      VALID CASES          abs     2       1     1
                           in %   66.7    50   100
      Choice 3             abs     1       1     0
                           in %   33.3    50     0
      
      
      $`7`
      $`7`[[1]]
      # mcg with non-labelled non-exclusively occurring value
                                 TOTAL age   -----
                                       18-39 40+  
      TOTAL                abs     3       2     1
      Sum of valid answe…  abs     6       3     3
      ch1                  abs     0       0     0
                           in %    0       0     0
      ch2                  abs     2       1     1
                           in %   66.7    50   100
      ch3                  abs     2       1     1
                           in %   66.7    50   100
      ch4                  abs     2       1     1
                           in %   66.7    50   100
      VALID CASES          abs     3       2     1
                           in %  100     100   100
      
      
      $`8`
      $`8`[[1]]
      # mcg with 1 non-labelled non-exclusively occurring and 1 labelled
      #   non-occurring value
                                 TOTAL age   -----
                                       18-39 40+  
      TOTAL                abs     3       2     1
      Sum of valid answe…  abs     6       3     3
      ch1                  abs     0       0     0
                           in %    0       0     0
      ch2                  abs     2       1     1
                           in %   66.7    50   100
      ch3                  abs     2       1     1
                           in %   66.7    50   100
      ch4                  abs     2       1     1
                           in %   66.7    50   100
      VALID CASES          abs     3       2     1
                           in %  100     100   100
      
      

# a table is reproduced with a global filter

    Code
      m_filter$qrows$`2`$qtabs
    Output
      [[1]]
      # mcg with invalids
                                 TOTAL age   -----
                                       18-39 40+  
      TOTAL                abs       2     2     0
      Sum of valid answe…  abs       1     1     0
      ch1                  abs       0     0     0
                           in %      0     0     0
      ch2                  abs       1     1     0
                           in %    100   100     0
      VALID CASES          abs       1     1     0
                           in %     50    50     0
      ch3                  abs       1     1     0
                           in %     50    50     0
      

