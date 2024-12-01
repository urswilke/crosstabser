# mdg tables with no entries are reproduced

    Code
      m_mdg_no_entry
    Output
      $`2`
      $`2`[[1]]
      # mdg with no entry
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
      No entry in the re…  abs     1       1     0
                           in %   33.3    50     0
      
      
      $`3`
      $`3`[[1]]
      # mdg with MdgMissLab = FILTER
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
      FILTER               abs     1       1     0
                           in %   33.3    50     0
      
      
      $`4`
      $`4`[[1]]
      # mdg with MdgMissLab = new_lab & MdgMissValid
                                 TOTAL age   -----
                                       18-39 40+  
      TOTAL                abs     3       2     1
      Sum of valid answe…  abs     4       2     2
      Choice 1             abs     1       0     1
                           in %   33.3     0   100
      Choice 2             abs     2       1     1
                           in %   66.7    50   100
      new_lab              abs     1       1     0
                           in %   33.3    50     0
      VALID CASES          abs     3       2     1
                           in %  100     100   100
      
      

