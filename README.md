
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crosstabser

*The main home of this repository is
[here](https://codeberg.org/urswilke/crosstabser)*

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/crosstabser)](https://CRAN.R-project.org/package=crosstabser)
[![crosstabser status
badge](https://urswilke.r-universe.dev/crosstabser/badges/version)](https://urswilke.r-universe.dev/crosstabser)
<!-- badges: end -->

The goal of crosstabser is to generate crosstabs from labelled data
(usually survey data). Its parent dependency
[datadaptor](https://gitlab.com/urswilke/datadaptor) uses a similar
approach to modify datasets. The generated crosstabs can be plotted
interactively in html files with the apps provided by
[table_charter](https://gitlab.com/urswilke/table_charter).
[Here](https://urswilke.github.io/datadaptor-crosstabser-table_charter-demo/)
you can find an interactive introduction that also shows how the
resulting table_charter app can be dynamically created with the data
from crosstabser. This package (together with datadaptor) is also used
to generate the crosstabs in [table
books](https://www.data-connection.de/table-book.html) (commercial
product). <!-- TODO: refer to spreadator!-->

## Installation

You can install crosstabser from codeberg with:

``` r
devtools::install_git("https://codeberg.org/urswilke/crosstabser")
```

Otherwise, you can also install crosstabser from github or gitlab with:

``` r
devtools::install_github("urswilke/crosstabser")
devtools::install_gitlab("urswilke/crosstabser")
```

## Minimal example

First we’ll load the package.

``` r
library(crosstabser)
```

Now, suppose you have survey data `df`.

``` r
df <- tibble::tibble(
  q1 = c(1, 2, 1) |> haven::labelled(c(Yes = 1, No = 2), label = "Super important question"),
  age = c(2, 1, 1) |> haven::labelled(c("18-39" = 1, "40+" = 2), label = "age")
)
df
#> # A tibble: 3 × 2
#>   q1        age      
#>   <dbl+lbl> <dbl+lbl>
#> 1 1 [Yes]   2 [40+]  
#> 2 2 [No]    1 [18-39]
#> 3 1 [Yes]   1 [18-39]
```

The data consists of 3 cases with the answers Yes & No stored in the
variable `q1` and the age category stored in the variable `age`.

And suppose you want to generate a crosstab, summarizing the values of
`q1` on the y-axis, and on the x-axis showing the total counts, as well
as those in the sub-populations of the variable `age`.

crosstabser allows this if you define a mapping object like this:

``` r
(mapping_file = list(
  Questions = data.frame(
    Type  = "cat",
    RowVar = "q1",
    Title = "The crosstab's title"
  ),
  Macro = list(ColVar = "age")
))
#> $Questions
#>   Type RowVar                Title
#> 1  cat     q1 The crosstab's title
#> 
#> $Macro
#> $Macro$ColVar
#> [1] "age"
```

This is how you can calculate crosstabs and print them to the console:

``` r
Tabula$new(df, mapping_file)
#> $`2`
#> $`2`[[1]]
#> # The crosstab's title
#>                            TOTAL age   -----
#>                                  18-39 40+  
#> TOTAL                abs     3       2     1
#> Yes                  abs     2       1     1
#>                      in %   66.7    50   100
#> No                   abs     1       1     0
#>                      in %   33.3    50     0
#> VALID CASES          abs     3       2     1
#>                      in %  100     100   100
```

Please refer to `vignette("crosstabser")` for a more thorough
introduction, and `vignette("questions-parameters")` for details how to
use the parameters in the mapping. In `vignette("data-format")` we have
a closer look at the structure of the crosstabs’ underlying data.
