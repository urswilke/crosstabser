
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crosstabser

<!-- badges: start -->
<!-- badges: end -->

The goal of crosstabser is to …

## Installation

You can install the development version of crosstabser like so:

``` r
devtools::install_gitlab("urswilke/crosstabser")
```

## Example

First we’ll load the package.

``` r
library(crosstabser)
```

### Minimal example

Suppose you have survey data `df`.

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
variable `q1` and there age category stored in the variable `age`.

And suppose you want to generate a crosstab, summarizing the values of
`q1` on the y-axis, and on the x-axis showing the total counts, as well
as those in the sub-populations of the variable `age`.

crosstabser allows this if you define a mapping object like this:

``` r
(mapping_file = list(Questions = data.frame(
  Type  = "cat",
  RowVar = "q1",
  Title = "The crosstab's title"
)))
#> $Questions
#>   Type RowVar                Title
#> 1  cat     q1 The crosstab's title
```

This is a list with one element `"Questions"` containing a one row
data.frame. Then you can use the package’s `Tabula` class to generate
print output of the crosstab in the console:

``` r
Tabula$new(df, mapping_file, colvar = "age")
#> [[1]]
#> [[1]][[1]]
#> # The crosstab's title
#>                       NULL TOTAL age   -----
#>                       NULL       18-39 40+  
#> TOTAL                abs     3       2     1
#> Yes                  abs     2       1     1
#>                      in %   66.7    50   100
#> No                   abs     1       1     0
#>                      in %   33.3    50     0
#> VALID CASES          abs     3       2     1
#>                      in %  100     100   100
```
