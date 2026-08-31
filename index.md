# crosstabser

*The main home of this repository is
[here](https://codeberg.org/urswilke/crosstabser)*

The goal of crosstabser is to generate crosstabs from labelled data
(usually survey data). Its parent dependency
[datadaptor](https://codeberg.org/urswilke/datadaptor) uses a similar
approach to modify datasets. The generated crosstabs can be plotted
interactively in html files with the apps provided by
[table_charter](https://gitlab.com/urswilke/table_charter).
[Here](https://urswilke.github.io/datadaptor-crosstabser-table_charter-demo/)
you can find an interactive introduction that also shows how the
resulting table_charter app can be dynamically created with the data
from crosstabser. This package (together with datadaptor) is also used
to generate the crosstabs in [table
books](https://www.data-connection.de/table-book.html) (commercial
product).

## Installation

You can install crosstabser from codeberg with:

`devtools``::`[`install_git`](https://devtools.r-lib.org/reference/install-deprecated.html)`(``"https://codeberg.org/urswilke/crosstabser"``)`

Otherwise, you can also install crosstabser from github or gitlab with:

`devtools``::`[`install_github`](https://devtools.r-lib.org/reference/install-deprecated.html)`(``"urswilke/crosstabser"``)`` ``devtools``::`[`install_gitlab`](https://devtools.r-lib.org/reference/install-deprecated.html)`(``"urswilke/crosstabser"``)`

## Minimal example

First we’ll load the package.

[`library`](https://rdrr.io/r/base/library.html)`(`[`crosstabser`](https://urswilke.codeberg.page/crosstabser)`)`

Now, suppose you have survey data `df`.

`df`` ``<-`` ``tibble``::`[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` q1 ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``2``, ``1``)`` ``|>`` ``haven``::`[`labelled`](https://haven.tidyverse.org/reference/labelled.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``Yes ``=`` ``1``, No ``=`` ``2``)``, label ``=`` ``"Super important question"``)``,`` `` age ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2``, ``1``, ``1``)`` ``|>`` ``haven``::`[`labelled`](https://haven.tidyverse.org/reference/labelled.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"18-39"`` ``=`` ``1``, ``"40+"`` ``=`` ``2``)``, label ``=`` ``"age"``)`` ``)`` ``df`` ``#> # A tibble: 3 × 2`` ``#> q1 age `` ``#> <dbl+lbl> <dbl+lbl>`` ``#> 1 1 [Yes] 2 [40+] `` ``#> 2 2 [No] 1 [18-39]`` ``#> 3 1 [Yes] 1 [18-39]`

The data consists of 3 cases with the answers Yes & No stored in the
variable `q1` and the age category stored in the variable `age`.

And suppose you want to generate a crosstab, summarizing the values of
`q1` on the y-axis, and on the x-axis showing the total counts, as well
as those in the sub-populations of the variable `age`.

crosstabser allows this if you define a mapping object like this:

`(``mapping_file`` ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` Questions ``=`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`` `` Type ``=`` ``"cat"``,`` `` RowVar ``=`` ``"q1"``,`` `` Title ``=`` ``"The crosstab's title"`` `` ``)``,`` `` Macro ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``ColVar ``=`` ``"age"``)`` ``)``)`` ``#> $Questions`` ``#> Type RowVar Title`` ``#> 1 cat q1 The crosstab's title`` ``#> `` ``#> $Macro`` ``#> $Macro$ColVar`` ``#> [1] "age"`

This is how you can calculate crosstabs and print them to the console:

[`Tabula`](https://urswilke.codeberg.page/crosstabser/reference/Tabula.md)`$``new``(``df``, ``mapping_file``)`` ``` #> $`2` ``` ``` #> $`2`[[1]] ``` ``#> # The crosstab's title`` ``#> TOTAL age -----`` ``#> 18-39 40+ `` ``#> TOTAL abs 3 2 1`` ``#> Yes abs 2 1 1`` ``#> in % 66.7 50 100`` ``#> No abs 1 1 0`` ``#> in % 33.3 50 0`` ``#> VALID CASES abs 3 2 1`` ``#> in % 100 100 100`

Please refer to
[`vignette("crosstabser")`](https://urswilke.codeberg.page/crosstabser/articles/crosstabser.md)
for a more thorough introduction, and
[`vignette("questions-parameters")`](https://urswilke.codeberg.page/crosstabser/articles/questions-parameters.md)
for details how to use the parameters in the mapping. In
[`vignette("data-format")`](https://urswilke.codeberg.page/crosstabser/articles/data-format.md)
we have a closer look at the structure of the crosstabs’ underlying
data.
