# Tabulation class

The class `Tabula` can be used to calculate the crosstabs specified on
the Questions sheet of the Excel mapping file.

## Super class

[`datadaptor::Mapping`](https://urswilke.codeberg.page/datadaptor/reference/Mapping.html)
-\> `Tabula`

## Public fields

- `dat_mod`:

  `dat_mod` modified data field of the super-class
  [`datadaptor::Mapping`](https://urswilke.codeberg.page/datadaptor/reference/Mapping.html).

- `mapping_file`:

  `mapping_file` file path field of the super-class
  [`datadaptor::Mapping`](https://urswilke.codeberg.page/datadaptor/reference/Mapping.html).

- `dat`:

  `dat` input data field of the super-class
  [`datadaptor::Mapping`](https://urswilke.codeberg.page/datadaptor/reference/Mapping.html).
  If this is specified, `dat_mod` will be ignored, and instead generated
  with `datadaptor::Mapping$modify_data()`

- `qrows`:

  A [`list()`](https://rdrr.io/r/base/list.html) of `Qrow` objects

- `ditw`:

  This is the "dust in the wind" list object field that stores data that
  didn't make it into their own field. For developers only! For
  reproducible code you should NEVER rely on this field as it might be
  subject to change without any warning. This overwrites the
  `datadaptor::Mapping$ditw` field; the list field additionally contains
  the `ct` element.

## Methods

### Public methods

- [`Tabula$new()`](#method-Tabula-initialize)

- [`Tabula$set_options()`](#method-Tabula-set_options)

- [`Tabula$calc_crosstabs()`](#method-Tabula-calc_crosstabs)

- [`Tabula$save_html_app()`](#method-Tabula-save_html_app)

- [`Tabula$get_crosstabs_data()`](#method-Tabula-get_crosstabs_data)

- [`Tabula$print()`](#method-Tabula-print)

- [`Tabula$clone()`](#method-Tabula-clone)

Inherited methods

- [`datadaptor::Mapping$modify_data()`](https://urswilke.codeberg.page/datadaptor/reference/Mapping.html#method-modify_data)
- [`datadaptor::Mapping$process_sheet_commands()`](https://urswilke.codeberg.page/datadaptor/reference/Mapping.html#method-process_sheet_commands)
- [`datadaptor::Mapping$read_data()`](https://urswilke.codeberg.page/datadaptor/reference/Mapping.html#method-read_data)
- [`datadaptor::Mapping$save()`](https://urswilke.codeberg.page/datadaptor/reference/Mapping.html#method-save)

------------------------------------------------------------------------

### `Tabula$new()`

Initialize a Tabula object

#### Usage

    Tabula$new(
      dat_mod = NULL,
      mapping_file = NULL,
      row = NULL,
      dat = NULL,
      tabulate = TRUE,
      ...
    )

#### Arguments

- `dat_mod`:

  `dat_mod` modified data field of the super-class
  [`datadaptor::Mapping`](https://urswilke.codeberg.page/datadaptor/reference/Mapping.html).

- `mapping_file`:

  `mapping_file` file path field of the super-class
  [`datadaptor::Mapping`](https://urswilke.codeberg.page/datadaptor/reference/Mapping.html).

- `row`:

  Numeric vector with the row numbers in the Questions sheet, where
  crosstabs should be calculated, when calling
  `Tabula$calc_crosstabs()`. Or `NULL` (the default) resulting in the
  selection of all row numbers where `Type` is specified.

- `dat`:

  `dat` input data field of the super-class
  [`datadaptor::Mapping`](https://urswilke.codeberg.page/datadaptor/reference/Mapping.html).
  If this is specified, `dat_mod` will be ignored, and instead generated
  with `datadaptor::Mapping$modify_data()`

- `tabulate`:

  Logical, whether to call the `Tabula$calc_crosstabs()` method when
  initializing (defaults to `TRUE`).

- `...`:

  Arguments passed to `Tabula$set_options()`

------------------------------------------------------------------------

### `Tabula$set_options()`

Set `Tabula` options. This overwrites
`datadaptor::Mapping$set_options()`

#### Usage

    Tabula$set_options(...)

#### Arguments

- `...`:

  Arguments passed to
  [`get_tabula_options()`](https://urswilke.codeberg.page/crosstabser/reference/get_tabula_options.md).

------------------------------------------------------------------------

### `Tabula$calc_crosstabs()`

Calculate the crosstabs

#### Usage

    Tabula$calc_crosstabs(row = NULL)

#### Arguments

- `row`:

  Numeric vector with the row numbers in the Questions sheet, where
  crosstabs should be calculated, when calling
  `Tabula$calc_crosstabs()`. Or `NULL` (the default) resulting in the
  selection of all row numbers where `Type` is specified.

------------------------------------------------------------------------

### `Tabula$save_html_app()`

Write a table_charter app html file of the crosstab data

#### Usage

    Tabula$save_html_app(
      template_file =
        "https://gitlab.com/urswilke/table_charter/-/raw/main/example_dashboard.html",
      output_file = "dashboard.html",
      project_data = NULL
    )

#### Arguments

- `template_file`:

  Path to the template file (see description).

- `output_file`:

  File path to the table_charter app html file.

- `project_data`:

  Either a [`list()`](https://rdrr.io/r/base/list.html) object to modify
  the default:
  `list(logo_base64 = "", logo_url = "https://gitlab.com/urswilke/table_charter/-/raw/main/img/logo_small.svg", title = "Dashboard", date = Sys.Date())`,
  or `NULL` (the default). If `NULL`, nothing is done. The fields will
  modify the elements in the header of the dashboard.

#### Details

This needs a valid html `template_file`, i.e. one of:

- The file
  [example_dashboard.html](https://gitlab.com/urswilke/table_charter/-/blob/main/example_dashboard.html)
  which is directly scraped from the table_charter repo by default (no
  installation of table_charter needed).

- For [deploying it in the
  web](https://gitlab.com/urswilke/table_charter#deploying-it-in-the-web)
  or [running it on a dev
  server](https://gitlab.com/urswilke/table_charter#dev-server), you
  need to [install
  table_charter](https://gitlab.com/urswilke/table_charter#installation)
  first, and then use the file
  [index.html](https://gitlab.com/urswilke/table_charter/-/blob/main/index.html)
  on your machine.

- After installing, you can also generate a standalone html file
  (without the need to download javascript libraries) by running:

        npm run standalone-build

  and then using the template file created in the `dist/` sub-directory.

------------------------------------------------------------------------

### `Tabula$get_crosstabs_data()`

Return the crosstabs data of the `Tabula` object

This method returns a list of dataframes containing all the crosstabs
information. Thus it's not chainable.

#### Usage

    Tabula$get_crosstabs_data()

#### Returns

A list of dataframes with the data of the crosstabs; see
[`vignette("data-format")`](https://urswilke.codeberg.page/crosstabser/articles/data-format.md).

------------------------------------------------------------------------

### `Tabula$print()`

Print the crosstabs of the `Tabula` object

This method is called under the hood, if you
[`print()`](https://rdrr.io/r/base/print.html) a `Tabula` object. This
will call the print method of all `Qrow` elements in the `Tabula$qrows`
field.

#### Usage

    Tabula$print(...)

#### Arguments

- `...`:

  Not used for now.

------------------------------------------------------------------------

### `Tabula$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Tabula$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
df <- tibble::tibble(
  q1 = c(1, 2, 1) |> haven::labelled(c(Yes = 1, No = 2), label = "Super important question"),
  age = c(2, 1, 1) |> haven::labelled(c("18-39" = 1, "40+" = 2), label = "age")
)
mapping_file = list(
  Questions = data.frame(
    Type  = "cat",
    RowVar = "q1",
    Title = "The crosstab's title"
  ),
  Macro = list(ColVar = "age")
)
m <- Tabula$new(df, mapping_file)
m
#> $`2`
#> $`2`[[1]]
#> # The crosstab's title
#>                            TOTAL   age -----
#>                                  18-39   40+
#> TOTAL                abs     3       2     1
#> Yes                  abs     2       1     1
#>                      in %   66.7    50   100
#> No                   abs     1       1     0
#>                      in %   33.3    50     0
#> VALID CASES          abs     3       2     1
#>                      in %  100     100   100
#> 
#> 
# The previous line prints the "Tabula" object.
# Under the hood, a list of `Qrow` objects were generated.
# Printing `m` prints the list of `Qtab` elements of each `Qrow`:
m$qrows
#> $`2`
#> <Qrow>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     ditw: list
#>     initialize: function (df_qrow, mapping, ...) 
#>     log: list
#>     m: Tabula, Mapping, R6
#>     p: list
#>     qtabs: list
#>   Private:
#>     prep_tab_row_val: function () 
#> 
# For instance, this prints the list of `Qtab` elements
# of the first `Qrow` element:
m$qrows[[1]]$qtabs |> print()
#> [[1]]
#> # The crosstab's title
#>                            TOTAL   age -----
#>                                  18-39   40+
#> TOTAL                abs     3       2     1
#> Yes                  abs     2       1     1
#>                      in %   66.7    50   100
#> No                   abs     1       1     0
#>                      in %   33.3    50     0
#> VALID CASES          abs     3       2     1
#>                      in %  100     100   100
#> 
```
