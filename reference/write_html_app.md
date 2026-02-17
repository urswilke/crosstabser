# Write a table_charter app html file of crosstab data

This helper function is used by `Tabula$save_html_app()` to write a
table_charter app html file of crosstab data.

## Usage

``` r
write_html_app(
  data_string,
  template_file =
    "https://gitlab.com/urswilke/table_charter/-/raw/main/example_dashboard.html",
  output_file = "dashboard.html",
  project_data = NULL
)
```

## Arguments

- data_string:

  Crosstab data produced with
  `Tabula$get_crosstabs_data() |> gen_data_json()`.

- template_file:

  Path to the template file (see description).

- output_file:

  File path to the table_charter app html file.

- project_data:

  Either a [`list()`](https://rdrr.io/r/base/list.html) object to modify
  the default:
  `list(logo_base64 = "", logo_url = "https://gitlab.com/urswilke/table_charter/-/raw/main/img/logo_small.svg", title = "Dashboard", date = Sys.Date())`,
  or `NULL` (the default). If `NULL`, nothing is done. The fields will
  modify the elements in the header of the dashboard.

## Value

No value's returned. This function writes a file.

## Examples

``` r
# See documentation of `Tabula$save_html_app()`
```
