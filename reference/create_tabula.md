# Create an Excel mapping file based on a labelled dataframe

This function first calls
[`datadaptor::create_mapping_workbook()`](https://urswilke.gitlab.io/datadaptor/reference/create_mapping_workbook.html).
Additionally to the sheets "Variables", "Label", "Verbatims" & "Free",
it will insert 2 more sheets "Macro" & "Questions". The Excel workbook
is then written to the file `mapping_file`. Please refer to
[`vignette("crosstabser")`](https://urswilke.codeberg.page/crosstabser/articles/crosstabser.md)
&
[`vignette("questions-parameters")`](https://urswilke.codeberg.page/crosstabser/articles/questions-parameters.md)
for details how to use the mapping.

## Usage

``` r
create_tabula(df_raw, mapping_file, mapping_type = "excel")
```

## Arguments

- df_raw:

  dataframe with labelled variables, e.g. resulting from haven::read_sav

- mapping_file:

  name of the Excel file to be created

- mapping_type:

  String specifying the mapping type. Either "excel" or "list". Defaults
  to "excel".

## Examples

``` r
spss_file <- system.file(
  "extdata",
  "fruit_survey.sav",
  package = "datadaptor"
)
df <- haven::read_sav(spss_file)
# The next command creates an empty mapping file `mapping.xlsx`:
if (FALSE) { # \dontrun{
create_tabula(df, "mapping.xlsx")
} # }
```
