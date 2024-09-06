# TODO: check where to add helper functions (or other OOP structure or something else (?))
# for common tasks in different methods,
# e.g. stuff where mcg and mdg do the same, etc.

# TODO: check if possible to refactor code
# where information of the final 5 tables (Row, Tab, Head, Col & Val) is generated twice,
# for instance columns in row_table.R & crosstab.R

#' Tabulation class
#'
#' This is copied from datenanpassr and will just serve as a template when the
#' code will be documented...
#'
#' @description The class \code{Mapping} can be used to apply the changes
#'   specified in the command blocks of an Excel mapping file to a (labelled)
#'   dataframe.
#'
#'   The information of the Excel mapping file results in the `cmd_tbl`
#'   dataframe field of the mapping object. This dataframe has a column
#'   `command_blocks` which is applied to the data in the `dat` field by the
#'   method `modify_data()` and then results in the `dat_mod` field.
#'
#' @field dat (filepath to pass to \code{haven::read_sav()} to read in the)
#'   labelled dataframe to apply the mapping on.
#' @field mapping_file filepath of the Excel mapping file
#' @field cmd_tbl Dataframe with the command block information
#' @field cmd R list structure containing the processed command block
#'   information of the Excel mapping file. `r lifecycle::badge('experimental')`
#' @field dat_mod modified dataframe
#' @field params Parameter list object
#' @export
#'
#' @examples
#' # Create a Mapping object from the files provided by the package:
#' mapping_file <- system.file("extdata", "mapping.xlsx", package = "datenanpassr")
#' spss_file <- system.file("extdata", "mtcars_labelled.sav", package = "datenanpassr")
#' mapping <- Mapping$new(spss_file, mapping_file)
#'
#' # The spss_file path was read into a dataframe in the "dat" field of the
#' # mapping object:
#' mapping$dat
#'
#' # The Excel mapping file is translated to a `command_blocks()` object.
#' # It contains the processed information in a list structure that has
#' # its own print method.
#' # You can access it with
#' \dontrun{
#' mapping$cmd_tbl$command_blocks
#' }
#' # Apply the command blocks to the dataset:
#' mapping$modify_data()
#'
#' # Access the modified dataframe:
#' mapping$dat_mod
#'
#' # To write it back to an SPSS file, you could do:
#' # mapping$save("path/to/your/file.sav")
#' # or with haven (used under the hood by `save()`):
#' # haven::write_sav(mapping$dat_mod, "path/to/your/file.sav")
Tabula <- R6::R6Class("Tabula",
  public = list(
    mapping_file = NULL,
    dat_mod = NULL,
    options = NULL,
    long_tab_data = NULL,
    qsheet = list(),
    qtabs = list(),
    qrows = list(),
    crosstabs = list(),
    #' @description Initialize a Tabula object
    #'
    initialize = function(dat,
                          mapping_file,
                          row = NULL,
                          ...) {
      self$mapping_file <- mapping_file
      self$dat_mod <- read_data(dat)
      self$options <- setOptions(mapping_file)

      gen_col_tables(self)
      self$calc_qtabs(row)
    },
    calc_qtabs = function(row = NULL) {
      parse_qsheet(self, row)
      invisible(self)
    },
    aggregate_5_tables = function() {
      aggregate_5_tables(self)
      invisible(self)
    },
    xml = function(row = NULL) {
      write_xml_tables_from_qrows(self, row)
      invisible(self)
    },
    merge_long_tab_data = function() {
      self$long_tab_data <- gen_long_tab_data(self)
      invisible(self)
    },
    long_excel = function(file_name = "long_crosstab_data.xlsx") {
      self$merge_long_tab_data()
      list(Daten = self$long_tab_data) |>
        writexl::write_xlsx(file_name)
      invisible(self)
    },
    long_json = function(file_name = "long_crosstab_data.json", ...) {
      self$merge_long_tab_data()
      self$long_tab_data |>
        jsonlite::toJSON(...) |>
        writeLines(file_name)
      invisible(self)
    },
    long_csv = function(file_name = "long_crosstab_data.csv") {
      self$merge_long_tab_data()
      self$long_tab_data |>
        write.csv(file_name, row.names = FALSE)
      invisible(self)
    },
    # TODO: ask Wolf:
    # add more information to the print output, e.g.:
    #  - the row number in the Question sheet
    #  - the qtab type
    #  - the index of the qtab type
    #  - the parameters of the qtab object,
    #  - ... (?)
    print = function(...) {
      self$qrows |> lapply(\(x) x$qtabs$obj) |> print()
      invisible(self)
    }
  )
)
parse_qsheet <- function(mapping, row) {
  mapping$qsheet$qsheet_raw <- read_qsheet_raw(mapping$mapping_file, row)
  row <- set_row(mapping, row)
  mapping$qsheet$qrows_params <- preprocess_qrows_params(mapping)
  calc_row_lgl <- purrr::map_int(mapping$qsheet$qrows_params, "row") %in% row

  mapping$qrows <- lapply(
    mapping$qsheet$qrows_params[calc_row_lgl],
    \(p) Qrow$new(p, mapping)
  )
}

read_data <- function(dat) {
  UseMethod("read_data")
}
read_data.data.frame <- function(dat) {
  dat
}
read_data.character <- function(dat) {
  haven::read_sav(dat)
}
