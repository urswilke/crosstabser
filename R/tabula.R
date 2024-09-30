# TODO: check where to add helper functions (or other OOP structure or something else (?))
# for common tasks in different methods,
# e.g. stuff where mcg and mdg do the same, etc.

#' Tabulation class
#'
#' @description The class \code{Tabula} can be used to calculate the crosstabs
#'   specified on the Questions sheet of the Excel mapping file.
#'
#' @field dat_mod modified dataframe
#' @field mapping_file filepath of the Excel mapping file
#' @field row Numeric vector with the row numbers in the Questions sheet, where crosstabs should be calculated.
#'   Or `NULL` (the default) resulting in the selection of all row numbers where `Type` is specified.
#' @field dat `dat` field of the super-class `datenanpassr::Mapping`.
#'   If this is specified, dat_mod will be ignored, and instead generated with `datenanpassr::Mapping$modify_data()`
#'
#' @export
#'
#' @examples
#' # This is copied from datenanpassr and will just serve as a template when the
#' # code will be documented...
#'
#'
#' # Create a Mapping object from the files provided by the package:
#' mapping_file <- system.file("extdata", "mapping.xlsx", package = "datenanpassr")
#' spss_file <- system.file("extdata", "mtcars_labelled.sav", package = "datenanpassr")
#' # DOESN'T WORK WITH DATENANPASSR MAPPING FILE!!!
#' \dontrun{
#' mapping <- Tabula$new(spss_file, mapping_file)
#'
#' # The spss_file path was read into a dataframe in the "dat" field of the
#' # mapping object:
#' mapping$dat
#'
#' # The Excel mapping file is translated to a `command_blocks()` object.
#' # It contains the processed information in a list structure that has
#' # its own print method.
#' # You can access it with
#' mapping$cmd_tbl$command_blocks
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
#' }
Tabula <- R6::R6Class(
  "Tabula",
  inherit = datenanpassr::Mapping,
  public = list(
    mapping_file = NULL,
    dat = NULL,
    dat_mod = NULL,
    options = NULL,
    long_tab_data = NULL,
    qsheet = list(),
    qtabs = list(),
    qrows = list(),
    crosstabs = list(),
    #' @description Initialize a Tabula object
    #'
    initialize = function(dat_mod = NULL,
                          mapping_file = NULL,
                          row = NULL,
                          dat = NULL,
                          book_no = NULL,
                          tabulate = TRUE,
                          # TODO: ask Wolf if we should set this this to interactive() ...:
                          verbose = FALSE,
                          ...) {
      super$initialize(
        dat,
        mapping_file,
        process_sheets = FALSE,
        error_out = "safe",
        # TODO: move the definition of this parameter into the initialization of datenanpassr::Mapping,
        # if we want to use it there as well...!
        verbose = verbose,
        ...
      )
      if (is.null(dat) & is.null(dat_mod)) {
        stop("You have to specify at least one of `dat` or `dat_mod`")
      }
      if (!is.null(dat)) {
        super$modify_data()
      } else
      if (!is.null(dat_mod)) {
        self$dat_mod <- datenanpassr::read_data(dat_mod)
      }
      set_options(self, book_no, ...)

      if (tabulate) {
        self$calc_qtabs(row)
      }
    },
    calc_qtabs = function(row = NULL) {
      gen_col_tables(self)
      parse_qsheet(self, row)
      invisible(self)
    },
    aggregate_5_tables = function() {
      aggregate_5_tables(self)
      add_columns_for_tablebook(self)
      invisible(self)
    },
    write_to_db = function() {
      self$aggregate_5_tables()
      write_to_db(self)
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
  mapping$qsheet$qsheet_raw <- read_qsheet_raw(mapping, row)
  row <- set_row(mapping, row)
  mapping$qsheet$qrows_params <- preprocess_qrows_params(mapping)
  calc_row_lgl <- purrr::map_int(mapping$qsheet$qrows_params, "row") %in% row

  mapping$qrows <- lapply(
    mapping$qsheet$qrows_params[calc_row_lgl],
    \(p) Qrow$new(p, mapping)
  )
}
