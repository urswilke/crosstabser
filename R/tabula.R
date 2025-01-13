doc_row <- "Numeric vector with the row numbers in the Questions sheet,
where crosstabs should be calculated, when calling `Tabula$calc_qtabs()`.
Or `NULL` (the default) resulting in the selection of all row numbers
where `Type` is specified."
doc_dat <- "`dat` input data field of the super-class `datadaptor::Mapping`.
If this is specified, dat_mod will be ignored,
and instead generated with `datadaptor::Mapping$modify_data()`"
doc_dat_mod <- "`dat_mod` modified data field of the super-class `datadaptor::Mapping`."
doc_mapping_file <- "`mapping_file` file path field of the super-class `datadaptor::Mapping`."

# TODO: check where to add helper functions (or other OOP structure or something else (?))
# for common tasks in different methods,
# e.g. stuff where mcg and mdg do the same, etc.

#' Tabulation class
#'
#' @description The class \code{Tabula} can be used to calculate the crosstabs
#'   specified on the Questions sheet of the Excel mapping file.
#'
#' @param row `r doc_row`
#' @param dat `r doc_dat`
#' @param dat_mod `r doc_dat_mod`
#' @param mapping_file `r doc_mapping_file`
#'
#' @field dat_mod `r doc_dat_mod`
#' @field mapping_file `r doc_mapping_file`
#' @field dat `r doc_dat`
#' @field qrows A `list()` of `Qrow` objects
#'
#' @export
#'
#' @examples
#' # This is copied from datadaptor and will just serve as a template when the
#' # code will be documented...
#'
#'
#' # Create a Mapping object from the files provided by the package:
#' mapping_file <- system.file("extdata", "mapping.xlsx", package = "datadaptor")
#' spss_file <- system.file("extdata", "mtcars_labelled.sav", package = "datadaptor")
#' # DOESN'T WORK WITH datadaptor MAPPING FILE!!!
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
  inherit = datadaptor::Mapping,
  public = list(
    mapping_file = NULL,
    dat = NULL,
    dat_mod = NULL,
    dat_tab = NULL,
    qsheet = list(),
    qrows = list(),
    crosstabs = list(),
    ditw = list(da = NULL, ct = NULL),
    #' @description Initialize a Tabula object
    #'
    #' @param tabulate Logical, whether to call the `Tabula$calc_qtabs()`
    #'   method when initializing (defaults to `TRUE`).
    #' @param ... Arguments passed to `Tabula$set_options()`
    initialize = function(dat_mod = NULL,
                          mapping_file = NULL,
                          row = NULL,
                          dat = NULL,
                          tabulate = TRUE,
                          ...) {
      super$initialize(
        dat,
        mapping_file,
        process_sheets = FALSE,
        ...
      )
      if (is.null(dat) & is.null(dat_mod)) {
        stop("You have to specify at least one of `dat` or `dat_mod`")
      }
      if (!is.null(dat)) {
        super$modify_data()
      } else
      if (!is.null(dat_mod)) {
        self$dat_mod <- self$read_data(dat_mod)
      }
      if (tabulate) {
        self$calc_qtabs(row)
      }
    },
    #' @description Set `Tabula` options.
    #'   This overwrites `datadaptor::Mapping$set_options()`
    #' @param ... Arguments passed to `get_tabula_options()`.
    set_options = function(...) {
      excel_params <- private$get_named_region_params()
      # If specified in both, excel parameters will be overwritten by the dots:
      args <- excel_params |> modifyList(list(...))

      da <- datadaptor::use_known_args(datadaptor::get_mapping_options, args)
      ct <- datadaptor::use_known_args(crosstabser::get_tabula_options, c(list(tabula = self), args))
      dev <- args |> setdiff(c(da, ct))

      self$opts <- tibble::lst(
        da,
        ct,
        dev
      )
    },
    #' @description Calculate the crosstabs
    calc_qtabs = function(row = NULL) {
      private$filter_global()
      gen_col_tables(self)
      private$process_qsheet(row)
      invisible(self)
    },
    #' @description Calculate the crosstabs
    prepare_5_tables = function() {
      l <- self$qrows |>
        lapply(\(x) x$prep_tab_row_val()) |>
        lapply(\(x) x$crosstabs$data)
      self$crosstabs$data$tab_table <- l |> lapply(\(x) x$tab_table) |> dplyr::bind_rows()
      self$crosstabs$data$val_table <- l |> lapply(\(x) x$val_table) |> dplyr::bind_rows()
      self$crosstabs$data$row_table <- l |> lapply(\(x) x$row_table) |> dplyr::bind_rows()
      private$prepare_head_col_tables()
      invisible(self)
    },
    #' @description Write a table_charter app html file of the crosstab data
    #'
    #' In order to generate the `template_file`,
    #' run the following comands in your console (needs git & npm installed):
    #' \preformatted{
    #'   git clone https://gitlab.com/urswilke/table_charter
    #'   cd table_charter
    #'   npm i
    #'   npm run standalone-build
    #' }
    #' The template file should then be in the `dist/` sub-directory.
    #'
    #' @param template_file Path to the template file (see description).
    #' @param output_file File path to the table_charter app html file.
    save_html_app = function(
      template_file,
      output_file = "dashboard.html"
    ) {
      self$prepare_5_tables()

      l <- self$crosstabs$data |> purrr::set_names(c("Tab", "Val", "Row", "Head", "Col"))
      l$Val <- l$Val |> tidyr::drop_na(Value)

      data_string <- list(type = "table-object", data = l) |>
        jsonlite::toJSON(
          dataframe = "columns",
          na = "null",
          null = "null",
          auto_unbox = TRUE
        )
      html_code <- readr::read_lines(template_file)
      html_code_with_data <- html_code |>
        stringr::str_replace(
          "<table-charter-intro></table-charter-intro>",
          paste0("<table-charter-intro data='", data_string, "'></table-charter-intro>")
        )
      write(html_code_with_data, file = output_file)

    },
    #' @description Print the crosstabs of the `Tabula` object
    #'
    #'   This method is called under the hood, if you `print()` a `Tabula` object.
    #'   This will call the print method of all `Qrow` elements in the `Tabula$qrows` field.
    print = function(...) {
      self$qrows |> lapply(\(x) x$qtabs) |> print()
      invisible(self)
    }
  ),
  private = list(
    new_Qrow = Qrow,
    read_qsheet = function(row) {
      qsheet_raw <- read_qsheet_raw(self, row)
      self$qsheet$qsheet_raw <- qsheet_raw
    },
    process_qsheet = function(row) {
      private$read_qsheet(row)
      qsheet_raw <- self$qsheet$qsheet_raw
      self$qrows <- lapply(
        split(qsheet_raw, qsheet_raw$row),
        \(df) private$new_Qrow$new(df, self)
      )
    },
    glob_filter = NULL,
    filter_global = function() {
      global_filter <- self$opts$ct$l_macro_scenario$Filter
      private$glob_filter <- if (is.na(global_filter)) TRUE else {
        global_filter |>
          # hopefully, won't be needed one day:
          spss_to_r() |>
          rlang::parse_expr() |>
          rlang::eval_tidy(self$dat_mod)
      }

      self$dat_tab <- self$dat_mod[private$glob_filter,]
    },
    prepare_head_col_tables = function() {
      prepare_head_col_tables_(self)
    }
  )
)
