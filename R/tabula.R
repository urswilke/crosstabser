# from here: https://stackoverflow.com/a/64105860
#' @import R6
NULL

doc_row <- "Numeric vector with the row numbers in the Questions sheet,
where crosstabs should be calculated, when calling `Tabula$calc_qtabs()`.
Or `NULL` (the default) resulting in the selection of all row numbers
where `Type` is specified."
doc_dat <- "`dat` input data field of the super-class `datadaptor::Mapping`.
If this is specified, `dat_mod` will be ignored,
and instead generated with `datadaptor::Mapping$modify_data()`"
doc_dat_mod <- "`dat_mod` modified data field of the super-class `datadaptor::Mapping`."
doc_mapping_file <- "`mapping_file` file path field of the super-class `datadaptor::Mapping`."

# TODO: somehow hide fields with "Will be deprecated"
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
#' @field ditw This is the "dust in the wind" list object field
#'   that stores data that didn't make it into their own field.
#'   For developers only!
#'   For reproducible code you should NEVER rely on this field
#'   as it might be subject to change without any warning.
#'   This overwrites the `datadaptor::Mapping$ditw` field;
#'   the list field additionally contains the `ct` element.
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   q1 = c(1, 2, 1) |> haven::labelled(c(Yes = 1, No = 2), label = "Super important question"),
#'   age = c(2, 1, 1) |> haven::labelled(c("18-39" = 1, "40+" = 2), label = "age")
#' )
#' mapping_file = list(
#'   Questions = data.frame(
#'     Type  = "cat",
#'     RowVar = "q1",
#'     Title = "The crosstab's title"
#'   ),
#'   Macro = list(ColVar = "age")
#' )
#' m <- Tabula$new(df, mapping_file)
#' m
#' # The previous line prints the "Tabula" object.
#' # Under the hood, a list of `Qrow` objects were generated.
#' # Printing `m` prints the list of `Qtab` elements of each `Qrow`:
#' m$qrows
#' # For instance, this prints the list of `Qtab` elements
#' # of the first `Qrow` element:
#' m$qrows[[1]]$qtabs |> print()
Tabula <- R6::R6Class(
  "Tabula",
  inherit = datadaptor::Mapping,
  public = list(
    mapping_file = NULL,
    dat = NULL,
    dat_mod = NULL,
    qrows = list(),
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
      args <- excel_params |> utils::modifyList(list(...))

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
    #' @description Write a table_charter app html file of the crosstab data
    #'
    #' @details This needs a valid html `template_file`, i.e. one of:
    #'
    #' * The file
    #' [example_dashboard.html](https://gitlab.com/urswilke/table_charter/-/blob/main/example_dashboard.html)
    #' which is directly scraped from the table_charter repo by default (no installation of table_charter needed).
    #' * For [deploying it in the web](https://gitlab.com/urswilke/table_charter#deploying-it-in-the-web)
    #' or [running it on a dev server](https://gitlab.com/urswilke/table_charter#dev-server),
    #' you need to [install table_charter](https://gitlab.com/urswilke/table_charter#installation) first,
    #' and then use the file [index.html](https://gitlab.com/urswilke/table_charter/-/blob/main/index.html)
    #' on your machine.
    #' * After installing, you can also generate a standalone html file
    #' (without the need to download javascript libraries) by running:
    #' \preformatted{
    #'   npm run standalone-build
    #' }
    #' and then using the template file created in the `dist/` sub-directory.
    #'
    #' @param template_file Path to the template file (see description).
    #' @param output_file File path to the table_charter app html file.
    #' @param project_data Either a `list()` object to modify the default:
    #'   `list(logo_base64 = "",
    #'     logo_url =
    #'       "https://gitlab.com/urswilke/table_charter/-/raw/main/img/logo_small.svg",
    #'     title = "Dashboard",
    #'     date = Sys.Date())`, or `NULL` (the default).
    #'    If `NULL`, nothing is done.
    #'    The fields will modify the elements in the header of the dashboard.
    save_html_app = function(
      template_file = "https://gitlab.com/urswilke/table_charter/-/raw/main/example_dashboard.html",
      output_file = "dashboard.html",
      project_data = NULL
    ) {
      data_string <- self$get_crosstabs_data() |> gen_data_json()

      html <- template_file |> xml2::read_html()

      load_data_node <- html |>
        xml2::xml_find_all("//script[@id='load-example-data']")
      xml2::xml_remove(load_data_node)
      tc_node <- html |>
        xml2::xml_find_all(".//table-charter-intro|.//table-charter")
      xml2::xml_attr(tc_node, "data") <- data_string

      if (!is.null(project_data)) {
        project_data_node <- html |>
          xml2::xml_find_first(".//script[@id='project-data']")
        project_data_mod <- list(
          logo_base64 = "",
          logo_url =
            "https://gitlab.com/urswilke/table_charter/-/raw/main/img/logo_small.svg",
          title = "Dashboard",
          date = Sys.Date()
        ) |>
          utils::modifyList(project_data)
        xml2::xml_text(project_data_node) <- paste0(
          "\nconst project_data = ",
          jsonlite::toJSON(
            project_data_mod,
            auto_unbox = TRUE,
            pretty = TRUE
          ),
          ";\n"
        )
      }

      html |> xml2::write_html(output_file)

      invisible(self)
    },
    #' @description Return the crosstabs data of the `Tabula` object
    #'
    #'   This method returns a list of dataframes
    #'   containing all the crosstabs information.
    #'   Thus it's not chainable.
    #' @return A list of dataframes with the data of the crosstabs;
    #'   see `vignette("data-format")`.
    get_crosstabs_data = function() {
      private$prepare_5_tables()
      return(self$ditw$ct$crosstabs$data)
    },
    #' @description Print the crosstabs of the `Tabula` object
    #'
    #'   This method is called under the hood, if you `print()` a `Tabula` object.
    #'   This will call the print method of all `Qrow` elements in the `Tabula$qrows` field.
    #' @param ... Not used for now.
    print = function(...) {
      self$qrows |> lapply(\(x) x$qtabs) |> print(...)
      invisible(self)
    }
  ),
  private = list(
    new_Qrow = Qrow,
    read_qsheet = function(row) {
      qsheet_raw <- read_qsheet_raw(self, row)

      # append "_row_<row value>" to Abbreviation column if duplicated or NA:
      is_duplicated <- vctrs::vec_duplicate_detect(qsheet_raw$Abbreviation) |
        is.na(qsheet_raw$Abbreviation)
      qsheet_raw$Abbreviation[is_duplicated] <- paste0(
        qsheet_raw$Abbreviation[is_duplicated] |> dplyr::coalesce(""),
        "_row_",
        qsheet_raw$row[is_duplicated]
      )

      self$ditw$ct$qsheet$qsheet_raw <- qsheet_raw
    },
    process_qsheet = function(row) {
      private$read_qsheet(row)
      qsheet_raw <- self$ditw$ct$qsheet$qsheet_raw
      self$qrows <- lapply(
        split(qsheet_raw, qsheet_raw$row),
        \(df) private$new_Qrow$new(df, self)
      )
    },
    prepare_5_tables = function() {
      l <- self$qrows |>
        lapply(\(x) x$.__enclos_env__$private$prep_tab_row_val()) |>
        lapply(\(x) x$ditw$ct$crosstabs$data)
      self$ditw$ct$crosstabs$data$tab_table <- l |> lapply(\(x) x$tab_table) |> dplyr::bind_rows()
      self$ditw$ct$crosstabs$data$val_table <- l |> lapply(\(x) x$val_table) |> dplyr::bind_rows()
      self$ditw$ct$crosstabs$data$row_table <- l |> lapply(\(x) x$row_table) |> dplyr::bind_rows()
      private$prepare_head_col_tables()
      invisible(self)
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

      self$ditw$ct$dat_tab <- self$dat_mod[private$glob_filter,]
    },
    prepare_head_col_tables = function() {
      prepare_head_col_tables_(self)
    }
  )
)

#' Generate data json string
#'
#' This helper function generates a json string containing
#' the data produced by `Tabula$get_crosstabs_data()`,
#' and which then can be put as the `"data"` attribute
#' of the `<table-charter>` app. It is also used by `Tabula$save_html_app()`
#'
#' @param l list object generated by `Tabula$get_crosstabs_data()`.
#'
#' @returns A character string
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   q1 = c(1, 2, 1) |> haven::labelled(c(Yes = 1, No = 2), label = "Super important question"),
#'   age = c(2, 1, 1) |> haven::labelled(c("18-39" = 1, "40+" = 2), label = "age")
#' )
#' mapping_file = list(
#'   Questions = data.frame(
#'     Type  = "cat",
#'     RowVar = "q1",
#'     Title = "The crosstab's title"
#'   ),
#'   Macro = list(ColVar = "age")
#' )
#' m <- Tabula$new(df, mapping_file)
#' m$get_crosstabs_data() |> gen_data_json()
gen_data_json <- function(l) {
  l <- l |> purrr::set_names(c("Tab", "Val", "Row", "Head", "Col"))
  list(type = "table-object", data = l) |>
    jsonlite::toJSON(
      dataframe = "columns",
      na = "null",
      null = "null",
      auto_unbox = TRUE
    ) |>
    as.character()
}
