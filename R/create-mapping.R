# TODO: mapping_type is not correctly documented yet (also in datadaptor)

#' Create an Excel mapping file based on a labelled dataframe
#'
#' This function first calls `datadaptor::create_mapping_workbook()`.
#' Additionally to the sheets "Variables", "Label", "Verbatims" & "Free",
#' it will insert 2 more sheets "Macro" & "Questions".
#' The Excel workbook is then written to the file `mapping_file`.
#' Please refer to `vignette("questions")` & `vignette("questions-parameters")`
#' for details how to use the mapping.
#'
#' @param df_raw dataframe with labelled variables, e.g. resulting from
#'   haven::read_sav
#' @param mapping_file name of the Excel file to be created
#' @param mapping_type String specifying the mapping type.
#'   Either "excel" or "list". Defaults to "excel".
#'
#' @export
#'
#' @examples
#' spss_file <- system.file(
#'   "extdata",
#'   "fruit_survey.sav",
#'   package = "datadaptor"
#' )
#' df <- haven::read_sav(spss_file)
#' # The next command creates an empty mapping file `mapping.xlsx`:
#' \dontrun{
#' create_tabula(df, "mapping.xlsx")
#' }
create_tabula <- function(df_raw, mapping_file, mapping_type = "excel") {
  create_tabula_xlsx(df_raw, mapping_file)
}
create_tabula_xlsx <- function(df_raw, mapping_file) {
  wb <- datadaptor::create_mapping_workbook(df_raw)

  wb$add_worksheet("Macro")
  wb$add_worksheet("Questions")
  wb$add_worksheet("Config")
  wb$add_data(
    sheet = "Config",
    x = tibble::tribble(
      ~name,        ~value,
      # TODO: add more args, also add Config sheet in datadaptor
      "V_Scenario", 1,
      "V_Language", 4,
      "V_BookNo",   999999999L,
    ),
    col_names = FALSE
  )
  wb$add_named_region(sheet = "Config", name = "V_Scenario", dims = "B1")
  wb$add_named_region(sheet = "Config", name = "V_Language", dims = "B2")
  wb$add_named_region(sheet = "Config", name = "V_BookNo", dims = "B3")

  wb$add_data(
    sheet = "Macro",
    x = create_macro_sheet(),
    start_row = 3,
    col_names = FALSE
  )
  wb$add_data(sheet = "Questions", x = empty_qsheet())


  wb$save(mapping_file)
  message("Excel mapping file written to '", mapping_file, "'")
}

create_macro_sheet <- function() {
  params <- c("Scenario", "ScenName", "ColVar", "Invalid", "Filter", "Unwgt", "Weight", "WeightLab", "gesplab", "Gesamtstichprobe")
  N <- 10
  res <- tibble::as_tibble(matrix(rep("", length(params) * (N + 4)), ncol = N + 4, dimnames = list(NULL, paste0("col", seq_len(N + 4)))))
  res[1, 5:(N + 4)] <- t(seq_len(N) |> as.character())
  res[4, 5:(N + 4)] <- "-1,-3,-2"
  res[10, 5:(N + 4)] <- "1=1"
  res[1] <- params
  res
}
