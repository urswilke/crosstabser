setOptions <- function(mapping_file) {
  # TODO: check if faster bulk-wise or preloading a workbook object with
  # openxlsx, but rather together with Mapping class in datenanpassr...:
  v_scenario <- openxlsx::read.xlsx(
    mapping_file,
    namedRegion = "V_Scenario",
    colNames = FALSE
  )[[1]]
  V_Language <- openxlsx::read.xlsx(
    mapping_file,
    namedRegion = "V_Language",
    colNames = FALSE
  )[[1]]
  V_XMLName <- openxlsx::read.xlsx(
    mapping_file,
    namedRegion = "V_XMLName",
    colNames = FALSE
  )[[1]]
  df_macro_raw <- readxl::read_excel(
    mapping_file,
    sheet = "Macro",
    col_types = "text",
    col_names = FALSE
  ) |>
    suppressMessages()

  names(df_macro_raw) <- paste0("X", seq_len(ncol(df_macro_raw)))

  l_macro_scenario <- extract_scenario_options(df_macro_raw, v_scenario)

  df_lexikon_raw <- read.delim(
    # TODO: derive path to Lexikon in Funktionen.xlsm from mapping file:
    system.file("extdata", "lexikon.csv", package = "crosstabser"),
    header = FALSE,
    sep = ";"
  )
  l_lexikon <- df_lexikon_raw[-1, c(1, V_Language + 1)] |> tibble::deframe()

  colvar <- df_macro_raw[5, 4 + v_scenario][[1]] |> stringr::str_split_1("[ ,;]+")

  # TODO: find cleaner way to do this!...:
  mapping_r_params <- datenanpassr:::extract_named_region_params.excel(mapping_file)

  return(tibble::lst(
    v_scenario,
    V_Language,
    V_XMLName,
    df_macro_raw,
    l_macro_scenario,
    l_lexikon,
    colvar,
    mapping_r_params
  ))
}


extract_scenario_options <- function(df_macro_raw, v_scenario) {
  param_list <- df_macro_raw[c(1, 4 + v_scenario)] |> tibble::deframe()

  res <- list()
  res$ColVar <- param_list[["ColVar"]] |> stringr::str_split_1("[ \t]+")
  res$Unguelt <- param_list[c("Miss1", "Miss2", "Miss3")] |> as.numeric()
  res$Weight <- param_list[["Weight"]]
  res$Filter <- param_list[["Filter"]] |>
    # hopefully, won't be needed one day:
    spss_to_r()
  res$scenario_name <- param_list[[4]]
  res


}

add_global_options <- function(params, global_options) {
  res <- params
  res$Filter <- append(res$Filter, global_options$Filter[!is.na(global_options$Filter)])
  res$Weight <- dplyr::coalesce(res$Weight, global_options$Weight)
  if (is.na(res$Weight)) {
    res$Weight <- list(NULL)
  }
  if (length(res$Unguelt) == 0) {
    res$Unguelt <- global_options$Unguelt
  }
  res$ColVar <- global_options$ColVar
  res
}

