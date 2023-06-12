setOptions <- function(mapping_file) {
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
  df_macro_raw <- readxl::read_excel(
    mapping_file,
    sheet = "Macro",
    col_types = "text",
    col_names = FALSE
  ) |>
    suppressMessages()

  names(df_macro_raw) <- paste0("X", seq_len(ncol(df_macro_raw)))
  colvar <- df_macro_raw[5, 4 + v_scenario][[1]] |> stringr::str_split_1("[ ,;]+")

  return(tibble::lst(
    v_scenario,
    V_Language,
    df_macro_raw,
    colvar
  ))
}
