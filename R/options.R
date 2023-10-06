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

add_global_options <- function(params, mapping) {
  global_options <- mapping$options$l_macro_scenario
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


add_type_specific_params <- function(qtab) {
  UseMethod("add_type_specific_params")
}

add_type_specific_params.default <- function(qtab) {
  qtab$p$raw_data_rowvars <- paste0(
    "rowvar_",
    # for multi selvar:
    qtab$p$multi_selvar_rowvars_qtab
  )
  qtab$p$rowvars_string <- paste(qtab$p$multi_selvar_rowvars_qtab, collapse = ", ")
  qtab$p$raw_data_colvars <- paste0("colvar_", c(qtab$p$ColVar, "DC#STICHPROBE"))
  if (is.null(qtab$p$Weight[[1]])) {
    qtab$p$long_weight <- character()
  } else {
    qtab$p$long_weight <- "weight"
  }

  if (!is.null(qtab$p$Sort)) {
    sort_list <- stringr::str_extract_all(
      qtab$p$Sort,
      "\\w+ *= *\\w+"
    )[[1]] |>
      stringr::str_split(" *= *")
    # TODO: find a cleaner way to treat this and discuss with Wolf which sorting
    # options should be implemented:
    order_d <- any(sort_list |> purrr::map_lgl(\(x) all(x == c("ORDER", "D"))))
    key_count <- any(sort_list |> purrr::map_lgl(\(x) all(x == c("KEY", "COUNT"))))
    qtab$p$sort_params <- tibble::lst(
      order_d,
      key_count
    )
  }
}
add_type_specific_params.qtab_type_mdg <- function(qtab) {
  qtab$p$MdgVal <- qtab$p$MdgVal %||% "1"
  qtab$p$rowvars_valid_qtab <- qtab$p$RowVar
  # TODO: find cleaner way...!
  qtab$p$rowvars_qtab <- qtab$p$RowVar

  # this has to be done before adding the Unguelt variables
  # in order to make row_table_body.qtab_type_mdg() only pick the valid variables
  # for multi selvar mdg tables
  qtab$p$multi_selvar_rowvars_qtab <- concat_multi_selvar_rowvars(qtab)
  if (is.character(qtab$p$Unguelt)) {
    qtab$p$rowvars_qtab <- c(qtab$p$rowvars_qtab, qtab$p$Unguelt)
  }
  # HACK to remove the numeric values that were wrongly added from the Macro sheet:
  if (is.numeric(qtab$p$Unguelt)) {
    qtab$p$Unguelt <- NULL
  }
  NextMethod()
}
concat_multi_selvar_rowvars <- function(qtab) {
  if (is.null(qtab$p$SelVar)) {
    return(qtab$p$rowvars_qtab)
  }
  selvar_rowvars <- qtab$p$df_selvar$rowvar
  selvar_rowvars[[1]] |>
    seq_along() |>
    lapply(
      \(i) selvar_rowvars |>
        lapply(\(x) x[i]) |>
        unlist() |>
        paste(collapse = "/")
    ) |>
    unlist()
}

add_type_specific_params.qtab_type_mw <- function(qtab) {
  stat_fun <- qtab$p$ZsfgMW
  qtab$p$rowvars_qtab <- qtab$p$RowVar
  qtab$p$multi_selvar_rowvars_qtab <- concat_multi_selvar_rowvars(qtab)
  if (length(stat_fun) == 0) {
    stat_fun = "mean"
  }

  qtab$p$stat_fun <- stat_fun
  NextMethod()
}
add_type_specific_params.qtab_type_cat <- function(qtab) {
  qtab$p$rowvars_qtab <- unlist(qtab$p$df_selvar$rowvar) %||% qtab$p$RowVar
  # for multiple selvar:
  qtab$p$multi_selvar_rowvars_qtab <- qtab$p$rowvars_qtab |>
    paste(collapse = "/")
  if (!is.null(qtab$p$MetrMac)) {
    qtab$p$df_stat_funs <- process_metr_mac(qtab)
  }
  NextMethod()
}
add_type_specific_params.qtab_type_mcg <- function(qtab) {
  qtab$p$rowvars_qtab <- qtab$p$RowVar
  qtab$p$multi_selvar_rowvars_qtab <- concat_multi_selvar_rowvars(qtab)
  NextMethod()
}

df_metr_mac <- data.frame(
  shortcut = c("E", "M", "S", "P", "I", "A", "T"),
  fun = c("se", "median", "mean", "percentile", "min", "max", "sum"),
  # TODO: Wolf fragen wo das steht???
  ctab_entry = c("cTabStdErr", "cTabMedian", "cTabMean", "wo_steht???", "wo_steht2???", "wo_steht3???", "wo_steht4???")
)

process_metr_mac <- function(qtab) {
  l <- stringr::str_extract_all(qtab$p$MetrMac, "[A-Z]\\d+")[[1]] |>
    stringr::str_split("(?=\\d)")

  df_stat_funs <- data.frame(shortcut = l |> purrr::map_chr(1)) |>
    dplyr::mutate(
      fun = df_metr_mac$fun[match(shortcut, df_metr_mac$shortcut)],
      decimals = as.numeric(l |> purrr::map_chr(\(x) x[length(x)])),
      row_title = qtab$m$options$l_lexikon[
        df_metr_mac$ctab_entry[match(shortcut, df_metr_mac$shortcut)]
      ] |> unname()
    )
  df_stat_funs$quantile_val <- vector("list", length(l))
  df_stat_funs[df_stat_funs$shortcut == "P",]$quantile_val <-
    as.numeric(
      l[df_stat_funs$shortcut == "P"] |>
        purrr::map_chr(
          \(x) x[2:3] |>
            paste(collapse = ""))
    ) / 100
  df_stat_funs
}

