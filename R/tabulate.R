gen_col_tables <- function(mapping) {
  mapping$qsheet$head_table <- gen_head_table(mapping)
  mapping$qsheet$col_table <- gen_col_table(mapping)
}

# to be removed:
get_raw_data <- function(qtab) {
  UseMethod("get_raw_data")
}
get_raw_data.default <- function(qtab) {
  rowvars <- qtab$p$RowVar
  colvars <- qtab$p$ColVar
  weightvar <- qtab$p$Weight[[1]]
  if (!is.null(weightvar)) {
    weightvar <- weightvar |> purrr::set_names("weight")
  }

  rowvars_named <- rowvars |> purrr::set_names(paste0("rowvar_", rowvars))
  long_cols <- c(
    rowvars_named,
    colvars |> purrr::set_names(paste0("colvar_", colvars)),
    weightvar
  )
  prep_data <- function() {
    # same as:
    # mapping$dat_mod |>
    #   dplyr::filter(!!!rlang::parse_exprs(df_row$Filter[[1]])) |>
    #   dplyr::select(!!!long_cols) |>
    #   dplyr::mutate(across(everything(), strip_attributes))
    # ... but with base R (for better performance)
    if (length(qtab$p$Filter) == 0) {
      row_lgl <- TRUE
    } else {
      filter_exprs <- rlang::parse_exprs(qtab$p$Filter)
      row_lgls <- filter_exprs |> purrr::map(\(e) rlang::eval_tidy(e, qtab$d$dat_mod))
      row_lgl <- all_true(row_lgls)
    }
    if (is.null(qtab$p$SelVar)) {
      dat <- qtab$d$dat_mod[row_lgl, long_cols]
      names(dat) <- names(long_cols)
      # remove label information:
      for (col in seq_len(ncol(dat))) {
        attributes(dat[[col]]) <- NULL
      }
    } else {
      # TODO: clean up this mess!...:
      dfsel <- qtab$p$df_multi_selvar
      dm <- qtab$m$dat_mod

      dat <- seq_len(nrow(dfsel)) |> lapply(\(i) {
        dfsel_i <- qtab$p$df_multi_selvar[i,]
        long_cols <- c(
          dfsel_i$rowvar[[1]] |> purrr::set_names(qtab$p$long_rowvars),
          colvars |> purrr::set_names(paste0("colvar_", colvars)),
          weightvar
        )
        dat <- dm[
          row_lgl & selvar_eq_selval(dm[[dfsel_i$selvar]], qtab$p$SelVal),
          long_cols
        ]
        names(dat) <- names(long_cols)
        # remove label information:
        for (col in seq_len(ncol(dat))) {
          attributes(dat[[col]]) <- NULL
        }
        dat
      }) |>
        dplyr::bind_rows()
    }
    dat
  }
  res <- prep_data()
  # TODO: move this somewhere where it only concerns mdg!...:
  if (qtab$p$Type == "mdg") {
    if (is.na(suppressWarnings(as.numeric(qtab$p$MdgVal)))) {
      res <- as.data.frame(res)
      res[names(rowvars_named)] <- catrec(
        res[names(rowvars_named)] |>
          unlist(use.names = FALSE),
        paste0("(", qtab$p$MdgVal, " = 1)")
      )
      qtab$p$MdgVal = 1
    } else {
      qtab$p$MdgVal = as.numeric(qtab$p$MdgVal)
    }
  }

  res
}
get_raw_data2 <- function(qtab) {
  UseMethod("get_raw_data2")
}
get_raw_data2.qtab_type_mdg <- function(qtab) {
  res <- get_raw_data2.default(qtab)
  if (is.na(suppressWarnings(as.numeric(qtab$p$MdgVal)))) {
    rowvars <- qtab$p$rowvars_mdg
    rowvars_named <- rowvars |> purrr::set_names(paste0("rowvar_", rowvars))
    res <- as.data.frame(res)
    res[names(rowvars_named)] <- catrec(
      res[names(rowvars_named)] |>
        unlist(use.names = FALSE),
      paste0("(", qtab$p$MdgVal, " = 1)")
    )
    qtab$p$MdgVal = 1
  } else {
    qtab$p$MdgVal = as.numeric(qtab$p$MdgVal)
  }
  res
}
get_raw_data2.default <- function(qtab) {
  # TODO: better generate this derived parameter `rowvars_mdg` for all qtab types,
  # not just mdg... (?):
  rowvars <- qtab$p$rowvars_mdg %||% qtab$p$RowVar
  rowvars_named <- rowvars |> purrr::set_names(paste0("rowvar_", rowvars))
  colvars <- qtab$p$ColVar
  colvars_named <- colvars |> purrr::set_names(paste0("colvar_", colvars))
  weightvar <- qtab$p$Weight[[1]]
  if (!is.null(weightvar)) {
    weightvar <- weightvar |> purrr::set_names("weight")
  }

  long_cols <- c(
    rowvars_named,
    colvars_named,
    weightvar
  )

  row_filter_lgl <- get_row_filter_lgl(qtab)
  # same as:
  # mapping$dat_mod |>
  #   dplyr::filter(!!!rlang::parse_exprs(df_row$Filter[[1]])) |>
  #   dplyr::select(!!!long_cols) |>
  #   dplyr::mutate(across(everything(), strip_attributes))
  # ... but with base R (for better performance)
  if (is.null(qtab$p$SelVar)) {
    dat <- qtab$d$dat_mod[row_filter_lgl, long_cols]
    names(dat) <- names(long_cols)
    # remove label information:
    for (col in seq_len(ncol(dat))) {
      attributes(dat[[col]]) <- NULL
    }
  } else {
    # TODO: ask Wolf how to deal with Unguelt mdg vars together with multiple selvars...!
    # TODO: clean up this mess!...:
    dfsel <- qtab$p$df_multi_selvar
    dm <- qtab$m$dat_mod

    dat <- seq_len(nrow(dfsel)) |> lapply(\(i) {
      dfsel_i <- qtab$p$df_multi_selvar[i,]
      long_cols <- c(
        dfsel_i$rowvar[[1]] |> purrr::set_names(qtab$p$long_rowvars),
        colvars |> purrr::set_names(paste0("colvar_", colvars)),
        weightvar
      )
      dat <- dm[
        row_filter_lgl & selvar_eq_selval(dm[[dfsel_i$selvar]], qtab$p$SelVal),
        long_cols
      ]
      names(dat) <- names(long_cols)
      # remove label information:
      for (col in seq_len(ncol(dat))) {
        attributes(dat[[col]]) <- NULL
      }
      dat
    }) |>
      dplyr::bind_rows()
  }
  dat

}
get_row_filter_lgl <- function(qtab) {
  if (length(qtab$p$Filter) == 0) {
    return(TRUE)
  }
  filter_exprs <- rlang::parse_exprs(qtab$p$Filter)
  row_lgls <- filter_exprs |> purrr::map(\(e) rlang::eval_tidy(e, qtab$d$dat_mod))
  all_true(row_lgls)
}
selvar_eq_selval <- function(selvar, selval) {
  if (!is.na(as.numeric(selval) |> suppressWarnings())) {
    return(selvar == as.numeric(selval))
  }
  selval_interval <- selval |> stringr::str_remove(":.*") |> stringr::str_split_1("-") |> as.numeric()
  selvar >=  selval_interval[1] & selvar <= selval_interval[2]
}

pivot_table_data <- function(qtab) {
  UseMethod("pivot_table_data")
}
pivot_table_data.qtab_type_mw <- pivot_table_data.qtab_type_cat <- function(qtab) {
  # for TOTAL column:
  df <- qtab$d$raw_data
  df$"colvar_DC#STICHPROBE" <- 1

  qtab$d$long_data <- df |>
    pivot_rows() |>
    pivot_cols()
}
pivot_cols <- function(df) {
  df |>
    tidyr::pivot_longer(
      dplyr::matches("^colvar_"),
      names_pattern = "colvar_(.*)",
      names_to = "colvar",
      values_to = "colval"
    )
}
pivot_rows <- function(df) {
  df |>
    tidyr::pivot_longer(
      dplyr::matches("^rowvar_"),
      names_pattern = "rowvar_(.*)",
      names_to = "rowvar",
      values_to = "rowval"
    )
}
pivot_table_data.qtab_type_mdg <- function(qtab) {
  df_long <- pivot_table_data.qtab_type_cat(qtab)
  mdg_val <- qtab$p$MdgVal
  qtab$d$long_data <- df_long[df_long$rowval == mdg_val,]
}

pivot_table_data.qtab_type_mcg <- function(qtab) {
  # for TOTAL column:
  df <- qtab$d$raw_data
  df$"colvar_DC#STICHPROBE" <- 1

  df$i <- seq_len(nrow(df))

  df_rows_long <- df |>
    pivot_rows() |>
    # remove duplicated choices:
    dplyr::distinct(dplyr::across(dplyr::all_of(
      c("i", "rowval")
    )), .keep_all = TRUE)

  # TODO: add test to check if this works correctly!...:
  invalids_to_filter <- intersect(
    qtab$m$options$mapping_r_params$miss_rec_val,
    qtab$p$Unguelt
  )
  df_rows_long_invalids <- df_rows_long[df_rows_long$rowval %in% qtab$p$Unguelt,] |>
    dplyr::mutate(
      temp = order(factor(rowval, levels = qtab$p$Unguelt)),
      # calculate boolean that's TRUE if:
      val_to_count =
        # for each case, only take count one invalid value (the one that occurs
        # first in the list of invalid values):
        temp == 1 &
        # values not equal to the value defined in the cell of the named region
        # "R_miss_rec_val" in the mapping file:
        !rowval %in% invalids_to_filter,
      temp = NULL,
    .by = "i")

  df_rows_long_valids <- df_rows_long[!df_rows_long$rowval %in% qtab$p$Unguelt,]
  exclusives <- qtab$p$Exclusive
  if (!is.null(exclusives)) {
    df_rows_long_valids <- df_rows_long_valids |>
      dplyr::mutate(
        none_exclusive = !any(rowval %in% exclusives),
        temp = order(factor(rowval, levels = exclusives)),
        # order doesn't deal correctly with levels not occuring in the factors.
        # Therefore we set these values to Inf to not select values not in the
        # set of exclusives here...
        # TODO: find a cleaner way for Exclusive!...:
        temp = ifelse(!rowval %in% exclusives, Inf, temp),
        first_exclusive = temp %in% min(temp, na.rm = TRUE),
        val_to_count = first_exclusive | none_exclusive,
        temp = NULL,
        first_exclusive = NULL,
        none_exclusive = NULL,
        # same result but slower:
        # val_to_count = dplyr::case_when(
        #   !any(rowval %in% exclusives) ~ TRUE,
        #   !rowval %in% exclusives ~ FALSE,
        #   .default = {
        #     temp <- order(factor(rowval, levels = exclusives))
        #     temp[!rowval %in% exclusives] <- Inf
        #     temp == min(temp)
        #   }
        # ),
        .by = "i")
  } else {
    df_rows_long_valids$val_to_count <- TRUE
  }


  df_long <- dplyr::bind_rows(
    # TODO: remove rows of cases with multiple values in column "Ëxclusive"...:
    df_rows_long_valids,
    df_rows_long_invalids
  ) |>
    pivot_cols()

  rowvars <- qtab$p$RowVar |> paste(collapse = ", ")
  df_long$rowvar <- rowvars
  qtab$d$long_data <- df_long
}


gen_val_table <- function(qtab) {
  row_table <- qtab$d$row_table

  col_table <- qtab$d$col_table

  tab_values <- qtab$d$tab_values
  res <- tab_values |>
    merge(row_table |> dplyr::rename(rowval = RowValue, rowvar = RowVariable)) |>
    merge(col_table |> dplyr::rename(colval = ColValue, colvar = ColVariable)) |>
    dplyr::as_tibble()
  res[order(res$RowNo, res$ColNo), c("RowNo", "ColNo", "value")]
}
