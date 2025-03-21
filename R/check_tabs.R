check_tab <- function(qtab) {
  if (is.null(qtab$p$Checks)) {
    return(NULL)
  }
  if (qtab$p$Type == "mw" && qtab$p$Checks == "100percent") {
    check_100percent(qtab)
  }
}

check_100percent <- function(qtab) {
  df <- qtab$m$ditw$ct$dat_tab[qtab$p$RowVar]
  filtered_row_idx <- df[[1]] %in% qtab$p[["Unguelt"]]
  var_sums <- df[!filtered_row_idx,] |>
    rowSums(na.rm = TRUE)
  if (
    length(var_sums) == 0 ||
    var_sums |>
    unique() |>
    dplyr::setequal(100)
  ) {
    qtab$d$tab_table$TabDetails <- "100percent"
  } else {
    n_unequal_100 <- sum(var_sums != 100)
    # TODO: Specify the ID variable name in the mapping instead of "DC_ID"
    # (cf. R_id_var in datadaptor)...:
    ids_unequal_100 <- qtab$m$ditw$ct$dat_tab$DC_ID[!filtered_row_idx][var_sums != 100]

    warn_part1 <- paste0(
      "In table ",
      qtab$p$Abbreviation,
      " (in row ",
      qtab$p$row,
      "):"
    )
    warn_part2 <- cli::pluralize("The sum of the variables is not equal to 100 in {n_unequal_100} case{?s}.")
    if (n_unequal_100 > 10) {
      warn_part3 <- character()
    } else {
      warn_part3 <- cli::pluralize("(ID = {ids_unequal_100}).")
    }
    cli::cli_warn(c(
      warn_part1,
      warn_part2,
      warn_part3,
      ""
    ))
  }

}
