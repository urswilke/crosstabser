#' Print object of class "Qtab"
#'
#' @param x `Qtab` object
#' @param ... Arguments passed to `print()`
#' @examples
#' # see `?Tabula`
#'
#' @export
print.Qtab = function(x, ...) {
  x_formatted <- format(x)
  if (is.null(x_formatted)) {
    cat("No data\n")
    return(NULL)
  }
  print(x_formatted, n = Inf, ...)
}

#' @export
format.Qtab <- function(x, ...) {
  x$.__enclos_env__$private$gen_wide_tab()

  res <- x$d$wide_tab[-1]
  if (is.null(res)) {
    return(NULL)
  }
  col_table <- x$d$col_table
  col_table$ColTitle1[col_table$ColTitle1 == dplyr::lag(col_table$ColTitle1)] <- ""
  col_table$ColTitle2[is.na(col_table$ColTitle2)] <- ""
  res <- purrr::pmap_dfc(
    list(
      res,
      col_table$ColTitle1,
      col_table$ColTitle2
    ),
    \(x, t1, t2) {
      vctrs::new_vctr(
        x,
        col_title1 = t1,
        col_title2 = t2,
        class = "crosstab_column"
      )
    }
  )

  attr(res, "d") <- x$d[c("tab_table", "head_table", "col_table", "row_table")]
  class(res) <- c("pillar_wide_tab", class(res))
  res
}

str_trunc_pad <- function(string, width = 5) {
  stringr::str_trunc(string, width, ellipsis = "\u2026") |>
    stringr::str_pad(width, "right")
}

#' @importFrom pillar ctl_new_rowid_pillar
#' @export
ctl_new_rowid_pillar.pillar_wide_tab <- function(controller, x, width, rowlab_wid1 = 19, rowlab_wid2 = 5, rowlab_wid3 = 5, ...) {
  out <- NextMethod()
  tables <- attr(controller, "d")
  row_table <- tables$row_table |> rm_header_footer()
  row_title1 <- row_table$RowTitle1
  row_title2 <- row_table$RowTitle2
  row_title3 <- row_table$RowTitle3
  row_title1[is.na(row_title1)] <- ""
  row_title2[is.na(row_title2)] <- ""
  row_title3[is.na(row_title3)] <- ""
  row_title2[row_title1 == row_title2] <- ""
  row_title3[row_title1 == row_title3] <- ""
  row_title1[row_title1 == dplyr::lag(row_title1)] <- ""
  width2 <- max(nchar(as.character(row_title2)))
  if (width2 > 0) {
    row_title2 <- row_title2 |> str_trunc_pad(rowlab_wid2)
  }
  row_title1 <- row_title1 |> str_trunc_pad(rowlab_wid1)
  row_title3 <- row_title3 |> str_trunc_pad(rowlab_wid3)
  rowid <- paste(row_title1, row_title2, row_title3)
  width <- max(nchar(as.character(rowid)))
  pillar::new_pillar(
    list(
      title = "",
      type = "",
      data = pillar::pillar_component(
        pillar::new_pillar_shaft(list(row_ids = rowid),
                                 width = width,
                                 class = "pillar_rif_shaft"
        )
      )
    ),
    width = width
  )
}

#' @importFrom pillar ctl_new_pillar
#' @export
ctl_new_pillar.pillar_wide_tab <- function(controller, x, width, ..., title = NULL) {
  tables <- attr(controller, "d")
  out <- NextMethod()

  col_title1 <- attr(x, "col_title1", exact = TRUE)
  col_title2 <- attr(x, "col_title2", exact = TRUE)

  width = max(
    out$data[[1]][[1]] |> pillar::get_max_extent(),
    5
  )
  if (is.null(col_title1)) {
    col_title1 <- rep("-", width) |> paste(collapse = "")
    col_title2 <- rep("-", width) |> paste(collapse = "")
  }
  if (col_title1 == "") {
    col_title1 <- rep("-", width) |> paste(collapse = "")
  }
  if (length(x) == 0) {
    col_title1 <- ""
    col_title2 <- ""
  }
  col_title1 = stringr::str_trunc(col_title1, width, ellipsis = "\u2026") |> cli::col_br_blue()
  col_title2 = stringr::str_trunc(col_title2, width, ellipsis = "\u2026") |> cli::col_br_blue()
  pillar::new_pillar(list(
    col_title1 = pillar::new_pillar_component(list(col_title1), width = width),
    col_title2 = pillar::new_pillar_component(list(col_title2), width = width),
    data = out$data
  ))
}


#' @importFrom pillar tbl_sum
#' @export
tbl_sum.pillar_wide_tab <- function(x, ...) {
  tables <- attr(x, "d")
  c(tables$tab_table$TabTitle)
}

#' @importFrom pillar tbl_format_setup
#' @export
tbl_format_setup.pillar_wide_tab <- function(x, width, ...) {
  rt <- x |> attr("d") |> _$row_table |> rm_header_footer()
  row_style <- dplyr::case_when(
    rt$RowContent %in% c("Total", "Valid") ~ "bold",
    rt$RowAbsPercent %in% "Abs" ~ "subtle",
    .default = "normal"
  )
  for (i in seq_along(x)) {
    attr(x[[i]], "style") <- row_style
  }
  setup <- NextMethod()
  setup
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.crosstab_column <- function(x, ...) {
  fmt <- format(x)
  style <- attr(x, "style")
  pillar::new_pillar_shaft_simple(pillar::style_subtle_num(fmt, negative = style == "subtle"), align = "right")
}
#' @export
format.crosstab_column <- function(x, ...) {
  style <- attr(x, "style")

  res <- vector("character", length(x))
  x_fmt <- vctrs::vec_data(x) |>
    pillar::pillar() |>
    utils::capture.output() |>
    _[-(1:2)] |> stringr::str_replace("NA", " \u00b7")
  for (i in seq_along(x)) {
    res[i] <- dplyr::case_when(
      style[i] == "bold" ~ pillar::style_bold(x_fmt[i]),
      style[i] == "subtle" ~ pillar::style_subtle(x_fmt[i]),
      style[i] == "normal" ~ x_fmt[i] |> as.character()
    )
  }
  pillar::new_ornament(res, width = nchar(x_fmt[1]), align = "right")
}
