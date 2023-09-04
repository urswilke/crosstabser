#' @export
print.Qtab = function(x, ...) print(format(x), n = Inf, ...)

#' @export
format.Qtab <- function(qtab, ...) {
  res <- qtab$d$wide_tab
  col_table <- qtab$d$col_table
  col_table$ColTitle1[col_table$ColTitle1 == dplyr::lag(col_table$ColTitle1)] <- ""
  col_table$ColTitle2[is.na(col_table$ColTitle2)] <- ""
  res[-1] <- purrr::pmap_dfc(
    list(
      res[-1],
      col_table$ColTitle1,
      col_table$ColTitle2
    ),
    \(x, t1, t2) {
      attr(x, "col_title1") <- t1
      attr(x, "col_title2") <- t2
      x
    }
  )

  attr(res, "d") <- qtab$d[c("tab_table", "head_table", "col_table", "row_table")]
  class(res) <- c("pillar_wide_tab", class(res))
  res
}

str_trunc_pad <- function(string, width = 5) {
  stringr::str_trunc(string, width, ellipsis = "…") |>
    stringr::str_pad(width, "right")
}

#' @importFrom pillar ctl_new_rowid_pillar
#' @export
ctl_new_rowid_pillar.pillar_wide_tab <- function(controller, x, width, rowlab_wid1 = 19, rowlab_wid2 = 5, rowlab_wid3 = 5, ...) {
  out <- NextMethod()
  tables <- attr(controller, "d")
  row_title1 <- tables$row_table$RowTitle1
  row_title2 <- tables$row_table$RowTitle2
  row_title3 <- tables$row_table$RowTitle3
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
      title = out$title,
      type = out$type,
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
# # https://stackoverflow.com/a/69702705
# tbl_format_body.pillar_wide_tab <- function (x, setup, ...) {
#   force(setup)
#   setup$body[-c(1:2)]
# }

#' @importFrom pillar ctl_new_pillar
#' @export
ctl_new_pillar.pillar_wide_tab <- function(controller, x, width, ..., title = NULL) {
  tables <- attr(controller, "d")
  out <- NextMethod()

  col_title1 <- attr(x, "col_title1", exact = TRUE)
  col_title2 <- attr(x, "col_title2", exact = TRUE)

  width = pillar:::pillar_get_width(out)
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
  col_title1 = stringr::str_trunc(col_title1, width, ellipsis = "…") |> cli::col_blue()
  col_title2 = stringr::str_trunc(col_title2, width, ellipsis = "…") |> cli::col_blue()
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
