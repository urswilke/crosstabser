# to be removed:
gen_qrows <- function(mapping) {
  qrows <- mapping$qsheet$qsheet_raw |>
    tidyr::nest(p = c(Unguelt:Exclusive))

  qsheet_processed <- mapping$qsheet$qsheet_processed
  qrow_processed <- qsheet_processed |> split(qsheet_processed$row)

  l_qtabs <- purrr::map(qrow_processed, \(x) new_qtabs(x, mapping))
  qrows$qrow <- lapply(l_qtabs, \(x) Qrow$new(x, mapping))

  qrows
}
# to be removed:
Qrow <- R6::R6Class("Qrow",
                    public = list(
                      qtabs = list(),
                      m = list(),
                      initialize = function(qtabs,
                                            mapping,
                                            ...) {
                        self$qtabs <- qtabs
                        self$m <- mapping
                      },
                      calc_qrow_qtabs = function() {
                        calc_qrow_qtabs(self)
                        invisible(self)
                      },
                      wide_tabs = function() {
                        wide_tabs(self)
                        invisible(self)
                      }
                    )
)
Qrow2 <- R6::R6Class("Qrow",
                    public = list(
                      p = list(),
                      m = list(),
                      qtabs = tibble::tibble(),
                      initialize = function(p,
                                            tab_table,
                                            mapping,
                                            ...) {
                        self$p <- p
                        self$m <- mapping
                        params_with_tab_table <- purrr::map2(
                          process_qrow_params(self$p, self$m),
                          tab_table |> split(seq_along(tab_table$row)),
                          \(x, y) {x$tab_table <- y; x}
                        )
                        self$qtabs <- tibble::tibble(params = params_with_tab_table)
                        self$qtabs$obj <- new_qtabs2(self$qtabs$params, mapping)

                        self$calc_qrow_qtabs()
                      },
                      calc_qrow_qtabs = function() {
                        calc_qrow_qtabs2(self)
                        invisible(self)
                      },
                      wide_tabs = function() {
                        wide_tabs(self)
                        invisible(self)
                      }
                    )
)
# to be removed:
calc_qrow_qtabs <- function(qrow) {
  qrow$qtabs$obj |> lapply(\(x) x$calc_qtab())
}
calc_qrow_qtabs2 <- function(qrow) {
  qrow$qtabs$obj |> lapply(\(x) x$calc_qtab())
}
wide_tabs <- function(qrow) {
  qrow$qtabs |> lapply(\(x) x$wide_tab())
}
