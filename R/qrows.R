init_qrows <- function(mapping) {
  qrows <- mapping$qsheet$qsheet_raw |>
    dplyr::select(-dplyr::matches("^Col[A-Z]$")) |>
    tidyr::nest(p = c(Unguelt:Exclusive))

  qsheet_processed <- mapping$qsheet$qsheet_processed
  qrows$qrow_processed <- qsheet_processed |> split(qsheet_processed$row)

  l_qtabs <- purrr::map(qrows$qrow_processed, \(x) new_qtabs(x, mapping))
  qrows$qtabs <- lapply(l_qtabs, \(x) new_qrows(x, mapping))

  mapping$qrows <- qrows
}

new_qrows <- function(qtabs, mapping) {
  Qrow$new(qtabs, mapping)
}
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
                      }
                    )
)
calc_qrow_qtabs <- function(qrow) {
  qrow$qtabs |> lapply(\(x) x$calc_qtab())
}
