init_qrows <- function(mapping) {
  qrows <- mapping$qsheet$qsheet_raw |>
    dplyr::select(-dplyr::matches("^Col[A-Z]$")) |>
    tidyr::nest(p = c(Unguelt:Exclusive))

  qrows$qsheet_row_idx <- split(
    mapping$qsheet$tab_table$TabNo,
    mapping$qsheet$tab_table$row
  )
  qrows$qtabs <- purrr::map(qrows$qsheet_row_idx, \(x) mapping$qtabs[x] |> new_qrows(mapping))
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
                      },
                      write_xml = function() {

                      }
                    )
)
calc_qrow_qtabs <- function(qrow) {
  qrow$qtabs |> lapply(\(x) x$calc_qtab())
}
