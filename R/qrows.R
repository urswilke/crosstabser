Qrow <- R6::R6Class("Qrow",
                    public = list(
                      p = list(),
                      m = list(),
                      qtabs = tibble::tibble(),
                      initialize = function(p,
                                            mapping,
                                            ...) {
                        self$p <- p
                        self$m <- mapping
                        self$qtabs <- tibble::tibble(params = process_qrow_params(self$p, self$m))
                        self$qtabs$obj <- new_qtabs(self$qtabs$params, mapping)

                        self$calc_qrow_qtabs()
                      },
                      calc_qrow_qtabs = function() {
                        calc_qrow_qtabs(self)
                        invisible(self)
                      },
                      wide_tabs = function() {
                        wide_tabs(self)
                        invisible(self)
                      },
                      xml = function() {
                        file_name <- paste0(
                          self$m$options$V_XMLName,
                          stringr::str_pad(self$p$row, 4, pad = "0"),
                          ".xml"
                        )
                        self$qtabs$obj |>
                          lapply(gen_5_tables) |>
                          write_xml_file(file_name)
                        invisible(self)
                      }
                    )
)
calc_qrow_qtabs <- function(qrow) {
  qrow$qtabs$obj |> lapply(\(x) x$calc_qtab())
}
wide_tabs <- function(qrow) {
  qrow$qtabs |> lapply(\(x) x$wide_tab())
}
