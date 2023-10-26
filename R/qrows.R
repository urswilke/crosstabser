Qrow <- R6::R6Class(
  "Qrow",
  public = list(
    p = list(),
    m = list(),
    qtabs = tibble::tibble(),
    initialize = function(p,
                          mapping,
                          ...) {
      self$p <- p
      self$m <- mapping
      self$qtabs <-
        tibble::tibble(params = process_qrow_params(self$p, self$m))
      self$qtabs$obj <-
        new_qtabs(self$qtabs$params, mapping)

      self$calc_qrow_qtabs()
    },
    calc_qrow_qtabs = function() {
      self$qtabs$obj |> lapply(\(x) x$calc_qtab())
      invisible(self)
    },
    wide_tabs = function() {
      self$qtabs |> lapply(\(x) x$wide_tab())
      invisible(self)
    },
    xml = function() {
      scen_name_suffix <-
        ifelse(
          self$m$options$V_ScenName == "Standard",
          "",
          paste0(self$m$options$V_ScenName, "_")
        )
      file_name <- paste0(
        self$m$options$V_XMLName,
        scen_name_suffix,
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
