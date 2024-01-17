Qrow <- R6::R6Class(
  "Qrow",
  public = list(
    qtabs = tibble::tibble(),
    initialize = function(p,
                          mapping,
                          ...) {
      params <- process_qrow_params(p, mapping)
      obj <- params |>
        lapply(\(x) Qtab$new(x, mapping))
      self$qtabs <- tibble::tibble(params, obj)
    },
    wide_tabs = function() {
      self$qtabs |> lapply(\(x) x$wide_tab())
      invisible(self)
    },
    xml = function(
      file_name = paste0(
        self$m$options$V_XMLName,
        # scen_name_suffix,
        self$p$row,
        ".xml"
      )
    ) {
      self$qtabs$obj |>
        lapply(gen_5_tables) |>
        write_xml_file(file_name)
      invisible(self)
    }
  )
)
