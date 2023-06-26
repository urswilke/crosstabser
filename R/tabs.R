init_qtabs <- function(mapping) {
  global_options <- mapping$options$l_macro_scenario

  params <- mapping$qsheet$qsheet_processed |>
    purrr::transpose() |>
    purrr::map(\(x) x[!is.na(x)]) |>
    purrr::map(\(x) add_global_options(x, global_options))
  qtabs <- params |>
    purrr::map(\(x) new_qtabs(x, mapping))


  class(qtabs) <- c("crosstabser_tabs", class(qtabs))
  mapping$qtabs <- qtabs
}

new_qtabs <- function(params, mapping) {
  res <- Qtab$new(params, mapping)
  class(res) <- c(paste0("qtab_type_", params$Type), class(res))
  res
}
Qtab <- R6::R6Class("Qtab",
  public = list(
    p = list(),
    d = list(),
    initialize = function(params,
                          mapping,
                          ...) {
      self$p <- params
      self$p$l_lexikon <- mapping$options$l_lexikon
      self$d$dat_mod  <- mapping$dat_mod
      self$d$tab_table <- mapping$qsheet$tab_table
      self$d$head_table <- mapping$qsheet$head_table
      self$d$col_table <- mapping$qsheet$col_table
    },
    calc_qtab = function() {
      calc_qtab(self)
    }
  )
)
calc_qtab <- function(qtab) {
  qtab$d$raw_data = get_raw_data(qtab)
  pivot_table_data(qtab)
  qtab$d$counts = crosstab(qtab)
  qtab$d$row_table <- gen_row_table(qtab)
  qtab$d$val_table <- gen_val_table(qtab)
}
add_global_options <- function(params, global_options) {
  res <- params
  res$Filter <- append(res$Filter, global_options$Filter[!is.na(global_options$Filter)])
  res$Weight <- dplyr::coalesce(res$Weight, global_options$Weight)
  if (length(res$Unguelt) == 0) {
    res$Unguelt <- global_options$Unguelt
  }
  res$ColVar <- global_options$ColVar
  res
}

#' @export
print.crosstabser_tabs <- function(x,
                                   # unnest fields:
                                   uf = c("d"),
                                   n = 30,
                                   ...) {
  field_list <- x |> purrr::map(get_r6_fields)
  df <- tibble::tibble(x = field_list) |>
    tidyr::unnest_wider(x)
  if (length(uf) > 0) {
    df <- df |>
      tidyr::unnest_wider(uf)
    # |>
    #   dplyr::select(-matches("^Col[A-Z]$"))
  }
  print(df, n = n, ...)
}

get_r6_fields <- function(r6_obj) {
  r6_list <- as.list(r6_obj)
  r6_list[r6_list |> purrr::map_lgl(\(x) !is.environment(x) && !is.function(x))]

}
