new_qtabs <- function(qsheet_processed, mapping) {
  global_options <- mapping$options$l_macro_scenario
  params <- qsheet_processed |>
    purrr::transpose() |>
    purrr::map(\(x) x[!is.na(x)]) |>
    purrr::map(\(x) add_global_options(x, global_options))
  qtabs <- params |>
    purrr::map(\(x) new_qtab(x, mapping))
  qtabs |>
    purrr::walk(\(x) add_type_specific_params(x))


  class(qtabs) <- c("crosstabser_tabs", class(qtabs))
  qtabs

}

new_qtab <- function(params, mapping) {
  res <- Qtab$new(params, mapping)
  class(res) <- c(paste0("qtab_type_", params$Type), class(res))
  res
}

add_type_specific_params <- function(qtab) {
  UseMethod("add_type_specific_params")
}

add_type_specific_params.default <- function(qtab) {
  NULL
}
add_type_specific_params.qtab_type_mdg <- function(qtab) {
  mdg_val <- qtab$p$MdgVal %||% 1
  qtab$p$MdgVal <- as.numeric(mdg_val)
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
      self$d$tab_table <- mapping$qsheet$tab_table[
        mapping$qsheet$tab_table$TabNo == self$p$TabNo,
      ]
      self$d$head_table <- mapping$qsheet$head_table
      self$d$col_table <- mapping$qsheet$col_table
    },
    calc_qtab = function() {
      calc_qtab(self)
    }
  )
)
calc_qtab <- function(qtab) {
  qtab$d$raw_data <- get_raw_data(qtab)
  pivot_table_data(qtab)
  calc_stats_rows(qtab)
  qtab$d$detail_freqs <- calc_detail_freqs(qtab)
  calc_percentages(qtab)
  qtab$d$tab_values <- merge_table_parts(qtab)
  qtab$d$row_table <- gen_row_table(qtab)
  qtab$d$val_table <- gen_val_table(qtab)
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
