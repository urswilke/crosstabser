gen_tab_params <- function(mapping) {
  global_options <- mapping$options$l_macro_scenario

  params <- mapping$qsheet$qsheet_processed |>
    purrr::transpose() |>
    purrr::map(\(x) x[!is.na(x)]) |>
    purrr::map(\(x) add_global_options(x, global_options))
  tabs <- params |>
    purrr::map(\(x) new_tabs_subclass(x, mapping$dat_mod))


  class(tabs) <- c("crosstabser_tabs", class(tabs))
  mapping$tabs <- tabs
}

new_tabs_subclass <- function(params, dat_mod) {
  res <- Tabs$new(params, dat_mod)
  class(res) <- c(paste0("tab_type_", params$Type), class(res))
  res
}
Tabs <- R6::R6Class("Tabs",
  public = list(
    p = list(),
    d = list(),
    initialize = function(params,
                          dat_mod,
                          ...) {
      self$p <- params
      self$d$dat_mod = dat_mod
    },
    add_tab_data = function() {
      add_tab_data(self)
    }
  )
)
add_tab_data <- function(tab) {
  tab$d$raw_data = get_raw_data(tab)
  tab$d$long_data = pivot_table_data(tab)
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
