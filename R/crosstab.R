# hack to do double dispatch on is.na(weight) & stat_fun:
new_sum_stat <- function(df_long, weight, stat_fun) {
  subclass_str <- dplyr::case_when(
    is.na(weight)  & is.na(stat_fun)    ~ "counts_unweighted",
    !is.na(weight) & is.na(stat_fun)    ~ "counts_weighted",
    is.na(weight)  & stat_fun == "mean"   ~ "mean_unweighted",
    is.na(weight)  & stat_fun == "median" ~ "median_unweighted",
    is.na(weight)  & stat_fun == "sum"    ~ "sum_unweighted",
    !is.na(weight) & stat_fun == "mean"   ~ "mean_weighted",
    !is.na(weight) & stat_fun == "median" ~ "median_weighted",
    !is.na(weight) & stat_fun == "sum"    ~ "sum_weighted"
  )
  if (length(subclass_str) == 0) {
    subclass_str <- "counts_unweighted"
  }
  structure(df_long, class = c(subclass_str, class(df_long)))
}
apply_sum_stat <- function(df_long, ...) {
  UseMethod("apply_sum_stat")
}
apply_sum_stat.counts_unweighted <- function(df_long, ...) {
  df_long %>%
    dplyr::summarize(
      value = dplyr::n(),
      .groups = "drop"
    )
}
apply_sum_stat.counts_weighted <- function(df_long, ...) {
  df_long %>%
    dplyr::summarize(value = sum(weight, na.rm = TRUE), .groups = "drop")
}
apply_sum_stat.mean_unweighted <- function(df_long, ...) {
  df_long %>%
    dplyr::summarize(value = mean(rowval, na.rm = TRUE), .groups = "drop")
}
apply_sum_stat.median_unweighted <- function(df_long, ...) {
  df_long %>%
    dplyr::summarize(value = stats::median(rowval, na.rm = TRUE), .groups = "drop")
}
apply_sum_stat.sum_unweighted <- function(df_long, ...) {
  df_long %>%
    dplyr::summarize(value = sum(rowval, na.rm = TRUE), .groups = "drop")
}
apply_sum_stat.mean_weighted <- function(df_long, ...) {
  df_long %>%
    tidyr::drop_na(weight) %>%
    dplyr::summarize(value = stats::weighted.mean(rowval, weight, na.rm = TRUE), .groups = "drop")
}
apply_sum_stat.median_weighted <- function(df_long, ...) {
  df_long %>%
    dplyr::summarize(
      value = matrixStats::weightedMedian(rowval, weight, na.rm = TRUE, ties = "mean"),
      .groups = "drop"
    )
}
apply_sum_stat.sum_weighted <- function(df_long, ...) {
  df_long %>%
    dplyr::summarize(value = sum(rowval * weight, na.rm = TRUE), .groups = "drop")
}
