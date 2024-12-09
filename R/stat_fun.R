.onLoad <- function(...) {
  S7::methods_register()
}


summarize_stats <- function(df, x, wt = NULL, stat_fun = "length", ..., .by) {
  if (length(x) < 2) {
    # HACK to return the results in a column named "value":
    names(df)[names(df) %in% x] <- "value"
    if (is.null(x)) {
      df$value <- 1
    }
    x <- "value"
  }
  f = stat_fun|> ct_fun()
  # HACK to add an S3 dispatch mechanism for all the function subclasses needed:
  class(f) <- c(paste0("ct_", stat_fun), class(f))
  res <- if (!is.null(wt)) {
    df |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(x),
        \(vec) apply_fun(
          f = f,
          w = weight |> ct_weighted(),
          x = vec,
          ...
        )),
        .by = dplyr::all_of(.by)
      )
  } else {
    weight <- list(NULL) |> ct_unweighted()
    # df |>
    #   dplyr::summarise(
    #     dplyr::across(
    #       dplyr::all_of(x),
    #       \(x) apply_fun_helper(f = f, w = weight, x = x, ...)
    #     ),
    #     .by = dplyr::all_of(.by)
    #   )
    stats::aggregate(
      stats::reformulate(.by, aggregate_fml_lhs(x)),
      data = df,
      apply_fun_helper,
      f = f,
      w = weight,
      ...
    ) |> dplyr::as_tibble()

  }
  if (length(x) == 1) {
    names(res)[names(res) == x] <- "value"
  }
  res
}

# HACK to allow to compute multiple columns with aggregate():
aggregate_fml_lhs <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  paste0("cbind(", paste(x, collapse = ", "), ")")
}

ct_fun <- S7::new_class("ct_fun", S7::class_character)
ct_weighted <- S7::new_class("ct_weighted", S7::class_double)
ct_unweighted <- S7::new_class("ct_unweighted", S7::class_list)

apply_fun <- S7::new_generic("apply_fun", c("f", "w"))
S7::method(
  apply_fun,
  list(ct_fun, ct_unweighted)
) <- function(
    f,
    w,
    x,
    ...
) {
  apply_fun_unweighted(f, x, ...)
}
S7::method(
  apply_fun,
  list(ct_fun, ct_weighted)
) <- function(
    f,
    w,
    x,
    ...
) {
  apply_fun_weighted(f, S7::S7_data(w), x, na.rm = TRUE, ...)
}
apply_fun_helper <- function(
    x,
    w,
    f,
    ...
) {
  apply_fun(f, w, x, ...)
}

apply_fun_weighted <- function(f, w, x, na.rm = TRUE, ...) {
  UseMethod("apply_fun_weighted")
}
apply_fun_unweighted <- function(f, x, na.rm = TRUE, ...) {
  UseMethod("apply_fun_unweighted")
}
apply_fun_unweighted.ct_length <- function(f, x, na.rm = TRUE, ...) {
  length(x)
}
apply_fun_unweighted.ct_mean <- function(f, x, na.rm = TRUE, ...) {
  res <- mean(x, na.rm = na.rm, ...)
  if (is.nan(res)) res <- NA_real_
  res
}
apply_fun_unweighted.ct_median <- function(f, x, na.rm = TRUE, ...) {
  median(x, na.rm = na.rm, ...)
}
apply_fun_unweighted.ct_sum <- function(f, x, na.rm = TRUE, ...) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = na.rm, ...)
}
apply_fun_unweighted.ct_min <- function(f, x, na.rm = TRUE, ...) {
  min(x, na.rm = na.rm, ...) |> dplyr::na_if(Inf)
}
apply_fun_unweighted.ct_max <- function(f, x, na.rm = TRUE, ...) {
  max(x, na.rm = na.rm, ...) |> dplyr::na_if(-Inf)
}
apply_fun_unweighted.ct_se <- function(f, x, na.rm = TRUE, ...) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  stats::sd(x) / sqrt(length(x))
}
apply_fun_unweighted.ct_percentile <- function(f, x, na.rm = TRUE, ...) {
  stats::quantile(x, na.rm = na.rm, type = 2, ...)
}
apply_fun_weighted.ct_length <- function(f, w, x, na.rm = TRUE, ...) {
  sum(w, na.rm = na.rm)
}
apply_fun_weighted.ct_mean <- function(f, w, x, na.rm = TRUE, ...) {
  if (na.rm) {
    obs <- !is.na(x) & !is.na(w)
    x <- x[obs]
    w <- w[obs]
  }
  if (length(x) == 0) {
    return(NA_real_)
  }
  stats::weighted.mean(x, w, na.rm = na.rm)
}
apply_fun_weighted.ct_median <- function(f, w, x, na.rm = TRUE, ties = "mean", ...) {
  matrixStats::weightedMedian(x, w, na.rm = na.rm, ties = ties)
}
apply_fun_weighted.ct_sum <- function(f, w, x, na.rm = TRUE, ...) {
  if (na.rm) {
    obs <- !is.na(x) & !is.na(w)
    x <- x[obs]
    w <- w[obs]
  }
  if (length(x) == 0) {
    return(NA_real_)
  }
  sum(x * w, na.rm = na.rm)
}
apply_fun_weighted.ct_se <- function(f, w, x, na.rm = TRUE, ...) {
  # see here: https://stackoverflow.com/a/60235611
  if (na.rm) {
    obs <- !is.na(x) & !is.na(w)
    x <- x[obs]
    w <- w[obs]
  }
  if (length(x) == 1) {
    return(NA_real_)
  }
  if (sum(w) <= 1) {
    # see https://github.com/harrelfe/Hmisc/issues/69
    return(NA_real_)
  }
  sqrt(
    Hmisc::wtd.var(x, w) *
      sum(w / sum(w) ^ 2)
  )
}
apply_fun_weighted.ct_min <- function(f, w, x, na.rm = TRUE, ...) {
  min(x, na.rm = na.rm, ...) |> dplyr::na_if(Inf)
}
apply_fun_weighted.ct_max <- function(f, w, x, na.rm = TRUE, ...) {
  max(x, na.rm = na.rm, ...) |> dplyr::na_if(-Inf)
}
apply_fun_weighted.ct_percentile <- function(f, w, x, probs, na.rm = TRUE, ...) {
  Hmisc::wtd.quantile(x, w, probs = probs)
}
