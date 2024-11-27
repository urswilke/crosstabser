summarize_stats <- function(df, x, wt = NULL, stat_fun = "length", ..., .by) {
  if (is.null(wt)) {
    return(summarize_stats_unweighted(
      df = df,
      x = x,
      stat_fun = stat_fun,
      ...,
      .by = .by))
  }
  summarize_stats_weighted(
    df = df,
    x = x,
    wt = wt,
    stat_fun = stat_fun,
    ...,
    .by = .by)
}

summarize_stats_unweighted <- function(df, x, stat_fun = "length", ..., .by) {
  if (length(x) < 2) {
    # HACK to return the results in a column named "value":
    names(df)[names(df) %in% x] <- "value"
    if (is.null(x)) {
      df$value <- 1
    }
    x <- "value"
  }

  stats::aggregate(
    stats::reformulate(.by, aggregate_fml_lhs(x)),
    data = df,
    stat_fun,
    ...
  ) |> dplyr::as_tibble()

}
summarize_stats_weighted <- function(df, x, wt, stat_fun = "length", ..., .by) {
  if (is.null(x)) {
    x <- "value"
    df[[x]] <- NA_real_
  }

  res <- df |>
    dplyr::summarise(dplyr::across(dplyr::all_of(x),
      \(vec) apply_stat(
        x = vec,
        wt = weight,
        stat_fun = stat_fun,
        ...
      )),
      .by = dplyr::all_of(.by)
    )
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

apply_stat <- function(x, wt = NULL, stat_fun = "length", ...) {
  new_stat_vec(
    x = x,
    wt = wt,
    stat_fun = stat_fun,
    ...
  ) |>
    stat_fun_wt()
}

new_stat_vec <- function(x, wt = NULL, stat_fun = "length", ...) {
  res <- structure(
    list(
      vec = x,
      wt = wt,
      ...
    ),
    class = c(stat_fun, class(x))
  )
  if (!is.null(wt)) {
    class(res) <- c("weighted", class(res))
  } else {
    class(res) <- c("unweighted", class(res))
  }
  res
}
#' Calculate sample statistics
#'
#' @param x numeric vector
#' @param na.rm remove NAs
#' @keywords internal
#' @export
se <- function(x, na.rm = TRUE, ...) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  stats::sd(x) / sqrt(length(x))
}
#' @rdname se
#' @keywords internal
#' @export
percentile <- function(x, na.rm = TRUE, ...) {
  stats::quantile(x, na.rm = na.rm, type = 2, ...)
}
stat_fun_wt <- function(x, ...) {
  UseMethod("stat_fun_wt")
}
stat_fun_wt.length <- function(x, na.rm = TRUE, ...) {
  sum(x$wt, na.rm = na.rm)
}
stat_fun_wt.mean <- function(x, na.rm = TRUE, ...) {
  if (na.rm) {
    x <- rm_na(x)
  }
  if (length(x$vec) == 0) {
    return(NA_real_)
  }
  stats::weighted.mean(x$vec, x$wt, na.rm = na.rm)
}
stat_fun_wt.median <- function(x, na.rm = TRUE, ties = "mean", ...) {
  matrixStats::weightedMedian(x$vec, x$wt, na.rm = na.rm, ties = ties)
}
stat_fun_wt.sum <- function(x, na.rm = TRUE, ...) {
  if (na.rm) {
    x <- rm_na(x)
  }
  if (length(x$vec) == 0) {
    return(NA_real_)
  }
  sum(x$vec * x$wt, na.rm = na.rm)
}
# TODO: check if this leads to the same results as with SPSS:
stat_fun_wt.se <- function(x, na.rm = TRUE, ...) {
  # see here: https://stackoverflow.com/a/60235611
  if (na.rm) {
    x <- rm_na(x)
  }
  if (length(x$vec) == 1) {
    return(NA_real_)
  }
  w <- x$wt
  if (sum(w) <= 1) {
    # see https://github.com/harrelfe/Hmisc/issues/69
    return(NA_real_)
  }
  sqrt(
    Hmisc::wtd.var(x$vec, w) *
      sum(w / sum(w)^2)
  )
}
rm_na <- function(x) {
  obs <- !is.na(x$vec) & !is.na(x$wt)
  x$vec <- x$vec[obs]
  x$wt <- x$wt[obs]
  x
}

stat_fun_wt.min <- function(x, na.rm = TRUE, ...) {
  min(x$vec, na.rm = na.rm, ...) |> dplyr::na_if(Inf)
}
stat_fun_wt.max <- function(x, na.rm = TRUE, ...) {
  max(x$vec, na.rm = na.rm, ...) |> dplyr::na_if(-Inf)
}
stat_fun_wt.percentile <- function(x, na.rm = TRUE, ...) {
  Hmisc::wtd.quantile(x$vec, x$wt, probs = x$probs)
}

