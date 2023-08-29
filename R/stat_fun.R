apply_stat <- function(x, wt = NULL, stat_fun = "counts", ...) {
  new_stat_vec(
    x = x,
    wt = wt,
    stat_fun = stat_fun,
    ...
  ) |>
    stat_fun()
}

new_stat_vec <- function(x, wt = NULL, stat_fun = "counts", ...) {
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
stat_fun <- function(x, ...) {
  UseMethod("stat_fun")
}


stat_fun.unweighted <- function(x, ...) {
  stat_fun_uw(x, ...)
}
stat_fun.weighted <- function(x, ...) {
  stat_fun_wt(x, ...)
}

stat_fun_uw <- function(x, ...) {
  UseMethod("stat_fun_uw")
}
stat_fun_uw.counts <- function(x, ...) {
  length(x$vec)
}
stat_fun_uw.mean <- function(x, ...) {
  mean(x$vec)
}
stat_fun_uw.median <- function(x, na.rm = TRUE) {
  stats::median(x$vec, na.rm = na.rm)
}
stat_fun_uw.sum <- function(x, na.rm = TRUE, ...) {
  sum(x$vec, na.rm = na.rm)
}
stat_fun_uw.se <- function(x, na.rm = TRUE, ...) {
  if (na.rm) {
    x$vec <- x$vec[!is.na(x$vec)]
  }
  sd(x$vec) / sqrt(length(x$vec))
}
stat_fun_uw.percentile <- function(x, na.rm = TRUE, ...) {
  quantile(x$vec, probs = x$probs, na.rm = na.rm)
}
stat_fun_uw.min <- stat_fun_wt.min <- function(x, na.rm = TRUE, ...) {
  min(x$vec, na.rm = na.rm)
}
stat_fun_uw.max <- stat_fun_wt.max <- function(x, na.rm = TRUE, ...) {
  max(x$vec, na.rm = na.rm)
}
stat_fun_wt <- function(x, ...) {
  UseMethod("stat_fun_wt")
}
stat_fun_wt.counts <- function(x, na.rm = TRUE, ...) {
  sum(x$wt, na.rm = na.rm)
}
stat_fun_wt.mean <- function(x, ...) {
  stats::weighted.mean(x$vec, x$wt)
}
stat_fun_wt.median <- function(x, na.rm = TRUE, ties = "mean", ...) {
  matrixStats::weightedMedian(x$vec, x$wt, na.rm = na.rm, ties = ties)
}
stat_fun_wt.sum <- function(x, na.rm = TRUE, ...) {
  sum(x$vec * x$wt, na.rm = na.rm)
}
# TODO: check if this leads to the same results as with SPSS:
stat_fun_wt.se <- function(x, na.rm = TRUE, ...) {
  # see here: https://stackoverflow.com/a/60235611
  if (na.rm) {
    obs <- !is.na(x$vec) & !is.na(x$wt)
    x$vec <- x$vec[obs]
    x$wt <- x$wt[obs]
  }
  mu <- stats::weighted.mean(x, w)
  sqrt(sum(x$wt * ((x$vec - mu) ^ 2)) / (sum(x$wt) - 1))
}
