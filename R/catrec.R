catrec <- function(vec, cat_rec_string, else_value = NULL) {
  if (is.null(else_value)) else_value <- vec
  cat_rec_interval_splits <- split_cat_rec_string(cat_rec_string)
  cat_rec_quos <- lapply(cat_rec_interval_splits$interval_strings, gen_cat_rec_fun)
  cat_rec_exprs <- stringr::str_extract_all(cat_rec_string, "(?<=\\().*?(?=\\))")[[1]]
  cat_rec_vals <- stringr::str_extract(cat_rec_exprs, "(?<=\\=) *-?\\d+$") |> as.numeric()

  l_cat_rec <- purrr::map2(
    cat_rec_quos,
    cat_rec_vals,
    \(f, x) rlang::quo(!!f(vec) ~ !!x)
  )
  dplyr::case_when(!!!l_cat_rec, .default = else_value)
}

split_cat_rec_string <- function(cat_rec_string) {
  cat_rec_string <- cat_rec_string |> stringr::str_replace_all("\\bHI\\b", "Inf")

  cat_rec_exprs <- stringr::str_extract_all(cat_rec_string, "(?<=\\().*?(?=\\))")[[1]]

  cat_rec_vals <- stringr::str_extract(cat_rec_exprs, "(?<=\\=) *\\d+$") |> as.numeric()
  cat_rec_intervals <- stringr::str_extract(cat_rec_exprs, "^.*(?=\\=)")
  cat_rec_interval_splits <- strsplit(cat_rec_intervals, ", *")
  list(vals = cat_rec_vals, interval_strings = cat_rec_interval_splits)
}

split_cat_lab_string <- function(cat_lab_string) {
  cat_lab_exprs <- stringr::str_extract_all(cat_lab_string, "\\d+ *['\"].*?['\"]")[[1]]
  cat_lab_nums <- cat_lab_exprs |> stringr::str_extract("^\\d+") |> as.numeric()
  cat_lab_labs <- cat_lab_exprs |> stringr::str_extract("(?<=['\"]).*(?=['\"])")
  purrr::set_names(cat_lab_nums, cat_lab_labs)
}

gen_cat_rec_funs <- function(cat_rec_interval_splits) {
  perhaps_numeric <- as.numeric(cat_rec_interval_splits) |> suppressWarnings()
  if (!is.na(perhaps_numeric)) {
    return(
      function(x) {
        x == perhaps_numeric
      }
    )
  }
  if (stringr::str_detect(cat_rec_interval_splits, "THRU")) {
    cat_rec_boundaries <- strsplit(cat_rec_interval_splits, " *THRU *")[[1]] |> as.numeric()
    return(
      function(x) {
        x >= cat_rec_boundaries[1] & x <= cat_rec_boundaries[2]
      }
    )
  }
  stop("Couldn't read CatRec string", cat_rec_interval_splits)
}
gen_cat_rec_fun <- function(cat_rec_interval_split) {
  l <- cat_rec_interval_split |> purrr::map(gen_cat_rec_funs)
  function(x) {
    Reduce("|", lapply(l, \(f) f(x)))
  }
}

mw_rec_helper <- function(vec, mw_rec_string) {
  else_value <- high_value <- 98765432123
  else_copy_substring <- "\\( *ELSE *\\= *COPY *\\)"
  if (stringr::str_detect(mw_rec_string, else_copy_substring)) {
    mw_rec_string <- stringr::str_remove(mw_rec_string, else_copy_substring)
    else_value <- vec
  }
  vec <- catrec(vec, mw_rec_string, else_value)
  if (!is.na(match(high_value, vec))) {
    stop(
      "Did you forget to recode occurring values in the string\n",
      mw_rec_string, "\n",
      "You need to set `(ELSE=COPY)` in at the end of the recoding string to keep values without overwriting."
    )
  }
  vec
}
