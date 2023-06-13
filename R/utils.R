split_cell <- function(x, sep = "[,; ]+") {
  x |>
    stringr::str_squish() |>
    stringr::str_split(sep)
  # |>
  #   purrr::map(~.x[!is.na(.x)])
}

strip_attributes <- function(x) {
  attributes(x) <- NULL
  x
}

row_split <- function(df) {
  df |>
    dplyr::rowwise() |>
    dplyr::group_split()
}

all_true <- function (l) {
  Reduce("&", l)
}
