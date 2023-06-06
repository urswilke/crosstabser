split_cell <- function(x, sep = "[,; ]+") {
  x |>
    stringr::str_squish() |>
    stringr::str_split(sep)
  # |>
  #   purrr::map(~.x[!is.na(.x)])
}
