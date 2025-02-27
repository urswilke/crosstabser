#' Write a table_charter app html file of crosstab data
#'
#' This helper function is used by `Tabula$save_html_app()`
#' to write a table_charter app html file of crosstab data.
#'
#' @param data_string Crosstab data produced with
#'   `Tabula$get_crosstabs_data() |> gen_data_json()`.
#' @param template_file Path to the template file (see description).
#' @param output_file File path to the table_charter app html file.
#' @param project_data Either a `list()` object to modify the default:
#'   `list(logo_base64 = "",
#'     logo_url =
#'       "https://gitlab.com/urswilke/table_charter/-/raw/main/img/logo_small.svg",
#'     title = "Dashboard",
#'     date = Sys.Date())`, or `NULL` (the default).
#'    If `NULL`, nothing is done.
#'    The fields will modify the elements in the header of the dashboard.
#'
#' @return No value's returned. This function writes a file.
#' @export
#'
#' @examples
#' # See documentation of `Tabula$save_html_app()`
write_html_app <- function(
  data_string,
  template_file = "https://gitlab.com/urswilke/table_charter/-/raw/main/example_dashboard.html",
  output_file = "dashboard.html",
  project_data = NULL
) {
  html <- template_file |> xml2::read_html()

  load_data_node <- html |>
    xml2::xml_find_all("//script[@id='load-example-data']")
  xml2::xml_remove(load_data_node)
  tc_node <- html |>
    xml2::xml_find_all(".//table-charter-intro|.//table-charter")
  xml2::xml_attr(tc_node, "data") <- data_string

  if (!is.null(project_data)) {
    project_data_node <- html |>
      xml2::xml_find_first(".//script[@id='project-data']")
    project_data_mod <- list(
      logo_base64 = "",
      logo_url =
        "https://gitlab.com/urswilke/table_charter/-/raw/main/img/logo_small.svg",
      title = "Dashboard",
      date = Sys.Date()
    ) |>
      utils::modifyList(project_data)
    xml2::xml_text(project_data_node) <- paste0(
      "\nconst project_data = ",
      jsonlite::toJSON(
        project_data_mod,
        auto_unbox = TRUE,
        pretty = TRUE
      ),
      ";\n"
    )
  }

  html |> xml2::write_html(output_file)
}
