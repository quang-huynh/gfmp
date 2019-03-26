#' Create template Rmd file describing DLMtool Objects and Slots
#'
#' @param file_name Filename/path of where to save the .Rmd file.
#'
#' @return Nothing
#' @export
#'
#' @examples
#' create_rmd()
create_rmd <- function(file_name) {
  if (file.exists(fn)) {
    stop("File '", fn, "' exists. Delete or move before running this function.",
      call. = FALSE)
  }

  rmd <- c(
    "```{r message = FALSE}\nlibrary(DLMtool)\n```\n",
    format_desc(DLMtool::DataDescription, "Data"),
    format_desc(DLMtool::StockDescription, "Stock"),
    format_desc(DLMtool::FleetDescription, "Fleet"),
    format_desc(DLMtool::ObsDescription, "Obs"),
    format_desc(DLMtool::ImpDescription, "Imp"),
    format_desc(DLMtool::OMDescription, "OM")
  )

  conn <- file(fn)
  write(rmd, conn)
  close(conn)
}

#' Format a DLMtool description dataframe into Rmarkdown format and produce a
#'   string combining all of them together with a section name
#'
#' @param df DLMtool Description data frame
#' @param obj_name Name to use for the section
#' @param inst_obj_name The name to use for the instance of the object
#'
#' @return The Rmd - formatted vector of strings
#' @noRd
#'
#' @examples
#' format_desc(DLMtool::DataDescription, "Data")
format_desc <- function(df,
  obj_name = "Data",
  inst_obj_name = tolower(obj_name)) {
  df <- df %>%
    mutate(
      code = paste0(
        "```{r, ",
        inst_obj_name, "-", Slot,
        ", results = FALSE, echo = TRUE}\n",
        inst_obj_name, "@", Slot,
        "\n```"
      ),
      Slot = paste0(
        "## ",
        Slot,
        "\n"
      ),
      Description = paste0(
        "*",
        Description,
        "*\n"
      )
    )

  c(
    paste0("# **", obj_name, " slot descriptions**\n\n"),
    paste0(
      "```{r warnings = FALSE}\n  ",
      inst_obj_name, " <- methods::new('", obj_name, "')\n```\n"
    ),
    apply(df, 1, function(x) paste0(x, collapse = "\n\n"))
  )
}
