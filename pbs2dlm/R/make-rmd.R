#' Create template Rmd file describing DLMtool Objects and Slots
#'
#' @param file_name Filename/path of where to save the .Rmd file.
#' @param overwite Overwite?
#'
#' @return Nothing
#' @export
#'
#' @examples
#' create_rmd()
create_rmd <- function(file_name, overwrite = FALSE) {
  fn <- file_name
  if (file.exists(fn) && !overwrite)
    stop("File '", fn, "' already exists. ",
      "Set `overwite = TRUE` if you want to overwite it.", call. = FALSE)

  rmd <- c(
    "```{r message = FALSE}\nlibrary(DLMtool)\n```\n",
    # format_desc(DLMtool::DataDescription, "Data"),
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
        "```{r ",
        inst_obj_name, "-", Slot,
        ", results = FALSE, echo = TRUE}\n",
        inst_obj_name, "@", Slot,
        "\n```\n"
      ),
      Slot = paste0(
        "### ",
        Slot
      ),
      Description = paste0(
        "*",
        Description,
        "*"
      )
    )
  df <- df[!grepl("NO LONGER USED", toupper(df$Description)), , drop = FALSE]
  df$Description <- gsub("([A-Za-z0-9]+)\\*$", "\\1.*", df$Description)
  df$Description <- gsub("([A-Za-z0-9]+)@([A-Za-z0-9]+)", "`\\1@\\2`",
    df$Description)

  c(
    toupper(paste0("## ", obj_name, " slot descriptions\n")),
    paste0(
      "```{r warnings = FALSE}\n",
      inst_obj_name, " <- methods::new('", obj_name, "')\n```\n"
    ),
    apply(df, 1, function(x) paste0(x, collapse = "\n\n"))
  )
}
