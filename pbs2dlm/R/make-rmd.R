#' Create template Rmd file describing DLMtool Objects and Slots
#'
#' @param file_name Filename/path of where to save the .Rmd file.
#' @param overwrite Overwrite?
#' @param knitr_results Show knitr results?
#' @param knitr_echo Echo knitr code?
#'
#' @return Nothing
#' @export
#'
#' @examples
#' create_rmd()
create_rmd <- function(file_name, overwrite = FALSE,
  knitr_results = TRUE, knitr_echo = TRUE) {
  fn <- file_name
  if (file.exists(fn) && !overwrite)
    stop("File '", fn, "' already exists. ",
      "Set `overwrite = TRUE` if you want to overwrite it.", call. = FALSE)

  rmd <- c(
    "```{r message = FALSE}\nlibrary(DLMtool)",
    paste0("knitr_results <- ", as.character(eval(knitr_results))),
    paste0("knitr_echo <- ", as.character(eval(knitr_echo))),
    "```\n",
    # format_desc(DLMtool::DataDescription, "Data"),
    format_desc(DLMtool::StockDescription, "Stock"),
    format_desc(DLMtool::FleetDescription, "Fleet"),
    format_desc(DLMtool::ObsDescription, "Obs"),
    format_desc(DLMtool::ImpDescription, "Imp")
    # format_desc(DLMtool::OMDescription, "OM")
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

  df$chunk_name <- tolower(paste0("desc-", inst_obj_name, "-", df$Slot))

  df <- df %>%
    mutate(
      code = paste0(
        "```{r ",
        chunk_name,
        ", results = knitr_results, echo = knitr_echo}\n",
        inst_obj_name, "@", Slot,
        "\n```\n"
      ),
      Slot = paste0(
        "<!-- autogen-begin -->\n",
        "### ",
        Slot,
        paste0(" {#app:", chunk_name, "}")
      ),
      Description = paste0(
        "*",
        Description,
        "*\n",
        "<!-- autogen-end -->"
      )
    )

  df <- df[!grepl("NO LONGER USED", toupper(df$Description)), , drop = FALSE]
  df$chunk_name <- NULL
  df$Description <- gsub("([A-Za-z0-9]+)\\*$", "\\1.*", df$Description)
  df$Description <- gsub("([A-Za-z0-9]+)@([A-Za-z0-9]+)", "`\\1@\\2`",
    df$Description)

  c(
    paste0(
      toupper(paste0("## ", obj_name, " slot descriptions ")),
      tolower(paste0("{#app:desc-", obj_name, "}\n"))
    ),
    paste0(
      "```{r warnings = FALSE}\n",
      inst_obj_name, " <- methods::new('", obj_name, "')\n```\n"
    ),
    apply(df, 1, function(x) paste0(x, collapse = "\n\n"))
  )
}
