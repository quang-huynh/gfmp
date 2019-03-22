#' Create template Rmd file describing DLMtool Objects and Slots
#'
#' @param fn Filename
#'
#' @return Nothing
#' @export
#'
#' @examples
#' create_rmd()
create_rmd <- function(fn = file.path(here::here("report"),
                                      "description.Rmd")){

  if(file.exists(fn)){
    stop("File '", fn, "' exists. Delete or move before running this function.")
  }

  #' Format a DLMtool description dataframe into Rmarkdown format and produce a
  #'   string combining all of them together with a section name
  #'
  #' @param df DLMtool Description data frame
  #' @param sect_name Name to use for the section
  #' @param prepend_name Characters to prepend to each slot name (for easy reading
  #'   in the document where slot names are the same among types).
  #' @param inst_obj_name The name to use for the instance to be referenced. e.g.
  #'   a Data object will be referenced by a variable called 'dat' probably and
  #'   Stock by 'stk'.
  #'
  #' @return The Rmd - formatted string
  #'
  #' @examples
  #' format_desc(DLMtool::DataDescription, "Data parameters", "Data", "dat")
  format_desc <- function(df,
                          obj_name = "Data",
                          inst_obj_name = tolower(obj_name)){

    df <- df %>%
      mutate(code = paste0("```{r, ",
                           inst_obj_name, "-", Slot,
                           ", results = FALSE}\n  ",
                           inst_obj_name, "@", Slot,
                           "\n```"),
             Slot = paste0("## ",
                           Slot,
                           "\n"),
             Description = paste0("*",
                                  Description,
                                  "*\n"))
    c(paste0("# **", obj_name, " slot descriptions**\n\n"),
      paste0("```{r warnings = FALSE}\n  ",
             inst_obj_name, " <- methods::new('", obj_name, "')\n```\n"),
      apply(df, 1, function(x) paste0(x, collapse = "\n\n")))
  }

  rmd <- c("```{r message = FALSE}\n  library(DLMtool)\n```\n",
           format_desc(DLMtool::DataDescription, "Data"),
           format_desc(DLMtool::StockDescription, "Stock"),
           format_desc(DLMtool::FleetDescription, "Fleet"),
           format_desc(DLMtool::ObsDescription, "Obs"),
           format_desc(DLMtool::ImpDescription, "Imp"),
           format_desc(DLMtool::OMDescription, "OM"))

  conn <- file(fn)
  write(rmd, conn)
  close(conn)
}
