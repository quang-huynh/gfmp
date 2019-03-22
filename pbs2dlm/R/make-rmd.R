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
                          sect_name = "Default",
                          prepend_name = "",
                          inst_obj_name = "dat"){
    df <- df %>%
      mutate(code = paste0("```{r results = FALSE}\n  ",
                           inst_obj_name,
                           "@",
                           Slot,
                           "\n```"),
             Slot = paste0("## ",
                           prepend_name,
                           ifelse(prepend_name == "", "", " - "),
                           Slot,
                           "\n"),
             Description = paste0("*",
                                  Description,
                                  "*\n"))
    c(paste0("# **", sect_name, "**\n\n"),
      apply(df, 1, function(x) paste0(x, collapse = "\n\n")))
  }

  rmd <- c("```{r message = FALSE}\n  library(DLMtool)\n  dat <- methods::new(\"Data\")\n```",
           format_desc(DLMtool::DataDescription, "Data slots", "", "dat"),
           "```{r}\n  stk <- methods::new(\"Stock\")\n```",
           format_desc(DLMtool::StockDescription, "Stock slots", "", "stk"),
           "```{r}\n  flt <- methods::new(\"Fleet\")\n```",
           format_desc(DLMtool::FleetDescription, "Fleet slots", "", "flt"),
           "```{r}\n  obs <- methods::new(\"Obs\")\n```",
           format_desc(DLMtool::ObsDescription, "Obs slots", "", "obs"),
           "```{r}\n  imp <- methods::new(\"Imp\")\n```",
           format_desc(DLMtool::ImpDescription, "Imp slots", "", "imp"),
           "```{r}\n  om <- methods::new(\"OM\")\n```",
           format_desc(DLMtool::OMDescription, "OM slots", "", "om"))

  conn <- file(fn)
  write(rmd, conn)
  close(conn)
}
