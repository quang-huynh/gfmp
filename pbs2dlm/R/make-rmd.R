#' Create .rmd file describing DLMtool Objects and Slots and inject custom descriptions into it
#'
#' @param file_name Filename/path of the .rmd file to create/modify. If it does not exist,
#'  it will be created using create_default_rmd()
#' @param cust_desc_file_name Filename/path of the .csv file containing the custom descriptions.
#'  Use generate_default_custom_descriptions_file() in scratch.R to auto-generate it.
#'
#' @return Nothing
#' @export
#'
#' @examples
#' createt_rmd(here::here("report/om-specification.rmd"))
create_rmd <- function(file_name,
                       cust_desc_file_name = here::here("pbs2dlm/inst/alt-slot-descriptions.csv"),
                       ...){

  if (!file.exists(file_name)){
    create_default_rmd(file_name, ...)
  }

  cust_desc <- readr::read_csv(cust_desc_file_name)
  cust_desc[,c(1,2)] <- apply(cust_desc[,c(1,2)], c(1,2), tolower)

  rmd <- readLines(file_name)
  beg <- grep("<!-- autogen-begin -->", rmd)
  end <- grep("<!-- autogen-end -->", rmd)
  if(length(beg) != length(end)){
    stop("Error - mismatch between number of autogen start tags (", length(beg), ") and ",
         "end tags (", length(end), ").\nLine numbers for start tags are:\n", paste(beg, collapse = " "),
         "\nLine numbers for end tags are:\n", paste(end, collapse = " "), "\n")
  }
  lapply(seq_along(beg), function(y){
    j <- rmd[beg[y]:end[y]]
    k <- stringr::str_split(regmatches(j, regexpr("(?<=desc-)[\\w-]+(?=\\})", j, perl = TRUE)), "-")[[1]]

    kk <- cust_desc %>%
      dplyr::filter(slot_type == k[1]) %>%
      dplyr::filter(slot == k[2])
    if(nrow(kk) != 1){
      stop("Error trying to find slot_type '",
           k[1], "', slot '", k[2], "' in the descriptions file: ",
           cust_desc_fn)
    }
    if(kk$use_custom_description){
      val <- grep("^\\*.*\\*$", j)
      if(!length(val)){
        stop("Error trying to find the description inside autogen chunk. Note it needs to start and end with an asterisk:\n",
             paste0(j, collapse = "\n"))
      }
      if(length(val) > 1){
        stop("Error - more than one line matches as a description inside autogen chunk:\n",
             paste0(j, collapse = "\n"))
      }
      j[val] <- paste0("*", kk$custom_description, "*")
      rmd[beg[y]:end[y]] <<- j
    }
  })

  conn <- file(file_name)
  write(rmd, conn)
  close(conn)
}

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
#' create_rmd(here::here("report/om-specification.rmd"))
create_default_rmd <- function(file_name, overwrite = FALSE,
  knitr_results = TRUE, knitr_echo = TRUE) {
  if (file.exists(file_name) && !overwrite)
    stop("File '", file_name, "' already exists. ",
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

  conn <- file(file_name)
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
    dplyr::mutate(
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
