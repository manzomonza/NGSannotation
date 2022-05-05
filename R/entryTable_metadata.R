#' Collects Metadata and writes out table
#' This function extracts metadata stored in '##' commented lines from ThermoFisher IonReporter .tsv reports.
#' Is made to only work with '##' commented files/lines.
#'
#' @param filepath Absolute or relative path to IonReporter .tsv file
#' @return A table with one column, namely ## IR Workflow Metainformation, and '##' entries as rows
#' @export
#'
#' @examples
std_metadataCollector <- function(filepath){
  dirname = gsub(".tsv", '_watchdog', filepath)
  filename = paste0(dirname, "/Info.csv")
  metadat <- readr::read_tsv(filepath, col_names = F) %>%
    dplyr::filter(grepl("##", X1)) %>%
    dplyr::mutate(X1 = gsub("##", "", X1))
  colnames(metadat) = "IR_Workflow_Metainformation"
  write.table(metadat, file = filename)
  return(metadat)
}
