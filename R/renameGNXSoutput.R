#' Extract Sample name information from Genexus/Precision Info.csv
#'
#' @param filepath
#'
#' @return
#' @export
#'
#' @examples
infoName <- function(Infofilepath){
  info_csv = readr::read_csv(Infofilepath)
  sampleName =  info_csv %>%
    dplyr::rename(metadata = 1) %>%
    tidyr::separate(col = metadata, into = c('parameter', 'value'), sep = ",", fill = "right") %>%
    dplyr::filter(grepl('Sample', parameter, ignore.case = TRUE)) %>%
    dplyr::filter(parameter == "Sample Name" & value != "Assay Name") %>%
    dplyr::pull(value)
  return(sampleName)
}
