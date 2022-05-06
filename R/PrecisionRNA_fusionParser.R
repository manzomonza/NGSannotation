#' Parses Precision RNA Fusion.tsv
#'
#' @param filepath
#'
#' @return
#' @export
#'
#' @examples
fusionParser <- function(fusionpath){
  fusionParse = readr::read_tsv(fusionpath, skip_empty_rows = TRUE) %>%
    janitor::clean_names() %>%
    dplyr::filter(call == "PRESENT") %>%
    dplyr::filter(type != 'ProcControl')
  return(fusionParse)
}


#' Parse Fusion info
#'
#' @param infopath
#'
#' @return
#' @export
#'
#' @examples
fusionInfoParser <- function(infopath){
  info <- readr::read_csv(infopath) %>%
    dplyr::rename(metadata = 1) %>%
    tidyr::separate(col = metadata, into = c('parameter', 'value'), sep = ",",
                    extra = 'merge',
                    fill = "right") %>%
    dplyr::filter(grepl('Mapped|Mean Read Length|RNA Expression Ctrls', parameter, ignore.case = TRUE)) %>%
    tidyr::separate(col = value, into = c("value", "threshold", "qc"))
  return(info)
}


#' Generate Combined output file for Fusion.tsv
#'
#' @param fusionpath
#'
#' @return
#' @export
#'
#' @examples
FusionOutput <- function(fusionpath){
  dirname = gsub(".tsv", '_watchdog', fusionpath)
  print(dirname)
  ## create output dir
  dir.create(dirname)
  ## Original File
  orig = readr::read_csv(fusionpath) %>%
    dplyr::rename(metadata = 1) %>%
    tidyr::separate(col = metadata, into = c('parameter', 'value'), sep = ",",
                    extra = 'merge',
                    fill = "right")
  ## Info File
  infoPath = paste0(dirname(fusionpath), "/Info.csv")
  if (file.exists(infoPath)){
    infoFile = fusionInfoParser(infoPath)
  }
  ## Parsed Fusion file
  fusionParse = fusionParser(fusionpath)

  sampleName = infoName(infoPath)

  excel_file = list(orig, infoFile, fusionParse)
  names(excel_file) <- c("Fusion.tsv", "InfoParse", "FusionParse")
  writexl::write_xlsx(excel_file, path = paste0(dirname, "/",sampleName , "_GXS_RNA_combined_output.xlsx"))
}

