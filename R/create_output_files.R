#' #### Function to write out tables create_output_files
#' Generates various output files from IonReporter and Genexus files, respectively
#'
#' @param list_of_tables
#' @param filepath
#' @example

create_output_files <- function(list_of_tables, filepath){
  dirname = gsub(".tsv", '_watchdog', filepath)
  print(dirname)

  snv = list_of_tables$snv
  cnv = list_of_tables$cnv
  filtered = list_of_tables$filtered
  #info = list_of_tables$info

  # if (file.exists(dirname)) {
  #   cat("Watchdog folder already exists")
  #
  # } else {
  dir.create(dirname)
  if(is_tibble(snv)){
    snv = mtbpFormat(snvTable = snv)
    readr::write_tsv(snv, file = paste0(dirname, "/prep_snv.txt"))
  }
  if(is_tibble(cnv)){
    readr::write_tsv(cnv, file = paste0(dirname, "/prep_cnv.txt"))
  }
  if(is_tibble(filtered)){
    readr::write_tsv(filtered, file = paste0(dirname, "/prep_filtered.txt"))
  }
  # if(is_tibble(info)){
  #   write_tsv(info, file = paste0(dirname, "/prep_info.txt"))
  # }
  }
