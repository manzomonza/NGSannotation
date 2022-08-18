#' From filepath to watchdog/prep filepaths
#'
#' @param filepath 
#'
#' @return
#' @export
#'
#' @examples list(prep_snv_filepath, prep_cnv_filepath)
prep_filepaths <- function(filepath){
  dir_path = gsub(".tsv", '_watchdog', filepath)
  snv = paste0(dir_path, "/prep_snv.txt")
  cnv = paste0(dir_path, "/prep_cnv.txt")
  filepaths = list(prep_snv = snv, prep_cnv = cnv)
  return(filepaths)
}


