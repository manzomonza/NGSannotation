#' Shorten filename
#'
#' @param filepath
#'
#' @return shortened filename
#' @export
#'
#' @examples
#' W601.B022_v1_44534543535435345.tsv becomes 601.B022
shortenFilename <- function(filepath){
  if(grepl("v1", filepath)){
    filename = stringr::str_remove(filepath, pattern = "v1_.*.tsv")
  }else{
  filename = stringr::str_remove(filepath, pattern = "\\d{6,}.*.tsv")
  }
  filename = gsub("_+", '', filename)
  filename = paste0(filename, ".tsv")
  return(filename)
}

