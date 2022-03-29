#' Shorten filename
#'
#' @param filepath
#'
#' @return shortened filename
#' @export
#'
#' @examples
#' W601.B022_v1_44534543535435345.tsv becomes 601.B022
#' W775_44534543535435345.tsv becomes 601.B022
#' W775.44534_543535435345.tsv becomes 601.B022
shortenFilename <- function(filepath){
  filename = stringr::str_remove(filepath, pattern = "-.+-.+-.+")
  filename = gsub("\\.tsv$", '', filename)
  return(filename)
}
