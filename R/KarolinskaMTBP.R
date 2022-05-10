#' Ouput Karolinska MTBP compliant output
#'
#' @param snvTable
#'
#' @return
#' @export
#'
#' @examples
mtbpFormat <- function(snvTable){
  snvTable = snvTable %>%
    dplyr::mutate(mtbp = paste0(genes,':', coding))
  return(snvTable)
}
