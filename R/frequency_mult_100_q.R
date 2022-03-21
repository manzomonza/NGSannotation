#' Multiply by 100 or not
#'
#' @param IRoutput 
#'
#' @return
#' @export
#'
#' @examples
mult100 <- function(IRoutput){
  if ("allele_frequency" %in% colnames(IRoutput) | "allele_fraction" %in% colnames(IRoutput)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
