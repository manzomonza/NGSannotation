#' Allele column check
#'
#' checks name column to potentially multiply by or not 
#'
#' @param variant_table
#'
#' @return Returns boolean or NA based on allele column name.
#' @export
#'
#' @examples
allelecolumnCheck <- function(variant_table){
  if('allele_fraction' %in% colnames(variant_table)){
    return(TRUE)
  }else if('allele_frequency' %in% colnames(variant_table)){
    return(FALSE)
  }else{
    return(NA)
  }
}
