#' Extract position of SNVs
#'
#' @param clinvar_ready_AA
#'
#' @return SNV amino-acid position
#' @export
#'
#' @examples
extract_snv_position <- function(aa_change){
  if(is.character(aa_change) &  !is.na(aa_change)){
    aa_pos = stringr::str_extract(string = aa_change, pattern = "(?<=\\D)\\d+")
    aa_pos = as.integer(aa_pos)
    return(aa_pos)
  }else{
    return(NA)

  }
}




