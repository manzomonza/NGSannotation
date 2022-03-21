#' Frameshift clinvar fix
#'
#' @param amino_acid_change 
#'
#' @return
#' @export
#'
#' @examples 
#' removes 'stop after how many amino-acids' information, e.g.
#' # p.Ser1465ArgfsTer3 to p.Ser1465fs
fsClinvarfix <- function(amino_acid_change){
  # Is fs present 
  if( !is.null(amino_acid_change)){
    if(!is.na(amino_acid_change) & !grepl("p\\.\\?", amino_acid_change) ){
      if(grepl("fs", amino_acid_change,fixed = TRUE)){
        split_string = stringr::str_split(amino_acid_change, pattern = "\\d{1,}", simplify = TRUE)[,1]
        digit_split = stringr::str_extract(amino_acid_change, pattern = "\\d{1,}")
        fs_string = paste0(split_string, digit_split, "fs")
        return(fs_string)
        }else{
          return(amino_acid_change)
          }
      }else{
        return(NA)
      }
    }else{
      return(NA)
    }
}
