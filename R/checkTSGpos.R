###

#' Check if Gene is TSG and if variant leads to Ter or frameshift within 90% of AA length
#'
#' @param gene
#' @param aa_pos
#' @param TSG_list
#'
#' @return 'likely pathogenic' if criteria are fullfilled, NA otherwise
#' @export
#'
#' @examples
checkTSG <- function(gene, aa_pos, TSG_list){
  if(gene %in% names(TSG_list) & !is.na(aa_pos)){
    if(aa_pos <= round(0.9 * TSG_list[[gene]])){
      interpretation = 'likely pathogenic'
      return(interpretation)
    }else{
      return(NA)
    }
  }else{
    return(NA)
  }
}


#' tsgParseTable Checks SNV contains 'fs' or 'Ter'
#'
#' @param snvtable
#'
#' @return snvtable with tsgInfo column added
#' @export
#'
#' @examples
tsgParseTable <- function(snvtable){
  if(nrow(snvtable) >0){
    snvtable$tsgInfo = NA
    for (i in 1:nrow(snvtable)){
      if(grepl("\\*|fs", snvtable$one_AA[i])){
        snvtable$tsgInfo[i] = checkTSG(gene = snvtable$genes[i],
                                       aa_pos = snvtable$aa_pos[i],
                                       TSG_list = tsg_ls)

      }
    }
    snvtable <- subset(snvtable, selec=-aa_pos)
    return(snvtable)
  }
}

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





