### Check if Gene is TSG and if mutation is before last 10% of AA length

#' Checks if gene is part of TSG and checks position
#'
#' @param gene
#' @param aa_pos
#' @param TSG_list
#'
#' @return 'pathogenic' if criteria are fullfilled, NA otherwise
#' @export
#'
#' @examples
checkTSG <- function(gene, aa_pos, TSG_list){
  if(gene %in% names(TSG_list) & !is.na(aa_pos)){
    if(aa_pos <= round(0.9 * TSG_list[[gene]])){
      interpretation = 'pathogenic'
      return(interpretation)
    }else{
      return(NA)
      }
  }else{
      return(NA)
    }
}


#' tsgParseTable Parse SNV which went through 'watchdog annotation' step
#'
#' @param snvtable
#'
#' @return snvtable with tsgInfo column added
#' @export
#'
#' @examples
tsgParseTable <- function(snvtable){
  if(nrow(snvtable) >0){
    snvtable$aa_pos = as.numeric(sapply(snvtable$one_AA, function(x) stringr::str_remove_all(string = x, pattern = "\\D")))
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




