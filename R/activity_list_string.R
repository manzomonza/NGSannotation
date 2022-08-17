#' Output single string of variants
#'
#' @param snv_prep 
#'
#' @return
#' @export
#'
#' @examples ALK: p.D1529E (56.1%); EGFR: c.1498+22A>T (47.8%);
snv_activityList_format <- function(snv_prep_filepath){
  snv_prep = readr::read_tsv(snv_prep_filepath)
  snv_prep = snv_prep %>% tibble::rowid_to_column()
  snv_prep = lapply(snv_prep$rowid, function(i) row_activityList_format(snv_prep[i,]))
  snv_prep_string = paste0(unlist(snv_prep), collapse = "; ")
  return(snv_prep_string)
}


#' Output concatenated string of gene, change and allele frequency
#'
#' @param snv_prep 
#'
#' @return Returns one single string
#' @export
#'
#' @examples ALK: p.D1529E (56.1%)
row_activityList_format <- function(snv_prep_row){
  if(snv_prep_row$multiply_freq_by_100){
    snv_prep_row$percent_frequency = snv_prep_row$percent_frequency*100
  }
  if(!is.na(snv_prep_row$amino_acid_change) & snv_prep_row$amino_acid_change != "p.?"){
    snv_prep_string = paste0(snv_prep_row$gene,": ", snv_prep_row$one_AA," (", snv_prep_row$percent_frequency, "%)")
  }else{
    snv_prep_string = paste0(snv_prep_row$gene,": ", snv_prep_row$coding," (", snv_prep_row$percent_frequency, "%)")
  }
  return(snv_prep_string)
}



