#' Output single string of variants from  detected SNVs and CNVs
#'
#' @param snv_prep
#'
#' @return
#' @export
#'
#' @examples ALK: p.D1529E (56.1%); EGFR: c.1498+22A>T (47.8%);
activityList_format <- function(snv_prep_filepath, cnv_prep_filepath){
  ## SNV STRING
  snv_prep = readr::read_tsv(snv_prep_filepath)
  snv_prep = snv_prep %>% tibble::rowid_to_column()
  snv_prep = lapply(snv_prep$rowid, function(i) row_activityList_format_snv(snv_prep[i,]))
  snv_prep_string = paste0(unlist(snv_prep), collapse = "; ")

  ### CNV STRING
  cnv_prep = readr::read_tsv(cnv_prep_filepath)
  cnv_prep = cnv_prep %>% tibble::rowid_to_column()
  cnv_prep = lapply(cnv_prep$rowid, function(i) row_activityList_format_cnv(cnv_prep[i,]))
  cnv_prep = cnv_prep[!is.na(cnv_prep)]
  cnv_prep_string = paste0(unlist(cnv_prep), collapse = "; ")
  concatenated_string = paste0(snv_prep_string,"; ", cnv_prep_string)
  concatenated_string = gsub("; $",'', concatenated_string)

  return(concatenated_string)
}


#' Output concatenated string of gene, change and allele frequency
#'
#' @param snv_prep
#'
#' @return Returns one single string
#' @export
#'
#' @examples ALK: p.D1529E (56.1%)
row_activityList_format_snv <- function(snv_prep_row){
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


#' Output concatenated string of gene, copy number and chromosome
#'
#' @param cnv_prep
#'
#' @return Returns one single string of all detected CNV copy numbers
#' @export
#'
#' @examples BRAF: 10x (chr7)
row_activityList_format_cnv <- function(cnv_prep_row){
  if(is.na(cnv_prep_row$fivePercent_conf) & is.na(cnv_prep_row$ninetyfivePercent_conf)){
    cnv_string = NA
  }else if(cnv_prep_row$fivePercent_conf >=4 | cnv_prep_row$ninetyfivePercent_conf <= 1){
    cnv_string = paste0(cnv_prep_row$gene, ": ", cnv_prep_row$copy_number,"x ","(chr", cnv_prep_row$chromosome,")")
  }else{
    cnv_string = NA
    }
  return(cnv_string)
}





