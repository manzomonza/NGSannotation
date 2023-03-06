# GENE_NAME_LUT = readr::read_csv("./double_gene_name/Double_gene_name_LUT.csv")

#' Substitute comma separated gene symbols with single gene name
#'
#' @param gene_symbol
#'
#' @return
#' @export
#'
#' @examples
#' LOC101927447,SCUBE1 becomes SCUBE1
gene_symbol_sub = function(gene_symbol){
  if(gene_symbol %in% GENE_NAME_LUT$gene){
    res = subset(GENE_NAME_LUT,  gene == gene_symbol)
    sub_gene = res$substitute_gene_symbol
    return(sub_gene)
  }else{
    return(gene_symbol)
  }
}


#' Substitute gene symbols
#'
#' @param snvtable
#'
#' @return
#' @export
#'
#' @examples
gene_symbol_sub_table = function(snvtable){
  snvtable$gene = sapply(snvtable$gene, gene_symbol_sub)
  return(snvtable)
}

