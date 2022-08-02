#' Retrieve if variant codon position is covered in cancerHotspots (v2)
#'
#' @param gene
#' @param snv_position
#'
#' @return
#' @export
#'
#' @examples
#' Retrieves No. of total entries in CANCER_HOTSPOTS
#' KRAS p.G12C retrieves n_total == 2175
info_cancerHotspots <- function(geneName, snv_position){
  if(geneName %in% CANCER_HOTSPOTS$gene){
    ch = CANCER_HOTSPOTS %>% dplyr::filter(gene == geneName)
    if(snv_position %in% ch$Amino_Acid_Position){
      ch = ch %>% dplyr::filter(Amino_Acid_Position == snv_position)
      ch = ch %>% dplyr::pull(n_total)
    return(ch)
    }else{
      return(NA)
    }
  }else{
    return(NA)
  }
}

#' Apply cancerhotspot check to single row
#'
#' @param variantRow
#'
#' @return
#' @export
#'
#' @examples
wrapper_row_cancerHotspots <- function(variantRow){
  variantRow$aa_position = extract_snv_position(variantRow$one_AA)
  variantRow$cancerHotspot = info_cancerHotspots(geneName = variantRow$gene,
                                                 snv_position = variantRow$aa_position)
  return(variantRow)
}

#' Apply cancerhotspot check to snv_variant table
#'
#' @param variantTable
#'
#' @return
#' @export
#'
#' @examples
wrapper_table_cancerHotspots <- function(variantTable){
  variantTable = variantTable %>% tibble::rowid_to_column()
  variantTable = lapply(variantTable$rowid, function(x) wrapper_row_cancerHotspots(variantTable[x,]))
  variantTable = dplyr::bind_rows(variantTable)
  return(variantTable)
}



