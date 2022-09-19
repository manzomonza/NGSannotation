## BIMI TABLE

#' Match Variant with BIMI table
#'
#' @param clinvar_annot
#'
#' @return
#' @export
#'
#' @examples
bimiMatchUp <- function(clinvar_annot){
    if(file.exists(BIMI_TABLE_PATH)){
      BIMI_TABLE = readr::read_tsv(BIMI_TABLE) %>%
        dplyr::rename(three_AA = amino_acid_change,
                      gene = genes)
      stopifnot(nrow(clinvar_annot) > 0)
        clinvar_annot <- clinvar_annot %>% ungroup()
        print("BIMI variant annotation table: loaded")
        clinvar_annot = dplyr::left_join(clinvar_annot, BIMI_TABLE, by = c("gene", "coding", "three_AA"))
        return(clinvar_annot)
    }else{
      print("BIMI table missing: skipping BIMI variant annotation")
      return(clinvar_annot)
    }
}
