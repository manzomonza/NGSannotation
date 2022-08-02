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
    bimi_tbl = "/home/ionadmin/ngs_variant_annotation/variantAnnotation/NGS_mutation_list/BIMI_Variant_table.tsv"
    #bimi_tbl = "/Volumes/GoogleDrive/.shortcut-targets-by-id/1yuFiN1dlcUgo1_ELdNVXegTfB61oDv8G/Patientendaten/Development/tables/BIMI_Variant_table.tsv"
    if(file.exists(bimi_tbl)){
      bimi_tbl = readr::read_tsv(bimi_tbl) %>%
        dplyr::rename(three_AA = amino_acid_change,
                      gene = genes)
      stopifnot(nrow(clinvar_annot) > 0)
        clinvar_annot <- clinvar_annot %>% ungroup()
        print("BIMI variant annotation table: loaded")
        clinvar_annot = dplyr::left_join(clinvar_annot, bimi_tbl, by = c("gene", "coding", "three_AA"))
        return(clinvar_annot)
    }else{
      print("BIMI table missing: skipping BIMI variant annotation")
      return(clinvar_annot)
    }
}
