#' Annotate CNVs
#'
#' @param variant_table
#'
#' @return
#' @export
#'
#' @examples
cnvAnnot <- function(variant_table){
  variant_table <- variant_table %>% tibble::rowid_to_column()
  ## Round copy_number to single digit when 0, otherwise 2-digit
  variant_table <- variant_table %>%
    dplyr::mutate(copy_number = ifelse(copy_number != 0, signif(copy_number, digits = 2), 0 ))

  ## Identify if CNV is an amplification or loss
  variant_table <- variant_table %>%
    dplyr::mutate(Diagnose_D = ifelse(fivePercent_conf >= 4, paste0(gene,": ","Hinweis auf eine Amplifikation (", copy_number, "x) auf Chromosom ", chromosome),
                                      ifelse(ninetyfivePercent_conf <= 1, paste0(gene,": ","Hinweis auf einen Verlust (", copy_number, "x) auf Chromosom ", chromosome), NA)),
                  Diagnose_F = ifelse(fivePercent_conf >= 4, paste0(gene,": ","indication d'amplification (", copy_number, "x) sur le chromosome ", chromosome),
                                      ifelse(ninetyfivePercent_conf <= 1, paste0(gene,": ","indication d'une perte (", copy_number, "x)  sur le chromosome ", chromosome), NA))
    )

  # variant_table <- variant_table %>%
  #   mutate(Link_Methodik = NA,
  #          Allelic_Frequency = NA,
  #          HGVS = NA,
  #          Transcript_ID = NA) %>%
  #   dplyr::select(gene, type, coding, amino_acid_change, IR_clinvar, Diagnose_D, Diagnose_F, Link_Methodik, Allelic_Frequency, HGVS, Transcript_ID)
  return(variant_table)
}





