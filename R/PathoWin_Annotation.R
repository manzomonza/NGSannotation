
#' ## Prepare Diagnosis column in German and French to be copy/pasted to Pathowin
#' SNV annotation
#'
#' @param variant_tbl
#'
#' @return
#' @export
#'
#' @examples
diagnose_D_F_columns_snv <- function(variant_tbl){

  ## clinical significances translations
  variant_tbl <- variant_tbl %>%
    dplyr::rowwise() %>%
    dplyr::mutate(clinvar_deu =  ClassficationDe(clinical_significance),
                  clinvar_fra =  ClassficationFr(clinical_significance))

  ## Choosing coding or AA as 'insert'
  variant_tbl <- variant_tbl %>%
    dplyr::mutate(insert = ifelse(is.na(amino_acid_change) | amino_acid_change == "p.?", coding, one_AA))

  ## Selection of intron or exon into CDS position
  variant_tbl <- variant_tbl %>%
    dplyr::mutate(cds_D = ifelse(cds_region == "intron", "Intron", "Exon"),
                  cds_F = ifelse(cds_region == "intron", "intron", "exon"))

  ##
  variant_tbl <- variant_tbl %>%
    dplyr::mutate(percent_frequency = ifelse(multiply_freq_by_100, round(as.numeric(percent_frequency)*100, digits = 0), round(as.numeric(percent_frequency), digits = 0) ))

  ## Diagnose_D/Diagnose_F
  variant_tbl <- variant_tbl %>%
  dplyr::mutate(Diagnose_D = ifelse(is.na(exon),
                                    paste0(gene,": ", insert," (",percent_frequency,"%)"," Bewertung: ", clinvar_deu),
                                    paste0(gene,": ", insert," (",percent_frequency,"%)", " im ", cds_D, " ", cds_region_No, ", Bewertung: ", clinvar_deu)),
                Diagnose_F = ifelse(is.na(exon),
                                    paste0(gene,": ", insert," (",percent_frequency,"%)"," signification clinique: ", clinvar_fra),
                                    paste0(gene,": ", insert," (",percent_frequency,"%)", " au niveau de l'", cds_F, " ", cds_region_No, ", signification clinique: ", clinvar_fra)))


  variant_tbl <- variant_tbl %>%
    dplyr::mutate(clinvar_link = ifelse(is.na(variation_id), '', paste0("https://www.ncbi.nlm.nih.gov/clinvar/variation/", variation_id, "/")) )

  ## HGVS column
  variant_tbl <- variant_tbl %>%
    dplyr::mutate(Link_Methodik = ifelse(coding == insert,
                                         ifelse(clinvar_deu != "unbekannt",
                                                paste0(transcript," (", gene,"): ", coding," ", clinvar_link),''),
                                         ifelse(clinvar_deu != "unbekannt",
                                                paste0(transcript," (", gene,"): ", coding," (",one_AA,")" ," ", clinvar_link),'')) ) %>%
    dplyr::mutate(Link_Methodik = ifelse(is.na(transcript), '', Link_Methodik ))

  variant_tbl <- variant_tbl %>%
    dplyr::mutate(HGVS = ifelse(coding == insert, paste0(transcript," (", gene,"): ", coding),
                         paste0(transcript," (", gene,"): ", coding," (",one_AA,")")),
           Allelic_Frequency = percent_frequency) %>%
    dplyr::mutate(HGVS = ifelse(is.na(transcript), '', HGVS ))

  selection_tbl <- variant_tbl %>%
    dplyr::rename(Transcript_ID = transcript) %>%
    dplyr::select(Diagnose_D, Diagnose_F, Allelic_Frequency, BIMI_variant, Kommentar,IR_clinvar, Link_Methodik, gene, coding, three_AA, one_AA, clinvar_ready_AA, mtbp ) %>%
    dplyr::rename(amino_acid_change = three_AA)

  selection_tbl <- selection_tbl %>%
    dplyr::mutate(order = orderSignificance(Diagnose_D)) %>%
    dplyr::arrange(order) %>%
    dplyr::select(-order)

  return(selection_tbl)
}

