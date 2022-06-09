#' Generates output tables for precision
#'
#' @param snv_indel table containing indels and snvs
#' @param cnv table containing cnvs
#'
#' @return
#' @export
#'
#' @examples
make_output_tables_precision <- function(snv_indel, cnv){

  ## Filter for SNV entries
  snv = snv_indel %>%
    dplyr::filter(!is.na(coding) & !is.na(amino_acid_change)) %>%
    dplyr::filter(!grepl("cnv",type, ignore.case = TRUE)) %>%
    dplyr::filter(!grepl(",", gene)) %>%
    snvParse()

  filtered <- snv_indel %>%
    dplyr::filter(is.na(gene) | grepl(",", gene) | is.na(coding) & is.na(amino_acid_change))

  ## Edit AA changes
  snv$three_AA <- sapply(snv$amino_acid_change, amino_acid_conversion_one_to_three )
  snv$one_AA <-   sapply(snv$amino_acid_change, amino_acid_conversion_three_to_one )
  snv$one_AA <- gsub("Ter", "\\*", snv$one_AA)
  snv$clinvar_ready_AA <- sapply(snv$three_AA, fsClinvarfix)

  snv <- snv %>%
    dplyr::select(-contains("cnv_confidence"), -contains("copy_number")) %>%
    dplyr::mutate(percent_frequency = as.numeric(percent_frequency))

  ## Filter for CNV entries
  cnv = cnv %>%
    dplyr::filter(!is.na(gene)) %>%
    dplyr::select(gene, copy_number, cnv_confidence, locus) %>%
    cnvParse()

  table_ls <- list(snv = snv,
                   cnv = cnv,
                   filtered = filtered)
  return(table_ls)
}
