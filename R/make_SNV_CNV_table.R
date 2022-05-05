#' Generate tables to export
#'
#' @param ir_output
#'
#' @return
#' @export
#'
#' @examples
make_output_tables <- function(ir_output){

  ## Filter for SNV entries
  snv = ir_output %>%
    dplyr::filter(!is.na(coding) & !is.na(amino_acid_change)) %>%
    dplyr::filter(!grepl("cnv", type, ignore.case = TRUE)) %>%
    dplyr::filter(!grepl(",", gene)) %>%
    snvParse()

  filtered <- ir_output %>%
    dplyr::filter(is.na(gene) | grepl(",", gene) | is.na(coding) & is.na(amino_acid_change) & type != "CNV")

  ## Edit AA changes
  snv$three_AA <- sapply(snv$amino_acid_change, amino_acid_conversion_one_to_three )
  snv$one_AA <-   sapply(snv$amino_acid_change, amino_acid_conversion_three_to_one )
  snv$clinvar_ready_AA <- sapply(snv$three_AA, fsClinvarfix)

  snv <- snv %>%
    dplyr::select(-cnv_confidence, -copy_number)

  ## Filter for CNV entries
  cnv = ir_output %>%
    dplyr::filter(grepl("cnv", type, ignore.case = TRUE)) %>%
    dplyr::filter(!is.na(gene)) %>%
    dplyr::select(gene, copy_number, cnv_confidence, locus)

  cnv = cnv %>%
    cnvParse()
  table_ls <- list(snv = snv,
                   cnv = cnv,
                   filtered = filtered )
  return(table_ls)
}

