#' Renaming of inconsistently named columns
#'
#' @param IRoutput
#'
#' @return
#' @export
#'
#' @examples
renameCol <- function(IRoutput){

  # Allele Frequency
  if ("allele_frequency_percent" %in% colnames(IRoutput)){
    IRoutput <- IRoutput %>% dplyr::rename(percent_frequency = allele_frequency_percent)
  }else if("allele_fraction" %in% colnames(IRoutput)){
    IRoutput <- IRoutput %>% dplyr::rename(percent_frequency = allele_fraction)
  }else if ("allele_frequency" %in% colnames(IRoutput)){
    IRoutput <- IRoutput %>% dplyr::rename(percent_frequency = allele_frequency)
  }

  # Amino acid change
  if ("aa_change" %in% colnames(IRoutput)){
    IRoutput <- IRoutput %>% dplyr::rename(amino_acid_change = aa_change)
  }

  if ("protein" %in% colnames(IRoutput)){
    IRoutput <- IRoutput %>% dplyr::rename(amino_acid_change = protein)
  }

  # coding change
  if ("nucleotide_change" %in% colnames(IRoutput)){
    IRoutput <- IRoutput %>% dplyr::rename(coding = nucleotide_change)
  }

  # Gene column
  if ("genes_exons" %in% colnames(IRoutput) & "genes" %in% colnames(IRoutput)){
    IRoutput <- IRoutput %>%
      dplyr::select(-genes)
  }
  # Gene column
  if ("genes_exons" %in% colnames(IRoutput)){
    IRoutput <- IRoutput %>% dplyr::rename(gene = genes_exons)
  }

  if ("genes" %in% colnames(IRoutput)){
    IRoutput <- IRoutput %>% dplyr::rename(gene = genes)
  }

  # Locus
  if ("number_locus" %in% colnames(IRoutput)){
    IRoutput <- IRoutput %>% dplyr::rename(locus = number_locus)
  }
  # Clinvar
  if ("clin_var" %in% colnames(IRoutput)){
    IRoutput <- IRoutput %>% dplyr::rename(IR_clinvar = clin_var)
  }

  return(IRoutput)
}
