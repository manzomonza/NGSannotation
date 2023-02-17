#' read in, rename and select
#'
#' @param filepath
#'
#' @return
#' @export
#'
#' @examples
read_rename_select <- function(filepath){

  frequency_col_mult100 = readIn(filepath) %>%
    mult100()
  #print(paste0("Multiple by 100?: ", frequency_col_mult100))
  ir_output <- readIn(filepath) %>%
    renameCol() %>%
    gene_symbol_sub_table() %>%
    addMissingCols() %>%
    dplyr::select(type, gene, coding, amino_acid_change, percent_frequency, location,
                  transcript, exon, locus, copy_number, cnv_confidence, IR_clinvar) %>%
    splicesite_Annot() %>%
    #snvParse() %>%
    dplyr::mutate(multiply_freq_by_100 = frequency_col_mult100)
  return(ir_output)
}


#' Read rename select for GNXS files
#'
#' @param filepath
#'
#' @return
#' @export
#'
#' @examples
read_rename_select_precision_snv <- function(filepath){
  frequency_col_mult100 = readIn(filepath) %>%
    mult100()
  snvindelx <- readIn(filepath) %>%
    addMissingCols() %>%
    renameCol() %>%
    gene_symbol_sub_table() %>%
    snvParse() %>%
    dplyr::ungroup() %>%
    dplyr::select(-transcript) %>%
    dplyr::filter(grepl("PRESENT", call)) %>%
    dplyr::mutate(multiply_freq_by_100 = frequency_col_mult100) %>%
    dplyr::mutate(percent_frequency = as.numeric(percent_frequency))

    snvindelx <- exonAnnot(snvindelx) %>%
      dplyr::select(type, gene, coding, amino_acid_change, percent_frequency, location,
                    locus, transcript, exon, copy_number, cnv_confidence, IR_clinvar, multiply_freq_by_100)
    return(snvindelx)
  }
