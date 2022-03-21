#' SNV PARSE
#' Takes SNV part of IR output and converts entry with comma to long format.
#' This procedure is necessary to only parse one entry at a time for all downstream processes
#' @param snv
#'
#' @return Edited SNV
#' @export
#'
#' @examples
snvParse <- function(snv){
  snv <- snv %>%
    dplyr::mutate(gene  = (str_split(gene, pattern = "\\, ", simplify = F)),
           coding  = (str_split(coding, pattern = "\\, ", simplify = F)),
           amino_acid_change  = (str_split(amino_acid_change, pattern = "\\, |;", simplify = F)),
           transcript  = (str_split(transcript, pattern = "\\, ", simplify = F)),
           percent_frequency  = (str_split(percent_frequency, pattern = "\\, ", simplify = F))) %>%
    tidyr::unchop(cols = gene) %>%
    tidyr::unchop(cols = coding) %>%
    tidyr::unchop(cols = amino_acid_change) %>%
    tidyr::unchop(cols = transcript) %>%
    tidyr::unchop(cols = percent_frequency) %>%
    dplyr::mutate(percent_frequency = percentParse(percent_frequency)) %>%
    dplyr::group_by(gene, transcript, type, coding) %>%
    dplyr::arrange(gene) %>%
    dplyr::filter(!grepl("=",amino_acid_change)) %>%
    ## Caution: just takes highest observed frequency for given mutation
    dplyr::slice(which.max(percent_frequency))
  return(snv)
}
