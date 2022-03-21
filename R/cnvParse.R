#' Separates CNV confidence column in non-precision panels
#'
#' Tidy::separates cnv confidence column into two columns.
#' Bottom 5% and top95% conf. int. limits.
#'
#'
#' @param cnv_table
#'
#' @return Mutation table CNV confidence column edited
#' @export
#'
#' @examples
cnvParse <- function(cnv_table){
  cnv_table <- cnv_table %>%
    tidyr::separate(cnv_confidence, into = c("five","ninetyfive"), sep = ",") %>%
    dplyr::mutate(fivePercent_conf = as.numeric(gsub("5%:",'', five)),
           ninetyfivePercent_conf = as.numeric(gsub("95%:",'', ninetyfive)),
           copy_number = as.numeric(copy_number),
           chromosome = gsub("chr",'', str_remove(locus, ":\\d{1,}")))

  return(cnv_table)
}

