#' Count variant occurences per tissue in COSMIC db
#'
#' @param gene Character string containing gene symbol
#' @param change Character string with coding or amino acid change
#' @param sql_con_tbl Established SQL connection to COSMIC db
#'
#' @return Returns list with total count and tissue specific counts
#' @export
#'
#' @examples GATA3 p.M294K returns list with
#' COSMIC_n_tissuebreast (43)
#' COSMIC_n_total 43
counter_cosmic_variant_tissue <- function(gene, change, sql_con_tbl){
  require(magrittr)
  if(is.na(sql_con_tbl[1])){
    cosmic_results = list(COSMIC_n_tissue = NA,
                          COSMIC_n_total = NA)
    return(cosmic_results)
  }
  gene = paste0("%", gene, "%")
  change = paste0("%", change, "%")

  ##
  variants_per_tissue = sql_con_tbl %>%
    dplyr::filter(gene_name %LIKE% gene)

  if(grepl("c.", change)){
  variants_per_tissue = variants_per_tissue %>%
    dplyr::filter(mutation_cds %LIKE% change) %>%
    dplyr::collect()
  }else if(grepl("p.", change)){
    variants_per_tissue = variants_per_tissue %>%
      dplyr::filter(mutation_aa %LIKE% change) %>%
      dplyr::collect()
  }else{
    cosmic_results = list(COSMIC_n_tissue = NA,
                          COSMIC_n_total = NA)
    return(cosmic_results)
  }
  count_variants_per_tissue <- variants_per_tissue %>%
      dplyr::count(primary_site, sort = TRUE)
  COSMIC_n_total = nrow(variants_per_tissue)
  if(COSMIC_n_total == 0){
    COSMIC_n_tissue = NA
  }else{
    COSMIC_n_tissue = paste0(count_variants_per_tissue$primary_site, " (",count_variants_per_tissue$n, ")")
  }
  cosmic_results = list(COSMIC_n_tissue = COSMIC_n_tissue,
                        COSMIC_n_total = COSMIC_n_total)
  return(cosmic_results)
}
