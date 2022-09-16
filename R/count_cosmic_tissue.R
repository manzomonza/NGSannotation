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
#' COSMIC_n_tissue breast (43)
#' COSMIC_n_total 43
cosmic_sql_search <- function(gene, change, sql_con_tbl){
  stopifnot(grepl("^c.", change) | grepl("^p.", change))

  #gene = paste0("%", gene, "%")
  variants_per_tissue = sql_con_tbl %>%
    dplyr::filter(gene_name == gene)

  if(grepl("^c.", change)){
  variants_per_tissue = variants_per_tissue %>%
    dplyr::filter(mutation_cds == change) %>%
    dplyr::collect() %>%
    dplyr::distinct()
  }else if(grepl("^p.", change)){
    variants_per_tissue = variants_per_tissue %>%
      dplyr::filter(mutation_aa == change) %>%
      dplyr::collect() %>%
      dplyr::distinct()
  }
  return(variants_per_tissue)
}

#' Count variant occurences per tissue in COSMIC db#'
#' @param variants_per_tissue
#'
#' @return
#' @export
#'
#' @examples
counter_cosmic_variant_tissue <- function(variants_per_tissue){
  if(nrow(variants_per_tissue) == 0){
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
    COSMIC_n_tissue = paste0(paste0(count_variants_per_tissue$primary_site, " (", count_variants_per_tissue$n, ")"), collapse = "; ")
  }
  cosmic_results = list(COSMIC_n_tissue = COSMIC_n_tissue,
                        COSMIC_n_total = COSMIC_n_total)
  return(cosmic_results)
}

#' Applies counter_cosmic_variant_tissue rowwise to SNV table
#'
#' @param table_element Row of SNV table
#'
#' @return List of counter_cosmic_variant_tissue results (COSMIC_n_total, COSMIC_n_tissue)
#' @export
#'
#' @examples
cosmic_counter_per_table_element <- function(table_element){
  if(is.na(table_element$amino_acid_change) | table_element$amino_acid_change == "p.?"){
    var_per_tissue = cosmic_sql_search(gene = table_element$gene,
                                                change = table_element$coding,
                                                sql_con_tbl = CON_TBL)
  }else{
    var_per_tissue = cosmic_sql_search(gene = table_element$gene,
                                                change = table_element$one_AA,
                                                sql_con_tbl = CON_TBL)
  }
  count_cosmic_tissue = counter_cosmic_variant_tissue(var_per_tissue)
  return(count_cosmic_tissue)
}

#' Wrapper function for COSMIC tissue count
#'
#' @param snv_table Unedited watchdog prep_snv table
#'
#' @return snv_table with two additional columns: COSMIC_n_total and COSMIC_n_tissue
#' @export
#'
#' @examples
#' prep_snv.txt
cosmic_counter_wrapper <- function(snv_table){
  snv_table = snv_table %>% tibble::rowid_to_column()
  snv_table$COSMIC_n_total = NA
  snv_table$COSMIC_n_tissue = NA
  cosmic_count_results = lapply(snv_table$rowid, function(x)  cosmic_counter_per_table_element(snv_table[x,]))

  for (row_index in snv_table$rowid){
    if(is.na(cosmic_count_results[[row_index]]$COSMIC_n_total)){
      snv_table$COSMIC_n_total[row_index] = NA
      snv_table$COSMIC_n_tissue[row_index] = NA
    }else{
      snv_table$COSMIC_n_total[row_index] = cosmic_count_results[[row_index]]$COSMIC_n_total
      snv_table$COSMIC_n_tissue[row_index] = cosmic_count_results[[row_index]]$COSMIC_n_tissue
    }
  }
  return(snv_table)
}
