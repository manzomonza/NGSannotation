

#' ### ExonAnnot
#'
#' @param mutation_table 
#'
#' @return
#' @export
#'
#' @examples
exonAnnot <- function(mutation_table){
  
  ## Generate iterable id
  mutation_table <- mutation_table %>% rowid_to_column()
  
  ## Convert locus to GenomicRange and extract Exon information when locus overlaps exon
  mutation.gr = lapply(mutation_table$rowid, function(x)  subsetByOverlaps(exon.gr, GRanges(mutation_table[x,]$locus)) %>%
                         as_tibble() %>%
                         dplyr::select(hg19_ref_gene_name2, hg19_ref_gene_name_hg_fixed_gb_cdna_info_version, exonNo ) %>%
                         dplyr::rename(transcript = hg19_ref_gene_name_hg_fixed_gb_cdna_info_version,
                                       gene = hg19_ref_gene_name2,
                                       exon = exonNo) %>%
                         dplyr::mutate(rowid = x)
  )
  ## if no GRanges overlap is found, integer(0) is returned for 'pull(exonNo)'. Therefore those positions get replaced with NA
  # mutation.gr$exonNo[unlist(lapply(mutation.gr$exonNo, function(x) identical(x, integer(0))))] <- NA
  # ## Add exonNo column to mutation_table
  # mutation_table$exon <- unlist(mutation.gr$exonNo)
  mutation_table <- left_join(mutation_table, bind_rows(mutation.gr), by = c("rowid","gene")) %>%
    dplyr::relocate(transcript, exon) %>%
    dplyr::mutate(clin_var = NA) %>%
    dplyr::select(-rowid)
    #print("left joined")
    return(mutation_table)
}
