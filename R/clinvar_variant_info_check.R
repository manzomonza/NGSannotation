
## PROBLEM -- some mutations may be missed in the way the clinvar function checks for mutations

## Approach -- check coding first, then amino acid,
## if coding is not found,
## also check for amino acid change

clinvar_summary_filepath = "/home/ionadmin/ngs_variant_annotation/variantAnnotation/clinvar/variant_summary.txt.gz"

################################################# Check for presence of Clinvar variant summary file (and it being up-to-date)   #################################################

################################################# )   #################################################
#' Check, download and update Clinvar Summary Table (from NIH)
#' Checks for presence of Clinvar variant summary file (and it being up-to-date).
#' If not present (or up-to date, downloads from NIH at absolute path)
#' Caveat: Absolute paths are specified
#' @param clinvar_summary_filepath Absolute filepath of clinvar summary file
#' @return Boolean indicating if downloaded file has same MD5checksum as MD5checksum on NIH website
#'
#' @export
#'
#' @examples
clinvarCheck <- function(){
  ## check if new variant summary tables were created
  # clinvarDir <- getURL("https://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/",
  #                      verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
  #
  # return(getHTMLLinks(clinvarDir))
  ## new solution
  ## Check if MD5 sum of local variant_summary.txt.gz matches the current online one
  if(!file.exists(clinvar_summary_filepath)){
    download.file("https://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/variant_summary.txt.gz",
                  destfile=clinvar_summary_filepath)
  }
  md5check <- md5sum(clinvar_summary_filepath) ==
    strsplit(getURL("https://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/variant_summary.txt.gz.md5"), split = " ")[[1]][1]

  return(md5check)
}





#' Edit single variant
#'
#' @param srvt
#'
#' @return
#' @export
#'
#' @examples
clinvarSingleRow <- function(srvt){
  if(srvt$clinvar_ready_AA != "p.?" & !is.na(srvt$clinvar_ready_AA)){
    clinvar_hits <- clinvar %>%
      dplyr::filter(gene_symbol %in% srvt$gene) %>%
      dplyr::filter(grepl(srvt$coding, name, fixed = TRUE)) %>%
      dplyr::filter(grepl(srvt$clinvar_ready_AA, name, fixed = TRUE))
    ## Amino acid change and coding were found
    if(nrow(clinvar_hits) == 1){
      srvt$clinical_significance = clinvar_hits$clinical_significance
      srvt$variation_id = clinvar_hits$variation_id
      srvt$coding_match = TRUE
      return(srvt)
    }else{
      clinvar_hits <- clinvar %>%
        dplyr::filter(gene_symbol %in% srvt$gene) %>%
        dplyr::filter(grepl(srvt$clinvar_ready_AA, name, fixed = TRUE))
        ## Amino acid change was found but not coding
        if(nrow(clinvar_hits) == 1){
          srvt$clinical_significance = clinvar_hits$clinical_significance
          srvt$variation_id = clinvar_hits$variation_id
          srvt$coding_match = FALSE
          return(srvt)
        }
    }
  }else{
    ## Amino acid change NA or p.?
    ## Only greps 'coding' entry
    clinvar_hits <- clinvar %>%
      dplyr::filter(gene_symbol %in% srvt$gene) %>%
      dplyr::filter(grepl(srvt$coding, name, fixed = TRUE))
    if(nrow(clinvar_hits) == 1){
      srvt$clinical_significance = clinvar_hits$clinical_significance
      srvt$variation_id = clinvar_hits$variation_id
      srvt$coding_match = TRUE
      return(srvt)

    }
  }
  srvt$clinical_significance = "not found in database"
  srvt$variation_id = NA
  srvt$coding_match = FALSE
  return(srvt)
}

#' Combine single edited variants into table
#'
#' @param snv_tb
#'
#' @return
#' @export
#'
#' @examples
clinvarTableOutput <- function(snv_tb){
  stopifnot(nrow(snv_tb) > 0)

    snv_tb <- snv_tb %>% dplyr::select(-contains("rowid")) %>%  rowid_to_column()
    ## include if one wants to assess variation position, not exact variation
    #%>% dplyr::mutate(clinvar_pos_info = str_remove(clinvar_ready_AA, pattern = "...$")))
    tbl_output = bind_rows(lapply(snv_tb$rowid, function(i) clinvarSingleRow(srvt = snv_tb[i,]))) %>%
      dplyr::arrange(clinical_significance)
    return(tbl_output)
}

# clin_annots <- lapply(snvs, function(x) clinvarTableOutput(x) %>% ungroup() %>% dplyr::select(15:22) )
#
# bind_rows(clin_annots) %>%
#   dplyr::filter(isTRUE(coding_match))


## maybe include if one wants to assess variation position, not exact variation
# clinvarCheck_sig_and_pos <- function(srvt){
#
#   if(srvt$clinvar_ready_AA != "p.?" & !is.na(srvt$clinvar_ready_AA)){
#
#     clinvar_hits = clinvar %>%
#       dplyr::filter(gene_symbol == srvt$gene) %>%
#       dplyr::filter(grepl(srvt$coding, name, fixed = T)) %>%
#       #dplyr::filter(grepl(srvt$transcript, name)) %>%
#       dplyr::select(name, gene_symbol,type, clinical_significance, last_evaluated, variation_id )
#
#     clinvar_pos = clinvar %>%
#       dplyr::filter(gene_symbol == srvt$gene) %>%
#       dplyr::filter(grepl(srvt$clinvar_pos_info, name, fixed = T)) %>%
#       dplyr::filter(!grepl("fs)$|=)", name)) %>%
#       dplyr::select(name, gene_symbol,type, clinical_significance, last_evaluated, variation_id )
#
#     return(list(variant_info = clinvar_hits,
#                 position_info = clinvar_pos))
#   }
# }






