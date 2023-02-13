
#' ### splice site annotation Function to interrogate if variant occurs in splice site
#'
#' @param variant_table
#'
#' @return
#' @export
#'
#' @examples
splicesite_Annot <- function(variant_table){
  location_ls <- variant_table %>% dplyr::select(-contains("rowid")) %>%  tibble::rowid_to_column()
  location_ls <- location_ls %>% dplyr::group_by(rowid) %>% dplyr::group_split()
 # return(location_ls)
  variant_table <- bind_rows(lapply(location_ls, splicesite_rowwise )) %>%
    dplyr::select(-rowid)
  return(variant_table)
}



#' Rowwise splice site annotation
#'
#' @param srvt
#'
#' @return
#' @export
#'
#' @examples
splicesite_rowwise <- function(srvt){
  if(is.na(srvt$location)){
    srvt$cds_region = ifelse(is.na(srvt$exon), '', 'exon')
    srvt$cds_region_No = as.numeric(ifelse(is.na(srvt$exon), '', srvt$exon))
  }else{
    srvt$cds_region = ifelse(grepl("splice|intronic", srvt$location), "intron", "exon")
    srvt$cds_region_No = as.numeric(ifelse(is.na(srvt$exon), NA,
                               ifelse(srvt$cds_region == "intron" & grepl("-", srvt$coding),
                                      as.numeric(srvt$exon) - 1, as.numeric(srvt$exon) ) ) )
  }
  return(srvt)
}



