
#' # COUNT Variants per panel -- only SNVs for now.
#'
#' @param prep_tbl
#' @param SNV_COUNT
#'
#' @return
#' @export
#'
#' @examples
count_variants_per_panel = function(prep_tbl, SNV_COUNT){
  fil_tbl = dplyr::left_join(prep_tbl, SNV_COUNT)
  fil_tbl$total = sum(fil_tbl$n)
  fil_tbl$panel_list = paste0(paste0(fil_tbl$workflowName, " (", fil_tbl$n, ")"), collapse = "; ")
  fil_tbl = dplyr::ungroup(fil_tbl)
  fil_tbl = dplyr::select(fil_tbl, -workflowName, -n)
  fil_tbl = dplyr::distinct(fil_tbl)
  return(fil_tbl)
}
