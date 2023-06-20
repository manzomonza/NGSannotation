
#' Output counts of variants per panel
#' only SNVs for now
#' @param prep_tbl
#' @param SNV_COUNT
#'
#' @return
#' @export
#'
#' @examples
count_variants_per_panel = function(prep_tbl, default = SNV_COUNT){
  prep_tbl = dplyr::select(prep_tbl, gene, coding, one_AA)
  fil_tbl = dplyr::left_join(prep_tbl, SNV_COUNT)
  fil_tbl = dplyr::summarise(fil_tbl, total = sum(n), n = n)
  fil_tbl = dplyr::arrange(fil_tbl, desc(total))
  fil_tbl = dplyr::mutate(fil_tbl, panel_list = paste0(workflowName, " (", total, ")", collapse = "; "))
  fil_tbl = dplyr::group_by(fil_tbl, gene, coding, one_AA, panel_list)
  fil_tbl = dplyr::summarise(fil_tbl, total = sum(total))
  fil_tbl = dplyr::select(fil_tbl, gene, coding, one_AA, total, panel_list)
  fil_tbl = dplyr::distinct(fil_tbl)
  return(fil_tbl)
}

