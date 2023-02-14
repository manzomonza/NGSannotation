#' ## Output Tables pipelines
#'
#' @param snvindel_path
#'
#' @return
#' @export
#'
#' @examples
precisionPipeline_TableOutput <- function(snvindel_path){
  cnv_filepath =  paste0(dirname(snvindel_path),"/Cnv.tsv")
  snv_filepath =  paste0(dirname(snvindel_path),"/Snvindel.tsv")

  print(cnv_filepath)
  print(snv_filepath)
  cnv = readIn(cnv_filepath)
  snv = read_rename_select_precision_snv(snv_filepath) %>% splicesite_Annot()
  output_tables <- make_output_tables_precision(snv_indel = snv, cnv = cnv)
  create_output_files(list_of_tables = output_tables,
                      filepath = snvindel_path)
}
#' Pipeline from intro file to Watchdog output files.
#' These files can be used for further downstream/alternative website processing.
#'
#' @param snv_filepath
#'
#' @return
#' @export
#'
#' @examples
stdPipeline_TableOutput <- function(snv_filepath){
  ir_output = read_rename_select(snv_filepath) %>% splicesite_Annot()
  output_tables <- make_output_tables(ir_output)
  create_output_files(list_of_tables = output_tables,
                      filepath = snv_filepath)
}
