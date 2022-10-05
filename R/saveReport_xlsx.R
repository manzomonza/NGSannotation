#' Save report to XLSX file
#'
#' @param filepath
#'
#' @return
#' @export
#'
#' @examples
saveReport_xlsx <- function(filepath){
  dir_name = gsub(".tsv", '_watchdog', filepath)
  excel_file_name = basename(filepath)
  excel_file_name = shortenFilename(excel_file_name)
  excel_file_name = paste0(excel_file_name, '_combined_output.xlsx')
  print("Filename shortened")

  ## Activity list entry
  prep_file_entries = prep_filepaths(filepath)
  activityList_string = activityList_format(snv_prep_filepath = prep_file_entries$prep_snv,
                                            cnv_prep_filepath = prep_file_entries$prep_cnv)
  activityList = tibble::tibble(activityList_Entry = activityList_string)


  if(grepl("Snvindel.tsv", filepath)){
    cnv =  readr::read_tsv(paste0(dirname(filepath), "/Cnv.tsv"), skip_empty_rows = TRUE)
    snv =  readr::read_tsv(paste0(dirname(filepath), "/Snvindel.tsv"), skip_empty_rows = TRUE)
    annotation = readr::read_tsv(paste0(dir_name, "/clinvar_annotation.txt"))
    annotation = annotation %>% dplyr::relocate(contains('COSMIC'), .after = last_col())

    filtered = readr::read_tsv(paste0(dir_name, "/prep_filtered.txt"))
    infoFilepath = paste0(dirname(filepath), "/Info.csv")
    if(file.exists(infoFilepath)){
      ## Files
      info_csv = readr::read_tsv(infoFilepath)
      sampleName = infoName(infoFilepath)
      file.copy(from = infoFilepath, to = dir_name)


    }else{
      info_csv = tibble::tibble(Metadata = "Info.csv not found, no metadata provided")
      sampleName = basename(dirname(filepath))
    }



    excel_file = list(snv, cnv, info_csv, filtered, annotation, activityList)
    names(excel_file) <- c("Snvindel.tsv", "Cnv.tsv", "Metadata", "Filtered_lines", "Annotation", "activityList_Entry")
    writexl::write_xlsx(excel_file, path = paste0(dir_name, "/", sampleName, "_GXS_combined_output.xlsx"))
  }else{
    ## Files
    ir_output =  readr::read_tsv(filepath, skip_empty_rows = TRUE, comment = "##")
    info_csv = readr::read_tsv(paste0(dir_name, "/Info.csv"))
    filtered = readr::read_tsv(paste0(dir_name, "/prep_filtered.txt"))
    annotation = readr::read_tsv(paste0(dir_name, "/clinvar_annotation.txt")) %>% dplyr::select(-(gene:clinvar_ready_AA))
    annotation = annotation %>% dplyr::relocate(contains('COSMIC'), .after = last_col())

## Combine into list
    excel_file = list(ir_output, info_csv, filtered, annotation, activityList)
    names(excel_file) <- c("IR_Output","Metadata", "Filtered_lines", "Annotation", "activityList_Entry")
    writexl::write_xlsx(excel_file, path = paste0(dir_name, "/", excel_file_name))
  }
}
