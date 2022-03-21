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
  excel_file_name = gsub(".tsv", '_combined_output.xlsx', excel_file_name)
  print("ok")
  if(grepl("Snvindel.tsv", filepath)){

    cnv =  readr::read_tsv(paste0(dirname(filepath), "/Cnv.tsv"), skip_empty_rows = TRUE)
    snv =  readr::read_tsv(paste0(dirname(filepath), "/Snvindel.tsv"), skip_empty_rows = TRUE)
    annotation = readr::read_tsv(paste0(dir_name, "/clinvar_annotation.txt"))
    filtered = readr::read_tsv(paste0(dir_name, "/prep_filtered.txt"))
    if(file.exists(paste0(dirname(filepath), "/Info.csv"))){
    ## Files
    info_csv = readr::read_tsv(paste0(dirname(filepath), "/Info.csv"))
    ## Combine into list

    }else{
      info_csv = tibble(Metadata = "Info.csv not found, no metadata provided")
    }
    excel_file = list(snv, cnv, info_csv, filtered, annotation)
    names(excel_file) <- c("Snvindel.tsv", "Cnv.tsv", "Metadata", "Filtered_lines", "Annotation")
    writexl::write_xlsx(excel_file, path = paste0(dir_name, "/GXS_combined_output.xlsx"))
  }else{
    ## Files
    ir_output =  readr::read_tsv(filepath, skip_empty_rows = TRUE, comment = "##")
    info_csv = readr::read_tsv(paste0(dir_name, "/Info.csv"))
    filtered = readr::read_tsv(paste0(dir_name, "/prep_filtered.txt"))
    annotation = readr::read_tsv(paste0(dir_name, "/clinvar_annotation.txt"))

    ## Combine into list
    excel_file = list(ir_output, info_csv, filtered, annotation)
    names(excel_file) <- c("IR_Output","Metadata", "Filtered_lines", "Annotation")
    writexl::write_xlsx(excel_file, path = paste0(dir_name, "/", excel_file_name))
  }
}
devtools::document()




