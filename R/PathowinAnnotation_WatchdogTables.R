#' ### Annotate files in watchdog folder
#'
#' @param filepath
#'
#' @return
#' @export
#'
#' @examples
annotateWatchdogTables <- function(filepath){
  print(filepath)
  dir_path = gsub(".tsv", '_watchdog', filepath)
  snv = paste0(dir_path, "/prep_snv.txt")
  cnv = paste0(dir_path, "/prep_cnv.txt")
  clinvar_annotation = paste0(dir_path, "/clinvar_annotation.txt")

  if(file.exists(snv) & file.exists(cnv)){
    snv <- readr::read_tsv(snv)
    cnv <- readr::read_tsv(cnv)
    cnv <- cnvAnnot(cnv) %>% dplyr::select(Diagnose_D, Diagnose_F) %>% na.omit()

    # Addition of MTBP column
    snv = mtbpFormat(snv)
    if(nrow(snv) > 0){
      snv <- bimiMatchUp(snv)
      snv <- clinvarTableOutput(snv)
      snv <- diagnose_D_F_columns_snv(snv)
    }
    if(nrow(snv) > 0 & nrow(cnv) >0){
      annotation <- dplyr::bind_rows(snv, cnv)
    }else if(nrow(snv) > 0 & nrow(cnv) == 0){
      annotation = snv
    }else if(nrow(snv) == 0 & nrow(cnv) > 0){
      annotation = cnv
    }else{
      novar <- tibble(diagnose = "No Variants detected")
      annotation = novar
    }
    readr::write_tsv(annotation, file = clinvar_annotation)
  }
}

