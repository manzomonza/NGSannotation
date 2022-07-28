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

  if(file.exists(snv) | file.exists(cnv)){
    if(file.exists(snv)){
      snv = readr::read_tsv(snv)
    }
    if(file.exists(cnv)){
      cnv = readr::read_tsv(cnv)
      cnv = cnvAnnot(cnv) %>% dplyr::select(Diagnose_D, Diagnose_F) %>% na.omit()
    }

    # Addition of MTBP column

    if(nrow(snv) > 0){
      snv = bimiMatchUp(snv)
      snv = clinvarTableOutput(snv)
      snv = diagnose_D_F_columns_snv(snv)

      # TSG annotation
      snv_tsg = tsgParseTable(readr::read_tsv(paste0(dir_path, "/prep_snv.txt")))
      snv_tsg = snv_tsg %>% dplyr::select(3:5, tsgInfo)
      # COSMIC COUNTER
      snv_cosmic = cosmic_counter_wrapper(readr::read_tsv(paste0(dir_path, "/prep_snv.txt")))
      snv_cosmic = snv_cosmic %>% dplyr::select(3:5, contains("COSMIC"))
      # CANCER HOTSPOTS
      snv_cancerHotspot = wrapper_table_cancerHotspots(readr::read_tsv(paste0(dir_path, "/prep_snv.txt")))
      snv_cancerHotspot = snv_cancerHotspot %>% dplyr::select(3:5, cancerHotspot)

      # GENE COLUMN NAME PROBLEM
      if(!'gene' %in% colnames(snv) & 'genes' %in% colnames(snv)){
        snv$gene = snv$genes
      }
      #Join TSG
      snv = dplyr::left_join(snv, snv_tsg, by = c("gene", 'coding'))

      #Join cosmic
      snv = dplyr::left_join(snv, snv_cosmic, by = c("gene", 'coding'))
      #Join cancerhotspot
      snv = dplyr::left_join(snv, snv_cancerHotspot, by = c("gene", 'coding'))
    }
    if(nrow(snv) > 0 & nrow(cnv) > 0){
      annotation = dplyr::bind_rows(snv, cnv)
    }else if(nrow(snv) > 0 & nrow(cnv) == 0){
      annotation = snv
    }else if(nrow(snv) == 0 & nrow(cnv) > 0){
      annotation = cnv
    }else{
      novar = tibble::tibble(diagnose = "No Variants detected")
      annotation = novar
    }
    readr::write_tsv(annotation, file = clinvar_annotation)
  }
}





