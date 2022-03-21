
#' Add missing columns to make inconsistent IR output work
#'
#' @param ir_output 
#'
#' @return
#' @export
#'
#' @examples
addMissingCols <- function(ir_output){
  if(!"cnv_confidence" %in% colnames(ir_output)){
    ir_output$cnv_confidence <- NA
  }
  if(!"copy_number" %in% colnames(ir_output)){
    ir_output$copy_number <- NA
  }
  if(!"locus" %in% colnames(ir_output)){
    ir_output$locus <- NA
  }
  
  if(!"IR_clinvar" %in% colnames(ir_output)){
    ir_output$IR_clinvar <- NA
  }
  if(!"location" %in% colnames(ir_output)){
    ir_output$location <- NA
  }
  if(!"transcript" %in% colnames(ir_output)){
    ir_output$transcript <- NA
  }
  
  return(ir_output)
}
