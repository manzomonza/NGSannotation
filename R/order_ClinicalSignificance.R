#' Order by clinical significance
#'
#' @param diagnosestring 
#'
#' @return
#' @export
#'
#' @examples
orderSignificance <- function(diagnosestring){
  if(grepl("wahrscheinlich pathogen", diagnosestring)){
    order = 2
  }else if( grepl("pathogen", diagnosestring)){
    order = 1
  }else if(grepl("unklar", diagnosestring)){
    order = 3
  }else if(grepl("unbekannt", diagnosestring)){
    order = 4
  }else if(grepl("benign", diagnosestring)){
    order = 5
  }else{
    order = 10
  }
  return(order)
}
