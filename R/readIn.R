###################################################### READ IN FILE ###################################################### 

#' Read in files
#'
#' @param filepath 
#'
#' @return
#' @export
#'
#' @examples
readIn <- function(filepath){ 
  IRoutput <- readr::read_tsv(filepath,
                              comment = "##", skip_empty_rows = TRUE) %>% 
    janitor::clean_names()
  return(IRoutput)
}