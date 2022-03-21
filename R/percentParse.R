#' Percent Frequency parsing
#'
#' @param percent_frequency
#'
#' @return
#' @export
#'
#' @examples Converts A=0.08,T=0.00 to 0.08
percentParse <- function(percent_frequency){
  percent_frequency = as.numeric(gsub("\\w{1,}=",'', percent_frequency))
  return(percent_frequency)
}
