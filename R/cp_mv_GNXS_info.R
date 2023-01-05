#'  COPY and rename GNXSinfo to GNXS_metadata
#'
#' @param infofilepath
#' @param directory_name
#'
#' @return
#' @export
#'
#' @examples
cp_mv_gnxs = function(infofilepath, directory_name){
  base_filename = basename(infofilepath)
  file.copy(from=infofilepath,
            to = directory_name)
  file.rename(from = paste0(directory_name,"/", base_filename), to=paste0(directory_name,"/GNXS_metadata.txt"))
}
