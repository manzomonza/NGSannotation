#'  COPY and rename GNXSinfo to GNXS_metadata
#'
#' @param infofilepath
#' @param dirname
#'
#' @return
#' @export
#'
#' @examples
cp_mv_gnxs = function(infofilepath, directory_name){
  base_filename = basename(infofilepath)
  file.copy(from=infofilepath,
            to = dirname)
  file.rename(from = paste0(dirname,"/", base_filename), to=paste0(dirname,"/GNXS_metadata.txt"))
}
