# Introduce functions to parse gnomad DB


#' Convert coding and location in prep_snv.txt to gnomad conforming location
#'
#' @param prep_snv_path
#'
#' @return
#' @export
#'
#' @examples
prep_chr = function(prep_snv){
  chr = stringr::str_extract(prep_snv$locus, pattern = 'chr\\w{1,2}(?=:)')
  return(chr)
}

prep_coding = function(prep_snv){
  ref = stringr::str_extract(prep_snv$coding, pattern = '(?<=\\d)\\D{1,}(?=>)')
  obs = stringr::str_extract(prep_snv$coding, pattern = '(?<=>)\\D{1,}')
  ref_obs = list(ref = ref, obs = obs)
  return(ref_obs)
}

prep_position = function(prep_snv){
  chr_position = stringr::str_extract(prep_snv$locus, pattern = '(?<=:)\\d{1,}')
  return(chr_position)
}


prep_rowwise = function(prep_snv_path){
  psf = readr::read_tsv(prep_snv_path)
  psf = tibble::rowid_to_column(psf)
  gnomad_locations = dplyr::bind_rows(lapply(psf$rowid, function(i) prep_integrate(psf[i,])))
  return(gnomad_locations)
}


prep_integrate = function(prep_snv){
  psf = prep_snv
  chr = prep_chr(psf)
  ref_obs = prep_coding(psf)
  chr_position = prep_position(psf)
  gnomad_ls = list(chr = chr,
                   pos = chr_position,
       ref = ref_obs$ref,
       obs = ref_obs$obs)

  return(gnomad_ls)
}
#
# testfiles = '/Users/manzo/Downloads/preps'
# testfiles = list.files(path =testfiles, full.names = TRUE)
#
# quick_chck = function(filepath){
#   (testf = readr::read_tsv(filepath) |> dplyr::select(locus, coding))
#   (prep_rowwise(filepath))
# }
# res = lapply(testfiles, quick_chck)
# as.data.frame(res[[3]])
#
# (testf = readr::read_tsv(testfiles[3]) |> dplyr::select(locus, coding))
# prep_rowwise(testfile)
#
# readr::read_tsv(testfiles[3]) |> dplyr::slice(c(8,18))
#
#
# # introduce functions to parse output of prep_snv.txt in gnomad conforming format
# # introduce method/table to easily introduce oncogenic mutations
#
#
