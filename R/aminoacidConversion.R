
aminocode  <- c("Ala", "Arg", "Asn", "Asp", "Cys", "Gln", "Glu", "Gly", "His", "Ile", "Leu", "Lys", "Met", "Phe", "Pro", "Ser", "Thr",
                "Trp", "Tyr", "Val", "Sec", "Pyl", "Asx", "Xle", "Glx", "Xaa")
names(aminocode) <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T",
                      "W", "Y", "V", "U", "O", "B", "J", "Z","X")
aminocode['\\*']  <- "Ter"
aminocode['dup']  <- "dup"



#' Single or three letter code
#'
#' @param amino_acid_change 
#'
#' @return
#' @export
#'
#' @examples
oneORthree_code <- function(amino_acid_change){
  if(stringr::str_detect(amino_acid_change, regex(pattern = '[:upper:][:digit:]'))){
    return("one")
  }else{
    return("three")
  }
}



#' ###### Change three to one letter code
#'
#' @param amino_acid_change_entry 
#'
#' @return
#' @export
#'
#' @examples
amino_acid_conversion_three_to_one <- function(amino_acid_change_entry){
  amino_acid_change_parse = amino_acid_change_entry
  if(!is.na(amino_acid_change_entry) & !grepl("p\\.\\?", amino_acid_change_entry) ){
    if(oneORthree_code(amino_acid_change_entry) == "three"){
      for(i in seq_along(aminocode)){
        amino_acid_change_parse <- gsub(aminocode[i], names(aminocode)[i], amino_acid_change_parse, ignore.case = FALSE)
      }
      if(!grepl("p.", amino_acid_change_parse)){
        amino_acid_change_parse <- paste0("p.", amino_acid_change_parse)
      }
      return(amino_acid_change_parse)
      }else{
        return(amino_acid_change_entry)
        }
    }else{
      return(NA)
  }
}



#' ###### Change one letter to three letter code
#'
#' @param amino_acid_change_entry 
#'
#' @return
#' @export
#'
#' @examples
amino_acid_conversion_one_to_three <- function(amino_acid_change_entry){
  if(!is.na(amino_acid_change_entry) & !grepl("p\\.\\?", amino_acid_change_entry) ){
    ## Pattern equals e.g. P45 (only one upper case followed by digit)
    if(oneORthree_code(amino_acid_change_entry) == "one"){
      amino_acid_change_parse = complex_one_to_three(amino_acid_change_entry)
      return(amino_acid_change_parse)
    }else{
      return(amino_acid_change_entry)
    }
    }else{
    return(NA)
  }
}



#' ####### Simple One To Three AA amino acid switch, used in aa_parser_singleletter
#'
#' @param trimmed_string 
#'
#' @return
#' @export
#'
#' @examples
one_three_aa_precision <- function(trimmed_string){
  lett = str_extract_all(trimmed_string, regex(pattern = '[[A-Z]]'))[[1]]
  position = str_extract(trimmed_string, regex(pattern = '\\d+'))

  lett = sapply(lett, function(x) aminocode[x])
  if(is.na(lett[2])){
    three_letter_code = paste0("p.", lett[1], position)
  }else{
    three_letter_code = paste0("p.", lett[1], position, lett[2])
  }
  return(three_letter_code)
}




#' #### Complex One To Three AA switch
#'
#' @param parsestring 
#'
#' @return
#' @export
#'
#' @examples
complex_one_to_three <- function(parsestring){
  if(parsestring != "p.?" & !is.na(parsestring)){
    if(grepl("_", parsestring)){
      # extract characters that are not amino-acids
      remove_it = stringr::str_extract(parsestring, pattern = "delins|del|ins|dup|fsTer.*")
      # remove characters that are not amino-acids
      trimmed_string = stringr::str_remove(parsestring, pattern = "del.*|dup.*|fsTer.*")
      # Split mutation string by "_"
      double_pos = unlist(stringr::str_split(trimmed_string, pattern = "_", simplify = FALSE))
      # Extract first position
      pos1 = stringr::str_extract(double_pos[1], regex(pattern = '\\d+'))
      # Extract second position
      pos2 = stringr::str_extract(double_pos[2], regex(pattern = '\\d+'))
      # Split string based "_" and mutation type (delins, ins )
      aa_characters = unlist(stringr::str_split(parsestring, pattern = "_|del|delins|ins", simplify = FALSE))
      aa_characters = aa_characters[!sapply(aa_characters, function(x) x == "")]
      from_AA = aminocode[stringr::str_extract_all(aa_characters[1], regex(pattern = '[[A-Z]]'))[[1]]]
      to_AA = aminocode[stringr::str_extract_all(aa_characters[2], regex(pattern = '[[A-Z]]'))[[1]]]
      #print(aa_characters[3])
      if(!is.na(aa_characters[3])){
        delins_AA = stringr::str_split(aa_characters[3], pattern = "")[[1]]
        for(i in seq_along(delins_AA)){
          delins_AA[i] = aminocode[delins_AA[i]]
        }
        delins_AA = paste0(delins_AA, collapse = "")
        three_letter_aa = paste0("p.", from_AA, pos1,"_", to_AA, pos2, remove_it, delins_AA)
      }else{
        three_letter_aa = paste0("p.", from_AA, pos1,"_",to_AA, pos2, remove_it)
      }
    }else{
      remove_it = stringr::str_extract(parsestring, pattern = "del|ins|dup.*|fsTer.*")
      parsestring = stringr::str_remove(parsestring, pattern = "del|dup.*|fsTer.*")
      if(!is.na(remove_it)){
        three_letter_aa = paste0(one_three_aa_precision(parsestring), remove_it)
      }else{
        three_letter_aa = one_three_aa_precision(parsestring)
      }
    }
  }else{
    three_letter_aa = parsestring
  }
  return(three_letter_aa)
}
