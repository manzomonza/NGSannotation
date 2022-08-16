################################################# Diagnose De + Fr #################################################
# Clinvar translations taken from Jürgen Hench (AutoClinvar Bewertung)
# https://script.google.com/home/projects/1dPu2vLfMhjGGvbUZ6kHqiU0QrCOLRQnD3N0i7Jfk0bmCDA_AlVP5AEaf/edit
## FUNCTIONS

#' Translate clinvar clinical significance to german
#'
#' @param input
#'
#' @return
#' @export
#'
#' @examples
ClassficationDe <- function(input){
  rep = ""
  if (input=="Benign/Likely benign"){rep="benigne/wahrscheinlich benigne"}else
    if (input=="Likely benign"){rep="wahrscheinlich benigne"}else
      if (input=="Benign"){rep="benigne"}else
        if (input=="Pathogenic"){rep="pathogen"}else
          if (input=="Likely pathogenic"){rep="wahrscheinlich pathogen"}else
            if (input=="Pathogenic/Likely pathogenic"){rep="pathogen/wahrscheinlich pathogen"}else
              if (input=="contradictory"){rep="widersprüchlich"}else
                if (input=="Conflicting interpretations of pathogenicity"){rep="unklar"}else
                  if (input=="not found in database"){rep="unklar"}else
                    if (input=="Uncertain significance"){rep="unklar"}else
                      if (input=="drug response"){rep="drug response"}else
                    {rep=input}
  return(rep);
}

#' Translate clinvar clinical significance to french
#'
#' @param input
#'
#' @return
#' @export
#'
#' @examples
ClassficationFr <- function(input){
  rep = ""
  if (input=="Benign/Likely benign"){rep="bénigne/probablement bénigne"}else
    if (input=="Likely benign"){rep="probablement bénigne"}else
      if (input=="Benign"){rep="bénigne"}else
        if (input=="Pathogenic"){rep="pathogénique"}else
          if (input=="Likely pathogenic"){rep="probablement pathogénique"}else
            if (input=="Pathogenic/Likely pathogenic"){rep="pathogénique/probablement pathogénique"}else
              if (input=="Contradictory"){rep="contradictoire"}else
                if (input=="Conflicting interpretations of pathogenicity"){rep="inconnue"}else
                  if (input=="not found in database"){rep="inconnue"}else
                    if (input=="Uncertain significance"){rep="inconnue"}else
                      if (input=="drug response"){rep="drug response"}else
                    {rep=input}
  return(rep);
}
