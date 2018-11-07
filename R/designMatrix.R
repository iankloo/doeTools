#' Create Design Matrix
#'
#' This function creates a design matrix for a given set of factors.  By convention, your factors should be single letters.
#' The user can determine if they want just the main effects, or all (interaction) effects included.
#' makeTreatments() is used to create the treatment column in the standard order and notation.
#'
#' @param factors A vector of factors used in your experimental design - usually single letters.
#' @param mainOnly If set to TRUE, only columns with main effects (i.e., no interaction effects) will be created.
#' @param standardOrder If set to TRUE, effect columns will appear in standard order.  Otherwise, they will be reported in order of interaction.  No effect if mainOnly = TRUE.
#' @return A matrix with a row for each treatment combination and a column for each main effect (and interaction effects, if included).
#' @examples
#' ##create full design matrix
#' designMatrix(factors = c('A','B','C','D'))
#'
#' ##create design matrix with main effects only
#' designMatrix(factors = c('A','B','C','D'), mainOnly = TRUE)
#'
#' @export
designMatrix <- function(factors, mainOnly = FALSE, standardOrder = TRUE){
  lfactors <- tolower(factors)
  n <- 1:(length(lfactors))
  treatment <- makeTreatments(lfactors)
  tmp <- lapply(lfactors, function(d) unlist(lapply(treatment, FUN = function(e) ifelse(d %in% unlist(strsplit(e, "")), 1, -1))))
  tmpMat <- as.matrix(do.call(cbind, tmp))
  colnames(tmpMat) <- toupper(factors)
  row.names(tmpMat) <- treatment
  if(mainOnly){
    return(tmpMat)
  } else{
    return(makeInteractionGrid(tmpMat, standardOrder = standardOrder))
  }
}

#' Create all interact effects
#'
#' This is an internal function that is called by designMatrix when mainOnly = FALSE.
#'
#' @param myData A design matrix with main effects only.
#' @param standardOrder Passed from designMatrix.
#' @return A matrix with a row for each treatment combination and a column for each main effect (and interaction effects, if included).
#'
#' @export
makeInteractionGrid <- function(myData, standardOrder = TRUE){
  order <- toupper(row.names(myData))
  order <- order[order != '(1)']
  inters <- order[nchar(order) > 1]
  z <- lapply(inters, FUN = function(d) apply(myData[,unlist(strsplit(d,''))], 1, FUN = prod))
  tmp <- do.call(cbind, z)
  colnames(tmp) <- inters
  out <- cbind(myData, tmp)
  if(standardOrder){
    out <- out[,order]
  } else{
    out <- out[,order(nchar(colnames(out)))]
  }
  return(out)
}
