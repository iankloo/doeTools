#' Create Treatment Combinations in Standard Notation
#'
#' This function helps define the treatment combinations in the correct (standard) notation and order.
#' According to standard notation, the combinations should be lowercase letters, so all inputs will be transformed to lower case.
#' This function can be used alone, but is more likely just a side effect of calling designMatrix().
#'
#' @param factors A vector of factors used in your experimental design - usually single capital letters.
#' @param out Only used for recursion - leave this set to NULL.
#' @return A vector of treatment combinations in standard notation (and order)
#' @examples
#' ##create treatment combinations
#' makeTreatments(factors = c('A','B','C','D'))
#'
#' @export

makeTreatments <- function(factors, out = NULL){
  if(length(factors) > 0){
    if(is.null(out)){
      combo <- factors[1]
      newVec <- factors[-1]
      makeTreatments(newVec, out = combo)
    } else{
      doneTmp <- out
      combo <- append(out, factors[1])
      combo <- append(combo, unlist(lapply(doneTmp, FUN = function(d) paste(d, factors[1], sep = ''))))
      newVec <- factors[-1]
      makeTreatments(newVec, out = combo)
    }
  } else{
    return(c('(1)',tolower(out)))
  }
}
