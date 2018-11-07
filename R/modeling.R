#' Estimate effects
#'
#' This function estimates the effects of factors in an experiment (single replication...for now).
#' Note, this function assumes you are using single letters to detnote main effects (and uses the number of main effects for computation).
#'
#' @param dm A design matrix.  Generate these with designMatrix() and add observations.
#' @param obsCol The name of the column with observations in it.
#' @return A dataframe with a columns for factor and effect.
#' @examples
#' ##Estimate the effect of all factors:
#'
#' #create a design matrix
#' z <- designMatrix(c('A','B','C','D'), standardOrder = TRUE)
#'
#' #add observations
#' z <- cbind(z, obs = c(550,669,604,650,633,642,601,635,1037,749,1052,868,1075,860, 1063, 729))
#'
#' #estimate effects
#' estEffects(dm = z, obsCol = 'obs')
#'
#'
#' @export
estEffects <- function(dm, obsCol = 'obs'){
  cols <- colnames(dm)
  effectCols <- cols[cols != obsCol]

  k <- length(cols[nchar(cols) == 1])

  tmpdf <- lapply(effectCols, function(d) data.frame(factor = d, effect = sum(dm[,d] * dm[,obsCol]) * (1/(2^(k-1)))))
  tmpdf <- data.table::rbindlist(tmpdf)

  return(tmpdf)
}



#' Analyze (using ANOVA) a design using center points or some effects to estimate standard error
#'
#' This function estimates the effects of factors in an experiment (single replication...for now) and uses either centerpoints
#' or pools some effects to estimate standard error.  This allows for the calculation of F statistics and P values in the ANOVA analysis.
#'
#' @param dm A design matrix.  Generate these with designMatrix() and add observations.
#' @param obsCol The name of the column with observations in it.
#' @param centerpoints A vector of centerpoints used to estimate standard error (either this or errorEst are required)
#' @param errorEst A vector of effects to use to estimate standard error (either this or centerpoints are required)
#' @return A dataframe with a columns for factor, sum of squares, degrees of freedom, mean squares, F-statistics, and P-values
#' @examples
#' ##Analysis using high-order interactions to estimate error:
#'
#' #create a design matrix with observations
#' z <- designMatrix(c('A','B','C','D'), standardOrder = TRUE)
#' z <- cbind(z, obs = c(550,669,604,650,633,642,601,635,1037,749,1052,868,1075,860, 1063, 729))
#'
#' #analyze the design using 3 and 4-factor interactions to estimate error
#' analyzeDesign(z, errorEst = c('ABC','ABD','ACD','BCD','ABCD'))
#'
#'
#' ##Analysis using centerpoints to estimate error:
#'
#' #create a design matrix with observations
#' z <- designMatrix(c('A','B'), standardOrder = TRUE)
#' z <- cbind(z, obs = c(39.3,40.9,40,41.5))
#'
#' #analyze the design using centerpoints to estimate error
#' analyzeDesign(z, errorEst = c('ABC','ABD','ACD','BCD','ABCD'))
#'
#' @export
analyzeDesign <- function(dm, obsCol = 'obs', centerpoints = NULL, errorEst = NULL){
  if(is.null(centerpoints) & is.null(errorEst)){
    stop('Must use either centerpoints or leave out some effects to estimate error!')
  }

  baseEffects <- estEffects(dm)
  cols <- colnames(dm)
  effectCols <- cols[cols != obsCol]

  k <- length(cols[nchar(cols) == 1])

  tmpdf <- lapply(effectCols, function(d) data.frame(factor = d, SS = (sum(dm[,d] * dm[,obsCol])^2) / (2^k)))
  tmpdf <- data.table::rbindlist(tmpdf)

  #!!!only for a single replicated design!!!
  tmpdf$df <- 1

  if(is.null(errorEst)){
    SSerror <- sum((centerpoints - mean(centerpoints))^2)

    SScurve <- (nrow(dm) * length(centerpoints) * ((mean(dm[,obsCol]) - mean(centerpoints))^2)) / (nrow(dm) + length(centerpoints))

    tmpdf <- rbind(tmpdf, data.frame(factor = c('Curve','Error'), SS = c(SScurve, SSerror), df = c(1, (length(centerpoints)-1))))

  } else{
    pooled <- sum(tmpdf$SS[tmpdf$factor %in% errorEst])
    df <- length(errorEst)

    tmpdf <- tmpdf[!tmpdf$factor %in% errorEst,]

    tmpdf <- rbind(tmpdf, data.frame(factor = 'Error', SS = pooled, df = length(errorEst)))

  }

  tmpdf$MS <- tmpdf$SS / tmpdf$df
  tmpdf$F <- tmpdf$MS / tmpdf$MS[tmpdf$factor == 'Error']
  tmpdf$P <- stats::pf(tmpdf$F, df1 = tmpdf$df, df2 = tmpdf$df[tmpdf$factor == 'Error'], lower.tail = FALSE)
  tmpdf[tmpdf$factor == 'Error', c('F','P')] <- NA

  return(tmpdf)
}
