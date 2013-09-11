summary.gambin <-
function(object, ...)
{
  res <- list()
  res$Data <- object$Data
  res$Dataname <- object$Dataname
  
  res$Alpha <- object$Alpha
  res$MaxOctave <- object$MaxOctave
  res$coefficients <- res$coefficients
  res$residuals <- object$Data$species - object$fitted.values
  
  res$ConfInt95 <- confint(object)

  res$logLik <- object$logLik
  
  compare <- cbind(object$Data$species, object$fitted.values)
  suppressWarnings(res$ChiSq <- chisq.test(compare)[1:3])
  
  attr(res, "nobs") <- nrow(res$Data)
  class(res) <- "summary.gambin"
  return(res)
}
