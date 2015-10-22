#' @title Check percentage format
#' 
#' @description Check the percentage format as obtained by the function \code{percent} from the \code{scales} package
#' 
#' @param x the object to test
#' @param min the minimum valid percentage as an integer
#' @param max the maximum valid percentage as an integer
#' 
#' @return A \code{logical} vector
#' 
#' @details Checks that a value is specified as a percentage, as obtained by the function \code{\link{percent}}
#' 
#' @import scales
#' 
#' @author Steve Pederson
#' 
#' @examples
#' isPercent("30%")
#' isPercent("200%")
#' isPercent("200%", max=100)
#' isPercent(100)
#' 
#' @export
isPercent <- function(x, min=-Inf, max=Inf){
  
  out <- rep(TRUE, length(x))
  out[!is.character(x)] <- FALSE

  n <- nchar(x)
  
  out[!substr(x, n, n) == "%"] <- FALSE
  
  val <- as.integer(gsub(",", "", substr(x, 1, n-1)))
  out[is.na(val)] <- FALSE
  out[val > max] <- FALSE
  out[val < min] <- FALSE

  out
  
}
