#' @title Quick calculations used in the package
#' 
#' @description Quick calculations of the unconditional odds ratio and fisher.test for 2x2 matrices only
#' 
#' @param x a 2x2 matrix
#' 
#' @details Quick calculations designed to be utilised in the function \code{\link{comparePWMFreqs}}
#' The function \code{quickFisher} simply formats the output for consistency within this function
#' 
#' @author Steve Pederson
#' 
#' @rdname quickOR
#' @export
quickOR <- function(x){
  stopifnot(all(dim(x) == c(2, 2), is.numeric(x)))
  a <- x[1,1]*x[2,2]
  b <- x[2,1]*x[1,2]
  if ( a==0 && b==0 ) return(1)
  a/b
}

#' @rdname quickOR
#' @export
quickFisher <- function(x){
  stopifnot(all(dim(x) == c(2, 2), is.numeric(x)))
  f <- fisher.test(x)
  data.frame(testFreq = x[1,1]/sum(x[1,]), 
             refFreq = x[2,1]/sum(x[2,]), 
             oddsRatio = f$estimate,
             p = f$p.value)
}


