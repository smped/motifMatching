#' @title Compare PWM matrices
#' 
#' @description Compare PWM matrices
#' 
#' @param name the name of the motif without the dataSource specific prefix
#' @param pre1 the prefix from the first dataSource
#' @param pre2 the prefix from the second dataSource
#' @param db the common database which contains PWM information from the two dataSources. 
#' Should be in \code{MotifDb} format.
#' 
#' @details Designed to compare two matrices with an identical name from two databases, 
#' and to check if they are the same
#' 
#' @return A single row \code{data_frame} with the supplied \code{name} 
#' and the \code{logical} value in the variable \code{identical}
#' 
#' @author Steve Pederson
#' 
#' @import dplyr
#' 
#' @export
comparePWM <- function(name, 
                       pre1="Mmusculus-JASPAR_CORE-", pre2="Mmusculus-JASPAR_2014-", 
                       db=tfMotifs){
  
  if(!grepl("MotifList", class(db))) stop("db must be a MotifList from the package MotifDb.\n")
  if(!paste0(pre1, name) %in% names(db)) stop("Couldn't find", paste0(pre1, name), ".\n")
  if(!paste0(pre2, name) %in% names(db)) stop("Couldn't find", paste0(pre2, name), ".\n")
  res <- db[[paste0(pre1, name)]] - db[[paste0(pre2, name)]]
  data_frame(name = name, identical = !as.logical(sum(res)))
}