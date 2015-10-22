#' @title Obtain the number of sequences containing matches
#' 
#' @description Counts the number of \code{XStringViews} in a \code{XStringSet} which contain a match to a motif.
#' 
#' @details This function takes a set of \code{\link{XStringViews}} obtained by motif matching performed on an 
#' \code{\link{XStringSet}} which has been concatenated into a single \code{\link{XString}}.
#' All strings in the original XStringSet must be the same length, which must be specified by the
#' argument \code{seqLength}.
#' This parameter cannot be calculated directly from the object supplied in \code{views}.
#' 
#' @param views contains the XStringViews which correspond to motif matches obtained previously
#' @param seqLength the length of the sequences (i.e. promoters) initially searched
#' @param singleHits \code{logical}. Determines whether a single match is returned for each sequence, 
#' regardless of multiple matches (\code{singleHits=TRUE}), 
#' or whether the number of total matches are returned (\code{singleHits=FALSE})
#' 
#' @return The number of the initial sequences containing at least one match (\code{singleHits = TRUE}), 
#' or the total number of matches (\code{singleHits = FALSE}).
#' 
#' @seealso \code{\link{DNAStringSet}}
#' 
#' @author Steve Pederson
#' 
#' @rdname countMatches
#' 
#' @import Biostrings
#' @export
countViews <- function(views, seqLength, singleHits = TRUE){
  stopifnot(class(views) == "XStringViews")
  if (length(seqLength) > 1){
    warning("The argument seqLength can only take single values. All but the first will be ignored")
    seqLength <- seqLength[1]
  }
  if (singleHits) {
      starts <- start(views)
      matchProms <- ceiling(starts/seqLength)
      return(length(unique(matchProms)))
  }
  else return(length(views))
}