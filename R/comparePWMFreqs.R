#' @title Compare the matches to a set of PWMs in two sets of sequences
#'
#' @description Compare the frequency of matches to a set of motifs within two sets of sequences
#' 
#' @param refSeq the set of reference sequences as an \code{\link{XStringSet}}
#' @param testSeq the set of test sequences as an \code{\link{XStringSet}}
#' @param motifs the set of motifs as a \code{\link{MotifList}}
#' @param minScore the minimum matching score for the PWM
#' @param testCounts if resampling over multiple reference sequences, supply this as
#' the output from \code{\link{countViews}}
#' @param useFisher \code{logical}. Adds the p-values from the \code{\link{fisher.test}} function to the output
#' 
#' @details Used to compare matches to the same set of motifs within two sets of sequences.
#' 
#' If using to bootstrap multiple sets of reference sequences, set \code{useFisher = FALSE} and
#'  use \code{\link{lapply}} (or it's relatives) to specify multiple sets of \code{XStringSet}s.
#'  
#' If simply testing two sets of sequences, set \code{iseFisher = TRUE} to add p-values
#'  from Fisher tests to every motif
#'  
#' @return Returns a \code{tbl_df} with columns \code{motif}, \code{testFreq}, \code{refFreq} & \code{oddsRatio}, 
#' with the additional column \code{p} if \code{useFisher=TRUE}.
#' If the argument \code{useFisher=TRUE}, the odds ratio will be the conditional MLE estimate as calculated using that function, 
#' whereas otherwise it will be the sample odds ratio.
#' 
#' @seealso \code{\link{tbl_df}}
#' 
#' @import abind
#' @import Biostrings
#' @import dplyr
#' 
#' @author Steve Pederson
#' 
#' @export
comparePWMFreqs <- function(refSeq, testSeq, motifs, minScore="90%", testCounts = NULL, useFisher = FALSE){
  
  refLen <- unique(width(refSeq))
  if (length(refLen) > 1) stop("All reference sequences must be the same length.\n")
  if (!grepl("MotifList", motifs)) stop("Motifs must be suppled as a MotifDb::MotifList.\n")
  if (length(minScore) > 1) {
    warning("Only the first value will be used for the minScore argument\n")
    minScore <- minScore[1]
  }
  stopifnot(all(isPercent(minScore, 0, 100), is.logical(useFisher)))
  
  if (is.null(testCounts)) { 
    testLen <- unique(width(testSeq))
    if (length(testLen) > 1) stop("All test sequences must be the same length.\n")
    if (testLen != refLen) stop("Test and reference sequences must be the same length.\n")
    testMatches <- lapply(motifs, matchPWM, subject = unlist(testSeq), min.score=minScore)
    testCounts <- lapply(testMatches, countViews, seqLength=testLen)
  }
  
  refMatches <- lapply(motifs, matchPWM, subject = unlist(refSeq), min.score=minScore)
  refCounts <- lapply(refMatches, countViews, seqLength = refLen)
  
  nTest <- length(testSeq)
  nRef <- length(refSeq)
  nMot <- length(motifs)
  allCounts <- rbind(unlist(testCounts), unlist(refCounts))
  countArray <- abind(allCounts, 
                      matrix(rep(c(nTest, nRef), nMot), nrow=2) - allCounts, 
                      along=3)
  
  if (useFisher){
    out <- apply(countArray, 2, quickFisher)
    out <- mutate(bind_rows(out), motif = names(motifs))
    return(dplyr::select(out, motif, testFreq, refFreq, oddsRatio, p))
  }
  else{
    or <- apply(countArray, MARGIN=2, FUN = quickOR)
    data_frame(motif = names(motifs), 
               testFreq = allCounts[1,] / nTest, 
               refFreq = allCounts[2,] / nRef,
               oddsRatio = or)
  }
}