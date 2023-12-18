#' @title Function demonstrating segfault with flang
#'
#' @description Minimal example reproducing an issue
#'    in package sequoia
#'
#' @param N  Integer number
#'
#' @return nothing
#'
#' @useDynLib minWE, .registration = TRUE
#'
#' @examples
#' test_fun(510)   # N<=510 always runs OK
#' test_fun(5e8)   # N>=512 always segfault
#'
#' @export

test_fun <- function(N)
{
  TMP <- .Fortran(dostuff,
                  N = as.integer(N),
                  V = integer(N*N))

  cat('done. ', TMP$V[1], '\n')
}

