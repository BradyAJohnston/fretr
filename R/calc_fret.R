#' Calculate FRET from `donor` and `acceptor` signals.
#'
#' @param donor Numeric vector of donor signal.
#' @param acceptor Numeric vector of acceptor signal.
#'
#' @return Numeric vector, the same length as donor and acceptor.
#' @export
#'
calc_fret <- function(donor, acceptor) {
  acceptor / (donor + acceptor)
}
