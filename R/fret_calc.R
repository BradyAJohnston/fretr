#' Calculate FRET from `donor` and `acceptor` signals.
#'
#' @param trans Signal from the transfer channel. (Excitation with donor laser
#'   and emission in acceptor window.)
#' @param donor Signal from the donor channel. (Excitation with the donor laser
#'   and emission in donor window.)
#'
#' @return Numeric vector, the same length as donor and acceptor.
#' @export
#'
fret_calc <- function(donor, trans) {
  stopifnot(is.numeric(donor))
  stopifnot(is.numeric(trans))

  trans / (donor + trans)
}
