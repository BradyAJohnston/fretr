#' Predict the FRET value given a distance and R0.
#'
#' @param distance Distance between the two fluorophores.
#' @param R0 Distance in angstroms at which the two fluorophores have `FRET =
#'   0.5`.
#'
#' @return Numeric vector of calculated FRET distances, with the same length as
#'   distance.
#' @export
pred_fret <- function(distance, R0 = 54) {
  stopifnot(is.numeric(distance))
  stopifnot(is.numeric(R0))

  1 / (1 + (distance / R0)^6)
}
