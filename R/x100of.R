#' Compute a value given its percentage of a total
#'
#' @description
#' Converts a percentage (`p`) and a total (`t`) into the corresponding value,
#' i.e. the inverse operation of \code{pndr.as_x100()}.
#' Mathematically: \eqn{x = (p * t) / 100}.
#'
#' @param p Numeric vector of percentages.
#' @param t Numeric scalar or vector representing the total or denominator.
#'   Must be of compatible length with `p` (recycled if length 1).
#' @param d Integer; number of decimal digits to round to (default `2`).
#'
#' @return
#' A numeric vector of the same length as `p`, representing the computed values
#' corresponding to the given percentages.
#'
#' @examples
#' \dontrun{
#' pndr.x100of(12.5, 200)     # → 25
#' pndr.x100of(c(10, 20), 50, d = 1)
#' }
#'
#' @seealso [pndr.as_x100()]
#' @export
pndr.x100of <- function(p, t, d = 2) {
  x <- as.numeric(round((p * t) / 100, digits = d))
  x
}
