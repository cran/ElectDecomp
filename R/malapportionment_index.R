#' Malapportionment index
#'
#' @description Computes the malapportionment index proposed in Samuels and Snyder (2001).
#'
#' @author Alberto Penades, \email{alberto.penades@@gmail.com}
#' @author Jose M. Pavia, \email{pavia@@uv.es}
#'
#' @references Samuels, D and Snyder, R (2001) The Value of a Vote: Malapportionment in Comparative Perspective. *British Journal of Political Science*, 31 , 651-671.
#'
#' @param seats A vector (matrix or data.frame) of length k (the number of constituencies/districts),
#'              components in the same order as `census`, with the seats apportioned
#'              in each constituency/district.
#'
#' @param census A vector (matrix or data.frame) of length k (the number of constituencies/districts), with
#'               components in the same order as `seats`, with the
#'               census (overall population entitled to vote) in each constituency/district.

#'
#' @return
#' The Value of the malapportionment index.
#'
#' @export
#'
#' @examples
#'
#'  seats.ex <- c(5, 7, 7, 7, 5, 7, 8, 12, 3, 3, 6, 10, 6, 6, 7, 5, 4, 5, 4,
#'                3, 5, 3, 4, 6, 3, 4, 3, 3, 5, 4, 24, 3, 2, 4, 7, 5, 9, 5, 5,
#'                8, 29, 5, 3, 3, 6, 8, 4, 9, 4, 14, 1, 1)
#'  census.ex <- c(239935L, 559590L, 451021L, 456952L, 255557L, 403142L, 569535L,
#'                 842042L, 153109L, 112376L, 547396L, 783206L, 407899L, 365063L,
#'                 397732L, 327008L, 211249L, 310547L, 151099L, 95061L, 309595L,
#'                 130998L, 238885L, 364127L, 125385L, 241301L, 99408L, 73992L,
#'                 295410L, 164706L, 3004988L, 304448L, 242148L, 331744L, 413193L,
#'                 279372L, 731499L, 302293L, 315033L, 548711L, 2744152L, 319222L,
#'                 153080L, 437592L, 749572L, 551021L, 167030L, 677156L, 283161L,
#'                 1287981L, 31672L, 25368L)
#'
#' malapportionment_index(seats.ex, census.ex)
#'

malapportionment_index <- function(seats, census){

  # Tests
  seats <- as.vector(as.matrix(seats))
  census <- as.vector(as.matrix(census))
  # sizes
  if (length(seats) != length(census))
    stop("The number of units (constituencies) in 'seats' and 'census' must be equal.")
  # Positive
  if(min(seats) < 0)
    stop("Non negative values are allowed in `seats`.")
  if(min(census) < 0)
    stop("Non negative values are allowed in `census`.")
  # Computation
  r_j <- census # voters in constituency
  m_j <- seats # magnitude

  ## MAL
  MAL <- 0.5*sum(abs(m_j/sum(seats) - census/sum(census)))
  return(MAL)
}
