#' Traditional indexes for measuring deviations from proportionality
#'
#' @description Computes a list of indexes proposed in the literature to measure the seat-to-vote deviations from proportionality. (See the `malapportionment_index` function for the Samuels and Snyder’s index).
#'
#' @author Alberto Penades, \email{alberto.penades@@gmail.com}
#' @author Jose M. Pavia, \email{pavia@@uv.es}
#'
#' @references Riera, P and Penades, A *Indices of Disproportionality*. Elgar Encyclopedia of Political Representation (Eds. A Freire, A Pedrazzani & H Schmit). Cheltenham: Edgar Elgar Publishing.
#' @references Taagepera, R and Shugart, MS (1991) *Seats and Votes: The Effects and Determinants of Electoral Systems*. New Haven: Yale University Press.
#'
#' @param votes A vector of length p (where p the number of parties) with the votes gained by each party in the election.
#'              When blank or null votes are included in `votes`, they are
#'              treated in the same way as the other parties.
#'
#' @param seats A vector of length p (where p the number of parties) with components in the same order as `votes`,
#'              with the seats gained by each party/candidacy in the election.
#'
#' @return
#' A data.frame of order 10x3 with the Rae, Loosemore-Handy, Gallagher, Monroe, Sainte-Lague,
#' Entropy, Taagepera-Laasko-v, Taagepera-Laasko-s, Taagapera-Shugart, Pessini-Gini and
#' Pavia-Gini indexes.
#'
#' @note
#' The Entropy index (EI) only considers parties gaining seats.
#'
#' @export
#'
#' @examples
#' votes.ex <- c(Party1 = 6310391, Party2 = 5371866, Party3 = 1709890,
#'               Party4 = 1504771, Party5 = 323444)
#'
#' seats.ex <- c(Party1 = 165, Party2 = 118, Party3 = 20,
#'               Party4 = 16, Party5 = 0)
#'
#' example <- traditional_indexes(votes.ex, seats.ex)
#' example
#'

traditional_indexes <- function(votes, seats){

  # Tests
  # sizes
  if (length(votes) != length(seats))
    stop("The number of parties in 'votes' and 'seats' must be equal.")
  # Positive
  if(min(votes) < 0)
    stop("Non negative values are allowed in `votes`.")
  if(min(seats) < 0)
    stop("Non negative values are allowed in `seats`.")
  # Computations
  np <- length(seats)
  S_i <- seats/sum(seats) # proportion of seats
  V_i <- votes/sum(votes) # proportion of votes
  D_i<- S_i - V_i
  ## Indexes
  I <- sum(abs(D_i))/np
  D <- 0.5 * sum(abs(D_i))
  LSQ <- sqrt( 0.5 * sum(D_i ^ 2))
  DD <- sqrt( sum(D_i ^ 2)/(1 + sum(V_i ^ 2)))
  DSL <- sum((D_i ^ 2)/V_i)
  EI <- sum(S_i[S_i>0] * log(S_i[S_i>0]/V_i[S_i>0]))
  PG <- 0
  for (ii in 1L:np){
    for (jj in 1L:np){
      PG <- PG + abs(S_i[ii]/V_i[ii] - S_i[jj]/V_i[jj])
    }
  }
  PG1 <- PG / (sum(votes) * sum(seats))
  PG2 <- PG / (2 * np * (np - 1) * mean(S_i/V_i))
  ENEP <- 1/sum(V_i ^ 2)
  ENPP <- 1/sum(S_i ^ 2)
  TS <- (ENEP - ENPP)/ENEP

  Value <- c(I, D, LSQ, DD, DSL, EI, PG1, PG2, ENEP, ENPP, TS)
  Index <- c("Rae", "Loosemore-Hanby", "Gallagher", "Monroe", "Sainte-Lague",
             "Entropy", "Pennisi-Gini", "Pavia-Gini", "Taagepera-Laasko-v",
             "Taagepera-Laasko-s", "Taagapera-Shugart")
  Acronym <- c("I", "D", "LSQ", "DD", "DSL", "EI", "PG", "PG2", "ENEP",
               "ENPP", "TS")
  output <- data.frame(Index, Acronym, Value)

  return(output)
}
