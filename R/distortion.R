#' Decomposition of Votes-to-Seats Distortion
#'
#' @description Decomposes the seats-to-votes deviation for each party by means
#'              of the geographical distribution of electoral support. The deviation
#'              from proportionality is divided into two fractions: one attributable
#'              to the electoral system —further separating the mean effect and geographical variance effect—
#'              and another due to population distribution, which includes
#'              malapportionment and turnout differential effects. Additionally,
#'              the function aggregates individual party deviations into an election-wide
#'              index of deviation from proportionality (the Loosemore-Hanby index).
#'              This index is also decomposed into components attributed to the
#'              major causes of deviation from proportionality, along with
#'              interactions among them.
#'
#' @author Alberto Penades, \email{alberto.penades@@gmail.com}
#' @author Jose M. Pavia, \email{pavia@@uv.es}
#'
#' @references Penades, A and Pavia, JM. *The decomposition of seats-to-votes distortion in elections: mean, variance, malapportionment and participation*.
#'
#' @param votes A data.frame or matrix of order kxp (where k is the number of constituencies/districts and p the number of parties)
#'              with the votes gained by each party/candidacy in each constituency/district.
#'              When blank and/or null votes are included as columns in `votes`, they are
#'              treated in the same way as the other parties.
#'
#' @param seats A data.frame or matrix of order kxp (where k is the number of constituencies/districts and p the number of parties),
#'              with row and columns in the same order as `votes`, with the seats gained by each party/candidacy
#'              in each constituency/district.
#'
#' @param census A vector (matrix or data.frame) of length k (the number of constituencies/districts), with
#'               components in the same order as the rows in `votes`, with the
#'               census (overall population entitled to vote) in each constituency/district.

#'
#' @return
#' A list with three components
#'
#' \itemize{
#'  \item `inputs`: A list containing all the objects with the values used as arguments by the function.
#'  \item `district2party.contributions`: A matrix of order kxp with the district contributions to the total seat-to-vote deviation of each party. The sums across columns of this matrix yields the vector of total seat-to-vote deviations.
#'  \item `party.distortions`: A matrix of order 7xp with the estimates of the distortions at the party level.
#'  \item `aggregate.distortions`: A matrix of order 7x1 with the Loosemore-Handy index and its decomposition due to parties with and without representation as well as its cumulative decomposition step-by-step into mean effects, variance effects, malapportionment effects and turnout (and interaction) effects.
#' }
#'
#' @export
#'
#' @examples
#' votes.ex <- structure(list(Party1 = c(92019L, 112927L, 117482L, 152498L, 89494L,
#'                                    103697L, 115390L, 223252L, 55837L, 46820L,
#'                                    138575L, 177843L, 163536L, 187254L, 140237L,
#'                                    102719L, 64603L, 102362L, 70550L, 39321L,
#'                                    101891L, 71650L, 89363L, 137495L, 51568L,
#'                                    108862L, 50270L, 35324L, 104145L, 60626L,
#'                                    355857L, 45231L, 45191L, 69014L, 148697L,
#'                                    115729L, 221996L, 87059L, 99440L, 198231L,
#'                                    737699L, 75036L, 38338L, 0L, 91262L, 181633L,
#'                                    56917L, 197100L, 84115L, 334705L, 8808L, 10723L),
#'                          Party2 = c(50723L, 150896L, 122361L, 111746L, 63172L,
#'                                   123708L, 185095L, 251336L, 33693L, 16423L,
#'                                   111293L, 182850L, 73554L, 39616L, 50951L,
#'                                   67611L, 56332L, 79715L, 28350L, 17133L, 83956L,
#'                                   14984L, 44388L, 64766L, 25878L, 44168L, 18210L,
#'                                   10757L, 76308L, 26225L, 721880L, 60747L, 27809L,
#'                                   59926L, 107797L, 54827L, 78598L, 20808L, 21079L,
#'                                   54642L, 731380L, 54720L, 34244L, 93010L, 140643L,
#'                                   155871L, 36186L, 213242L, 69976L, 395211L,
#'                                   7886L, 5186L),
#'                           Party3 = c(11926L, 41809L, 59668L, 33879L, 10217L, 29847L,
#'                                   50990L, 91914L, 7398L, 2442L, 22446L, 60297L,
#'                                   14046L, 7467L, 10643L, 13971L, 13623L, 15562L,
#'                                   7927L, 5521L, 21942L, 2406L, 4765L, 12460L, 3744L,
#'                                   5522L, 2101L, 1196L, 15798L, 2783L, 469361L,
#'                                   24746L, 22680L, 41345L, 21932L, 6797L, 16777L,
#'                                   2949L, 2627L, 11835L, 247038L, 6319L, 3906L,
#'                                   12042L, 29968L, 29840L, 3846L, 50444L, 14029L,
#'                                   106133L, 0L, 966L),
#'                            Party4 = c(14886L, 20424L, 33616L, 24573L, 9415L, 26422L,
#'                                   34838L, 42860L, 7223L, 15180L, 34857L, 77932L,
#'                                   28472L, 15944L, 27828L, 36598L, 16005L, 31310L,
#'                                   10277L, 12815L, 44091L, 7123L, 28707L, 33285L,
#'                                   14638L, 15259L, 7407L, 3792L, 20521L, 30677L,
#'                                   75097L, 7953L, 10067L, 15216L, 21880L, 19516L,
#'                                   50256L, 36377L, 21502L, 40104L, 242077L, 0L,
#'                                   7927L, 27048L, 36934L, 30167L, 19925L, 35755L,
#'                                   14596L, 60410L, 2915L, 2074L)),
#'                             class = "data.frame", row.names = c(NA, -52L))
#'
#' seats.ex <- structure(list(Party1 = c(3L, 2L, 3L, 4L, 3L, 3L, 3L, 5L, 2L, 2L, 3L,
#'                                    4L, 4L, 5L, 5L, 3L, 2L, 3L, 3L, 2L, 2L, 3L,
#'                                    3L, 4L, 2L, 3L, 2L, 3L, 3L, 2L, 5L, 1L, 1L,
#'                                    2L, 4L, 4L, 6L, 4L, 4L, 6L, 11L, 3L, 2L, 0L,
#'                                    2L, 4L, 2L, 4L, 2L, 5L, 1L, 1L),
#'                            Party2 = c(2L, 4L, 3L, 3L, 2L, 4L, 4L, 5L, 1L,
#'                                    1L, 3L, 4L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L,
#'                                    2L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 2L, 1L, 11L,
#'                                    2L, 1L, 1L, 3L, 1L, 2L, 0L, 0L, 1L, 11L, 2L,
#'                                    1L, 3L, 3L, 4L, 1L, 4L, 2L, 7L, 0L, 0L),
#'                            Party3 = c(0L, 1L, 1L, 0L, 0L, 0L, 1L, 2L, 0L, 0L, 0L,
#'                                    1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
#'                                    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 7L, 0L, 0L,
#'                                    1L, 0L, 0L, 0L, 0L, 0L, 0L, 4L, 0L, 0L, 0L,
#'                                    0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L),
#'                            Party4 = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
#'                                   1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L,
#'                                   0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L,
#'                                   0L, 0L, 0L, 1L, 1L, 1L, 1L, 3L, 0L, 0L, 0L,
#'                                   1L, 0L, 1L, 0L, 0L, 1L, 0L, 0L)),
#'                            class = "data.frame", row.names = c(NA, -52L))
#'  census.ex <- c(239935L, 559590L, 451021L, 456952L, 255557L, 403142L, 569535L,
#'                 842042L, 153109L, 112376L, 547396L, 783206L, 407899L, 365063L,
#'                 397732L, 327008L, 211249L, 310547L, 151099L, 95061L, 309595L,
#'                 130998L, 238885L, 364127L, 125385L, 241301L, 99408L, 73992L,
#'                 295410L, 164706L, 3004988L, 304448L, 242148L, 331744L, 413193L,
#'                 279372L, 731499L, 302293L, 315033L, 548711L, 2744152L, 319222L,
#'                 153080L, 437592L, 749572L, 551021L, 167030L, 677156L, 283161L,
#'                 1287981L, 31672L, 25368L)
#'
#' example <- distortion(votes.ex, seats.ex, census.ex)
#' example$party.distortions
#'

distortion <- function(votes,
                       seats,
                       census
){

  inputs <- c(as.list(environment()))
  # Tests
  votes <- as.matrix(votes)
  seats <- as.matrix(seats)
  census <- as.vector(as.matrix(census))
  # sizes
  if (nrow(votes) != nrow(seats))
    stop("The number of constituencies (rows) in 'votes' and 'seats' must be equal.")
  if (ncol(votes) != ncol(seats))
    stop("The number of columns (parties) in 'votes' and 'seats' must be equal.")
  if (nrow(votes) != length(census))
    stop("The number of rows (constituencies) in 'votes' and the length of 'census' must be equal.")
  # Positive
  if(min(votes) < 0)
    stop("Non negative values are allowed in `votes`.")
  if(min(seats) < 0)
    stop("Non negative values are allowed in `seats`.")
  if(min(census) < 0)
    stop("Non negative values are allowed in `census`.")
  # Check census
  if (min(census - rowSums(votes)) < 0)
    stop("At least in a constituency/district the sum of votes is higher than the population entitled to vote.")
  # Computation
  k <- nrow(votes) # number of districts
  r_j <- census # voters in constituency
  t_j <- rowSums(votes)/sum(r_j) # turnout
  m_j <- rowSums(seats) # magnitude
  S_i <- colSums(seats)/sum(seats) # proportion of seats
  V_i <- colSums(votes)/sum(votes) # proportion of votes
  D_i<- S_i - V_i
  v_ij <- votes/rowSums(votes) # proportion of vote by party-constituency
  S.ask_i <- V.ask_i <- colSums(v_ij*m_j)/sum(seats)
  a_j <- r_j/m_j # number of electors per seat in district j
  A_i <- S_i - S.ask_i # Electoral System effect
  B_i <- V.ask_i - V_i # Population effect
  S.2ask_i <- V.2ask_i <- colSums(v_ij)/k
  d_i <- S_i - S.2ask_i # mean effect
  e_i <- V.2ask_i - V.ask_i # variance effect
  f_i <- V.ask_i - colSums(v_ij*r_j)/sum(r_j) # malapportionment
  g_i <- colSums(v_ij*r_j)/sum(r_j) - V_i # Participation effect
  ## Outputs
  district_party.distortions <- seats/sum(seats) - votes/sum(votes)
  party.distorsions <- rbind(D_i, A_i, d_i, e_i, B_i, f_i, g_i)
  colnames(party.distorsions) <- colnames(votes)
  rownames(party.distorsions) <- c("Total deviation, D_i",
                                   " Electoral system effect, A_i",
                                   "   Mean effect, d_i",
                                   "   Variance effect, e_i",
                                   " Population effect, B_i",
                                   "   Malapportionment effect, f_i",
                                   "   Turnout effect, g_i")

  ## Aggregate components
  D <- 0.5 * sum(abs(D_i))
  no.rep <- colSums(seats) == 0 # parties without representation
  Dr <- 0.5 * sum(abs(D_i[!no.rep]))
  Dwr <- 0.5 * sum(abs(D_i[no.rep]))
  D.d <- 0.5 * sum(abs(d_i))
  D.de <- 0.5 * sum(abs(d_i + e_i))
  D.def <- 0.5 * sum(abs(d_i + e_i + f_i))
  D.defg <- 0.5 * sum(abs(d_i + e_i + f_i + g_i))
  aggregate.distorsions <- rbind(D, Dr, Dwr, D.d, D.de, D.def, D.defg)
  rownames(aggregate.distorsions) <- c("Loosemore-Handy index",
                                       " Distortion due to parties with representation",
                                       " Distortion due to parties without representation",
                                       "Electoral system mean effect on distortion",
                                       "Electoral system (mean + variance) effect on distortion",
                                       "Electoral system plus malapportionment effects on distortion",
                                       "Distortion (including turnout effects and interactions): Loosemore-Handy index"
                                       )
  colnames(aggregate.distorsions) <- "Value"
  output <- list("inputs" = inputs,
                 "district2party.contributions" = district_party.distortions,
                 "party.distortions" = party.distorsions,
                 "aggregate.distortions" = aggregate.distorsions)
  class(output) <- c("ElectDecomp", "distorsion.ElectDecomp")

  return(output)


}
