#' Inequality in votes-to-seats conversion
#'
#' @description  Computes the votes-to-seats conversion ratios for each combination
#'               of district and party in order to draw the representational inequality
#'               (Lorenz) curve of an election (or multiple elections under one electoral
#'               system). The Representational Inequality index proposed by Kedar,
#'               Harsgor, and Sheinerman, (2016) is calculated as a system-level
#'               index based on the area under the curve.
#'               (Use `plot` on its output to draw the curve.)
#'
#' @author Jose M. Pavia, \email{pavia@@uv.es}
#' @author Alberto Penades, \email{alberto.penades@@gmail.com}
#'
#' @references Kedar, O, Harsgor, L and Sheinerman, RA (2016). Are voters equal under proportional representation? *American Journal of Political Science*, 60(3), 676-691 \doi{10.1111/ajps.12225}
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

#'
#' @return
#' A list with four components
#'
#' \itemize{
#'  \item `inputs`: A list containing all the objects with the values used as arguments by the function.
#'  \item `conversion.ratios`: A data.frame of order Nx7 (where N <= k*p) with conversion ratios of votes-to-seats for each combination of district and party. The first and second columns identify the district and party, respectively. The third column contains the conversion ratios. The last two columns (which are derived from the forth and fifth columns) present the cumulative proportions of seats and votes, from which the representational inequality curve of the election can be drawn.
#'  \item `RI`: The estimated Representational Inequality (RI) Index, which equals two times the area between the 45-degree line of perfect equality in votes conversion and the (Lorenz) representational inequality curve.
#'  \item `other.statistics`: A matrix of order 3x1 with the total election proportions of non-represented, underrepresented and overrepresented votes.
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
#'
#' example <- inequality(votes.ex, seats.ex)
#' example$RI
#'

inequality <- function(votes,
                       seats
){

  inputs <- c(as.list(environment()))
  # Tests
  votes <- as.matrix(votes)
  seats <- as.matrix(seats)
  # sizes
  if (nrow(votes) != nrow(seats))
    stop("The number of constituencies (rows) in 'votes' and 'seats' must be equal.")
  if (ncol(votes) != ncol(seats))
    stop("The number of columns (parties) in 'votes' and 'seats' must be equal.")
  # Positive
  if(min(votes) < 0)
    stop("Non negative values are allowed in `votes`.")
  if(min(seats) < 0)
    stop("Non negative values are allowed in `seats`.")

  # Computation
  p.seats <- seats/sum(seats)
  p.votes <- votes/sum(votes)
  CR_ij <- p.seats / p.votes
  CR_ij[is.nan(CR_ij)] <- Inf

  # Order of CR_ij
  CR.ij <- as.vector(CR_ij)
  ordenados <- order(CR.ij)

  # Rows and columns of ordered values
  filas.columnas <- arrayInd(ordenados, .dim = dim(CR_ij))
  cr.order <- CR.ij[ordenados]
  s.order <- p.seats[ordenados]
  v.order <- p.votes[ordenados]
  output <- as.data.frame(cbind(filas.columnas, cr.order, s.order, v.order))
  output <- output[!is.infinite(output$cr.order), ]
  if (is.null(rownames(CR_ij))) rownames(CR_ij) <- 1:nrow(CR_ij)
  output[, 1L] <- rownames(CR_ij)[output[, 1L]]
  output[, 2L] <- colnames(CR_ij)[output[, 2L]]
  names(output) <- c("District", "Party", "CR_ij", "Prop.Seats", "Prop.Votes")
  output <- output[order(output$CR_ij, output$Prop.Seats, output$Prop.Votes), ]
  Cum.Prop.Seats <- cumsum(output$Prop.Seats)
  Cum.Prop.Votes <- cumsum(output$Prop.Votes)
  output <- cbind(output, Cum.Prop.Seats, Cum.Prop.Votes)
  row.names(output) <- 1:nrow(output)
  RI <-  sum(Cum.Prop.Votes[-nrow(output)] - Cum.Prop.Seats[-nrow(output)])
  RI <- RI/sum(Cum.Prop.Votes[-nrow(output)])

  ## Outputs
  sin.rep <- which(output$CR_ij > 0)[1L] - 1L
  if (sin.rep == 0){
    sin.rep <- 0
  } else {
    sin.rep <- output$Cum.Prop.Votes[sin.rep]
  }
  under <- which(output$CR_ij < 1)
  under <- output$Cum.Prop.Votes[under[length(under)]]
  other.statistics <- rbind(sin.rep,
                            under,
                            1 - under)
  rownames(other.statistics) <- c("Non-represented votes",
                                  "Underrepresented votes",
                                  "Overrepresented votes")
  colnames(other.statistics) <- "Proportion"
  output <- list("inputs" = inputs,
                 "conversion.ratios" = output,
                 "RI" = RI,
                 "other.statistics" = other.statistics)
  class(output) <- c("ElectDecomp", "inequality.ElectDecomp")

  return(output)

}
