#' 	Graphical representation of ElectDecomp objects
#'
#' @description Plot method for objects obtained with the functions `distortion` and `inequality` of the ElectDecomp package.
#'
#' @author Jose M. Pavia, \email{pavia@@uv.es}
#' @author Alberto Penades, \email{alberto.penades@@gmail.com}
#'
#' @param x An object output of either the `distortion` or `inequality` functions of the ElectDecomp package.
#' @param type When `x` is an output of `distortion`, this argument informs the output to be plotted. If `type = "districts"`, the method plots the `district2party.contributions` matrix; otherwise it plots the decomposition of the total deviation available in the `party.distortions` matrix.
#' @param panels.size When plotting an output of `distortion`, this argument informs about how the widths of panels should be calculated: either considering the deviations of all the parties ("global") or only the deviations of the plotted parties ("local"). Default, `"global"`.
#' @param title.size Size of the text of the panel titles. Default, 12.
#' @param axis.title.size Size of the text of the axis titles. Default, 12.
#' @param summaries This argument Informs whether some summary statistics about the election should be included in the plot: "none", no summary is included; "all" RI and `other.statistics` are included; "RI", only the representational inequality (RI) index is included. Default, "RI". Only active when an output of the `inequality` function is to be plotted.
#' @param bar.width Width of the bars or of the inequality curve. Default, 0.2.
#' @param bar.color Color of the bars or of the inequality curve. Default, "skyblue".
#' @param text.color Color to be used for text inside the plot. Default, "black".
#' @param text.size  Size to be used for text inside the plot. Default, 3.
#' @param labels.size Size to be used for the labels of the axis. Default, 10.
#' @param ticks.linewidth  Width of the lines to be used as ticks. Default, 0.2. Only active when the `party.distortions` matrix output of the `distortion` function is to be plotted.
#' @param length.breaks Distance between labels of the breaks x-axis. Default, 0.02. Only active when the `party.distortions` matrix output of the `distortion` function is to be plotted.
#' @param labels.y.size Size to be used for the labels of the y axis when `type = "districts"`. Default, 9.
#' @param labels.x.size Size to be used for the labels of the x axis when `type = "districts"`. If missing it is determined as a function of the number of parties to be plotted.
#' @param parties A vector with the indexes of the parties (columns) to be plotted. If missing, the six parties with the highest support in the election are chosen. Only active when an output of the `distortion` function is to be plotted.
#' @param party.names A vector with the names of the parties. If missing, names of the parties are inferred from the names of the columns to be plotted. Only active when an output of the `distortion` function is to be plotted.
#' @param panels.title A vector of length seven (for outputs of `distortion`) or of length one (for outputs of `inequality`) with the titles of the panels. If missing, they are automatically assigned taking into account the plotted component in each panel.
#' @param axis.title A vector of length two with the titles of the axis. If missing, they are automatically assigned taking into account the variable plotted in each axis.
#' @param ... Other arguments passed on to methods.
#' @param show.plot A `TRUE/FALSE` argument indicating if the plot should be displayed as a side-effect. By default, `TRUE`.
#'
#' @return
#' Invisibly returns the (ggplot) description of the plot, which is a list with components that contain the plot itself, the data, information about the scales, panels, etc.
#'
#' @note The packages `ggplot2` and `gridExtra` need to be installed for this function to work.
#'
# @import ggplot2 gridExtra
#'
#' @export
#' @method plot ElectDecomp
#' @examples
#' votes.ex <- structure(list(UCD = c(92019L, 112927L, 117482L, 152498L, 89494L,
#'                                    103697L, 115390L, 223252L, 55837L, 46820L,
#'                                    138575L, 177843L, 163536L, 187254L, 140237L,
#'                                    102719L, 64603L, 102362L, 70550L, 39321L,
#'                                    101891L, 71650L, 89363L, 137495L, 51568L,
#'                                    108862L, 50270L, 35324L, 104145L, 60626L,
#'                                    355857L, 45231L, 45191L, 69014L, 148697L,
#'                                    115729L, 221996L, 87059L, 99440L, 198231L,
#'                                    737699L, 75036L, 38338L, 0L, 91262L, 181633L,
#'                                    56917L, 197100L, 84115L, 334705L, 8808L, 10723L),
#'                          PSOE = c(50723L, 150896L, 122361L, 111746L, 63172L,
#'                                   123708L, 185095L, 251336L, 33693L, 16423L,
#'                                   111293L, 182850L, 73554L, 39616L, 50951L,
#'                                   67611L, 56332L, 79715L, 28350L, 17133L, 83956L,
#'                                   14984L, 44388L, 64766L, 25878L, 44168L, 18210L,
#'                                   10757L, 76308L, 26225L, 721880L, 60747L, 27809L,
#'                                   59926L, 107797L, 54827L, 78598L, 20808L, 21079L,
#'                                   54642L, 731380L, 54720L, 34244L, 93010L, 140643L,
#'                                   155871L, 36186L, 213242L, 69976L, 395211L,
#'                                   7886L, 5186L),
#'                           PCE = c(11926L, 41809L, 59668L, 33879L, 10217L, 29847L,
#'                                   50990L, 91914L, 7398L, 2442L, 22446L, 60297L,
#'                                   14046L, 7467L, 10643L, 13971L, 13623L, 15562L,
#'                                   7927L, 5521L, 21942L, 2406L, 4765L, 12460L, 3744L,
#'                                   5522L, 2101L, 1196L, 15798L, 2783L, 469361L,
#'                                   24746L, 22680L, 41345L, 21932L, 6797L, 16777L,
#'                                   2949L, 2627L, 11835L, 247038L, 6319L, 3906L,
#'                                   12042L, 29968L, 29840L, 3846L, 50444L, 14029L,
#'                                   106133L, 0L, 966L),
#'                            AP = c(14886L, 20424L, 33616L, 24573L, 9415L, 26422L,
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
#' seats.ex <- structure(list(UCD = c(3L, 2L, 3L, 4L, 3L, 3L, 3L, 5L, 2L, 2L, 3L,
#'                                    4L, 4L, 5L, 5L, 3L, 2L, 3L, 3L, 2L, 2L, 3L,
#'                                    3L, 4L, 2L, 3L, 2L, 3L, 3L, 2L, 5L, 1L, 1L,
#'                                    2L, 4L, 4L, 6L, 4L, 4L, 6L, 11L, 3L, 2L, 0L,
#'                                    2L, 4L, 2L, 4L, 2L, 5L, 1L, 1L),
#'                            PSOE = c(2L, 4L, 3L, 3L, 2L, 4L, 4L, 5L, 1L,
#'                                    1L, 3L, 4L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L,
#'                                    2L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 2L, 1L, 11L,
#'                                    2L, 1L, 1L, 3L, 1L, 2L, 0L, 0L, 1L, 11L, 2L,
#'                                    1L, 3L, 3L, 4L, 1L, 4L, 2L, 7L, 0L, 0L),
#'                            PCE = c(0L, 1L, 1L, 0L, 0L, 0L, 1L, 2L, 0L, 0L, 0L,
#'                                    1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
#'                                    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 7L, 0L, 0L,
#'                                    1L, 0L, 0L, 0L, 0L, 0L, 0L, 4L, 0L, 0L, 0L,
#'                                    0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L),
#'                            AP = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
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
#' p <- plot(example, show.plot = FALSE)
#' p
#'
plot.ElectDecomp <- function(x,
                             type = "decomposition",
                             panels.size = "global",
                             title.size = 12,
                             axis.title.size = 12,
                             summaries = "RI",
                             bar.width = 0.2,
                             bar.color = "skyblue",
                             text.color = "black",
                             text.size = 3,
                             labels.size = 10,
                             ticks.linewidth  = 0.2,
                             length.breaks = 0.02,
                             labels.y.size = 9,
                             labels.x.size,
                             parties,
                             party.names,
                             panels.title,
                             axis.title,
                             ...,
                             show.plot = TRUE){

  if (inherits(x, "distorsion.ElectDecomp")){
    if (type == "districts"){
      pintar_con <- plot_party_district
    } else {
      pintar_con <- plot_distorsion
    }
  } else if (inherits(x, "inequality.ElectDecomp")){
    pintar_con <- plot_inequality
  }


  p <- pintar_con(x,
                  panels.size = panels.size,
                  title.size = title.size,
                  axis.title.size = axis.title.size,
                  summaries = summaries,
                  bar.width = bar.width,
                  bar.color = bar.color,
                  text.color = text.color,
                  text.size = text.size,
                  labels.size = labels.size,
                  ticks.linewidth  = ticks.linewidth,
                  length.breaks = length.breaks,
                  parties = parties,
                  party.names = party.names,
                  panels.title = panels.title,
                  axis.title = axis.title,
                  labels.x.size = labels.x.size,
                  labels.y.size = labels.y.size,
                  ... = ...)

  if (show.plot) suppressWarnings(plot(p))
  return(p)
}

# PLOT DISTORSION DECOMPOSITION
plot_distorsion <- function(x,
                            ...,
                            parties,
                            panels.title,
                            party.names,
                            title.size = 12,
                            bar.width = 0.2,
                            bar.color = "skyblue",
                            text.color = "black",
                            text.size = 3,
                            labels.size = 10,
                            ticks.linewidth  = 0.2,
                            length.breaks = 0.02,
                            panels.size = "global"){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package gridExtra needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (missing(panels.title)){
    panels.title <- c("Total deviation", "Electoral system effect",
                      "Population effect", "Mean effect",
                      "Variance effect", "Malapportionment effect",
                      "Turnout effect")
  }
  if (missing(parties)){
    parties <- order(colSums(x$inputs$votes), decreasing = TRUE)
    if (length(parties) >= 6) parties <- parties[1L:6L]
  }
  if(!(panels.size %in% c("local", "global")))
    stop("The value for argument 'panels.size' is incorrect. Only 'local' and 'global' are allowed.")

  # Start
  bbdd0 <- x$party.distortions
  bbdd <- x$party.distortions[, parties]
  if (!missing(party.names)){
    if (length(party.names) != ncol(bbdd)){
      warning("The length of the argument 'party.names' differs of the number of plotted parties. So, 'party.names' is not used.")
    } else {
      colnames(bbdd) <- party.names
    }
  }
  if (panels.size == "global"){
    maximo <- apply(abs(bbdd0), 1, max)
  } else {
    maximo <- apply(abs(bbdd), 1, max)
  }
  maximo1 <- max(maximo[1], max(maximo[2], max(maximo[3] + maximo[4])) +
                   max(maximo[5], max(maximo[6] + maximo[7])) )

  # Anchuras paneles inferiores
  suma1 <- maximo[1]
  suma2 <- maximo[2] + maximo[5]
  suma3 <- maximo[3] + maximo[4] + maximo[6] + maximo[7]
  suma3a <- maximo[3] + maximo[4]
  suma3b <- maximo[6] + maximo[7]
  if (maximo1 == suma1){
    ancho2a <- maximo[2]*suma1/suma2
    ancho2b <- maximo[5]*suma1/suma2
    ancho3a <- maximo[3]*ancho2a/maximo[2]
    ancho3b <- maximo[4]*ancho2a/maximo[2]
    ancho3c <- maximo[6]*ancho2b/maximo[5]
    ancho3d <- maximo[7]*ancho2b/maximo[5]
  } else if (maximo1 == suma2){
    ancho3a <- maximo[3]*maximo[2]/suma3a
    ancho3b <- maximo[4]*maximo[2]/suma3a
    ancho3c <- maximo[6]*maximo[5]/suma3b
    ancho3d <- maximo[7]*maximo[5]/suma3b
  } else {
    ancho3a <- maximo[3]
    ancho3b <- maximo[4]
    ancho3c <- maximo[6]
    ancho3d <- maximo[7]
  }

  ancho <- ancho3a + ancho3b + ancho3c + ancho3d
  breaks <- seq(-length.breaks*50, length.breaks*50, by = length.breaks)

  ## Figure 1
  data1 <- data.frame(party = colnames(bbdd), value = bbdd[1, ])
  # Ordenar los nombres según el orden original
  data1$party <- factor(data1$party,
                        levels = rev(data1$party))
  if (maximo1 < 0.02){
    text.size0 <- 0
  } else {
    text.size0 <- text.size
  }
  breaks0 <- breaks[breaks > -maximo1 & breaks < maximo1]

  Figure1 <- ggplot2::ggplot(data1, ggplot2::aes(x = !!quote(party), y = !!quote(value))) +
    ggplot2::geom_bar(stat = "identity", fill = bar.color, width = bar.width) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(label = !!quote(party), y = 0,
                                    hjust = ifelse(!!quote(value) > 0, 1.1, -0.1)),
                       color = text.color, size = text.size0) + # Pone los nombres justo al lado del cero
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),  # Quita el texto en el eje Y
                   axis.title.y = ggplot2::element_blank(), # Quita el título del eje Y
                   plot.title = ggplot2::element_text(size = title.size),
                   axis.text.x = ggplot2::element_text(size = labels.size),
                   axis.ticks.x = ggplot2::element_line(color = "black", linewidth = ticks.linewidth),
                   axis.title.x = ggplot2::element_blank(), # Quita el título del eje X
                   panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
                   #                 plot.background = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1)
    ) +
    ggplot2::labs(y = " ", title = panels.title[1]) +
    ggplot2::scale_y_continuous(limits = c(-maximo1, maximo1),
                                breaks = breaks0,
                                expand = ggplot2::expansion(mult = c(0.02, 0.02)))


  ## Figure 2a
  data2a <- data.frame(party = colnames(bbdd), value = bbdd[2, ])
  # Ordenar los nombres según el orden original
  data2a$party <- factor(data2a$party,
                         levels = rev(data2a$party))

  maximo2a <- max(maximo[2], max(maximo[3] + maximo[4]))
  if (maximo2a < 0.02){
    text.size0 <- 0
  } else {
    text.size0 <- text.size
  }
  breaks0 <- breaks[breaks > -maximo2a & breaks < maximo2a]

  Figure2a <- ggplot2::ggplot(data2a, ggplot2::aes(x = !!quote(party), y = !!quote(value))) +
    ggplot2::geom_bar(stat = "identity", fill = bar.color, width = bar.width) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(label = !!quote(party), y = 0,
                                    hjust = ifelse(!!quote(value) > 0, 1.1, -0.1)),
                       color = text.color, size = text.size0) + # Pone los nombres justo al lado del cero
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),  # Quita el texto en el eje Y
                   axis.title.y = ggplot2::element_blank(), # Quita el título del eje Y
                   plot.title = ggplot2::element_text(size = title.size),
                   axis.text.x = ggplot2::element_text(size = labels.size),
                   axis.ticks.x = ggplot2::element_line(color = "black", linewidth = ticks.linewidth),
                   axis.title.x = ggplot2::element_blank(), # Quita el título del eje X
                   panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
                   #                plot.background = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1)
    ) +
    ggplot2::labs(y = " ", title = panels.title[2]) +
    ggplot2::scale_y_continuous(limits = c(-maximo2a, maximo2a),
                                breaks = breaks0,
                                expand = ggplot2::expansion(mult = c(0.02, 0.02)))


  ## Figure 2b
  data2b <- data.frame(party = colnames(bbdd), value = bbdd[5, ])
  # Ordenar los nombres según el orden original
  data2b$party <- factor(data2b$party,
                         levels = rev(data2b$party))
  maximo2b <- max(maximo[5], max(maximo[6] + maximo[7]))
  if (maximo2b < 0.02){
    text.size0 <- 0
  } else {
    text.size0 <- text.size
  }
  breaks0 <- breaks[breaks > -maximo2b & breaks < maximo2b]

  Figure2b <- ggplot2::ggplot(data2b, ggplot2::aes(x = !!quote(party), y = !!quote(value))) +
    ggplot2::geom_bar(stat = "identity", fill = bar.color, width = bar.width) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(label = !!quote(party), y = 0,
                                    hjust = ifelse(!!quote(value) > 0, 1.1, -0.1)),
                       color = text.color, size = text.size0) + # Pone los nombres justo al lado del cero
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),  # Quita el texto en el eje Y
                   axis.title.y = ggplot2::element_blank(), # Quita el título del eje Y
                   plot.title = ggplot2::element_text(size = title.size),
                   axis.text.x = ggplot2::element_text(size = labels.size),
                   axis.ticks.x = ggplot2::element_line(color = "black", linewidth = ticks.linewidth),
                   axis.title.x = ggplot2::element_blank(), # Quita el título del eje X
                   panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
                   #                 plot.background = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1)
    ) +
    ggplot2::labs(y = " ", title = panels.title[3]) +
    ggplot2::scale_y_continuous(limits = c(-maximo2b, maximo2b),
                                breaks = breaks0,
                                expand = ggplot2::expansion(mult = c(0.02, 0.02)))

  ## Figure 3a
  data3a <- data.frame(party = colnames(bbdd), value = bbdd[3, ])
  # Ordenar los nombres según el orden original
  data3a$party <- factor(data3a$party,
                         levels = rev(data3a$party))
  maximo3a <- maximo[3]
  if (maximo3a < 0.02){
    text.size0 <- 0
  } else {
    text.size0 <- text.size
  }
  breaks0 <- breaks[breaks > -maximo3a & breaks < maximo3a]

  Figure3a <- ggplot2::ggplot(data3a, ggplot2::aes(x = !!quote(party), y = !!quote(value))) +
    ggplot2::geom_bar(stat = "identity", fill = bar.color, width = bar.width) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(label = !!quote(party), y = 0,
                                    hjust = ifelse(!!quote(value) > 0, 1.1, -0.1)),
                       color = text.color, size = text.size0) + # Pone los nombres justo al lado del cero
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),  # Quita el texto en el eje Y
                   axis.title.y = ggplot2::element_blank(), # Quita el título del eje Y
                   plot.title = ggplot2::element_text(size = title.size),
                   axis.text.x = ggplot2::element_text(size = labels.size),
                   axis.ticks.x = ggplot2::element_line(color = "black", linewidth = ticks.linewidth),
                   axis.title.x = ggplot2::element_blank(), # Quita el título del eje X
                   panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
                   #                 plot.background = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1)
    ) +
    ggplot2::labs(y = " ", title = panels.title[4]) +
    ggplot2::scale_y_continuous(limits = c(-maximo3a, maximo3a),
                                breaks = breaks0,
                                expand = ggplot2::expansion(mult = c(0.02, 0.02)))


  ## Figure 3b
  data3b <- data.frame(party = colnames(bbdd), value = bbdd[4, ])
  # Ordenar los nombres según el orden original
  data3b$party <- factor(data3b$party,
                         levels = rev(data3b$party))
  maximo3b <- maximo[4]
  if (maximo3b < 0.02){
    text.size0 <- 0
  } else {
    text.size0 <- text.size
  }
  breaks0 <- breaks[breaks > -maximo3b & breaks < maximo3b]

  Figure3b <- ggplot2::ggplot(data3b, ggplot2::aes(x = !!quote(party), y = !!quote(value))) +
    ggplot2::geom_bar(stat = "identity", fill = bar.color, width = bar.width) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(label = !!quote(party), y = 0,
                                    hjust = ifelse(!!quote(value) > 0, 1.1, -0.1)),
                       color = text.color, size = text.size0) + # Pone los nombres justo al lado del cero
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),  # Quita el texto en el eje Y
                   axis.title.y = ggplot2::element_blank(), # Quita el título del eje Y
                   plot.title = ggplot2::element_text(size = title.size),
                   axis.text.x = ggplot2::element_text(size = labels.size),
                   axis.ticks.x = ggplot2::element_line(color = "black", linewidth = ticks.linewidth),
                   axis.title.x = ggplot2::element_blank(), # Quita el título del eje X
                   panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
                   #                 plot.background = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1)
    ) +
    ggplot2::labs(y = " ", title = panels.title[5]) +
    ggplot2::scale_y_continuous(limits = c(-maximo3b, maximo3b),
                                breaks = breaks0,
                                expand = ggplot2::expansion(mult = c(0.02, 0.02)))


  ## Figure 3c
  data3c <- data.frame(party = colnames(bbdd), value = bbdd[6, ])
  # Ordenar los nombres según el orden original
  data3c$party <- factor(data3c$party,
                         levels = rev(data3c$party))
  maximo3c <- maximo[6]
  if (maximo3c < 0.02){
    text.size0 <- 0
  } else {
    text.size0 <- text.size
  }
  breaks0 <- breaks[breaks > -maximo3c & breaks < maximo3c]

  Figure3c <- ggplot2::ggplot(data3c, ggplot2::aes(x = !!quote(party), y = !!quote(value))) +
    ggplot2::geom_bar(stat = "identity", fill = bar.color, width = bar.width) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(label = !!quote(party), y = 0,
                                    hjust = ifelse(!!quote(value) > 0, 1.1, -0.1)),
                       color = text.color, size = text.size0) + # Pone los nombres justo al lado del cero
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),  # Quita el texto en el eje Y
                   axis.title.y = ggplot2::element_blank(), # Quita el título del eje Y
                   plot.title = ggplot2::element_text(size = title.size),
                   axis.text.x = ggplot2::element_text(size = labels.size),
                   axis.ticks.x = ggplot2::element_line(color = "black", linewidth = ticks.linewidth),
                   axis.title.x = ggplot2::element_blank(), # Quita el título del eje X
                   panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
                   #                 plot.background = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1)
    ) +
    ggplot2::labs(y = " ", title = panels.title[6]) +
    ggplot2::scale_y_continuous(limits = c(-maximo3c, maximo3c),
                                breaks = breaks0,
                                expand = ggplot2::expansion(mult = c(0.02, 0.02)))


  ## Figure 3d
  data3d <- data.frame(party = colnames(bbdd), value = bbdd[7, ])
  # Ordenar los nombres según el orden original
  data3d$party <- factor(data3d$party,
                         levels = rev(data3d$party))
  maximo3d <- maximo[7]
  if (maximo3d < 0.02){
    text.size0 <- 0
  } else {
    text.size0 <- text.size
  }
  breaks0 <- breaks[breaks > -maximo3d & breaks < maximo3d]

  Figure3d <- ggplot2::ggplot(data3d, ggplot2::aes(x = !!quote(party), y = !!quote(value))) +
    ggplot2::geom_bar(stat = "identity", fill = bar.color, width = bar.width) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(label = !!quote(party), y = 0,
                                    hjust = ifelse(!!quote(value) > 0, 1.1, -0.1)),
                       color = text.color, size = text.size0) + # Pone los nombres justo al lado del cero
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),  # Quita el texto en el eje Y
                   axis.title.y = ggplot2::element_blank(), # Quita el título del eje Y
                   plot.title = ggplot2::element_text(size = title.size),
                   axis.text.x = ggplot2::element_text(size = labels.size),
                   axis.ticks.x = ggplot2::element_line(color = "black", linewidth = ticks.linewidth),
                   axis.title.x = ggplot2::element_blank(), # Quita el título del eje X
                   panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
                   #                 plot.background = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1)
    ) +
    ggplot2::labs(y = " ", title = panels.title[7]) +
    ggplot2::scale_y_continuous(limits = c(-maximo3d, maximo3d),
                                breaks = breaks0,
                                expand = ggplot2::expansion(mult = c(0.02, 0.02)))


  # Composition of plots
  p <- gridExtra::arrangeGrob(Figure1, Figure2a, Figure2b,
                              Figure3a, Figure3b, Figure3c, Figure3d,
                              layout_matrix = matrix(c(1, 1, 1, 1,
                                                       2, 2, 3, 3,
                                                       4, 5, 6, 7),
                                                     nrow = 3,
                                                     byrow = TRUE),
                              widths = c(ancho3a, ancho3b, ancho3c, ancho3d))


}

# PLOT INEQUALITY
plot_inequality <- function(x,
                            ...,
                            axis.title,
                            panels.title,
                            labels.size = 10,
                            bar.color = "skyblue",
                            bar.width = 0.2,
                            text.color = "black",
                            text.size = 3,
                            summaries = "RI",
                            title.size = 12,
                            ticks.linewidth  = 0.2,
                            axis.title.size = 12
                            ){

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (missing(axis.title)){
    axis.title <- c("Cumulative vote-share", "Cumulative seat-share")
  }

  if (missing(panels.title)){
    panels.title <- "Inequality (Lorenz) curve of the election"
  }
  if(!(summaries %in% c("RI", "none", "all")))
    stop("The value for argument 'summaries' is incorrect. Only 'RI', 'none' and 'all' are allowed.")

  data.text <- format(round(c(x$RI, x$other.statistics)*100, 1),
                      digits = 3L, decimal.mark = ".")
  posic <- data.frame(x = 0, y = c(0.98, 0.93, 0.88, 0.83))
  nombres <- paste0(c("RI (in %): ", "% Non-represented: ",
                   "% Underrepresented: ", "% Overrepresented: "),
                   data.text)
  if (summaries == "none"){
    nombres <- ""
  } else if (summaries == "RI"){
    nombres[2L:4L] <- ""
  }
  data.text <- data.frame(posic, nombres)

  data <- x$conversion.ratios
  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!quote(Cum.Prop.Votes),
                                          y = !!quote(Cum.Prop.Seats))) +
    ggplot2::geom_line(color = bar.color, size = 4*bar.width) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = title.size),
                   axis.text = ggplot2::element_text(size = 0.8*labels.size),
                   axis.ticks = ggplot2::element_line(color = "black", linewidth = ticks.linewidth),
                   axis.title = ggplot2::element_text(size = 1.05*labels.size),
                   panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
                   #                 plot.background = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1)
    ) +
    ggplot2::labs(x = axis.title[1L], y = axis.title[2L],
                  title = panels.title[1]) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.2),
                                expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.2),
                                expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1),
               color = "black", size = 2*bar.width) +
    ggplot2::geom_text(data = data.text,
                       ggplot2::aes(x = !!quote(x), y = !!quote(y),
                                    label = !!quote(nombres), hjust = 0),
                       color = text.color, size = text.size)
}

# PLOT DISTORSION PARTY-DISTRICT
organize_dataset <- function(x, parties, party.names, ...){

  if (missing(parties)){
    parties <- order(colSums(x$inputs$votes), decreasing = TRUE)
    if (length(parties) >= 6) parties <- parties[1L:6L]
  }

  deviations <- x$district2party.contributions
  if (!missing(party.names)){
    if (length(party.names) != ncol(deviations)){
      warning("The length of the argument 'party.names' differs of the number of plotted parties. So, 'party.names' is not used.")
    } else {
      colnames(deviations) <- party.names
    }
  }
  parties <- colnames(deviations)
  sizes <- rowSums(x$inputs$seats)
  deviations <- as.data.frame(cbind(sizes, deviations))
  sizes <- sort(unique(sizes))

  dataset <- data.frame(size = NULL, parties = NULL, values = NULL)
  for (sz in min(sizes):max(sizes)){
   if (sz %in% sizes){
     temp <- deviations[deviations$sizes == sz, ]
     temp <- colSums(temp[, -1])
   } else {
     temp <- rep(NA, length(parties))
   }
   temp <- cbind(rep(sz, length(parties)), parties, temp)
   colnames(temp) <- c("size", "parties", "values")
   dataset <- rbind(dataset, temp)
  }
  class(dataset[, 1L]) <- "integer"
  class(dataset[, 3L]) <- "numeric"

  orden.parties <- order(colSums(x$inputs$votes), decreasing = TRUE)
  dataset$parties <- factor(dataset$parties,
                            levels = dataset$parties[orden.parties])
  dataset$size <- factor(dataset$size,
                         levels = rev(min(dataset$size):max(dataset$size)))

  return(dataset)
}

plot_party_district <- function(x,
                            ...,
                            axis.title,
                            labels.x.size,
                            labels.y.size = 9,
                            bar.color = "skyblue",
                            bar.width = 0.4,
                            axis.title.size = 12,
                            title.size = 12
){

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }

  datos <- organize_dataset(x)

  if (missing(labels.x.size)){
    n.parties <- length(unique(datos$parties))
    labels.x.size <- max(12 - n.parties, 6)
  }

  if (missing(axis.title)){
    axis.title <- c("Seats-to-party deviation", "District size")
  }

  bar.width <- 2.5*bar.width
  max.val <- 1.02*abs(max(datos$values, na.rm = TRUE))

p <- suppressMessages(
  ggplot2::ggplot(data = datos, ggplot2::aes(x = !!quote(values),
                                             y = !!quote(size))) +
      ggplot2::geom_bar(stat = "identity", position = "dodge",
                        width = bar.width, fill = bar.color) +
      ggplot2::geom_vline(xintercept = 0, linetype = "solid", color = "black") +
      ggplot2::facet_grid(. ~ parties) +                             # Facets by columns (parties)
      ggplot2::theme_minimal() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(size = title.size, face = "bold"),  # Names of parties
        panel.grid.major.y = ggplot2::element_blank(),                         # Without horizontal lines
        axis.text.y = ggplot2::element_text(size = labels.y.size),             # Size labels axis y
        axis.ticks.y = ggplot2::element_blank(),                               # Without ticks axis y
        axis.text.x = ggplot2::element_text(size = labels.x.size, angle = 0),  # Size labels axis x
        axis.ticks.x = ggplot2::element_blank(),                               # Without ticks axis x
        strip.background = ggplot2::element_blank(),                           # Without strip.background
        plot.title = ggplot2::element_blank(),                                 # Without title
        axis.title = ggplot2::element_text(size = axis.title.size),
        legend.position = "none",                                              # Without legend
        panel.spacing = ggplot2::unit(1, "lines")                              # Space between facets
      ) +
      ggplot2::labs(x = axis.title[1L], y = axis.title[2L]) +
      ggplot2::scale_y_discrete(labels = function(x)
        ifelse(x %in% unique(datos$size[!is.na(datos$values)]), x, "")) +
      ggplot2::scale_x_continuous(
        limits = c(-max.val, max.val),
        expand = c(0, 0)
       # breaks = seq(-max_val, max_val),
#        labels = scales::comma_format(),  # More readable number format
      )
   )

}
