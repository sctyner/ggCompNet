#' Runtime results from protein example
#' @format a data frame consisting of 100 comparison runs across five different packages
#' \itemize{
#'   \item iteration iteration number.
#'   \item igraph runtime under the \pkg{igraph} package.
#'   \item network runtime under the \pkg{network} package.
#'   \item ggnet2 runtime under the \code{GGally::}\code{\link[GGally]{ggnet2}} function.
#'   \item geomnet runtime under the \pkg{geomnet} package.
#'   \item ggnetwork runtime under the \pkg{ggnetwork} package.
#' }
"runtimes_protein"


#' Runtime comparison
#'
#' @format a data frame consisting of 50000 comparison runs across ten different systems,
#' ten different network sizes, and five different packages of 100 iterations each.
#' \itemize{
#'   \item setup character. Description of computer system running the code.
#'   \item network_size. Size of the network (25, 50, 75, 100, 125, 150, 175, 200, 225, or 250 nodes)
#'   \item iteration. Integer 1-100.
#'   \item `Visualization approach`. Method used. (\code{"igraph"}, \code{"network"}, \code{"ggnet2"}, \code{"geomnet"}, or \code{"ggnetwork"})
#'   \item time. Time taken to plot graph in seconds.
#' }
"compare_all"
