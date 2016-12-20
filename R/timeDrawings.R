#' Time drawings of random graphs
#'
#' @param niter integer. How many times to repeat each drawing for each drawing method. Default is 100.
#' @param sizes integer vector. What size(s) of network (how many nodes) should be drawn? Default is \code{seq(250,25,-25)}.
#' @param eprob numeric. Value between 0-1. Edge probability of random graph. Default is .2.
#' @param wd character. Working directory where you want to store the timing results. Default is current working directory.
#' @param newpack character. Name of additional package (or function) that you wish to compare to \pkg{igraph}, \pkg{network}, \code{\link[GGally]{ggnet2}}, \pkg{geomnet}, and \pkg{ggnetwork}.
#' @param classnew character. Class of object taken to be plotted with the new method. (think \code{"igraph"}, \code{"network"}, etc.)
#' @param newcode expression. The code required to plot the random graph using the \code{newpack} method. Must take an object called \code{n} for plotting.
#'
#' @importFrom grDevices dev.off
#' @importFrom graphics plot
#' @importFrom utils write.csv
#' @importFrom sna rgraph
#' @importFrom igraph graph_from_adjacency_matrix plot.igraph
#' @importFrom network network plot.network as.matrix.network.adjacency
#' @importFrom progress progress_bar
#' @importFrom GGally ggnet2
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggnetwork ggnetwork geom_edges geom_nodes
#' @importFrom geomnet as.adjmat geom_net
#' @export
timeDrawings <- function(niter = 100, sizes = seq(250,25,-25), eprob = .2, wd = "./",
                         newpack = NULL, classnew = NULL, newcode = NULL){
  pb <- progress::progress_bar$new(
         format = "  drawing [:bar] :percent eta: :eta",
         total = 100, clear = FALSE, width= 60)
  if (is.null(newpack) + is.null(classnew) + is.null(newcode) %in% c(1,2)) {
    stop("Error: newpack, classnew, and newcode must all be NULL or non-NULL.
         Please provide arguments to all three or none of them.")
  }
  # create niter/10 files per size
  if (niter %% 10 != 0) {
    stop("niter must be a multiple of 10")
  }
  for (k in 0:(niter/10-1)) {

    # the different sizes
    for (i in sizes) {

      f = paste0(wd, "runtimes-", sprintf("%04.0f", i), "-", k, ".csv")

      if (!file.exists(f)) {

        d = data.frame()

        # in each file store the runtimes of plotting 10 random graphs
        # do this for each of the 5 methods
        for (j in 1:10) {

          r = sna::rgraph(i, tprob = eprob)

         # cat("Network size", i,
          #    "iteration", sprintf("%3.0f", 10 * k + j), "/ 100\n")

          n = igraph::graph_from_adjacency_matrix(r, mode = "undirected")

          t1 = system.time({
            igraph::plot.igraph(n, vertex.label = NA)
          })[1]

          n = network::network(r, directed = FALSE)

          t2 = system.time({
            network::plot.network(n)
          })[1]

          t3 = system.time({
            print(GGally::ggnet2(n))
          })[1]

          from = NULL
          to = NULL
          e = fortify.adjmat(geomnet::as.adjmat(network::as.matrix.network.adjacency(n)))

          t4 = system.time({
            print(ggplot2::ggplot(data = e) +
                    geomnet::geom_net(ggplot2::aes(from_id = from, to_id = to)))
          })[1]

          x = NULL
          y = NULL
          xend = NULL
          yend = NULL
          t5 = system.time({
            print(ggplot2::ggplot(ggnetwork::ggnetwork(n),
                         ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
                    ggnetwork::geom_edges() +
                    ggnetwork::geom_nodes())
          })[1]

          if (is.null(newpack)){
            d = rbind(d, data.frame(
              network_size = i,
              iteration = 10 * k + j,
              igraph = t1,
              network = t2,
              ggnet2 = t3,
              geomnet = t4,
              ggnetwork = t5,
              row.names = NULL
            ))
          } else{
            if ("igraph" %in% classnew){
              n = igraph::graph_from_adjacency_matrix(r, mode = "undirected")
            } else
              if ("network" %in% classnew){
              n = network::network(r, directed = FALSE)
              }

            t6 = system.time(print(eval(newcode)))

            d = rbind(d, data.frame(
              network_size = i,
              iteration = 10 * k + j,
              igraph = t1,
              network = t2,
              ggnet2 = t3,
              geomnet = t4,
              ggnetwork = t5,
              newpack = t6,
              row.names = NULL
            ))
          }



          dev.off()
          pb$tick()
        }

        write.csv(d, f, row.names = FALSE)

      }

    }
  }
}
