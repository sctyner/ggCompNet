#' Time drawings of random graphs
#'
#'
#'
#'
#'
#' @export
timeDrawings <- function(niter = 100, sizes = seq(250,25,-25), wd = "./", newpack = NULL){
  pb <- progress::progress_bar$new(
         format = "  drawing [:bar] :percent eta: :eta",
         total = 100, clear = FALSE, width= 60)
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

          r = sna::rgraph(i, tprob = 0.2)

         # cat("Network size", i,
          #    "iteration", sprintf("%3.0f", 10 * k + j), "/ 100\n")

          n = igraph::graph_from_adjacency_matrix(r, mode = "undirected")

          t1 = system.time({
            plot(n, vertex.label = NA)
          })[1]

          n = network::network(r, directed = FALSE)

          t2 = system.time({
            plot.network(n)
          })[1]

          t3 = system.time({
            print(ggnet2(n))
          })[1]

          e = fortify(as.adjmat(network::as.matrix.network.adjacency(n)))

          t4 = system.time({
            print(ggplot(data = e) +
                    geom_net(aes(from_id = from, to_id = to)))
          })[1]

          t5 = system.time({
            print(ggplot(ggnetwork(n),
                         aes(x, y, xend = xend, yend = yend)) +
                    geom_edges() +
                    geom_nodes())
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
            t6 = system.time()
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
