#' Function for converting a network adjacency matrix into the correct format for use with geomnet
#'
#' @param model An adjacency matrix of class \code{"adjmat"}.
#' @param data not used in this function
#' @param ...  not used in this function
#' @importFrom tidyr gather
#' @importFrom readr parse_number
#' @importFrom ggplot2 fortify
#' @examples
#' library(ggCompNet)
#' data(emon, package = "network")
#' adjmat <- geomnet::as.adjmat(network::as.matrix.network.adjacency(emon$MtSi))
#' str(adjmat)
#' ggCompNet:::fortify.adjmat(adjmat)
fortify.adjmat <- function(model, data = NULL,  ...){
  net <- model
  if (dim(net)[1] != dim(net)[2]){
    stop("Error: Please supply a square adjacency matrix.")
  }
  if (!is.null(rownames(net))){
    ID <- rownames(net)
  } else if (!is.null(colnames(net))){
    ID <- colnames(net)
  } else ID <- 1:ncol(net)
  net <- as.data.frame(net, stringsAsFactors = F)
  net$from <- ID
  #  introduce visible binding for global variables
  to <- NULL
  from <- NULL
  value <- NULL

  net <- tidyr::gather(net, to, value, -from)

  #net <- dplyr::filter(net, value > 0)
  net <- subset(net, value > 0)
  #net <- dplyr::mutate(net, edge.weight = value)
  net$edge.weight = net$value
  #  edge.data <- dplyr::select(net, from, to, edge.weight)
  edge.data <- net[, c("from", "to", "edge.weight")]

  #  net %>%
  #    tidyr::gather(to, value, -from) %>%
  #    dplyr::filter(value > 0) %>%
  #    dplyr::mutate(edge.weight = value) %>%
  #    dplyr::select(from, to, edge.weight) -> edge.data
  froms <- unique(edge.data$from)
  tos <- unique(edge.data$to)
  if (class(froms) != class(tos)){
    if (class(froms) %in% c("numeric", "integer")){
      tos <- readr::parse_number(tos)
    } else if (class(froms) == "factor" && class(tos) == "character"){
      froms <- as.character(froms)
    } else if (class(tos) == "factor" && class(froms) == "character"){
      tos <- as.character(tos)
    } else {stop("Error: Cannot match from and to columns. Please provide an\nadjacency matrix with row or column names.")}
  }
  allnodes <- sort( unique(
    c(unique(froms), unique(tos))
  ) )
  node.data <- data.frame(id = allnodes, stringsAsFactors = F)
  dat <- merge(edge.data, node.data, by.x = 'from', by.y = 'id', all = T)
  return(dat)
}
