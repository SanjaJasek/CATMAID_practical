library(igraph)
library(visNetwork)

Okabe_Ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
  "#CC79A7", "#000000"
)

INNOS <- read.neurons.catmaid("celltype7", pid = 11)
cPRC <- read.neurons.catmaid("celltype5", pid = 11)
INRGW <- read.neurons.catmaid("celltype6", pid = 11)

# cell types
celltypes <- list(
  INNOS, cPRC, INRGW
)
celltype_names <- list(
  "INNOS", "cPRC", "INRGW"
)

# iterate through cell group neuron lists and get connectivity
# define empty synapse list with the right dimensions
synapse_list <- c()

for (i in 1:length(celltypes)) {
  for (j in 1:length(celltypes)) {
    # get connectors between two cell groups
    presyn_skids <- attr(celltypes[i][[1]], "df")$skid
    postsyn_skids <- attr(celltypes[j][[1]], "df")$skid
    connectivity <- catmaid_get_connectors_between(
      pre = presyn_skids,
      post = postsyn_skids, pid = 11
    )
    # check the number of synapses from group1 -> group2
    N_synapses <- dim(connectivity)[1]
    if(length(connectivity) == 0) {N_synapses = 0}
    # add value to synapse list
    synapse_list <- c(synapse_list, N_synapses)
  }
}
synapse_list
# convert synapse list into a matrix of appropriate dimensions
synapse_matrix <- matrix(
  unlist(synapse_list), byrow = TRUE, 
  nrow = length(celltypes)
)

rownames(synapse_matrix) <- as.character(celltype_names)
colnames(synapse_matrix) <- as.character(celltype_names)

# with the make_graph function of igraph we turn it into a graph (input is the list of edge pairs)
celltype_graph <- graph_from_adjacency_matrix(
  synapse_matrix,
  mode = c("directed"),
  weighted = TRUE, diag = TRUE
)

# calculate node weighted degree -------------
degree=degree(
  graph, v = V(graph), mode = c("all"), 
  loops = TRUE, normalized = FALSE
)


## convert to VisNetwork-list
celltype_graph.visn <- toVisNetworkData(celltype_graph)

#filter low-weight edges
celltype_graph.visn$edges$weight
celltype_graph.visn$edges$weight <- sqrt(celltype_graph.visn$edges$weight)
celltype_graph.visn$edges$weight

## copy column "weight" to new column "value" in list "edges"
celltype_graph.visn$edges$value <- celltype_graph.visn$edges$weight
celltype_graph.visn$nodes$value <- degree

#define node color
celltype_graph.visn$nodes$color <- Okabe_Ito[1:3]

#hierarchical layout - define level of nodes
celltype_graph.visn$nodes$level <- c(2, 4, 1)

#hierarchical layout
visNet <- visNetwork(celltype_graph.visn$nodes, celltype_graph.visn$edges) %>%
  visIgraphLayout(
    layout = "layout_nicely", physics = FALSE
  ) %>%
  visEdges(
    smooth = list(type = 'curvedCW', roundness=0.2),
    scaling=list(min=2, max=12),
    color = list(inherit=TRUE, opacity=0.7),
    arrows = list(
      to = list(enabled = TRUE, 
                scaleFactor = 1, type = 'arrow'))
  )
visNet

