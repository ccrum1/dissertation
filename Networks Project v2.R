library(ggraph)
library(readxl)
library(igraph)
library(tidyverse)
file_path <- "insert file path here"
edge_list <- read_excel(file_path)
colnames(edge_list) <- c("id_a", "id_b", "weight", "year", "type")
#create the graph object using igraph functions 
graph <- graph_from_data_frame(d = edge_list, directed = FALSE)

#calculate the degree of each node, the number of nodes with degree 0, and the top highest degree nodes
node_degrees <- degree(graph)
num_isolated <- sum(node_degrees == 1)

#determine the top X number of nodes with the highest degree
node_degrees <- degree(graph)
top_nodes <- order(node_degrees, decreasing = TRUE)

#identify graph components
components <- components(graph)
largest_component <- which.max(components$csize)
filtered_vertices <- V(graph)[components$membership == largest_component]
filtered_graph <- induced_subgraph(graph, filtered_vertices)


#remove any isolate nodes
graph <- delete_vertices(graph, V(graph)[degree(graph) == 1])

#display the graph with a Fruchterman-Reingold Layout
#another option is "graphopt"
#insert filtered graph if desired where "graph" presently is
ggraph(graph, layout = "fr") +
  #plot the edges with width indicating weight
  geom_edge_link(aes(edge_width = weight / 10), edge_colour = "gray") + 
  #plot the nodes
  geom_node_point(size = 2, colour = "red") +
  #not necessary
  geom_node_text(aes(label = name), vjust = 1.5, hjust = 1.5) +
  #removes the background grid and axes
  theme_void() +
  ggtitle("Filtered Graph - Largest Component Only")

#generate an E-R random graph with set nodes and edges for comparison
num_nodes1 <- 891
edge_probability <- 0.3
er_graph_1 <- erdos.renyi.game(n = num_nodes1, p = edge_probability, type = "gnp", directed = FALSE, loops = FALSE)
average_path_length2 <- mean_distance(er_graph_1, directed = FALSE, unconnected = TRUE)
average_cluster_coefficient1 <- transitivity(er_graph_1, type = "average")

#generate a graph with a lattice structure for comparison
g <- graph.lattice( c(5, 6) )
#insert the avg. degree of the real-world graph as k
g <- connect(g, 4)
plot(g)
average_clustering1 <- transitivity(g, type = "average")
print(average_clustering1)