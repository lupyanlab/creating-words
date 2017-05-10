source("R/setup.R")

# ---- dendrogram
library(ggraph)
library(igraph)

data("edges")

# Create a data frame of unique nodes from all edges
# and add node attributes as columns.
categories <- c("glass", "tear", "water", "zipper")
node_type_map <- data_frame(
  node = c("root", categories),
  node_type = c("root", rep("category", 4))
)

filler_edges <- edges %>%
  filter(edge_type == "invis")
filler_nodes <- data_frame(node = unique(filler_edges$y)) %>%
  mutate(node_type = "filler", node_label = "")

node_type_map %<>% bind_rows(filler_nodes)

nodes <- data_frame(node = unique(c(edges$x, edges$y))) %>%
  left_join(node_type_map) %>%
  mutate(
    node_type = ifelse(is.na(node_type), "message", node_type),
    node_label = ifelse(node %in% categories, node, "")
  )

# Add edge attributes as columns.
graph <- graph_from_data_frame(edges, vertices = nodes)
layout <- create_layout(graph, "dendrogram")

gg_dendrogram <- ggraph(layout) +
  geom_edge_diagonal(aes(edge_linetype = node2.node_type)) +
  geom_node_point(aes(shape = node_type), size = 0.8) +
  geom_node_text(aes(label = node_label), vjust = -0.5) +
  scale_x_continuous("", breaks = NULL) +
  scale_y_continuous("Generation", breaks = 0:8, labels = c(8:1, "seeds")) +
  scale_shape_manual(values = c(32, 32, 16, 32)) +
  scale_edge_linetype_manual(values = c("blank", "blank", "solid", "blank")) +
  ggtitle("Imitations collected in the transmission chain experiment") +
  base_theme +
  theme(
    legend.position = "none",
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# ggsave("~/Desktop/dendrogram.png", gg_dendrogram, height = 4, width = 6)