library(tidyverse)
library(magrittr)
library(ggraph)
library(igraph)
library(wordsintransition)

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

category_node_categories <- data_frame(
  node = c("glass", "tear", "water", "zipper"),
  category = c("glass", "tear", "water", "zipper")
)

# data("imitations")
imitations <- read_csv("~/Research/Telephone/words-in-transition/wordsintransition/data-raw/imitations.csv")
node_categories <- imitations %>%
  mutate(node = as.character(message_id)) %>%
  select(node, category = chain_name) %>%
  bind_rows(
    category_node_categories
  )

new_nodes <- left_join(nodes, node_categories)

# Add edge attributes as columns.
graph <- graph_from_data_frame(edges, vertices = new_nodes)
layout <- create_layout(graph, "dendrogram")

ggraph(layout) +
  geom_edge_diagonal(aes(edge_linetype = node2.node_type, color = node2.category), edge_width = 0.4) +
  geom_node_point(aes(shape = node_type, color = category), size = 0.6) +
  geom_node_text(aes(label = node_label), vjust = -0.5, size = 4) +
  scale_x_continuous("", breaks = NULL) +
  scale_y_continuous("generation", breaks = 0:8, labels = c(8:1, "seeds")) +
  scale_shape_manual(values = c(32, 32, 16, 32)) +
  scale_edge_linetype_manual(values = c("blank", "blank", "solid", "blank")) +
  coord_cartesian(ylim = c(0, 9)) +
  scale_edge_color_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  ggtitle("Repeated imitation makes human vocalizations more word-like") +
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  theme(
    legend.position = "none",
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.4, margin = margin(0, -6, 0, 0))
  )

# Caption: Participants repeated imitations of seed sounds along transmission
# chains. The imitations became more word-like the more times they were
# repeated.

ggsave("cover.png", width = 200, height = 200, units = "mm", dpi = 300)
ggsave("cover.pdf", width = 200, height = 200, units = "mm", dpi = 300)
