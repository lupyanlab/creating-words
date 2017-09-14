library(tidyverse)
library(stringr)
library(magrittr)

library(grid)
library(gridExtra)
library(png)

library(lme4)  # lmerTest is also required
library(AICcmodavg)
library(broom)

library(ggraph) # igraph is also required

library(crotchet)
library(wordsintransition)

## Theme ##

base_theme <- theme_minimal(base_size = 18, base_family = "Helvetica") +
  theme(plot.title = element_text(face = "bold"))

make_colors <- function() {
  colors <- RColorBrewer::brewer.pal(4, "Set2")
  names(colors) <- c("blue", "orange", "green", "pink")
  function(...) unname(colors[c(...)])
}
colors <- make_colors()

question_types <- c("True seed", "Category match", "Specific match")
question_type_colors <- colors("green", "blue", "orange")
generation_colors <- colors("green", "blue")

scales <- list(
  y = list(
    rt = scale_y_continuous("Reaction time (msec)"),
    accuracy = scale_y_continuous("Accuracy", breaks = seq(0, 1, by = 0.1),
                                  labels = scales::percent)
  ),
  color = list(
    message_type = scale_color_manual(
      "Transcription of",
      labels = c("First generation imitation", "Last generation imitation"),
      values = colors("green", "blue")
    )
  ),
  linetype = list(
    message_type = scale_linetype_manual(
      "Transcription of",
      labels = c("First generation imitation", "Last generation imitation"),
      values = c("solid", "longdash")
    )
  )
)

sizes <- list(
  point = 3,
  line = 2
)

## Dendrogram ##

data("edges")

# Create a dataframe of unique nodes from all edges
# and add node attributes as columns.
categories <- c("glass", "tear", "water", "zipper")
node_type_map <- data_frame(
  node = c("root", categories),
  node_type = c("root", rep("category", 4))
)

# Create filler nodes for filler edges
filler_edges <- edges %>%
  filter(edge_type == "invis")
filler_nodes <- data_frame(node = unique(filler_edges$y)) %>%
  mutate(node_type = "filler", node_label = "")
node_type_map %<>% bind_rows(filler_nodes)

# Create a dataframe of all nodes
nodes <- data_frame(node = unique(c(edges$x, edges$y))) %>%
  left_join(node_type_map) %>%
  mutate(
    node_type = ifelse(is.na(node_type), "message", node_type),
    node_label = ifelse(node %in% categories, node, "")
  )

# Create graph from nodes and edges
graph <- igraph::graph_from_data_frame(edges, vertices = nodes)

# Plot the graph with ggraph
gg_dendrogram <- ggraph(graph, "dendrogram") +
  geom_edge_diagonal(aes(edge_linetype = node2.node_type), edge_width = 0.6) +
  geom_node_point(aes(shape = node_type), size = 0.8) +
  geom_node_text(aes(label = node_label), vjust = -0.2, size = 7) +
  scale_x_continuous("", breaks = NULL) +
  scale_y_continuous("Generation", breaks = 0:8, labels = c(8:1, "seeds")) +
  scale_shape_manual(values = c(32, 32, 16, 32)) +
  scale_edge_linetype_manual(values = c("blank", "blank", "solid", "blank")) +
  coord_cartesian(ylim = c(0, 9)) +
  base_theme +
  theme(
    legend.position = "none",
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(hjust = 0.4, margin = margin(0, -6, 0, 0))
  )

## Imitations ##

data("imitations")

## Acoustic similarity judgments ##

data("acoustic_similarity_judgments")

acoustic_similarity_judgments %<>%
  mutate(similarity = ifelse(similarity == -1, NA, similarity)) %>%
  z_score_by_subj() %>%
  recode_edge_generations() %>%
  determine_trial_id()

seed_id_map <- imitations %>%
  select(message_id, seed_id)

acoustic_similarity_judgments %<>%
  left_join(seed_id_map, by = c("sound_x" = "message_id"))

similarity_judgments_mod <- lmer(
  similarity_z ~ edge_generation_n +
    (edge_generation_n|name) + (edge_generation_n|category/seed_id),
  data = acoustic_similarity_judgments
)

similarity_judgments_preds <- data_frame(edge_generation_n = 1:7) %>%
  cbind(., predictSE(similarity_judgments_mod, newdata = ., se = TRUE)) %>%
  rename(similarity_z = fit, se = se.fit) %>%
  recode_edge_generations()

similarity_judgments_means <- acoustic_similarity_judgments %>%
  group_by(edge_generations, category) %>%
  summarize(similarity_z = mean(similarity_z, na.rm = TRUE)) %>%
  recode_edge_generations()

set.seed(949)
gg_similarity_judgments <- ggplot(similarity_judgments_means) +
  aes(x = edge_generations, y = similarity_z) +
  geom_point(aes(color = category, shape = category),
             position = position_jitter(0.1, 0.0),
             size = sizes$point) +
  geom_smooth(aes(group = 1, ymin = similarity_z - se, ymax = similarity_z + se),
              data = similarity_judgments_preds, stat = "identity",
              alpha = 0.2, color = "gray",
              size = sizes$line) +
  scale_x_discrete("Generation") +
  scale_y_continuous("Acoustic similarity judgment (z-score)") +
  scale_color_brewer("", palette = "Set2") +
  scale_shape_discrete("") +
  coord_cartesian(ylim = c(-0.6, 0.8)) +
  base_theme +
  theme(
    legend.position = c(0.2, 0.89),
    legend.key.size = unit(2, "lines"),
    axis.title.y = element_text(margin = margin(0, -2, 0, 0))
  )

## Algorithmic similarity ##

# Compare within and between similarity
data("algo_linear")
data("algo_between_consecutive")

z_score <- function(x) (x - mean(x))/sd(x)

algo_linear %<>%
  recode_edge_generations()

algo_between_consecutive %<>%
  recode_edge_generations()

algo_similarity <- bind_rows(
  within = algo_linear,
  between = algo_between_consecutive,
  .id = "type"
) %>%
  mutate(
    type_c = ifelse(type == "within", -0.5, 0.5),
    similarity_z = z_score(similarity)
  )

algo_similarity_mod <- lm(similarity_z ~ edge_generation_n * type_c,
                          data = algo_similarity)

algo_similarity_preds <- expand.grid(
    edge_generation_n = unique(algo_similarity$edge_generation_n),
    type_c = c(-0.5, 0.5)
  ) %>%
  cbind(., predict(algo_similarity_mod, newdata = ., se = TRUE)) %>%
  rename(similarity_z = fit, se = se.fit) %>%
  mutate(
    type = factor(type_c, levels = c(-0.5, 0.5), labels = c("within", "between"))
  ) %>%
  recode_edge_generations()

gg_algo_similarity <- ggplot(algo_similarity_preds) +
  aes(edge_generations, similarity_z, color = type, group = type) +
  geom_smooth(aes(ymin = similarity_z - se, ymax = similarity_z + se),
              stat = "identity", size = sizes$line, alpha = 0.2) +
  geom_point(stat = "summary", fun.y = "mean", data = algo_similarity,
             size = sizes$point) +
  scale_x_discrete("Generations") +
  scale_y_continuous("Algorithmic similarity (MFCCs)") +
  scale_color_manual("", labels = c("Within chain", "Between chain"),
                     values = colors("blue", "green")) +
  base_theme +
  theme(legend.position = "top")

## Correlation between subjective judgments and algorithmic similarity

edge_similarities <- 
  merge(
    acoustic_similarity_judgments %>%
      group_by(sound_x, sound_y) %>%
      summarize(similarity_judgments = mean(similarity_z, na.rm = TRUE)),
    acoustic_similarity_linear %>%
      mutate(similarity_z = z_score(similarity)) %>%
      group_by(sound_x, sound_y) %>%
      summarize(similarity_algorithmic = mean(similarity_z, na.rm = TRUE))
  )

gg_comparing_similarities <- ggplot(edge_similarities) +
  aes(similarity_judgments, similarity_algorithmic) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous("Similarity judgments") +
  scale_y_continuous("Algorithmic similarity") +
  base_theme

## Transcriptions ##

data("transcription_matches")

transcription_examples <- transcription_matches %>%
  recode_message_type() %>%
  filter(!str_detect(word, "[\ *-]")) %>%
  mutate(word = str_to_lower(word)) %>%
  group_by(word_category, seed_id, message_id, word, message_type) %>%
  summarize(match_accuracy = mean(is_correct)) %>%
  ungroup() %>%
  group_by(word_category, seed_id, message_type) %>%
  arrange(desc(match_accuracy), seed_id, message_type) %>%
  mutate(rank = 1:n()) %>%
  filter(rank == 1) %>%
  arrange(word_category, message_type) %>%
  select(-message_id, -match_accuracy) %>%
  spread(message_type, word) %>%
  ungroup() %>%
  select(-rank) %>%
  group_by(word_category) %>%
  mutate(seed_id = 1:n()) %>%
  ungroup() %>%
  select(
    Category = word_category,
    Seed = seed_id,
    `First generation` = first_gen_imitation,
    `Last generation` = last_gen_imitation
  )

## Transcription distances ##

data("transcription_distances")

gen_labels <- select(imitations, message_id, generation)
message_type_labels <- select(imitations, message_id, seed_id, generation)
message_id_map <- select(imitations, message_id, chain_name, seed_id, generation)

seed_distances <- transcription_distances %>%
  left_join(message_type_labels) %>%
  recode_message_type() %>%
  filter(!is.na(message_type))

transcription_distances %<>%
  left_join(message_id_map) %>%
  recode_transcription_frequency() %>%
  recode_message_type() %>%
  filter(message_type != "sound_effect")

transcription_distances %<>%
  mutate(category = chain_name)

orthographic_distance_mod <- lmer(
  distance ~ message_c + (message_c|category/seed_id),
  data = transcription_distances
)

orthographic_distance_preds <- data_frame(message_c = c(-0.5, 0.5)) %>%
  cbind(., predictSE(orthographic_distance_mod, newdata = ., se = TRUE)) %>%
  rename(distance = fit, se = se.fit) %>%
  recode_message_type()

set.seed(733)
gg_distance <- ggplot(transcription_distances) +
  aes(message_label, distance, color = message_label) +
  geom_point(aes(group = message_id),
             stat = "summary", fun.y = "mean",
             position = position_jitter(0.1, 0.01),
             size = sizes$point, alpha = 0.8) +
  geom_errorbar(aes(ymin = distance - se, ymax = distance + se),
                data = orthographic_distance_preds,
                size = sizes$line, width = 0.3) +
  scale_x_discrete("Generation", labels = c("First", "Last")) +
  scale_y_continuous("Orthographic distance", breaks = seq(0, 1, by = 0.2)) +
  scale_color_manual(values = generation_colors) +
  scale_fill_manual(values = generation_colors) +
  coord_cartesian(ylim = c(0.0, 0.8)) +
  base_theme +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

# Transcription distance of seeds

seed_distance_mod <- lmer(distance ~ message_type + (message_type|seed_id),
                          data = seed_distances)
seed_distance_preds <- data_frame(message_type = c("sound_effect", "first_gen_imitation", "last_gen_imitation")) %>%
  cbind(., predictSE(seed_distance_mod, newdata = .)) %>%
  rename(distance = fit, se = se.fit) %>%
  recode_message_type()

gg_seed_distance <- ggplot(seed_distances) +
  aes(message_label, distance, fill = message_label) +
  geom_bar(stat = "summary", fun.y = "mean", alpha = 0.8) +
  geom_errorbar(aes(ymin = distance - se, ymax = distance + se),
                data = seed_distance_preds, width = 0.2,
                size = sizes$line) +
  xlab("") +
  ylab("Distance between transcriptions") +
  scale_fill_brewer(palette = "Set2") +
  base_theme +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())

## Imitation matches ##

data("imitation_matches")

imitation_matches %<>%
  filter(question_type != "catch_trial") %>%
  recode_generation() %>%
  recode_survey_type() %>%
  add_chance()

imitation_matches_mod <- glmer(
  is_correct ~ offset(chance_log) + generation_1 * (same_v_between + same_v_within) +
    (generation_1|chain_name/seed_id) + (1|subj_id),
  family = "binomial", data = imitation_matches
)

imitation_matches_preds <- expand.grid(
  generation_1 = unique(imitation_matches$generation_1) %>% na.omit(),
  survey_type = c("between", "same", "within"),
  stringsAsFactors = FALSE
) %>%
  recode_survey_type() %>%
  mutate(
    generation = generation_1 + 1,
    generation_label = paste("Generation", generation)
  ) %>%
  add_chance() %>%
  cbind(., predictSE(imitation_matches_mod, newdata = ., se = TRUE)) %>%
  rename(is_correct = fit, se = se.fit)

gg_match_to_seed <- ggplot(imitation_matches) +
  aes(x = generation_1, y = is_correct) +
  geom_smooth(aes(ymin = is_correct - se, ymax = is_correct + se,
                  color = survey_type, linetype = survey_type),
              stat = "identity", data = imitation_matches_preds,
              size = sizes$line) +
  scale_x_continuous("Generation", breaks = 0:11, labels = 1:12) +
  scales$y$accuracy +
  scale_color_manual("", values = question_type_colors, labels = question_types) +
  scale_linetype_manual(
    "",
    values = c("longdash", "dotdash", "solid"),
    labels = question_types
  ) +
  geom_hline(yintercept = 0.25, lty = 2, alpha = 0.4, size = sizes$line) +
  coord_cartesian(xlim = c(-0.2, 7.2), ylim = c(0.11, 0.75)) +
  base_theme +
  theme(
    legend.position = c(0.5, 0.95),
    legend.key.width = unit(3.2, "lines"),
    legend.key.size = unit(0.6, "lines"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

## Transcription matches ##

data("transcription_matches")

transcription_match_failed_catch_trial <- transcription_matches %>%
  filter(question_type == "catch_trial", is_correct == 0) %>%
  .$subj_id %>%
  unique()

transcription_matches %<>%
  recode_question_type() %>%
  recode_message_type() %>%
  recode_version() %>%
  add_chance() %>%
  filter(
    message_type != "sound_effect",
    !(subj_id %in% transcription_match_failed_catch_trial)
  )

transcription_matches_mod <- glmer(
  is_correct ~ offset(chance_log) + question_c * message_c + (question_c * message_c|subj_id),
  family = binomial, data = transcription_matches
)

message_labels <- data_frame(
  message_type = c("first_gen_imitation", "last_gen_imitation"),
  message_label_2 = c("First generation transcription",
                      "Last generation transcription")
)

transcription_matches_preds <- expand.grid(
    question_c = c(-0.5, 0.5),
    message_c = c(-0.5, 0.5)
  ) %>%
  add_chance() %>%
  cbind(., predictSE(transcription_matches_mod, newdata = ., se = TRUE)) %>%
  rename(is_correct = fit, se = se.fit) %>%
  recode_question_type() %>%
  recode_message_type() %>%
  left_join(message_labels)

dodger = position_dodge(width = -0.1)
gg_match_transcriptions <- ggplot(transcription_matches_preds) +
  aes(message_c, is_correct) +
  geom_line(aes(color = question_type, linetype = question_type),
            size = sizes$line, position = dodger) +
  geom_errorbar(aes(color = question_type, ymin = is_correct - se, ymax = is_correct + se),
                width = 0.2, position = dodger, size = sizes$line) +
  scale_x_continuous("Generation", breaks = c(-0.5, 0.5), labels = c("First", "Last")) +
  scales$y$accuracy +
  scale_color_manual("Question type", values = colors("blue", "green"),
                     labels = c("Category match", "True seed"),
                     guide = guide_legend(reverse = TRUE)) +
  scale_linetype_manual("Question type", values = c("dotdash", "longdash"),
                        labels = c("Category match", "True seed"),
                        guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0.25, lty = 2, alpha = 0.4, size = sizes$line) +
  coord_cartesian(ylim = c(0.11, 0.75), xlim = c(-1, 1)) +
  base_theme +
  theme(legend.position = c(0.5, 0.75),
        legend.key.width = unit(2, "lines"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

## Seed transcription matches ##

data("transcription_matches")

transcription_matches %<>%
  recode_question_type() %>%
  recode_message_type() %>%
  recode_version() %>%
  add_chance() %>%
  filter(!(subj_id %in% transcription_match_failed_catch_trial))

transcription_matches_mod <- glmer(
  is_correct ~ offset(chance_log) + question_c * message_type + (1|subj_id),
  family = binomial, data = transcription_matches
)

transcription_matches_preds <- expand.grid(
  question_c = c(-0.5, 0.5),
  message_type = c("sound_effect", "first_gen_imitation", "last_gen_imitation"),
  stringsAsFactors = FALSE
) %>%
  add_chance() %>%
  cbind(., predictSE(transcription_matches_mod, newdata = .)) %>%
  rename(is_correct = fit, se = se.fit) %>%
  recode_message_type() %>%
  recode_question_type()

gg_seed_matching <- ggplot(transcription_matches_preds) +
  aes(question_c, is_correct) +
  geom_bar(aes(fill = question_type), stat = "identity",
           width = 0.95, alpha = 0.6) +
  geom_errorbar(aes(ymin = is_correct - se, ymax = is_correct + se),
                data = transcription_matches_preds,
                width = 0.2) +
  facet_wrap("message_label", strip.position = "bottom") +
  scale_x_continuous("", breaks = c(-0.5, 0.5), labels = c("True seed", "Category match")) +
  scales$y$accuracy +
  scale_fill_manual("", values = colors("blue", "green")) +
  coord_cartesian(ylim = c(0.18, 0.71)) +
  geom_hline(yintercept = 0.25, lty = 2, alpha = 0.4, size = 0.6) +
  base_theme +
  theme(legend.position = "none",
        strip.placement = "outside",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size = 8))

## Learning sound names ##

data("learning_sound_names")

learning_sound_names %<>%
  mutate(rt = ifelse(is_correct == 1, rt, NA),
         is_error = 1 - is_correct) %>%
  mutate(word_category_by_block_ix = paste(word_category, block_ix, sep = ":")) %>%
  recode_lsn_word_type() %>%
  mutate(
    block_ix_sqr = block_ix^2
  )

lsn_outliers <- c("LSN102", "LSN148", "LSN104", "LSN147")
learning_sound_names %<>% filter(!(subj_id %in% lsn_outliers))

first_last_gen <- filter(learning_sound_names, message_type != "sound_effect") %>%
  mutate(block_ix_sqr = block_ix^2)

## Error ceiling ##

n_errors <- first_last_gen %>%
  group_by(message_type, subj_id, block_ix) %>%
  summarize(n_errors = sum(1 - is_correct))

gg_lsn_performance_ceiling <- ggplot(n_errors) +
  aes(block_ix, n_errors, color = message_type, linetype = message_type) +
  geom_line(stat = "summary", fun.y = "mean", size = sizes$line) +
  scale_x_continuous("Block (24 trials)") +
  scale_y_continuous("Number of errors") +
  scales$color$message_type +
  scales$linetype$message_type +
  base_theme +
  theme(legend.position = c(0.8, 0.8))

## RTs over blocks ##

lsn_quad_mod <- lmer(
  rt ~ message_c * (block_ix + block_ix_sqr) + (block_ix + block_ix_sqr|subj_id),
  data = first_last_gen
)

lsn_quad_preds <- expand.grid(message_c = c(-0.5, 0.5),
                              block_ix = 1:4) %>%
  mutate(block_ix_sqr = block_ix^2) %>%
  cbind(., predictSE(lsn_quad_mod, newdata = ., se = TRUE)) %>%
  rename(rt = fit, se = se.fit) %>%
  recode_message_type()

rt_plot <- ggplot(first_last_gen) +
  aes(block_ix, rt) +
  geom_smooth(aes(ymin = rt - se, ymax = rt + se, color = message_label,
                  linetype = message_label),
              fill = "gray", alpha = 0.4,
              stat = "identity", data = lsn_quad_preds,
              size = sizes$line) +
  scale_x_continuous("Block number\n(24 trials per block)", breaks = 1:4) +
  scales$y$rt +
  scales$color$message_type +
  scales$linetype$message_type +
  coord_cartesian(ylim = c(600, 1200)) +
  base_theme +
  theme(legend.position = c(0.65, 0.9),
        legend.key.width = unit(4, "lines"),
        legend.key.size = unit(0.5, 'lines'))

## Generalization ##

trials_per_block <- 24
n_trials <- 6

lsn_transition <- learning_sound_names %>%
  label_trial_in_block() %>%
  bin_trials("block_transition", "trial_in_block",
             before = (trials_per_block-n_trials):trials_per_block,
             after = 1:n_trials) %>%
  filter(
    !(block_ix == 1 & block_transition == "after"),
    !is.na(block_transition),
    message_type != "sound_effect"
  ) %>%
  recode_block_transition()

transition_mod <- lmer(
  rt ~ block_transition_c * message_c + block_ix + (block_transition_c + block_ix|subj_id),
  data = lsn_transition
)

transition_preds <- expand.grid(block_transition_c = c(-0.5, 0.5),
                                message_c = c(-0.5, 0.5),
                                block_ix = 3) %>%
  cbind(., predictSE(transition_mod, ., se = TRUE)) %>%
  rename(rt = fit, se = se.fit) %>%
  recode_block_transition() %>%
  recode_message_type()

dodger <- position_dodge(width = 0.1)

gg_transition <- ggplot(lsn_transition) +
  aes(block_transition_label, rt, color = message_type) +
  geom_linerange(aes(ymin = rt - se, ymax = rt + se),
                 data = transition_preds,
                 position = dodger, show.legend = FALSE,
                 size = sizes$line) +
  geom_line(aes(group = message_type, linetype = message_type),
            data = transition_preds,
            position = dodger, size = sizes$line) +
  scale_x_discrete("New category members\n(±6 trials)", labels = c("Before", "After")) +
  scales$y$rt +
  scales$color$message_type +
  scales$linetype$message_type +
  coord_cartesian(ylim = c(600, 1000)) +
  base_theme +
  theme(
    legend.position = "none",
    legend.key.width = unit(5, "lines")
  )

## Learning transcriptions of seed sounds

learning_seed_sounds <- learning_sound_names %>%
  mutate(block_ix_sqr = block_ix^2)

lsn_seed_quad_mod <- lmer(
  rt ~ message_type * (block_ix + block_ix_sqr) + (1|subj_id),
  data = learning_seed_sounds
)

lsn_seed_quad_preds <- expand.grid(
  message_type = c("sound_effect", "first_gen_imitation", "last_gen_imitation"),
  block_ix = 1:4,
  stringsAsFactors = FALSE
) %>%
  mutate(block_ix_sqr = block_ix^2) %>%
  cbind(., predictSE(lsn_seed_quad_mod, newdata = ., se = TRUE)) %>%
  rename(rt = fit, se = se.fit) %>%
  recode_message_type()

gg_seed_rt_plot <- ggplot(lsn_seed_quad_preds) +
  aes(block_ix, rt) +
  geom_smooth(aes(ymin = rt - se, ymax = rt + se, color = message_label,
                  linetype = message_label),
              fill = "gray", alpha = 0.4,
              stat = "identity") +
  scale_x_continuous("Block number\n(24 trials per block)",
                     breaks = 1:4) +
  scales$y$rt +
  scale_color_manual("", values = RColorBrewer::brewer.pal(3, "Set2")[c(2, 3, 1)]) +
  scale_linetype_manual("", values = c("dotdash", "solid", "longdash")) +
  coord_cartesian(ylim = c(600, 1200)) +
  base_theme +
  theme(legend.position = c(0.8, 0.75),
        legend.key.width = unit(5, "lines"))
