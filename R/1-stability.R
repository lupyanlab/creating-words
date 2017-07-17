source("R/0-setup.R")

# ---- stability

## Selecting seed sounds ##

data("sound_similarity_4")
data("sound_similarity_6")

n_norming_subjects <- count_unique(sound_similarity_4, "workerId") +
  count_unique(sound_similarity_6, "workerId")

drop_bad_trials <- . %>%
  filter(failed_catch_trial == 0, problem_with_audio == 0)

count_responses <- function(frame, filename_levels) {
  frame %>%
    drop_bad_trials() %>%
    recode_filename(custom_levels = filename_levels) %>%
    count(filename_f) %>%
    select(filename_f, n) %>%
    complete(filename_f, fill = list(n = 0)) %>%
    mutate(filename = as.character(filename_f)) %>%
    left_join(filename_map) %>%
    arrange(category, desc(n))
}

# Create a factor for ordered and missing filenames.
filename_map <- expand.grid(
    category = unique(sound_similarity_6$category),
    ix = 1:6
  ) %>%
  mutate(filename = paste0(category, "_0", ix, ".mp3")) %>%
  arrange(category, ix) %>%
  select(category, filename)

filename_levels <- filename_map$filename

sound_similarity_6_counts <- sound_similarity_6 %>%
  count_responses(filename_levels) %>%
  # label two most frequently selected messages as odd
  group_by(category) %>%
  mutate(odd_one_out = ifelse(n >= n[2], "odd", "normal")) %>%
  ungroup()

kept <- sound_similarity_6_counts %>%
  filter(odd_one_out == "normal") %>%
  .$filename %>%
  unique()

filename_levels <- filename_levels[filename_levels %in% kept]
final_categories <- c("water", "tear", "zipper", "glass")
sound_similarity_4_counts <- sound_similarity_4 %>%
  count_responses(filename_levels) %>%
  # label categories used in final experiment
  mutate(odd_one_out = ifelse(category %in% final_categories, "normal", "odd"))

odd_one_out <- ggplot(mapping = aes(x = filename_f, y = n)) +
  geom_bar(aes(fill = odd_one_out, alpha = odd_one_out), stat = "identity") +
  scale_x_discrete("") +
  scale_y_continuous("Number of times selected as odd one out") +
  scale_fill_manual("", labels = c("kept", "dropped"),
                    values = RColorBrewer::brewer.pal(3, "Set2")[c(1, 2)]) +
  scale_alpha_manual("", values = c(1.0, 0.4), labels = c("kept", "dropped")) +
  facet_wrap("category", scales = "free_x", ncol = 2) +
  base_theme +
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.direction = "vertical",
    legend.position = c(0.06, 1.0)
  )

gg_seed_selection_round_1 <- (odd_one_out %+% sound_similarity_6_counts) +
  theme(
    legend.position = c(0.15, 0.98),
    legend.key.size = unit(0.5, "lines"),
    axis.title.y = element_text(margin = margin(0, 2, 0, 0, unit = "lines"))
  )

gg_seed_selection_round_2 <- (odd_one_out %+% sound_similarity_4_counts) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  )


## Dendrogram ##

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
  geom_edge_diagonal(aes(edge_linetype = node2.node_type), edge_width = 0.2) +
  geom_node_point(aes(shape = node_type), size = 0.3) +
  geom_node_text(aes(label = node_label), vjust = -0.5, size = 2.5) +
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

n_all_imitations <- count_imitations(imitations)
n_imitators <- count_subjects(imitations)
n_removed <- imitations %>%
  filter(rejected == "True") %>%
  count_imitations()
n_removed_pct <- round(n_removed/count_imitations(imitations) * 100)
n_final_imitations <- imitations %>%
  filter(rejected == "False") %>%
  count_imitations()
n_branches <- imitations$first_gen_id %>% unique() %>% na.omit() %>% length()

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

similarity_judgments_lmertest_mod <- lmerTest::lmer(
  formula(similarity_judgments_mod), data = similarity_judgments_mod@frame
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
             size = 1) +
  geom_smooth(aes(group = 1, ymin = similarity_z - se, ymax = similarity_z + se),
              data = similarity_judgments_preds, stat = "identity",
              alpha = 0.2, color = "gray") +
  scale_x_discrete("Generation") +
  scale_y_continuous("Perceived acoustic similarity (z-score)") +
  scale_color_brewer("", palette = "Set2") +
  scale_shape_discrete("") +
  coord_cartesian(ylim = c(-0.6, 0.8)) +
  base_theme +
  theme(
    legend.position = c(0.2, 0.91),
    legend.key.size = unit(0.7, "lines"),
    axis.title.y = element_text(margin = margin(0, -2, 0, 0))
  )

# Inter-rater reliability
acoustic_similarity_judgments %<>%
  mutate(similarity = ifelse(similarity == -1, NA, similarity)) %>%
  z_score_by_subj() %>%
  recode_edge_generations() %>%
  determine_trial_id()

irr_ratings <- acoustic_similarity_judgments %>%
  group_by(name, trial_id) %>%
  summarize(similarity_z = mean(similarity_z)) %>%
  ungroup() %>%
  spread(name, similarity_z) %>%
  drop_na() %>%
  select(-trial_id)

irr_results <- icc(irr_ratings, model = "twoway", type = "consistency", unit = "a")

## Algorithmic similarity ##

data("algo_linear")
data("algo_between_consecutive")
data("algo_within_chain")
data("algo_within_seed")
data("algo_within_category")
data("algo_between_fixed")

# Compare within and between similarity

z_score <- function(x) (x - mean(x))/sd(x)

algo_linear %<>%
  recode_edge_generations() %>%
  mutate(similarity_z = z_score(similarity))

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
              stat = "identity") +
  geom_point(stat = "summary", fun.y = "mean", data = algo_similarity) +
  scale_x_discrete("Generations") +
  scale_y_continuous("Algorithmic similarity") +
  scale_color_discrete("Type of pairwise comparison", labels = c("Within", "Between")) +
  base_theme +
  theme(legend.position = "top")

# Comparison of all 6 edge types

acoustic_similarity_comparison <- bind_rows(
  linear = algo_linear,
  within_chain = algo_within_chain,
  within_seed = algo_within_seed,
  within_category = algo_within_category,
  between_fixed = algo_between_fixed,
  between_consecutive = algo_between_consecutive,
  .id = "edge_type"
) %>%
  mutate(
    edge_type_label = factor(edge_type, levels = c("linear", "within_chain", "within_seed", "within_category", "between_fixed", "between_consecutive")),
    edge_type_helmert = edge_type_label
  )

contrasts(acoustic_similarity_comparison$edge_type_helmert) <- contr.helmert(n = 6)

acoustic_similarity_comparison_mod <- lm(
  similarity ~ edge_type_helmert, data = acoustic_similarity_comparison
)

gg_algo_compare <- ggplot(acoustic_similarity_comparison) +
  aes(edge_type_label, similarity) +
  geom_bar(stat = "summary", fun.y = "mean", alpha = 0.4) +
  stat_summary(fun.data = mean_se, geom = "linerange") +
  scale_x_discrete("Type of pairwise comparison",
                   labels = c("Within chain\n(consecutive)", "Within chain\n(all)", "Within seed", "Within cat.", "Between cat.\n(same)", "Between cat.\n(consecutive)")) +
  scale_y_continuous("Algorithmic similarity") +
  coord_cartesian(ylim = c(0.015, 0.018)) +
  base_theme +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 8)
  )

# Correlation between subjective and objective measures

similarity_cor <- left_join(
  select(acoustic_similarity_judgments, sound_x, sound_y, similarity_z),
  select(algo_linear, sound_x, sound_y, similarity)
)

similarity_cor_test <- cor.test(
  similarity_cor$similarity_z, similarity_cor$similarity,
  use = "pairwise.complete.obs")

similarity_algo_mod <- lmer(
  similarity_z ~ edge_generation_n + (edge_generation_n|sound_x_category),
  data = algo_linear
)

similarity_algo_lmertest_mod <- lmerTest::lmer(
  formula(similarity_algo_mod), data = similarity_algo_mod@frame
)



# Compare human and machine similarity
edge_similarities <- 
  merge(
    acoustic_similarity_judgments %>%
      group_by(sound_x, sound_y) %>%
      summarize(similarity_judgments = mean(similarity, na.rm = TRUE)),
    acoustic_similarity_linear %>%
      group_by(sound_x, sound_y) %>%
      summarize(similarity_algorithmic = mean(similarity, na.rm = TRUE))
  )

comparing_similarities_cor_test <- with(edge_similarities,
                                        cor.test(similarity_judgments, similarity_algorithmic)
) %>%
  tidy()

gg_comparing_similarities <- ggplot(edge_similarities) +
  aes(similarity_judgments, similarity_algorithmic) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous("Similarity judgments") +
  scale_y_continuous("Algorithmic similarity") +
  base_theme



## Transcriptions ##

data("transcriptions")
data("transcription_frequencies")
data("transcription_distances")

transcription_catch_trials <- transcriptions %>%
  filter(is_catch_trial == 1) %>%
  select(subj_id, chain_name, text)

n_all_transcribers <- count_subjects(transcriptions)
transcription_bad_subjs <- c("A3A8P4UR9A0DWQ", "AAMLJUUYM484")
transcriptions %<>% filter(!(subj_id %in% transcription_bad_subjs))
n_bad_transcribers <- length(transcription_bad_subjs)
n_transcribers <- count_subjects(transcriptions)
n_transcriptions <- nrow(transcriptions)
n_english_transcriptions <- transcription_frequencies %>%
  filter(is_english == 1) %>%
  select(text) %>%
  nrow()

n_imitations_transcribed <- count_imitations(transcriptions)
n_transcriptions_per_imitation <- transcriptions %>%
  count(message_id) %>%
  .$n %>%
  mean() %>%
  round(0)

gen_labels <- imitations %>%
  select(message_id, generation)

transcription_frequencies %<>%
  left_join(gen_labels) %>%
  recode_message_type()

n_created_words <- transcription_frequencies %>%
  filter(
    is_english == 0,
    # Exclude transcriptions of seed sounds
    seed_id != message_id
  ) %>%
  .$text %>%
  unique() %>%
  length()

message_type_labels <- select(imitations, message_id, seed_id, generation)

seed_distances <- transcription_distances %>%
  left_join(message_type_labels) %>%
  recode_message_type() %>%
  filter(!is.na(message_type))

message_id_map <- select(imitations, message_id, chain_name, seed_id, generation)

transcription_distances %<>%
  left_join(message_id_map) %>%
  recode_transcription_frequency() %>%
  recode_message_type() %>%
  filter(message_type != "sound_effect")

# Percentage of imitations will all unique transcriptions
messages_with_all_unique_transcriptions <- transcription_frequencies %>%
  group_by(message_id) %>%
  summarize(all_unique_transcriptions = (max(n) == 1))

messages_with_duplicated_transcriptions <- messages_with_all_unique_transcriptions %>%
  filter(all_unique_transcriptions == FALSE) %>%
  .$message_id

pct_of_messages_with_all_unique_transcriptions <- paste0(
  round(47/(64 + 47) * 100), "%"
)

transcription_distances %<>%
  mutate(category = chain_name)

orthographic_distance_mod <- lmer(distance ~ message_c + (message_c|category/seed_id),
                                  data = transcription_distances)

orthographic_distance_lmertest_mod <- lmerTest::lmer(
  formula(orthographic_distance_mod), data = orthographic_distance_mod@frame
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
             size = 1, alpha = 0.6, shape = 1) +
  geom_errorbar(aes(ymin = distance - se, ymax = distance + se),
                data = orthographic_distance_preds,
                size = 1, width = 0.3) +
  scale_x_discrete("Generation", labels = c("First", "Last")) +
  scale_y_continuous("Orthographic distance between transcriptions", breaks = seq(0, 1, by = 0.2)) +
  scale_color_manual(values = imitation_gen_colors) +
  scale_fill_manual(values = imitation_gen_colors) +
  coord_cartesian(ylim = c(0.0, 0.8)) +
  base_theme +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

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

transcription_uniqueness <- transcription_frequencies %>%
  recode_message_type() %>%
  filter(message_type != "sound_effect") %>%
  group_by(message_type, message_label, message_c, message_id) %>%
  summarize(
    num_words = sum(n),
    num_unique = n_distinct(text),
    perct_unique = num_unique/num_words,
    perct_agreement = 1 - perct_unique
  ) %>%
  ungroup %>%
  mutate(
    no_agreement = as.integer(perct_agreement == 0)
  ) %>%
  recode_transcription_frequency()

exact_string_matches_mod <- lm(perct_agreement ~ message_c,
                               data = transcription_uniqueness)

substr_length_mod <- lmerTest::lmer(length ~ message_c + (message_c|seed_id),
                                    data = transcription_distances)


set.seed(752)  # for replicable position_jitter
gg_exact_matches <- ggplot(transcription_uniqueness) +
  aes(x = message_label, y = perct_agreement) +
  geom_point(aes(color = frequency_type),
             position = position_jitter(0.1, 0.01), shape = 1) +
  geom_point(aes(color = frequency_type), stat = "summary", fun.y = "mean",
             size = 3) +
  scale_x_discrete("") +
  scale_y_continuous("Transcription agreement", labels = scales::percent) +
  scale_color_brewer("", palette = "Set2") +
  base_theme +
  theme(legend.position = "top")


message_id_map <- select(imitations, message_id, seed_id, generation)

distance_plot <- ggplot(transcription_distances) +
  aes(message_label, distance) +
  base_theme

gg_string_distance <- distance_plot + 
  geom_bar(aes(fill = frequency_type, width = 0.96), stat = "summary", fun.y = "mean",
           alpha = 0.6) +
  geom_point(aes(color = frequency_type, group = message_id), stat = "summary", fun.y = "mean",
             position = position_jitter(0.3, 0.01)) +
  scale_x_discrete("") +
  scale_y_continuous("Distance between transcriptions") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap("frequency_type") +
  guides(color = "none", fill = "none")

gg_length_plot <- ggplot(transcription_distances) +
  aes(message_label, length) +
  geom_point(aes(group = message_id, color = frequency_type), stat = "summary", fun.y = "mean",
             position = position_jitter(0.3, 0.1)) +
  geom_line(aes(group = frequency_type, color = frequency_type), stat = "summary", fun.y = "mean") +
  scale_color_brewer("", palette = "Set2") +
  labs(x = "", y = "Longest substring match (characters)",
       title = "c") + 
  base_theme +
  theme(legend.position = "top")


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
                data = seed_distance_preds, width = 0.2) +
  xlab("") +
  ylab("Distance between transcriptions") +
  scale_fill_brewer(palette = "Set2") +
  base_theme +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())
