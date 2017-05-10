source("R/setup.R")

# ---- stability

## Imitations ##

similarity_judgments_mod <- lmer(
  similarity_z ~ edge_generation_n + (edge_generation_n|name) + (edge_generation_n|category),
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
  recode_edge_generations

set.seed(949)
gg_similarity_judgments <- ggplot(similarity_judgments_means) +
  aes(x = edge_generations, y = similarity_z) +
  geom_point(aes(color = category, shape = category),
             position = position_jitter(0.1, 0.0),
             size = 2.5) +
  geom_smooth(aes(group = 1, ymin = similarity_z - se, ymax = similarity_z + se),
              data = similarity_judgments_preds, stat = "identity",
              alpha = 0.2, color = "gray") +
  scale_x_discrete("Generation") +
  scale_y_continuous("Acoustic similarity judgments (z-score)") +
  scale_color_brewer("", palette = "Set2") +
  scale_shape_discrete("") +
  coord_cartesian(ylim = c(-0.6, 0.8)) +
  ggtitle("Repeating imitations makes them more repeatable")
  base_theme +
  theme(legend.position = c(0.1, 0.85))

# Inter-rater reliability
irr_ratings <- acoustic_similarity_judgments %>%
  group_by(name, trial_id) %>%
  summarize(similarity_z = mean(similarity_z)) %>%
  ungroup() %>%
  spread(name, similarity_z) %>%
  drop_na() %>%
  select(-trial_id)
irr_results <- icc(irr_ratings, model = "twoway", type = "consistency")

report_icc_results <- function(irr_results) {
  if (irr_results$p.value < 0.001) {
    p_value_str = "_p_ < 0.001"
  } else {
    p_value_str = sprintf("_p_ = %.3f", irr_results$p.value)
  }
  sprintf("ICC = %.2f, 95%% CI [%.2f, %.2f], F(%d, %d) = %.2f, %s",
          irr_results$value,
          irr_results$lbound, irr_results$ubound,
          irr_results$df1, irr_results$df2, irr_results$Fvalue,
          p_value_str)
}

# Correlation between subjective and objective measures

similarity_cor <- left_join(
  select(acoustic_similarity_judgments, sound_x, sound_y, similarity_z),
  select(algo_linear, sound_x, sound_y, similarity)
)

similarity_cor_test <- cor.test(
  similarity_cor$similarity_z, similarity_cor$similarity,
  use = "pairwise.complete.obs")

report_cor_test <- function(cor_test) {
  results <- broom::tidy(cor_test) %>% as.list()
  sprintf("r = %.2f, 95%% CI [%.2f, %.2f]",
          results$estimate, results$conf.low, results$conf.high)
}

# Automated analyses of acoustic similarity
similarity_algo_mod <- lmer(
  similarity_z ~ edge_generation_n + (edge_generation_n|sound_x_category),
  data = algo_linear
)

similarity_algo_lmertest_mod <- lmerTest::lmer(
  formula(similarity_algo_mod), data = similarity_algo_mod@frame
)

data("algo_within_chain")
data("algo_within_seed")
data("algo_within_category")
data("algo_between_fixed")
data("algo_between_consecutive")

algo_between_consecutive %<>%
  recode_edge_generations()

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
    edge_type_label = factor(edge_type, levels = c("linear", "within_chain", "within_seed", "within_category", "between_fixed", "between_consecutive"))
  )

gg_algo_compare <- ggplot(acoustic_similarity_comparison) +
  aes(edge_type_label, similarity) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.1) +
  geom_bar(stat = "summary", fun.y = "mean", alpha = 0.4)

algo_similarity <- bind_rows(
  within = algo_linear,
  between = algo_between_consecutive,
  .id = "edge_type"
)

set.seed(603)
gg_algo_similarity <- ggplot(algo_similarity) +
  aes(edge_generations, similarity, color = edge_type, group = edge_type) +
  geom_point(position = position_jitter(0.4, 0.0), alpha = 0.1) +
  geom_line(stat = "summary", fun.y = "mean") +
  geom_smooth(method = "lm", se = FALSE) +
  base_theme

## Transcriptions ##

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

orthographic_distance_mod <- lmer(distance ~ message_c + (message_c|chain_name/seed_id),
                                  data = transcription_distances)

orthographic_distance_lmertest_mod <- lmerTest::lmer(
  formula(orthographic_distance_mod), data = orthographic_distance_mod@frame
)

orthographic_distance_preds <- data_frame(message_c = c(-0.5, 0.5)) %>%
  cbind(., predictSE(orthographic_distance_mod, newdata = ., se = TRUE)) %>%
  rename(distance = fit, se = se.fit) %>%
  recode_message_type()

gg_distance <- ggplot(transcription_distances) +
  aes(message_label, distance, color = message_label) +
  geom_point(aes(group = message_id),
             stat = "summary", fun.y = "mean",
             position = position_jitter(0.2, 0.01),
             alpha = 0.8, size = 2) +
  geom_errorbar(aes(ymin = distance - se, ymax = distance + se),
                data = orthographic_distance_preds,
                size = 1.4, width = 0.1) +
  scale_x_discrete("", labels = c("First generation imitations", "Last generation imitations")) +
  scale_y_continuous("Distance between transcriptions", breaks = seq(0, 1, by = 0.2)) +
  scale_color_manual(values = imitation_gen_colors) +
  scale_fill_manual(values = imitation_gen_colors) +
  coord_cartesian(ylim = c(0.0, 0.8)) +
  ggtitle("Iterated imitations were easier to spell")
  base_theme +
  theme(legend.position = "none")

transcription_examples <- transcription_matches %>%
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
