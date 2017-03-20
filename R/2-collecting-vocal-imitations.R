source("R/0-setup.R")

# ---- 2-measuring-acoustic-similarity
data("acoustic_similarity_judgments")

acoustic_similarity_judgments %<>%
  mutate(similarity = ifelse(similarity == -1, NA, similarity)) %>%
  z_score_by_subj() %>%
  recode_edge_generations() %>%
  determine_trial_id()

data("algo_linear")
data("algo_within_chain")
data("algo_within_seed")
data("algo_within_category")
data("algo_between_fixed")
data("algo_between_consecutive")

algo_linear %<>%
  recode_edge_generations()

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
  global_theme

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
  geom_smooth(method = "lm", se = FALSE)
