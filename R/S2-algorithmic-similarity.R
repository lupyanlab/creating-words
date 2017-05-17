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
  ) +
  ggtitle("A. Types of algorithmic similarity comparisons")

algo_similarity <- bind_rows(
  within = algo_linear,
  between = algo_between_consecutive,
  .id = "edge_type"
)

set.seed(603)
gg_algo_similarity <- ggplot(algo_similarity %>% filter(edge_type == "within")) +
  aes(edge_generations, similarity) +
  geom_point(aes(group = sound_x_category, shape = sound_x_category, color = sound_x_category),
             position = position_jitter(0.2, 0.0),
             stat = "summary", fun.y = "mean", size = 2.0) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "gray") +
  scale_x_discrete("Generation") +
  scale_y_continuous("Algorithmic similarity") +
  scale_color_brewer("", palette = "Set2") +
  scale_shape_discrete("") +
  base_theme +
  theme(legend.position = "top") +
  ggtitle("B. Algorithmic similarity over generations")

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
  base_theme +
  ggtitle("C. Correlation between similarity measures")
gg_comparing_similarities
