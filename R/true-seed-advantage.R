source("R/2-matching.R")

imitation_matches_true_seed_adv <- filter(imitation_matches, survey_type != "within") %>%
  mutate(question_c = ifelse(survey_type == "between", -0.5, 0.5))

imitation_matches_true_seed_adv_mod <- glmer(
  is_correct ~ offset(chance_log) + generation_1 * question_c +
    (generation_1|chain_name/seed_id) + (1|subj_id),
  family = "binomial", data = imitation_matches_true_seed_adv
)

imitation_matches_true_seed_adv_preds <- expand.grid(
    generation_1 = unique(imitation_matches_true_seed_adv$generation_1) %>% na.omit(),
    question_c = c(-0.5, 0.5),
    stringsAsFactors = FALSE
  ) %>%
  mutate(
    generation = generation_1 + 1,
    generation_label = paste("Generation", generation)
  ) %>%
  add_chance() %>%
  cbind(., predictSE(imitation_matches_true_seed_adv_mod, ., se = TRUE)) %>%
  rename(is_correct = fit, se = se.fit)

scale_linetype_distractors <- scale_linetype_manual(
  "",
  values = c("longdash", "dotdash", "solid"),
  labels = distractor_labels
)

gg_match_to_seed_true_seed_adv <- ggplot(imitation_matches_true_seed_adv) +
  aes(x = generation_1, y = is_correct) +
  geom_smooth(aes(ymin = is_correct - se, ymax = is_correct + se,
                  color = factor(question_c), linetype = factor(question_c)),
              stat = "identity", data = imitation_matches_true_seed_adv_preds) +
  scale_x_generation_1 +
  scale_y_gts_accuracy +
  scale_color_distractors +
  scale_linetype_distractors +
  chance_line +
  annotate("text", x = 0.5, y = 0.26, label = "chance",
           size = 2, vjust = -0.1, fontface = "italic") +
  coord_cartesian(xlim = c(-0.2, 7.2), ylim = ylim_gts) +
  base_theme +
  theme(
    legend.position = c(0.5, 0.95),
    legend.key.width = unit(3.2, "lines"),
    legend.key.size = unit(0.6, "lines"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

grid.arrange(
  gg_match_to_seed_true_seed_adv +
    ggtitle("A"),
  gg_match_transcriptions +
    ggtitle("B") +
    theme(axis.title.y = element_blank()),
  nrow = 1,
  widths = c(0.6, 0.4)
)
