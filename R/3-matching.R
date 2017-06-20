source("R/0-setup.R")

# ---- matching

## Imitations ##

q_true_seed <- read_graphviz("true-seed", "wordsintransition")
q_category_match <- read_graphviz("category-match", "wordsintransition")
q_specific_match <- read_graphviz("specific-match", "wordsintransition")

imitation_matches_overall_mod <- glmer(
  is_correct ~ offset(chance_log) + generation_1 + (generation_1|chain_name/seed_id),
  family = "binomial", data = imitation_matches
)

imitation_matches_mod <- glmer(
  is_correct ~ offset(chance_log) + generation_1 * (same_v_between + same_v_within) +
    (generation_1|chain_name/seed_id) + (1|subj_id),
  family = "binomial", data = imitation_matches
)

x_preds <- expand.grid(
  generation_1 = unique(imitation_matches$generation_1) %>% na.omit(),
  survey_type = c("between", "same", "within"),
  stringsAsFactors = FALSE
) %>%
  recode_survey_type() %>%
  mutate(
    generation = generation_1 + 1,
    generation_label = paste("Generation", generation)
  ) %>%
  add_chance()

transition_preds <- predictSE(imitation_matches_mod, x_preds, se = TRUE) %>%
  cbind(x_preds, .) %>%
  rename(is_correct = fit, se = se.fit)

scale_linetype_distractors <- scale_linetype_manual(
  "",
  values = c("longdash", "dotdash", "solid"),
  labels = distractor_labels
)

gg_match_to_seed <- ggplot(imitation_matches) +
  aes(x = generation_1, y = is_correct) +
  geom_smooth(aes(ymin = is_correct - se, ymax = is_correct + se,
                  color = survey_type, linetype = survey_type),
              stat = "identity", data = transition_preds) +
  scale_x_generation_1 +
  scale_y_gts_accuracy +
  scale_color_distractors +
  scale_linetype_distractors +
  chance_line +
  annotate("text", x = 0.5, y = 0.26, label = "chance",
           size = 2, vjust = -0.1, fontface = "italic") +
  coord_cartesian(xlim = c(-0.2, 7.2), ylim = ylim_gts) +
  base_theme +
  ggtitle("b") +
  theme(
    legend.position = c(0.5, 0.95),
    legend.key.width = unit(3.2, "lines"),
    legend.key.size = unit(0.6, "lines"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

## Transcriptions ##

transcription_matches_last_gen_mod <- glmer(
  is_correct ~ offset(chance_log) + (1|word_category/seed_id),
  family = "binomial", data = transcription_matches
)

transcription_matches_last_gen_mod_category <- glmer(
  is_correct ~ offset(chance_log) + (1|word_category/seed_id),
  family = "binomial", data = filter(transcription_matches, question_type == "category")
)

transcription_matches_last_gen_mod_exact <- glmer(
  is_correct ~ offset(chance_log) + (1|word_category/seed_id),
  family = "binomial", data = filter(transcription_matches, question_type == "exact")
)

acc_mod <- glmer(
  is_correct ~ offset(chance_log) + question_c * message_c + (question_c * message_c|subj_id),
  family = binomial, data = transcription_matches
)

x_preds <- expand.grid(question_c = c(-0.5, 0.5), message_c = c(-0.5, 0.5)) %>%
  add_chance()
y_preds <- predictSE(acc_mod, x_preds, se = TRUE)

message_labels <- data_frame(
  message_type = c("first_gen_imitation", "last_gen_imitation"),
  message_label_2 = c("First generation transcription",
                      "Last generation transcription")
)

preds <- cbind(x_preds, y_preds) %>%
  rename(is_correct = fit, se = se.fit) %>%
  recode_question_type() %>%
  recode_message_type() %>%
  left_join(message_labels)

# Overlay imitation match accuracy means
imitation_accuracies <- imitation_matches %>%
  filter(
    survey_type != "within",
    message_id %in% unique(transcription_matches$message_id)
  ) %>%
  mutate(
    question_type = ifelse(question_type == "true_seed", "exact", "category"),
    message_type = ifelse(generation == 1, "first_gen_imitation", "last_gen_imitation")
  ) %>%
  group_by(question_type, message_type) %>%
  summarize(
    error = sd(is_correct)/sqrt(n()),
    is_correct = mean(is_correct)
  ) %>%
  ungroup() %>%
  recode_question_type() %>%
  recode_message_type() %>%
  left_join(message_labels)

dodger = position_dodge(width = -0.1)
gg_match_transcriptions <- ggplot(preds) +
  aes(message_c, is_correct) +
  geom_line(aes(color = question_type, linetype = question_type), position = dodger) +
  geom_errorbar(aes(color = question_type, ymin = is_correct - se, ymax = is_correct + se),
                width = 0.2, position = dodger) +
  geom_point(aes(shape = "imitations", color = question_type),
             data = imitation_accuracies,
             size = 1, position = dodger) +
  geom_linerange(
    aes(ymin = is_correct - error, ymax = is_correct + error, color = question_type),
    data = imitation_accuracies,
    size = 0.3, position = dodger
  ) +
  scale_x_continuous("Generation", breaks = c(-0.5, 0.5), labels = c("First", "Last")) +
  scale_y_gts_accuracy +
  scale_color_manual("", values = unname(colors[c("blue", "green")])) +
  scale_linetype_manual("", values = c("dotdash", "longdash")) +
  scale_shape_manual("", labels = "= Imitations", values = 1) +
  guides(color = "none", linetype = "none") +
  chance_line +
  coord_cartesian(ylim = ylim_gts) +
  ggtitle("c") +
  base_theme +
  theme(legend.position = c(0.7, 0.85),
        legend.key.width = unit(0.1, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())


q_true_seed <- read_graphviz("true-seed", "wordsintransition")
q_category_match <- read_graphviz("category-match", "wordsintransition")
q_specific_match <- read_graphviz("specific-match", "wordsintransition")

# pdf("~/Desktop/fig3.pdf", width=6, height=2.5)
# grid.arrange(
#   arrangeGrob(q_true_seed, q_category_match, q_specific_match, ncol = 1,
#               top = textGrob('a', hjust = 15, gp = gpar(fontsize = 8, fontface = "bold"))),
#   gg_match_to_seed,
#   gg_match_transcriptions + theme(axis.title.y = element_blank()),
#   nrow = 1,
#   widths = c(0.4, 0.35, 0.25)
# )
# dev.off()
