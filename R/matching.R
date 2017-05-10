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
                  color = survey_type),
              stat = "identity", data = transition_preds,
              size = 1.0) +
  scale_x_generation_1 +
  scale_y_gts_accuracy +
  scale_color_distractors +
  scale_linetype_distractors +
  chance_line +
  annotate("text", x = 0.5, y = 0.26, label = "chance",
           size = 4, vjust = -0.1, fontface = "italic",
           alpha = 0.6) +
  coord_cartesian(xlim = c(-0.2, 7.2), ylim = ylim_gts) +
  base_theme +
  ggtitle("Iterated imitations retain category information")
  theme(
    legend.position = c(0.8, 0.85),
    legend.key.width = unit(5, "lines"),
    panel.grid.minor.x = element_blank()
  )

# Find chains that made it 7 or more generations
long_chain_seeds <- imitation_matches %>%
  filter(generation >= 6) %>%
  .$seed_id %>%
  unique()

imitation_matches %<>%
  mutate(long_chain = seed_id %in% long_chain_seeds)

means_by_generation <- imitation_matches %>%
  group_by(survey_type, generation, long_chain) %>%
  summarize(is_correct = mean(is_correct),
            n = n()) %>%
  recode_generation()

# Show plot with means overlayed
ggplot(imitation_matches) +
  aes(x = generation_1, y = is_correct) +
  geom_smooth(aes(ymin = is_correct - se, ymax = is_correct + se,
                  color = survey_type),
              stat = "identity", data = transition_preds,
              size = 1.0) +
  geom_point(aes(size = n, color = survey_type), data = means_by_generation)

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
  message_label_2 = c("Transcription of first generation imitation",
                      "Transcription of last generation imitation")
)

preds <- cbind(x_preds, y_preds) %>%
  rename(is_correct = fit, se = se.fit) %>%
  recode_question_type() %>%
  recode_message_type() %>%
  left_join(message_labels)

gg_match_transcriptions <- ggplot(preds) +
  aes(question_c, is_correct) +
  geom_bar(aes(fill = question_type), stat = "identity", width = 0.95, alpha = 0.6) +
  geom_linerange(aes(group = question_type, ymin = is_correct - se, ymax = is_correct + se)) +
  scale_x_continuous("Question type", breaks = c(-0.5, 0.5), labels = c("True seed", "Category match")) +
  scale_y_gts_accuracy +
  scale_fill_manual("", values = unname(colors[c("blue", "green")])) +
  chance_line +
  geom_text(aes(label = label),
            data = data.frame(message_label_2 = "Transcription of first generation imitation",
                              question_c = -0.7, is_correct = 0.27, label = "chance"),
            fontface = "italic") +
  coord_cartesian(ylim = ylim_gts) +
  facet_wrap("message_label_2") +
  base_theme +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())