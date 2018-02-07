source("R/0-setup.R")

# ---- matching

## Imitations ##

data("imitation_matches")
n_all_matching_imitations <- count_subjects(imitation_matches)
imitation_matches %<>%
  filter(
    question_type != "catch_trial"
  ) %>%
  recode_generation() %>%
  recode_survey_type() %>%
  add_chance()
n_matching_imitations <- count_subjects(imitation_matches)

imitation_matches_overall_mod <- glmer(
  is_correct ~ offset(chance_log) + generation_1 + (generation_1|chain_name/seed_id),
  family = "binomial", data = imitation_matches
)

# Center generation at 8 to see if imitations are still recognizable after 8 repetitions.
imitation_matches$generation_8 <- imitation_matches$generation - 8
imitation_matches_gen8_mod <- glmer(
  is_correct ~ offset(chance_log) + generation_8 + (generation_8|chain_name/seed_id),
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
  theme(
    legend.position = c(0.5, 0.95),
    legend.key.width = unit(3.2, "lines"),
    legend.key.size = unit(0.6, "lines"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# Does removing the low accuracy within-category questions change
# the interpretation of the decreasing true seed advantage?
imitation_matches_mod_no_low_acc <- glmer(
  is_correct ~ offset(chance_log) + generation_1 * same_v_between +
    (generation_1|chain_name/seed_id) + (1|subj_id),
  family = "binomial", data = filter(imitation_matches, survey_type != "within")
)

## Transcriptions ##

data("transcription_matches")

transcription_matches %<>%
  recode_question_type() %>%
  recode_message_type() %>%
  recode_version() %>%
  add_chance() %>%
  filter(message_type != "sound_effect")

n_all_transcription_match_subjs <- count_subjects(transcription_matches)

data("transcription_matches")
transcription_match_failed_catch_trial <- transcription_matches %>%
  filter(question_type == "catch_trial", is_correct == 0) %>%
  .$subj_id %>%
  unique()
n_transcription_match_subjs_failed_catch_trial <- length(transcription_match_failed_catch_trial)

transcription_matches %<>%
  recode_question_type() %>%
  recode_message_type() %>%
  recode_version() %>%
  add_chance() %>%
  filter(message_type != "sound_effect")

transcription_matches %<>% filter(!(subj_id %in% transcription_match_failed_catch_trial))
n_transcription_match_subjs <- count_subjects(transcription_matches)

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
  geom_point(aes(shape = "transcriptions", color = question_type),
             position = dodger) +
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
  scale_shape_manual("", labels = c("Imitations",
                                    "Transcriptions"),
                     values = c(1, 4)) +
  guides(color = "none", linetype = "none") +
  chance_line +
  coord_cartesian(ylim = ylim_gts) +
  base_theme +
  theme(legend.position = c(0.5, 0.85),
        legend.key.width = unit(0.1, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

## Matching accuracy of transcriptions of seed sounds
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
  scale_y_gts_accuracy +
  scale_fill_manual("", values = unname(colors[c("blue", "green")])) +
  coord_cartesian(ylim = c(0.18, 0.71)) +
  chance_line +
  base_theme +
  theme(legend.position = "none",
        strip.placement = "outside",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size = 8))