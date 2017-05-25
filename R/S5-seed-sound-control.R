source("R/0-setup.R")

# ---- seed-sound-control
data("transcription_distances")

message_type_labels <- select(imitations, message_id, seed_id, generation)

seed_distances <- transcription_distances %>%
  left_join(message_type_labels) %>%
  recode_message_type() %>%
  filter(!is.na(message_type))

unloadNamespace("lmerTest")
library(lme4)

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
        panel.grid.major.x = element_blank()) +
  ggtitle("A. Distance between transcriptions")

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
  scale_x_block_ix +
  scale_y_rt +
  scale_color_brewer("", palette = "Set2") +
  scale_linetype_manual("", values = c("longdash", "dotdash", "solid")) +
  coord_cartesian(ylim = c(600, 1200)) +
  base_theme +
  ggtitle("B. Learning categories of sounds") +
  theme(legend.position = c(0.7, 0.75),
        legend.key.width = unit(5, "lines"))

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
        axis.text.x = element_text(size = 8)) +
  ggtitle("C. Transcription matching accuracy")
