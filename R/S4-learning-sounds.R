source("R/0-setup.R")

# ---- s4-learning-sounds
first_last_gen <- filter(learning_sound_names, message_type != "sound_effect") %>%
  mutate(block_ix_sqr = block_ix^2)

n_errors <- first_last_gen %>%
  group_by(message_type, subj_id, block_ix) %>%
  summarize(n_errors = sum(1 - is_correct))

gg_lsn_performance_ceiling <- ggplot(n_errors) +
  aes(block_ix, n_errors, color = message_type, linetype = message_type) +
  geom_line(stat = "summary", fun.y = "mean") +
  scale_x_continuous("Block (24 trials)") +
  scale_y_continuous("Number of errors") +
  scale_color_message_label_2 +
  scale_linetype_message_label_2 +
  base_theme +
  theme(legend.position = c(0.8, 0.8)) +
  ggtitle("Accuracy performance ceiling in category learning task")
