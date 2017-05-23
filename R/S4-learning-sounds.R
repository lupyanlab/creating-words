source("R/0-setup.R")

# ---- s4-learning-sounds
gg_lsn_performance_ceiling <- first_last_gen %>%
  group_by(message_type, subj_id, block_ix) %>%
  summarize(n_errors = sum(1 - is_correct)) %>%
  ggplot() +
  aes(block_ix, n_errors, color = message_type) +
  geom_point(stat = "summary", fun.y = "mean") +
  base_theme
