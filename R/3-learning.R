source("R/0-setup.R")

# ---- learning
data("learning_sound_names")

n_all_lsn_subjs <- count_subjects(learning_sound_names)
n_lsn_words <- count_unique(learning_sound_names, "word")

learning_sound_names %<>%
  mutate(rt = ifelse(is_correct == 1, rt, NA),
         is_error = 1 - is_correct) %>%
  mutate(word_category_by_block_ix = paste(word_category, block_ix, sep = ":")) %>%
  recode_lsn_word_type() %>%
  mutate(
    block_ix_sqr = block_ix^2
  )

lsn_outliers <- c("LSN102", "LSN148", "LSN104", "LSN147")
n_lsn_outliers <- length(lsn_outliers)
learning_sound_names %<>% filter(!(subj_id %in% lsn_outliers))
n_lsn_subjs <- count_subjects(learning_sound_names)

trials_per_block <- 24
n_trials <- 6

lsn_transition <- learning_sound_names %>%
  label_trial_in_block() %>%
  bin_trials("block_transition", "trial_in_block",
             before = (trials_per_block-n_trials):trials_per_block,
             after = 1:n_trials) %>%
  filter(
    !(block_ix == 1 & block_transition == "after"),
    !is.na(block_transition),
    message_type != "sound_effect"
  ) %>%
  recode_block_transition()


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
  theme(legend.position = c(0.8, 0.8))

first_block <- first_last_gen %>% filter(block_ix == 1)

lsn_first_block_error_mod <- glmer(
  is_correct ~ message_c + (1|subj_id),
  data = first_block, family = "binomial"
)

lsn_first_block_rt_mod <- lmerTest::lmer(
  rt ~ message_c + (1|subj_id),
  data = first_block
)

after_first_block <- filter(first_last_gen, block_ix > 1)

lsn_after_first_block_error_mod <- glmer(
  is_correct ~ message_c + (1|subj_id),
  data = after_first_block, family = "binomial"
)

lsn_after_first_block_mod <- lmer(
  rt ~ message_c + block_ix + (1 + block_ix|subj_id),
  data = after_first_block)

lsn_after_first_block_lmertest_mod <- lmerTest::lmer(
  formula(lsn_after_first_block_mod), data = lsn_after_first_block_mod@frame
)

lsn_quad_mod <- lmer(
  rt ~ message_c * (block_ix + block_ix_sqr) + (block_ix + block_ix_sqr|subj_id),
  data = first_last_gen
)

lsn_quad_preds <- expand.grid(message_c = c(-0.5, 0.5),
                              block_ix = 1:4) %>%
  mutate(block_ix_sqr = block_ix^2) %>%
  cbind(., predictSE(lsn_quad_mod, newdata = ., se = TRUE)) %>%
  rename(rt = fit, se = se.fit) %>%
  recode_message_type()

rt_plot <- ggplot(first_last_gen) +
  aes(block_ix, rt) +
  geom_smooth(aes(ymin = rt - se, ymax = rt + se, color = message_label,
                  linetype = message_label),
              fill = "gray", alpha = 0.4,
              stat = "identity", data = lsn_quad_preds) +
  scale_x_block_ix +
  scale_y_rt +
  scale_color_message_label_2 +
  scale_linetype_message_label_2 +
  coord_cartesian(ylim = c(600, 1200)) +
  base_theme +
  theme(legend.position = c(0.65, 0.9),
        legend.key.width = unit(4, "lines"),
        legend.key.size = unit(0.5, 'lines'))

transition_mod <- lmer(
  rt ~ block_transition_c * message_c + block_ix + (block_transition_c + block_ix|subj_id),
  data = lsn_transition
)

transition_lmertest_mod <- lmerTest::lmer(
  formula(transition_mod), data = transition_mod@frame
)

transition_preds <- expand.grid(block_transition_c = c(-0.5, 0.5),
                                message_c = c(-0.5, 0.5),
                                block_ix = 3) %>%
  cbind(., predictSE(transition_mod, ., se = TRUE)) %>%
  rename(rt = fit, se = se.fit) %>%
  recode_block_transition() %>%
  recode_message_type()

dodger <- position_dodge(width = 0.1)

gg_transition <- ggplot(lsn_transition) +
  aes(block_transition_label, rt, color = message_type) +
  geom_linerange(aes(ymin = rt - se, ymax = rt + se),
                 data = transition_preds,
                 position = dodger, show.legend = FALSE,
                 size = 1) +
  geom_line(aes(group = message_type, linetype = message_type),
            data = transition_preds,
            position = dodger, size = 1) +
  scale_x_discrete("New category members\n(±6 trials)", labels = c("Before", "After")) +
  scale_y_rt +
  scale_color_message_label_2 +
  scale_linetype_message_label_2 +
  coord_cartesian(ylim = c(600, 1000)) +
  base_theme +
  theme(
    legend.position = "none",
    legend.key.width = unit(5, "lines")
  )

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
  scale_x_continuous("Block number\n(24 trials per block)",
                     breaks = 1:4) +
  scale_y_rt +
  scale_color_manual("", values = RColorBrewer::brewer.pal(3, "Set2")[c(2, 3, 1)]) +
  scale_linetype_manual("", values = c("dotdash", "solid", "longdash")) +
  coord_cartesian(ylim = c(600, 1200)) +
  base_theme +
  theme(legend.position = c(0.8, 0.75),
        legend.key.width = unit(5, "lines"))
