source("R/0-setup.R")

# ---- learning
first_last_gen <- filter(learning_sound_names, message_type != "sound_effect") %>%
  mutate(block_ix_sqr = block_ix^2)

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
  ggtitle("a") +
  theme(legend.position = c(0.55, 0.9),
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
  scale_x_discrete("Introduction of new category members (±6 trials)", labels = c("Before", "After")) +
  scale_y_rt +
  scale_color_message_label_2 +
  scale_linetype_message_label_2 +
  coord_cartesian(ylim = c(600, 1000)) +
  ggtitle("b") +
  base_theme +
  theme(
    legend.position = "none",
    legend.key.width = unit(5, "lines")
  )

# pdf("~/Desktop/fig2.pdf", width=5.8, height=2.5)
# grid.arrange(rt_plot, gg_transition, nrow = 1)
# dev.off()
