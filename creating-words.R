# ---- setup
library(tidyverse)
library(grid)
library(magrittr)
library(lme4)
library(AICcmodavg)
library(gridExtra)
library(crotchet)

library(wordsintransition)

add_chance <- function(frame) {
  frame %>%
    mutate(
      chance = 0.25,
      chance_log = log(chance)
    )
}

count_imitations <- . %>% .$message_id %>% unique() %>% length()
count_subjects <- . %>% .$subj_id %>% na.omit() %>% unique() %>% length()


data("imitations")

n_all_imitations <- count_imitations(imitations)
n_imitators <- count_subjects(imitations)
n_removed <- imitations %>%
  filter(rejected == "True") %>%
  count_imitations()
n_final_imitations <- imitations %>%
  filter(rejected == "False") %>%
  count_imitations()
n_branches <- imitations$first_gen_id %>% unique() %>% na.omit() %>% length()


data("acoustic_similarity_judgments")
acoustic_similarity_judgments %<>%
  mutate(similarity = ifelse(similarity == -1, NA, similarity)) %>%
  z_score_by_subj() %>%
  recode_edge_generations() %>%
  determine_trial_id()

data("imitation_matches")
imitation_matches %<>%
  filter(question_type != "catch_trial") %>%
  recode_generation() %>%
  recode_survey_type() %>%
  add_chance()

n_matching_imitations <- count_subjects(imitation_matches)

data("transcriptions")

n_transcribers <- count_subjects(transcriptions)
n_imitations_transcribed <- count_imitations(transcriptions)
n_transcriptions_per_imitation <- transcriptions %>%
  count(message_id) %>%
  .$n %>%
  mean() %>%
  round(0)
n_transcriptions <- nrow(transcriptions)

data("transcription_distances")
data("transcription_matches")

n_transcription_match_subjs <- count_subjects(transcription_matches)

message_id_map <- select(imitations, message_id, chain_name, seed_id, generation)

transcription_distances %<>%
  left_join(message_id_map) %>%
  recode_transcription_frequency() %>%
  recode_message_type() %>%
  filter(message_type != "sound_effect")

transcription_matches %<>%
  recode_question_type() %>%
  recode_message_type() %>%
  recode_version() %>%
  add_chance()

bad_subj_ids <- transcription_matches %>%
  filter(question_type == "catch_trial", is_correct == 0) %>%
  .$subj_id %>% unique

transcription_matches %<>%
  filter(question_type != "catch_trial",
         !(subj_id %in% bad_subj_ids),
         message_type != "sound_effect")

recode_word_type <- . %>%
 rename(message_type = word_type) %>%
 recode_message_type()

data("learning_sound_names")
n_lsn_subjs <- count_subjects(learning_sound_names)

learning_sound_names %<>%
 mutate(rt = ifelse(is_correct == 1, rt, NA),
        is_error = 1 - is_correct) %>%
 mutate(word_category_by_block_ix = paste(word_category, block_ix, sep = ":")) %>%
 recode_word_type() %>%
 mutate(
   block_ix_sqr = block_ix^2
 )

lsn_transition <- learning_sound_names %>%
 label_trial_in_block()

trials_per_block <- max(lsn_transition$trial_in_block)
n_trials <- 6

recode_block_transition <- function(frame) {
 block_transition_levels <- c("before", "after")
 block_transition_map <- data_frame(
   block_transition = block_transition_levels,
   block_transition_label = factor(block_transition_levels,
                                   levels = block_transition_levels),
   block_transition_c = c(-0.5, 0.5)
 )
 left_join(frame, block_transition_map)
}

lsn_transition %<>%
 bin_trials("block_transition", "trial_in_block",
            before = (trials_per_block-n_trials):trials_per_block,
            after = 1:n_trials) %>%
 filter(
   !(block_ix == 1 & block_transition == "after"),
   !is.na(block_transition),
   message_type != "sound_effect"
 ) %>%
 recode_block_transition()

outliers <- c("LSN102", "LSN148", "LSN104", "LSN147")
learning_sound_names %<>% filter(!(subj_id %in% outliers))

# ggplot theme, colors, and scales ---------------------------------------------
base_theme <- theme_minimal(base_size=14)

colors <- RColorBrewer::brewer.pal(4, "Set2")
names(colors) <- c("blue", "orange", "green", "pink")
question_type_colors <- unname(colors[c("green", "blue", "orange")])
distractor_labels <- c("True seed", "Category match", "Specific match")
distractor_colors <- question_type_colors
imitation_gen_colors <- unname(colors[c("green", "blue")])

scale_x_generation_1 <- scale_x_continuous(
  "Generation",
  breaks = 0:11,
  labels = 1:12
)
scale_color_distractors <- scale_color_manual(
  "",
  values = distractor_colors,
  labels = distractor_labels
)
scale_x_trial_ix <- scale_x_continuous("Trial number (24 trials per block)",
                                       breaks = seq(1, 96, by = 24))
scale_x_block_ix <- scale_x_continuous("Block number (24 trials per block)",
                                       breaks = 1:4)
scale_y_rt <- scale_y_continuous("Reaction time (msec)")
scale_color_message_label <- scale_color_manual(
  "Transcription of",
  labels = c("Sound effect", "First generation", "Last generation"),
  values = unname(colors[c("orange", "green", "blue")])
)
scale_color_message_label_2 <- scale_color_manual(
  "Transcription of",
  labels = c("First generation", "Last generation"),
  values = unname(colors[c("green", "blue")])
)

chance_line <- geom_hline(yintercept = 0.25, lty = 2, alpha = 0.4, size = 1)
chance_label <- annotate("text", x = 1, y = 0.26, label = "chance",
                         size = 7, vjust = -0.1, fontface = "italic")
ylim_gts <- c(0.15, 0.75)
scale_y_gts_accuracy <- scale_y_continuous("Accuracy", breaks = seq(0, 1, by = 0.1),
                                           labels = scales::percent)

# reporting statistical results ------------------------------------------------
lmer_mod_results <- function(lmertest_mod, param) {
  results <- broom::tidy(lmertest_mod, effects = "fixed") %>%
    filter(term == param) %>%
    as.list()

  lmertest_summary <- lmerTest::summary(lmertest_mod) %>%
    .$coefficients %>%
    as.data.frame()
  lmertest_summary$term <- rownames(lmertest_summary)
  lmertest_results <- lmertest_summary %>%
    filter(term == param) %>%
    as.list()

  results$df <- lmertest_results[["df"]]
  results$p_value <- lmertest_results[["Pr(>|t|)"]]

  if (results$p_value < 0.001) {
    results$p_value_str <- "_p_ < 0.001"
  } else {
    results$p_value_str <- paste("_p_ = ", round(results$p_value, 3))
  }

  sprintf("_b_ = %.2f (%.2f), _t_(%.1f) = %.2f, %s",
          results$estimate, results$std.error, results$df, results$statistic, results$p_value_str)
}


glmer_mod_results <- function(glmer_mod, param, odds = FALSE) {
  results <- broom::tidy(glmer_mod, effects = "fixed") %>%
    filter(term == param) %>%
    as.list()

  if (results$p.value < 0.001) {
    results$p_value_str <- "_p_ < 0.001"
  } else {
    results$p_value_str <- paste("_p_ = ", round(results$p.value, 3))
  }

  if (odds == TRUE) {
    results["odds"] <- log(results$estimate)
    formatted = sprintf("_b_ = %.2f (%.2f) log-odds, odds = %.2f, _z_ = %.2f, %s",
                        results$estimate, results$std.error, results$odds, results$statistic, results$p_value_str)
  } else {
    formatted = sprintf("_b_ = %.2f (%.2f) log-odds, _z_ = %.2f, %s",
                        results$estimate, results$std.error, results$statistic, results$p_value_str)
  }
  formatted
}


# ---- collecting-imitations ---------------------------------------------------
similarity_judgments_mod <- lmer(
  similarity_z ~ edge_generation_n + (edge_generation_n|name) + (edge_generation_n|category),
  data = acoustic_similarity_judgments
)

similarity_judgments_lmertest_mod <- lmerTest::lmer(
  formula(similarity_judgments_mod), data = similarity_judgments_mod@frame
)

similarity_judgments_preds <- data_frame(edge_generation_n = 1:7) %>%
  cbind(., predictSE(similarity_judgments_mod, newdata = ., se = TRUE)) %>%
  rename(similarity_z = fit, se = se.fit) %>%
  recode_edge_generations()

similarity_judgments_means <- acoustic_similarity_judgments %>%
  group_by(edge_generations, category) %>%
  summarize(similarity_z = mean(similarity_z, na.rm = TRUE)) %>%
  recode_edge_generations

set.seed(949)
gg_similarity_judgments <- ggplot(similarity_judgments_means) +
  aes(x = edge_generations, y = similarity_z) +
  geom_point(aes(color = category), position = position_jitter(0.2, 0.0),
             size = 2.5, alpha = 0.8) +
  geom_smooth(aes(group = 1, ymin = similarity_z - se, ymax = similarity_z + se),
              data = similarity_judgments_preds, stat = "identity",
              alpha = 0.2, color = "gray") +
  scale_x_discrete("Generation") +
  scale_y_continuous("Acoustic similarity (z-score)") +
  scale_color_brewer("Category", palette = "Set2") +
  coord_cartesian(ylim = c(-0.6, 0.8)) +
  base_theme +
  theme(legend.position = "top")

# ---- matching-imitations -----------------------------------------------------
q_true_seed <- read_graphviz("true-seed", "wordsintransition")
q_category_match <- read_graphviz("category-match", "wordsintransition")
q_specific_match <- read_graphviz("specific-match", "wordsintransition")

imitation_matches_overall_mod <- glmer(
  is_correct ~ offset(chance_log) + generation_1 + (generation_1|chain_name/seed_id),
  family = "binomial", data = imitation_matches
)

imitation_matches_mod <- glmer(
  is_correct ~ offset(chance_log) + generation_1 * (same_v_between + same_v_within) + (generation_1|chain_name/seed_id),
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

gg_match_to_seed <- ggplot(imitation_matches) +
  aes(x = generation_1, y = is_correct) +
  geom_smooth(aes(ymin = is_correct - se, ymax = is_correct + se, color = survey_type),
              stat = "identity", data = transition_preds,
              size = 1.0) +
  scale_x_generation_1 +
  scale_y_gts_accuracy +
  scale_color_distractors +
  chance_line +
  chance_label +
  coord_cartesian(xlim = c(-0.2, 7.2), ylim = ylim_gts) +
  base_theme +
  theme(legend.position = c(0.8, 0.85))

# ---- transcriptions ------------------------------------------------------------
orthographic_distance_mod <- lmer(distance ~ message_c + (message_c|chain_name/seed_id),
                                  data = transcription_distances)

orthographic_distance_lmertest_mod <- lmerTest::lmer(
  formula(orthographic_distance_mod), data = orthographic_distance_mod@frame
)

orthographic_distance_preds <- data_frame(message_c = c(-0.5, 0.5)) %>%
  cbind(., predictSE(orthographic_distance_mod, newdata = ., se = TRUE)) %>%
  rename(distance = fit, se = se.fit) %>%
  recode_message_type()

gg_distance <- ggplot(transcription_distances) +
  aes(message_label, distance, color = message_label) +
  geom_point(aes(group = message_id),
             stat = "summary", fun.y = "mean",
             position = position_jitter(0.2, 0.01),
             alpha = 0.8, size = 2) +
  geom_errorbar(aes(ymin = distance - se, ymax = distance + se),
                data = orthographic_distance_preds,
                size = 1.4, width = 0.1) +
  scale_x_discrete("Generation",
                   labels = c("First", "Last")) +
  scale_y_continuous("Distance between transcriptions", breaks = seq(0, 1, by = 0.2)) +
  scale_color_manual(values = imitation_gen_colors) +
  scale_fill_manual(values = imitation_gen_colors) +
  coord_cartesian(ylim = c(0.0, 0.8)) +
  base_theme +
  theme(legend.position = "none")

# ---- matching-transcriptions ---------------------------------------------------
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
  message_label_2 = c("First generation", "Last generation")
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
            data = data.frame(message_label_2 = "First generation", question_c = -0.7,
                              is_correct = 0.26, label = "chance"),
            fontface = "italic") +
  coord_cartesian(ylim = ylim_gts) +
  facet_wrap("message_label_2") +
  base_theme +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# ---- category-learning
first_last_gen <- filter(learning_sound_names, message_type != "sound_effect") %>%
  mutate(block_ix_sqr = block_ix^2)

after_first_block <- filter(first_last_gen, block_ix > 1)
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
  geom_smooth(aes(ymin = rt - se, ymax = rt + se, color = message_label),
              fill = "gray", alpha = 0.4,
              stat = "identity", data = lsn_quad_preds) +
  scale_x_block_ix +
  scale_y_rt +
  scale_color_message_label_2 +
  coord_cartesian(ylim = c(600, 1200)) +
  base_theme +
  theme(legend.position = c(0.8, 0.7))

transition_mod <- lmer(
  rt ~ block_transition_c * message_c + block_ix + (block_ix|subj_id),
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
                 size = 2) +
  geom_line(aes(group = message_type), data = transition_preds,
            position = dodger, size = 2) +
  scale_x_discrete("Block transition", labels = c("Before", "After")) +
  scale_y_rt +
  scale_color_message_label_2 +
  coord_cartesian(ylim = c(600, 1200)) +
  base_theme +
  theme(legend.position = c(0.85, 0.8))
