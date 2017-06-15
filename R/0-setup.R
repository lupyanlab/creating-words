# ---- setup
library(tidyverse)
library(stringr)
library(magrittr)

library(grid)
library(gridExtra)
library(png)

library(lme4)  # lmerTest is also required
library(AICcmodavg)
library(broom)

library(irr)

library(crotchet)
library(wordsintransition)

count_subjects   <- . %>% count_unique("subj_id")
count_imitations <- . %>% count_unique("message_id")

# Selecting seed sounds
data("sound_similarity_4")
data("sound_similarity_6")
n_norming_subjects <- count_unique(sound_similarity_4, "workerId") +
  count_unique(sound_similarity_6, "workerId")


data("imitations")
n_all_imitations <- count_imitations(imitations)
n_imitators <- count_subjects(imitations)
n_removed <- imitations %>%
  filter(rejected == "True") %>%
  count_imitations()
n_removed_pct <- round(n_removed/count_imitations(imitations) * 100)
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

data("algo_linear")

z_score <- function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
algo_linear %<>%
  recode_edge_generations() %>%
  group_by(sound_x_category) %>%
  mutate(similarity_z = z_score(similarity)) %>%
  ungroup()

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


data("transcriptions")
transcription_catch_trials <- transcriptions %>%
  filter(is_catch_trial == 1) %>%
  select(subj_id, chain_name, text)

n_all_transcribers <- count_subjects(transcriptions)
transcription_bad_subjs <- c("A3A8P4UR9A0DWQ", "AAMLJUUYM484")
transcriptions %<>% filter(!(subj_id %in% transcription_bad_subjs))
n_bad_transcribers <- length(transcription_bad_subjs)
n_transcribers <- count_subjects(transcriptions)
n_transcriptions <- nrow(transcriptions)
n_english_transcriptions <- transcription_frequencies %>%
  filter(is_english == 1) %>%
  select(text) %>%
  nrow()


n_imitations_transcribed <- count_imitations(transcriptions)
n_transcriptions_per_imitation <- transcriptions %>%
  count(message_id) %>%
  .$n %>%
  mean() %>%
  round(0)

data("transcription_frequencies")
n_created_words <- transcription_frequencies %>%
  filter(
    is_english == 0,
    # Exclude transcriptions of seed sounds
    seed_id != message_id
  ) %>%
  .$text %>%
  unique() %>%
  length()


data("transcription_distances")
message_id_map <- select(imitations, message_id, chain_name, seed_id, generation)
transcription_distances %<>%
  left_join(message_id_map) %>%
  recode_transcription_frequency() %>%
  recode_message_type() %>%
  filter(message_type != "sound_effect")

data("transcription_matches")
n_all_transcription_match_subjs <- count_subjects(transcription_matches)
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
  filter(
    message_type != "sound_effect",
    !(subj_id %in% transcription_match_failed_catch_trial)
  )

n_transcription_match_subjs <- count_subjects(transcription_matches)

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

# ggplot theme, colors, and scales ---------------------------------------------
base_theme <- theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))

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
# scale_linetype_distractors <- 
scale_x_trial_ix <- scale_x_continuous("Trial number (24 trials per block)",
                                       breaks = seq(1, 96, by = 24))
scale_x_block_ix <- scale_x_continuous("Block number (24 trials per block)",
                                       breaks = 1:4)
scale_y_rt <- scale_y_continuous("Reaction time (msec)")
scale_color_message_label <- scale_color_manual(
  "Transcription of",
  labels = c("Sound effect", "First generation imitation", "Last generation imitation"),
  values = unname(colors[c("orange", "green", "blue")])
)
scale_color_message_label_2 <- scale_color_manual(
  "Transcription of",
  labels = c("First generation imitation", "Last generation imitation"),
  values = unname(colors[c("green", "blue")])
)
scale_linetype_message_label_2 <- scale_linetype_manual(
  "Transcription of",
  labels = c("First generation imitation", "Last generation imitation"),
  values = c("solid", "longdash")
)

chance_line <- geom_hline(yintercept = 0.25, lty = 2, alpha = 0.4, size = 1)
ylim_gts <- c(0.11, 0.75)
scale_y_gts_accuracy <- scale_y_continuous("Accuracy", breaks = seq(0, 1, by = 0.1),
                                           labels = scales::percent)

# reporting statistical results ------------------------------------------------
lmer_mod_results <- function(lmertest_mod, param, p_value_only = FALSE) {
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
  
  if (p_value_only == TRUE) {
    return(results$p_value_str)
  }
  
  sprintf("_b_ = %.2f (SE = %.2f), _t_(%.1f) = %.2f, %s",
          results$estimate, results$std.error, results$df, results$statistic, results$p_value_str)
}


glmer_mod_results <- function(glmer_mod, param, odds = FALSE, p_value_only = FALSE) {
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
    formatted = sprintf("_b_ = %.2f (SE = %.2f) log-odds, odds = %.2f, _z_ = %.2f, %s",
                        results$estimate, results$std.error, results$odds, results$statistic, results$p_value_str)
  } else {
    formatted = sprintf("_b_ = %.2f (SE = %.2f) log-odds, _z_ = %.2f, %s",
                        results$estimate, results$std.error, results$statistic, results$p_value_str)
  }
  
  if (p_value_only == TRUE) {
    return(results$p_value_str)
  }

  formatted
}
