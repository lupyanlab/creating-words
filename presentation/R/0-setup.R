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

library(ggraph)
library(igraph)

library(crotchet)
library(wordsintransition)

# helper functions

count_subjects   <- . %>% count_unique("subj_id")
count_imitations <- . %>% count_unique("message_id")

recode_filename <- function(frame, custom_levels) {
  if (!missing(custom_levels)) filename_levels <- custom_levels
  frame %>%
    mutate(filename_f = factor(filename, levels = filename_levels))
}


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

chance_line <- geom_hline(yintercept = 0.25, lty = 2, alpha = 0.4, size = 0.6)
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

lm_mod_results <- function(lm_mod, param, p_value_only = FALSE) {
  results <- broom::tidy(lm_mod) %>%
    filter(term == param) %>%
    as.list()
  
  lm_summary <- broom::glance(lm_mod) %>% as.list()
  results$df <- lm_summary$df.residual

  if (results$p.value < 0.001) {
    results$p_value_str <- "_p_ < 0.001"
  } else {
    results$p_value_str <- paste("_p_ = ", round(results$p.value, 3))
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

report_icc_results <- function(irr_results) {
  if (irr_results$p.value < 0.001) {
    p_value_str = "_p_ < 0.001"
  } else {
    p_value_str = sprintf("_p_ = %.3f", irr_results$p.value)
  }
  sprintf("ICC = %.2f, 95%% CI [%.2f, %.2f], F(%d, %d) = %.2f, %s",
          irr_results$value,
          irr_results$lbound, irr_results$ubound,
          irr_results$df1, irr_results$df2, irr_results$Fvalue,
          p_value_str)
}

report_cor_test <- function(cor_test) {
  results <- broom::tidy(cor_test) %>% as.list()
  sprintf("_r_ = %.2f, 95%% CI [%.2f, %.2f]",
          results$estimate, results$conf.low, results$conf.high)
}