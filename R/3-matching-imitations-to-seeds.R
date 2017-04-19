source("R/0-setup.R")

# ---- 3-matching-imitations-to-seeds
data("imitation_matches")

question_counts <- imitation_matches %>%
  filter(question_type != "catch_trial") %>%
  select(survey_type, question_pk) %>%
  unique() %>%
  count(survey_type) %>%
  rename(n_questions = n)

perfect_question_counts <- imitation_matches %>%
  filter(question_type != "catch_trial") %>%
  group_by(survey_type, question_pk) %>%
  summarize(
    mean_accuracy = mean(is_correct),
    is_perfect = (mean_accuracy == 0 | mean_accuracy == 1)
  ) %>%
  ungroup() %>%
  filter(is_perfect == 1) %>%
  count(survey_type, mean_accuracy) %>%
  left_join(question_counts) %>%
  mutate(
    pct_of_questions = n/n_questions,
    label = paste0("over( ", n, ", ", n_questions, ")"),
    label_y = pct_of_questions - 0.007
  )

dodge <- position_dodge(width = 0.9)

ggplot(perfect_question_counts) +
  aes(survey_type, pct_of_questions, fill = factor(mean_accuracy)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_text(aes(label = label, y = label_y), position = dodge, parse = TRUE) +
  scale_x_discrete("Question type", labels = c("True seed", "Same category", "Specific match")) +
  scale_y_continuous("Percentage of questions", labels = scales::percent) +
  scale_fill_discrete("Type of perfect question", labels = c("All incorrect", "All correct")) +
  theme(legend.position = "bottom") +
  ggtitle("Questions with perfect performance (all correct or all incorrect)")
