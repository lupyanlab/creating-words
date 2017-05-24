source("R/0-setup.R")

messages_transcribed_and_matched <- unique(transcription_matches$message_id)

imitation_matches_subset <- imitation_matches %>%
  filter(
    message_id %in% messages_transcribed_and_matched,
    survey_type != "within"
  )

transcription_accuracies <- transcription_matches %>%
  group_by(message_id, question_type, generation) %>%
  summarize(accuracy = mean(is_correct)) %>%
  ungroup() %>%
  mutate(question_type = ifelse(question_type == "category", "category_match",
                                ifelse(question_type == "exact", "true_seed", NA)))

imitation_accuracies <- imitation_matches_subset %>%
  group_by(message_id, question_type, generation) %>%
  summarize(accuracy = mean(is_correct)) %>%
  ungroup()

compare_accuracies <- bind_rows(
    transcriptions = transcription_accuracies,
    imitations = imitation_accuracies,
    .id = "experiment"
  ) %>%
  mutate(
    message_type = ifelse(generation == 1, "first_gen", "last_gen")
  )

ggplot(compare_accuracies) +
  aes(experiment, accuracy, group = message_id) +
  geom_line(color = "gray", size = 0.6) +
  geom_line(aes(group = 1), stat = "summary", fun.y = "mean", size = 2) +
  facet_grid(question_type ~ message_type)
