source("R/0-setup.R")

# ---- 4-transcribing-imitations
transcriptions %<>%
  filter(is_catch_trial == 0) %>%
  # label the generation of the imitations being transcribed
  left_join(gen_labels) %>%
  recode_message_type

base <- ggplot() +
  base_theme

scale_x_generation <- scale_x_continuous(breaks = 0:8)

hist <- base +
  geom_histogram(aes(x = generation), binwidth = 1,
                 color = "black", fill = "white", alpha = 0.6) +
  geom_text(aes(x = generation, label = ..count..),
            stat = "bin", binwidth = 1, vjust = -0.6) +
  scale_x_generation

transcribed_imitations <- imitations %>%
  filter(message_id %in% transcriptions$message_id)

hist_no_labels <- hist
hist_no_labels$layers[[2]] <- NULL

gg_proportion_transcriptions <- (hist_no_labels %+% transcribed_imitations) +
  geom_histogram(aes(x = generation), data = imitations,
                 binwidth = 1, fill = "black", alpha = 0.2) +
  ggtitle("Proportion of imitations transcribed")


transcription_frequencies %<>%
  recode_message_type %>%
  filter(message_type != "sound_effect")

transcription_uniqueness <- transcription_frequencies %>%
  group_by(message_type, message_label, message_id) %>%
  summarize(
    num_words = sum(n),
    num_unique = n_distinct(text),
    perct_unique = num_unique/num_words,
    perct_agreement = 1 - perct_unique
  ) %>%
  ungroup %>%
  mutate(
    no_agreement = as.integer(perct_agreement == 0)
  ) %>%
  recode_transcription_frequency

set.seed(752)  # for replicable position_jitter
gg_exact_matches <- ggplot(transcription_uniqueness) +
  aes(x = message_label, y = perct_agreement) +
  geom_point(aes(color = frequency_type),
             position = position_jitter(0.1, 0.01), shape = 1) +
  geom_point(aes(color = frequency_type), stat = "summary", fun.y = "mean",
             size = 3) +
  scale_x_discrete("") +
  scale_y_continuous("Transcription agreement", labels = scales::percent) +
  scale_color_brewer("", palette = "Set2") +
  base_theme +
  theme(legend.position = "top") +
  ggtitle("A. Exact string matches")


message_id_map <- select(imitations, message_id, seed_id, generation)

distance_plot <- ggplot(transcription_distances) +
  aes(message_label, distance) +
  base_theme

gg_string_distance <- distance_plot + 
  geom_bar(aes(fill = frequency_type, width = 0.96), stat = "summary", fun.y = "mean",
           alpha = 0.6) +
  geom_point(aes(color = frequency_type, group = message_id), stat = "summary", fun.y = "mean",
             position = position_jitter(0.3, 0.01)) +
  scale_x_discrete("") +
  scale_y_continuous("Distance between transcriptions") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap("frequency_type") +
  guides(color = "none", fill = "none") +
  ggtitle("B. Transcription distance by agreement")

gg_length_plot <- ggplot(transcription_distances) +
  aes(message_label, length) +
  geom_point(aes(group = message_id, color = frequency_type), stat = "summary", fun.y = "mean",
             position = position_jitter(0.3, 0.1)) +
  geom_line(aes(group = frequency_type, color = frequency_type), stat = "summary", fun.y = "mean") +
  scale_color_brewer("", palette = "Set2") +
  labs(x = "", y = "Longest substring match (characters)",
       title = "C. Substring match length") + 
  base_theme +
  theme(legend.position = "top")

labels <- imitations %>%
  select(message_id, seed_id, first_gen_id, generation)

sample_first_last_transcriptions <- transcription_frequencies %>%
  filter(
    is_english == 0,
    !str_detect(text, "[\ *-]")
  ) %>%
  left_join(labels) %>%
  recode_message_type() %>%
  filter(message_type != "sound_effect") %>%
  group_by(chain_name, seed_id, first_gen_id, message_type) %>%
  arrange(desc(n)) %>%
  mutate(word_ix = 1:n()) %>%
  filter(word_ix == 1) %>%
  ungroup() %>%
  select(chain_name, seed_id, first_gen_id, message_type, text) %>%
  spread(message_type, text)
