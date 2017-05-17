source("R/0-setup.R")

# ---- 4-transcribing-imitations
data("transcription_frequencies")
data("imitations")

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

# write.csv(sample_first_last_transcriptions, "sample_first_last_transcriptions.csv", row.names = FALSE)
