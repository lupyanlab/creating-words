library(tidyverse)
library(magrittr)
library(wordsintransition)
data("transcription_frequencies")
data("imitations")
generations <- imitations %>% select(message_id, generation)
transcription_frequencies %<>% left_join(generations)

transcription_frequencies %>%
  filter(chain_name == "tear") %>%
  arrange(generation) %>%
  View()
