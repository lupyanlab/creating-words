source("R/0-setup.R")

# ---- 2-measuring-acoustic-similarity
data("acoustic_similarity_judgments")
data("acoustic_similarity_linear")

# Compare human and machine similarity
edge_similarities <- 
  merge(
    acoustic_similarity_judgments %>%
      group_by(sound_x, sound_y) %>%
      summarize(similarity_judgments = mean(similarity, na.rm = TRUE)),
    acoustic_similarity_linear %>%
      group_by(sound_x, sound_y) %>%
      summarize(similarity_algorithmic = mean(similarity, na.rm = TRUE))
  )

comparing_similarities_cor_test <- with(edge_similarities,
    cor.test(similarity_judgments, similarity_algorithmic)
  ) %>%
  tidy()

gg_comparing_similarities <- ggplot(edge_similarities) +
  aes(similarity_judgments, similarity_algorithmic) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
