source("R/0-setup.R")

# ---- s1-selecting-seed-sounds

## Selecting seed sounds ##

drop_bad_trials <- . %>%
  filter(failed_catch_trial == 0, problem_with_audio == 0)

recode_filename <- function(frame, custom_levels) {
  if (!missing(custom_levels)) filename_levels <- custom_levels
  frame %>%
    mutate(filename_f = factor(filename, levels = filename_levels))
}

count_responses <- function(frame, filename_levels) {
  frame %>%
    drop_bad_trials() %>%
    recode_filename(custom_levels = filename_levels) %>%
    count(filename_f) %>%
    select(filename_f, n) %>%
    complete(filename_f, fill = list(n = 0)) %>%
    mutate(filename = as.character(filename_f)) %>%
    left_join(filename_map) %>%
    arrange(category, desc(n))
}

# Create a factor for ordered and missing filenames.
filename_map <- expand.grid(
  category = unique(sound_similarity_6$category),
  ix = 1:6
) %>%
  mutate(filename = paste0(category, "_0", ix, ".mp3")) %>%
  arrange(category, ix) %>%
  select(category, filename)

filename_levels <- filename_map$filename
sound_similarity_6_counts <- sound_similarity_6 %>%
  count_responses(filename_levels) %>%
  # label two most frequently selected messages as odd
  group_by(category) %>%
  mutate(odd_one_out = ifelse(n >= n[2], "odd", "normal")) %>%
  ungroup()

kept <- sound_similarity_6_counts %>%
  filter(odd_one_out == "normal") %>%
  .$filename %>%
  unique()

filename_levels <- filename_levels[filename_levels %in% kept]
final_categories <- c("water", "tear", "zipper", "glass")
sound_similarity_4_counts <- sound_similarity_4 %>%
  count_responses(filename_levels) %>%
  # label categories used in final experiment
  mutate(odd_one_out = ifelse(category %in% final_categories, "normal", "odd"))

odd_one_out <- ggplot(mapping = aes(x = filename_f, y = n)) +
  geom_bar(aes(fill = odd_one_out), stat = "identity") +
  scale_x_discrete("") +
  scale_y_continuous("Number of times selected as odd one out") +
  scale_fill_manual("", labels = c("kept", "dropped"),
                    values = RColorBrewer::brewer.pal(3, "Set2")[c(1, 2)]) +
  facet_wrap("category", scales = "free_x", ncol = 2) +
  base_theme +
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.direction = "vertical",
    legend.position = c(0.06, 1.0)
  )

gg_seed_selection_round_1 <- (odd_one_out %+% sound_similarity_6_counts) +
  theme(
    legend.position = c(0.15, 0.98),
    legend.key.size = unit(0.5, "lines"),
    axis.title.y = element_text(margin = margin(0, 2, 0, 0, unit = "lines"))
  ) +
  ggtitle("a. Odd one out (6 per category)")

gg_seed_selection_round_2 <- (odd_one_out %+% sound_similarity_4_counts) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  ) +
  ggtitle("b. Odd one out (4 per category)")

pdf("~/Desktop/s1.pdf", width=6.5, height=10)
grid.arrange(
  gg_seed_selection_round_1,
  gg_seed_selection_round_2,
  nrow = 1
)
dev.off()