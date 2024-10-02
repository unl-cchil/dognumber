## ---
##
## Script name: deboer_etal_2024_rcode.R
##
## Purpose of script: Analyze dog food quantity preference data
##
## Author: Jeffrey R. Stevens (jeffrey.r.stevens@gmail.com)
##
## Date Created: 2024-08-16
##
## Date Finalized: 2024-09-24
##
## License: All materials presented here are released under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0).
##  You are free to:
##   Share — copy and redistribute the material in any medium or format
##   Adapt — remix, transform, and build upon the material for any purpose, even commercially.
##  Under the following terms:
##   Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licencor endorses you or your use.
##   No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
##
##
## ---


# Load libraries ----------------------------------------------------------

# Install cocoon and detritus packages with:
#  install.packages("remotes")
#  remotes::install_github("JeffreyRStevens/cocoon")
#  remotes::install_github("JeffreyRStevens/detritus")

suppressPackageStartupMessages({
  library(tidyverse)
  library(BayesFactor)
  library(bayestestR)
  library(cocoon)
  library(detritus)
  library(ggrepel)
  
  library(labelled)
  library(lme4)
  library(papaja)
  library(patchwork)
  library(performance)
  library(scales)
})

# Create functions ----------------------------------------------------------

# Function that finds best fitting difference and ratio models
# If rand is NULL, find best fitting random effects model. Otherwise, use specified random effect
compare_diff_ratio_models <- function(x, rand = NULL) {
  if (!is.null(rand)) {
    # use specified random effects model
    stopifnot(is.character(rand))
    rand_effect <- rand
  } else {
    # find best fitting random effect
    rand_model <- detritus::find_best_random_effect(x, "choice", c("dog_id", "pair"))
    print(paste0("Best random effect model is ", rand_model))
    rand_effect <- sub("choice ~ 1 ", "", rand_model)
  }

  # Fit all models
  model_null <- glmer(formula(paste0("choice ~ 1", rand_effect)), data = x, family = binomial)
  model_diff <- glmer(formula(paste0("choice ~ diff", rand_effect)), data = x, family = binomial)
  model_ratio <- glmer(formula(paste0("choice ~ ratio", rand_effect)), data = x, family = binomial)
  model_diff_ratio <- glmer(formula(paste0("choice ~ diff + ratio", rand_effect)), data = x, family = binomial)
  model_diffxratio <- glmer(formula(paste0("choice ~ diff * ratio", rand_effect)), data = x, family = binomial)

  # Find best fitting model
  all_models <- detritus::compare_models(model_null,
                                         model_diff,
                                         model_ratio,
                                         model_diff_ratio,
                                         model_diffxratio)
  best_model <- detritus::find_best_model(all_models, envir = environment())
  output <- list(models = all_models, best_model = best_model)
  return(output)
}


# Import data -------------------------------------------------------------

data_dictionary <- read_csv("deboer_etal_2024_datadictionary.csv",
                            show_col_types = FALSE)
all_data <- read_csv("deboer_etal_2024_data.csv",
                     show_col_types = FALSE) |>
  labelled::set_variable_labels(.labels = data_dictionary$label)

our_data <- all_data |>
  filter(study == "Current study")
rb_dogs1 <- all_data |>
  filter(study == "Rivas-Blanco Dogs - Phase 1")
rb_dogs2 <- all_data |>
  filter(study == "Rivas-Blanco Dogs - Phase 2")


# Clean complete session data ---------------------------------------------

# Find complete sessions

complete_sessions <- our_data |>
  group_by(dog_id, block) |>
  count() |>
  filter(n == 11)

complete_subjects <- our_data |>
  filter(block == 10) |> # find subjects who reach block 10
  pull(dog_id) |>
  unique()

# Clean data

clean_data <- our_data |>
  filter(pair != "1:6" & dog_id %in% complete_subjects) |>
  semi_join(complete_sessions, by = c("dog_id", "block")) |>
  group_by(dog_id) |>
  mutate(block = consecutive_id(session), .after = session) |>
  mutate(pair = paste0("[", pair, "]"),
         pair = sub(":", ", ", pair)) |>
  ungroup() |>
  mutate(
    ratio_clean = case_when(ratio == 1 / 3 ~ "1:3",
                            ratio == 0.5 ~ "1:2",
                            ratio == 2 / 3 ~ "2:3"),
    ratio_clean = fct_relevel(ratio_clean, "1:3", "1:2", "2:3")
  )


# Demographics ------------------------------------------------------------

demographics <- clean_data |>
  group_by(dog_id) |>
  slice_head(n = 1) |>
  ungroup() |>
  summarise(
    dog_age_mean = mean(dog_age),
    dog_age_sd = sd(dog_age),
    dog_weight_mean = mean(dog_weight),
    owner_age_mean = mean(owner_age),
    owner_age_sd = sd(owner_age)
  )

demographics$dog_female <-
  clean_data |>
  group_by(dog_id) |>
  slice_head(n = 1) |>
  ungroup() |>
  count(dog_sex) |>
  mutate(freq = n / sum(n) * 100) |>
  filter(dog_sex == "Female") |>
  pull(freq)

demographics$dog_neutered <-
  clean_data |>
  group_by(dog_id) |>
  slice_head(n = 1) |>
  ungroup() |>
  count(dog_neutered) |>
  mutate(freq = n / sum(n) * 100) |>
  filter(dog_neutered == "Yes") |>
  pull(freq)

demographics$owner_woman <-
  clean_data |>
  group_by(dog_id) |>
  slice_head(n = 1) |>
  ungroup() |>
  count(owner_gender) |>
  mutate(freq = n / sum(n) * 100) |>
  filter(owner_gender == "Female/Woman") |>
  pull(freq)


# Reliability -------------------------------------------------------------

reliability <- clean_data |>
  filter(!is.na(recode_side)) |>
  select(dog_id, block, trial, pair, choice_side, recode_side) |>
  mutate(agree = if_else(choice_side == recode_side, 1, 0))
agreement_recode <- mean(reliability$agree, na.rm = TRUE)
ratings <- select(reliability, choice_side, recode_side) |>
  as.matrix()
kappa_recode <- psych::cohen.kappa(ratings)


# Pair means --------------------------------------------------------------

# Mean & SD accuracy per pair/ratio/diff
pair_means <- clean_data |>
  summarise(
    .by = c(pair, diff, ratio_clean),
    mean_larger = mean(choice, na.rm = TRUE),
    sd_larger = sd(choice, na.rm = TRUE)
  ) |>
  arrange(pair)

# Pair means per subject
subject_pair_means <- clean_data |>
  summarise(
    .by = c(dog_id, pair, ratio, ratio_clean, diff),
    mean_larger = mean(choice, na.rm = TRUE)
  )


# Acquisition -------------------------------------------------------------

subject_block_means <- clean_data |>
  summarise(.by = c(dog_id, block),
            mean_larger = mean(choice, na.rm = TRUE)) |>
  mutate(block = as.factor(block))
subject_block_wsci <- subject_block_means |>
  wsci(id = "dog_id",
       dv = "mean_larger",
       factors = c("block")) |>
  summary()

subject_block_wsci |>
  ggplot(aes(x = block, y = mean)) +
  geom_hline(yintercept = 0.5,
             linetype = 2,
             color = "grey") +
  geom_line(aes(y = mean_larger, group = dog_id, color = dog_id),
            data = subject_block_means,
            alpha = 0.25) +
  geom_pointrange(aes(y = mean, ymin = lower_limit, ymax = upper_limit)) +
  labs(x = "Session", y = "Mean choice for larger") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0.35, 1.05),
    breaks = seq(0.4, 1, 0.1)
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Arial", size = 14),
    panel.grid = element_blank(),
    legend.position = "none"
  )
ggsave(
  "figures/acquisition.png",
  height = 6,
  width = 9,
  scale = 0.5
)


# Hypothesis 1 ----------------------------------------------------------

subject_diff_ratio <- subject_pair_means |>
  summarise(
    .by = c("dog_id", "diff", "ratio", "ratio_clean"),
    mean_larger = mean(mean_larger, na.rm = TRUE)
  )
pair_diff_ratio_wsci <- subject_diff_ratio |>
  complete(dog_id, ratio, diff, fill = list(mean_larger = 0)) |>
  wsci(id = "dog_id",
       dv = "mean_larger",
       factors = c("diff", "ratio")) |>
  summary() |>
  filter(mean != 0) |>
  mutate(
    ratiochar = ratio,
    diff = as.numeric(as.character(diff)),
    ratio = as.numeric(as.character(ratio)),
    ratio_clean = case_when(
      ratio == 0.333333333333333 ~ "1:3",
      ratio == 0.5 ~ "1:2",
      ratio == 0.666666666666667 ~ "2:3"
    ),
    ratio_clean = fct_relevel(ratio_clean, "1:3", "1:2", "2:3"),
    ratio2 = format_num(ratio, 2)
  )

subject_diff <- subject_pair_means |>
  summarise(mean_larger = mean(mean_larger, na.rm = TRUE),
            .by = c("dog_id", "diff"))
subject_pair_diff_wsci <- subject_diff |>
  wsci(id = "dog_id", dv = "mean_larger", factors = "diff") |>
  summary() |>
  mutate(diff = as.numeric(as.character(diff)))


diff_plot <- subject_pair_diff_wsci |>
  ggplot(aes(x = diff, y = mean)) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "grey"
  ) +
  geom_line(
    aes(y = mean_larger, group = dog_id, color = dog_id),
    alpha = 0.25,
    data = subject_diff
  ) +
  geom_pointrange(aes(ymin = lower_limit, ymax = upper_limit)) +
  labs(x = "Difference", y = "Mean choice for larger") + #, caption = "WSCIs.") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  theme(
    text = element_text(family = "Arial", size = 14),
    panel.grid = element_blank(),
    # theme_classic() +
    # theme(panel.border = element_rect(color = "grey20", fill = "transparent"),
    legend.position = "none"
  )


subject_ratio <- subject_pair_means |>
  summarise(
    .by = c("dog_id", "ratio"),
    mean_larger = mean(mean_larger, na.rm = TRUE)
  )
subject_pair_ratio_wsci <- subject_ratio |>
  wsci(id = "dog_id", dv = "mean_larger", factors = "ratio") |>
  summary() |>
  mutate(ratio = as.numeric(as.character(ratio)))

ratio_plot <- subject_pair_ratio_wsci |>
  ggplot(aes(x = ratio, y = mean)) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "grey"
  ) +
  geom_line(
    aes(y = mean_larger, group = dog_id, color = dog_id),
    alpha = 0.25,
    data = subject_ratio
  ) +
  geom_pointrange(aes(ymin = lower_limit, ymax = upper_limit)) +
  scale_y_continuous(labels = scales::percent, limits = c(0.5, 1)) +
  labs(x = "Ratio", y = "Mean choice for larger") +
  theme_bw() +
  theme(
    text = element_text(family = "Arial", size = 14),
    panel.grid = element_blank(),
    legend.position = "none"
  )



# Hypothesis 2 ----------------------------------------------------------

diff_ratio_plot <- subject_pair_means |>
  mutate(ratio2 = format_num(ratio, 2)) |>
  ggplot(aes(x = diff, y = mean_larger)) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "grey"
  ) +
  geom_line(aes(group = dog_id, color = dog_id), alpha = 0.25) +
  geom_pointrange(aes(
    y = mean, ymin = lower_limit, ymax = upper_limit
  ), data = pair_diff_ratio_wsci) +
  facet_wrap(vars(ratio2), labeller = labeller(ratio2 = ~ paste("Ratio:", .x))) +
  labs(x = "Difference", y = "Mean choice for larger") + #, caption = "WSCIs.") +
  scale_x_continuous(breaks = 1:6) +
  theme_bw() +
  theme(
    text = element_text(family = "Arial", size = 14),
    panel.grid = element_blank(),
    legend.position = "none"
  )


(diff_plot + ratio_plot) / diff_ratio_plot + plot_layout(axes = "collect_y") + plot_annotation(tag_levels = "A")
ggsave(
  "figures/ratio_diff.png",
  width = 8,
  height = 8,
  scale = 0.85
)


# Pre-registered models ---------------------------------------------------

null_model1 <- glmer(choice ~ 1 + (1 | dog_id),
                     family = binomial,
                     data = clean_data)
diff_model1 <- glmer(choice ~ diff + (1 | dog_id),
                     family = binomial,
                     data = clean_data)
ratio_model1 <- glmer(choice ~ ratio + (1 | dog_id),
                      family = binomial,
                      data = clean_data)
diff_ratio_model1 <- glmer(choice ~ diff + ratio + (1 | dog_id),
                           family = binomial,
                           data = clean_data)
diffxratio_model1 <- glmer(choice ~ diff * ratio + (1 | dog_id),
                           family = binomial,
                           data = clean_data)

detritus::compare_models(null_model1, diff_model1, ratio_model1)

diff_bf <- detritus::compare_models(diff_model1, diff_ratio_model1)
ratio_bf <- detritus::compare_models(ratio_model1, diff_ratio_model1)


# Exploratory models ------------------------------------------------------

# Add in interaction model
(all_models_bfs <- compare_diff_ratio_models(clean_data, rand = "+ (1 | dog_id)"))
models_bfs <- all_models_bfs$models

# Split data by ratio and test for difference effects
split_data <- clean_data |>
  summarise(
    .by = c(dog_id, diff, ratio_clean),
    mean_choice = mean(choice, na.rm = TRUE)
  ) |>
  split(clean_data$ratio_clean)
diff_bfs <- map(split_data,
                ~ lmBF(mean_choice ~ diff, whichRandom = "dog_id", data = .))

# Re-run model comparison allowing selection of random effects
diff_ratio_comparison <- compare_diff_ratio_models(clean_data)
diff_ratio_comparison$best_model$formula



# Ward and Smuts 2007 -----------------------------------------------------

ward_smuts <- data.frame(
  small = c(3, 1, 2, 1, 2, 3, 2, 1),
  large = c(4, 2, 3, 3, 5, 5, 4, 4),
  choice = c(55.5, 60.5, 60.5, 71.9, 71.9, 77.8, 82.8, 88.7)
) |>
  mutate(
    pair = paste0("[", small, ", ", large, "]"),
    diff = large - small,
    ratio = small / large,
    .after = large
  )

ws_pairs_plot <- ward_smuts |>
  summarise(.by = c(pair, diff, ratio),
            mean_larger = mean(choice, na.rm = TRUE) / 100) |>
  mutate(difff = as.factor(diff)) |>
  ggplot(aes(
    x = ratio,
    y = mean_larger,
    color = difff,
    shape = difff
  )) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             color = "grey") +
  geom_point(size = 3) +
  geom_text(
    aes(label = pair, family = "Arial"),
    nudge_x = 0.03,
    nudge_y = 0.025,
    show.legend = FALSE
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0.5, 1)) +
  xlim(0.33, 0.71) +
  labs(
    x = "Numerical ratio",
    y = "Mean choice for larger",
    color = "Numerical\ndifference",
    shape = "Numerical\ndifference"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Arial", size = 14),
    legend.position = "inside",
    legend.position.inside = c(0.68, 0.9),
    legend.direction = "horizontal",
    legend.box.background = element_rect(color = "grey50"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.margin = margin(0, 0, 0, 0),
    legend.key.spacing = unit(0, "cm"),
    panel.grid = element_blank()
  )

ws_null <- lm(choice ~ 1, data = ward_smuts)
ws_diff <- lm(choice ~ diff, data = ward_smuts)
ws_ratio <- lm(choice ~ ratio, data = ward_smuts)
ws_diff_ratio <- lm(choice ~ diff + ratio, data = ward_smuts)
ws_diffxratio <- lm(choice ~ diff * ratio, data = ward_smuts)

(
  ws_models <- detritus::compare_models(ws_null, ws_diff, ws_ratio, ws_diff_ratio, ws_diffxratio)
)
ws_best_model <- detritus::find_best_model(ws_models)


# Rivas-Blanco et al. 2020 ------------------------------------------------

## Dogs Phase 1 ------------------------------------------------------------

rb_pairs_plot <- rb_dogs1 |>
  mutate(pair = paste0("[", small, ", ", large, "]")) |>
  summarise(.by = c(pair, diff, ratio),
            mean_larger = mean(choice, na.rm = TRUE)) |>
  mutate(difff = as.factor(diff)) |>
  ggplot(aes(
    x = ratio,
    y = mean_larger,
    color = difff,
    shape = difff
  )) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             color = "grey") +
  geom_point(size = 3) +
  geom_text(
    aes(label = pair, family = "Arial"),
    nudge_x = 0.03,
    nudge_y = 0.025,
    show.legend = FALSE
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0.5, 1)) +
  xlim(0.33, 0.71) +
  labs(
    x = "Numerical ratio",
    y = "Mean choice for larger",
    color = "Numerical\ndifference",
    shape = "Numerical\ndifference"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Arial", size = 14),
    legend.position = "inside",
    legend.position.inside = c(0.68, 0.9),
    legend.direction = "horizontal",
    legend.box.background = element_rect(color = "grey50"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.margin = margin(0, 0, 0, 0),
    legend.key.spacing = unit(0, "cm"),
    panel.grid = element_blank()
  )


rbd1_models <- compare_diff_ratio_models(rb_dogs1, rand = "+ (1 | dog_id)")
rbd1_models$best_model$formula


## Dogs Phase 2 ------------------------------------------------------------

rbd2_models <- compare_diff_ratio_models(rb_dogs2, rand = "+ (1 | dog_id)")
rbd2_models$best_model$formula


# Pairs plot --------------------------------------------------------------

current_pairs <- clean_data |>
  filter(diff != 6) |>
  summarise(.by = c(pair, diff, ratio),
            mean_larger = mean(choice, na.rm = TRUE)) |>
  mutate(study = "Current study")
ws_pairs <- ward_smuts |>
  summarise(.by = c(pair, diff, ratio),
            mean_larger = mean(choice, na.rm = TRUE) / 100) |>
  mutate(study = "Ward & Smuts (2007)")

rb_pairs <- rb_dogs1 |>
  mutate(pair = paste0("[", small, ", ", large, "]")) |>
  summarise(.by = c(pair, diff, ratio),
            mean_larger = mean(choice, na.rm = TRUE)) |>
  mutate(study = "Rivas-Blanco et al. (2020)")

all_pairs <- bind_rows(current_pairs, ws_pairs, rb_pairs)

all_pairs |>
  mutate(
    difff = as.factor(diff),
    study = fct_relevel(study, "Current study", "Ward & Smuts (2007)")
  ) |>
  ggplot(aes(
    x = ratio,
    y = mean_larger,
    color = difff,
    shape = difff
  )) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             color = "grey") +
  geom_point(size = 2.25) +
  geom_text_repel(aes(label = pair, family = "Arial"),
                  size = 3,
                  show.legend = FALSE) +
  facet_wrap(vars(study)) +
  scale_y_continuous(labels = scales::percent, limits = c(0.5, 1)) +
  labs(
    x = "Numerical ratio",
    y = "Mean choice for larger",
    color = "Numerical\ndifference",
    shape = "Numerical\ndifference"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Arial", size = 14),
    legend.position = "inside",
    legend.direction = "horizontal",
    legend.position.inside = c(0.84, 0.9),
    legend.box.background = element_rect(color = "grey50"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.margin = margin(0, 0, 0, 0),
    legend.key.spacing = unit(0, "cm"),
    panel.grid = element_blank()
  )
ggsave(
  "figures/all_study_pairs.png",
  height = 4,
  width = 10,
  scale = 0.7
)
