library(lme4)
library(lmerTest)
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(car)

# ============================================================
# FLAG: set to "IL" for Israeli participants, "UK" for British
# ============================================================
ORIGIN <- "IL"
# ============================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Dynamic output paths based on flag
origin_lower   <- tolower(ORIGIN)
results_file   <- paste0("results-", origin_lower, ".txt")
img_dir        <- paste0("images/", origin_lower, "/")
img_prefix     <- img_dir
dir.create(img_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Load data ----
rd_il <- bind_rows(
  read.csv("il_data_10-5/rd_il.csv"),
  read.csv("il_data_10-5/rd_il_cash.csv")
)

ratings_il <- bind_rows(
  read.csv("il_data_10-5/ratings_il.csv"),
  read.csv("il_data_10-5/ratings_il_cash.csv")
)

rd_uk      <- read.csv("UK2_rd.csv")
ratings_uk <- read.csv("UK2_ratings.csv")

ratings_il$Participant.Public.ID <- as.character(ratings_il$Participant.Public.ID)
rd_il$Participant.Public.ID      <- as.character(rd_il$Participant.Public.ID)

# add participant origin column
rd_il$participant_origin      <- "IL"
rd_uk$participant_origin      <- "UK"
ratings_il$participant_origin <- "IL"
ratings_uk$participant_origin <- "UK"

ratings_all <- bind_rows(ratings_il, ratings_uk)
rd_all      <- bind_rows(rd_il, rd_uk)

# ---- Preprocessing (all participants) ----
rd_clean <- subset(rd_all,
                   Display == "reproductionTask" &
                   Participant.Status == "complete")

rd_cleaner <- subset(rd_clean,
                     Store..RT > 100 & Store..RT < 4000)

ratings_clean <- subset(ratings_all,
                        Response.Type == "response" & !is.na(Response))

rd_analysis <- rd_cleaner[, c(
  "Participant.Public.ID",
  "Spreadsheet..Image",
  "Spreadsheet..stimulus_duration",
  "Store..RT",
  "participant_origin")]

rd_analysis <- rd_analysis[!duplicated(rd_analysis), ]

ratings_analysis <- ratings_clean[ratings_clean$Display == "ratings", c(
  "Event.Index",
  "Participant.Public.ID",
  "Object.Name",
  "Spreadsheet..Image",
  "Response")]

ratings_analysis$rating_type <- factor(ratings_analysis$Object.Name,
                                       levels = c("Slider", "Slider_att"),
                                       labels = c("familiarity", "appeal"))

ratings_wide <- pivot_wider(ratings_analysis,
                            id_cols   = c("Participant.Public.ID", "Spreadsheet..Image"),
                            names_from  = rating_type,
                            values_from = Response)

full_data_all <- merge(rd_analysis, ratings_wide,
                       by = c("Participant.Public.ID", "Spreadsheet..Image"))

names(full_data_all) <- c(
  "participant",
  "face",
  "duration",
  "rd",
  "participant_origin",
  "familiarity",
  "appeal")

full_data_all$duration    <- as.numeric(full_data_all$duration)
full_data_all$rd          <- as.numeric(full_data_all$rd)
full_data_all$familiarity <- as.numeric(full_data_all$familiarity)
full_data_all$appeal      <- as.numeric(full_data_all$appeal)

full_data_all$duration_factor <- factor(full_data_all$duration,
                                        levels = c(800, 1600),
                                        labels = c("short", "long"))

full_data_all$error <- full_data_all$rd - full_data_all$duration

# remove bad participants (mean short > mean long)
df_avg <- full_data_all %>%
  group_by(participant, duration_factor) %>%
  summarise(mean_rd = mean(rd), .groups = "drop")

df_avg_wide <- pivot_wider(df_avg,
                           id_cols     = "participant",
                           names_from  = duration_factor,
                           values_from = mean_rd)

df_avg_wide$bad_sub <- ifelse(df_avg_wide$short > df_avg_wide$long, TRUE, FALSE)
bad_subs <- df_avg_wide$participant[df_avg_wide$bad_sub == TRUE]
print(bad_subs)

full_data_all <- full_data_all[!full_data_all$participant %in% bad_subs, ]

# classify faces
full_data_all$stimuli_condition <- ifelse(
  substr(full_data_all$face, 1, 1) == "I", "IL",
  ifelse(substr(full_data_all$face, 1, 1) == "U", "UK",
         ifelse(substr(full_data_all$face, 1, 1) == "N", "Neutral", NA)))

# ============================================================
# FILTER: keep only the selected participant origin
# ============================================================
full_data_all <- full_data_all %>%
  filter(participant_origin == ORIGIN)

cat(sprintf("\n>>> Running analysis for %s participants only <<<\n", ORIGIN))
cat(sprintf("Participants after filter: %d\n\n", length(unique(full_data_all$participant))))

# ---- Face familiarity category (familiar = home faces, unfamiliar = other faces) ----
# For IL participants: IL faces = familiar, UK + Neutral = unfamiliar
# For UK participants: UK faces = familiar, IL + Neutral = unfamiliar
full_data_all$face_familiarity <- factor(
  ifelse(full_data_all$stimuli_condition == ORIGIN, "familiar", "unfamiliar"),
  levels = c("unfamiliar", "familiar"))

# ---- Residuals and derived variables ----
full_data_all <- full_data_all %>%
  group_by(participant, duration_factor) %>%
  mutate(rd_resid = rd - mean(rd, na.rm = TRUE)) %>%
  ungroup()

full_data_all$abs_resid <- abs(full_data_all$rd_resid)
full_data_all$abs_error <- abs(full_data_all$rd - full_data_all$duration)

full_data_all <- full_data_all %>%
  group_by(participant, duration_factor) %>%
  mutate(
    participant_mean = mean(rd, na.rm = TRUE),
    cv_resid_trial   = abs(rd_resid) / participant_mean) %>%
  ungroup()

# ---- Figure 1 – Familiarity distribution by stimulus condition ----
ggplot(full_data_all, aes(x = stimuli_condition, y = familiarity)) +
  geom_violin(trim = FALSE, alpha = 0.6, fill = ifelse(ORIGIN == "IL", "#E41A1C", "#377EB8")) +
  geom_boxplot(width = 0.15, alpha = 0.8, outlier.size = 0.5) +
  scale_x_discrete(labels = c("IL" = "IL faces", "UK" = "UK faces", "Neutral" = "Neutral faces")) +
  labs(title = sprintf("Familiarity by stimulus condition (%s participants)", ORIGIN),
       x = "Stimulus condition",
       y = "Familiarity rating (0-100)") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave(paste0(img_prefix, "figure_1.png"), width = 8, height = 6, dpi = 300)

# ---- Figure 1b – Familiarity per face (home faces only) ----
home_familiarity <- full_data_all %>%
  filter(stimuli_condition == ORIGIN) %>%
  group_by(face, stimuli_condition) %>%
  summarise(
    mean_fam = mean(familiarity, na.rm = TRUE),
    se_fam   = sd(familiarity, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop") %>%
  arrange(mean_fam) %>%
  mutate(face = factor(face, levels = face))

ggplot(home_familiarity, aes(x = face, y = mean_fam)) +
  geom_point(size = 2, colour = ifelse(ORIGIN == "IL", "#E41A1C", "#377EB8")) +
  geom_errorbar(aes(ymin = mean_fam - se_fam, ymax = mean_fam + se_fam),
                width = 0.4, alpha = 0.6,
                colour = ifelse(ORIGIN == "IL", "#E41A1C", "#377EB8")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = sprintf("Familiarity per face (%s faces rated by %s participants)", ORIGIN, ORIGIN),
       x = "Face",
       y = "Mean familiarity rating (0-100)") +
  theme_bw() +
  theme(plot.title  = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
ggsave(paste0(img_prefix, "figure_1b.png"), width = 10, height = 6, dpi = 300)

# ---- Figure 1e – Mean familiarity per participant across all face conditions ----
participant_fam_all <- full_data_all %>%
  filter(stimuli_condition %in% c("IL", "UK", "Neutral")) %>%
  group_by(participant, stimuli_condition) %>%
  summarise(mean_fam = mean(familiarity, na.rm = TRUE), .groups = "drop") %>%
  mutate(stimuli_condition = factor(stimuli_condition, levels = c("IL", "Neutral", "UK")))

ggplot(participant_fam_all, aes(x = stimuli_condition, y = mean_fam,
                                group = participant)) +
  geom_line(alpha = 0.25, linewidth = 0.4,
            colour = ifelse(ORIGIN == "IL", "#E41A1C", "#377EB8")) +
  geom_point(alpha = 0.4, size = 1.2,
             colour = ifelse(ORIGIN == "IL", "#E41A1C", "#377EB8")) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.5, colour = "black", aes(group = 1)) +
  stat_summary(fun = mean, geom = "point", size = 3, colour = "black", aes(group = 1)) +
  scale_x_discrete(labels = c("IL" = "IL faces", "Neutral" = "Neutral faces", "UK" = "UK faces")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = sprintf("Mean familiarity per participant by face condition (%s participants)", ORIGIN),
       subtitle = "Thick black line = group mean",
       x = "Face condition",
       y = "Mean familiarity rating (0-100)") +
  theme_bw() +
  theme(plot.title    = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40"))
ggsave(paste0(img_prefix, "figure_1e.png"), width = 8, height = 6, dpi = 300)

# ---- Figure 2 – Familiarity vs reproduced duration by duration ----
ggplot(full_data_all, aes(x = familiarity, y = rd, colour = stimuli_condition)) +
  facet_wrap(~duration_factor,
             labeller = labeller(duration_factor = c("short" = "800 ms", "long" = "1600 ms"))) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  scale_colour_manual(
    values = c("IL" = "#E41A1C", "UK" = "#377EB8", "Neutral" = "#4DAF4A"),
    labels = c("IL" = "IL faces", "UK" = "UK faces", "Neutral" = "Neutral faces"),
    name = "Stimulus condition") +
  labs(title = sprintf("Familiarity vs reproduced duration by duration (%s participants)", ORIGIN),
       x = "Familiarity rating (0-100)",
       y = "Reproduced duration (ms)") +
  theme_bw()
ggsave(paste0(img_prefix, "figure_2.png"), width = 10, height = 6, dpi = 300)

# ---- Figure 2b – Familiarity vs reproduced duration (all stimuli combined) ----
ggplot(full_data_all, aes(x = familiarity, y = rd)) +
  facet_wrap(~duration_factor,
             labeller = labeller(duration_factor = c("short" = "800 ms", "long" = "1600 ms"))) +
  geom_point(alpha = 0.2, size = 1, colour = "grey50") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, colour = "black") +
  labs(title = sprintf("Familiarity vs reproduced duration by duration (%s participants)", ORIGIN),
       x = "Familiarity rating (0-100)",
       y = "Reproduced duration (ms)") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave(paste0(img_prefix, "figure_2b.png"), width = 10, height = 6, dpi = 300)

# ---- Figure 3 – Familiarity vs reproduced duration by duration and stimulus condition ----
ggplot(full_data_all, aes(x = familiarity, y = rd)) +
  facet_grid(stimuli_condition ~ duration_factor,
             labeller = labeller(
               stimuli_condition = c("IL" = "IL faces", "UK" = "UK faces", "Neutral" = "Neutral faces"),
               duration_factor   = c("short" = "800 ms", "long" = "1600 ms"))) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  labs(title = sprintf("Familiarity vs reproduced duration by duration and stimulus condition (%s participants)", ORIGIN),
       x = "Familiarity rating (0-100)",
       y = "Reproduced duration (ms)") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave(paste0(img_prefix, "figure_3.png"), width = 10, height = 8, dpi = 300)

# ---- Statistical models ----

# Main effects model
model_main <- lmer(
  rd ~ familiarity + duration_factor + appeal + stimuli_condition +
    (1 | participant) + (1 | face),
  data = full_data_all)

summary(model_main)

# Interaction model
model_interaction <- lmer(
  rd ~ familiarity * duration_factor + appeal + stimuli_condition +
    (1 | participant) + (1 | face),
  data = full_data_all)

summary(model_interaction)

# Main effects model – familiar vs unfamiliar (binary, based on participant origin)
# face_familiarity: familiar = home faces, unfamiliar = all other faces
model_main_fam <- lmer(
  rd ~ familiarity + duration_factor + appeal + face_familiarity +
    (1 | participant) + (1 | face),
  data = full_data_all)

summary(model_main_fam)

# Interaction model – familiar vs unfamiliar (binary)
model_interaction_fam <- lmer(
  rd ~ familiarity * duration_factor + appeal + face_familiarity +
    (1 | participant) + (1 | face),
  data = full_data_all)

summary(model_interaction_fam)

# ---- Plots for familiar vs unfamiliar models ----

# Predictions from model_interaction_fam
pred_grid_fam <- expand.grid(
  familiarity      = seq(0, 100, length.out = 200),
  duration_factor  = c("short", "long"),
  appeal           = mean(full_data_all$appeal, na.rm = TRUE),
  face_familiarity = c("familiar", "unfamiliar"))

pred_grid_fam$pred_rd <- predict(model_interaction_fam, newdata = pred_grid_fam, re.form = NA)

pred_plot_fam <- pred_grid_fam %>%
  group_by(familiarity, duration_factor, face_familiarity) %>%
  summarise(pred_rd = mean(pred_rd), .groups = "drop")

# Observed means binned by familiarity, split by duration and face_familiarity
obs_plot_fam <- full_data_all %>%
  mutate(fam_bin = cut(familiarity,
                       breaks = seq(0, 100, by = 20),
                       include.lowest = TRUE,
                       labels = c("0-20", "21-40", "41-60", "61-80", "81-100")),
         fam_mid = as.numeric(fam_bin) * 20 - 10) %>%
  group_by(duration_factor, face_familiarity, fam_mid) %>%
  summarise(
    mean_rd = mean(rd, na.rm = TRUE),
    se_rd   = sd(rd, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")

# Figure 4b – familiar vs unfamiliar, faceted by face_familiarity
ggplot() +
  geom_point(data = obs_plot_fam,
             aes(x = fam_mid, y = mean_rd, colour = duration_factor),
             size = 2.8, alpha = 0.9) +
  geom_errorbar(data = obs_plot_fam,
                aes(x = fam_mid, ymin = mean_rd - se_rd, ymax = mean_rd + se_rd,
                    colour = duration_factor),
                width = 1.5, alpha = 0.5) +
  geom_line(data = pred_plot_fam,
            aes(x = familiarity, y = pred_rd, colour = duration_factor),
            linewidth = 1.3) +
  facet_wrap(~ face_familiarity,
             labeller = labeller(face_familiarity =
               c("familiar" = sprintf("Familiar (%s faces)", ORIGIN),
                 "unfamiliar" = "Unfamiliar (other faces)"))) +
  scale_colour_manual(
    values = c("short" = "#F97316", "long" = "#0D9488"),
    labels = c("short" = "800 ms", "long" = "1600 ms"),
    name = "True duration") +
  labs(title = sprintf("Familiarity × Duration – Familiar vs Unfamiliar faces (%s participants)", ORIGIN),
       x = "Familiarity rating (0-100)",
       y = "Reproduced duration (ms)") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave(paste0(img_prefix, "figure_4b_fam.png"), width = 11, height = 6, dpi = 300)

# Figure 4c – familiar vs unfamiliar on same axes, duration as linetype
ggplot() +
  geom_point(data = obs_plot_fam,
             aes(x = fam_mid, y = mean_rd,
                 colour = face_familiarity, shape = duration_factor),
             size = 2.8, alpha = 0.9) +
  geom_errorbar(data = obs_plot_fam,
                aes(x = fam_mid, ymin = mean_rd - se_rd, ymax = mean_rd + se_rd,
                    colour = face_familiarity),
                width = 1.5, alpha = 0.4) +
  geom_line(data = pred_plot_fam,
            aes(x = familiarity, y = pred_rd,
                colour = face_familiarity, linetype = duration_factor),
            linewidth = 1.2) +
  scale_colour_manual(
    values = c("familiar" = "#9B2226", "unfamiliar" = "#005F73"),
    labels = c("familiar"   = sprintf("Familiar (%s faces)", ORIGIN),
               "unfamiliar" = "Unfamiliar (other faces)"),
    name = "Face type") +
  scale_linetype_manual(
    values = c("short" = "dashed", "long" = "solid"),
    labels = c("short" = "800 ms", "long" = "1600 ms"),
    name = "True duration") +
  scale_shape_manual(
    values = c("short" = 16, "long" = 17),
    labels = c("short" = "800 ms", "long" = "1600 ms"),
    name = "True duration") +
  labs(title = sprintf("Familiarity × Duration – Familiar vs Unfamiliar faces (%s participants)", ORIGIN),
       x = "Familiarity rating (0-100)",
       y = "Reproduced duration (ms)") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave(paste0(img_prefix, "figure_4c_fam.png"), width = 9, height = 6, dpi = 300)

# ---- Model predictions for Figure 4 ----
full_data_all$duration_factor <- factor(full_data_all$duration_factor, levels = c("short", "long"))

pred_grid <- expand.grid(
  familiarity       = seq(0, 100, length.out = 200),
  duration_factor   = c("short", "long"),
  appeal            = mean(full_data_all$appeal, na.rm = TRUE),
  stimuli_condition = unique(full_data_all$stimuli_condition))

pred_grid$pred_rd <- predict(model_interaction, newdata = pred_grid, re.form = NA)

pred_plot <- pred_grid %>%
  group_by(familiarity, duration_factor) %>%
  summarise(pred_rd = mean(pred_rd), .groups = "drop")

obs_plot <- full_data_all %>%
  mutate(fam_bin = cut(familiarity,
                       breaks = seq(0, 100, by = 20),
                       include.lowest = TRUE,
                       labels = c("0-20", "21-40", "41-60", "61-80", "81-100")),
         fam_mid = as.numeric(fam_bin) * 20 - 10) %>%
  group_by(duration_factor, fam_mid) %>%
  summarise(
    mean_rd = mean(rd, na.rm = TRUE),
    se_rd   = sd(rd, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")

# ---- Figure 4 – Interaction between familiarity and duration ----
ggplot() +
  geom_point(data = obs_plot,
             aes(x = fam_mid, y = mean_rd, colour = duration_factor),
             size = 2.8, alpha = 0.9) +
  geom_errorbar(data = obs_plot,
                aes(x = fam_mid, ymin = mean_rd - se_rd, ymax = mean_rd + se_rd,
                    colour = duration_factor),
                width = 1.5, alpha = 0.5) +
  geom_line(data = pred_plot,
            aes(x = familiarity, y = pred_rd, colour = duration_factor),
            linewidth = 1.3) +
  labs(title = sprintf("Familiarity × Duration interaction (%s participants)", ORIGIN),
       x = "Familiarity rating (0-100)",
       y = "Reproduced duration (ms)",
       colour = "True duration") +
  scale_color_discrete(labels = c("800 ms", "1600 ms")) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave(paste0(img_prefix, "figure_4.png"), width = 8, height = 6, dpi = 300)

# ---- Precision model (participant-level CV) ----
precision_data <- full_data_all %>%
  group_by(participant, duration_factor) %>%
  summarise(
    cv_rd            = sd(rd, na.rm = TRUE) / mean(rd, na.rm = TRUE),
    mean_familiarity = mean(familiarity, na.rm = TRUE),
    .groups          = "drop")

model_precision <- lmer(
  cv_rd ~ mean_familiarity * duration_factor +
    (1 | participant),
  data = precision_data)

summary(model_precision)

# ---- Figure CV model ----
pred_cv <- expand.grid(
  mean_familiarity = seq(0, 100, length.out = 200),
  duration_factor  = c("short", "long"))
pred_cv$pred_cv <- predict(model_precision, newdata = pred_cv, re.form = NA)

ggplot() +
  geom_point(data = precision_data,
             aes(x = mean_familiarity, y = cv_rd, colour = duration_factor),
             alpha = 0.4, size = 1.5) +
  geom_line(data = pred_cv,
            aes(x = mean_familiarity, y = pred_cv,
                colour = duration_factor, linetype = duration_factor),
            linewidth = 1.2) +
  scale_colour_manual(
    values = c("short" = "#F97316", "long" = "#0D9488"),
    labels = c("short" = "800 ms", "long" = "1600 ms"),
    name = "True duration") +
  scale_linetype_manual(
    values = c("short" = "dashed", "long" = "solid"),
    labels = c("short" = "800 ms", "long" = "1600 ms"),
    name = "True duration") +
  labs(title = sprintf("Temporal precision (CV) by familiarity and duration (%s participants)", ORIGIN),
       subtitle = "Points = participant CV | Lines = model predictions",
       x = "Mean familiarity rating (0-100)",
       y = "Coefficient of variation (CV = SD/mean)") +
  theme_bw() +
  theme(plot.title    = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40"))
ggsave(paste0(img_prefix, "figure_cv_model.png"), width = 10, height = 6, dpi = 300)

# ---- Figure precision_cv – within-participant variability by familiarity bin ----
precision_binned <- full_data_all %>%
  mutate(fam_bin = cut(familiarity,
                       breaks = seq(0, 100, by = 20),
                       include.lowest = TRUE,
                       labels = c("0-20", "21-40", "41-60", "61-80", "81-100"))) %>%
  group_by(duration_factor, fam_bin) %>%
  summarise(
    sd_resid = sd(rd_resid, na.rm = TRUE),
    n        = n(),
    se       = sd_resid / sqrt(n),
    .groups  = "drop")

ggplot(precision_binned, aes(x = fam_bin, y = sd_resid,
                              colour = duration_factor, group = duration_factor)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = sd_resid - se, ymax = sd_resid + se),
                width = 0.2, alpha = 0.6) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(
    values = c("short" = "#F97316", "long" = "#0D9488"),
    labels = c("short" = "800 ms", "long" = "1600 ms"),
    name = "True duration") +
  scale_y_continuous(limits = c(250, 500)) +
  labs(title = sprintf("Within-participant variability by familiarity level (%s participants)", ORIGIN),
       subtitle = "SD of residuals (rd minus participant mean), controlling for individual bias",
       x = "Familiarity bin (0-100)",
       y = "SD of residuals (ms)") +
  theme_bw() +
  theme(plot.title    = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40"))
ggsave(paste0(img_prefix, "figure_precision_cv.png"), width = 9, height = 6, dpi = 300)

# ---- Figure precision_cv2 – same but CV-normalised ----
cv_binned <- full_data_all %>%
  mutate(fam_bin = cut(familiarity,
                       breaks = seq(0, 100, by = 20),
                       include.lowest = TRUE,
                       labels = c("0-20", "21-40", "41-60", "61-80", "81-100"))) %>%
  group_by(duration_factor, fam_bin) %>%
  summarise(
    cv_resid = sd(rd_resid, na.rm = TRUE) / mean(participant_mean, na.rm = TRUE),
    n        = n(),
    se       = sd(rd_resid, na.rm = TRUE) / mean(participant_mean, na.rm = TRUE) / sqrt(n),
    .groups  = "drop")

ggplot(cv_binned, aes(x = fam_bin, y = cv_resid,
                      colour = duration_factor, group = duration_factor)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cv_resid - se, ymax = cv_resid + se),
                width = 0.2, alpha = 0.6) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(
    values = c("short" = "#F97316", "long" = "#0D9488"),
    labels = c("short" = "800 ms", "long" = "1600 ms"),
    name = "True duration") +
  labs(title = sprintf("Within-participant CV by familiarity level (%s participants)", ORIGIN),
       subtitle = "CV = SD of residuals / participant mean (normalised for duration)",
       x = "Familiarity bin (0-100)",
       y = "CV of residuals") +
  theme_bw() +
  theme(plot.title    = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40"))
ggsave(paste0(img_prefix, "figure_precision_cv2.png"), width = 9, height = 6, dpi = 300)

# ---- Statistical model: |residual| ~ familiarity × duration ----
model_precision_resid <- lmer(
  abs_resid ~ familiarity * duration_factor + (1 | participant),
  data = full_data_all)

summary(model_precision_resid)

# ---- Statistical model: CV of residuals ~ familiarity × duration ----
model_cv_resid <- lmer(
  cv_resid_trial ~ familiarity * duration_factor + (1 | participant),
  data = full_data_all)

summary(model_cv_resid)

# ---- High vs Low familiarity: |residual| model ----
full_data_hilo <- full_data_all %>%
  filter(familiarity <= 33 | familiarity >= 67) %>%
  mutate(fam_group = factor(
    ifelse(familiarity <= 33, "low", "high"),
    levels = c("low", "high")))

model_precision_hilo <- lmer(
  abs_resid ~ fam_group * duration_factor + (1 | participant),
  data = full_data_hilo)

summary(model_precision_hilo)

# ---- Levene test: variance of residuals – High vs Low familiarity ----
leveneTest(rd_resid ~ fam_group, data = full_data_hilo)

# ---- Accuracy model ----
model_accuracy <- lmer(
  abs_error ~ familiarity * duration_factor + (1 | participant),
  data = full_data_all)

summary(model_accuracy)

# ---- Figure accuracy – absolute error by familiarity bin ----
accuracy_binned <- full_data_all %>%
  mutate(fam_bin = cut(familiarity,
                       breaks = seq(0, 100, by = 20),
                       include.lowest = TRUE,
                       labels = c("0-20", "21-40", "41-60", "61-80", "81-100"))) %>%
  group_by(duration_factor, fam_bin) %>%
  summarise(
    mean_abs_error = mean(abs_error, na.rm = TRUE),
    se             = sd(abs_error, na.rm = TRUE) / sqrt(n()),
    .groups        = "drop")

ggplot(accuracy_binned, aes(x = fam_bin, y = mean_abs_error,
                             colour = duration_factor, group = duration_factor)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_abs_error - se, ymax = mean_abs_error + se),
                width = 0.2, alpha = 0.6) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(
    values = c("short" = "#F97316", "long" = "#0D9488"),
    labels = c("short" = "800 ms", "long" = "1600 ms"),
    name = "True duration") +
  scale_y_continuous(limits = c(250, 500)) +
  labs(title = sprintf("Accuracy by familiarity level and duration (%s participants)", ORIGIN),
       subtitle = "Mean absolute error (|reproduced - true duration|)",
       x = "Familiarity bin (0-100)",
       y = "Mean absolute error (ms)") +
  theme_bw() +
  theme(plot.title    = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40"))
ggsave(paste0(img_prefix, "figure_accuracy.png"), width = 9, height = 6, dpi = 300)

# ---- Spaghetti plot ----
participant_means <- full_data_all %>%
  group_by(participant, duration_factor) %>%
  summarise(mean_rd = mean(rd, na.rm = TRUE), .groups = "drop")

grand_means <- participant_means %>%
  group_by(duration_factor) %>%
  summarise(mean_rd = mean(mean_rd, na.rm = TRUE), .groups = "drop")

participant_wide2 <- participant_means %>%
  pivot_wider(id_cols = participant, names_from = duration_factor, values_from = mean_rd)

participant_wide2$pattern <- ifelse(participant_wide2$short < participant_wide2$long,
                                    "normal", "reversed")

n_normal   <- sum(participant_wide2$pattern == "normal")
n_reversed <- sum(participant_wide2$pattern == "reversed")
n_total    <- nrow(participant_wide2)

cat(sprintf("\n========== PARTICIPANT SUMMARY (%s) ==========\n", ORIGIN))
cat(sprintf("Total participants: %d\n", n_total))
cat(sprintf("Normal pattern  (800 < 1600): %d  (%.1f%%)\n", n_normal,  100 * n_normal  / n_total))
cat(sprintf("Reversed pattern (800 >= 1600): %d  (%.1f%%)\n", n_reversed, 100 * n_reversed / n_total))
if (n_reversed > 0) {
  cat("\nReversed participants:\n")
  reversed_subs <- participant_wide2 %>% filter(pattern == "reversed")
  for (i in seq_len(nrow(reversed_subs))) {
    cat(sprintf("  %s — mean 800ms: %.1f, mean 1600ms: %.1f\n",
                reversed_subs$participant[i],
                reversed_subs$short[i],
                reversed_subs$long[i]))
  }
}
cat("==============================================\n\n")

participant_means <- participant_means %>%
  left_join(participant_wide2[, c("participant", "pattern")], by = "participant")

ggplot(participant_means, aes(x = duration_factor, y = mean_rd)) +
  geom_line(aes(group = participant, colour = pattern),
            alpha = 0.3, linewidth = 0.5) +
  geom_point(aes(group = participant, colour = pattern),
             alpha = 0.4, size = 1.5) +
  scale_colour_manual(values  = c("normal" = "steelblue", "reversed" = "orange"),
                      labels  = c("normal" = "800 < 1600 (expected)", "reversed" = "800 >= 1600 (reversed)"),
                      name    = "Pattern") +
  geom_line(data = grand_means, aes(group = 1),
            linewidth = 1.8, colour = "firebrick") +
  geom_point(data = grand_means, size = 4, colour = "firebrick") +
  annotate("point", x = c("short", "long"), y = c(800, 1600),
           shape = 4, size = 5, colour = "grey40", stroke = 1.5) +
  scale_x_discrete(labels = c("short" = "800 ms", "long" = "1600 ms")) +
  labs(title = sprintf("Reproduced duration per participant (%s)", ORIGIN),
       subtitle = "Red line = grand mean | × = true duration",
       x = "True duration",
       y = "Mean reproduced duration (ms)") +
  theme_bw() +
  theme(plot.title    = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40"))
ggsave(paste0(img_prefix, "figure_spaghetti.png"), width = 8, height = 6, dpi = 300)

# ---- Save model summaries ----
sink(results_file)
cat(sprintf("========== Analysis for %s participants only ==========\n\n", ORIGIN))

cat("========== Main Effects Model ==========\n")
print(summary(model_main))

cat("\n\n========== Interaction Model ==========\n")
print(summary(model_interaction))

cat("\n\n========== Main Effects Model – Familiar vs Unfamiliar faces (binary) ==========\n")
print(summary(model_main_fam))

cat("\n\n========== Interaction Model – Familiar vs Unfamiliar faces (binary) ==========\n")
print(summary(model_interaction_fam))

cat("\n\n========== Precision Model (cv_rd ~ familiarity × duration) ==========\n")
print(summary(model_precision))

cat("\n\n========== Within-Participant Variability Model (|residual| ~ familiarity × duration) ==========\n")
print(summary(model_precision_resid))

cat("\n\n========== CV of Residuals Model (|residual|/participant_mean ~ familiarity × duration) ==========\n")
print(summary(model_cv_resid))

cat("\n\n========== Accuracy Model (|rd - true duration| ~ familiarity × duration) ==========\n")
print(summary(model_accuracy))

cat("\n\n========== Within-Participant Variability: High vs Low Familiarity Only ==========\n")
print(summary(model_precision_hilo))

cat("\n\n========== Levene Test: Variance of residuals – High vs Low familiarity ==========\n")
print(leveneTest(rd_resid ~ fam_group, data = full_data_hilo))

sink()

cat(sprintf("\nDone! Results saved to '%s'\nImages saved with prefix '%s'\n", results_file, img_prefix))
