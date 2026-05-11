library(lme4)
library(lmerTest)
library(ggplot2)
library(tidyr)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data
rd_il <- bind_rows(
  read.csv("il_data_10-5/rd_il.csv"),
  read.csv("il_data_10-5/rd_il_cash.csv")
)

ratings_il <- bind_rows(
  read.csv("il_data_10-5/ratings_il.csv"),
  read.csv("il_data_10-5/ratings_il_cash.csv")
)

rd_uk = read.csv('UK2_rd.csv')
ratings_uk = read.csv('UK2_ratings.csv')
ratings_il$Participant.Public.ID <- as.character(ratings_il$Participant.Public.ID)
rd_il$Participant.Public.ID <- as.character(rd_il$Participant.Public.ID)

# add participant origin column
rd_il$participant_origin <- "IL"
rd_uk$participant_origin <- "UK"
ratings_il$participant_origin <- "IL"
ratings_uk$participant_origin <- "UK"

ratings_il <- bind_rows(ratings_il, ratings_uk)
rd_il = bind_rows(rd_il, rd_uk)

# keep completed reproduction trials only
rd_clean_il <- subset(rd_il,Display == "reproductionTask" & 
                        Participant.Status == "complete")

# remove very short and very long reaction times
rd_cleaner_il <- subset(rd_clean_il, 
                        # תנאי בסיסי: זמן תגובה בטווח הגיוני כללי
                        Store..RT > 100 & Store..RT < 4000 #& 
                        # החרגה ספציפית לתנאי ה-1600:
                        # אנחנו שומרים רק את מי שלא (גם בתנאי 1600 וגם מחוץ לטווח 1000-2000)
                        #!(Spreadsheet..stimulus_duration == 1600 & (Store..RT < 1000 | Store..RT > 2000))
)
# keep valid rating responses
ratings_clean_il <- subset(ratings_il,
                           Response.Type == "response" & !is.na(Response))

# keep only the relevant columns for reproduction data
rd_analysis <- rd_cleaner_il[, c(
  "Participant.Public.ID",
  "Spreadsheet..Image",
  "Spreadsheet..stimulus_duration",
  "Store..RT",
  "participant_origin")]

# remove duplicate rows 
rd_analysis <- rd_analysis[!duplicated(rd_analysis), ]

# keep only relevant columns for ratings data
ratings_analysis <- ratings_clean_il[ratings_clean_il$Display == "ratings", c(
  "Event.Index",
  "Participant.Public.ID",
  "Object.Name",
  "Spreadsheet..Image",
  "Response")]

# label each rating as familiarity or appeal
ratings_analysis$rating_type <- factor(ratings_analysis$Object.Name,
                                       levels = c("Slider", "Slider_att"),
                                       labels = c("familiarity", "appeal"))

# convert ratings to wide format
ratings_wide <- pivot_wider(ratings_analysis,
                            id_cols = c("Participant.Public.ID", "Spreadsheet..Image"),
                            names_from = rating_type,values_from = Response)

# merge the datasets
full_data_all <- merge(rd_analysis,ratings_wide,
                       by = c("Participant.Public.ID", "Spreadsheet..Image"))


# rename variables 
# rd = reproduced duration
names(full_data_all) <- c(
  "participant",
  "face",
  "duration",
  "rd",
  "participant_origin",
  "familiarity",
  "appeal")

# convert to numeric values.
full_data_all$duration <- as.numeric(full_data_all$duration)
full_data_all$rd <- as.numeric(full_data_all$rd)
full_data_all$familiarity <- as.numeric(full_data_all$familiarity)
full_data_all$appeal <- as.numeric(full_data_all$appeal)

# create duration categories (short vs long)
full_data_all$duration_factor <- factor(full_data_all$duration,
                                        levels = c(800, 1600),
                                        labels = c("short", "long"))

# calculate reproduction error
full_data_all$error <- full_data_all$rd - full_data_all$duration

# calaculate participant means and flag
df_avg <- full_data_all %>%
  group_by(participant, duration_factor) %>%
  summarise(mean_rd = mean(rd), .groups = "drop")

df_avg_wide <- pivot_wider(df_avg,id_cols = "participant",
                           names_from = duration_factor,values_from = mean_rd)

df_avg_wide$bad_sub <- ifelse(df_avg_wide$short > df_avg_wide$long, TRUE, FALSE)
bad_subs <- df_avg_wide$participant[df_avg_wide$bad_sub == TRUE]
print(bad_subs)

# remove participants whose mean reproduced duration was higher for short than long trials
full_data_all <- full_data_all[!full_data_all$participant %in% bad_subs, ]

# How many participants after removal
length(unique(full_data_all$participant))

# classify faces
full_data_all$stimuli_condition <- ifelse(substr(full_data_all$face, 1, 1) == "I", "IL",
                                          ifelse(substr(full_data_all$face, 1, 1) == "U", "UK",
                                                 ifelse(substr(full_data_all$face, 1, 1) == "N", "Neutral", NA)))

# Figure 1 – Familiarity distribution by stimulus condition and participant origin
ggplot(full_data_all, aes(x = stimuli_condition, y = familiarity, fill = participant_origin)) +
  geom_violin(trim = FALSE, alpha = 0.6, position = position_dodge(width = 0.8)) +
  geom_boxplot(width = 0.15, alpha = 0.8, outlier.size = 0.5,
               position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = c("IL" = "#E41A1C", "UK" = "#377EB8"),
    labels = c("IL" = "Israeli participants", "UK" = "UK participants"),
    name = "Participant origin") +
  scale_x_discrete(labels = c("IL" = "IL faces", "UK" = "UK faces", "Neutral" = "Neutral faces")) +
  labs(title = "Familiarity by stimulus condition and participant origin",
       x = "Stimulus condition",
       y = "Familiarity rating (0-100)") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("images/figure_1.png", width = 8, height = 6, dpi = 300)

# Figure 1b – Familiarity per face: IL faces rated by IL participants, UK faces rated by UK participants
home_familiarity <- full_data_all %>%
  filter((stimuli_condition == "IL" & participant_origin == "IL") |
           (stimuli_condition == "UK" & participant_origin == "UK")) %>%
  group_by(face, stimuli_condition) %>%
  summarise(
    mean_fam = mean(familiarity, na.rm = TRUE),
    se_fam   = sd(familiarity, na.rm = TRUE) / sqrt(n()),
    .groups = "drop") %>%
  arrange(stimuli_condition, mean_fam) %>%
  mutate(face = factor(face, levels = face))

ggplot(home_familiarity, aes(x = face, y = mean_fam, colour = stimuli_condition)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_fam - se_fam, ymax = mean_fam + se_fam),
                width = 0.4, alpha = 0.6) +
  scale_colour_manual(
    values = c("IL" = "#E41A1C", "UK" = "#377EB8"),
    labels = c("IL" = "IL faces (rated by IL)", "UK" = "UK faces (rated by UK)"),
    name = "Stimulus condition") +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~stimuli_condition, scales = "free_x",
             labeller = labeller(stimuli_condition = c("IL" = "IL faces", "UK" = "UK faces"))) +
  labs(title = "Familiarity per face (rated by home participants)",
       x = "Face",
       y = "Mean familiarity rating (0-100)") +
  theme_bw() +
  theme(plot.title  = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
ggsave("images/figure_1b.png", width = 10, height = 6, dpi = 300)

# Figure 1c – Familiarity per face: IL faces rated by UK participants, UK faces rated by IL participants
away_familiarity <- full_data_all %>%
  filter((stimuli_condition == "IL" & participant_origin == "UK") |
           (stimuli_condition == "UK" & participant_origin == "IL")) %>%
  group_by(face, stimuli_condition) %>%
  summarise(
    mean_fam = mean(familiarity, na.rm = TRUE),
    se_fam   = sd(familiarity, na.rm = TRUE) / sqrt(n()),
    .groups = "drop") %>%
  arrange(stimuli_condition, mean_fam) %>%
  mutate(face = factor(face, levels = face))

ggplot(away_familiarity, aes(x = face, y = mean_fam, colour = stimuli_condition)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_fam - se_fam, ymax = mean_fam + se_fam),
                width = 0.4, alpha = 0.6) +
  scale_colour_manual(
    values = c("IL" = "#E41A1C", "UK" = "#377EB8"),
    labels = c("IL" = "IL faces (rated by UK)", "UK" = "UK faces (rated by IL)"),
    name = "Stimulus condition") +
  facet_wrap(~stimuli_condition, scales = "free_x",
             labeller = labeller(stimuli_condition = c("IL" = "IL faces (rated by UK)", "UK" = "UK faces (rated by IL)"))) +
  labs(title = "Familiarity per face (rated by away participants)",
       x = "Face",
       y = "Mean familiarity rating (0-100)") +
  theme_bw() +
  theme(plot.title  = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
ggsave("images/figure_1c.png", width = 10, height = 6, dpi = 300)

# helper function for participant familiarity plots
plot_participant_fam <- function(data, face_cond, part_orig, title) {
  df <- data %>%
    filter(stimuli_condition == face_cond, participant_origin == part_orig) %>%
    group_by(participant) %>%
    summarise(
      mean_fam = mean(familiarity, na.rm = TRUE),
      se_fam   = sd(familiarity, na.rm = TRUE) / sqrt(n()),
      .groups = "drop") %>%
    arrange(mean_fam) %>%
    mutate(participant = factor(participant, levels = participant))
  
  colour <- ifelse(part_orig == "IL", "#E41A1C", "#377EB8")
  
  ggplot(df, aes(x = participant, y = mean_fam)) +
    geom_point(size = 2, colour = colour) +
    geom_errorbar(aes(ymin = mean_fam - se_fam, ymax = mean_fam + se_fam),
                  width = 0.4, alpha = 0.6, colour = colour) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(title = title,
         x = "Participant",
         y = "Mean familiarity rating (0-100)") +
    theme_bw() +
    theme(plot.title  = element_text(size = 12, hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
}

# Figure 1d – all four combinations in one figure
library(gridExtra)

p1 <- plot_participant_fam(full_data_all, "IL", "IL", "IL participants rating IL faces")
p2 <- plot_participant_fam(full_data_all, "UK", "UK", "UK participants rating UK faces")
p3 <- plot_participant_fam(full_data_all, "UK", "IL", "IL participants rating UK faces")
p4 <- plot_participant_fam(full_data_all, "IL", "UK", "UK participants rating IL faces")

fig_1d <- grid.arrange(p1, p2, p3, p4, ncol = 2)
ggsave("images/figure_1d.png", plot = fig_1d, width = 14, height = 10, dpi = 300)

# Figure 1e – Mean familiarity per participant across all three face conditions
participant_fam_all <- full_data_all %>%
  filter(stimuli_condition %in% c("IL", "UK", "Neutral")) %>%
  group_by(participant, participant_origin, stimuli_condition) %>%
  summarise(mean_fam = mean(familiarity, na.rm = TRUE), .groups = "drop") %>%
  mutate(stimuli_condition = factor(stimuli_condition, levels = c("IL", "Neutral", "UK")))

ggplot(participant_fam_all, aes(x = stimuli_condition, y = mean_fam,
                                group = participant, colour = participant_origin)) +
  geom_line(alpha = 0.25, linewidth = 0.4) +
  geom_point(alpha = 0.4, size = 1.2) +
  stat_summary(aes(group = participant_origin),
               fun = mean, geom = "line", linewidth = 1.5) +
  stat_summary(aes(group = participant_origin),
               fun = mean, geom = "point", size = 3) +
  scale_colour_manual(
    values = c("IL" = "#E41A1C", "UK" = "#377EB8"),
    labels = c("IL" = "Israeli participants", "UK" = "UK participants"),
    name = "Participant origin") +
  scale_x_discrete(labels = c("IL" = "IL faces", "Neutral" = "Neutral faces", "UK" = "UK faces")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Mean familiarity per participant by face condition",
       subtitle = "Thick lines = group mean",
       x = "Face condition",
       y = "Mean familiarity rating (0-100)") +
  theme_bw() +
  theme(plot.title    = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40"))
ggsave("images/figure_1e.png", width = 8, height = 6, dpi = 300)

# Figure 1f – Mean familiarity per participant: IL vs UK faces only
participant_fam_no_neutral <- full_data_all %>%
  filter(stimuli_condition %in% c("IL", "UK")) %>%
  group_by(participant, participant_origin, stimuli_condition) %>%
  summarise(mean_fam = mean(familiarity, na.rm = TRUE), .groups = "drop") %>%
  mutate(stimuli_condition = factor(stimuli_condition, levels = c("IL", "UK")))

ggplot(participant_fam_no_neutral, aes(x = stimuli_condition, y = mean_fam,
                                       group = participant, colour = participant_origin)) +
  geom_line(alpha = 0.25, linewidth = 0.4) +
  geom_point(alpha = 0.4, size = 1.2) +
  stat_summary(aes(group = participant_origin),
               fun = mean, geom = "line", linewidth = 1.5) +
  stat_summary(aes(group = participant_origin),
               fun = mean, geom = "point", size = 3) +
  scale_colour_manual(
    values = c("IL" = "#E41A1C", "UK" = "#377EB8"),
    labels = c("IL" = "Israeli participants", "UK" = "UK participants"),
    name = "Participant origin") +
  scale_x_discrete(labels = c("IL" = "IL faces", "UK" = "UK faces")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Mean familiarity per participant: IL vs UK faces",
       subtitle = "Thick lines = group mean",
       x = "Face condition",
       y = "Mean familiarity rating (0-100)") +
  theme_bw() +
  theme(plot.title    = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40"))
ggsave("images/figure_1f.png", width = 8, height = 6, dpi = 300)

# Figure 1fa – Mean appeal per participant: IL vs UK faces only
participant_ap_no_neutral <- full_data_all %>%
  filter(stimuli_condition %in% c("IL", "UK")) %>%
  group_by(participant, participant_origin, stimuli_condition) %>%
  summarise(mean_ap = mean(appeal, na.rm = TRUE), .groups = "drop") %>%
  mutate(stimuli_condition = factor(stimuli_condition, levels = c("IL", "UK")))

ggplot(participant_ap_no_neutral, aes(x = stimuli_condition, y = mean_ap,
                                      group = participant, colour = participant_origin)) +
  geom_line(alpha = 0.25, linewidth = 0.4) +
  geom_point(alpha = 0.4, size = 1.2) +
  stat_summary(aes(group = participant_origin),
               fun = mean, geom = "line", linewidth = 1.5) +
  stat_summary(aes(group = participant_origin),
               fun = mean, geom = "point", size = 3) +
  scale_colour_manual(
    values = c("IL" = "#E41A1C", "UK" = "#377EB8"),
    labels = c("IL" = "Israeli participants", "UK" = "UK participants"),
    name = "Participant origin") +
  scale_x_discrete(labels = c("IL" = "IL faces", "UK" = "UK faces")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Mean appeal per participant: IL vs UK faces",
       subtitle = "Thick lines = group mean",
       x = "Face condition",
       y = "Mean appeal rating (0-100)") +
  theme_bw() +
  theme(plot.title    = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40"))
ggsave("images/figure_1fa.png", width = 8, height = 6, dpi = 300)

# Figure 1g – Home minus away familiarity per participant
participant_fam_diff <- full_data_all %>%
  filter(stimuli_condition %in% c("IL", "UK")) %>%
  group_by(participant, participant_origin, stimuli_condition) %>%
  summarise(mean_fam = mean(familiarity, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = stimuli_condition, values_from = mean_fam) %>%
  mutate(diff = ifelse(participant_origin == "IL", IL - UK, UK - IL)) %>%
  arrange(participant_origin, diff) %>%
  group_by(participant_origin) %>%
  mutate(participant = factor(participant, levels = participant)) %>%
  ungroup()

ggplot(participant_fam_diff, aes(x = participant, y = diff, colour = participant_origin)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  geom_segment(aes(xend = participant, y = 0, yend = diff), alpha = 0.4) +
  scale_colour_manual(
    values = c("IL" = "#E41A1C", "UK" = "#377EB8"),
    labels = c("IL" = "Israeli participants (IL−UK)", "UK" = "UK participants (UK−IL)"),
    name = "Participant origin") +
  facet_wrap(~participant_origin, scales = "free_x",
             labeller = labeller(participant_origin = c("IL" = "Israeli participants (IL−UK)", "UK" = "UK participants (UK−IL)"))) +
  labs(title = "Home minus away familiarity per participant",
       subtitle = "Positive = rated home faces higher | Negative = rated away faces higher",
       x = "Participant",
       y = "Mean familiarity (home − away)") +
  theme_bw() +
  theme(plot.title    = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40"),
        axis.text.x   = element_text(angle = 90, hjust = 1, size = 6))
ggsave("images/figure_1g.png", width = 10, height = 6, dpi = 300)

# Figure 2 – Familiarity vs reproduced duration by duration
ggplot(full_data_all, aes(x = familiarity, y = rd, colour = stimuli_condition)) +
  facet_wrap(~duration_factor,
             labeller = labeller(duration_factor =
                                   c("short" = "800 ms", "long" = "1600 ms"))) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  scale_colour_manual(
    values = c("IL" = "#E41A1C", "UK" = "#377EB8", "Neutral" = "#4DAF4A"),
    labels = c("IL" = "IL faces", "UK" = "UK faces", "Neutral" = "Neutral faces"),
    name = "Stimulus condition") +
  labs( title = "Familiarity vs reproduced duration by duration",
        x = "Familiarity rating (0-100)",
        y = "Reproduced duration (ms)") +
  theme_bw()
ggsave("images/figure_2.png", width = 10, height = 6, dpi = 300)

# Figure 2b – Familiarity vs reproduced duration: short vs long (all stimuli combined)
ggplot(full_data_all, aes(x = familiarity, y = rd)) +
  facet_wrap(~duration_factor,
             labeller = labeller(duration_factor =
                                   c("short" = "800 ms", "long" = "1600 ms"))) +
  geom_point(alpha = 0.2, size = 1, colour = "grey50") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, colour = "black") +
  labs(title = "Familiarity vs reproduced duration by duration",
       x = "Familiarity rating (0-100)",
       y = "Reproduced duration (ms)") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("images/figure_2b.png", width = 10, height = 6, dpi = 300)

# Figure 3 – Familiarity vs reproduced duration by duration and stimulus condition
ggplot(full_data_all, aes(x = familiarity, y = rd)) +
  facet_grid(stimuli_condition ~ duration_factor,
             labeller = labeller(
               stimuli_condition = c("IL" = "IL faces", "UK" = "UK faces", "Neutral" = "Neutral faces"),
               duration_factor = c("short" = "800 ms", "long" = "1600 ms") )) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  labs(title = "Familiarity vs reproduced duration by duration and stimulus condition",
       x = "Familiarity rating (0-100)",
       y = "Reproduced duration (ms)") +
  theme_bw() + theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("images/figure_3.png", width = 10, height = 8, dpi = 300)

#full_data_all$face <- as.factor(full_data_all$face) 
# main effects model
model_il_rd_main <- lmer(
  rd ~ familiarity + duration_factor + appeal + stimuli_condition +
    (1 | participant ) + (1 | face),data = full_data_all)

summary(model_il_rd_main)

# interaction model
model_il_rd <- lmer(
  rd ~ familiarity * duration_factor + appeal + stimuli_condition +
    (1 | participant) + (1 | face),data = full_data_all)

summary(model_il_rd)

# three-way interaction model: familiarity × duration × stimuli_condition
model_il_rd_3way <- lmer(
  rd ~ familiarity * duration_factor * stimuli_condition + appeal +
    (1 | participant) + (1 | face), data = full_data_all)

summary(model_il_rd_3way)

# precision model – cv_rd (coefficient of variation) as a function of familiarity, duration, and participant origin
precision_data <- full_data_all %>%
  group_by(participant, participant_origin, duration_factor) %>%
  summarise(
    cv_rd          = sd(rd, na.rm = TRUE) / mean(rd, na.rm = TRUE),
    mean_familiarity = mean(familiarity, na.rm = TRUE),
    .groups = "drop")

model_precision <- lmer(
  cv_rd ~ mean_familiarity * duration_factor * participant_origin +
    (1 | participant),
  data = precision_data)

summary(model_precision)

# save model summaries to TXT
sink("results-all.txt")
cat("========== Main Effects Model ==========\n")
print(summary(model_il_rd_main))
cat("\n\n========== Interaction Model ==========\n")
print(summary(model_il_rd))
cat("\n\n========== Three-Way Interaction Model (familiarity × duration × stimuli_condition) ==========\n")
print(summary(model_il_rd_3way))
cat("\n\n========== Precision Model (cv_rd ~ familiarity × duration × participant_origin) ==========\n")
print(summary(model_precision))
sink()

full_data_all$duration_factor <- factor(full_data_all$duration_factor, levels = c("short", "long"))

# get model predictions
pred_grid <- expand.grid(
  familiarity = seq(0, 100, length.out = 200),
  duration_factor = c("short", "long"),
  appeal = mean(full_data_all$appeal, na.rm = TRUE),
  stimuli_condition = c("IL", "UK", "Neutral"))

pred_grid$pred_rd <- predict(model_il_rd, newdata = pred_grid, re.form = NA)

pred_plot <- pred_grid %>%
  group_by(familiarity, duration_factor) %>%
  summarise(pred_rd = mean(pred_rd), .groups = "drop")

# grouping familiarity into bins
obs_plot <- full_data_all %>% mutate(fam_bin = cut(familiarity,
                                                   breaks = seq(0, 100, by = 10),include.lowest = TRUE)) %>%
  group_by(duration_factor, fam_bin) %>%summarise(
    fam_mid = mean(familiarity, na.rm = TRUE),
    mean_rd = mean(rd, na.rm = TRUE),
    se_rd = sd(rd, na.rm = TRUE) / sqrt(n()),.groups = "drop")

# Figure 4 – Interaction between familiarity and duration
ggplot() +geom_point(data = obs_plot,
                     aes(x = fam_mid, y = mean_rd, colour = duration_factor),
                     size = 2.8,alpha = 0.9) +
  geom_errorbar(data = obs_plot,
                aes(x = fam_mid,ymin = mean_rd - se_rd,ymax = mean_rd + se_rd,
                    colour = duration_factor),
                width = 1.5,alpha = 0.5) +
  geom_line(data = pred_plot,
            aes(x = familiarity, y = pred_rd, colour = duration_factor),
            linewidth = 1.3) +
  labs(title = "Interaction between familiarity and duration",
       x = "Familiarity rating (0-100)",
       y = "Reproduced duration (ms)",
       colour = "True duration") +
  scale_color_discrete(labels = c("800 ms", "1600 ms")) +
  theme_bw() + theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("images/figure_4.png", width = 8, height = 6, dpi = 300)

# Figure 4.1 – Interaction between familiarity and duration (Neutral faces only)
neutral_data <- full_data_all %>% filter(stimuli_condition == "Neutral")

obs_plot_neutral <- neutral_data %>%
  mutate(fam_bin = cut(familiarity, breaks = seq(0, 100, by = 10), include.lowest = TRUE)) %>%
  group_by(duration_factor, fam_bin) %>%
  summarise(
    fam_mid = mean(familiarity, na.rm = TRUE),
    mean_rd = mean(rd, na.rm = TRUE),
    se_rd = sd(rd, na.rm = TRUE) / sqrt(n()), .groups = "drop")

pred_grid_neutral <- expand.grid(
  familiarity = seq(0, 100, length.out = 200),
  duration_factor = c("short", "long"),
  appeal = mean(neutral_data$appeal, na.rm = TRUE),
  stimuli_condition = "Neutral")

pred_grid_neutral$pred_rd <- predict(model_il_rd, newdata = pred_grid_neutral, re.form = NA)

ggplot() +
  geom_point(data = obs_plot_neutral,
             aes(x = fam_mid, y = mean_rd, colour = duration_factor),
             size = 2.8, alpha = 0.9) +
  geom_errorbar(data = obs_plot_neutral,
                aes(x = fam_mid, ymin = mean_rd - se_rd, ymax = mean_rd + se_rd, colour = duration_factor),
                width = 1.5, alpha = 0.5) +
  geom_line(data = pred_grid_neutral,
            aes(x = familiarity, y = pred_rd, colour = duration_factor),
            linewidth = 1.3) +
  labs(title = "Interaction between familiarity and duration (Neutral faces)",
       x = "Familiarity rating (0-100)",
       y = "Reproduced duration (ms)",
       colour = "True duration") +
  scale_color_discrete(labels = c("800 ms", "1600 ms")) +
  theme_bw() + theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("images/figure_4_1.png", width = 8, height = 6, dpi = 300)

# Figure 4.2 – Interaction between familiarity and duration (UK faces only)
uk_data <- full_data_all %>% filter(stimuli_condition == "UK")

obs_plot_uk <- uk_data %>%
  mutate(fam_bin = cut(familiarity, breaks = seq(0, 100, by = 10), include.lowest = TRUE)) %>%
  group_by(duration_factor, fam_bin) %>%
  summarise(
    fam_mid = mean(familiarity, na.rm = TRUE),
    mean_rd = mean(rd, na.rm = TRUE),
    se_rd = sd(rd, na.rm = TRUE) / sqrt(n()), .groups = "drop")

pred_grid_uk <- expand.grid(
  familiarity = seq(0, 100, length.out = 200),
  duration_factor = c("short", "long"),
  appeal = mean(uk_data$appeal, na.rm = TRUE),
  stimuli_condition = "UK")

pred_grid_uk$pred_rd <- predict(model_il_rd, newdata = pred_grid_uk, re.form = NA)

ggplot() +
  geom_point(data = obs_plot_uk,
             aes(x = fam_mid, y = mean_rd, colour = duration_factor),
             size = 2.8, alpha = 0.9) +
  geom_errorbar(data = obs_plot_uk,
                aes(x = fam_mid, ymin = mean_rd - se_rd, ymax = mean_rd + se_rd, colour = duration_factor),
                width = 1.5, alpha = 0.5) +
  geom_line(data = pred_grid_uk,
            aes(x = familiarity, y = pred_rd, colour = duration_factor),
            linewidth = 1.3) +
  labs(title = "Interaction between familiarity and duration (UK faces)",
       x = "Familiarity rating (0-100)",
       y = "Reproduced duration (ms)",
       colour = "True duration") +
  scale_color_discrete(labels = c("800 ms", "1600 ms")) +
  theme_bw() + theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("images/figure_4_2.png", width = 8, height = 6, dpi = 300)

# Figure 4.3 – Interaction between familiarity and duration (IL faces only)
il_data <- full_data_all %>% filter(stimuli_condition == "IL")

obs_plot_il <- il_data %>%
  mutate(fam_bin = cut(familiarity, breaks = seq(0, 100, by = 10), include.lowest = TRUE)) %>%
  group_by(duration_factor, fam_bin) %>%
  summarise(
    fam_mid = mean(familiarity, na.rm = TRUE),
    mean_rd = mean(rd, na.rm = TRUE),
    se_rd = sd(rd, na.rm = TRUE) / sqrt(n()), .groups = "drop")

pred_grid_il <- expand.grid(
  familiarity = seq(0, 100, length.out = 200),
  duration_factor = c("short", "long"),
  appeal = mean(il_data$appeal, na.rm = TRUE),
  stimuli_condition = "IL")

pred_grid_il$pred_rd <- predict(model_il_rd, newdata = pred_grid_il, re.form = NA)

ggplot() +
  geom_point(data = obs_plot_il,
             aes(x = fam_mid, y = mean_rd, colour = duration_factor),
             size = 2.8, alpha = 0.9) +
  geom_errorbar(data = obs_plot_il,
                aes(x = fam_mid, ymin = mean_rd - se_rd, ymax = mean_rd + se_rd, colour = duration_factor),
                width = 1.5, alpha = 0.5) +
  geom_line(data = pred_grid_il,
            aes(x = familiarity, y = pred_rd, colour = duration_factor),
            linewidth = 1.3) +
  labs(title = "Interaction between familiarity and duration (IL faces)",
       x = "Familiarity rating (0-100)",
       y = "Reproduced duration (ms)",
       colour = "True duration") +
  scale_color_discrete(labels = c("800 ms", "1600 ms")) +
  theme_bw() + theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("images/figure_4_3.png", width = 8, height = 6, dpi = 300)

# Spaghetti plot – participant means for 800 vs 1600 ms
# calculate participant means per duration
participant_means <- full_data_all %>%
  group_by(participant, duration_factor) %>%
  summarise(mean_rd = mean(rd, na.rm = TRUE), .groups = "drop")

# grand mean per duration (for the thick overlay line)
grand_means <- participant_means %>%
  group_by(duration_factor) %>%
  summarise(mean_rd = mean(mean_rd, na.rm = TRUE), .groups = "drop")

# --- Participant breakdown: who reproduced 800 < 1600 and who didn't? ---
participant_wide <- participant_means %>%
  pivot_wider(id_cols = participant,
              names_from = duration_factor,
              values_from = mean_rd)

participant_wide$pattern <- ifelse(participant_wide$short < participant_wide$long,
                                   "normal",   # 800 < 1600 (expected)
                                   "reversed") # 800 >= 1600 (unexpected)

n_normal   <- sum(participant_wide$pattern == "normal")
n_reversed <- sum(participant_wide$pattern == "reversed")
n_total    <- nrow(participant_wide)

cat("\n========== PARTICIPANT SUMMARY ==========\n")
cat(sprintf("Total participants: %d\n", n_total))
cat(sprintf("Normal pattern  (800 < 1600): %d  (%.1f%%)\n",
            n_normal, 100 * n_normal / n_total))
cat(sprintf("Reversed pattern (800 >= 1600): %d  (%.1f%%)\n",
            n_reversed, 100 * n_reversed / n_total))

if (n_reversed > 0) {
  cat("\nReversed participants:\n")
  reversed_subs <- participant_wide %>% filter(pattern == "reversed")
  for (i in seq_len(nrow(reversed_subs))) {
    cat(sprintf("  %s — mean 800ms: %.1f, mean 1600ms: %.1f\n",
                reversed_subs$participant[i],
                reversed_subs$short[i],
                reversed_subs$long[i]))
  }
}
cat("=========================================\n\n")

# colour each participant by pattern in the plot
participant_means <- participant_means %>%
  left_join(participant_wide[, c("participant","pattern")], by = "participant")

# spaghetti plot
ggplot(participant_means, aes(x = duration_factor, y = mean_rd)) +
  # individual participant lines (coloured by pattern)
  geom_line(aes(group = participant, colour = pattern),
            alpha = 0.3, linewidth = 0.5) +
  geom_point(aes(group = participant, colour = pattern),
             alpha = 0.4, size = 1.5) +
  scale_colour_manual(values = c("normal" = "steelblue",
                                 "reversed" = "orange"),
                      labels = c("normal"   = "800 < 1600 (expected)",
                                 "reversed" = "800 ≥ 1600 (reversed)"),
                      name = "Pattern") +
  # grand mean line
  geom_line(data = grand_means, aes(group = 1),
            linewidth = 1.8, colour = "firebrick") +
  geom_point(data = grand_means, size = 4, colour = "firebrick") +
  # identity reference line at actual durations
  annotate("point", x = c("short","long"), y = c(800, 1600),
           shape = 4, size = 5, colour = "grey40", stroke = 1.5) +
  scale_x_discrete(labels = c("short" = "800 ms", "long" = "1600 ms")) +
  labs(title = "Reproduced duration per participant",
       subtitle = "Red line = grand mean | × = true duration",
       x = "True duration",
       y = "Mean reproduced duration (ms)") +
  theme_bw() +
  theme(plot.title    = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40"))
ggsave("images/figure_spaghetti.png", width = 8, height = 6, dpi = 300)
