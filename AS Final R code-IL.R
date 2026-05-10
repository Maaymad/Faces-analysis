library(lme4)
library(lmerTest)
library(ggplot2)
library(tidyr)
library(dplyr)

# load data
#rd <- read.csv("C:\Users\Owner\Downloads\Faces_analysis\IL_rd.csv")
#ratings <- read.csv("C:\Users\Owner\Downloads\Faces_analysis\IL_rating.csv")
#rd_cash <- read.csv("C:\Users\Owner\Downloads\Faces_analysis\IL_rd_cash.csv")
#ratings_cash <- read.csv("/Users/maaymadar/Downloads/data_exp_259711-v3/IL_ratings3.csv")

rd_il <- bind_rows(
  read.csv("//Users/maaymadar/Downloads/Faces_analysis/il_data_10-5/rd_il.csv"),
  read.csv("/Users/maaymadar/Downloads/Faces_analysis/il_data_10-5/rd_il_cash.csv")
)

ratings_il <- bind_rows(
  read.csv("/Users/maaymadar/Downloads/Faces_analysis/il_data_10-5/ratings_il.csv"),
  read.csv("/Users/maaymadar/Downloads/Faces_analysis/il_data_10-5/ratings_il_cash.csv")
)


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
  "Store..RT")]

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
full_data_il <- merge(rd_analysis,ratings_wide,
  by = c("Participant.Public.ID", "Spreadsheet..Image"))


# rename variables 
# rd = reproduced duration
names(full_data_il) <- c(
  "participant",
  "face",
  "duration",
  "rd",
  "familiarity",
  "appeal")

# convert to numeric values.
full_data_il$duration <- as.numeric(full_data_il$duration)
full_data_il$rd <- as.numeric(full_data_il$rd)
full_data_il$familiarity <- as.numeric(full_data_il$familiarity)
full_data_il$appeal <- as.numeric(full_data_il$appeal)

# create duration categories (short vs long)
full_data_il$duration_factor <- factor(full_data_il$duration,
  levels = c(800, 1600),
  labels = c("short", "long"))

# calculate reproduction error
full_data_il$error <- full_data_il$rd - full_data_il$duration

# calaculate participant means and flag
df_avg <- full_data_il %>%
  group_by(participant, duration_factor) %>%
  summarise(mean_rd = mean(rd), .groups = "drop")

df_avg_wide <- pivot_wider(df_avg,id_cols = "participant",
  names_from = duration_factor,values_from = mean_rd)

df_avg_wide$bad_sub <- ifelse(df_avg_wide$short > df_avg_wide$long, TRUE, FALSE)
print(df_avg_wide$participant[df_avg_wide$bad_sub == TRUE])

# remove 3 participants because mean reproduced duration was higher for short than long trials
full_data_il <- full_data_il[full_data_il$participant != "13132", ]
full_data_il <- full_data_il[full_data_il$participant != "13903", ]
full_data_il <- full_data_il[full_data_il$participant != "14580", ]
full_data_il <- full_data_il[full_data_il$participant != "14713", ]
full_data_il <- full_data_il[full_data_il$participant != "14867", ]
full_data_il <- full_data_il[full_data_il$participant != "60088", ]

# How many participants after removal
length(unique(full_data_il$participant))

# classify faces
full_data_il$stimuli_condition <- ifelse(substr(full_data_il$face, 1, 1) == "I",
  "IL",
  "other")

# Figure 1 – Familiarity by stimulus condition
ggplot(full_data_il, aes(x = stimuli_condition, y = familiarity)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.08, alpha = 0.2, size = 1, height = 0) +
  labs(title = "Familiarity by stimulus condition",
    x = "Stimulus condition",
    y = "Familiarity rating (0-100)" ) +
  scale_x_discrete(labels = c("IL" = "IL faces", "other" = "Other faces")) +
  theme_bw()

# Figure 2 – Familiarity vs reproduced duration by duration
ggplot(full_data_il, aes(x = familiarity, y = rd)) +
  facet_wrap(~duration_factor,
    labeller = labeller(duration_factor = 
                          c("short" = "800 ms", "long" = "1600 ms"))) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  labs( title = "Familiarity vs reproduced duration by duration",
    x = "Familiarity rating (0-100)",
    y = "Reproduced duration (ms)") +
  theme_bw()

# Figure 3 – Familiarity vs reproduced duration by duration and stimulus condition
ggplot(full_data_il, aes(x = familiarity, y = rd)) +
  facet_grid(stimuli_condition ~ duration_factor,
    labeller = labeller(
      stimuli_condition = c("other" = "Other faces", "IL" = "IL faces"),
      duration_factor = c("short" = "800 ms", "long" = "1600 ms") )) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  labs(title = "Familiarity vs reproduced duration by duration and stimulus condition",
    x = "Familiarity rating (0-100)",
    y = "Reproduced duration (ms)") +
  theme_bw() + theme(plot.title = element_text(size = 12, hjust = 0.5))

#full_data_il$face <- as.factor(full_data_il$face) 
# main effects model
model_il_rd_main <- lmer(
  rd ~ familiarity + duration_factor + appeal + stimuli_condition +
    (1 | participant ) + (1 | face),data = full_data_il)

summary(model_il_rd_main)

# interaction model
model_il_rd <- lmer(
  rd ~ familiarity * duration_factor + appeal + stimuli_condition +
    (1 | participant) + (1 | face),data = full_data_il)

summary(model_il_rd)

full_data_il$duration_factor <- factor(full_data_il$duration_factor, levels = c("short", "long"))

# get model predictions
pred_grid <- expand.grid(
  familiarity = seq(0, 100, length.out = 200),
  duration_factor = c("short", "long"),
  appeal = mean(full_data_il$appeal, na.rm = TRUE),
  stimuli_condition = c("IL", "other"))

pred_grid$pred_rd <- predict(model_il_rd, newdata = pred_grid, re.form = NA)

pred_plot <- pred_grid %>%
  group_by(familiarity, duration_factor) %>%
  summarise(pred_rd = mean(pred_rd), .groups = "drop")

# grouping familiarity into bins
obs_plot <- full_data_il %>% mutate(fam_bin = cut(familiarity,
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



# Spaghetti plot – face means for 800 vs 1600 ms
face_means <- full_data_il %>%
  group_by(face, duration_factor, stimuli_condition) %>%
  summarise(mean_rd = mean(rd, na.rm = TRUE), .groups = "drop")

grand_means_face <- face_means %>%
  group_by(duration_factor) %>%
  summarise(mean_rd = mean(mean_rd, na.rm = TRUE), .groups = "drop")

ggplot(face_means, aes(x = duration_factor, y = mean_rd)) +
  geom_line(aes(group = face, colour = stimuli_condition),
            alpha = 0.4, linewidth = 0.5) +
  geom_point(aes(colour = stimuli_condition),
             alpha = 0.5, size = 1.5) +
  scale_colour_manual(
    values = c("IL" = "#E41A1C", "other" = "#377EB8"),
    labels = c("IL" = "IL faces", "other" = "Other faces"),
    name = "Stimulus condition") +
  geom_line(data = grand_means_face, aes(group = 1),
            linewidth = 1.8, colour = "firebrick") +
  geom_point(data = grand_means_face, size = 4, colour = "firebrick") +
  annotate("point", x = c("short", "long"), y = c(800, 1600),
           shape = 4, size = 5, colour = "grey40", stroke = 1.5) +
  scale_x_discrete(labels = c("short" = "800 ms", "long" = "1600 ms")) +
  labs(title = "Reproduced duration per face",
       subtitle = "Red line = grand mean | × = true duration",
       x = "True duration",
       y = "Mean reproduced duration (ms)") +
  theme_bw() +
  theme(plot.title    = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40"))

# Spaghetti plot – participant means for 800 vs 1600 ms
# calculate participant means per duration
participant_means <- full_data_il %>%
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

