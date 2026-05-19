# ==============================================================================
# Thesis Analysis Script
# Name: Anastasia Michaelidou
# Project's topic: Cognitive Performance in Aging Populations
#
# Note:
# OpenAI ChatGPT was used to assist with debugging and support the
# programming process during the development of this analysis.
# ==============================================================================




# ===================== LOADING PACKAGES =======================================
library(readxl)    # reading excel files
library(dplyr)     # data management (grouping, filtering, mutating, summarizing)
library(tidyr)     # reshaping data (long and wide formats)
library(ggplot2)   # plotting
library(patchwork) # combining plots

# ===================== LOADING DATA ===========================================


# ---- Older adults: trial-level datasets ----
VISUAL_OLD <- read_excel("data/VISUAL_OLD.xlsx")
WACK_OLD <- read_excel("data/WACK_OLD.xlsx")
CORSI_OLD <- read_excel("data/CORSI_OLD.xlsx")
FLANKER_OLD <- read_excel("data/FLANKER_OLD.xlsx")
RETRO_OLD <- read_excel("data/RETRO_OLD.xlsx")

# ---- Younger adults datasets ----
VISUAL_YOUNG <- read_excel("data/VISUAL_YOUNG.xlsx")
WACK_YOUNG<- read_excel("data/WACK_YOUNG.xlsx")


# Note.Comparison datasets (data for both groups) are loaded later in the script,
# since they use the older datasets transformed per participant.


# ===================== VISUAL SEARCH TASK =====================================


# ---- Create condition variable (search type + number of objects) per trial ---

VISUAL_OLD <- VISUAL_OLD %>%
  mutate(condition = paste(search_type, number_of_objects, sep = "_"))


# ---- Mean RT per participant and condition -----------------------------------

means_V <- VISUAL_OLD %>%
  group_by(`A/A`, condition) %>%
  summarise(mean_rt = mean(reaction_time, 
                           na.rm = TRUE)) #ignores missing values


# ---- Convert to wide format --------------------------------------------------

visual_data_old <- pivot_wider(means_V, names_from = condition, 
                               values_from = mean_rt)


# ---- Save dataset ------------------------------------------------------------

write.csv(visual_data_old, "visual_search_means_old.csv",
          row.names = FALSE) #avoids additional indexes


# ---- Participant-level means (Feature, Conjunction, Overall) -----------------

summary_means_participant_old <- VISUAL_OLD %>%
  group_by(`A/A`) %>%
  summarise(
    Mean_Conjunction = mean(reaction_time[search_type == "Conjunction_Search"],
                            na.rm = TRUE),
    Mean_Feature = mean(reaction_time[search_type == "Feature_Search"],
                        na.rm = TRUE),
    MeanVS = mean(reaction_time, 
                  na.rm = TRUE)
  )

write.csv(summary_means_participant_old,
          "summary_means_of_visual_search_old.csv", row.names = FALSE)


# ---- Plot function (mean + SE) -----------------------------------------------

plot_mean_rt <- function(data, title_text) {
  
  ggplot(data,
         aes(x = search_type,
             y = reaction_time,
             color = factor(number_of_objects),
             group = number_of_objects)) +
    
    stat_summary(fun = mean,
                 geom = "line", #mean per condition
                 linewidth = 1.3) + 
    stat_summary(fun = mean, 
                 geom = "point",
                 size = 2.5) +
    stat_summary(fun.data = mean_se,
                 geom = "errorbar",
                 width = 0.1, 
                 linewidth = 1) +
    
    theme_minimal() +
    labs(
      y = "Reaction Time (seconds)",
      x = "Search Type",
      title = title_text,
      color = "Number of objects"
    )
}


# ---- Convert younger dataset to long format and reshape it for the plot ------

VISUAL_LONG_Y <- VISUAL_YOUNG %>%
  pivot_longer(cols = -`A/A`,
               names_to = "condition",
               values_to = "reaction_time") %>%
  
  filter(grepl("Conjunction|Feature", condition)) %>%            
  separate(condition, into = c("search_type", "set_size"), sep = "_") %>%
  rename(number_of_objects = set_size)


# -------------------- COMBINE PLOTS (OLDER+YOUNGER ADULTS) IN ONE FIGURE ------


# ---- Consistent legend -------------------------------------------------------

VISUAL_OLD$number_of_objects <- factor(VISUAL_OLD$number_of_objects,
                                       levels = c(5, 15, 25, 40))

VISUAL_LONG_Y$number_of_objects <- factor(VISUAL_LONG_Y$number_of_objects,
                                        levels = c(5, 15, 25, 40))


# ---- Consistent label ordering -----------------------------------------------

VISUAL_OLD$search_type <- factor(VISUAL_OLD$search_type,
                                 levels = c("Feature_Search", 
                                            "Conjunction_Search"),
                                 labels = c("Feature", "Conjunction"))

VISUAL_LONG_Y$search_type <- factor(VISUAL_LONG_Y$search_type,
                                  levels = c("Feature", "Conjunction"))


# ---- Plots for conditions in each group separately --------------------------- 

plot_older <- plot_mean_rt(VISUAL_OLD, "a) Older Adults")+
  labs(                                                      
    x = "Search Type",
    y = "Reaction Time (seconds)"
  ) +
  theme(
    axis.title.x = element_text(hjust = 1.4) # x axis title to the center
  )

plot_younger <- plot_mean_rt(VISUAL_LONG_Y, "b) Younger Adults")+
  theme(
    axis.title.x = element_blank(), #leaving the x and y axis blank:                                 
    axis.title.y = element_blank()  # axis title is shared with the older plot
  )


# ---- Combine plots -----------------------------------------------------------

combined_visual_plot <- (plot_older + plot_younger) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

combined_visual_plot +
  plot_annotation(
    title = "Visual Search Performance" #figure title
  ) &
  theme(
    plot.title = element_text(hjust = 0.5, size = 11)
  ) 


# --------------------- VISUAL COMPARISON/ SLOPE PLOT --------------------------


VISUAL_COMPARISON <- read_excel("data/VISUAL_COMPARISON.xlsx") # loading file with young and old data


# ---- Convert to long format and structure specific variables -----------------

long_visual_ALL <- VISUAL_COMPARISON %>%
  pivot_longer(
    cols = -c(Subject,                #leave behind these columns
              Group, Mean_Conjunction,
              Mean_Feature,
              MeanVS),
    names_to = "condition",
    values_to = "reaction_time"
  ) %>%
  
  separate(condition,
           into = c("search_type", "number_of_objects"),
           sep = "_") %>%
  
  mutate(
    number_of_objects = as.numeric(number_of_objects),
    search_type = factor(search_type),
    Group = factor(Group)
  )


# ---- SLOPE PLOT FOR COMPARING AGE GROUPS -------------------------------------

ggplot(long_visual_ALL,
       aes(x = number_of_objects,
           y = reaction_time,
           color = search_type,
           linetype = Group,
           group = interaction(search_type, Group))) +
  
  stat_summary(fun = mean, # mean per condition of each group
               geom = "line", 
               linewidth = 1.3) + 
  stat_summary(fun = mean,
               geom = "point", 
               size = 2.5) +
  
  scale_x_continuous(breaks = c(5,15,25,40)) +
  scale_color_manual(values = c("magenta", "cyan4")) +
  scale_y_continuous(breaks = seq(0, 12, by = 2.5)) +
  
  theme_minimal() +
  scale_linetype_manual(values = c("solid", "dotted"),
                        labels = c("O" = "Older",
                                   "Y" = "Younger")) +
  labs(
    x = "Number of Objects",
    y = "Reaction Time (seconds)",
    title = "Comparison of Reaction Time Slopes Across Conditions in Visual Search Between Groups",
    color = "Search Type",
    linetype = "Age Group"
  )

# ----------------- VIOLIN PLOT FOR EACH AGE GROUP (jitter data) ---------------

plot_data <- long_visual_ALL %>%
  group_by(Subject, Group, search_type) %>%
  summarise(mean_rt = mean(reaction_time,
                           na.rm = TRUE), .groups = "drop") %>%
  
  mutate(Group = recode(Group, "O" = "Older Group", 
                        "Y" = "Younger Group"))

ggplot(plot_data, aes(x = search_type, 
                      y = mean_rt, 
                      fill = search_type)) +
  
  geom_violin(alpha = 0.3, color = "black") +
  geom_jitter(aes(shape = Group), 
              width = 0.2, 
              size = 1.5, 
              alpha = 0.6, 
              show.legend = FALSE) +
  
  stat_summary(fun = mean,       # mean per search type of each group
               geom = "point",
               shape = 23,
               fill = "red",
               color = "black", 
               size = 2, 
               show.legend = FALSE) +
  
  stat_summary(fun.data = mean_se, 
               geom = "errorbar",
               width = 0.1) +
  
  scale_y_log10() +
  facet_wrap(~Group) +
  
  scale_fill_manual(values = c("Feature" = "cyan4",
                               "Conjunction" = "magenta")) +
  
  scale_shape_manual(values = c(16, 17)) +
  guides(shape = "none") +
  
  labs(
    x = "Search Type",
    y = "Reaction Time (s) [Log Transformation]",
    title = "Visual Search Reaction Time by Search Type and Age Group",
    fill = "Search Type"
  ) +
  
  theme_minimal(base_size = 12)


# ===================== WACK-A-MOLE TASK =======================================


# ---- Function to compute d'---------------------------------------------------

compute_dprime <- function(hit_rate, fa_rate) {
  
  # Apply boundary correction
  
  boundary_hit_rate <- ifelse(hit_rate == 1, 0.99,  #avoid 0 or 1/extreme values that would 
                                                    #give infinite outcome with qnorm function
                                                                     
                              ifelse(hit_rate == 0, 0.01, hit_rate))
  
  boundary_fa_rate <- ifelse(fa_rate == 1, 0.99,
                             ifelse(fa_rate == 0, 0.01, fa_rate))
  
  # Compute d'
  
  dprime <- qnorm(boundary_hit_rate) - qnorm(boundary_fa_rate) #convert to z scores 
                                                               #and subtract them
  
  return(dprime)
}


# ---- Apply to young adults dataset -------------------------------------------

WACK_YOUNG$WTM_dprime <- compute_dprime(WACK_YOUNG$hit_rate,
                                        WACK_YOUNG$fa_rate)

write.csv(WACK_YOUNG, "WACK_YOUNG_dprime.csv", row.names = FALSE)


# ---- Calculate signal detection metrics per participant (older adults) -------

WTM_metrics <- WACK_OLD %>%
  group_by(`A/A`) %>% # per participant
  
  summarise(
    Hits = sum(response_classification == "Hit"),
    Miss = sum(response_classification == "Miss"),
    FA   = sum(response_classification == "FA"),
    CR   = sum(response_classification == "CR"),
    
    # ---- Compute hit rate and false alarm rate -------------------------------
    
    hit_rate = Hits / (Hits + Miss),
    fa_rate  = FA / (FA + CR),
  
    # ---- d' for older adults -------------------------------------------------
    
    WTM_dprime = compute_dprime(hit_rate, fa_rate),
    
    # ---- Mean reaction time for correct hits ---------------------------------
    
    WTM_RT_hits = mean(reaction_time[response_classification == "Hit"], 
                       na.rm = TRUE)
  )

write.csv(WTM_metrics, "wack_search_means.csv", row.names = FALSE)


# -------------- VIOLIN PLOT/RT FOR HITS BY GROUP (jitter data) ----------------

WACK_COMPARISON <- read_excel("data/WACK_COMPARISON.xlsx") # loading file with young and old data     

WACK_COMPARISON$Group <- factor(WACK_COMPARISON$Group,
                                levels = c("O", "Y"),
                                labels = c("Older Group", 
                                           "Younger Group"))

ggplot(WACK_COMPARISON, aes(x = Group, y = WTM_RT_hits, fill = Group)) +
  geom_violin(alpha = 0.3) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.6, color = "black") +
  stat_summary(fun = mean,             #mean rt of hits per group 
               geom = "point", 
               shape = 23,
               size = 3, 
               fill = "red") +
  
  stat_summary(fun.data = mean_se, 
               geom = "errorbar",
               width = 0.1) +
  labs(
    title = "Reaction Time Hits by Group in Wack-a-Mole Task",
    x = "Group",
    y = "RT Hits (seconds)"
  ) +
  
  scale_fill_manual(values = c("Older Group" = "darkred",
                               "Younger Group" = "darkgrey")) +
  theme_minimal() +
  theme(legend.position = "none")

# --------------------- WACK-A-MOLE ACCURACY -----------------------------------

# ---- Convert hit/FA rates to long format -------------------------------------

rate_long <- WACK_COMPARISON %>%
  pivot_longer(
    cols = c(hit_rate, fa_rate),
    names_to = "Response_Type",
    values_to = "Rate"
  )

# ---- Rename response labels --------------------------------------------------

rate_long$Response_Type <- recode(rate_long$Response_Type,
                                  hit_rate = "Hit",
                                  fa_rate  = "FA")


# ---- ACCURACY BOXPLOT (jitter data) ----------------------------------------

ggplot(rate_long, aes(x = Response_Type, y = Rate, fill = Group)) +
  geom_boxplot(
    position = position_dodge(width = 1),
    alpha = 0.4,
    outlier.shape = NA
  ) +
  
  geom_point(
    aes(group = Group, 
        shape = Group, 
        colour = Group),
    position = position_jitterdodge(jitter.width = 0.95, dodge.width = 1.2),
    size = 1.5,
    alpha = 0.5,
    show.legend = TRUE
  ) +
  
  stat_summary(aes(group = Group),
               fun = mean,              #mean accuracy per group
               geom = "point",
               shape = 23,
               fill = "red",
               color = "black",
               size = 2,
               position = position_dodge(width = 1),
               show.legend = FALSE) +
  
  scale_color_manual(name = "Age Group",
                     values = c("O" = "darkred",
                                "Y" = "darkgrey"),
                     labels = c("O" = "Older Adults", 
                                "Y" = "Younger Adults")) +
  
  scale_fill_manual(name = "Age Group",
                    values = c("O" = "darkred", 
                               "Y" = "darkgrey"),
                    labels = c("O" = "Older Adults", 
                               "Y" = "Younger Adults")) +
  
  scale_shape_manual(name = "Age Group",
                     values = c("O" = 16, "Y" = 17),
                     labels = c("O" = "Older Adults", 
                                "Y" = "Younger Adults")) +
  
  scale_x_discrete(labels = c("FA" = "False Alarms", 
                              "Hit" = "Hits")) +
  labs(
    x = "Response Type",
    y = "Rate",
    title = "Response Type by Group in Wack-a-Mole Task"
  ) +
  theme_minimal()


# ===================== CORSI TASK =============================================


# ---- Extract maximum span per participant ------------------------------------

CORSI_ANOVA <- CORSI_OLD %>%
  group_by(`A/A`) %>%
  summarise(
    corsi_span = max(corsi_span, na.rm = TRUE)
  )

write.csv(CORSI_ANOVA, "Corsi_anova.csv", row.names = FALSE)


# ---- VIOLIN SPAN PLOT/ GROUP COMPARISON (jitter data) ------------------------

CORSI_COMPARISON <- read_excel("data/CORSI_COMPARISON.xlsx") #loading file with young and old data

ggplot(CORSI_COMPARISON,
       aes(x = GROUP,
           y = Corsi_Span, 
           fill = GROUP)) +
  
  geom_violin(alpha = 0.3, 
              width = 0.6, 
              color = "black") +
  
  geom_jitter(width = 0.1,   # jitter both vertically and horizontally by default/however span score gives integer number
              size = 1.5, 
              alpha = 0.6) +   
  
  stat_summary(fun = mean,     # mean span per group
               geom = "point", 
               shape = 23, 
               size = 3,
               fill = "red") + 
  
  stat_summary(fun.data = mean_se,
               geom = "errorbar", 
               width = 0.1, 
               color = "black") +
  
  scale_fill_manual(values = c("Older Group" = "darkred",
                               "Younger Group" = "darkgrey")) +
  labs(
    x = "Group",
    y = "Corsi Span",
    title = "Spatial Working Memory Performance in Corsi Block Game"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# ---- VIOLIN PLOT BY AGE COHORT (jitter data) ---------------------------------

ggplot(CORSI_COMPARISON,
       aes(x = as.factor(Age_Group),
           y = Corsi_Span, fill = as.factor(Age_Group))) +
  
  geom_violin(alpha = 0.3, width = 0.6, color = "black") +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.6) +
  
  stat_summary(fun = mean, geom = "point",           #mean span per age subgroup
               shape = 23, size = 3, fill = "red") + 
  
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, color = "black") +
  
  scale_fill_manual(values = c(
    "1" = "green",
    "2" = "darkgreen",
    "3" = "lightblue",
    "4" = "purple",
    "5" = "violet"
  )) +
  scale_x_discrete(labels = c(
    "5" = "18–19",       #cohorts divided based on age range and sample size
    "4" = "20–29",
    "3" = "60–69",
    "2" = "70–79",
    "1" = "80+"
  )) +
  labs(
    x = "Age Cohorts",
    y = "Corsi Span",
    title = "Corsi Span by Age Cohort in Corsi Block Game"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# ===================== FLANKER TASK ===========================================


# ---- Compute mean RT (correct trials) and accuracy ---------------------------

FLANKER <- FLANKER_OLD %>%
  group_by(`A/A`, stimulus) %>%
  summarise(
    mean_RT = mean(reaction_time[accuracy == 1], na.rm = TRUE),
    accuracy = mean(accuracy, na.rm = TRUE)
  ) %>%
  
  pivot_wider(                         #convert to wide format
    names_from = stimulus,
    values_from = c(mean_RT, accuracy)
  )

write.csv(FLANKER, "FLANKER.csv", row.names = FALSE)


# ---- Exclude participants with low accuracy ----------------------------------

ACCURACY_FLANKER <- FLANKER %>%
  mutate(overall_accuracy = (accuracy_Congruent + accuracy_Incongruent) / 2)

C_FLANKER <- ACCURACY_FLANKER %>%
  filter(overall_accuracy > 0.50)   #include participants with over 50% accuracy

original <- ACCURACY_FLANKER
cleaned  <- C_FLANKER


# ---- Identify excluded participants ------------------------------------------

excluded <- setdiff(original$`A/A`, cleaned$`A/A`)
print(excluded) 

write.csv(C_FLANKER, "C_FLANKER.csv", row.names = FALSE)


# ---- Convert RT to long format -----------------------------------------------

RT_long_F <- C_FLANKER %>%
  pivot_longer(
    cols = c(mean_RT_Congruent, mean_RT_Incongruent),
    names_to = "Condition",
    values_to = "RT"
  )


# ---- VIOLIN RT PLOT (jitter data) --------------------------------------------

ggplot(RT_long_F, aes(x = Condition, y = RT, fill = Condition)) +
  geom_violin(alpha = 0.3, trim = FALSE) +
  geom_jitter(width = 0.15, 
              size = 2,
              alpha = 0.6, 
              color = "black") +
  stat_summary(fun = mean,      #mean rt per condition
               geom = "point",              
               shape = 23, 
               size = 3, 
               fill = "red") + 
  
  stat_summary(fun.data = mean_se, 
               geom = "errorbar",
               width = 0.1, 
               color = "black") +
  
  scale_fill_manual(values = c("mean_RT_Congruent" = "steelblue",
                               "mean_RT_Incongruent" = "purple")) +
  
  scale_x_discrete(labels = c("Congruent", "Incongruent")) +
  labs(
    title = "Reaction Time Distribution by Condition in Flanker Task",
    x = "Condition",
    y = "Mean Reaction Time (seconds)"
  ) +
  
  theme_minimal() +
  theme(legend.position = "none")


# ---- Accuracy long format ----------------------------------------------------

C_FLANKER_long <- C_FLANKER %>%
  pivot_longer(
    cols = c(accuracy_Congruent, accuracy_Incongruent),
    names_to = "Condition",
    values_to = "Accuracy"
  )


# ---- ACCURACY BOXPLOT (jitter data) ------------------------------------------

ggplot(C_FLANKER_long, aes(x = Condition, 
                           y = Accuracy, 
                           fill = Condition)) +
  
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  
  geom_jitter(width = 0.15, size = 2,
              alpha = 0.6,
              color = "black") +
  
  stat_summary(fun = mean,  #mean accuracy per condition 
               geom = "point",  
              shape = 23,
              size = 3,
              fill = "red") + 
  
  ylim(0, 1) +
  scale_fill_manual(values = c("steelblue", "purple")) +
  scale_x_discrete(labels = c("Congruent", "Incongruent")) +
  
  labs(
    x = "Condition",
    y = "Accuracy",
    title = "Accuracy by Condition Type in Flanker Task"
  ) +
  
  theme_minimal() +
  theme(legend.position = "none")


# ===================== PRESENT / ABSENT TASK ==================================


# ---- Compute mean RT (correct trials) and accuracy ---------------------------

RETRO_ANOVA <- RETRO_OLD %>%
  group_by(`A/A`, condition_type) %>%
  summarise(
    mean_RT = mean(reaction_time[accuracy == 1], na.rm = TRUE),
    accuracy = mean(accuracy, na.rm = TRUE)
  ) %>%
  
  pivot_wider(                         #convert to wide format
    names_from = condition_type,
    values_from = c(mean_RT, accuracy)
  )

write.csv(RETRO_ANOVA, "RETRO_ANOVA.csv", row.names = FALSE)


# ---- Compute overall accuracy per participant---------------------------------

RETRO_ACC <- RETRO_ANOVA %>%
  mutate(overall_accuracy =
           (accuracy_NeutralCue + accuracy_PreCue + accuracy_RetroCue) / 3)

write.csv(RETRO_ACC, "retro_overall_acc.csv", row.names = FALSE)

# ---- Exclude low-accuracy participants ---------------------------------------

C_RETRO_ANOVA <- RETRO_ACC %>%
  filter((overall_accuracy > 0.50)) #include participants with over 50% overall accuracy 

original <- RETRO_ACC
cleaned  <- C_RETRO_ANOVA


# ---- Identify excluded participants ------------------------------------------

excluded <- setdiff(original$`A/A`, cleaned$`A/A`)
print(excluded)

write.csv(C_RETRO_ANOVA, "C_RETRO_ANOVA.csv", row.names = FALSE)


# ---- Compute overall RT per participant for statistical analyses in jamovi ---

RETRO_RT <-C_RETRO_ANOVA %>%
  mutate(overall_RT =
           (mean_RT_NeutralCue +
              mean_RT_PreCue + 
              mean_RT_RetroCue) / 3)

write.csv(RETRO_RT, "retro_overall_rt.csv", row.names = FALSE)


# ---- Convert RT to long format -----------------------------------------------

RT_long_R <- C_RETRO_ANOVA %>%
  pivot_longer(
    cols = starts_with("mean_RT"),
    names_to = "Cue",
    values_to = "RT"
  ) %>%
  mutate(Cue = recode(Cue,
                      "mean_RT_NeutralCue" = "Neutral Cue",
                      "mean_RT_PreCue" = "Pre Cue",
                      "mean_RT_RetroCue" = "Retro Cue"))


# ---- Convert accuracy to long format -----------------------------------------

ACC_long <- C_RETRO_ANOVA %>%
  pivot_longer(
    cols = starts_with("accuracy"),
    names_to = "Cue",
    values_to = "Accuracy"
  ) %>%
  
  mutate(Cue = recode(Cue,
                      "accuracy_NeutralCue" = "Neutral Cue",
                      "accuracy_PreCue" = "Pre Cue",
                      "accuracy_RetroCue" = "Retro Cue"))


# ---- VIOLIN RT PLOT (jitter data) --------------------------------------------

ggplot(RT_long_R, aes(x = Cue,
                      y = RT,
                      fill = Cue)) +
  geom_violin(trim = FALSE,
              alpha = 0.3) +
  
  geom_jitter(width = 0.1, 
              size = 1.5, 
              alpha = 0.7) +
  
  stat_summary(fun = mean, #mean rt per cue type
               geom = "point",    
               shape = 23, 
               size = 3, 
               fill = "red") + 
  
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(
    x = "Cue Type",
    y = "Reaction Time (seconds)",
    title = "Reaction Time by Cue Condition in Present/Absent task"
  ) +
  
  theme_minimal() +
  theme(legend.position = "none")


# ---- ACCURACY BOXPLOT (jitter data) --------------------------------------------------------

ggplot(ACC_long, aes(x = Cue, 
                     y = Accuracy, 
                     fill = Cue)) +
  
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  
  geom_jitter(width = 0.15, size = 2, alpha = 0.6, color = "black") +
  
  stat_summary(fun = mean,  #mean accuracy per cue type
               geom = "point",          
               shape = 23,
               size = 3,
               fill = "red") + 
  ylim(0, 1) +
  labs(
    x = "Cue Condition",
    y = "Accuracy Rate",
    title = "Accuracy by Cue Type in Present/Absent task"
  ) +
  
  theme_minimal() +
  theme(legend.position= "none") 
        
        
#============================END OF SCRIPT======================================