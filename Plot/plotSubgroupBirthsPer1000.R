#calculate age specific birth rates by year and state for all women, less educated women, unmarried women and less educated unmarried women

library(tidyverse)
library(grid)
library(ggnewscale)
library(gtable)
library(cowplot)

data <- read_csv("Data/Clean/mergedBirth.csv")
crosswalk <- read_csv("Data/Clean/capStateCrosswalk.csv")

# All people --------------------------------------------------------------
df <- data
#create variable for number of people per state
df$FemBirth <- ifelse(df$BIRTHi == 1, df$WTFINL, 0)

agg <- df %>%
  group_by(STATECENSUS, YEAR) %>%
  summarise(nF = sum(WTFINL), 
            nBirth = sum(FemBirth),
            YEARCAPPED = max(YEARCAPPED),
            CAPGrp =max(CAPGrp)) %>%
  #express birth rate as births per 1000 women
  mutate(birthrateFem = 1000*nBirth/nF) %>%
  mutate(treat = case_when(
    CAPGrp != 0 ~ 0,
    TRUE ~ 1))

fullSample <- agg %>%
  dplyr::select(STATECENSUS, YEAR, birthrateFem, YEARCAPPED, treat) %>%
  mutate(type = 'All women')

# Summarise group data --------------------------------------------------
df_groups <- df %>%
  mutate(group = case_when(EDUCATIONi == 0 & MARSTi == 0 ~ 1,
                           EDUCATIONi == 1 & MARSTi == 0 ~ 2,
                           EDUCATIONi == 0 & MARSTi == 1 ~ 3,
                           EDUCATIONi == 1 & MARSTi == 1 ~ 4,
                           TRUE ~ NA),
         group_label = case_when(group == 1 ~ "Unmarried, less-educated women",
                                 group == 2 ~ "Unmarried, more-educated women",
                                 group == 3 ~ "Married, less-educated women",
                                 group == 4 ~ "Married, more-educated women",
                                 TRUE ~ NA)) %>%
  rbind(df %>%
          mutate(group = 5,
                 group_label = "All women")) %>%
  mutate(cohort = case_when(is.na(YEARCAPPED) == T ~ "Never treated",
                            TRUE ~ as.character(YEARCAPPED)),
         treatment_group = case_when(is.na(YEARCAPPED) == T ~ 0,
                                      YEAR < YEARCAPPED ~ 1,
                                     YEAR >= YEARCAPPED & is.na(YEARREPEALED) == T ~ 2,
                                     YEAR >= YEARCAPPED & is.na(YEARREPEALED) == F & YEAR < YEARREPEALED ~ 2,
                                     TRUE ~ NA),
         treatment_label1 = case_when(treatment_group == 0 ~ "Never treated",
                                            treatment_group == 1 ~ "Not yet treated",
                                            treatment_group == 2 ~ "Treated",
                                      TRUE ~ NA
                                            ),
         treatment_label2 = case_when(treatment_group == 0 ~ "Never treated",
                                      treatment_group %in% c(1,2) ~ "Ever treated",
                                      TRUE ~ NA),
         treatment_label3 = case_when(treatment_group %in% c(0, 1) ~ "Untreated",
                                      treatment_group==2  ~ "Treated",
                                      TRUE ~ NA)) %>%
  select(YEAR, group, group_label, cohort, treatment_group, 
         treatment_label1, treatment_label2, treatment_label3, WTFINL, BIRTHi) %>%
  mutate(group_label = factor(group_label, levels = c("Unmarried, less-educated women", "Unmarried, more-educated women",
                                                      "Married, less-educated women", "Married, more-educated women", 
                                                      "All women")),
         treatment_label2 = factor(treatment_label2, levels = c("Never treated", "Ever treated")))


# Cohort level summary ----------------------------------------------------
cohort_summary <- df_groups %>%
  group_by(YEAR, cohort, group, group_label) %>%
  summarise(n_women = sum(WTFINL), # total person weights
            n_births = sum(BIRTHi*WTFINL)) %>% # person weight * birth dummy
  ungroup() %>%
  mutate(births_per_1000 = 1000*n_births/n_women) %>%
  filter(YEAR %in% c(1982:2010))


# Group level summary -----------------------------------------------------
group_summary <- df_groups %>%
  group_by(YEAR, treatment_label2, group, group_label) %>%
  summarise(n_women = sum(WTFINL),
            n_births = sum(BIRTHi*WTFINL)) %>%
  ungroup() %>%
  mutate(births_per_1000 = 1000*n_births/n_women) %>%
  na.omit() %>%
  filter(YEAR %in% c(1982:2010))

fig <- ggplot(data = cohort_summary, aes(x = YEAR, y = births_per_1000, group = cohort)) +
  geom_line(data = group_summary, aes(x = YEAR, y = births_per_1000, group = treatment_label2, colour = treatment_label2), linewidth = 0.5) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(colour = "") +
  new_scale_color()+
  geom_point(aes(colour = cohort), size = 0.8) +
  scale_colour_manual(values = c("#8dd3c7", "lightgoldenrod2", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "black")) +
  labs(colour = "Cohort") +
  facet_wrap(~group_label, scales = nrow = 3, ncol = 2) +
  labs(y = "Births per 1000 women", x = "Year") +
  scale_x_continuous(breaks = seq(1982, 2010, 4), labels = seq(1982, 2010, 4)) +
  geom_vline(xintercept = 1992, colour = "black", linetype = "longdash") +
  theme_bw() +
  theme(legend.box = "horizontal",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = c(1, 0),
        legend.justification = c(1 ,0),
        plot.margin = margin(0.25, 0.5, 0.25, 0.5, "cm"))

ggsave("Output/Figures/groupBirthsPer1000.png", fig, width = 7, height = 8)
