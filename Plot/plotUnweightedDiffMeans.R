# Compute difference in means for target group over time
library(tidyverse)
library(estimatr)
library(patchwork)


# Unweighted diff means for birth -----------------------------------------
df <- read_csv("Data/Clean/mergedBirth.csv")

#subset data by characteristics and window & calculate state birth rates
stateBirths <- df %>%
  filter(proxyDem == 1) %>%
  filter(YEAR > 1981 & YEAR < 2011) %>%
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  group_by(YEAR, STATECENSUS) %>%
  summarise(birthRate = mean(BIRTHi)*1000,
            TREAT = max(TREAT))

uci <- c()
lci <- c()
diff <- c()
years <- c(1982:2010)
for (year in years){
  test <- difference_in_means(birthRate ~ TREAT, data = stateBirths[stateBirths$YEAR == year,])
  lci <- c(lci, test$conf.low)
  uci <- c(uci, test$conf.high)
  diff <- c(diff, test$coefficients[[1]])
}


dat <- cbind(years, diff, uci, lci) %>% as.data.frame()

diffBirth <- ggplot(dat, aes(x = years, y = diff)) +
  geom_vline(xintercept = 1992, colour = "grey", linetype = "longdash") +
  geom_hline(yintercept = 0, colour = "grey", linetype = "longdash")+
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin=lci, ymax=uci), width=.2,
                position=position_dodge(.9)) + 
  scale_x_continuous(breaks = seq(1982, 2010, 4), labels = seq(1982, 2010, 4)) +
  scale_y_continuous(breaks = seq(-35, 35, 5), labels = seq(-35, 35, 5)) +
  labs(x = "Year", y = "Difference in birth rates \nbetween treated and untreated states") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

#plot actual means 
means <- stateBirths %>%
  group_by(YEAR, TREAT) %>%
  summarise(mean = mean(birthRate)) %>%
  mutate(TREAT = case_when(
    TREAT == 0 ~ "Untreated states",
    TREAT == 1 ~ "Treated states"
  ))

meanBirth <- ggplot(means, aes(x = YEAR, y = mean, group = TREAT, colour = TREAT)) +
  geom_line() +
  scale_colour_manual(values = c("red", "blue")) +
  geom_vline(xintercept = 1992, colour = "grey", linetype = "longdash") +
  labs(x = "Year", y = "Average birth rate", colour = "", title = "A: Births per 1000 women") +
  scale_x_continuous(breaks = seq(1982, 2010, 4), labels = seq(1982, 2010, 4)) +
  scale_y_continuous(breaks = seq(15,60,5), labels = seq(15,60,5)) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
  

# Unweighted diff means for employment ------------------------------------
df <- read_csv("Data/Clean/mergedEmployment.csv")

#subset data by characteristics and window & calculate state birth rates
stateEmpl <- df %>%
  filter(proxyDem == 1) %>%
  filter(YEAR > 1981 & YEAR < 2011) %>%
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  group_by(YEAR, STATECENSUS) %>%
  summarise(emplRate = mean(EMPLi)*100,
            TREAT = max(TREAT))

uci <- c()
lci <- c()
diff <- c()
years <- c(1982:2010)
for (year in years){
  test <- difference_in_means(emplRate ~ TREAT, data = stateEmpl[stateEmpl$YEAR == year,])
  lci <- c(lci, test$conf.low)
  uci <- c(uci, test$conf.high)
  diff <- c(diff, test$coefficients[[1]])
}


dat <- cbind(years, diff, uci, lci) %>% as.data.frame()

diffEmpl <- ggplot(dat, aes(x = years, y = diff)) +
  geom_vline(xintercept = 1992, colour = "grey", linetype = "longdash") +
  geom_hline(yintercept = 0, colour = "grey", linetype = "longdash")+
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin=lci, ymax=uci), width=.2,
                position=position_dodge(.9)) + 
  scale_x_continuous(breaks = seq(1982, 2010, 4), labels = seq(1982, 2010, 4)) +
  scale_y_continuous(breaks = seq(-7,8,2), labels = seq(-7,8,2)) +
  labs(x = "Year", y = "Difference in employment rates \nbetween treated and untreated states") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

#plot actual means 
means <- stateEmpl %>%
  group_by(YEAR, TREAT) %>%
  summarise(mean = mean(emplRate)) %>%
  mutate(TREAT = case_when(
    TREAT == 0 ~ "Untreated states",
    TREAT == 1 ~ "Treated states"
  ))

meanEmpl <- ggplot(means, aes(x = YEAR, y = mean, group = TREAT, colour = TREAT)) +
  geom_line() +
  scale_colour_manual(values = c("red", "blue")) +
  geom_vline(xintercept = 1992, colour = "grey", linetype = "longdash") +
  labs(x = "Year", y = "Average employment rate", colour = "", title = "B: Employment rate") +
  scale_x_continuous(breaks = seq(1982, 2010, 4), labels = seq(1982, 2010, 4)) +
  scale_y_continuous(breaks = seq(50,70,2), labels = seq(50,70,2)) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())




unweighted <- (meanBirth + meanEmpl) / (diffBirth + diffEmpl)

ggsave("Output/Figures/unweightedDiffMeans.png", unweighted, height = 7, width = 8)
