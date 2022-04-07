#calculate state level birth rates with weights
df <- read_csv("Data/Clean/mergedBirth.csv")

df$Female <- ifelse(df$MARSTi ==  0 & df$EDUCATIONi == 0,  df$WTFINL, 0)
df$FemBirth <- ifelse(df$MARSTi ==  0 & df$EDUCATIONi == 0 & df$BIRTHi == 1, df$WTFINL, 0)

agg <- df %>%
  group_by(STATECENSUS, YEAR) %>%
  summarise(nF = sum(Female), 
            nBirth = sum(FemBirth),
            CAPGrp = max(CAPGrp)) %>%
  #express birth rate as births per 1000 women
  mutate(birthRate = 1000*nBirth/nF) %>%
  filter(YEAR > 1981 & YEAR < 2011) %>%
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1))

uci <- c()
lci <- c()
diff <- c()
years <- c(1982:2010)
for (year in years){
  test <- difference_in_means(birthRate ~ TREAT, data = agg[agg$YEAR == year,])
  lci <- c(lci, test$conf.low)
  uci <- c(uci, test$conf.high)
  diff <- c(diff, test$coefficients[[1]])
}


dat <- cbind(years, diff, uci, lci) %>% as.data.frame()

diff <- ggplot(dat, aes(x = years, y = diff)) +
  geom_vline(xintercept = 1992, colour = "grey", linetype = "longdash") +
  geom_hline(yintercept = 0, colour = "grey", linetype = "longdash")+
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin=lci, ymax=uci), width=.2,
                position=position_dodge(.9)) + 
 scale_x_continuous(breaks = seq(1982, 2010, 4), labels = seq(1982, 2010, 4)) +
  scale_y_continuous(breaks = seq(-35, 35, 5), labels = seq(-35, 35, 5)) +
  labs(x = "Year", y = "Difference in births per 1000 women between treated and untreated states") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

#plot actual means 
means <- agg %>%
  group_by(YEAR, TREAT) %>%
  summarise(mean = mean(birthRate)) %>%
  mutate(TREAT = case_when(
    TREAT == 0 ~ "Untreated states",
    TREAT == 1 ~ "Treated states"
  ))

meanPlot <- ggplot(means, aes(x = YEAR, y = mean, group = TREAT, colour = TREAT)) +
  geom_line() +
  scale_colour_manual(values = c("red", "blue")) +
  geom_vline(xintercept = 1992, colour = "grey", linetype = "longdash") +
  labs(x = "Year", y = "Births per 1000 women", colour = "") +
  scale_x_continuous(breaks = seq(1982, 2010, 4), labels = seq(1982, 2010, 4)) +
  scale_y_continuous(breaks = seq(15,60,5), labels = seq(15,60,5)) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())



weighted <- meanPlot / diff

ggsave("Output/Figures/weightedDiffMeans.png", weighted, height = 8, width = 7)



