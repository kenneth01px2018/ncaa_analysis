library(tidyverse)
library(caret)
library(usmap)
library(ggforce)
library(ggdark)

# Import college data
ncaa22 <- read_csv("Desktop/NBA Projects/NCAA Project/Data/ncaa22.csv")
ncaa22 <- ncaa22 %>% # Remove first column
  select(-c(`...1`))

# Import conference data
conf22 <- read_csv("Desktop/NBA Projects/NCAA Project/Data/conf22.csv")
extract_state <- function(x) { # Function to extract the state of the college
  str_extract(x, "[:upper:]{2}")
}
conf22 <- conf22 %>% # Extract important columns
  mutate(State = extract_state(Location)) %>% # DC is included as a state, Alaska is not (total = 50)
  mutate(Team = School) %>%
  select(Team, Conference, State) # 32 conferences
View(conf22)

# Join data frames
ncaa22 <- ncaa22 %>%
  left_join(conf22, by = "Team")
View(ncaa22)

# Visualize Win %
ggplot(ncaa22, aes(x = `Win %`)) + 
  geom_histogram(aes(y = ..density..), colour = "#00BFC4", fill = "#00BFC4", alpha = 0.5)+
  geom_density(alpha = .2, fill = "#00BFC4", color = "#00BFC4") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  labs(
    title = "Distribution of Win Percentage",
    subtitle = "For the 2021-2022 NCAA D1 Season"
  ) + ylab("Density")
summary(ncaa22$`Win %`)

# See which variables are most correlated with win %
ncaa22 %>%
  select(-c(Team, Conference, State, `W's`, `L's`, `Win %`)) %>%
  cor(ncaa22 %>% select(`Win %`)) %>% abs() %>% round(3) %>%
  data.frame() %>%
  arrange(desc(`Win..`))

# Teams with highest win %
ncaa22 %>%
  arrange(desc(`Win %`)) %>%
  slice(1:20) %>%
  select(Team, Conference, `Win %`)
ncaa22 %>%
  arrange(desc(`Win %`)) %>%
  slice(1:20) %>%
  mutate("Win %" = round(`Win %`, 3)) %>%
  ggplot(aes(x = reorder(Team, `Win %`), y = `Win %`, fill = `Win %`, color = `Win %`)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "palegreen2", high = "palegreen4") + 
  scale_color_gradient(low = "palegreen2", high = "palegreen4", guide = NULL) + 
  geom_text(aes(label = `Win %`),  hjust = 1.3, color = "white", size = 5) + coord_flip(ylim = c(0.7, 0.95)) +
  labs(subtitle = "For the 2021-2022 NCAA D1 Season", 
       y = "Win Percentage", 
       x = "Team", 
       title = "Teams with the Highest Win Percentage") + theme_fivethirtyeight()

# Teams with highest pace
ncaa22 %>%
  arrange(desc(Pace)) %>%
  slice(1:10) %>%
  select(Team, Conference, `Win %`, Pace)

# Teams with highest ORtg
ncaa22 %>%
  arrange(desc(ORtg)) %>%
  slice(1:10) %>%
  select(Team, Conference, `Win %`, ORtg)

# Teams with lowest DRtg
ncaa22 %>%
  arrange(DRtg) %>%
  slice(1:10) %>%
  select(Team, Conference, `Win %`, DRtg)

# Teams with highest eDiff
ncaa22 %>%
  arrange(desc(eDiff)) %>%
  slice(1:10) %>%
  select(Team, Conference, `Win %`, eDiff)

# Conferences by avg. win %
ncaa22 %>%
  group_by(Conference) %>%
  summarize(`Avg. Win %` = mean(`Win %`)) %>%
  arrange(desc(`Avg. Win %`))
ncaa22 %>% # Bar plot
  group_by(Conference) %>%
  summarize(`Avg. Win %` = round(mean(`Win %`), 3)) %>%
  ggplot(aes(x = reorder(Conference, `Avg. Win %`), y = `Avg. Win %`, fill = `Avg. Win %`)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightskyblue", high = "royalblue4", label = NULL) + 
  geom_text(aes(label = `Avg. Win %`),  hjust = 1.3, color = "white", size = 5) + coord_flip(ylim = c(0.35, 0.65)) +
  labs(subtitle = "For the 2021-2022 NCAA D1 Season", 
       y = "Average Win Percentage", 
       x = "Conference", 
       title = "Average Win Percentage by Conference") + theme_fivethirtyeight()
ncaa22 %>% # Top conference: Big 12
  filter(Conference == "Big 12") %>%
  select(Team, `Win %`)

# Conferences by win % sd.
ncaa22 %>%
  group_by(Conference) %>%
  summarize(`Win % SD.` = sd(`Win %`)) %>%
  arrange(desc(`Win % SD.`))
nbar <- length(unique(ncaa22$Conference)) # Label polar coords for circular bar plot
angle <-  90 - 360 * (1:nbar - 0.5) / nbar
hjust <- ifelse(angle < -90, 1, 0)
angle<-ifelse(angle < -90, angle + 180, angle)
ncaa22 %>% # Circular bar plot
  group_by(Conference) %>%
  summarize(`Win % SD.` = sd(`Win %`)) %>%
  ggplot(aes(x = Conference, y = `Win % SD.`, fill = `Win % SD.`)) + 
  geom_bar(stat = "identity") + 
  ylim(-0.1, 0.25) + 
  theme_classic() +
  scale_fill_gradient(low = "tan1", high = "firebrick") +
  labs(title = "Win Percentage Standard Deviation by Conference",
       subtitle = "For the 2021-2022 NCAA D1 Season") +
  geom_text(aes(x = Conference, y = `Win % SD.` + 0.01, label = Conference, hjust = hjust), angle = angle) +
  coord_polar(start = 0) +
  theme(
    plot.title = element_text(vjust = -15, hjust = 0.1),
    plot.subtitle = element_text(vjust = -18, hjust = 0.07),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
ncaa22 %>% # Large variation: OVC
  filter(Conference == "OVC") %>%
  select(Team, `Win %`)
ncaa22 %>% # Small variation: Big 12
  filter(Conference == "Big 12") %>%
  select(Team, `Win %`)

# Avg win % by state
df <- ncaa22 %>%
  group_by(State) %>%
  summarize(Avg_Win = mean(`Win %`), Count = n()) %>%
  mutate(abbr = as.factor(State)) %>%
  left_join(statepop, by = "abbr") %>%
  select(fips, abbr, full, Avg_Win, Count)
df %>% # Look at states with more than 5 schools
  filter(Count >= 5) %>%
  arrange(desc(Avg_Win))
plot_usmap(data = df, values = "Avg_Win", color = "black", labels = TRUE, label_color = "white") +
    scale_fill_continuous(low = "lightblue", high = "royalblue4", name = "Avg. Win %") +
  theme_fivethirtyeight() +
  theme(
    axis.text = element_blank()
    ) +  
  labs(title = "Average Win Percentage by State",
       subtitle = "For the 2021-2022 NCAA D1 Season") 

# K-means clustering
set.seed(24)
cluster_df <- ncaa22 %>% # Create df for kmeans
  select(ORtg, DRtg, `3PA`) %>% as.data.frame()
rownames(cluster_df) <- ncaa22$Team
kmeans_wss <- numeric(15) # Find optimal number of clusters (k)
for (i in 1:15) {
  temp <- kmeans(cluster_df, i, nstart = 50)
  kmeans_wss[i] <- temp$tot.withinss
}
plot(1:15, kmeans_wss, type = "b") # k = 4 seems optimal
k4_out <- kmeans(cluster_df, 4, nstart = 50)
cluster_df2 <- tibble(Team = names(k4_out$cluster), Cluster = as.factor(k4_out$cluster)) # Save clusters to tibble
cluster_final <- ncaa22 %>% # Join to original data
  left_join(cluster_df2, by = "Team")

# Plot clusters
lbls <- c("20.5 3PAs", "24 3PAs", "20.4 3PAs", "22.9 3PAs")
lbls <- lbls[k4_out$cluster]
ggplot(cluster_final, aes(x = ORtg, y = DRtg, fill = Cluster)) + geom_point(size = 5, shape = 21, color = "black") + 
  scale_fill_manual(values = c('deepskyblue', 'royalblue4', 'lightblue1', "dodgerblue")) + theme_clean() + 
  theme(axis.title = element_text(), legend.position = "right", legend.direction = "vertical") + 
  xlab("Offensive Rating") + 
  ylab("Defensive Rating") + 
  labs(title = "Offensive Rating vs Defensive Rating",
       subtitle = "Clustered using K-Means") + geom_mark_hull(aes(fill = Cluster, label = lbls), concavity = 2) + ylim(85, 120) + xlim(80, 125)

# Alternative cluster plot
x_centers <- k4_out$centers[, 1]
y_centers <- k4_out$centers[, 2]
cluster_final <- cluster_final %>%
  mutate(x_center = x_centers[k4_out$cluster], y_center = y_centers[k4_out$cluster])
lbls2 <- c("20.5 3PAs", "24 3PAs", "20.4 3PAs", "22.9 3PAs")
cols <- c('deepskyblue', 'royalblue4', 'lightblue1', "dodgerblue")
cluster_final %>%
  ggplot(aes(fill = Cluster)) +
  geom_segment(aes(x = ORtg, xend = x_center, y = DRtg, yend = y_center, color = Cluster), lty = "solid", lwd = 1.5, alpha = 0.5) +
  geom_point(aes(x_center, y_center, color = Cluster), size = 10) +
  geom_point(aes(ORtg, DRtg, color = Cluster), show.legend = FALSE, size = 5) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) + 
  dark_theme_gray() +
  theme(axis.title = element_text(), legend.position = "right", legend.direction = "vertical") + 
  xlab("Offensive Rating") + 
  ylab("Defensive Rating") + 
  labs(title = "Offensive Rating vs Defensive Rating",
       subtitle = "Clustered using K-Means") + 
  geom_text(
    x = 88, y = 110, label = lbls2[1], color = cols[1], size = 6, family = "Helvetica"
  ) + 
  geom_text(
    x = 113, y = 110, label = lbls2[2], color = cols[2], size = 6, family = "Helvetica"
  ) + 
  geom_text(
    x = 93, y = 91, label = lbls2[3], color = cols[3], size = 6, family = "Helvetica"
  ) + 
  geom_text(
    x = 115, y = 96, label = lbls2[4], color = cols[4], size = 6, family = "Helvetica"
  )

# Visualize eDiff
ggplot(ncaa22, aes(x = eDiff)) + 
  geom_histogram(aes(y = ..density..), colour = "darkorange2", fill = "darkorange2", alpha = 0.5)+
  geom_density(alpha = .2, fill = "darkorange2", color = "darkorange2") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  labs(
    title = "Distribution of Net Rating",
    subtitle = "For the 2021-2022 NCAA D1 Season"
  ) + ylab("Density") + xlab("Net Rating")

# eDiff vs Win %
ggplot(ncaa22, aes(x = eDiff, y = `Win %`)) +
  geom_point(size = 4, shape = 21, color = "black", fill = "darkgray") + 
  geom_smooth(method = "lm", color = "#3366FF") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  xlab("Net Rating") + 
  labs(title = "Net Rating vs Win Percentage",
       subtitle = "Correlation: 0.934") 
summary(lm(`Win %` ~ eDiff, data = ncaa22)) # Regression model

# eDiff model
eDiff_mdl <- lm(eDiff ~ PPG + FGM + FTM + FGA + FTA + DRB + ORB + APG + SPG + BPG + PF + TOV, data = ncaa22)
summary(eDiff_mdl)

# Create Team Efficiency Rating (TER) variable
ncaa22 <- ncaa22 %>%
  mutate(
    TER = scale(1.5 * PPG - 3 * FGA - 1 * FTA + 2 * DRB + 3 * ORB + 3 * SPG + 0.5 * BPG + 0.5 * PF - 3 * TOV)
  )

# Team with highest TER
ncaa22 %>%
  arrange(desc(TER)) %>%
  slice(1:20) %>%
  select(Team, Conference, `Win %`, TER)

# Visualize TER
ggplot(ncaa22, aes(x = TER)) + 
  geom_histogram(aes(y = ..density..), colour = "#F8766D", fill = "#F8766D", alpha = 0.5)+
  geom_density(alpha = .2, fill = "#F8766D", color = "#F8766D") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  labs(
    title = "Distribution of Team Efficiency Rating",
    subtitle = "For the 2021-2022 NCAA D1 Season"
  ) + ylab("Density") + xlab("Team Efficiency Rating")
summary(ncaa22$TER)

# TER vs Win %
mdl <- lm(`Win %` ~ TER, data = ncaa22)
summary(mdl)
plot(mdl) # model diagnostics
ggplot(ncaa22, aes(x = TER, y = `Win %`)) +
  geom_point(size = 4, shape = 21, color = "black", fill = "azure3") + 
  geom_smooth(method = "lm", color = "goldenrod") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  xlab("Team Efficiency Rating") + 
  labs(title = "Team Efficiency Rating vs Win Percentage",
       subtitle = "Correlation: 0.893") 

# Save final data frame
ncaa22 %>%
  mutate(Cluster = k4_out$cluster) %>%
  write.csv(file = "Desktop/NBA Projects/NCAA Project/Data/ncaa_final22.csv") 

