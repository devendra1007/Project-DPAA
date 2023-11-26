library(readxl)
a1=read.csv("D:/IIT Chicago/Fall 23/DPAA/Project/SP1.csv")
names(a1)[names(a1) == "HomeTeam"] = "Home"
names(a1)[names(a1) == "AwayTeam"] = "Away"
a1$Date=as.Date(a1$Date, format = "%d/%m/%Y")
a2=read.csv("D:/IIT Chicago/Fall 23/DPAA/Project/SP2.csv")
names(a2)[names(a2) == "HomeTeam"] = "Home"
names(a2)[names(a2) == "AwayTeam"] = "Away"
a2$Date=as.Date(a2$Date, format = "%d/%m/%Y")
a3=read.csv("D:/IIT Chicago/Fall 23/DPAA/Project/SPF3.csv")
names(a3)[names(a3) == "HomeTeam"] = "Home"
names(a3)[names(a3) == "AwayTeam"] = "Away"
a3$Date=as.Date(a3$Date, format = "%d/%m/%Y")
a4=read.csv("D:/IIT Chicago/Fall 23/DPAA/Project/SPF4.csv")
names(a4)[names(a4) == "HomeTeam"] = "Home"
names(a4)[names(a4) == "AwayTeam"] = "Away"
a4$Date=as.Date(a4$Date, format = "%d/%m/%Y")
a5=read.csv("D:/IIT Chicago/Fall 23/DPAA/Project/SP5.csv")
names(a5)[names(a5) == "HomeTeam"] = "Home"
names(a5)[names(a5) == "AwayTeam"] = "Away"
a5$Date=as.Date(a5$Date, format = "%d/%m/%Y")


b1=read_excel("D:/IIT Chicago/Fall 23/DPAA/Project/attend1.xlsx")
b1$Date=as.Date(b1$Date, format = "%d-%m-%Y")
b2=read_excel("D:/IIT Chicago/Fall 23/DPAA/Project/attend2.xlsx")
b2$Date=as.Date(b2$Date, format = "%d-%m-%Y")
b3=read_excel("D:/IIT Chicago/Fall 23/DPAA/Project/attend3.xlsx")
b3$Date=as.Date(b3$Date, format = "%d-%m-%Y")
b4=read_excel("D:/IIT Chicago/Fall 23/DPAA/Project/attend4.xlsx")
b4$Date=as.Date(b4$Date, format = "%d-%m-%Y")
b5=read_excel("D:/IIT Chicago/Fall 23/DPAA/Project/attend5.xlsx")
b5$Date=as.Date(b5$Date, format = "%d-%m-%Y")


library(dplyr)
df1=inner_join(a1,b1, by = c("Home", "Date","Away"))
df2=inner_join(a2,b2, by = c("Home", "Date","Away"))
df3=inner_join(a3,b3, by = c("Home", "Date","Away"))
df4=inner_join(a4,b4, by = c("Home", "Date","Away"))
df5=inner_join(a5,b5, by = c("Home", "Date","Away"))



common_cols1=Reduce(intersect, lapply(list(df1, df2, df3, df4, df5), colnames))
df_list1=lapply(list(df1, df2, df3, df4, df5),
                function(x) x[common_cols1])
df=do.call(rbind, df_list1)
df=df[order(df$Date), ]
a1=a1[order(a1$Date), ]
a1$FTHA=c(1,2,0,0,1,2,0,0,0,2,0,2,2,2,0,1,1,0,3,0,1,2,1,0,0,0,1,0,2,0,0,0,2,2,1,1,0,1,3,0,2,3,0,1,0,0,0,1,0,2,0,0,2,3,0,0,0,1,0,1,1,1,2,0,0,2,1,2,5,3,0,0,3,1,0,2,0,2,0,1,2,0,0,0,2,2,0,0,3,0,1,3,0,0,0,1,2,1,1,2,
          0,1,0,0,0,5,0,2,1,0,3,3,4,0,0,1,2,1,2,0,3,0,1,1,1,1,0,4,0,0,0,0,0,1,5,1,0,2,5,1,0,0,0,1,2,0,2,1,0,0,0,3,0,0,0,1,1,1,2,2,2,1,0,0,1,0,0,2,2,0,1,0,1,2,0,0,2,0,0,1,0,0,3,1,1,0,0,0,2,1,0,0,1,0,1,3,1,2,2,0,
          1,1,0,3,1,1,0,2,1,2,0,3,2,1,2,1,0,0,0,0,1,0,0,2,3,4,1,2,0,0,2,2,0,0,0,0,0,2,1,0,1,0,0,3,2,0,0,0,2,1,1,1,1,1,1,1,3,0,4,0,1,1,0,0,2,0,2,1,2,0,0,0,2,0,0,1,2,0,3,0,0,0,0,1,2,0,1,4,0,1,0,1,1,4,0,0,0,0,0,0,
          2,3,0,1,2,0,1,1,1,3,2,3,1,1,2,0,1,2,0,0,0,2,0,0,0,1,1,1,2,3,0,4,1,0,1,2,1,0,1,1,2,0,0,0,1,1,1,0,1,1,0,1,1,1,2,1,0,1,0,2,0,1,2,1,1,2,0,0,0,0,0,2,2,2,3,0,0,2,2,3)
a1$FTAA=c(1,0,0,0,0,1,0,2,0,0,0,0,0,1,1,2,1,2,2,0,1,0,0,0,2,0,1,0,0,1,1,0,1,1,8,1,3,1,1,2,1,0,1,1,0,0,1,0,0,0,1,0,0,0,2,2,0,0,1,1,1,2,1,0,2,0,3,0,0,0,1,1,0,1,4,0,1,0,0,2,0,1,1,1,1,1,0,1,0,1,1,2,1,4,1,0,0,1,2,2,
          0,2,0,0,0,1,1,1,1,0,1,1,0,2,0,2,1,0,1,0,0,1,0,1,2,0,2,0,0,1,0,2,0,0,0,1,1,0,2,1,4,0,0,0,0,1,1,1,0,0,0,0,1,0,0,2,2,1,0,1,1,0,1,0,2,1,0,0,0,0,0,0,0,0,1,2,0,0,0,1,0,1,2,0,0,0,3,2,0,1,1,5,0,0,0,0,2,0,1,0,
          0,1,0,1,0,2,1,0,0,1,0,0,0,1,0,3,2,1,0,0,2,0,1,0,0,0,0,0,0,1,1,0,0,2,1,0,1,2,0,0,0,0,2,2,2,1,0,0,0,0,1,0,0,0,2,0,0,0,1,1,0,0,1,0,1,0,0,0,0,1,2,0,0,0,1,0,1,0,1,1,2,4,1,1,0,1,1,1,0,0,0,0,2,0,1,1,2,1,1,2,
          0,1,1,0,1,1,0,0,1,0,1,0,0,1,1,0,0,0,0,0,1,0,2,1,0,0,3,1,0,0,1,0,0,0,3,0,0,0,1,0,0,0,6,1,3,1,1,2,1,0,0,1,0,0,2,1,1,2,0,0,1,1,1,3,0,0,1,1,1,1,3,0,0,1,0,0,0,3,4,2)

summary(df)
seasons <- character(1900) 

for(i in 1:1900){
  if(i <= 380){
    seasons[i] <- "season1"
  } else if(i <= 760){
    seasons[i] <- "season2" 
  } else if(i <= 1140){
    seasons[i] <- "season3" 
  } else if(i <= 1520){
    seasons[i] <- "season4" 
  } else {
    seasons[i] <- "season5"
  } 
}

df$season <- seasons
colnames(df)
#Q1 Home Win Percentage and Away Team Percentage by Each Team.
library(ggplot2)
df$HomeWin=0
df$HomeWin[df$FTHG > df$FTAG]=1
home_wins = df %>%
  group_by(Home) %>%
  summarize(PctWins = mean(HomeWin)) %>%
  arrange(desc(PctWins))
home_wins[order(home_wins$PctWins,decreasing = TRUE),]

df$AwayWin=0
df$AwayWin[df$FTHG < df$FTAG]=1
away_wins = df %>%
  group_by(Away) %>%
  summarize(PctWins = mean(AwayWin)) %>% 
  arrange(desc(PctWins))
away_wins[order(away_wins$PctWins,decreasing = TRUE),]

combined_df = left_join(home_wins, away_wins, by = c("Home" = "Away"))
combined_df <- combined_df[order(-combined_df$PctWins.x), ]
ggplot(combined_df, aes(x=Home, y=PctWins.x)) +
  geom_col() +
  geom_col(aes(y = PctWins.y), fill="blue") +
  geom_text(aes(y = PctWins.x, label = round(PctWins.x,2)), vjust=-0.5, size=3) +
  geom_text(aes(y = PctWins.y, label = round(PctWins.y,2)), vjust=-0.5, size=3) +
  labs(title="Home and Away Win Percentage by Team") +
  xlab("Team") + 
  ylab("Win Percentage") +
  annotate("text", x = Inf, y = Inf, label = "Blue bars: Away win % \n Grey bars: Home win %",
           hjust = 1, vjust = 1, color = "black")


#Q2 Does the Attendance affect the results the result of the Away Team.
q3=df[,c("Attendance","Home","FTAG","FTHG")]
q3=data.frame(q3)
summary(q3)
home_teams = unique(q3$Home)
for(i in home_teams) {
  team_df = subset(q3, Home == i)
  team_attn = team_df$Attendance
  team_agoals = team_df$FTAG
  team_hgoals = team_df$FTHG
  plot(team_attn, team_agoals, 
       main = paste("Team", i))
  abline(lm(team_agoals ~ team_attn), col='red')
  plot(team_attn, team_hgoals, 
       main = paste("Team", i))
  abline(lm(team_hgoals ~ team_attn), col='green')
}

home_teams

#Q3 Build a model to estimate win probability a match based on Halftime score, Halftime result, and other situational factors (Red and Yellow Cards) etc.
library(randomForest)
library(tidyverse)
library(caTools)
library(caret)

q4 = df[, c("Home","Away","FTHG", "FTAG", "FTR", "HTHG", "HTAG", "HTR", "HS", "AS", "HST", "AST", "HF", "AF", "HC", "AC", "HY", "AY", "HR", "AR")]
colnames(df)
q4 = data.frame(q4)
q4$FTR = as.factor(q4$FTR)


str(q4)
set.seed(123)
split1 = sample.split(q4$Home, SplitRatio = 0.9)
dftrain2 = subset(q4, split1 == TRUE)
dftest2 = subset(q4, split1 == FALSE)
rf_home_model = randomForest(FTR ~ Home +HTHG + HTAG + HS + AS + HST + AST + HF + AF + HC + AC + HY + AY + HR + AR +HTR, data = dftrain2, ntree=1000)
rf_away_model = randomForest(FTR ~ Away +HTHG + HTAG + HS + AS + HST + AST + HF + AF + HC + AC + HY + AY + HR + AR +HTR, data = dftrain2, ntree=1000) 

#Predictions
predictions_home = predict(rf_home_model, newdata = dftest2)
predictions_away = predict(rf_away_model, newdata = dftest2)
actual_outcomes=dftest2$FTR
confusion_matrix = table(actual_outcomes, predictions_home)
cm = caret::confusionMatrix(confusion_matrix)
cm
confusion_matrix1 = table(actual_outcomes, predictions_away)
cm1 = caret::confusionMatrix(confusion_matrix1)
cm1



#Q4 Do Solo goals are effective, or the assisted ones impact the most.
#Analyze solo vs assisted goals
library(dplyr)
library(tidyverse)
# Create new columns for solo and assisted goals
a1 = a1 %>%
  mutate(Home_Solo = FTHG - FTHA, 
         Home_Assisted = FTHA,
         Away_Solo = FTAG - FTAA,
         Away_Assisted = FTAA)


# Overall stats
a1 %>% 
  summarise(Total_Goals = sum(FTHG + FTAG),
            Solo_Goals = sum(Home_Solo + Away_Solo),
            Assisted_Goals = sum(Home_Assisted + Away_Assisted),
            Pct_Solo = Solo_Goals/Total_Goals,
            Pct_Assisted = Assisted_Goals/Total_Goals)

# Per team stats
a1 %>%
  group_by(Home) %>%
  summarise(Total = sum(FTHG), 
            Solo = sum(Home_Solo),
            Assisted = sum(Home_Assisted),
            Pct_Solo = Solo/Total,
            Pct_Assisted = Assisted/Total) %>%
  arrange(Total)

# Same for Away team
a1 %>%
  group_by(Away) %>%
  summarise(Total = sum(FTAG),
            Solo = sum(Away_Solo),
            Assisted = sum(Away_Assisted),  
            Pct_Solo = Solo/Total,
            Pct_Assisted = Assisted/Total) %>%
  arrange(Total)

# Calculate percentages
a1 = a1 %>%
  group_by(Home) %>%
  mutate(Pct_Solo = Home_Solo/FTHG, 
         Pct_Assisted = Home_Assisted/FTHG) 
a1 = a1 %>%  
  group_by(Away) %>%
  mutate(Pct_Solo = Away_Solo/FTAG,
         Pct_Assisted = Away_Assisted/FTAG)

# Gather home and away summaries
team_stats = bind_rows(
  a1 %>% 
    summarise(Team = Home, 
              Total = sum(FTHG),
              Solo = sum(Home_Solo),
              Assisted = sum(Home_Assisted)) %>%
    gather(Type, Value, Solo:Assisted),
  
  a1 %>%
    summarise(Team = Away,
              Total = sum(FTAG),
              Solo = sum(Away_Solo),
              Assisted = sum(Away_Assisted)) %>%
    gather(Type, Value, Solo:Assisted)  
)

# Bar chart per team
ggplot(team_stats, aes(x=Team, y=Value, fill=Type)) +
  geom_bar(stat="identity", position="stack")


#Q5 Which Betting System gives you almost guaranteed results. Compare b/w them.
odds_data = df[c("B365H","B365D","B365A","BWH","BWD","BWA","IWH","IWD","IWA","PSH","PSD","PSA","WHH","WHD","WHA","VCH","VCD","VCA")]
odds_data=data.frame(odds_data)
teams = c(unique(df$Home))
teams=data.frame(teams)
best_odds = data.frame(team = teams$team, 
                       Home = character(nrow(teams)),
                       Draw = character(nrow(teams)),
                       Away = character(nrow(teams)))
for(i in 1:nrow(teams)){
  team = teams$team[i]
  home_cols = grep("H$", colnames(odds_data))
  best_odds$Home[i] = names(which.min(odds_data[i, home_cols]))
  draw_cols = grep("D$", colnames(odds_data))
  best_odds$Draw[i] = names(which.min(odds_data[i, draw_cols]))
  away_cols = grep("A$", colnames(odds_data))
  best_odds$Away[i] = names(which.min(odds_data[i, away_cols]))
}
print(best_odds)
library(ggplot2)
library(tidyr)
best_odds_long <- pivot_longer(best_odds, cols = c("Home", "Draw", "Away"), names_to = "Outcome", values_to = "Odds")
ggplot(best_odds_long, aes(x = team, y = Odds, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Odds for Different Outcomes by Team",
       x = "Team",
       y = "Odds") +
  theme_minimal()

#Q6 Do a deeper dive on home advantage analyze performance over time, against different opponents.

data = df
library(lubridate)
data$Date = as.Date(data$Date)
data$Year = year(data$Date)

# Home win percentage over time
library(dplyr)
q6=data %>%
  group_by(Home) %>%
  summarize(HomeWinPct = mean(FTR == "H")) %>%
  arrange(desc(HomeWinPct))
print(q6)

# Home win,Away Win and Draw percentage by team strength  
q61=data %>%
  group_by(Home) %>%
  summarize(HomeWinPct = mean(FTR == "H"),AwayWinPct = mean(FTR == "A"),DrawPct = mean(FTR == "D"), 
            AvgHomeGoalsH = mean(FTHG[FTR == "H"]),AvgHomeGoalsA = mean(FTHG[FTR == "A"]),AvgHomeGoalsD = mean(FTHG[FTR == "D"]),
            AvgAwayGoalsH = mean(FTAG[FTR == "H"]),AvgAwayGoalsA = mean(FTAG[FTR == "A"]),AvgAwayGoalsD = mean(FTAG[FTR == "D"])) %>%
  arrange(desc(HomeWinPct))
print(q61)

#Visualization
library(ggplot2)
dataplot = data.frame(
  Team = c(unique(data$Home)),
  HomeWinPct = q61$HomeWinPct,
  AwayWinPct = q61$AwayWinPct,
  DrawPct = q61$DrawPct
)

# Melt the data frame to make it suitable for pie charts
library(reshape2)
dataplot_melted <- melt(dataplot, id.vars = "Team")

# Create a pie chart for each team
ggplot(dataplot_melted, aes(x = 1, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  facet_wrap(~Team) +
  theme_void() +
  labs(title = "Home vs Away vs Draw Win Percentage", 
       x = NULL, y = NULL) +
  scale_fill_manual(values = c("blue", "red", "green")) +
  geom_text(aes(label = paste0(round(value, 2) * 100, "%")), position = position_stack(vjust = 0.5))


# Home win percentage by Year and its visualization
q63=data %>%
  group_by(Home,Year) %>%
  summarize(HomeWinPct = mean(FTR == "H"),
            AvgHomeGoals = mean(FTHG),
            AvgAwayGoals = mean(FTAG)) %>%
  arrange(desc(HomeWinPct))
print(q63)


# Plot home advantage over time
unique_teams = unique(data$Home)
plots = list()

# Loop through each team and create a plot
for (team in unique_teams) {
  yearly_data = data %>%
    filter(Home == team) %>%
    group_by(Year) %>%
    summarize(HomeWinPct = mean(FTR == "H"))
  
  p = ggplot(yearly_data, aes(x = Year, y = HomeWinPct)) +
    geom_line() +
    labs(
      title = paste("Home Win Percentage Over Time for Team", team),
      x = "Year",
      y = "Win Percentage"
    )
  
  plots[[team]] = p
}

for (team in unique_teams) {
  print(plots[[team]])
}



# Q7 Team wise Home Win Analysis

unique_teams = unique(data$Home)

opponents = unique_teams

results = data.frame(data$Home, data$Away, data$FTR)

# Plot for each team

par(mfrow=c(1,1))

for(team in unique(unique_teams)) {
  
  # Filter to this home team
  
  team_results = results[results$data.Home==team,]
  
  # Wins vs each opponent
  
  opp_wins = table(team_results$data.Away[team_results$data.FTR=="H"])
  
  total_wins = sum(opp_wins)
  
  total_matches = nrow(team_results)
  
  # labels
  
  labels = paste(names(opp_wins), "\nWins:", opp_wins)
  
  # Pie chart
  
  pie(opp_wins, labels=labels, main=paste( "Total Home Wins by:",team, "\nWins:", total_wins, "\nTotal Matches:", total_matches))
  
  # Legend if >= 2 opponents
  
  if(length(opp_wins) >= 2) {
    
    legend("topright", legend=names(opp_wins), fill=seq(1, length(opp_wins)), cex=0.0000001, x=1.0, y=1.0)
    
  }
  
}

