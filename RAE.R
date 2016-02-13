#---------------- Setup ----------------#
# Source functions
source("~/Classes/DSS/Functions.R")

# Load packages
library("rvest")
library("lubridate")
library("ggplot2")
library("xtable")
library("scales")
library("dplyr")

#---------------- U17 Mens ----------------#
#---------------- Extract player data ----------------#
url_u17_mens <- "https://en.wikipedia.org/wiki/2015_FIFA_U-17_World_Cup_squads"

# get all players from all teams
teams <- data.frame()
page <- read_html(url_u17_mens)

for (i in 1:24) {
  data <-
    html_nodes(page, xpath = paste('//*[@id="mw-content-text"]/table[',i,']'))
  team <- html_table(data, fill = TRUE, header = FALSE)
  team <- team[[1]]
  team <- team[3:nrow(team), 1:7]
  teams <- rbind(teams, team)
}

# rename the data frame columns
colnames(teams) <- c('Num', 'Position', 'Player', 'DOB', 'Caps', 'Goals', 'Club')

#---------------- Extract team names for all teams ----------------#
team.names <- data.frame(Team = character(),stringsAsFactors=FALSE)

for (i in 1:24) {
  data <-
    html_nodes(page, xpath = paste('//*[@id="mw-content-text"]/h3[',i,']'))
  for (j in 1:21) {
    team.name <-
      as.data.frame(html_text(data), stringsAsFactors = FALSE)
    print(team.name)
    team.names <- rbind(team.names, team.name)
  }
}
  
# rename the data frame column
colnames(team.names) <- 'Team'

# clean up team names
team.names <- as.data.frame(sapply(team.names, trim), stringsAsFactors = FALSE)
team.names <- as.data.frame(sapply(team.names,gsub,pattern="[edit]",replacement="", fixed = TRUE))

# Join the team names and teams data frames
data <- cbind(team.names, teams)

# Clean up birthdate
data$DOB <- substr(data$DOB, 2, 11)
data$DOBYM <- format(as.Date(data$DOB), format="%Y %b") 

# Get last day of month for aggregating
data$LDOM <- last_day(as.Date(data$DOB))

#---------------- Create barplot of frequencies ALL TEAMS ----------------#
bdayprop <- data.frame(prop.table(table(sort(data$LDOM))))
colnames(bdayprop) <- c("Birth_Month", "Pct_Total")

bplt <- barplot(bdayprop$Pct_Total,
        main = "2016 U17 World Cup \n%of players by birth month - ALL TEAMS",
        cex.main = 0.8,
        cex.lab = 0.8,
        axes = FALSE,
        # kill axis names so they can be added back closer
        axisnames = FALSE,
        las=2,
        cex.names=0.65,
        col = "steelblue",
        border = NA,
        ylim = c(0,max(bdayprop$Pct_Total)+0.02)
        )

# add data labels to bars
text(x = bplt, y = bdayprop$Pct_Total, label = round(bdayprop$Pct_Total*100,2),
     pos = 3, cex = 0.3, col = "black")

# add the fair share line 
abline(h=100/12/100,col=4,lty=2)

# add fair share label
text( x = 26, y = 0.083, label = "Single year \n% fair share", col = "gray48", cex = 0.9)

# add the x axis labels
mtext(text = format(as.Date(bdayprop$Birth_Month), format = "%b %Y"),
      side = 1, at = bplt, line = 0.25, las = 2, cex = 0.6)

#---------------- Create barplot of frequencies USA ----------------#
# limit months to target country
trgt.team <- 'Mexico'
one.only <- subset(data, Team == trgt.team, 
                  select=c(LDOM))

# sequence of two years of month ends 
date.list <- as.data.frame(as.factor(seq(as.Date("1998-02-01"), length=24, by="1 month") - 1))
colnames(date.list) <- c("Birth_Month")

# proportions of birthdays
one.team.bdayprop <- data.frame(prop.table(table(sort(one.only$LDOM))))
colnames(one.team.bdayprop) <- c("Birth_Month", "Pct_Total")

# join proportions to month ends
team.prop <- left_join(date.list,one.team.bdayprop,by="Birth_Month")
team.prop$Birth_Month <- as.factor(team.prop$Birth_Month)

bplt <- barplot(team.prop$Pct_Total,
                main = paste("2016 U17 World Cup \n%of players by birth month - ", trgt.team),
                cex.main = 0.8,
                cex.lab = 0.8,
                axes = FALSE,
                # kill axis names so they can be added back closer
                axisnames = FALSE,
                las=2,
                cex.names=0.65,
                col = "steelblue",
                border = NA,
                ylim = c(0,max(bdayprop$Pct_Total)+0.2)
)

# add data labels to bars
text(x = bplt, y = team.prop$Pct_Total, label = round(team.prop$Pct_Total*100,2),
     pos = 3, cex = 0.3, col = "black")

# add the fair share line 
abline(h=100/12/100,col=4,lty=2)

# add fair share label
text( x = 26, y = 0.083, label = "Single year \n% fair share", col = "gray48", cex = 0.9)

# add the x axis labels
mtext(text = format(as.Date(team.prop$Birth_Month), format = "%b %Y"),
      side = 1, at = bplt, line = 0.25, las = 2, cex = 0.6)

png(paste(trgt.team,".png"), width=6, height=6, units="in", res=300)


#---------------- Mens ----------------#
#---------------- Extract player data ----------------#
url_mens <- "https://en.wikipedia.org/wiki/2014_FIFA_World_Cup_squads"

# get all players from all teams
teams <- data.frame()
page <- read_html(url_mens)

for (i in 1:32) {
  data <-
    html_nodes(page, xpath = paste('//*[@id="mw-content-text"]/table[',i,']'))
  team <- html_table(data, fill = TRUE, header = FALSE)
  team <- team[[1]]
  team <- team[3:nrow(team), 1:6]
  teams <- rbind(teams, team)
}

# rename the data frame columns
colnames(teams) <- c('Num', 'Position', 'Player', 'DOB', 'Caps', 'Club')

#---------------- Extract team names for all teams ----------------#
team.names <- data.frame(Team = character(),stringsAsFactors=FALSE)

for (i in 1:32) {
  data <-
    html_nodes(page, xpath = paste('//*[@id="mw-content-text"]/h3[',i,']'))
  for (j in 1:23) {
    team.name <-
      as.data.frame(html_text(data), stringsAsFactors = FALSE)
    print(team.name)
    team.names <- rbind(team.names, team.name)
  }
}

# rename the data frame column
colnames(team.names) <- 'Team'

# clean up team names
team.names <- as.data.frame(sapply(team.names, trim), stringsAsFactors = FALSE)
team.names <- as.data.frame(sapply(team.names,gsub,pattern="[edit]",replacement="", fixed = TRUE))

# Join the team names and teams data frames
data <- cbind(team.names, teams)

# Clean up birthdate
data$DOB <- substr(data$DOB, 2, 11)
data$DOBYM <- format(as.Date(data$DOB), format="%Y %b") 

# Get last day of month for aggregating
data$LDOM <- last_day(as.Date(data$DOB))

#---------------- Create barplot of frequencies ALL TEAMS ----------------#
bdayprop <- data.frame(prop.table(table(format(sort(data$LDOM),"%m"))))
colnames(bdayprop) <- c("Birth_Month", "Pct_Total")
bdayprop$Birth_Month <- month.abb[bdayprop$Birth_Month] 

bplt <- barplot(bdayprop$Pct_Total,
                main = "2014 World Cup \n%of players by birth month - ALL TEAMS",
                cex.main = 0.8,
                cex.lab = 0.8,
                axes = FALSE,
                # kill axis names so they can be added back closer
                axisnames = FALSE,
                las=2,
                cex.names=0.65,
                col = "steelblue",
                border = NA,
                ylim = c(0,max(bdayprop$Pct_Total)+0.02)
)

# add data labels to bars
text(x = bplt, y = bdayprop$Pct_Total, label = round(bdayprop$Pct_Total*100,2),
     pos = 3, cex = 0.6, col = "black")

# add the fair share line 
abline(h=100/12/100,col=4,lty=2)

# add fair share label
text( x = 13, y = 0.083, label = "Single year \n% fair share", col = "gray48", cex = 0.9)

# add the x axis labels
mtext(text = bdayprop$Birth_Month,
      side = 1, at = bplt, line = 0.25, las = 2, cex = 0.6)

