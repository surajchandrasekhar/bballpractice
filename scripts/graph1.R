library(dplyr)
library(plotly)
#This Scatter Plot will compare the ppg of the leading scorer of each team with a selected efficiency category the user chooses
#create function BuildGraph1, with 2 parameters: the data set and the variable the user wants to compare with
#use dplyr to filter out only the leading scorer of each team
#create plotly scatter plot with x as Points per game, and y variable as the user selected variable, and as size being the number of wins the team had
#display the important information, like player name, team name, team wins, and other information you find necessary
# theres a string literal here 
BuildGraph1 <- function(statvar, xvar ='PS.G') {
  pergame <- paste0("./data/","2015-16","playerpergame.csv")
  advanced <- paste0("./data/","2015-16","advanced.csv")
  team <- paste0("./data/","2015-16","team.csv")
  player.stats <- read.csv(pergame)
  advanced.stats <- read.csv(advanced)
  team.stats <- read.csv(team)
  player.joined <- left_join(player.stats,advanced.stats, by=c("Player","Tm"))
  dataset <- left_join(player.joined,team.stats, by="Tm")
  newdata <- filter(dataset, G.x > 41) %>% 
    group_by(Tm) %>% 
    filter(PS.G == max(PS.G))
  x.equation = paste0("~",xvar)
  y.equation = paste0("~",statvar)
  p <- (ggplot(newdata,
              aes(x=PS.G, y=eval(parse(text = statvar))))
              + geom_point(aes(color=Team, size=W)) 
              + geom_vline(aes(xintercept=mean(xvar)), color="black") 
              + geom_hline(aes(yintercept=mean(FG.), color="blue"))
 )
  return (p)
}
