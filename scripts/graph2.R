#Create scatter plot That shows teams success of offense vs defense
#Create function BuildGraph2 with 2 parameters: the data set and the search value
#Create plotly scatter plot with offensive rating in x and defensive rating as y, with the size of the plot points as the team wins
# display necesssary information when it hovers, such as team name, offensive rating, defensive rating, and team wins
# display information such that the user types in a fragment and plot will show all points that match what the user types in
library(plotly)
library(dplyr)

BuildGraph2 <- function(search = "") {
  team.stats <- paste0("./data/","2015-16","team.csv")
  dataset <- read.csv(team.stats)
  dataset <- dataset %>% filter(grepl(search, Team))
  x.equation <- paste0("~", "ORtg")
  y.equation <- paste0("~", "DRtg")
  
  
  graph <- ggplot(team.stats, aes(x=ORtg, y=DRtg)) 
            + geom_point(aes(color=team, size=W, 
                             text = ~paste('Team Name: ', team,
                             '</br> Offensive Rating: ', ORtg,
                             '</br> Deffensive Rating: ', DRtg,
                             '</br> Wins: ', W))) 
            + geom_vline(aes(xintercept=mean(ORtg)), color="black") 
            + geom_hline(aes(yintercept=mean(DRtg)), color="blue")

  return (graph)
}



