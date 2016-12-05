library(shiny)
library(plotly)
library(dplyr)

source('./scripts/graph1.R')
source('./scripts/graph2.R')
source('./scripts/graph3.R')

pergame <- paste0("./data/","2006-07","playerpergame.csv")
advanced <- paste0("./data/","2006-07","advanced.csv")
team <- paste0("./data/","2006-07","team.csv")
player.stats <- read.csv(pergame)
advanced.stats <- read.csv(advanced)
team.stats <- read.csv(team)
player.joined <- left_join(player.stats,advanced.stats, by=c("Player","Tm"))
dataset <- left_join(player.joined,team.stats, by="Tm")
newdata <- filter(dataset, G.x > 41) %>% 
  group_by(Tm) %>% 
  filter(PS.G == max(PS.G))

# Start shinyServer
shinyServer(function(input, output) { 
  
  # Render a plotly object that returns your map
  output$scatter1 <- renderPlot({
    ggplot(newdata,aes(x=PS.G, y=eval(parse(text = input$statvar)))) + geom_point(aes(color=Team, size=W)) + geom_vline(aes(xintercept=mean(PS.G)), color="black") + geom_hline(aes(yintercept=mean(FG.), color="blue"))
  }) 
  output$hover_info <- renderPrint({
    
  })
  output$scatter2 <- renderPlotly({
    #team.stats <- read.csv(paste0("./data/2015-16team.csv"))
    return(BuildGraph2(input$year2, input$search))
  })
  
  output$scatter3 <- renderPlotly({
    player.stats <- read.csv(paste0("./data/2015-16playerpergame.csv"))
    advanced.stats <- read.csv(paste0("./data/2015-16advanced.csv"))
    player.joined <- left_join(player.stats,advanced.stats, by=c("Player","Tm"))
    return(BuildGraph3(player.joined, input$dstat,input$qty))
  })
})