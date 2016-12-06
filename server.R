library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
source('./scripts/graph1.R')
source('./scripts/graph2.R')
source('./scripts/graph3.R')

pergame <- paste0("./data/","2015-16","playerpergame.csv")
advanced <- paste0("./data/","2015-16","advanced.csv")
team <- paste0("./data/","2015-16","team.csv")
player.stats <- read.csv(pergame)
advanced.stats <- read.csv(advanced)
team.stats <- read.csv(team)
player.joined <- left_join(player.stats,advanced.stats, by=c("Player","Tm"))
dataset <- left_join(player.joined,team.stats, by="Tm")

# Start shinyServer
shinyServer(function(input, output) { 
  
  # Render a plotly object that returns your map
  output$scatter1 <- renderPlot({
    return(BuildGraph1(input$statvar))
  }) 

  output$text1 <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n") 
      paste0("Points Per Game=", round(e$x, 1), "\nField Goal %", round(e$y, 1))
    }
    xy_str(input$plot_hover)
  })
  output$scatter2 <- renderPlot({
    return(BuildGraph2(input$search))
  })
  
  output$scatter3 <- renderPlot({
   filtered <- filter(player.joined, STL >= input$qty)
   ggplot(filtered, aes(x=eval(parse(text=input$dstat)),y=DBPM)) + geom_point(aes(color=Tm, size=DWS)) + geom_vline(aes(xintercept=mean(STL)), color="black") + geom_hline(aes(yintercept=mean(DBPM)), color="blue")
  })
})