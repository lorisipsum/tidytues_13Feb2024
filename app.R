library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(shinyjs)
library(png)
library(ggimage)
library(fmsb)
library(ggalluvial)
library(viridis)


heritage <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv')
heritage$flags <- c("www/Norway.png", "www/Denmark.png", "www/Sweden.png")

title = tags$div(tags$strong("100 Graphs Dashboard"))

header <- dashboardHeader(title = title)

sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))

body <- dashboardBody(
  shinyjs::useShinyjs(),
  uiOutput("bodypanel"),
  tags$head(tags$style(HTML('
  /* logo */
     .skin-green .main-header .logo {
     background-color: #6d90b3;
     }
   
    /* navbar (rest of the header) */
      .skin-green .main-header .navbar {
      background-color: #6d90b3;
      }
     
     /* main sidebar */
      .skin-green .main-sidebar {
      background-color: #6d90b3;
     }
 
    /* body */
    .content-wrapper, .right-side {
      background-color: #8fa2b5;
    }
  ')))
)

graph_choices <- c( "Slope Plot", "Scatter Plot", "Alluvial Plot", "Lollipop Plot", "Radar Plot", "Grouped Bar Chart")

ui<-dashboardPage(header, sidebar, body, skin = "green")

server <- function(input, output, session) {
  
output$bodypanel <- renderUI({tabItems(
  tabItem(
    tabName ="graphs", class = "active",
    column(6,
           selectInput("chosen_graph", tags$label("Graph of choice", style = "color: #fcfcfc;"), choices = graph_choices)
    ),
    br(), br(),
    plotOutput("graph_of_choice"),
    br(), br(), br(),
    align = "center",
    "Another silly Shiny by: ", 
    span(strong("lorisipsum"), style = "color:blue"), 
    br(), 
    tags$img(src = 'photo.png', height = 75, width = 100),
    br()
    ))
    })

output$sidebarpanel <- renderUI({
  sidebarMenu(
    menuItem(tags$label("Graphs", style = "Color: #fcfcfc;"), tabName = "graphs", icon=icon("chart-simple"))
  )
})


output$graph_of_choice <- renderPlot({
  if (input$chosen_graph == "Scatter Plot"){
     heritage %>%
      ggplot(aes(x = `2004`, y = `2022`, image = flags)) + 
      geom_point() + geom_image() + 
      theme_bw() + 
      theme(text = element_text(size=20)) + 
      ggtitle("Number of World Heritage Sites by Country")
    }
  else if (input$chosen_graph == "Radar Plot"){
    heritage_transposed <- as.data.frame(t(heritage))
    colnames(heritage_transposed) <- heritage_transposed[1,]
    heritage_transposed[1,] <- c(15, 15, 15)
    temp <- heritage_transposed[2,]
    heritage_transposed[2,] <- c(0, 0, 0)
    heritage_transposed[5,] <- temp
    rownames(heritage_transposed) <- c(1, 2, "2022", "flags", "2004")
    heritage_transposed <- heritage_transposed[-4,]
    heritage_transposed <- as.data.frame(lapply(heritage_transposed, as.numeric))
    colors_border=c("#440154FF", "#FDE725FF")
    colors_in=c("#440154FF", "#FDE725FF")
   radarchart(heritage_transposed, #custom polygon
               pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
               #custom the grid
               cglcol="black", cglty=1, axislabcol="blue", caxislabels=seq(0,15,5), cglwd=0.8,
               #custom labels
               vlcex=1, pdensity = 25 )
  legend(x=0.5, y=1, legend = c("2022", "2004"), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3) 
  }
  else if (input$chosen_graph == "Lollipop Plot"){
    heritage %>% ggplot() +
      geom_segment(aes(x=country, xend=country, y=`2004`, yend=`2022`), color="grey") +
      geom_point(aes(x=country, y=`2004`), color="#FDE725FF", size=4) +
      geom_point(aes(x=country, y=`2022`), color="#440154FF", size=4) +
      coord_flip()+
      theme_bw() + 
      theme(text = element_text(size=20), title = element_text(size=14)) +
      xlab("") +
      ylab("Number of World Heritage Sites") + 
      ggtitle("Yellow = 2004, Purple = 2022")
  }
  else if (input$chosen_graph == "Alluvial Plot"){
    heritage %>% pivot_longer(cols = !c(country, flags)) %>% rename(Country = country) %>%
      ggplot(aes(x = name, y = value, alluvium = Country)) +
      geom_alluvium(aes(fill = Country, colour = Country),
                    alpha = .75, decreasing = FALSE) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = -30, hjust = 0), text = element_text(size=20), 
            title = element_text(size=14)) +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      xlab("Year") +
      ylab("Number of World Heritage Sites") + 
      ggtitle("Number of World Heritage Sites by Country")
  }
  else if (input$chosen_graph == "Grouped Bar Chart"){
    heritage %>% pivot_longer(cols = !c(country, flags)) %>% rename(Year = name) %>%
      ggplot(aes(x=country, y = value, fill = Year)) +
      xlab("Country") +
      ylab("Number of World Heritage Sites") + 
      geom_bar(position = "dodge", stat = "identity") + 
      scale_fill_viridis_d() + 
      theme_bw() +
      theme(text = element_text(size=20), 
            title = element_text(size=14))
  }
  else if (input$chosen_graph == "Slope Plot"){
    heritage %>% pivot_longer(cols = !c(country, flags)) %>% rename(Year = name) %>% rename(Country = country) %>%
      ggplot(aes(x=Year, y = value, image = flags)) +
      xlab("Year") +
      ylab("Number of World Heritage Sites") + 
      geom_point(aes(color = Country)) + 
      geom_line(aes(color = Country, group = Country), linewidth = 1) +
      scale_color_viridis_d() + 
      theme_bw() +
      theme(text = element_text(size=20), 
            title = element_text(size=14)) + 
      geom_image() 
  }
})
 
} 
  
shinyApp(ui = ui, server = server)


