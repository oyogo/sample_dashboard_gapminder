
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
  fluentPage(
    br(),
    br(),
    div(
      class = "title",
      h3("About this data"),
      p("Most of our data are not good enough for detailed numeric analysis. 
         They are only good enough to revolutionize people’s worldview. 
         But we only fill in gaps whenever we believe we know roughly what the numbers would have been, had they existed. 
         The uncertainties are often large. 
         But we comfort ourselves by knowing the errors in peoples worldview are even larger. 
         Our data is constantly improved by feedback in our data forum from users finding mistakes: ",
        em("gapminder documentation."),style="color: black;"),
      
    ),
    br(),
  
    div(
  shiny.fluent::Stack(horizontal = TRUE, horizontalAlign="center",
                        tokens = list(childrenGap = 40),
                      
                      div(
                        Stack(horizontal = FALSE, horizontalAlign="center", 
                              tokens = list(childrenGap = 1,padding=1),
                              
                        img(src="transactions-icon-19.jpg",width = "auto", height = "50px"),
                        h3("7215"),
                        h3("Average gdp per Capita")
                        )
                        
                      ),
                      div(
                        Stack(horizontal = FALSE, horizontalAlign="center", 
                              tokens = list(childrenGap = 1,padding=1),
                        img(src="lifeExp-removebg-preview.png",width = "auto", height = "50px"),
                        h3("59yrs"),
                        h3("Average life expectancy")
                        )
                      ),
                      div(
                        Stack(horizontal = FALSE, horizontalAlign="center", 
                              tokens = list(childrenGap = 1,padding=1),
                        img(src="countryy-removebg-preview.png",width = "auto", height = "50px"),
                        h3("142"),
                        h3("Countries covered")
                        )
                        
                      )#,
                      # div(
                      #   Stack(horizontal = FALSE, horizontalAlign="center", 
                      #         tokens = list(childrenGap = 1,padding=1),
                      #   img(src="churn.png",width = "auto", height = "50px"),
                      #   h3("51.8%"),
                      #   h3("Percentage rate 2")
                      #   )
                      #   
                      # )
               )
          ),
  br(),
  br(),
  div(
    class = "md-Grid", dir = "ltr",
    div( 
      class = "ms-Grid-row",
      div(
        class = "ms-Grid-col ms-sm12 ms-md12 ms-lg6",
        div( class="card",
             # h4("Use the slider below to interact with the plot", style="margin-left: 30%;"),
             # Slider.shinyInput(ns("year"), value = 1952, min = 1952, max = 2007, step=5,showValue=TRUE),
             # 
             # h3("Gapminder plot", style="margin-left: 40%;"),
             # plotlyOutput(ns("gapPlot"),height = "300px"),
             plotlyOutput(ns("bar"))
             
          # div(
          #   Stack(horizontal = FALSE, horizontalAlign="center",
          #         tokens = list(childrenGap = 10,padding=10),
          #         tagList(
          #           reactOutput(ns("modal1")),
          #           DefaultButton.shinyInput(ns("showModal1"), text = "Click for insights on above plot"),
          #         )
          #   )
          # 
          # )
        )
      ),
      div(
        class = "ms-Grid-col ms-sm12 ms-md12 ms-lg6",
        div(
          class = "card",
          
          plotlyOutput(ns("bubble"))
          
        )
      )
      
    )#,
    # div( 
    #   class = "ms-Grid-row",
    #   
    #   div(
    #     class = "ms-Grid-col ms-sm12 ms-md12 ms-lg6",
    #     div(
    #       class = "card",
    #       
    #       plotlyOutput(ns('linegraph'))
    #       
    #     )
    #   )
    #   
    # )
  )
 )
)
}

mod_home_server <- function(input,output, session){
  
  ns <- session$ns
 
  dat <- data.table(gapminder)
  
  gapminder_single_year <- reactive({
    # take just the selected year of GapMinder data
    gapminder %>%
      filter(year == input$year) %>%
      mutate(pop_m = pop / 1000000)
  })
  
  output$gapPlot <- renderPlotly({
    gapminder_single_year() %>%
      ggplot(aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop_m, text = country)) +
      geom_point(show.legend = TRUE) +
      scale_x_log10(label = scales::comma) +
      scale_size_continuous(label = scales::comma, breaks = c(0, 1, 10, 100, 1000), range = c(1, 10)) +
      scale_colour_discrete() +
      expand_limits(x = c(1e2, 1e5), y = c(20, 80)) +
      labs(x = "GDP per capita ($)", y = "Life expectancy (years)", 
           size = "Population, millions", colour = "Continent") +
      theme(#legend.position = "none",
            axis.line.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill='#AAB5BC', colour = NA_character_),
            panel.background = element_rect(fill='#AAB5BC', colour = NA_character_),
            axis.text.y = element_text(face="bold",size = 14),
            axis.ticks.x = element_blank()
      ) 
    
    ggplotly(tooltip = "text")
  })
  
  output$bubble <- renderPlotly({
    
    # calculate the average by continent
    population <- dat[,.(total_pop=sum(pop)),by=continent]
 
    population %>%
      plot_ly() %>%
      add_trace(x = ~reorder(continent, -total_pop), 
                y = ~total_pop,
                size = ~total_pop,
                color = ~continent,
                alpha = 10,
                #color=I("#0B1F51"),
                type = "scatter",
                mode = "markers",
                marker = list(symbol = 'circle', sizemode = 'diameter',color="#0B1F51",
                              line = list(width = 2, color = '#FFFFFF'), opacity=1)) %>%
      add_text( x = ~reorder(continent, -total_pop), 
                y = ~continent, 
                text = ~total_pop,
                textposition="middle center",
                color = I("black")) %>%
      layout(
        showlegend = FALSE,
        title="Asia population is about 4 times that of Americas.",
        xaxis = list(
          title = "Continent"
        ),
        yaxis = list(
          title = "Population",rangemode = "tozero"
        ),
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)"
      ) %>%
      config(displayModeBar = FALSE, displaylogo = FALSE, 
             scrollZoom = FALSE, showAxisDragHandles = TRUE, 
             showSendToCloud = FALSE)
    
  })
  
  output$bar <- renderPlotly({
    
    # get the averages by continent
    gdp <- dat[,.(avg_gdp=round(mean(gdpPercap)),1),by=continent]
    
    gdp %>% 
      plot_ly(y=~avg_gdp,
              x=~continent,
              type="bar",
              color=I("#0B1F51")
      ) %>%
      layout(title="Oceania and Europe's gdp per capita is \n more than twice other Continent's.",
             yaxis=list(title="Average gdp per capita"),
             xaxis=list(title="Continent"),
             plot_bgcolor  = "rgba(0, 0, 0, 0)",
             paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
      config(displayModeBar = FALSE, displaylogo = FALSE, 
             scrollZoom = FALSE, showAxisDragHandles = TRUE, 
             showSendToCloud = FALSE)
    
  })
  
  output$linegraph <- renderPlotly({
    
    life_exp <- dat[,.(avg_lifeExp=round(mean(lifeExp),1)),by=year]
    
    life_exp %>% 
      plot_ly(x=~as.character(year),y=~avg_lifeExp,type = 'scatter', 
              mode = "lines",
              color = I('#0B1F51')) %>%
      plotly::layout(title = "",
                     xaxis = list(zerolinecolor = '#ffff',
                                  zerolinewidth = 2, gridcolor = 'ffff', title="Year"),
                     yaxis = list(zerolinecolor = '#ffff',rangemode = "tozero",
                                  zerolinewidth = 2,gridcolor = 'ffff', title="Life Expectancy"),
                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
      config(displayModeBar = FALSE, displaylogo = FALSE, 
             scrollZoom = FALSE, showAxisDragHandles = TRUE, 
             showSendToCloud = FALSE)
    
  })
  
  # modalVisible <- reactiveVal(FALSE)
  # observeEvent(input$showModal1, modalVisible(TRUE))
  # observeEvent(input$hideModal1, modalVisible(FALSE))
  # output$modal1 <- renderReact({
  #   Modal(isOpen = modalVisible(),
  #         Stack(tokens = list(padding = "15px", childrenGap = "10px"),
  #               div(style = list(display = "flex"),
  #                   Text("Gapminder Documentation", variant = "large"),
  #                   div(style = list(flexGrow = 1)),
  #                   IconButton.shinyInput(
  #                     ns("hideModal1"),
  #                     iconProps = list(iconName = "Cancel")
  #                   ),
  #               ),
  #               div(
  #                 p("Most of our data are not good enough for detailed numeric analysis. 
  #                   They are only good enough to revolutionize people’s worldview. 
  #                   But we only fill in gaps whenever we believe we know roughly what the numbers would have been, had they existed. 
  #                   The uncertainties are often large. 
  #                   But we comfort ourselves by knowing the errors in peoples worldview are even larger. 
  #                   Our data is constantly improved by feedback in our data forum from users finding mistakes.")
  #               )
  #         )
  #   )
  # })
  
}