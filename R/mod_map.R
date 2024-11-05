
variables <- list(
  list(key = 'pop', text = 'Population'), 
  list(key = 'gdpPercap', text = 'GDP per Capita'),
  list(key = 'lifeExp', text = 'Life Expectancy')
)


countries <- list(
  list(key = 'Zimbabwe', text = 'Zimbabwe'), 
  list(key = 'Zambia', text = 'Zambia'),
  list(key = 'Yemen', text = 'Yemen'), 
  list(key = 'Vietnam', text = 'Vietnam'),
  list(key = 'Venezuela', text = 'Venezuela'),
  list(key = 'Vatican', text = 'Vatican')
  
)

mod_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluentPage(
      br(),
      br(),
      div(
        class = "md-Grid", dir = "ltr",
        div(
          class = "ms-Grid-row",

          div(
            class = "ms-Grid-col ms-sm12 ms-md12 ms-lg4",
            div( class="card",
              shiny.fluent::Stack(horizontal = FALSE, 
                                  tokens = list(childrenGap = 20), 
                                  div( 
                                    h4('Move the slider to select year: '),
                                    Slider.shinyInput(ns("year"),
                                                      value = 2002, min = 1952, max = 2007, step = 5,
                                                      label = "year",
                                                      snapToStep = TRUE
                                    )
                                  ),
                                  div( 
                                    h4('Select Country from dropdown: '),
                                    Dropdown.shinyInput(ns('country'),value = '',
                                                        options = countries)
                                  ),
                                  div(
                                    h4('Select variable to visualize on map: '),
                                    ChoiceGroup.shinyInput(ns("var"), value = "pop", options = variables),
                                    br(),
                                    br()
                                  )
              )
            )
          ),
          div(
            class = "ms-Grid-col ms-sm12 ms-md12 ms-lg8",
            div( class="card",
                 leafletOutput(ns("map"), height = '450px')

            )
          )

        )
      )
    )
  )
}

mod_map_server <- function(input,output, session){
  
  ns <- session$ns
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world <- world %>% 
    select(name_sort,label_x,label_y,pop_est) %>%
    rename(country=name_sort)%>%
    full_join(gapminder,by='country')
  
  
 
observeEvent({input$year
              input$var
              input$country},
             {

    filtered.data <- reactive({

      world <- world %>% 
        filter(year==input$year)
      

    })
    
     varr <- reactive({input$var})
     #print(get(input$var))
    # variab <- subset[world,select=varr()]
    # pal <- colorNumeric(palette = "YlOrRd", domain = variab)

  output$map <- renderLeaflet({
    

    leaflet(filtered.data()) %>%
      addTiles() %>%
      addPolygons(data=filtered.data(),
                  popup = paste("Country: ", world$country),
                  weight = 2,
                  opacity = 1,
                  color = ~ colorNumeric("YlOrRd",get(input$var))(get(input$var)),
                  dashArray = "3",
                  fillOpacity = 0.7) #%>%
      # addLegend(data = filtered.data(),
      #           pal = colorNumeric("YlOrRd",get(varr()))(get(varr())),
      #           values = ~get(varr()),
      #           opacity = 1.0,
      #           #title = pop#paste0(input$var),
      #           position = 'bottomright')
    
  })

    selected.country <- filtered.data() %>% filter(country==input$country)

    proxy <- leafletProxy("map")
    proxy %>%
      flyTo(
        lng = selected.country$label_x,
        lat = selected.country$label_y,
        zoom = 6
      ) %>%
      clearShapes()%>%
      addPolygons(data=filtered.data(),
                  popup = paste("Country: ", filtered.data()$country),
                  color = ~ colorNumeric("YlOrRd",get(input$var))(get(input$var)),
                  weight = 2,
                  opacity = 1,
                  dashArray = "3",
                  fillOpacity = 0.7)
    
  })
  

}