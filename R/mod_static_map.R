
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

mod_static_map_ui <- function(id){
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
                                       br
                                     )
                 )
            )
          ),
          div(
            class = "ms-Grid-col ms-sm12 ms-md12 ms-lg8",
            div( class="card",
                 plotOutput(ns("map"),height = '450px')
                 
            )
          )
          
        )
      )
    )
  )
}

mod_static_map_server <- function(input,output, session){
  
  ns <- session$ns
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world <- world %>% 
    select(name_sort,label_x,label_y,pop_est) %>%
    rename(country=name_sort)%>%
    full_join(gapminder,by='country')
  
 output$map <- renderPlot({
   
   world %>% filter(year==2002) %>%
     ggplot() +
     geom_sf(aes(fill = lifeExp)) +
     theme_void() +
     theme(plot.background = element_rect(fill="#AAB5BC",color=NA),
           legend.background = element_rect(fill = "transparent"), 
           legend.box.background = element_rect(fill = "transparent"), 
           panel.background = element_rect(fill = "transparent"), 
           panel.grid.major = element_blank(), 
           panel.grid.minor = element_blank()
           
           )
   
 })
  
  
}