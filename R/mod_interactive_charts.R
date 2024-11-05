
mod_interactive_charts_ui <- function(id){
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
            class = "ms-Grid-col ms-sm12 ms-md12 ms-lg12",
            div( class="card",
                plotlyOutput(ns('gapminder.interactive'))
            )
          )
        )
      )
    )
  )
}

mod_interactive_charts_server <- function(input,output, session){
  
  ns <- session$ns
  
 
  ## Animated chart
  output$gapminder.interactive <- renderPlotly({

    gapminder %>%
      plot_ly(
        x = ~gdpPercap, 
        y = ~lifeExp, 
        size = ~pop, 
        color = ~continent, 
        frame = ~year, 
        text = ~country,
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers'
      ) %>% layout(
      xaxis = list(type = "log"),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    ) %>% animation_opts(
      1000, easing = "elastic", redraw = FALSE
    ) %>% animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    ) %>% animation_slider(
      currentvalue = list(prefix = "YEAR ", font = list(color="red"))
    ) %>%
      config(displayModeBar = FALSE, displaylogo = FALSE, 
             scrollZoom = FALSE, showAxisDragHandles = TRUE, 
             showSendToCloud = FALSE)
    
  })
  
}