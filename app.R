
#renv::load("/srv/shiny-server/gapminder")

library(shiny)
library(shiny.fluent)
library(shiny.router)
library(data.table)
library(plotly)
library(gapminder)
library(ggtext)
library(dplyr)
library(ggthemes)

source("R/mod_home.R")
#source("R/mod_map.R")
#source('R/mod_static_map.R')
source('R/mod_interactive_charts.R')
source('R/mod_static_charts.R')


header <- tagList(
  img(src = "opm_logo.png",style="height: 30px; width:100px; margin-top: 7px", class = "logo"),
  div(Text(variant = "xLarge", "Gapminder Dashboard"), class = "title"),
  CommandBar(
    items = list(
      CommandBarItem("Download detailed reports",text='',
                     subitems = list(CommandBarItem(key = "Component 1", text = "Download"),
                                     CommandBarItem(key = "Component 2", text = "Download"),
                                     CommandBarItem(key = "Component 3", text = "Download")))
    ),
    style = list(width = "100%"))
  
)


navigation <- Nav(
  groups = list(
    list(links = list(
      list(name = 'Home', url = '#!/home', key = 'home', icon = 'Home'),
      # list(
      #   name = "Mapping",
      #   isExpanded = TRUE,
      #   links = list(
      #     list(
      #       name = "Interactive Map",
      #       url = "#!/map",
      #       key = "map",
      #       icon = "Group"
      #     ),
      #     list(
      #       name = "Static Map",
      #       url = "#!/static_map",
      #       key = "an2",
      #       icon = "Money"
      #     )
      #   )
      # ),
      list(name = "Charts", 
           isExpanded = TRUE,
           links = list(
             list(
               name = "Gapminder Animated chart",
               url = "#!/gapminderInteractive",
               key = "gapminder1",
               icon = "Sync"
             ),
             list(
               name = "Gapminder Static charts",
               url = "#!/gapminderStatic",
               key = "gapminder2",
               icon = "ShoppingCart"
             )
           ))
    )
    )
  ),
  initialSelectedKey = 'home',
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
  )
)


footer <- tagList(
  div(id="center",img(src="OPM-Core-Logo-Reversed-RGB.png",style="height: 30px; width:100px;"),br(),
      h4("Developed by the OPM data innovation team",style="color:white;")
      )
)

layout <- function(mainUI){
  div(class = "grid-container",
      div(class = "header", header),
      div(class = "sidenav", navigation),
      div(class = "main", mainUI),
      div(class = "footer", footer)
  )
}


router <- make_router(
  route("home", div(mod_home_ui('home_ui_1'))),
  #route("map", div(mod_map_ui('map_ui_1'))),
  #route("static_map", div(mod_static_map_ui('static_map_ui_1'))),
  route("gapminderInteractive", div(mod_interactive_charts_ui('interactive_charts_ui_1'))),
  route("gapminderStatic", div(mod_static_charts_ui('static_charts_ui_1')))
)

shiny_router_js_src <- file.path("www", "shiny.router.js")
shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)
shiny::addResourcePath("./www", "./www")

# Define UI for application
# ---
ui <- fluentPage(
  
  layout(router$ui),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
    shiny_router_script_tag
    
  ))


# Define server logic 
server <- function(input, output,session) {
  
  router$server(input, output, session)
  
  # call the server part
  callModule(mod_home_server, "home_ui_1")
  #callModule(mod_map_server, "map_ui_1")
  #callModule(mod_static_map_server, "static_map_ui_1")
  callModule(mod_interactive_charts_server, "interactive_charts_ui_1")
  callModule(mod_static_charts_server, "static_charts_ui_1")
  
}

# Run the application 
shinyApp(ui = ui, server = server)
