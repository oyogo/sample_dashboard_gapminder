
mod_static_charts_ui <- function(id){
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
            class = "ms-Grid-col ms-sm12 ms-md12 ms-lg6",
            div( class="card",
                 plotOutput(ns('gapminder.static1'))
            )
          ),
          div(
            class = "ms-Grid-col ms-sm12 ms-md12 ms-lg6",
            div( class="card",
                 plotOutput(ns('gapminder.static2'))
            )
          )
        )
      )
    )
  )
}

mod_static_charts_server <- function(input,output, session){
  
  ns <- session$ns

  ## calculate rate change from previous year per country
  df_rate_change <- gapminder %>%
    janitor::clean_names() %>% # good package and function to make column names more coding friendly.
    group_by(country) %>%
    arrange(year) %>%
    mutate(pc_lifeexp_change = round(((life_exp/ lag(life_exp)) -1) * 100, 1)) %>% # here, rounding is done to 1 decimal place.
    mutate(pc_pop_change = round(((pop / lag(pop)) -1) * 100, 1)) %>%
    mutate(pc_gdp_cap_change = round(((gdp_percap / lag(gdp_percap)) -1) * 100, 1)) %>% #changes here are * 100 to make %
    arrange(country)
  
  ## plot a line graph showing the changes in life expectancy per year for 5 countries
  ## find the 5 countries with the lowest population in 2007 per continent
  low_pop_2007 <- df_rate_change %>%
    filter(year == 2007) %>%
    group_by(continent) %>%
    arrange(life_exp) %>%
    slice(1) %>% # slice() takes the first row per group - here it is important that the previous steps are correct. head() will only take the 1st row of the dataframe.
    ungroup() %>%
    pull(country) #pull() will return a list of the countries, use class(low_pop_2007) to see the structure of the outcome.
  
  high_pop_2007 <- df_rate_change %>%
    filter(year == 2007) %>%
    group_by(continent) %>%
    arrange(desc(life_exp)) %>% # nesting desc() within arrange() will arrange in descending order.
    slice(1) %>%
    ungroup() %>%
    pull(country)
  
  countries_to_plot <- c(as.character(low_pop_2007), as.character(high_pop_2007)) # have to change to character type from factors. See what happens if you remove the as.character() calls.
  
  ## Static chart1
  output$gapminder.static1 <- renderPlot({
    
    ggplot(data = df_rate_change %>%
             filter(country %in% countries_to_plot), aes(x = year, colour = country)) +
      geom_point(aes(y = pc_gdp_cap_change)) +
      geom_line(aes(y = pc_gdp_cap_change), linetype = "dashed") + # difficult to add in the legend of linetypes when layering graphs like this.
      geom_point(aes(y = pc_pop_change)) +
      geom_line(aes(y = pc_pop_change)) +
      facet_grid(rows = vars(continent)) +
      scale_x_continuous(breaks = seq(min(df_rate_change$year), max(df_rate_change$year), by = 5)) +
      scale_y_continuous(breaks = seq(-50, 50, by = 25)) +
      coord_cartesian(xlim = c(1957, 2007), y = c(-50,50)) +
      labs(title = "Change in GDP per capita and population time", x = "Year", y = "Percent change (%)") +
      theme_light()
    
  })
  
  
  ## Static chart2
  
  output$gapminder.static2 <- renderPlot({
    
    ggplot(data = gapminder, aes(x = reorder(continent, -lifeExp), y = lifeExp, fill = continent)) + # useful reordering function suppled to the x aesthetic. Remove it and see the effects.
      geom_boxplot(outlier.color = NA) + #removed outliers in this plot because we are layering on geom_point() of the data, try commenting out the geom_point() and removing the outlier.color = NA command to see how outliers are presented by default.
      geom_jitter(alpha = 0.2) + #geom_jitter is a variation on geom_point(). Swap out for geom_point() to see the effects.
      scale_y_continuous(breaks = round(seq(min(gapminder$lifeExp), max(gapminder$lifeExp), by = 10),0)) + # seq() is for sequence, change the by = 10 to a different value to see the effects.
      coord_flip() + # comment coord_flip() out to see the effects.
      labs(title = "Boxplot of life expectancy by continent", x = "Life Expectancy", y = "Continent") +
      theme_light() +
      theme(legend.position = "none", # removed the legend as not visually necessary in this plot.
            axis.line.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill='#AAB5BC', colour = NA_character_),
            panel.background = element_rect(fill='#AAB5BC', colour = NA_character_),
            axis.text.y = element_text(face="bold",size = 14),
            axis.ticks.x = element_blank())  
      
    
  })
  
}