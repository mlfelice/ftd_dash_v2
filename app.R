#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# TODO: Add TPA by size class and sp (maybe have dropdown to select split by year vs size class?)
# TODO: Add check boxes for selecting size classes
# Relative diversity to mean of whole porfolio (maybe same for TPA and BA?)

# Overstory summary plot
## 'overstory_summary'
## input$year
# Understory summary plot
## 'understory_summary'
## input$year
# Year menu overview
##
##
# Overstory diversity overview
##
##
# Understory diversity overview
##
##
# Overstory richness overview
##
##
# Understory richness overview
##
##
# Plot-level overstory overview
## 'overstory_plot'
## input$plot
# Plot-level understory overview
## 'understory_plot
## input$plot
# Qualitative site notes
## 'site_summary_table'
## input$site_year (dynamically updated based on site)
## input$plot
##

# TODO: Can't totally figure out how to get both static number o fx axis labels and ordering by BA/TPA. I think we might have to maybe do something where we insert all species for all groups even if no counts, then do reorder within aes()

# To render the website after updating app:
# shinylive::export(appdir = 'C:/Users/mark.felice/Documents/ftd_dash_v2/', destdir = 'docs')


# Packages ----------------------------------------------------------------

library(rlang) # not sure if needed
library(vctrs) # not sure if needed


library(dplyr)
library(tidyr)
#library(stringr)
library(ggplot2)
library(plotly)

auto_size_plt <- function(df, px, min = 400){
  nrow(df) * px + min
}

# Data import -------------------------------------------------------------


load('data/ftd_dash_data.RData')

sum_na_not_zero <- function(x) {
  if(all(is.na(x))) {
    NA
  }
  else(sum(x, na.rm = T))
}

# NOTE 2024 causing issues with repeats
## Must be mistake in import/source fields, because the spring and fall 2024 appear to have same data - at least in the spp and dbh sheets
understory_meta_df_2 <- dbhclass_repeat_df %>%
  #filter(Year %in% c('2022', '2023', '2025')) %>%
  mutate(Number.of.Seedlings = na_if(Number.of.Seedlings, 0), # 0 would interfere with calculation of mean abundance that goes into BA, TPA calcs
         DBH.Class = case_when(is.na(DBH.Class) ~ 'unknown_dbh',
                               DBH.Class == '0-1' ~ 'zero_to_one', 
                               .default = DBH.Class),
         Seedling.Species = factor(Seedling.Species, levels = levels(tree_attribute_df$TreeSpAbb)) # want to keep levels for ordering of legends, need to convert to factor 1st. Easiest to borrow levels from the tree attribute df, as they must match
         ) %>% 
  pivot_wider(names_from = DBH.Class, values_from = Number.of.Seedlings) %>%
  left_join(ftd_mon_site_data_df, by = c('ParentGlobalID' = 'GlobalID', 'Year', 'Season')) %>%
  left_join(tree_attribute_df, by = c('Seedling.Species' = 'TreeSpAbb')) %>%
  
  group_by(Site.Name, Year, Season, #ParentGlobalID, CreationDate, # I don't think we want ParentGlobalID, as I think this is the plot level, not site level
           TreeSpFull, CropOther, PlantedNot,
           Seedling.Species) %>%
  summarise(TPA_0_1 = (100/mean(nPlots, na.rm = T)) * sum_na_not_zero(zero_to_one),
            TPA_1_3 = (100/mean(nPlots, na.rm = T)) * sum_na_not_zero(one_to_three),
            TPA_3_5 = (100/mean(nPlots, na.rm = T)) * sum_na_not_zero(three_to_five),
            TPA_unk = (100/mean(nPlots, na.rm = T)) * sum_na_not_zero(unknown_dbh)) %>%
  mutate(TPAAll = sum_na_not_zero(c(TPA_0_1, TPA_1_3, TPA_3_5, TPA_unk)), # THink we need to rethink how we get the TPAAll number so we don't have separate cols for each
         TextBox = paste0(       'Species: ', Seedling.Species, '<br>',
                                 'Total TPA: ', TPAAll, '<br>',
                                 'TPA 0-1: ', TPA_0_1, '<br>',
                                 'TPA 1-3: ', TPA_1_3, '<br>',
                                 'TPA 3-5: ', TPA_3_5, '<br>')) %>%  
  pivot_longer(cols = TPA_0_1:TPA_unk, names_to = 'DBHClass', values_to = 'TPA') %>%
  mutate(TPA = na_if(TPA, 0),
         as.factor(Seedling.Species)) %>%
  ungroup() %>%
  group_by(Site.Name, Year) %>%
  mutate(Site.Name = if (n_distinct(Season) > 1)  # append the season if there are more than one
  {
    paste0(Site.Name,  ' - ', Season)
  }
  else if (n_distinct(Season) == 1) 
  {
    Site.Name
  }
  )




  


# Application -------------------------------------------------------------



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(


  tags$head(
    tags$style(HTML("
      #wrapped_site_summary td {
        white-space: normal !important;
        word-break: break-word !important;
      }
    ")#,
      #         HTML(".well { width: fit-content; }")
               )
  ),
    
    # Application title
    titlePanel('Resilient Forests Data Dashboard'),
    
    tabsetPanel(id = 'nav_tabs',
                tabPanel('Overview',

                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                              sidebarPanel(
                                selectInput('year', 'Year', 
                                            choices = sort(unique(spp_meta_df$Year)) # May need to modify the source of this
                                ),
                                ),
                      
                              # Show a plot of the generated distribution
                              mainPanel(
                                fluidRow(
                                  h3('Overstory'),
                                  plotlyOutput('overstory_summary', height = 'auto'),
                                  h3('Understory'),
                                  plotlyOutput('understory_summary', height = 'auto')
                                  ),
                              )
                          ) 
                ),
                tabPanel('Diversity',
                         sidebarLayout(
                           sidebarPanel(
                             selectInput('year_diversity', 'Year', 
                                         choices = sort(unique(spp_meta_df$Year)) # May need to modify the source of this
                             ),
                             selectInput('div_metric', 'Diversity Metric', 
                                         choices = list('Richness (# of Species)' = 'SpeciesNum', 
                                                        'Evenness' = 'Pielou', 
                                                        'Shannon Diversity Index (H\')' = 'H') # May need to modify the source of this
                             ),
                           ),

                           # Show a plot of the generated distribution
                           mainPanel(
                             #h3('Overstory'),
                             #h4('Diversity (Shannon-Weiner'),
                             #plotOutput('overstory_diversity'),
                             #h4('Richness (# of Species)'),
                             #plotOutput('overstory_richness'),
                             #h4('Evenness'),
                             #plotOutput('overstory_evenness'),
                             
                             #h3('Understory'),
                             #h4('Diversity (Shannon-Weiner'),
                             #plotOutput('understory_diversity'),
                             #h4('Richness (# of Species)'),
                             #plotOutput('understory_richness'),
                             #h4('Evenness'),
                             #plotOutput('understory_evenness')
                             fluidRow(
                               h3(
                                 textOutput('div_metric_head')
                               ),
                               h4('Overstory'),
                               plotOutput('overstory_diversity', height = 'auto'),
                               h4('Understory'),
                               plotOutput('understory_diversity', height = 'auto')
                             )

                           )
                         )
                         ),
                tabPanel('Site-Level Data',
                        
                         sidebarLayout(
                           sidebarPanel(
                             selectInput('site', 'Site Name', 
                                         choices = sort(unique(ftd_mon_site_data_df$Site.Name)) # unique(spp_meta_df$Site.Name)
                             ),
                             # put a frame of site level data here
                             h4('Site Notes'),
                             selectInput('site_year', 'Year', 
                                         choices = NULL # unique(spp_meta_df$Year)
                             ),
                             div(id = 'wrap_site_summary',
                                 tableOutput('site_summary_table'), 
                                       style = 'font-size: 80%;'),
                             width = 5
                           ),
                           # Show a plot of the generated distribution
                           mainPanel(
                             h4('Overstory Basal Area'),
                             plotlyOutput("overstory_plot"),
                             h4('Understory Plot Seedling Density'),
                             plotlyOutput("understory_plot"),
                             #h4('Understory Plot Seedling Density'),
                             #plotOutput("understory_class_plot"),
                             width = 7
                           )
                         )
                )
                
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # TODO: Try to sort the legend in a way that the color values look a little more continuous
  #TODO: add option to sort by name or value

### Old ggplot2 version of overstory summary ###  
#  output$overstory_summary <- renderPlot({
#    # Stack bar plot all sites, with groups for aspen, oak, non-planted sp
#    spp_meta_df %>%
#      filter(Year == input$year,
#             !is.na(Site.Name)
#      ) %>%
#      group_by(Site.Name) %>%
#      mutate(SiteBA = sum(BA, na.rm = T)) %>% # This is just to help for sorting plot
#      ggplot() +
#      geom_col(aes(x = reorder(Site.Name, SiteBA), y = BA, fill = Tree.Species), 
#               #color = 'black'
#      ) +
#      coord_flip() +
#      scale_fill_manual(values = conifer_hardwood_pal) +
#      labs(y = 'Basal Area', x = 'Site', fill = 'Tree Species') +
#      theme_bw() +
#      theme(panel.grid.minor.x = element_blank(),
#            plot.background = element_rect(fill = "transparent", colour = NA),
#            #panel.background = element_rect(fill = "transparent", colour = NA)
#      )
#  },
#  height = function() {
#    nrow(
#      filter(.data = spp_meta_df, Year == input$year,
#             !is.na(Site.Name)
#      )
#    ) * 1 + 400
#  }
#  )
  
  output$overstory_summary <- renderPlotly({
    
    plot_height <- 
      nrow(
        filter(.data = understory_meta_df_2, 
               Year == input$year,
               !is.na(Site.Name)
        )
      ) * 0.2 + 400
    
    
    spp_meta_df %>%
      filter(Year == input$year,
             !is.na(Site.Name)
      ) %>%
      group_by(Site.Name) %>%
      mutate(SiteBA = round(sum(BA, na.rm = T), 2),
             BA = round(BA, 2)
      ) %>% # This is just to help for sorting plot
      plot_ly(
        data = .,
        y = ~reorder(Site.Name, SiteBA),
        x = ~BA,
        type = 'bar',
        color = ~Tree.Species,
        colors = conifer_hardwood_pal,
        text = ~TreeSpFull,
        textposition = 'none',
        customdata = ~SiteBA,
        hovertemplate = paste0('%{y}<br>', 
                               '%{text} BA: %{x}ft<sup>2</sup> acre<sup>-1</sup><br>',
                               'All Species BA: %{customdata}ft<sup>2</sup> acre<sup>-1</sup>',
                               '<extra></extra>'
        ),
        height = plot_height
        
      ) %>%
      
      layout(
        barmode = 'stack',
        title = 'Tree Abundance', 
        legend = list(
          title = list(
            text ='Tree Species'
          )
        ),
        plot_bgcolor = NULL, 
        xaxis = list( 
          zerolinecolor = 'black', 
          zerolinewidth = 1, 
          gridcolor = '#e5ecf6',
          title = 'Basal Area: (ft<sup>2</sup> acre<sup>-1</sup>)'
        ), 
        yaxis = list( 
          showline = T,
          zerolinecolor = 'black', 
          zerolinewidth = 1, 
          gridcolor = '#e5ecf6',
          title = 'Site Name')
      )
  })
  
### Old ggplot2 version of understory summary plot ###
#  output$understory_summary <- renderPlot({
#    understory_meta_df_2 %>%
#      filter(Year == input$year,
#             !is.na(Site.Name),
#             !is.na(TPAAll)
#      ) %>%
#      group_by(Site.Name) %>%
#      mutate(SiteTPA = sum_na_not_zero(TPAAll)) %>% # This is just to help for sorting plot
#      ggplot() +
#      geom_col(aes(x = reorder(Site.Name, SiteTPA), y = TPAAll, fill = Seedling.Species), 
#               #color = 'black'
#      ) +
#      coord_flip() +
#      scale_fill_manual(values = conifer_hardwood_pal) +
#      labs(y = 'Trees per Acre', x = 'Site', fill = 'Seedling Species') +
#      theme_bw() +
#      theme(panel.grid.minor.x = element_blank(),
#            plot.background = element_rect(fill = "transparent", colour = NA),
#            #panel.background = element_rect(fill = "transparent", colour = NA)
#      )
#  },
#  height = function() { # from when this was ggplot instead of plotly
#    nrow(
#      filter(.data = understory_meta_df, Year == input$year,
#             !is.na(Site.Name)
#      )
#    ) * 1 + 400
#  }
#  )
  
  output$understory_summary <- renderPlotly({
    
    plot_height <- 
      nrow(
        filter(.data = understory_meta_df_2, 
               Year == input$year,
               !is.na(Site.Name)
        )
      ) * 0.2 + 400
    
    understory_meta_df_2 %>%
      filter(Year == input$year,
             !is.na(Site.Name),
             !is.na(TPAAll)
      ) %>%
      group_by(Site.Name) %>%
      mutate(SiteTPA = sum_na_not_zero(TPAAll)) %>% # This is just to help for sorting plot
      plot_ly(
        data = .,
        y = ~reorder(Site.Name, SiteTPA),
        x = ~TPAAll,
        type = 'bar',
        color = ~Seedling.Species,
        colors = conifer_hardwood_pal,
        text = ~TreeSpFull,
        textposition = 'none',
        customdata = ~SiteTPA,
        hovertemplate = paste0('%{y}<br>', 
                               '%{text} TPA: %{x}<br>',
                               'All Species TPA: %{customdata}<br>',
                               '<extra></extra>'
        ),
        height = plot_height
      ) %>%
      
      layout(
        barmode = 'stack',
        title = 'Seedling Abundance', 
        legend = list(
          title = list(
            text ='Seedling Species'
            )
        ),
        plot_bgcolor = NULL, 
        xaxis = list( 
          zerolinecolor = 'black', 
          zerolinewidth = 1, 
          gridcolor = '#e5ecf6',
          title = 'Trees per Acre'
          ), 
        yaxis = list( 
          showline = T,
          zerolinecolor = 'black', 
          zerolinewidth = 1, 
          gridcolor = '#e5ecf6',
          title = 'Site Name')
      )
  })

# Diversity tab -----------------------------------------------------------


#  output$overstory_diversity <- renderPlot({
#    over_diversity_df %>%
#      filter(Year == input$year_diversity) %>%  ### Need to remember we have multiple years together, but want to plot single
#    
#      ggplot() +
#      geom_col(aes(x = reorder(Site.Name, H), y = H), 
#               fill = 'forestgreen',
#               color = 'black'
#               ) +
#      coord_flip() +
#      labs(y = 'Shannon Diversity Index (H\')', x = 'Site') +
#      theme_bw() +
#      theme(panel.grid.minor.x = element_blank(),
#            plot.background = element_rect(fill = "transparent", colour = NA),
#            #panel.background = element_rect(fill = "transparent", colour = NA)
#      )
#  })

#  output$understory_diversity <- renderPlot({
#    under_diversity_df %>%
#      filter(Year == input$year_diversity) %>%
#      
#      ggplot() +
#      geom_col(aes(x = reorder(Site.Name, H), y = H), 
#               fill = 'forestgreen',
#               color = 'black'
#      ) +
#      coord_flip() +
#      labs(y = 'Shannon Diversity Index (H\')', x = 'Site') +
#      theme_bw() +
#      theme(panel.grid.minor.x = element_blank(),
#            plot.background = element_rect(fill = "transparent", colour = NA),
#            #panel.background = element_rect(fill = "transparent", colour = NA)
#      )
#  })
#
#  output$overstory_richness <- renderPlot({
#    over_diversity_df %>%
#      filter(Year == input$year_diversity) %>%  ### Need to remember we have multiple years together, but want to plot single
#      
#      ggplot() +
#      geom_col(aes(x = reorder(Site.Name, SpeciesNum), y = SpeciesNum), 
#               fill = 'forestgreen',
#               color = 'black'
#      ) +
#      coord_flip() +
#      labs(y = "Species Ricness (# of Species)", x = 'Site') +
#      theme_bw() +
#      theme(panel.grid.minor.x = element_blank(),
#            plot.background = element_rect(fill = "transparent", colour = NA),
#            #panel.background = element_rect(fill = "transparent", colour = NA)
#      )
#  })
#  
#  output$understory_richness <- renderPlot({
#    under_diversity_df %>%
#      filter(Year == input$year_diversity) %>%  ### Need to remember we have multiple years together, but want to plot single
#      
#      ggplot() +
#      geom_col(aes(x = reorder(Site.Name, SpeciesNum), y = SpeciesNum), 
#               fill = 'forestgreen',
#               color = 'black'
#      ) +
#      coord_flip() +
#      labs(y = "Species Ricness (# of Species)", x = 'Site') +
#      theme_bw() +
#      theme(panel.grid.minor.x = element_blank(),
#            plot.background = element_rect(fill = "transparent", colour = NA),
#            #panel.background = element_rect(fill = "transparent", colour = NA)
#      )
#  })
#  
#  output$overstory_evenness <- renderPlot({
#    over_diversity_df %>%
#      filter(Year == input$year_diversity) %>%  ### Need to remember we have multiple years together, but want to plot single
#      
#      ggplot() +
#      geom_col(aes(x = reorder(Site.Name, Pielou), y = Pielou), 
#               fill = 'forestgreen',
#               color = 'black'
#      ) +
#      coord_flip() +
#      labs(y = "Pielou's Evenness", x = 'Site') +
#      theme_bw() +
#      theme(panel.grid.minor.x = element_blank(),
#            plot.background = element_rect(fill = "transparent", colour = NA),
#            #panel.background = element_rect(fill = "transparent", colour = NA)
#      )
#  })
#  
#  output$understory_evenness <- renderPlot({
#    over_diversity_df %>%
#      filter(Year == input$year_diversity) %>%  ### Need to remember we have multiple years together, but want to plot single
#      
#      ggplot() +
#      geom_col(aes(x = reorder(Site.Name, Pielou), y = Pielou), 
#               fill = 'forestgreen',
#               color = 'black'
#      ) +
#      coord_flip() +
#      labs(y = "Pielou's Evenness", x = 'Site') +
#      theme_bw() +
#      theme(panel.grid.minor.x = element_blank(),
#            plot.background = element_rect(fill = "transparent", colour = NA),
#            #panel.background = element_rect(fill = "transparent", colour = NA)
#      )
#  })
  
  #####
  #####
  output$div_metric_head <- renderText({ # need to create a function for mapping back the content of list item to its name
    #TODO: make a variable to store the list so we don't have to keep repeating it
    list_labs <- names(list('Richness (# of Species)' = 'SpeciesNum', 
                            'Evenness' = 'Pielou', 
                            'Shannon Diversity Index (H\')' = 'H'))
    heading <- list_labs[list('Richness (# of Species)' = 'SpeciesNum', 
                              'Evenness' = 'Pielou', 
                              'Shannon Diversity Index (H\')' = 'H') == input$div_metric]
    return(heading)
    })
  
  output$overstory_diversity <- renderPlot({
    over_diversity_df %>%
      filter(Year == input$year_diversity) %>%  ### Need to remember we have multiple years together, but want to plot single
      
      ggplot() +
      geom_col(aes(x = reorder(Site.Name, .data[[input$div_metric]]), y = .data[[input$div_metric]]), 
               fill = 'forestgreen',
               color = 'black'
      ) +
      coord_flip() +
      labs(y = input$div_metric, x = 'Site') +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "transparent", colour = NA),
            #panel.background = element_rect(fill = "transparent", colour = NA)
      )
  },
  height = function() {
    nrow(
      filter(.data = over_diversity_df, Year == input$year,
             !is.na(Site.Name)
      )
    ) * 2 + 400
  })
  
  output$understory_diversity <- renderPlot({
    under_diversity_df %>%
      filter(Year == input$year_diversity) %>%
      
      ggplot() +
      geom_col(aes(x = reorder(Site.Name, .data[[input$div_metric]]), y = .data[[input$div_metric]]), 
               fill = 'forestgreen',
               color = 'black'
      ) +
      coord_flip() +
      labs(y = input$div_metric, x = 'Site') +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "transparent", colour = NA),
            #panel.background = element_rect(fill = "transparent", colour = NA)
      )
  },
  height = function() {
    nrow(
      filter(.data = under_diversity_df, Year == input$year,
             !is.na(Site.Name)
      )
    ) * 2 + 400
  })
  
  #####
  #####


# Site tab --------------------------------------------------------------


  
  observeEvent(input$site, {
    filtered_years <- site_summary_df %>%
      filter(Site.Name == input$site) %>%
      pull(Year) %>%
      unique()
    
    updateSelectInput(
      session = session,
      inputId = 'site_year',
      choices = filtered_years
    )
  })
  # TODO: need to make sure we are properly summarizing across plots (on back end)
  # TODO: transform release year into date format (on back end)
  output$site_summary_table <- renderTable({site_summary_df %>%
      filter(Site.Name == input$site,
             Year == input$site_year) %>%
      mutate(release_year = format(as.Date(as.numeric(release_year), origin = '1899-12-30'), format = '%Y'), # This should probably go outside of server
             mean_grass_density_score = round(mean_grass_density_score, 2),
             mean_shrub_density_score = round(mean_shrub_density_score, 2),
             across(everything(), ~ as.character(.x))
             ) %>%
      pivot_longer(cols = -Year, names_to = 'Field') %>%
      ungroup() %>%
      select(-c(Year)) #%>%
      #rename(Site = Site.Name,
      #      `# Plots` = nPlots,
      #       `Protected sp. stocking` = stocking_protspp,
      #       `Action needs` = action_needs,
      #       `Release year` = release_year,
      #       `Stocking performance` = stock_perf)
    
  }, width = '100%', bordered = T # Force table to use available width
  )
  
### Old ggplot2 version ###  
#  output$overstory_plot <- renderPlot({
#    
#    spp_meta_df %>%
#      filter(Site.Name == input$site) %>%
#      #mutate(BasalArea = Number.of.trees * 10) %>%
#      group_by(Tree.Species) %>%
#      #mutate(BasalArea = max(BasalArea)) %>%
#      ggplot() +
#      geom_point(aes(x = reorder(Tree.Species, -BA), y = BA, color = Tree.Species, shape = as.factor(Year)), size = 5, alpha = 0.6) +
#      scale_color_manual(values = conifer_hardwood_pal) +
#      labs(x = 'Tree Species', y = 'Basal Area', color = 'Tree Species', shape = 'Year') +
#      #scale_x_discrete(limits = unique(spp_meta_df$Tree.Species)) + # force same axis regardless of site (which have variable number of species)
#      theme_bw() +
#      theme(panel.grid.minor.x = element_blank(),
#            plot.background = element_rect(fill = "transparent", colour = NA),
#            #panel.background = element_rect(fill = "transparent", colour = NA)
#      )
#  })

  output$overstory_plot <- renderPlotly({
    spp_meta_df %>%
      filter(Site.Name == input$site) %>%
      #mutate(BasalArea = Number.of.trees * 10) %>%
      group_by(Tree.Species) %>%
      #mutate(BasalArea = max(BasalArea)) %>%
      
      plot_ly(
        data = ., 
        x = ~reorder(Tree.Species, -BA), 
        y = ~BA, 
        color = ~Tree.Species,
        symbol = ~as.factor(Year),
        type = 'scatter', 
        mode = 'markers', 
        marker = list(
          size = 12,
          line = list(
            color = 'black',
            width = 0.5
          )
        ),
        colors = conifer_hardwood_pal,
        hovertemplate = paste0('Species: %{x}<br>', 
                               'BA: %{y}ft<sup>2</sup> acre<sup>-1</sup>',
                               '<extra></extra>'
          
        )
      ) %>%
      
      layout(
        title = 'Basal Area',
        legend = list(
          title = list(
            text = 'Tree Species'
          )
        ),
        plot_bgcolor = NULL,
        xaxis = list( 
          zerolinecolor = 'black', 
          zerolinewidth = 1, 
          gridcolor = '#e5ecf6',
          title = 'Tree Species'), 
        yaxis = list( 
          showline = T,
          zerolinecolor = 'black', 
          zerolinewidth = 1, 
          gridcolor = '#e5ecf6',
          title = 'Basal Area (ft<sup>2</sup> acre<sup>-1</sup>')
      )
  })
    
    
    

  
### Old ggplot2 version ###
#  output$understory_plot <- renderPlot({
#    
#    understory_meta_df_2 %>%
#      filter(Site.Name == input$site) %>%
#      #mutate(BasalArea = Number.of.Seedlings * 10) %>%
#      group_by(Seedling.Species) %>%
#      #mutate(BasalArea = max(BasalArea)) %>%
#      ggplot() +
#      #geom_point(aes(x = as.numeric (Year), y = TPAAll, color = Seedling.Species), size = 5, alpha = 0.6) +
#      #geom_line(aes(x = as.numeric (Year), y = TPAAll, color = Seedling.Species), linewidth = 2, alpha = 0.6) +
#      #labs(x = 'Year', y = 'Trees per Acre') +
#      geom_point(aes(x = reorder(Seedling.Species, -TPAAll), y = TPAAll, color = Seedling.Species, shape = as.factor(Year)), size = 5, alpha = 0.6) +
#      scale_color_manual(values = conifer_hardwood_pal) +
#      labs(x = 'Seedling Species', y = 'Trees per Acre', color = 'Seedling Species', shape = 'Year') +
#      theme_bw() +
#      theme(panel.grid.minor.x = element_blank(),
#            plot.background = element_rect(fill = "transparent", colour = NA),
#            #panel.background = element_rect(fill = "transparent", colour = NA)
#      ) #+
#      #facet_grid(Seedling.Species ~ .)
#    
#    
#  })
  output$understory_plot <- renderPlotly({
    understory_meta_df_2 %>%
      filter(Site.Name == input$site) %>%
      group_by(Seedling.Species) %>%
      
      plot_ly(
        data = ., 
        x = ~reorder(Seedling.Species, -TPAAll), 
        y = ~TPAAll, 
        color = ~Seedling.Species,
        symbol = ~as.factor(Year),
        type = 'scatter', 
        mode = 'markers', 
        marker = list(
          size = 12,
          line = list(
            color = 'black',
            width = 0.5
          )
        ),
        colors = conifer_hardwood_pal,
        text = ~I(TextBox),
        hovertemplate = '%{text}'
      ) %>%
      
      layout(
        title = 'Seedling Abundance',
        legend = list(
          title = list(
            text = 'Seedling Species'
          )
        ),
        plot_bgcolor = NULL,
        xaxis = list( 
          zerolinecolor = 'black', 
          zerolinewidth = 1, 
          gridcolor = '#e5ecf6',
          title = 'Seedling Species'), 
        yaxis = list( 
          showline = T,
          zerolinecolor = 'black', 
          zerolinewidth = 1, 
          gridcolor = '#e5ecf6',
          title = 'Trees per Acre')
      )
  })

  
  ##############
  #### Testing out using shapes to code by size and having checkboxes to select size classes
#  output$understory_class_plot <- renderPlot({
#    
#    understory_meta_df_2 %>%
#      filter(Site.Name == input$site) %>%
#      #mutate(BasalArea = Number.of.Seedlings * 10) %>%
#      group_by(Seedling.Species) %>%
#      #mutate(BasalArea = max(BasalArea)) %>%
#      ggplot() +
#      geom_point(aes(x = Seedling.Species, y = TPA, color = as.factor(Year), shape = DBHClass), size = 5, alpha = 0.6) +
#      #geom_line(aes(x = as.numeric (Year), y = TPAAll, color = Seedling.Species), linewidth = 2, alpha = 0.6) +
#      labs(x = 'Seedling Species', y = 'Trees per Acre', color = 'Year') +
#      theme_bw() +
#      theme(panel.grid.minor.x = element_blank(),
#            plot.background = element_rect(fill = "transparent", colour = NA),
#            #panel.background = element_rect(fill = "transparent", colour = NA)
#      )
#    
#    
#  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
