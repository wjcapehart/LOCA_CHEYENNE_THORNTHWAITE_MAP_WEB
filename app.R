#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# load libraries


library(package = "shiny") 
library(package = "tidyverse")
library(package = "tidync")
library(package = "maps")
library(package = "usmap")

###############################################################################
###############################################################################
##
## Prepwork
##

##################################################################
#
# Get OS

OS      = Sys.info()[1]
Machine = Sys.info()[4]
print("")
print("--------------------------------")
print("")
print(OS)
print("")
print("--------------------------------")
print("")
print(Machine)
print("")
print("--------------------------------")
print("")
print(as.data.frame(Sys.info()))
print("")
print(list.files(recursive = TRUE))
print("")
print("--------------------------------")
print("")
system("pwd")
print("")
print("--------------------------------")
print("")



root_path = "./" 
print("")
print("--------------------------------")
if (str_detect(string  = Machine,
               pattern = "mandrenke"))
{
    root_path = "~/GitHub/LOCA_CHEYENNE_THORNTHWAITE_MAP_WEB/" 
}

if (str_detect(string  = Machine,
               pattern = "kyrill"))
{
    root_path = "~/GitHub/LOCA_CHEYENNE_THORNTHWAITE_MAP_WEB/" 
}


#
##################################################################


##################################################################
#
# Geospatial Data
#
# Cracking the Natural Map Boundary Set for the first-level subdivisions of 
# the US and Canada (states and provinces)

us_states = map_data(map = "state")
counties  = map_data(map = "county")

dem_file_location = str_c(root_path,
                          "gtopo30_016_Cheyenne.RData",
                          sep="") 

load(file = dem_file_location)


awc_file_location =  str_c(root_path,
                           "AWC_Cheyenne.RData",
                           sep="") 

load(file = awc_file_location)


# Our domain boundary for our maps

min_lat =   min(dem_frame$lat) 
max_lat =   max(dem_frame$lat) 

min_lon  =   min(dem_frame$lon) 
max_lon  =   max(dem_frame$lon) 

#
##################################################################

##################################################################
#
# Get Variable List and LUT

variable_metadata_file = str_c(root_path,
                               "Thornthwaite_Budget_Parameters.RData",
                               sep = "")

load(file = variable_metadata_file)
#
##################################################################



##################################################################
#
# Get User Controlled Parameters - Emulating Pull-down Menus

input = NULL
    input$period_length     = "30" # climatology period years
    input$selected_variable = "monthly_precipitation"
    input$baseline_period   = "1976-2005"
    input$examined_period   = "2036-2065"


period_file_location = str_c(root_path,
                             "Cheyenne_climatology_years_",
                             input$period_length,
                             "-yr.RData",
                             sep="") 

load(file = period_file_location)

baseline_period_choices = climatology_period_info %>%
    filter(string_rcp45 == "Historical")

examined_period_choices = climatology_period_info %>%
    filter(string_rcp45 != "Historical")





#
###############################################################################
###############################################################################





###############################################################################
###############################################################################
##
## User Interface Function
##

ui <- fluidPage(

    # Application title
    titlePanel("Cheyenne Thornthwaite Mapping Tool"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p("Note: This app takes about 1-to-2 minutes to load the needed data, process it, and render it."),
            p("If running under the shinyapps.io domain, it may take even longer."),
            p("Changing settings will require data to be reloaded."),
            p("Be patient as the page re-draws."),
            
            selectInput(inputId  = "period_length",
                        label    = "Climatology Period Duration",
                        choices  = c(10,30),
                        selected = 30),
            selectInput(inputId  = "baseline_period",
                        label    = "Baseline Period",
                        choices  = baseline_period_choices$climatology_period,
                        selected = baseline_period_choices$climatology_period[1]),
            selectInput(inputId  = "examined_period",
                        label    = "Examined Period",
                        choices  = examined_period_choices$climatology_period,
                        selected = examined_period_choices$climatology_period[1]),
            selectInput(inputId  = "selected_variable",
                        label    = "Variable to Plot",
                        choices  = thornthwaite_variable_list$variable,
                        selected = thornthwaite_variable_list$variable[4]),

        ),

        
        mainPanel(
            h2("Terrain and Available Water Conent"),
            
            splitLayout(
                # Show a plot of the Terrain Map
                plotOutput(outputId = "terrainPlot"), 
                
                # Show a plot of the AWC Map
                plotOutput(outputId = "awcPlot")
            ),
            
            

 
 
            h2(textOutput("variable_name")),
            
            # Show a plot of the Historical Map
            h3("Historical Case"),
            plotOutput(outputId = "histPlots"), 
            
            # Show a plot of the Future RCP 4.5 Case Map
            h3("Future RCP 4.5 Case"),
            plotOutput(outputId = "rcp45Plots"), 
            
            
            # Show a plot of the Future RCP 8.5 Case Map
            h3("Future RCP 8.5 Case"),
            plotOutput(outputId = "rcp85Plots"), 
            
            
            # Show a plot of the Future RCP 4.5 vs Historical Anomaly Map
            h3("Future RCP 4.5 vs Historical Anomaly"),
            plotOutput(outputId = "Anom45Plots"), 

            
            # Show a plot of the Future RCP 8.5 vs Historical Anomaly Map
            h3("Future RCP 8.5 vs Historical Anomaly"),
            plotOutput(outputId = "Anom85Plots"), 
            
        )
    )
)

#
###############################################################################
###############################################################################




###############################################################################
###############################################################################
##
## Server Function
##

server <- function(input, output, session) {

    ###############################################################################
    #
    # Update Your Lookup Selectors Periods
    #
    
    observeEvent(input$period_length,  {
        
        period_file_location = str_c(root_path,
                                     "Cheyenne_climatology_years_",
                                     input$period_length,
                                     "-yr.RData",
                                     sep="") 
        
        load(file = period_file_location)

        baseline_period_choices = climatology_period_info %>%
            filter(string_rcp45 == "Historical")

        examined_period_choices = climatology_period_info %>%
            filter(string_rcp45 != "Historical")
        
        updateSelectInput(session = session, 
                          inputId = "baseline_period", 
                          choices = baseline_period_choices$climatology_period)
        
        updateSelectInput(session = session, 
                          inputId = "examined_period", 
                          choices = examined_period_choices$climatology_period)
    })
    
    #
    ###############################################################################
    
    
        
    ###############################################################################
    #
    # Update target_variable
    #
    
    target_variable <- reactive({ 
        target_variable = str_c("mean_",
                                input$selected_variable,
                                sep = "")
        return(target_variable)
        })
    
    #
    ###############################################################################    
    
    ###############################################################################
    #
    # Update stdev_variable
    #
    
    stdev_variable <- reactive({ 

        stdev_variable = str_c("stdev_",
                                input$selected_variable,
                                sep = "")
        return(stdev_variable)
        })
    
    #
    ###############################################################################
    
    ###############################################################################
    #
    # Update color tables
    #
    
    colortables <- reactive({ 
        

        
        
        # colour palate selection
        
        # default 
        
        colortables = NULL
        
        colortables$ensavg_palette  = "Spectral"
        colortables$ensavg_direction = 1
        
        colortables$anom_palette    = "BrBG"
        colortables$anom_direction   = 1
        
        if (str_detect(string = input$selected_variable, pattern = "emperature")) {
            colortables$ensavg_palette  = "Spectral"
            colortables$ensavg_direction = -1
            
            colortables$anom_palette    = "RdBu"
            colortables$anom_direction   = -1
        }
        
        
        # drying (deficit)
        
        if (str_detect(string = input$selected_variable, pattern = "deficit") | 
            str_detect(string = input$selected_variable, pattern = "evap") ) {
            colortables$ensavg_palette   = "YlOrRd"
            colortables$ensavg_direction = 1
            
            colortables$anom_palette     = "BrBG"
            colortables$anom_direction   = -1  
        }
        
        
        # recharge & surplus & storage 
        
        if (str_detect(string = input$selected_variable, pattern = "recharge") |
            str_detect(string = input$selected_variable, pattern = "surplus")  |
            str_detect(string = input$selected_variable, pattern = "storage")  |
            str_detect(string = input$selected_variable, pattern = "recipit")) {
            
            colortables$ensavg_palette   = "YlGnBu"
            colortables$ensavg_direction = 1
            
            colortables$anom_palette     = "BrBG"
            colortables$anom_direction   = 1 
        }
        
        # snowmelt 
        
        if (str_detect(string = input$selected_variable, pattern = "snow"))  {
            colortables$ensavg_palette   = "PuBu"
            colortables$ensavg_direction = 1
            
            colortables$colortables$anom_palette     = "RdBu"
            colortables$colortables$anom_direction   = 1 
        }

        
        return(colortables)
        
        
    })
    
    #
    ###############################################################################
    
    ###############################################################################
    #
    # Update Baseline Data Frame
    #
    
    ensemble_average_data_frame <- reactive({
        
        data_file_location = str_c(root_path,
                                   "/local_climatologies/",
                                   "LOCA_THORNTHWAITE_ALL_ENSEMBLES_CLIM_",
                                   input$period_length,
                                   "-yr.nc", 
                                   sep = "")

        
        baseline_and_future_variable_frame = tidync(x = data_file_location)  %>% 
            activate(target_variable(), 
                     stdev_variable()) %>% 
            hyper_tibble(select_var = c(target_variable(),
                                        stdev_variable())) %>%
            rename(value = target_variable(),
                   stdv  = stdev_variable()) %>%
            filter(!is.na(value)) %>%
            mutate(scenario           = as.factor(scenario),
                   ensemble           = as.factor(ensemble),
                   climatology_period = as.factor(climatology_period),
                   month              = fct_recode(as.factor(month), 
                                                   January   =  "1",
                                                   February  =  "2",
                                                   March     =  "3",
                                                   April     =  "4",
                                                   May       =  "5",
                                                   June      =  "6",
                                                   July      =  "7",
                                                   August    =  "8",
                                                   September =  "9",
                                                   October   = "10",
                                                   November  = "11",
                                                   December  = "12") ) %>%
            filter((climatology_period == input$baseline_period) | 
                   (climatology_period == input$examined_period) )  %>%
            mutate(scenario = ifelse(test = (scenario == "HIST_RCP45") & 
                                            (climatology_period == input$baseline_period),
                                     yes  = "HIST",
                                     no   = ifelse(test = (scenario == "HIST_RCP45"),
                                                   yes  = "RCP45",
                                                   no   = "RCP85"))) %>%
            select(-c("climatology_period","stdv")) %>%
            mutate(baseline_period = input$baseline_period,
                   examined_period = input$examined_period)
        
        baseline_and_future_variable_frame = spread(data  = baseline_and_future_variable_frame,
                                                    key   = scenario,
                                                    value = value)
        
        
        baseline_and_future_variable_frame$anom_RCP45_m_HIST = baseline_and_future_variable_frame$RCP45 - baseline_and_future_variable_frame$HIST
        baseline_and_future_variable_frame$anom_RCP85_m_HIST = baseline_and_future_variable_frame$RCP85 - baseline_and_future_variable_frame$HIST
        

        ensemble_average_data_frame = baseline_and_future_variable_frame %>%
            group_by(lon,
                     lat,
                     month) %>%
            summarize(ensemble_mean_HIST  = mean(HIST),
                      ensemble_mean_RCP45 = mean(RCP45),
                      ensemble_mean_RCP85 = mean(RCP85),
                      ensemble_mean_A4mH  = mean(anom_RCP45_m_HIST),
                      ensemble_mean_A8mH  = mean(anom_RCP85_m_HIST),
                      sign_A4mH           = sum( sign(mean(anom_RCP45_m_HIST)) == sign(anom_RCP45_m_HIST) )/n(),
                      sign_A8mH           = sum( sign(mean(anom_RCP85_m_HIST)) == sign(anom_RCP85_m_HIST) )/n()) %>%
            ungroup()  
        
        ensemble_average_data_frame = ensemble_average_data_frame %>%
            mutate(baseline_period = input$baseline_period,
                   examined_period = input$examined_period)
        

        return(ensemble_average_data_frame)
        
    })

    #        
    ###############################################################################
    

    ###############################################################################
    #
    # Plot Available Terrain Content
    #
    
    output$terrainPlot <- renderPlot({
        ggplot(data = dem_frame)+
            aes(x    = lon, 
                y    = lat, 
                fill = elevation) +
            theme_bw() +
            labs(title    = "Cheyenne Basin",
                 subtitle =  dem_long_name,
                 fill      = "m") +
            xlab(label = "Longitude") +
            ylab(label = "Latitude") +
            coord_fixed(xlim    = c(min_lon, max_lon),
                        ylim    = c(min_lat, max_lat) ,
                        ratio   = 1/cos(pi/180 * mean(dem_frame$lat)) ) +
            scale_fill_gradientn(colors     = terrain.colors(10),
                                 space      = "Lab",
                                 na.value   = "white",
                                 aesthetics = "fill",
                                 limits     = c(min(dem_frame$elevation),
                                                max(dem_frame$elevation))) +
            geom_raster()  +
            geom_polygon(data    = us_states,
                         mapping = aes(x     =   long, 
                                       y     =    lat,
                                       group =  group),
                         fill    = NA, 
                         size    = 1.0, 
                         color   = "black")  +
            geom_polygon(data    = counties,
                         mapping = aes(x     =   long, 
                                       y     =    lat,
                                       group =  group),
                         size    = 0.25, 
                         fill    = NA, 
                         color   = "black")  
        
 

    })

    #
    ###############################################################################

    ###############################################################################
    #
    # Plot Available Soil Water Content
    #
    
    output$awcPlot <- renderPlot({
        ggplot(data = awc_frame)+
            aes(x    = lon, 
                y    = lat, 
                fill = usda_awc) +
            theme_bw() +
            labs(title    = "Cheyenne Basin",
                 subtitle =  "Max Available Soil Water Storage",
                 fill      = "mm") +
            xlab(label = "Longitude") +
            ylab(label = "Latitude") +
            coord_fixed(xlim    = c(min_lon, max_lon),
                        ylim    = c(min_lat, max_lat) ,
                        ratio   = 1/cos(pi/180 * mean(awc_frame$lat)) ) +
            scale_fill_distiller(palette    = "YlGnBu",
                                 space      = "Lab",
                                 direction  = 1,
                                 na.value   = "white",
                                 guide      = "colourbar",
                                 aesthetics = "fill",
                                 limits     = c(0,max(awc_frame$usda_awc))) +
            geom_raster()  +
            geom_polygon(data    = us_states,
                         mapping = aes(x     =   long, 
                                       y     =    lat,
                                       group =  group),
                         fill    = NA, 
                         size    = 1.0, 
                         color   = "black")  +
            geom_polygon(data    = counties,
                         mapping = aes(x     =   long, 
                                       y     =    lat,
                                       group =  group),
                         size    = 0.25, 
                         fill    = NA, 
                         color   = "black")  
    })    
    
    #
    ###############################################################################   

    
    
    ###############################################################################
    #
    # Plot Budget Maps
    
    #
    # Historical Map
    #
    
    output$histPlots <- renderPlot({

        variable_metadata = thornthwaite_variable_list %>%
            filter(variable == input$selected_variable)
        
        ensemble_mean_value_max = ceiling(max(c(ensemble_average_data_frame()$ensemble_mean_HIST,
                                                ensemble_average_data_frame()$ensemble_mean_RCP45,
                                                ensemble_average_data_frame()$ensemble_mean_RCP85)))
        
        ensemble_mean_value_min = floor(min(c(ensemble_average_data_frame()$ensemble_mean_HIST,
                                              ensemble_average_data_frame()$ensemble_mean_RCP45,
                                              ensemble_average_data_frame()$ensemble_mean_RCP85)))
        
        ensemble_mean_value_limits = c(ensemble_mean_value_min,ensemble_mean_value_max)
        
        
        
        ensemble_mean_anom_abs_max  = ceiling(max(c(abs(ensemble_average_data_frame()$ensemble_mean_A4mH),
                                                    abs(ensemble_average_data_frame()$ensemble_mean_A8mH))))
        
        
        ensemble_mean_anom_limits = ensemble_mean_anom_abs_max * c(-1,1)
        
        
        ggplot(data = ensemble_average_data_frame()) +
            aes(x    = lon, 
                y    = lat, 
                fill = ensemble_mean_HIST) +
            theme_bw() +
            theme(strip.background = element_rect(fill=NA),
                  axis.title.x     = element_blank(),
                  axis.text.x      = element_blank(),
                  axis.ticks.x     = element_blank(),
                  axis.title.y     = element_blank(),
                  axis.text.y      = element_blank(),
                  axis.ticks.y     = element_blank()) +
            facet_wrap(facets = "month") +
            labs(title    = "Cheyenne Basin Thornthwaite Budgets",
                 subtitle =  str_c(variable_metadata$long_name,
                                   ";  Historical (",
                                   input$baseline_period, 
                                   ")",
                                   sep = ""),
                 caption = "CMIP5 LOCA Ensemble Means",
                 fill    = variable_metadata$units) +
            xlab(label = "Longitude") +
            ylab(label = "Latitude") +
            coord_fixed(xlim    = c(min_lon, max_lon),
                        ylim    = c(min_lat, max_lat) ,
                        ratio   = 1/cos(pi/180 * mean(dem_frame$lat)) ) +
            scale_fill_distiller(palette    = colortables()$ensavg_palette,
                                 space      = "Lab",
                                 direction  = colortables()$ensavg_direction,
                                 na.value   = "white",
                                 guide      = "colourbar",
                                 aesthetics = "fill",
                                 limits     = ensemble_mean_value_limits) +
            geom_raster()  +
            geom_polygon(data    = us_states,
                         mapping = aes(x     =   long, 
                                       y     =    lat,
                                       group =  group),
                         fill    = NA, 
                         size    = 0.5, 
                         color   = "black")  +
            geom_polygon(data    = counties,
                         mapping = aes(x     =   long, 
                                       y     =    lat,
                                       group =  group),
                         size    = 0.2, 
                         fill    = NA, 
                         color   = "black") 
    })

    #
    # RCP 4.5 Map
    #   
    
    output$rcp45Plots <- renderPlot({
        
        
        variable_metadata = thornthwaite_variable_list %>%
            filter(variable == input$selected_variable)
        
        ensemble_mean_value_max = ceiling(max(c(ensemble_average_data_frame()$ensemble_mean_HIST,
                                                ensemble_average_data_frame()$ensemble_mean_RCP45,
                                                ensemble_average_data_frame()$ensemble_mean_RCP85)))
        
        ensemble_mean_value_min = floor(min(c(ensemble_average_data_frame()$ensemble_mean_HIST,
                                              ensemble_average_data_frame()$ensemble_mean_RCP45,
                                              ensemble_average_data_frame()$ensemble_mean_RCP85)))
        
        ensemble_mean_value_limits = c(ensemble_mean_value_min,ensemble_mean_value_max)
        
        
        
        ensemble_mean_anom_abs_max  = ceiling(max(c(abs(ensemble_average_data_frame()$ensemble_mean_A4mH),
                                                    abs(ensemble_average_data_frame()$ensemble_mean_A8mH))))
        
        
        ensemble_mean_anom_limits = ensemble_mean_anom_abs_max * c(-1,1)
        
        
        
        ggplot(data = ensemble_average_data_frame())+
            aes(x    = lon, 
                y    = lat, 
                fill = ensemble_mean_RCP45) +
            theme_bw() +
            theme(strip.background = element_rect(fill=NA),
                  axis.title.x     = element_blank(),
                  axis.text.x      = element_blank(),
                  axis.ticks.x     = element_blank(),
                  axis.title.y     = element_blank(),
                  axis.text.y      = element_blank(),
                  axis.ticks.y     = element_blank()) +
            facet_wrap(facets = "month") +
            labs(title    = "Cheyenne Basin Thornthwaite Budgets",
                 subtitle =  str_c(variable_metadata$long_name,
                                   ";  RCP 4.5 (",
                                   input$examined_period, 
                                   ")",
                                   sep = ""),
                 caption = "CMIP5 LOCA Ensemble Means",
                 fill    = variable_metadata$units) +
            xlab(label = "Longitude") +
            ylab(label = "Latitude") +
            coord_fixed(xlim    = c(min_lon, max_lon),
                        ylim    = c(min_lat, max_lat) ,
                        ratio   = 1/cos(pi/180 * mean(dem_frame$lat)) ) +
            scale_fill_distiller(palette    = colortables()$ensavg_palette,
                                 space      = "Lab",
                                 direction  = colortables()$ensavg_direction,
                                 na.value   = "white",
                                 guide      = "colourbar",
                                 aesthetics = "fill",
                                 limits     = ensemble_mean_value_limits) +
            geom_raster()  +
            geom_polygon(data    = us_states,
                         mapping = aes(x     =   long, 
                                       y     =    lat,
                                       group =  group),
                         fill    = NA, 
                         size    = 0.5, 
                         color   = "black")  +
            geom_polygon(data    = counties,
                         mapping = aes(x     =   long, 
                                       y     =    lat,
                                       group =  group),
                         size    = 0.2, 
                         fill    = NA, 
                         color   = "black") 
    })
 
    #
    # RCP 8.5 Map
    #      
    
    output$rcp85Plots <- renderPlot({
        

        
        variable_metadata = thornthwaite_variable_list %>%
            filter(variable == input$selected_variable)
        
        ensemble_mean_value_max = ceiling(max(c(ensemble_average_data_frame()$ensemble_mean_HIST,
                                                ensemble_average_data_frame()$ensemble_mean_RCP45,
                                                ensemble_average_data_frame()$ensemble_mean_RCP85)))
        
        ensemble_mean_value_min = floor(min(c(ensemble_average_data_frame()$ensemble_mean_HIST,
                                              ensemble_average_data_frame()$ensemble_mean_RCP45,
                                              ensemble_average_data_frame()$ensemble_mean_RCP85)))
        
        ensemble_mean_value_limits = c(ensemble_mean_value_min,ensemble_mean_value_max)
        
        
        
        ensemble_mean_anom_abs_max  = ceiling(max(c(abs(ensemble_average_data_frame()$ensemble_mean_A4mH),
                                                    abs(ensemble_average_data_frame()$ensemble_mean_A8mH))))
        
        
        ensemble_mean_anom_limits = ensemble_mean_anom_abs_max * c(-1,1)
        
        
        ggplot(data = ensemble_average_data_frame())+
            aes(x    = lon, 
                y    = lat, 
                fill = ensemble_mean_RCP85) +
            theme_bw() +
            theme(strip.background = element_rect(fill=NA),
                  axis.title.x     = element_blank(),
                  axis.text.x      = element_blank(),
                  axis.ticks.x     = element_blank(),
                  axis.title.y     = element_blank(),
                  axis.text.y      = element_blank(),
                  axis.ticks.y     = element_blank()) +
            facet_wrap(facets = "month") +
            labs(title    = "Cheyenne Basin Thornthwaite Budgets",
                 subtitle =  str_c(variable_metadata$long_name,
                                   ";  RCP 8.5 (",
                                   input$examined_period, 
                                   ")",
                                   sep = ""),
                 caption = "CMIP5 LOCA Ensemble Means",
                 fill    = variable_metadata$units) +
            xlab(label = "Longitude") +
            ylab(label = "Latitude") +
            coord_fixed(xlim    = c(min_lon, max_lon),
                        ylim    = c(min_lat, max_lat) ,
                        ratio   = 1/cos(pi/180 * mean(dem_frame$lat)) ) +
            scale_fill_distiller(palette    = colortables()$ensavg_palette,
                                 space      = "Lab",
                                 direction  = colortables()$ensavg_direction,
                                 na.value   = "white",
                                 guide      = "colourbar",
                                 aesthetics = "fill",
                                 limits     = ensemble_mean_value_limits) +
            geom_raster()  +
            geom_polygon(data    = us_states,
                         mapping = aes(x     =   long, 
                                       y     =    lat,
                                       group =  group),
                         fill    = NA, 
                         size    = 0.5, 
                         color   = "black")  +
            geom_polygon(data    = counties,
                         mapping = aes(x     =   long, 
                                       y     =    lat,
                                       group =  group),
                         size    = 0.2, 
                         fill    = NA, 
                         color   = "black") 
    })   
  
    #
    # RCP 4.5 vs Historical Anomaly Map
    #     
  
    output$Anom45Plots <- renderPlot({
        

        
        variable_metadata = thornthwaite_variable_list %>%
            filter(variable == input$selected_variable)
        
        ensemble_mean_value_max = ceiling(max(c(ensemble_average_data_frame()$ensemble_mean_HIST,
                                                ensemble_average_data_frame()$ensemble_mean_RCP45,
                                                ensemble_average_data_frame()$ensemble_mean_RCP85)))
        
        ensemble_mean_value_min = floor(min(c(ensemble_average_data_frame()$ensemble_mean_HIST,
                                              ensemble_average_data_frame()$ensemble_mean_RCP45,
                                              ensemble_average_data_frame()$ensemble_mean_RCP85)))
        
        ensemble_mean_value_limits = c(ensemble_mean_value_min,ensemble_mean_value_max)
        
        
        
        ensemble_mean_anom_abs_max  = ceiling(max(c(abs(ensemble_average_data_frame()$ensemble_mean_A4mH),
                                                    abs(ensemble_average_data_frame()$ensemble_mean_A8mH))))
        
        
        ensemble_mean_anom_limits = ensemble_mean_anom_abs_max * c(-1,1)
        
        
        ggplot(data = ensemble_average_data_frame())+
            aes(x    = lon, 
                y    = lat, 
                fill = ensemble_mean_A4mH) +
            theme_bw() +
            theme(strip.background = element_rect(fill=NA),
                  axis.title.x     = element_blank(),
                  axis.text.x      = element_blank(),
                  axis.ticks.x     = element_blank(),
                  axis.title.y     = element_blank(),
                  axis.text.y      = element_blank(),
                  axis.ticks.y     = element_blank()) +
            facet_wrap(facets = "month") +
            labs(title    = "Cheyenne Basin Thornthwaite Budget Anomalies",
                 subtitle =  str_c(variable_metadata$long_name,
                                   ";  RCP 4.5 (",
                                   input$examined_period, 
                                   ") - (",
                                   input$baseline_period,
                                   ")",
                                   sep = ""),
                 caption = "CMIP5 LOCA Ensemble Means",
                 fill    = variable_metadata$units) +
            xlab(label = "Longitude") +
            ylab(label = "Latitude") +
            coord_fixed(xlim    = c(min_lon, max_lon),
                        ylim    = c(min_lat, max_lat) ,
                        ratio   = 1/cos(pi/180 * mean(dem_frame$lat)) ) +
            scale_fill_distiller(palette    = colortables()$anom_palette,
                                 space      = "Lab",
                                 direction  = colortables()$anom_direction,
                                 na.value   = "white",
                                 guide      = "colourbar",
                                 aesthetics = "fill",
                                 limits     = ensemble_mean_anom_limits) +
            geom_raster()  +
            geom_polygon(data    = us_states,
                         mapping = aes(x     =   long, 
                                       y     =    lat,
                                       group =  group),
                         fill    = NA, 
                         size    = 0.5, 
                         color   = "black")  +
            geom_polygon(data    = counties,
                         mapping = aes(x     =   long, 
                                       y     =    lat,
                                       group =  group),
                         size    = 0.2, 
                         fill    = NA, 
                         color   = "black") 
    })   
 
    #
    # RCP 8.5 vs Historical Anomaly Map
    #
    
    output$Anom85Plots <- renderPlot({
        

        
        variable_metadata = thornthwaite_variable_list %>%
            filter(variable == input$selected_variable)
        
        ensemble_mean_value_max = ceiling(max(c(ensemble_average_data_frame()$ensemble_mean_HIST,
                                                ensemble_average_data_frame()$ensemble_mean_RCP45,
                                                ensemble_average_data_frame()$ensemble_mean_RCP85)))
        
        ensemble_mean_value_min = floor(min(c(ensemble_average_data_frame()$ensemble_mean_HIST,
                                              ensemble_average_data_frame()$ensemble_mean_RCP45,
                                              ensemble_average_data_frame()$ensemble_mean_RCP85)))
        
        ensemble_mean_value_limits = c(ensemble_mean_value_min,ensemble_mean_value_max)
        
        
        
        ensemble_mean_anom_abs_max  = ceiling(max(c(abs(ensemble_average_data_frame()$ensemble_mean_A4mH),
                                                    abs(ensemble_average_data_frame()$ensemble_mean_A8mH))))
        
        
        ensemble_mean_anom_limits = ensemble_mean_anom_abs_max * c(-1,1)
        
        
        ggplot(data = ensemble_average_data_frame())+
            aes(x    = lon, 
                y    = lat, 
                fill = ensemble_mean_A8mH) +
            theme_bw() +
            theme(strip.background = element_rect(fill=NA),
                  axis.title.x     = element_blank(),
                  axis.text.x      = element_blank(),
                  axis.ticks.x     = element_blank(),
                  axis.title.y     = element_blank(),
                  axis.text.y      = element_blank(),
                  axis.ticks.y     = element_blank()) +
            facet_wrap(facets = "month") +
            labs(title    = "Cheyenne Basin Thornthwaite Budget Anomalies",
                 subtitle =  str_c(variable_metadata$long_name,
                                   ";  RCP 8.5 (",
                                   input$examined_period, 
                                   ") - (",
                                   input$baseline_period,
                                   ")",
                                   sep = ""),
                 caption = "CMIP5 LOCA Ensemble Means",
                 fill    = variable_metadata$units) +
            xlab(label = "Longitude") +
            ylab(label = "Latitude") +
            coord_fixed(xlim    = c(min_lon, max_lon),
                        ylim    = c(min_lat, max_lat) ,
                        ratio   = 1/cos(pi/180 * mean(dem_frame$lat)) ) +
            scale_fill_distiller(palette    = colortables()$anom_palette,
                                 space      = "Lab",
                                 direction  = colortables()$anom_direction,
                                 na.value   = "white",
                                 guide      = "colourbar",
                                 aesthetics = "fill",
                                 limits     = ensemble_mean_anom_limits) +
            geom_raster()  +
            geom_polygon(data    = us_states,
                         mapping = aes(x     =   long, 
                                       y     =    lat,
                                       group =  group),
                         fill    = NA, 
                         size    = 0.5, 
                         color   = "black")  +
            geom_polygon(data    = counties,
                         mapping = aes(x     =   long, 
                                       y     =    lat,
                                       group =  group),
                         size    = 0.2, 
                         fill    = NA, 
                         color   = "black") 
    })   
    
    #
    ###############################################################################   
    
    
    output$variable_name <- renderText(expr = {
        
        variable_metadata = thornthwaite_variable_list %>%
            filter(variable == input$selected_variable)
        
        str_c("Maps for ",
              variable_metadata$long_name,
              sep = " ")}
        )
    
    
    
    ###############################################################################
    #
    # Display Thornthwaite Budgets as a Table
    #
    
    output$budget_table =  renderDataTable( ensemble_average_data_frame() )
    
    #
    ###############################################################################
    
 
    
    
}

#
###############################################################################
###############################################################################















###############################################################################
###############################################################################
##
## shinyApp Function
##

# Run the application 
shinyApp(ui     = ui, 
         server = server)

##
###############################################################################
###############################################################################

