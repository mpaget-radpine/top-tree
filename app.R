# install packages used
# install.packages('shiny')
# install.packages('readxl')
# install.packages('tidyverse')
# install.packages('plotly')
# install.packages('shinyWidgets')
# install.packages('shinydashboard')
# install.packages('shinyBS')
# install.packages('hover')
# install.packages('png')
# install.packages('viridis')
# install.packages('qpcR')
# install.packages('fontawesome')
# install.packages('DT')
# install.packages('base')
# install.packages('imputeTS')
# install.packages("writexl")
# install.packages("shinycssloaders")
# install.packages("data.table")      



# Load packages
library(shiny)
library(readxl)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(shinyBS)
library(hover)
library(png)
library(viridis)
library(qpcR)
library(fontawesome)
library(DT)
library(base)
library(imputeTS)
library(writexl)
library(shinycssloaders)
library(shinyjs)
library(data.table)

# ------------------------------------------------------------------
# Fixed files and data cleaning that do not rely on app inputs
# -----------------------------------------------------------------
# read in files to set sliders up with
economic_weights <-
  read_excel("www/input_files/Economic weights.xlsx") %>%
  as.data.frame()
colnames(economic_weights) <- c('trait', 'unit', 'base_value',
                                'min', 'max', 'date_updated')
objective_traits <- read.csv("www/input_files/objective_traits.csv")

# Replace white space and corrupted characters
economic_weights$trait <- gsub(' ', '', economic_weights$trait)
economic_weights$trait <- gsub('\u00a0', '', economic_weights$trait)

# get EV info for each trait
economic_weight_diameter        <-
  economic_weights %>% filter(trait == 'Diameterbyheight')
economic_weight_straightness    <-
  economic_weights %>% filter(trait == 'Straightness')
economic_weight_branching_habit <-
  economic_weights %>% filter(trait == 'Branchinghabit')
economic_weight_density         <-
  economic_weights %>% filter(trait == "Density")
economic_weight_moe             <-
  economic_weights %>% filter(trait == "MoE")
economic_weight_dothistroma     <-
  economic_weights %>% filter(trait == "Dothistroma")

# get objective value info for each trait
objective_trait_volume    <-
  objective_traits %>% filter(objective_trait == 'Volume')
objective_trait_bix       <-
  objective_traits %>% filter(objective_trait == 'Branching index (BIX)')
objective_trait_density   <-
  objective_traits %>% filter(objective_trait == 'Density')
objective_trait_stiffness <-
  objective_traits %>% filter(objective_trait == "Stiffness")

# Read in Breeding values
breeding_values <- read.csv("www/input_files/Breeding_values.csv") %>%
  # arrange in rank order
  arrange(Rank) %>%
  # take the best 1600 ortets based on original index as discussed
  slice(1:1600) %>%
  dplyr::select(-Index, -Rank)

# rename accuracy columns as they are the same in ebv and gbv
colnames(breeding_values)[which(names(breeding_values) == 'acc_dbh')]   <-
  'acc_dbh_ebv'
colnames(breeding_values)[which(names(breeding_values) == 'acc_brh')]   <-
  'acc_brh_ebv'
colnames(breeding_values)[which(names(breeding_values) == 'acc_str')]   <-
  'acc_str_ebv'
colnames(breeding_values)[which(names(breeding_values) == 'acc_den')]   <-
  'acc_den_ebv'
colnames(breeding_values)[which(names(breeding_values) == 'acc_pme')]   <-
  'acc_pme_ebv'
colnames(breeding_values)[which(names(breeding_values) == 'acc_dothi')] <-
  'acc_dothi_ebv'

# Read in info for if the line is available for purchase
available_for_purchase <-
  read_excel("www/input_files/Entity data - available for purchase.xlsx")
available_for_purchase <- available_for_purchase %>%
  dplyr::select(Ortet, Available, Arbogen, `PF Olsen`, Proseed)

# Read in pedigree file
draft_pedigree  <-
  read.csv("www/input_files/Draft Pedigree file.csv")

# Merge the breeding values and if available for purchase
complete_breeding_values_purchase <- full_join(breeding_values,
                                               available_for_purchase,
                                               by = c('Ortet'))

# Merge the pedigree values with breeding & purchase data
complete_breeding_values_purchase_pedigree <-
  full_join(complete_breeding_values_purchase,
            draft_pedigree,
            by = c('Ortet' = 'Tree'))

# Read in Breeding value mapping file
# This has the dates the files where generated
dates <- read_excel("www/input_files/BV mapping file 1.xlsx",
                    col_names = F)
colnames(dates) <- c('file_name', 'file type', 'date', 'unknown')

# ==============================================================================

# Define UI
ui <- fluidPage(
  # Allow use of hover
  use_hover(),
  
  # Allow use of shinjs to enable/disable download button
  shinyjs::useShinyjs(),
  
  # set up app style
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
  ),
  
  HTML(
    "<nav class=\"navbar navbar-light bg-light\">
             <div class=\"container-fluid\">
             <a class=\"navbar-brand\">
             <img src=\"RPBC_logo.png\" width=\"90\" height=\"30\" class=\"d-inline-block align-top\">
             </a>
             <div style='float:left;'> <h1 class=\"navbar-heading\"> TopTree </h1></div>
             <div style='float:right;'> <h4 class=\"navbar-brand\"> Radiata Pine index ranking application </h4></div>
             </div>
             </nav>"
  ),
  tags$head(tags$style(
    HTML(".shiny-output-error-validation {font-weight: bold;}")
  )),
  
  # Loading message when app first starts up
  div(id = "loading-content",
      h2("Loading TopTree...", align = 'center')),
  
  # index value table at top of app page
  tags$div(
    class = "container-fluid page",
    # Name of table
    fluidRow(column(
      12, column(
        12,
        class = "well",
        h3("Selection age weights used in index calculation", align = 'center'),
        DT::dataTableOutput('EV_table')
      )
    )),
    
    # selection button row for traits and providers
    fluidRow(class = "main-content",
             column(
               6,
               column(
                 12,
                 align = 'Center',
                 class = "col well",
                 class = "well economic-values",
                 h3("Chart Filters", align = 'center'),
                 h6(paste0('Breeding Values updated on ', dates$date[1]), 
                    align = 'center'),
                 tags$head(
                   tags$style("#economic_or_genomic {border: 2px solid transparent;}")
                 ),
                 
                 # check buttons to select which traits are wanted in the index
                 h4("Select traits in index"),
                 checkboxGroupButtons(
                   inputId = "trait_selection",
                   choices = c(
                     "DBH",
                     "Branching Habit",
                     "Straightness",
                     "Corewood Density",
                     "MoE"
                   ),
                   selected = c(
                     "DBH",
                     "Straightness",
                     "Branching Habit",
                     "Corewood Density",
                     "MoE"
                   ),
                   justified = F,
                   individual = T,
                   checkIcon = list(yes = icon("check"))
                 ),
               )
             ),
             column(
               6,
               column(
                 12,
                 align = 'Center',
                 class = "col well",
                 class = "well economic-values",
                 # check boxes to select on purchase availability
                 h3("Providers", align = 'center'),
                 h4("Select availability"),
                 fluidRow(column(
                   width = 12,
                   align = 'center',
                   radioGroupButtons(
                     inputId = "availability_select",
                     label = NULL,
                     choices = c("All Ortets",
                                 "Only Available Ortets"),
                     individual = TRUE,
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-circle",
                                    style = "color: #c4c4c4"),
                       no = tags$i(class = "fa fa-circle-o",
                                   style = "color: #c4c4c4")
                     )
                   )
                 )),
                 h4("Select providers in plot"),
                 fluidRow(column(
                   width = 12,
                   align = 'center',
                   radioGroupButtons(
                     inputId = "provider_select",
                     label = NULL,
                     choices = c("All providers",
                                 "ArborGen",
                                 "Proseed",
                                 "PF Olsen"),
                     individual = TRUE,
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-circle",
                                    style = "color: #c4c4c4"),
                       no = tags$i(class = "fa fa-circle-o",
                                   style = "color: #c4c4c4")
                     )
                   )
                 )),
               )
             )),
    
    # Sidebar with Objective traits sliders
    fluidRow(
      class = "main-content",
      column(
        3,
        column(
          12,
          align = 'Center',
          class = "col well",
          class = "well economic-values",
          h3("Harvest age traits", align = 'center'),
          # Button to reset EV to baseline levels
          fluidRow(column(
            width = 12,
            actionBttn(
              inputId = "RESET_OBJECTIVE",
              icon = icon("refresh"),
              color = "primary",
              size = 'sm'
            ),
            actionBttn(
              inputId = "SHOW_HELP_OBJECTIVE",
              icon = icon("question"),
              color = "primary",
              size = 'sm'
            ),
            align = 'right'
          )),
          # reset objective traits button
          bsTooltip(
            id = "RESET_OBJECTIVE",
            title = "Reset values",
            placement = "bottom"
          ),
          # show help objective traits button
          bsTooltip(
            id = "SHOW_HELP_OBJECTIVE",
            title = "Help",
            placement = "bottom"
          ),
          # action button to update economic values based on objective traits
          br(),
          actionButton(
            "update_evs",
            style = 'font-size:120%',
            label =  HTML("<b>Update Selection age weights</b>")
          ),
          br(),
          br(),
          fluidRow(column(width = 7, h4("Volume", align = 'left')),
                   column(
                     width = 5, align = 'right',
                     h5(HTML(paste(
                       "($/m", tags$sup(3), ' ha)', sep = ""
                     )),
                     align = 'right')
                   )),
          
          # These numeric values might need hard coding depending on how info 
          # needs to be read into app
          sliderInput(
            "volume_objective",
            NULL,
            min = objective_trait_volume$Range_min,
            max = objective_trait_volume$Range_max,
            value = objective_trait_volume$EW_Obj_val,
            step = 1,
            ticks = F
          ),
          
          fluidRow(column(width = 7, h4("Density", align = 'left')),
                   column(
                     width = 5, align = 'right', h5(HTML(paste(
                       "($/kg m", tags$sup(3), ')', sep = ""
                     )),
                     align = 'right')
                   )),
          
          sliderInput(
            "density_objective",
            NULL,
            min = objective_trait_density$Range_min,
            max = objective_trait_density$Range_max,
            value = objective_trait_density$EW_Obj_val,
            step = 1,
            ticks = F
          ),
          
          fluidRow(column(width = 7, h4("Stiffness", align = 'left')),
                   column(
                     width = 5,
                     align = 'right',
                     h5("$/GPa", align = 'right')
                   )),
          
          sliderInput(
            "stiffness_objective",
            NULL,
            min = objective_trait_stiffness$Range_min,
            max = objective_trait_stiffness$Range_max,
            value = objective_trait_stiffness$EW_Obj_val,
            step = 1,
            ticks = F
          ),
          
          fluidRow(column(
            width = 7, h4("Branching index (BIX)", align = 'left')
          ),
          column(
            width = 5, align = 'right', h5("$/mm", align = 'right')
          )),
          
          sliderTextInput(
            inputId = "BIX_objective",
            label = NULL,
            choices = seq(
              from = objective_trait_bix$Range_min,
              to = objective_trait_bix$Range_max,
              by = -0.5
            ),
            selected = objective_trait_bix$EW_Obj_val,
            grid = F
          )
          
        )
      ),
      # sidebarPanel bracket
      
      # Sidebar with economic value sliders
      column(
        3,
        column(
          12,
          align = 'Center',
          class = "col well",
          class = "well economic-values",
          h3("Selection age weights", align = 'center'),
          
          # Button to reset EV to baseline levels
          fluidRow(column(
            width = 12,
            actionBttn(
              inputId = "RESET_EV",
              icon = icon("refresh"),
              color = "primary",
              size = 'sm'
            ),
            actionBttn(
              inputId = "SHOW_HELP",
              icon = icon("question"),
              color = "primary",
              size = 'sm'
            ),
            align = 'right'
          )),
          # reset Ev to baseline button
          bsTooltip(
            id = "RESET_EV",
            title = "Reset values",
            placement = "bottom"
          ),
          # show help on EVs button
          bsTooltip(
            id = "SHOW_HELP",
            title = "Help",
            placement = "bottom"
          ),
          
          # action button to update objective traits based on economic values
          br(),
          actionButton(
            "update_objvs",
            style = 'font-size:120%',
            label =  HTML("<b>Update Harvest age traits</b>")
          ),
          br(),
          br(),
          # Sliders for EVs
          fluidRow(column(width = 7, h4("DBH", align = 'left')),
                   column(
                     width = 5,
                     align = 'right',
                     h5("($/mm)", align = 'right')
                   )),
          
          # Slider code
          sliderInput(
            "diameter",
            NULL,
            min = economic_weight_diameter$min,
            max = economic_weight_diameter$max,
            value = economic_weight_diameter$base_value,
            step = 0.01,
            ticks = F
          ),
          
          # Repeat same structure as above for other traits
          fluidRow(column(
            width = 7, h4("Branching Habit", align = 'left')
          ),
          column(
            width = 5,
            align = 'right',
            h5("($/Scale unit)", align = 'right')
          )),
          
          sliderInput(
            "branching_habit",
            NULL,
            min = economic_weight_branching_habit$min,
            max = economic_weight_branching_habit$max,
            value = economic_weight_branching_habit$base_value,
            # extra 0 in step stop rounding which shifts value to automatically
            # be greater than baseline
            step = 133.51,
            ticks = F
          ),
          
          fluidRow(column(width = 7, h4(
            "Straightness", align = 'left'
          )),
          column(
            width = 5,
            align = 'right',
            h5("($/Scale unit)", align = 'right')
          )),
          
          sliderTextInput(
            inputId = "straightness",
            label = NULL,
            choices = seq(
              from = economic_weight_straightness$min,
              to = economic_weight_straightness$max,
              by = -0.01
            ),
            selected = economic_weight_straightness$base_value,
            grid = F
          ),
          
          fluidRow(column(
            width = 7, h4("Corewood Density", align = 'left')
          ),
          column(
            width = 5, align = 'right', h5(HTML(paste(
              "($/kg m", tags$sup(-3), ')', sep = ""
            )),
            align = 'right'),
          )),
          
          sliderInput(
            "density",
            NULL,
            min = economic_weight_density$min,
            max = economic_weight_density$max,
            value = economic_weight_density$base_value,
            step = 0.5,
            ticks = F
          ),
          
          fluidRow(column(width = 7, h4("MoE", align = 'left')),
                   column(
                     width = 5,
                     align = 'right',
                     h5("($/GPa)", align = 'right')
                   )),
          
          sliderInput(
            "moe",
            NULL,
            min = economic_weight_moe$min,
            max = economic_weight_moe$max,
            value = economic_weight_moe$base_value,
            step = 0.01,
            ticks = F
          )
        ),
      ),
      
      
      # Main panel of app
      column(
        6,
        column(
          12,
          align = 'Center',
          class = "col well",
          h3("Ortet rank according to index", align = 'center'),
          # background colour of table
          tags$style(
            HTML(
              'table.dataTable tr:nth-child(odd) {background-color: #F8F9F8 !important;}'
            )
          ),
          tags$style(
            HTML('table.dataTable th {background-color: white !important;}')
          ),
          
          # center everything
          align = "center",
          class = "well",
          
          # Index bar plot
          fluidRow(
            column(
              width = 12,
              align = 'Center',
              shinycssloaders::withSpinner(
                # Adds loading spinner
                plotlyOutput("bar_plot", height = "100%"),
                type = 5,
                color = "#72b9ab",
                size = 1
              )
            ),
          ),
        )
      )
    )
    
  ),
  
  # add download button
  uiOutput("download_btn_require"),
  
  # styling of page
  HTML(
    "<nav class=\"footer\">
           <div class=\"container-fluid\">
           <h5>Powered by AbacusBio<h5>
           </div>
           </nav>"
  ),
) # fluidPage bracket


#===============================================================================

# Define server logic
server <- function(input, output, session) {
  # Hide loading screen after app has initialized
  hide(id = "loading-content",
       anim = TRUE,
       animType = "fade")
  
  # Set up the reactive values needed in code
  reactive_plot_data <-
    reactiveValues(ranked    = as.data.frame(x = 20))
  reactive_plot_data <-
    reactiveValues(download  = as.data.frame(x = 20))
  reactive_plot_data <-
    reactiveValues(ev_inputs = as.data.frame(x = 6))
  
  # reset economic values back to baseline when button is pressed
  observeEvent(input$RESET_EV, {
    updateSliderInput(session, 'diameter',           
                      value = economic_weight_diameter$base_value)
    updateSliderTextInput(session, 'straightness',    
                          selected = economic_weight_straightness$base_value)
    updateSliderInput(session, 'branching_habit',     
                      value = economic_weight_branching_habit$base_value)
    updateSliderInput(session, 'density',             
                      value = economic_weight_density$base_value)
    updateSliderInput(session, 'moe',                 
                      value = economic_weight_moe$base_value)
    updateSliderInput(session, 'stiffness_objective', 
                      value = objective_trait_stiffness$EW_Obj_val)
    updateSliderInput(session, 'volume_objective',    
                      value = objective_trait_volume$EW_Obj_val)
    updateSliderInput(session, 'density_objective',   
                      value = objective_trait_density$EW_Obj_val)
    updateSliderTextInput(session, 'BIX_objective',   
                          selected = objective_trait_bix$EW_Obj_val)
  })
  
  # reset objective values back to baseline when button is pressed
  observeEvent(input$RESET_OBJECTIVE, {
    updateSliderInput(session, 'stiffness_objective', 
                      value = objective_trait_stiffness$EW_Obj_val)
    updateSliderInput(session, 'volume_objective',    
                      value = objective_trait_volume$EW_Obj_val)
    updateSliderInput(session, 'density_objective',   
                      value = objective_trait_density$EW_Obj_val)
    updateSliderTextInput(session, 'BIX_objective',   
                          selected = objective_trait_bix$EW_Obj_val)
    updateSliderInput(session, 'diameter',            
                      value = economic_weight_diameter$base_value)
    updateSliderTextInput(session, 'straightness',    
                          selected = economic_weight_straightness$base_value)
    updateSliderInput(session, 'branching_habit',     
                      value = economic_weight_branching_habit$base_value)
    updateSliderInput(session, 'density',             
                      value = economic_weight_density$base_value)
    updateSliderInput(session, 'moe',                 
                      value = economic_weight_moe$base_value)
  })
  
  # Update EVs based on objective traits when update button is pushed
  # The correlation Values between objective and economic traits were calculated
  # outside of this code.
  # volume objective is multiplied by 350 to account for stocking of trees
  # because objective traits are based on DBH08 per tree (selection criterion)
  # and volume per tree (VOL), but economic weight is at the hectare level. Therefore
  # a correction factor of 350 to account for stocking (trees/ha) is needed.
  observeEvent(input$update_evs, {
    updateSliderInput(session, 'diameter', 
                      value = ((0.0141300907 * input$volume_objective * 350) +
                               (0.192250168 * input$BIX_objective) +
                               (-0.002081443 * input$stiffness_objective) +
                               (0.1085235 * input$density_objective)
    ))
    updateSliderTextInput(
      session,
      inputId = "straightness",
      label = NULL,
      choices = seq(
        from = economic_weight_straightness$min,
        to = economic_weight_straightness$max,
        by = -0.01
      ),
      # must be rounded to the same number of 'steps' in the slider otherwise errors occur
      selected = round(((0.0320203456 * input$volume_objective * 350) +
                          (0.59167283 * input$BIX_objective) +
                          (-0.114467398 * input$stiffness_objective) +
                          (-8.6158715 * input$density_objective)
      ), 2)
    )
    updateSliderInput(session, 'branching_habit', 
                      value = ((-0.0157223109 * input$volume_objective * 350) +
                               (-2.736290746 * input$BIX_objective) +
                               (-0.006282754 * input$stiffness_objective) +
                               (8.0434036 * input$density_objective)
    ))
    updateSliderInput(session, 'density', 
                      value = ((-0.0007059268 * input$volume_objective * 350) +
                               (0.002623647 * input$BIX_objective) +
                               (0.006481773 * input$stiffness_objective) +
                               (0.7138365 * input$density_objective)
    ))
    updateSliderInput(session, 'moe', 
                      value = ((0.0165101969 * input$volume_objective * 350) +
                               (0.013736525 * input$BIX_objective) +
                               (0.479462404 * input$stiffness_objective) +
                               (5.4226079 * input$density_objective)
    ))
  })
  
  # Update objective traits based on EVs when update button is pushed
  observeEvent(input$update_objvs, {
    updateSliderInput(session, 'volume_objective', 
                      value = ((67.17597182 * input$diameter) +
                               (2.0614679 * input$straightness) +
                               (5.12062661 * input$branching_habit) +
                               (-55.157659 * input$density) +
                               (1.59654819 * input$moe)
    ) / 350)
    # must be rounded to the same number of 'steps' in the slider otherwise errors occur
    updateSliderTextInput(
      session,
      inputId = "BIX_objective",
      label = NULL,
      choices = seq(
        from = objective_trait_bix$Range_min,
        to = objective_trait_bix$Range_max,
        by = -0.5
      ),
      selected = round(((0.07131819 * input$diameter) +
                        (-0.09624179 * input$straightness) +
                        (-0.378121967 * input$branching_habit) +
                        (3.675437 * input$density) +
                        (-0.07730971 * input$moe)), 0)
    )
    updateSliderInput(session, 'stiffness_objective', 
                      value = ((-4.98918604 * input$diameter) +
                               (0.80238207 * input$straightness) +
                               (-0.170444867 * input$branching_habit) +
                               (-5.297727 * input$density) +
                               (2.32495744 * input$moe)
    ))
    updateSliderInput(session, 'density_objective', 
                      value = ((0.12417387 * input$diameter) +
                               (-0.01222078 * input$straightness) +
                               (0.007232485 * input$branching_habit) +
                               (1.304299 *  input$density) +
                               (-0.01991637 * input$moe)
    ))
  })
  
  # Info presented when pushing EV help button
  observeEvent(input$SHOW_HELP, {
    showModal(
      modalDialog(
        title = "Selection age weights",
        strong("DBH - Diameter at breast height:"),
        br(),
        "Included within the production sub-index. Measured in mm at 1.4m above ground level.",
        br(),
        br(),
        strong("Branching - Branch frequency:"),
        br(),
        "Included within the production sub-index, branching is based on a 1 (uni-nodal) - 6 (multi-nodal) score of branch cluster frequency.",
        br(),
        br(),
        strong("Straightness:"),
        br(),
        "Included within the production sub-index, stem straightness is based on a 1 (crooked) - 6 (very straight) score.",
        br(),
        br(),
        strong("Corewood Density:"),
        br(),
        "Included within the wood property sub-index, Corewood density at selection age in kg/m3 is measured from wood cores or using an IML Resistograph tool on standing trees.",
        br(),
        br(),
        strong("MoE:"),
        br(),
        "Included within the wood property sub-index, Modulus of Elasticity is predicted by acoustic velocity (GigaPascals, GPa) using either a Tree Tap, ST300 or IML Hammer tool on standing trees.",
      )
    )
  })
  
  # Info presented when pushing objective trait help button
  observeEvent(input$SHOW_HELP_OBJECTIVE, {
    showModal(
      modalDialog(
        title = "Harvest age traits",
        "These traits can be used to update the selection age weights",
        br(),
        br(),
        strong("Volume:"),
        br(),
        "Economic value ($NPV) of harvest volume (kg m-3) represents the additional economic return per marginal unit improvement in harvest volume. Volume is 44% of the total emphasis of the RPBC (baseline) breeding objectiv",
        br(),
        br(),
        strong("Density:"),
        br(),
        "Economic value ($NPV) of wood density (kg m-3) at harvest represents the additional economic return per marginal unit improvement in wood density. Density is 12% of the total emphasis of the RPBC (baseline) breeding objective.",
        br(),
        br(),
        strong("Stiffness:"),
        br(),
        "Economic value ($NPV) of wood stiffness (GPa) at harvest represents the additional economic return per marginal unit improvement in wood stiffness. Stiffness is 26% of the total emphasis of the RPBC (baseline) breeding objective.",
        br(),
        br(),
        strong("BIX - Branching Index:"),
        br(),
        "Economic value ($NPV) of BIX at harvest (BIX is the average size of four branches per 5.5 m log, using the largest branch in each quadrant) represents the additional economic return per marginal unit improvement in BIX. BIX is 17% of the total emphasis of the RPBC (baseline) breeding objective.",
      )
    )
  })
  
  # Barplot of index values
  output$bar_plot <-  renderPlotly({
    plot_data <- complete_breeding_values_purchase_pedigree
    # Filter for available or not based on availability_select radio buttons
    if (input$availability_select == "Only Available Ortets") {
      plot_data_final <- plot_data %>% filter(Available == 'Yes')
    }
    
    if (input$availability_select == "All Ortets") {
      plot_data_final <- plot_data
    }
    
    # if this combination of purchase availability has no data message below will print
    validate(
      need(
        plot_data_final != "",
        "There are no Ortets with selected purchase combination available"
      )
    )
    
    # Filter which traits to include in the index calculation
    calculated_val <- 0
    ifelse(
      input$trait_selection == "DBH",
      calculated_val  <-
        calculated_val + (na_replace(plot_data_final$EBV_dbh, 0) * input$diameter),
      calculated_val
    )
    ifelse(
      input$trait_selection == "Straightness",
      calculated_val  <-
        calculated_val + (
          na_replace(plot_data_final$EBV_str, 0) * input$straightness
        ),
      calculated_val
    )
    ifelse(
      input$trait_selection == "Branching Habit",
      calculated_val  <-
        calculated_val + (
          na_replace(plot_data_final$EBV_brh, 0) * input$branching_habit
        ),
      calculated_val
    )
    print(calculated_val)
    
    ifelse(
      input$trait_selection == "Corewood Density",
      calculated_val  <-
        calculated_val + (na_replace(plot_data_final$EBV_den, 0) * input$density),
      calculated_val
    )
    ifelse(
      input$trait_selection == "MoE",
      calculated_val  <-
        calculated_val + (na_replace(plot_data_final$EBV_pme, 0) * input$moe),
      calculated_val
    )

    print(calculated_val)
    
        calculated_value <-
      cbind(
        Ortet          = plot_data_final$Ortet,
        Available      = plot_data_final$Available,
        Arbogen        = plot_data_final$Arbogen,
        `PF Olsen`     = plot_data_final$`PF Olsen`,
        Proseed        = plot_data_final$Proseed,
        calculated_val = calculated_val
      )
        
    calculated_value <- as.data.frame(calculated_value)

        calculated_value$calculated_val <-
      as.numeric(calculated_value$calculated_val)
    
    calculated_value_plot <- calculated_value
    
    # Colour plot based on provider radio buttons
    # removed Impact of selecting providers for now as requested
    # this has been left as permission is received it will likely need to be
    # re-added
    # To add back in the compnay needs adding to the provider_select radio button
    # then adding to the filter set below
    validate(need(input$provider_select == "All providers",
                  "Data withheld"))
    if (input$provider_select == "ArborGen") {
      plot_data_arbogen     <-
        calculated_value_plot %>% filter(Arbogen == 'yes')
      plot_data_no_arbogen  <-
        calculated_value_plot %>% filter(Arbogen == 'no')
      plot_data_unknown_arbogen <-
        calculated_value_plot %>% filter(Arbogen == NA)
      plot_data_arbogen     <-
        plot_data_arbogen %>% mutate(Purchase_availabity = 'Available from ArborGen')
      plot_data_no_arbogen  <-
        plot_data_no_arbogen %>% mutate(Purchase_availabity = 'Not available from ArborGen')
      plot_data_unknown_arbogen  <-
        plot_data_unknown_arbogen %>% mutate(Purchase_availabity = 'Unknown')
      calculated_value_plot <-
        rbind(plot_data_arbogen,
              plot_data_no_arbogen,
              plot_data_unknown_arbogen)
    }
    
    if (input$provider_select == "Proseed") {
      plot_data_proseed     <-
        calculated_value_plot %>% filter(Proseed == 'yes')
      plot_data_no_proseed  <-
        calculated_value_plot %>% filter(Proseed == 'no')
      plot_data_unknown_proseed <-
        calculated_value_plot %>% filter(Proseed == NA)
      plot_data_proseed     <-
        plot_data_proseed %>% mutate(Purchase_availabity = 'Available from Proseed')
      plot_data_no_proseed  <-
        plot_data_no_proseed %>% mutate(Purchase_availabity = 'Not available from Proseed')
      plot_data_unknown_proseed  <-
        plot_data_unknown_proseed %>% mutate(Purchase_availabity = 'Unknown')
      calculated_value_plot <-
        rbind(plot_data_proseed,
              plot_data_no_proseed,
              plot_data_unknown_proseed)
    }
    
    if (input$provider_select == "PF Olsen") {
      plot_data_pf_olsen    <-
        calculated_value_plot %>% filter(`PF Olsen` == 'yes')
      plot_data_no_pf_olsen <-
        calculated_value_plot %>% filter(`PF Olsen` == 'no')
      plot_data_unknown_pf_olsen <-
        calculated_value_plot %>% filter(`PF Olsen` == NA)
      plot_data_pf_olsen    <-
        plot_data_pf_olsen %>% mutate(Purchase_availabity = 'Available from PF Olsen')
      plot_data_no_pf_olsen <-
        plot_data_no_pf_olsen %>% mutate(Purchase_availabity = 'Not available from PF Olsen')
      plot_data_unknown_pf_olsen <-
        plot_data_unknown_pf_olsen %>% mutate(Purchase_availabity = 'Unknown')
      calculated_value_plot <-
        rbind(plot_data_pf_olsen,
              plot_data_no_pf_olsen,
              plot_data_unknown_pf_olsen)
    }
    
    if (input$provider_select == "All providers") {
      plot_data_available    <-
        calculated_value_plot %>% filter(Available == 'Yes')
      plot_data_no_available <-
        calculated_value_plot %>% filter(Available == 'No')
      plot_data_unknown_available <-
        calculated_value_plot %>% filter(Available == NA)
      
      plot_data_available    <-
        plot_data_available %>% mutate(Purchase_availabity = 'Available')
      plot_data_no_available <-
        plot_data_no_available %>% mutate(Purchase_availabity = 'Not Available')
      plot_data_unknown_available <-
        plot_data_unknown_available %>% mutate(Purchase_availabity = 'Unknown')
      
      calculated_value_plot <-
        rbind(plot_data_available,
              plot_data_no_available,
              plot_data_unknown_available)
    }
    
    # combine purchase companies into a single column
    calculated_value_plot <-
      unite(calculated_value_plot, Company, Arbogen:Proseed, sep = '')
    calculated_value_plot$Company <-
      dplyr:::recode_factor(
        calculated_value_plot$Company,
        noyesno  = "PF Olsen",
        yesnono = "ArborGen",
        nonoyes = 'Proseed',
        nonono = 'Not available',
        NANANA = 'Unknown',
        yesyesno = "ArborGen & PF Olsen",
        yesnoyes = 'ArborGen & Proseed',
        yesyesyes = 'ArborGen, PF Olsen, & Proseed',
        noyesyes = 'PF Olsen & Proseed'
      )
    
    calculated_value_plot$Company <-
      as.factor(calculated_value_plot$Company)
    
    # select top trees
    calculated_value_plot$calculated_val <-
      as.numeric(calculated_value_plot$calculated_val)
    
    # Place in plot number of trees either 20 or based on how many trees are available
    if (20 < length(calculated_value_plot$calculated_val)) {
      calculated_value_plot <- setDT(calculated_value_plot)
      final_plot_data <- calculated_value_plot %>%
        arrange(desc(calculated_val)) %>%
        slice(1:20)
      final_plot_data <- as.data.frame(final_plot_data)
      final_plot_data <- cbind(final_plot_data,
                               Rank = c(1:20))
    } else {
      final_plot_data <- calculated_value_plot %>%
        arrange(desc(calculated_val)) %>%
        slice(1:length(calculated_value_plot$calculated_val))
      final_plot_data <- cbind(final_plot_data,
                               Rank = c(1:length(
                                 calculated_value_plot$calculated_val
                               )))
    }
    final_plot_data$calculated_val <-
      round(final_plot_data$calculated_val, digits = 0)
    
    # reorder top 20 trees from best to worst
    final_plot_data$Rank <- factor(final_plot_data$Rank,
                                   levels = unique(final_plot_data$Rank)[order(final_plot_data$Rank, decreasing = TRUE)])
    
    reactive_plot_data$ranked <- final_plot_data
    
    # Making top 20 rank table for export - this is done here as download
    # handler function is unable to having working code before it
    export_data <-
      calculated_value %>% arrange(desc(calculated_val))
    export_data <- cbind(export_data,
                         Rank = c(1:length(export_data$calculated_val)))
    export_data$calculated_val <-
      round(export_data$calculated_val, digits = 0)
    export_data$Rank <- factor(export_data$Rank,
                               levels = unique(export_data$Rank)[order(export_data$Rank, 
                                                                       decreasing = TRUE)])
    download_data_all <-
      unite(export_data, Company, Arbogen:Proseed, sep = '')
    download_data_all$Company <-
      dplyr::recode_factor(
        download_data_all$Company,
        noyesno  = "PF Olsen",
        yesnono = "ArborGen",
        nonoyes = 'Proseed',
        nonono = 'Not available',
        NANANA = 'Unknown',
        yesyesno = "ArborGen & PF Olsen",
        yesnoyes = 'ArborGen & Proseed',
        yesyesyes = 'ArborGen, PF Olsen, & Proseed',
        noyesyes = 'PF Olsen & Proseed'
      )
    
    download_data_all$Ortet <- as.numeric(download_data_all$Ortet)
    download_data <- left_join(
      download_data_all,
      complete_breeding_values_purchase_pedigree,
      by = c('Ortet',
             'Available')
    )
    download_data <- download_data %>% dplyr::select(
      Ortet,
      Available,
      Company,
      calculated_val,
      Rank,
      EBV_dbh,
      EBV_brh,
      EBV_str,
      EBV_den,
      EBV_pme,
      acc_dbh_ebv,
      acc_brh_ebv,
      acc_str_ebv,
      acc_den_ebv,
      acc_pme_ebv,
      Mcln,
      Fcln
    )
    download_data <- as.data.frame(download_data)
    
    # select which traits to include based on which traits have been selected
    # in the index calculation
    EV_inputs <- NA
    ifelse(input$trait_selection == "DBH",
           EV_inputs <- rbind(
             EV_inputs,
             cbind(
               Traits = 'DBH',
               EV_value = input$diameter,
               Unit = '$/mm'
             )
           ),
           EV_inputs)
    ifelse(
      input$trait_selection == "Branching Habit",
      EV_inputs <-
        rbind(
          EV_inputs,
          cbind(
            Traits = 'Branching_habit',
            EV_value = input$branching_habit,
            Unit = '$/ Scale Unit'
          )
        ),
      EV_inputs
    )
    ifelse(
      input$trait_selection == "Straightness",
      EV_inputs <-
        rbind(
          EV_inputs,
          cbind(
            Traits = 'Straightness',
            EV_value = input$straightness,
            Unit = '$/ Scale Unit'
          )
        ),
      EV_inputs
    )
    ifelse(
      input$trait_selection == "Corewood Density",
      EV_inputs <- rbind(
        EV_inputs,
        cbind(
          Traits = 'Density',
          EV_value = input$density,
          Unit = '$/kg m^-3'
        )
      ),
      EV_inputs
    )
    ifelse(input$trait_selection == "MoE",
           EV_inputs <- rbind(
             EV_inputs,
             cbind(
               Traits = 'Moe',
               EV_value = input$moe,
               Unit = '$/ GPa'
             )
           ),
           EV_inputs)
    
    # set up data to enable/disable download button
    # removes first row that is blank due to NA when setting up reactive dataframe
    EV_inputs <- as.data.frame(EV_inputs)
    EV_inputs_download <- EV_inputs %>% slice(-1)
    reactive_plot_data$ev_inputs <- EV_inputs_download
    
    # If no traits are selected print the message below
    # this only works at this point (don't know why) do not try and move earlier
    validate(need(EV_inputs != "", paste0('\n', "Please select trait/s")))
    
    EV_inputs <- as.data.frame(EV_inputs)
    colnames(EV_inputs) <- c('Trait', 'Economic Weight', 'Unit')
    
    # bind the two data frames together filling in the blank spaces where
    # the column lengths don't match with NA
    download_data <-
      download_data %>%
      rename(
        .,
        `Index value`                            = calculated_val,
        `Mother`                                 = Mcln,
        `Father`                                 = Fcln,
        `EBV DBH`                                = EBV_dbh,
        `EBV branching habit`                    = EBV_brh,
        `EBV straightness`                       = EBV_str,
        `EBV corewood density`                   = EBV_den,
        `EBV MoE`                                = EBV_pme,
        `Accuracy EBV DBH` = acc_dbh_ebv,
        `Accuracy EBV branching habit`           = acc_brh_ebv,
        `Accuracy EBV straightness`              = acc_str_ebv,
        `Accuracy EBV corewood density`          = acc_den_ebv,
        `Accuracy EBV MoE`                       = acc_pme_ebv
      )
    
    all_download_data <- list(Index_data = download_data,
                              Selection_age_weights_used = EV_inputs_download)
    
    all_download_data[is.na(all_download_data)] <- ""
    reactive_plot_data$download <- all_download_data
    
    # Plot the index values
    # Change the availability from yes/no to available/unavailable
    final_plot_data$Available[final_plot_data$Available == 'No'] <-
      'Unavailable'
    final_plot_data$Available[final_plot_data$Available == 'Yes'] <-
      'Available'
    final_plot_data$Available[is.na(final_plot_data$Available)] <-
      'Unknown'
    
    plot_colors <- setNames(c("#3c9180", "#c4c4c4", "#97ab79"),
                            c("Available", 'Unavailable', 'Unknown'))
    
    print(final_plot_data)
    
    # Set colour plot based on provider radio buttons
    if (input$provider_select == "ArborGen") {
      plot_colors <- setNames(
        c("#3c9180", "#c4c4c4", "#97ab79"),
        c(
          "Available from ArborGen",
          'Not available from ArborGen',
          "Unknown"
        )
      )
    }
    
    if (input$provider_select == "Proseed") {
      plot_colors <- setNames(
        c("#3c9180", "#c4c4c4", "#97ab79"),
        c(
          "Available from Proseed",
          'Not available from Proseed',
          "Unknown"
        )
      )
    }
    
    if (input$provider_select == "PF Olsen") {
      plot_colors <- setNames(
        c("#3c9180", "#c4c4c4", "#97ab79"),
        c(
          "Available from PF Olsen",
          'Not available from PF Olsen',
          "Unknown"
        )
      )
    }
    
    if (input$provider_select == "All providers") {
      plot_colors <- setNames(c("#3c9180", "#c4c4c4", "#97ab79"),
                              c("Available", "Not Available", "Unknown"))
    }
    
    # Fix order legend appears in
    final_plot_data$Available <- ordered(final_plot_data$Available,
                                         levels = c("Unknown", "Unavailable", 
                                                    "Available"))
    
    plot_ly(
      final_plot_data,
      x = ~ calculated_val,
      y = ~ Rank,
      color = ~ Purchase_availabity,
      colors = plot_colors,
      type = 'bar',
      source = 'bar_plot',
      text = ~ Ortet,
      textposition = 'auto',
      textfont = list(
        family = "Times",
        size = c(16),
        color = c("white")
      ),
      orientation = 'h',
      width = c(),
      marker = list(line = list(color = 'rgb(8,48,107)',
                                width = 1.5)),
      hovertemplate = 'Index Value = %{x:}<extra></extra>'
    ) %>%
      layout(
        height = 700,
        yaxis = list(titlefont = list(size = 20),
                     title = "Rank"),
        xaxis = list(titlefont = list(size = 20),
                     title = "Index Value ($)"),
        font = list(size = 18),
        legend = list(
          orientation = 'h',
          x = 0,
          y = -.15
        )
      ) %>%
      event_register('plotly_click')  %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = list(
          'sendDataToCloud',
          'editInChartStudio',
          'autoScale2d',
          'resetScale2d',
          'hoverClosestCartesian',
          'hoverCompareCartesian',
          'zoom2d',
          'pan2d',
          'select2d',
          'lasso2d',
          'drawclosedpath',
          'drawopenpath',
          'drawline',
          'drawrect',
          'zoomIn2d',
          'zoomOut2d'
        )
      )
  })
  
  # when a bar is clicked on create a pop-up with this info
  observeEvent(event_data("plotly_click", source = "bar_plot"),
               {
                 event_data <- event_data("plotly_click", source = "bar_plot")
                 pop_up_data <-
                   reactive_plot_data$ranked %>% filter(Rank == event_data$y)
                 pop_up_data$Ortet <- as.numeric(pop_up_data$Ortet)
                 single_pop_up_data <-
                   (left_join(
                     pop_up_data,
                     complete_breeding_values_purchase_pedigree,
                     by = c('Ortet',
                            'Available')
                   ))
                 
                 single_pop_up_data <- single_pop_up_data %>%
                   dplyr::select(
                     Ortet,
                     Available,
                     Company,
                     calculated_val,
                     Rank,
                     EBV_dbh,
                     EBV_brh,
                     EBV_str,
                     EBV_den,
                     EBV_pme,
                     acc_dbh_ebv,
                     acc_brh_ebv,
                     acc_str_ebv,
                     acc_den_ebv,
                     acc_pme_ebv,
                     Mcln,
                     Fcln
                   )
                 single_pop_up_data <-
                   as.data.frame(single_pop_up_data)
                 showModal(
                   modalDialog(
                     style = "font-size:150%",
                     title = "Breeding values of tree",
                     fluidRow(
                       column(width = 6, align = 'left',
                              paste0('Ortet: ')),
                       column(
                         width = 6,
                         align = 'left',
                         paste0(single_pop_up_data$Ortet)
                       )
                     ),
                     HTML("<br>"),
                     fluidRow(
                       column(
                         width = 6,
                         align = 'left',
                         paste0('Availability: ')
                       ),
                       column(
                         width = 6,
                         align = 'left',
                         paste0(single_pop_up_data$Available)
                       )
                     ),
                     fluidRow(
                       column(width = 6, align = 'left',
                              paste0('Company: ')),
                       column(
                         width = 6,
                         align = 'left',
                         paste0(single_pop_up_data$Company)
                       )
                     ),
                     HTML("<br>"),
                     fluidRow(
                       column(width = 6, align = 'left',
                              paste0('Rank: ')),
                       column(
                         width = 6,
                         align = 'left',
                         paste0(single_pop_up_data$Rank)
                       )
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         align = 'left',
                         paste0('Index value: ')
                       ),
                       column(width = 6, align = 'left',
                              paste0(
                                '$',
                                format(single_pop_up_data$calculated_val,
                                       big.mark = ',')
                              ))
                     ),
                     HTML("<br>"),
                     fluidRow(
                       column(width = 6, align = 'left',
                              paste0('DBH EBV: ')),
                       column(width = 6, align = 'left',
                              paste0(
                                round(single_pop_up_data$EBV_dbh,
                                      digits = 3)
                              ))
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         align = 'left',
                         paste0('Branching habit EBV: ')
                       ),
                       column(width = 6, align = 'left',
                              paste0(
                                round(single_pop_up_data$EBV_brh,
                                      digits = 3)
                              ))
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         align = 'left',
                         paste0('Straightness EBV: ')
                       ),
                       column(width = 6, align = 'left',
                              paste0(
                                round(single_pop_up_data$EBV_str,
                                      digits = 3)
                              ))
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         align = 'left',
                         paste0('Corewood Density EBV: ')
                       ),
                       column(width = 6, align = 'left',
                              paste0(
                                round(single_pop_up_data$EBV_den,
                                      digits = 3)
                              ))
                     ),
                     fluidRow(
                       column(width = 6, align = 'left',
                              paste0('MoE EBV: ')),
                       column(width = 6, align = 'left',
                              paste0(
                                round(single_pop_up_data$EBV_pme,
                                      digits = 3)
                              ))
                     ),
                     HTML("<br>"),
                     fluidRow(
                       column(
                         width = 6,
                         align = 'left',
                         paste0('DBH accuracy: ')
                       ),
                       column(width = 6, align = 'left',
                              paste0(
                                round(single_pop_up_data$acc_dbh_ebv,
                                      digits = 3)
                              ))
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         align = 'left',
                         paste0('Branching habit accuracy: ')
                       ),
                       column(width = 6, align = 'left',
                              paste0(
                                round(single_pop_up_data$acc_brh_ebv,
                                      digits = 3)
                              ))
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         align = 'left',
                         paste0('Straightness accuracy: ')
                       ),
                       column(width = 6, align = 'left',
                              paste0(
                                round(single_pop_up_data$acc_str_ebv,
                                      digits = 3)
                              ))
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         align = 'left',
                         paste0('Corewood Density accuracy: ')
                       ),
                       column(width = 6, align = 'left',
                              paste0(
                                round(single_pop_up_data$acc_den_ebv,
                                      digits = 3)
                              ))
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         align = 'left',
                         paste0('MoE accuracy: ')
                       ),
                       column(width = 6, align = 'left',
                              paste0(
                                round(single_pop_up_data$acc_pme_ebv,
                                      digits = 3)
                              ))
                     ),
                     HTML("<br>"),
                     fluidRow(
                       column(width = 6, align = 'left',
                              paste0('Mother: ')),
                       column(
                         width = 6,
                         align = 'left',
                         paste0(single_pop_up_data$Mcln)
                       )
                     ),
                     fluidRow(
                       column(width = 6, align = 'left',
                              paste0('Father: ')),
                       column(
                         width = 6,
                         align = 'left',
                         paste0(single_pop_up_data$Fcln)
                       )
                     ),
                   )
                 )
               })
  
  
  # data table to show economic values selected and baseline economic values
  output$EV_table <- DT::renderDataTable({
    # make table
    ev_table_data <- rbind(
      cbind(
        DBH = 'mm',
        `Branching habit` = 'Scale (1-6)',
        Straightness = 'Scale (1-6)',
        # this makes the -3 superscript
        `Corewood Density` = HTML(paste("kg m", tags$sup(-3), sep = "")),
        MoE = 'GPa'
      ),
      cbind(
        DBH = economic_weight_diameter$base_value,
        `Branching habit` = economic_weight_branching_habit$base_value,
        Straightness = economic_weight_straightness$base_value,
        `Corewood Density` = economic_weight_density$base_value,
        MoE = economic_weight_moe$base_value
      ),
      cbind(
        DBH = round(input$diameter, digits = 2),
        `Branching habit` = round(input$branching_habit, digits = 2),
        Straightness = round(input$straightness, digits = 2),
        `Corewood Density` = round(input$density, digits = 2),
        MoE = round(input$moe, digits = 2)
      )
    )
    
    ev_table_data <- as.data.frame(ev_table_data)
    ev_table_data_truth <- ev_table_data
    
    # Add truth column based on whether economic value is above/below
    # baseline value - this allows the cell the be coloured based on truth value
    # Also add an arrow icon next to values presented based on truth column
    # have to do for each trait
    if (as.numeric(ev_table_data_truth$DBH[2]) > as.numeric(ev_table_data_truth$DBH[3])) {
      ev_table_data$Dia_fact <- c(NA, NA, 1)
      ev_table_data$DBH[3] <- paste0(fa(name = "arrow-down"),
                                     ' ',
                                     ev_table_data$DBH[3])
    }
    if (as.numeric(ev_table_data_truth$DBH[2]) < as.numeric(ev_table_data_truth$DBH[3])) {
      ev_table_data$Dia_fact <- c(NA, NA, 2)
      ev_table_data$DBH[3] <- paste0(fa(name = "arrow-up"),
                                     ' ',
                                     ev_table_data$DBH[3])
    }
    if (as.numeric(ev_table_data_truth$DBH[2]) == as.numeric(ev_table_data_truth$DBH[3])) {
      ev_table_data$Dia_fact <- c(NA, NA, 3)
    }
    if (as.numeric(ev_table_data_truth$Straightness[2]) > as.numeric(ev_table_data_truth$Straightness[3])) {
      ev_table_data$straight_fact <- c(NA, NA, 2)
      ev_table_data$Straightness[3] <-
        paste0(fa(name = "arrow-down"),
               ' ',
               ev_table_data$Straightness[3])
    }
    if (as.numeric(ev_table_data_truth$Straightness[2]) < as.numeric(ev_table_data_truth$Straightness[3])) {
      ev_table_data$straight_fact <- c(NA, NA, 1)
      ev_table_data$Straightness[3] <-
        paste0(fa(name = "arrow-up"),
               ' ',
               ev_table_data$Straightness[3])
    }
    if (as.numeric(ev_table_data_truth$Straightness[2]) == as.numeric(ev_table_data_truth$Straightness[3])) {
      ev_table_data$straight_fact <- c(NA, NA, 3)
    }
    if (as.numeric(ev_table_data_truth$`Branching habit`[2]) > as.numeric(ev_table_data_truth$`Branching habit`[3])) {
      ev_table_data$bra_fact <- c(NA, NA, 1)
      ev_table_data$`Branching habit`[3] <-
        paste0(fa(name = "arrow-down"),
               ' ',
               ev_table_data$`Branching habit`[3])
    }
    if (as.numeric(ev_table_data_truth$`Branching habit`[2]) < as.numeric(ev_table_data_truth$`Branching habit`[3])) {
      ev_table_data$bra_fact <- c(NA, NA, 2)
      ev_table_data$`Branching habit`[3] <-
        paste0(fa(name = "arrow-up"),
               ' ',
               ev_table_data$`Branching habit`[3])
    }
    if (as.numeric(ev_table_data_truth$`Branching habit`[2]) == as.numeric(ev_table_data_truth$`Branching habit`[3])) {
      ev_table_data$bra_fact <- c(NA, NA, 3)
    }
    if (as.numeric(ev_table_data_truth$`Corewood Density`[2]) > as.numeric(ev_table_data_truth$`Corewood Density`[3])) {
      ev_table_data$den_fact <- c(NA, NA, 1)
      ev_table_data$`Corewood Density`[3] <-
        paste0(fa(name = "arrow-down"),
               ' ',
               ev_table_data$`Corewood Density`[3])
    }
    if (as.numeric(ev_table_data_truth$`Corewood Density`[2]) < as.numeric(ev_table_data_truth$`Corewood Density`[3])) {
      ev_table_data$den_fact <- c(NA, NA, 2)
      ev_table_data$`Corewood Density`[3] <-
        paste0(fa(name = "arrow-up"),
               ' ',
               ev_table_data$`Corewood Density`[3])
    }
    if (as.numeric(ev_table_data_truth$`Corewood Density`[2]) == as.numeric(ev_table_data_truth$`Corewood Density`[3])) {
      ev_table_data$den_fact <- c(NA, NA, 3)
    }
    if (as.numeric(ev_table_data_truth$MoE[2]) > as.numeric(ev_table_data_truth$MoE[3])) {
      ev_table_data$moe_fact <- c(NA, NA, 1)
      ev_table_data$MoE[3] <- paste0(fa(name = "arrow-down"),
                                     ' ',
                                     ev_table_data$MoE[3])
    }
    if (as.numeric(ev_table_data_truth$MoE[2]) < as.numeric(ev_table_data_truth$MoE[3])) {
      ev_table_data$moe_fact <- c(NA, NA, 2)
      ev_table_data$MoE[3] <- paste0(fa(name = "arrow-up"),
                                     ' ',
                                     ev_table_data$MoE[3])
    }
    if (as.numeric(ev_table_data_truth$MoE[2]) == as.numeric(ev_table_data_truth$MoE[3])) {
      ev_table_data$moe_fact <- c(NA, NA, 3)
    }
    
    # make table and hide the truth columns
    # color the cells based on values in truth column
    rownames(ev_table_data) <-
      c('Units',
        'Baseline economic Value ($/trait unit change)',
        'Custom index economic Value ($/trait unit change)')
    DT::datatable(
      ev_table_data,
      escape = F,
      options = list(
        dom = 't',
        ordering = F,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
          # make sure targets match the columns needed!
          list(targets = c(6, 7, 8, 9, 10), visible = FALSE)
        )
      )
    ) %>%
      formatStyle(
        columns = "DBH",
        valueColumns = "Dia_fact",
        backgroundColor = styleEqual(
          levels = c(1, 2),
          values = c("#C1E1C1", "#A7C7E7")
        )
      ) %>%
      formatStyle(
        columns = "Straightness",
        valueColumns = "straight_fact",
        backgroundColor = styleEqual(
          levels = c(1, 2),
          values = c("#C1E1C1", "#A7C7E7")
        )
      ) %>%
      formatStyle(
        columns = "Branching habit",
        valueColumns = "bra_fact",
        backgroundColor = styleEqual(
          levels = c(1, 2),
          values = c("#C1E1C1", "#A7C7E7")
        )
      ) %>%
      formatStyle(
        columns = "Corewood Density",
        valueColumns = "den_fact",
        backgroundColor = styleEqual(
          levels = c(1, 2),
          values = c("#C1E1C1", "#A7C7E7")
        )
      ) %>%
      formatStyle(
        columns = "MoE",
        valueColumns = "moe_fact",
        backgroundColor = styleEqual(
          levels = c(1, 2),
          values = c("#C1E1C1", "#A7C7E7")
        )
      )
  })
  
  # download data button
  output$download_btn_require <- renderUI({
    req(reactive_plot_data$ev_inputs != "")
    downloadButton(
      'downloadData',
      label = "",
      class = "fixed-download-btn",
      icon = shiny::icon("download"),
    )
  })
  
  # Download file when download button is pushed
  output$downloadData <-  downloadHandler(
    filename = function() {
      validate(need(input$provider_select == "All providers",
                    "Data withheld"))
      paste('Ranked-ortets_',
            format(Sys.time(), "%y-%m-%d_%Hh-%Mm-%OSs"),
            '.xlsx',
            sep = '')
    },
    content = function(file) {
      write_xlsx(reactive_plot_data$download, file)
    }
  )
  
  # Automatically stops shinyapp running when browers window is closed
  session$onSessionEnded(stopApp)
}

# Run app
shinyApp(ui = ui, server = server)
