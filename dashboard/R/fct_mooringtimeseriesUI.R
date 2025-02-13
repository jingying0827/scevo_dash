#' mooringtimeseriesUI
#'
#' @description Generates the flow plots UI elements e.g. date inputs, region. Requires @param namespace to individualise elements.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
#load("data/sensorslist_hydromet.rda")
methydro_data<-read.csv("www/Met_Hydro_sites.csv")

mooringtimeseriesUI <- function(namespace){
  
  moorsites<- methydro_data %>%
    dplyr::filter(group %in% 'mooring') %>% 
    dplyr::select('Site')
  moorsites <- unique(moorsites$Site)
  moorvars <- methydro_data %>%
    dplyr::filter(group %in% 'mooring') %>% 
    dplyr::select('s_graph_value')
  moorvars <- unique(moorvars$s_graph_value)
  
  
  return(
    tags$div(
      wellPanel(
        fluidRow(
          column(3,selectInput(inputId =paste0(namespace,"Sites"),
                               label = "Select Site(s):" ,
                               choices = moorsites, 
                               multiple = TRUE,
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(3,selectInput(inputId =paste0(namespace,"Variable"), 
                               label ="Select Variable:",
                               choices = moorvars,
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(2,dateInput(inputId = paste0(namespace,"DateFrom"),
                             label = "From:", 
                             value = Sys.Date()-7),
                 style="margin-top: 50px; "),
          column(2,dateInput(inputId = paste0(namespace,"DateTo"), 
                             label = "To:", 
                             value = Sys.Date()),
                 style="margin-top: 50px;"),
          column(2,actionButton(inputId = paste0(namespace,"FetchData"),
                                label = "Plot",
                                icon = icon("bar-chart"), 
                                style="margin-top: 83px; width: 100%"))
        ),
        fluidRow(
          column(12,uiOutput(outputId = paste0(namespace,"DateSlider")))
        ),
        #shinycssloaders::withSpinner(imageOutput(outputId="profPlot"))
        fluidRow(
          conditionalPanel(
            condition = paste0("input.", namespace, "FetchData > 0"),
          column(12,shinycssloaders::withSpinner(plotly::plotlyOutput(outputId="wqMooringPlot", height = "500px")))
          )
        )
      )
    )
  )
}
