#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(incidence)
library(tidyverse)

# Import saved data
load('case_table.Rdata')
load('covid_parametric.Rdata')
print(covid_parametric)
load('most_recent_date.Rdata')
load('most_recent_r.Rdata')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Has Riverside County flattened the curve?"),

    # Yes or no?
    htmlOutput('yes_no') ,
   
    h3("Plot of daily incidence counts in Riverside County"),
    imageOutput('case_plot'),
    
    h3("Estimate of daily R value in Riverside County"),
    imageOutput('R_plot'),
    
    h4("Notes:"),
    p("OK, here's the nitty gritty details. We want R to be lower than 1, so that eventually COVID-19 will be completely gone."),
    p("Data is pulled from official Riverside County Public Health Data, available at ",
      a(href = "https://www.rivcoph.org/coronavirus", "https://www.rivcoph.org/coronavirus"),
      " ."
      ),
    p("R is calculated using the package ",
     a(href="https://cran.r-project.org/web/packages/EpiEstim/index.html","EpiEstim"),
     ", using the default sliding weekly windows, with a parametric serial interval (m=4.6, sd=4.2), derived from this publication:",
     a(
       href="https://www.medrxiv.org/content/10.1101/2020.11.17.20231548v2",
       "Meta-analysis of the SARS-CoV-2 serial interval and the impact of parameter uncertainty on the COVID-19 reproduction number"
       ),
     "Robert Challen, Ellen Brooks-Pollock, Krasimira Tsaneva-Atanasova,Leon Danon"
       ),
    p("I am not an epidemiologist, but wanted more detail on local incidence data than provided by Riverside County Public Health. 
      There are many assumptions that may not be met in order for this basic analysis to be completely accurate, such as 
      increase in case counts due to variations in testing rates. The weekly sliding window is intended to help smooth out effects 
      due to case counts dropping over the weekend, etc. Don't assume a single bad day represents an increase in transmission. A rising 
      R over multiple days indicates an increase in transmission."),
    
    h4("Detailed Table"),
    tableOutput('case_table')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$yes_no <- renderText({
        if_else(most_recent_r >= 1, 
                paste0(
                    h1("No"), 
                    p(
                        "As of ",
                        most_recent_date,
                        ", with an R value of ",
                        strong(round(most_recent_r,2)),
                        " , daily cases are still growing."
                        )
                    ),
                paste0(
                    h3("Yes!"), 
                    p(
                     "As of ",
                     most_recent_date,
                     ", with an R value of ",
                     strong(round(most_recent_r,2)),
                        " , daily cases are shrinking. Way to go Riverside. Let's keep it up!"
                        )
                    )
        )
    })
    
    output$case_plot <- renderImage({
    list(src = 'incid.png')
      }, deleteFile = FALSE)
    
    output$R_plot <- renderImage({
    list(src = 'R.png')
      }, deleteFile = FALSE)
    
    output$case_table <- renderTable({
      case_table %>%
        arrange(desc(Date)) %>%
        mutate(Date = as.character((Date)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
