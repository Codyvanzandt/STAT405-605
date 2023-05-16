library(shiny)
library(tidyverse)
library(grid)
source("killer_plot.R")


clean_titles <- function(titles){
    titles <- sub("\\s?\\/\\s?.*$", "", titles)
    sub(":\\s?(A|a)\\s.*$","", titles)
}

data <- read_csv("./data/killer_plot_data.csv") %>%
    mutate(title = clean_titles(title))


title_choices <- data %>%
    group_by(title) %>%
    summarize(n = sum(checkouts))%>%
    arrange(desc(n)) %>%
    top_n(20, wt=n) %>%
    pull(title)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Book Review Checkout Timeseries"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("title_selector",
                        "Title",
                        choices=title_choices,
                        selected="The Girl on the Train"
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlot({
        plot_word_timeseries(data, input$title_selector, remove_overlap=TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
