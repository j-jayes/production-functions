library(shiny)
library(tidyverse)
library(thematic)
library(bslib)
library(DT)
library(plotly)
library(kableExtra)

thematic_shiny()
theme_update(text = element_text(size = 17))

A <- 25
N <- 145
alpha <- .3


df <- tibble(k = 1:20)

df <- df %>%
  mutate(
    k_alpha = k^alpha,
    y = A * k_alpha * N
  )

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "minty", font_scale = 1.5),

  # Application title
  titlePanel("Production function sandpit"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        em("Choose the characteristics of your new production function here"),
      sliderInput("n_alpha",
        "Capital share of income (alpha):",
        min = 0.1,
        max = 0.9,
        value = 0.3,
        step = .05
      ),
      sliderInput("n_A",
        "Level of Total Factor Productivity (A):",
        min = 5,
        max = 50,
        value = 25,
        step = 5
      ),
      sliderInput("n_N",
        "Level of labour (N):",
        min = 100,
        max = 300,
        value = 145
      ),
      em("Compare the outputs of each production function at this specified level of capital"),
      sliderInput("chosen_k",
        "Chosen level of capital (K):",
        min = 1,
        max = 20,
        value = 15,
        step = 1
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("Plots",
                             plotlyOutput("comp_plot"),
                             plotlyOutput("levels_plot")),
                    tabPanel("Table",
                             tableOutput("comp_table")))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$comp_plot <- renderPlotly({
      
    g <- df_filtered() %>%
      ggplot() +
      geom_point(aes(x = k, y = n_y, color = "New production function"), alpha = .7, cex = 3) +
      geom_line(aes(x = k, y = n_y, color = "New production function"), alpha = .7, cex = 1) +
      geom_point(aes(x = k, y = y, color = "Baseline"), alpha = .7, cex = 3) +
      geom_line(aes(x = k, y = y, color = "Baseline"), alpha = .7, cex = 1) +
      geom_vline(xintercept = input$chosen_k, lty = 2) +
      scale_y_continuous(labels = scales::dollar_format()) +
          theme(legend.position = "top") +
      labs(
          title = "Production functions",
        x = "Level of capital",
        y = "Level of output",
        colour = NULL
      )
    
    ggplotly(g)
  })

  output$levels_plot <- renderPlotly({
    f <- df_line() %>%
      pivot_longer(c(n_y, y), names_to = "output", values_to = "value") %>%
          mutate(output = case_when(
              output == "y" ~ "Baseline output",
              output == "n_y" ~ "New output"
          )) %>%
      ggplot(aes(x = output, y = value, fill = output)) +
          scale_y_continuous(labels = scales::dollar_format()) +
      geom_col() +
          theme(legend.position = "none") +
          labs(title = glue::glue("Output at level of capital { input$chosen_k }"),
               x = "Level of capital (K)",
               y = "")

    ggplotly(f)
  })
  
  output$comp_table <- function() {
      
      y <- df_line() %>% select(y) %>% pull()
      
      n_y <- df_line() %>% select(n_y) %>% pull()
      
      o_m_a <- 1 - alpha
      
      o_m_a_n <- 1-input$n_alpha
      
      tbl <- tibble(
          fn = c("Baseline Production Function", "New Production Function"),
          alpha = c(alpha, input$n_alpha),
             one_minus_alpha = c(o_m_a, o_m_a_n),
             tfp = c(A, input$n_A),
             labour = c(N, input$n_N),
             capital = c(input$chosen_k, input$chosen_k),
             output = c(y, n_y)) %>% 
          mutate(across(c(alpha, one_minus_alpha), scales::percent)) %>% 
          mutate(across(where(is.numeric), ~ scales::number(.x)))
      
      tbl <- tbl %>% 
          pivot_longer(-fn) %>% 
          pivot_wider(names_from = "fn") %>% 
          select(-name)
      
      rownames(tbl) <- c("Capital share of income",
                         "Labour share of income",
                         "Total Factor Productivity",
                         "Labour",
                         "Capital",
                         "Output")
      
      tbl %>% 
          knitr::kable("html") %>% 
          kable_styling("striped", full_width = T)
      
  }

  df_filtered <- reactive({
    df %>%
      mutate(
        n_k_alpha = k^input$n_alpha,
        n_y = input$n_A * n_k_alpha * input$n_N
      ) %>% 
        mutate(across(where(is.numeric), ~ round(.x, 0)))
          
  })

  df_line <- reactive({
    df_filtered() %>%
      filter(k == input$chosen_k)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
