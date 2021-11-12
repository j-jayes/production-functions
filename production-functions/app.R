library(shiny)
library(tidyverse)
library(thematic)
library(bslib)
library(DT)
library(plotly)
library(kableExtra)

theme_set(theme_light())

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
    theme = bslib::bs_theme(bootswatch = "minty", font_scale = 1.3),

  # Application title
  titlePanel("Production function sandpit"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        em("Choose the parameters of your new production function here"),
      sliderInput("n_A",
        "Total Factor Productivity (A):",
        min = 5,
        max = 50,
        value = 25,
        step = 5
      ),
      sliderInput("n_N",
        "Labour (N):",
        min = 100,
        max = 200,
        value = 120,
        step = 5
      ),
      sliderInput("n_alpha",
                  "Output elasticity of capital (alpha):",
                  min = 0.1,
                  max = 0.9,
                  value = 0.4,
                  step = .05
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
                             tableOutput("comp_table")),
                    tabPanel("Formula",
                             em("The production function is:"),
                             uiOutput("formula"),
                             tableOutput("explanation"),
                             em("And plugging in the values we get:"),
                             uiOutput("formula_inputs")))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = df_filtered, { 
        # event will be called when histdata changes, which only happens once, when it is initially calculated
        showModal(modalDialog(
            title = "Production function sandpit", 
            # h1('Landing Page'),
            p("Have a look at how changing the parameters of a basic Cobb-Douglas production functions impacts output and changes the shape of the function."),
            p("Click on the 'Table' tab to compare the inputs."),
            p("Click on the 'Formula' tab to see the values plugged in."),
            p("Mouse over the graphs on the 'Plots' tab to see the different values."),
            a(href="https://en.wikipedia.org/wiki/Cobb%E2%80%93Douglas_production_function", "Learn more about the Cobb-Douglas Production Function")
        ))
    })
    
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
        x = "Level of capital (K)",
        y = "Level of output (Y)",
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
               x = NULL,
               y = glue::glue("Output Y when K = { input$chosen_k }"))

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
          mutate(across(c(alpha, one_minus_alpha), scales::percent),
                 across(c(tfp, labour, capital), ~ scales::number(.x)),
                 across(output, ~ scales::dollar(.x)))
      
      tbl <- tbl %>% 
          pivot_longer(-fn) %>% 
          pivot_wider(names_from = "fn") %>% 
          select(-name)
      
      rownames(tbl) <- c("Output elasticity of capital (alpha)",
                         "Output elasticity of labour (1-alpha)",
                         "Total Factor Productivity (TFP)",
                         "Labour (N)",
                         "Capital (K)",
                         "Output (Y)")
      
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
  
  output$formula <- renderUI({
    withMathJax(paste0("$$ Y=A K^{alpha} N^{1 - alpha} $$"))
  })
  
  output$formula_inputs <- renderUI({
    
    n_y <- df_line() %>% select(n_y) %>% pull()
    
    n_beta = 1 - input$n_alpha
    
    withMathJax(paste0("$$", n_y, " = ", input$n_A, "(", input$chosen_k, ")", "^{", input$n_alpha, "}", 
                       "(", input$n_N, ")", "^{", n_beta, "} $$"))
  })
  
  output$explanation <- function() {
    
    tibble(`Where:` = c("Y = Total production",
                     "A = Total Factor Productivity (TFP)",
                     "K = Capital input",
                     "N = Labour input",
                     "alpha = Output elasticity of capital")) %>% 
      knitr::kable("html")
    
    
  }
}

# Run the application
shinyApp(ui = ui, server = server)
