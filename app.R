# Load required libraries
library(shiny)
library(plotly)
library(dplyr)
library(readxl)
library(DT)
library(forecast)
library(lubridate)
library(tidyverse)
library(xgboost)
library(shinyjs)
library(sodium) # For password hashing
library(RSQLite) # For user database
library(shinyauthr) # For authentication helpers
library(googleAuthR)
library(httr)
library(jsonlite)

readRenviron("./.env")
google_client_id <- Sys.getenv("GOOGLE_CLIENT_ID")
google_client_secret <- Sys.getenv("GOOGLE_CLIENT_SECRET")
google_redirect_uri <- Sys.getenv("GOOGLE_REDIRECT_URI")

# Initialize SQLite database for users
init_db <- function() {
  con <- dbConnect(SQLite(), "users.db")
  if (!dbExistsTable(con, "users")) {
    dbExecute(con, "CREATE TABLE users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE,
      password TEXT,
      email TEXT UNIQUE,
      google_id TEXT UNIQUE,
      google_email TEXT,
      profile_picture TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )")
  }
  dbDisconnect(con)
}

# Initialize database
init_db()

# Authentication functions
check_credentials <- function(user, password) {
  con <- dbConnect(SQLite(), "users.db")
  on.exit(dbDisconnect(con))
  
  query <- sprintf("SELECT password FROM users WHERE username = '%s'", user)
  result <- dbGetQuery(con, query)
  
  if (nrow(result) == 1) {
    stored_hash <- result$password
    return(password_verify(stored_hash, password))
  }
  return(FALSE)
}

# Create user function
create_user <- function(username, password, email) {
  con <- dbConnect(SQLite(), "users.db")
  on.exit(dbDisconnect(con))
  
  hashed_password <- password_store(password)
  
  tryCatch({
    query <- sprintf(
      "INSERT INTO users (username, password, email) VALUES ('%s', '%s', '%s')",
      username, hashed_password, email
    )
    dbExecute(con, query)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}
# Load model data
final_model <- readRDS("final_model.rds")
test_data <- readRDS("test_data.rds")
final_predictions <- readRDS("final_predictions.rds")

# Load sales data
sales_data <- try(read_excel("Final.xlsx"))


if(is.data.frame(sales_data)) {
  sales_data <- sales_data[1:200, ]
  if(!inherits(sales_data$Date, "Date")) {
    sales_data$Date <- as.Date(sales_data$Date)
  }
}


# UI Components

# Login Page UI
loginUI <- function(id) {
  ns <- NS(id)
  
  div(
    class = "login-container",
    style = "max-width: 400px; margin: 100px auto; padding: 20px; background: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
    
    h2("Login", style = "text-align: center; color: #2c3e50;"),
    
    textInput("username", "Username"),
    passwordInput("password", "Password"),
    
    div(
      style = "text-align: center; margin-top: 20px;",
      actionButton("login", "Login", 
                   class = "btn-primary",
                   style = "width: 100%; margin-bottom: 10px;"),
      tags$hr(style = "margin: 20px 0;"),
      div(class = "auth-switch",
          "Or sign in with:",
      ),
          actionButton("google_signin", "Sign in with Google",
                       class = "auth-btn",
                       style = "background-color: #4285f4; margin-top: 10px;",
                       icon = icon("google")
          ),
      div(
        "Don't have an account?",
      actionButton("goto_signup", "Sign Up",
                   class = "btn-info",
                   style = "width: 100%;"),
      )
    )
    
  )
}

# Sign Up Page UI
signupUI <- function(id) {
  ns <- NS(id)
  
  div(
    class = "signup-container",
    style = "max-width: 400px; margin: 100px auto; padding: 20px; background: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
    
    h2("Sign Up", style = "text-align: center; color: #2c3e50;"),
    
    textInput("username", "Username"),
    textInput("email", "Email"),
    passwordInput("password", "Password"),
    passwordInput("confirm_password", "Confirm Password"),
    
    div(
      style = "text-align: center; margin-top: 20px;",
      actionButton("signup", "Sign Up", 
                   class = "btn-primary",
                   style = "width: 100%; margin-bottom: 10px;"),
      tags$hr(style = "margin: 20px 0;"),
      div(class = "auth-switch",
          "Or sign in with:",
      ),
      actionButton("google_signin", "Sign in with Google",
                   class = "auth-btn",
                   style = "background-color: #4285f4; margin-top: 10px;",
                   icon = icon("google")
      ),
      div(
        "Already an account?",
      actionButton("goto_login", "Back to Login",
                   class = "btn-info",
                   style = "width: 100%;")
      )
    )
  )
}

# Home Page UI
homeUI <- function(id) {
  ns <- NS(id)
  
  div(
    class = "home-container",
    style = "padding: 20px;",
    
    # Hero Section
    div(
      class = "hero",
      style = "text-align: center; padding: 60px 20px; background: linear-gradient(135deg, #2c3e50 0%, #3498db 100%); color: white; border-radius: 8px; margin-bottom: 40px;",
      
      h1("Welcome to Sales Analytics Dashboard", 
         style = "font-size: 2.5em; margin-bottom: 20px;"),
      p("Comprehensive sales analysis and forecasting platform",
        style = "font-size: 1.2em; margin-bottom: 30px;"),
      actionButton("goto_dashboard", "Go to Dashboard",
                   class = "btn-primary",
                   style = "font-size: 1.2em; padding: 10px 30px;")
    ),
    
    # Features Section
    fluidRow(
      style = "margin-bottom: 40px;",
      
      column(4,
             div(class = "feature-card",
                 style = "background: white; padding: 20px; border-radius: 8px; text-align: center; height: 250px;",
                 icon("chart-line", "fa-3x", style = "color: #3498db; margin-bottom: 20px;"),
                 h3("Real-time Analytics"),
                 p("Track your sales performance with interactive charts and real-time updates")
             )
      ),
      
      column(4,
             div(class = "feature-card",
                 style = "background: white; padding: 20px; border-radius: 8px; text-align: center; height: 250px;",
                 icon("robot", "fa-3x", style = "color: #3498db; margin-bottom: 20px;"),
                 h3("AI-Powered Forecasting"),
                 p("Make data-driven decisions with our advanced forecasting algorithms")
             )
      ),
      
      column(4,
             div(class = "feature-card",
                 style = "background: white; padding: 20px; border-radius: 8px; text-align: center; height: 250px;",
                 icon("file-export", "fa-3x", style = "color: #3498db; margin-bottom: 20px;"),
                 h3("Custom Reports"),
                 p("Generate and export customized reports for your business needs")
             )
      )
    )
  )
}

dashboardUI <- function(id) {
  fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      /* Global Styles */
      body {
        background-color: #f8f9fa;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        color: #2c3e50;
      }
      
      /* Header Styles */
      .main-header {
        background: linear-gradient(135deg, #2c3e50 0%, #3498db 100%);
        color: white;
        padding: 20px;
        margin-bottom: 30px;
        border-radius: 0 0 10px 10px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      /* Card Styles */
      .card {
        background: white;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }

      /* Sidebar Styles */
      .sidebar {
        background: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }

      /* Plot Styles */
      .plotly {
        margin: 15px 0;
        border-radius: 8px;
        background: white;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }

      /* Tab Styles */
      .nav-tabs {
        border-bottom: 2px solid #3498db;
      }

      .nav-tabs > li > a {
        color: #2c3e50;
        border: none;
        border-radius: 4px 4px 0 0;
      }

      .nav-tabs > li.active > a {
        background-color: #3498db;
        color: white;
      }

      /* Button Styles */
      .btn {
        background-color: #3498db;
        color: white;
        border: none;
        border-radius: 4px;
        padding: 8px 15px;
        transition: background-color 0.3s;
      }

      /* Metric Card Styles */
      .metric-card {
        background: white;
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 15px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }

      .metric-value {
        font-size: 24px;
        font-weight: bold;
        color: #3498db;
      }
      .card {
        background: white;
        border-radius: 12px;
        padding: 25px;
        margin-bottom: 25px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        transition: transform 0.3s ease;
      }
      
      .card:hover {
        transform: translateY(-5px);
      }
      
      .card-title {
        color: #2c3e50;
        border-bottom: 2px solid #3498db;
        padding-bottom: 10px;
        margin-bottom: 20px;
      }
      
      /* Enhanced Metric Card */
      .metric-card {
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        padding: 20px;
        border-radius: 12px;
        margin-bottom: 20px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        transition: all 0.3s ease;
      }
      
      .metric-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 6px 12px rgba(0,0,0,0.15);
      }
      
      .metric-value {
        font-size: 28px;
        font-weight: bold;
        color: #3498db;
      }
      
      .metric-label {
        color: #7f8c8d;
        font-size: 14px;
        margin-top: 5px;
      }
      
      /* Plot Container */
      .plotly {
        border-radius: 8px;
        overflow: hidden;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      /* Updated Sidebar Styles */
      #sidebarCol {
        transition: width 0.3s ease;
        padding: 15px;
      }
      
      #mainCol {
        transition: margin-left 0.3s ease;
      }
      
      .sidebar-collapsed {
        width: 50px !important;
        padding: 15px 5px !important;
      }
      
      .main-expanded {
        margin-left: 50px !important;
      }
      
      /* Hide sidebar content when collapsed */
      .sidebar-collapsed .sidebar-content {
        display: none;
      }
      
      /* Toggle Button Styles */
      .toggle-button {
        margin-bottom: 10px;
        cursor: pointer;
        background-color: #3498db;
        color: white;
        border: none;
        border-radius: 4px;
        padding: 8px 15px;
        width: 100%;
      }
      
      .sidebar-collapsed .toggle-button {
        padding: 8px;
        width: 40px;
      }
    "))
  ),
  
  # Updated JavaScript for toggle functionality
  tags$script(HTML("
    $(document).ready(function() {
      var sidebarCollapsed = false;
      
      $('#toggle_sidebar').click(function() {
        if (sidebarCollapsed) {
          $('#sidebarCol').removeClass('sidebar-collapsed');
          $('#mainCol').removeClass('main-expanded');
          $(this).html('Hide Sidebar');
        } else {
          $('#sidebarCol').addClass('sidebar-collapsed');
          $('#mainCol').addClass('main-expanded');
          $(this).html('⇌');
        }
        sidebarCollapsed = !sidebarCollapsed;
      });
    });
  ")),
  
  # Dashboard Header
  div(class = "main-header",
      h1("Integrated Sales Analytics Dashboard", align = "center"),
      p("Sales Performance Monitoring and Forecasting", align = "center"),
      
        
        div(
          class = "dropdown",
          actionButton(
            "logout_btn", 
            "Logout", 
            icon = icon("sign-out-alt"),
            style = "margin-top: -20px; margin-right: 10px; background-color: #e74c3c; position: absolute; right:0; color: white;"
          )
        )
      
  ),
  
  
  # Updated Layout Structure
  fluidRow(
    # Sidebar Column
    column(3, id = "sidebarCol",
           actionButton("toggle_sidebar", "Hide Sidebar", class = "toggle-button"),
           div(class = "sidebar-content",
               # [Previous sidebar content remains the same]
               h4("Sales Analysis Filters"),
               selectInput("product_category", "Product Category:", 
                           c("All", unique(sales_data$`Product Category`)), selected = "All"),
               dateRangeInput("date_range", "Date Range:", 
                              start = min(sales_data$Date), 
                              end = max(sales_data$Date)),
               selectInput("region", "Region:", 
                           c("All", unique(sales_data$Region)), selected = "All"),
               
               hr(),
               
               h4("Forecast Settings"),
               dateRangeInput("forecast_range",
                              "Forecast Period:",
                              start = min(test_data$date),
                              end = max(test_data$date)),
               
               
               
               downloadButton("download_predictions", "Download Forecast Data"),
               fileInput("file", "Upload CSV file with stock data", accept = c("xlxs"), multiple = FALSE),
               downloadButton("downloadTemplate", "Download Template", 
                              style = "margin: 15px; width: 90%; background-color: #1abc9c; border: none;"),
           )
    ),
    
    # Main Content Column
    column(9, id = "mainCol",
           # [Previous mainPanel content remains the same]
           tabsetPanel(
             # Historical Sales Analysis
             tabPanel("Sales Analysis",
                      fluidRow(
                        column(3, div(class = "metric-card",
                                      icon("shopping-cart"), 
                                      h4("Total Units Sold"),
                                      div(class="metric-value", textOutput("total_units")))),
                        column(3, div(class = "metric-card",
                                      icon("dollar-sign"),
                                      h4("Total Revenue"),
                                      div(class="metric-value", textOutput("total_revenue")))),
                        column(3, div(class = "metric-card",
                                      icon("chart-line"),
                                      h4("Average Price"),
                                      div(class="metric-value", textOutput("avg_price")))),
                        column(3, div(class = "metric-card",
                                      icon("percentage"),
                                      h4("Growth Rate"),
                                      div(class="metric-value", textOutput("growth_rate"))))
                      ),
                      
                      # Trends Section
                      div(class = "card",
                          h3("Sales Trends", class = "card-title"),
                          fluidRow(
                            column(6, plotlyOutput("units_sold_plot")),
                            column(6, plotlyOutput("revenue_plot"))
                          )
                      ),
                      
                      # Analysis Section
                      div(class = "card",
                          h3("Deep Dive Analysis", class = "card-title"),
                          fluidRow(
                            column(4, plotlyOutput("daily_pattern_plot")),
                            column(4, plotlyOutput("monthly_pattern_plot")),
                            column(4, plotlyOutput("region_comparison_plot"))
                          )
                      ),
                      
                      # Distribution Analysis
                      div(class = "card",
                          h3("Distribution Analysis", class = "card-title"),
                          fluidRow(
                            column(6, plotlyOutput("sales_distribution_plot")),
                            column(6, plotlyOutput("revenue_by_category_plot"))
                          )
                      )
             ),
             
             # Enhanced Forecast Tab
             tabPanel("Sales Forecast",
                      div(class = "card",
                          h3("Forecast Overview"),
                          fluidRow(
                            column(8, plotlyOutput("forecast_plot", height = "400px")),
                            column(4, 
                                   div(class = "metric-card",
                                       h4("Forecast Accuracy"),
                                       div(class="metric-value", textOutput("forecast_accuracy")),
                                       div(class="metric-label", "Accuracy"))
                                   
                            )
                          )
                      ),
                      
                      fluidRow(
                        column(6,
                               div(class = "card",
                                   h3("Feature Importance"),
                                   plotOutput("importance_plot"))),
                        column(6,
                               div(class = "card",
                                   h3("Forecast Error Analysis"),
                                   plotlyOutput("error_distribution_plot")))
                      ),
                      
                      div(class = "card",
                          h3("Seasonal Patterns"),
                          fluidRow(
                            column(6, plotlyOutput("seasonal_decomposition_plot")),
                            column(6, plotlyOutput("forecast_components_plot"))
                          )
                      )
             ),
             
             # Enhanced Data View Tab
             tabPanel("Data View",
                      div(class = "card",
                          h3("Historical Sales Data"),
                          DTOutput("sales_table")),
                      div(class = "card",
                          h3("Forecast Results"),
                          DTOutput("forecast_table"))
             )    )
    )
  )
)
}


# Modified UI
ui <- function(request) {
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$script("
        Shiny.addCustomMessageHandler('redirectToGoogle', function(authUrl) {
          window.location.href = authUrl;
        });
      ")
    ),
    
    # Include custom CSS
    tags$head(
      tags$style(HTML("
        /* Your existing CSS styles */
        
        /* Additional styles for authentication pages */
        .auth-container {
          max-width: 400px;
          margin: 100px auto;
          padding: 20px;
          background: white;
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        .btn-primary {
          background-color: #3498db;
          border-color: #3498db;
          color: white;
          transition: all 0.3s ease;
        }
        
        .btn-primary:hover {
          background-color: #2980b9;
          border-color: #2980b9;
        }
        
        .feature-card {
          transition: transform 0.3s ease;
        }
        
        .feature-card:hover {
          transform: translateY(-10px);
        }
      "))
    ),
    
    # Navigation state
    uiOutput("page")
  )
}

## Google OAuth
# Add these Google authentication functions
setup_google_auth <- function() {
  #print("heresetup")
  options(googleAuthR.scopes.selected = c(
    "https://www.googleapis.com/auth/userinfo.email",
    "https://www.googleapis.com/auth/userinfo.profile"
  ))
  
  # Replace with your OAuth credentials
  options(googleAuthR.webapp.client_id =google_client_id,
          googleAuthR.webapp.client_secret =google_client_secret,
          googleAuthR.webapp.redirect_uri =google_redirect_uri)
  # Configure for same window authentication
  # options(googleAuthR.webapp.use_basic_auth = TRUE,
  #         googleAuthR.webapp.redirect_on_signout = TRUE,
  #         googleAuthR.webapp.popup = FALSE)
}


get_google_user_info <- function(access_token) {
  # Get user info from Google using the access token directly in the header
  response <- GET(
    "https://www.googleapis.com/oauth2/v1/userinfo",
    add_headers(Authorization = paste("Bearer", access_token))
  )
  
  if (status_code(response) == 200) {
    user_info <- fromJSON(rawToChar(response$content))
    return(user_info)
  }
  return(NULL)
}
# Handle the OAuth code exchange and user authentication
handle_google_auth <- function(code, session) {
  #print("herehandle")
  # Exchange the authorization code for an access token
  token_response <- POST(
    "https://oauth2.googleapis.com/token",
    body = list(
      code = code,
      client_id = google_client_id,
      client_secret = google_client_secret,
      redirect_uri = google_redirect_uri,
      grant_type = "authorization_code"
    ),
    encode = "form"
  )
  
  if (status_code(token_response) == 200) {
    token_data <- fromJSON(rawToChar(token_response$content))
    access_token <- token_data$access_token
    
    # Get user info using the access token
    user_info <- get_google_user_info(access_token)
    
    if (!is.null(user_info)) {
      # Connect to database
      con <- dbConnect(SQLite(), "users.db")
      # Check if user exists
      existing_user <- dbGetQuery(con, sprintf(
        "SELECT * FROM users WHERE google_id = '%s' OR email = '%s'",
        user_info$id, user_info$email
      ))
      
      if (nrow(existing_user) == 0) {
        # Create new user
        dbExecute(con, 
                  "INSERT INTO users (username, email, google_id, google_email, profile_picture) 
           VALUES (?, ?, ?, ?, ?)",
                  params = list(
                    user_info$email,
                    user_info$email,
                    user_info$id,
                    user_info$email,
                    user_info$picture
                  )
        )
        
    
        # Get the newly created user
        user <- dbGetQuery(con, sprintf(
          "SELECT * FROM users WHERE google_id = '%s'",
          user_info$id
        ))
      } else {
        # Update existing user's Google info
        dbExecute(con,
                  "UPDATE users SET 
           google_id = ?, 
           google_email = ?, 
           profile_picture = ? 
           WHERE email = ?",
                  params = list(
                    user_info$id,
                    user_info$email,
                    user_info$picture,
                    user_info$email
                  )
        )
        user <- existing_user
      }
      
      # Return user data
      return(list(
        username = user_info$email,
        email = user_info$email,
        profile_picture = user_info$picture
      ))
    }
  }
  return(NULL)
}

# Server logic
server <- function(input, output, session) {
  # Authentication state
  credentials <- reactiveVal(NULL)
  user_data <- reactiveVal(NULL)
  current_page <- reactiveVal("home")
  
  # Page routing
  output$page <- renderUI({
    if (is.null(credentials())) {
      switch(current_page(),
             "home" = homeUI("home"),
             "login" = loginUI("login"),
             "signup" = signupUI("signup"),
              "main" = dashboardUI("main"))
    } else {
      # Your existing dashboard UI code
      current_page("main")
    }
  })
  
  # Login handler
  observeEvent(input$login, {
    if (check_credentials(input$username, input$password)) {
      
      current_page("main")
      showNotification("successfull")
    } else {
      showNotification("Invalid credentials", type = "error")
    }
  })
  
  # Signup handler
  observeEvent(input$signup, {
    if (input$password != input$confirm_password) {
      showNotification("Passwords don't match!", type = "error")
      return()
    }
    
    if (create_user(input$username, input$password, input$email)) {
      current_page("main")
      showNotification("successfull")
    } else {
      showNotification("Username or email already exists", type = "error")
    }
  })
  
  # Navigation handlers
  observeEvent(input$goto_dashboard, {
    if (is.null(credentials())) {
      current_page("login")
    }
    else{
      current_page("main")
    }
  })
  
  observeEvent(input$goto_signup, {
    current_page("signup")
  })
  
  observeEvent(input$goto_login, {
    current_page("login")
  })
  
  # Logout handler

  observeEvent(input$logout_btn, {
    # Clear user data
    credentials(NULL)
    current_page("home")
    
    # Reset app state to landing page
   
    # Optional: Show a logout notification
    showNotification("You have been logged out.", type = "message")
  })
  
  # Setup Google Authentication
  # Setup Google Authentication
  setup_google_auth()
  # Handle Google Sign-In button click
  observeEvent(input$google_signin, {
    # Create OAuth URL
    auth_url <- paste0(
      "https://accounts.google.com/o/oauth2/v2/auth?",
      "client_id=", google_client_id,
      "&redirect_uri=", URLencode(google_redirect_uri, reserved = TRUE),
      "&response_type=code",
      "&scope=", URLencode("email profile openid https://www.googleapis.com/auth/userinfo.profile https://www.googleapis.com/auth/userinfo.email", reserved = TRUE)
    )
    
    # Redirect to Google login
    session$sendCustomMessage("redirectToGoogle", auth_url)
  })
  
  # Handle the OAuth callback observer
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$code)) {
      # Handle the authorization code
      user <- handle_google_auth(query$code, session)
      #print(user)
      if (!is.null(user)) {
        credentials<- reactiveVal(user)
        current_page("main")
        showNotification("successful login",type="message")
      } else {
        showNotification("Failed to authenticate with Google",type="error")
        current_page("login")
      }
    }
  })
  
  # Your existing server logic here
  filtered_sales <- reactive({
    query <- sales_data
    
    if (input$product_category != "All") {
      query <- query %>% filter(`Product Category` == input$product_category)
    }
    
    if (input$region != "All") {
      query <- query %>% filter(Region == input$region)
    }
    
    query %>% filter(Date >= input$date_range[1] & Date <= input$date_range[2])
  })
  
  # Filtered forecast data
  filtered_forecast <- reactive({
    data.frame(
      Date = test_data$date,
      Actual = test_data$sales,
      Predicted = final_predictions
    ) %>%
      filter(Date >= input$forecast_range[1] & Date <= input$forecast_range[2])
  })
  
  # Sales Analysis Outputs
  output$total_units <- renderText({
    sum(filtered_sales()$`Units Sold`) %>% format(big.mark = ",")
  })
  
  output$total_revenue <- renderText({
    paste0("$", format(sum(filtered_sales()$Revenue), big.mark = ","))
  })
  
  output$avg_price <- renderText({
    paste0("$", round(mean(filtered_sales()$Revenue / filtered_sales()$`Units Sold`), 2))
  })
  
  output$units_sold_plot <- renderPlotly({
    plot_ly(filtered_sales(), x = ~Date, y = ~`Units Sold`, type = "scatter", mode = "lines") %>%
      layout(title = "Units Sold Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Units Sold"))
  })
  
  output$revenue_plot <- renderPlotly({
    plot_ly(filtered_sales(), x = ~Date, y = ~Revenue, type = "scatter", mode = "lines") %>%
      layout(title = "Revenue Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Revenue"))
  })
  
  # Forecast Outputs
  output$forecast_content <- renderUI({
    switch(input$forecast_view,
           "overall" = plotlyOutput("forecast_plot"),
           "importance" = plotOutput("importance_plot"),
           "metrics" = plotOutput("metrics_plot"))
  })
  
  output$forecast_plot <- renderPlotly({
    plot_ly(filtered_forecast()) %>%
      add_lines(x = ~Date, y = ~Actual, name = "Actual",
                line = list(color = "blue")) %>%
      add_lines(x = ~Date, y = ~Predicted, name = "Predicted",
                line = list(color = "red")) %>%
      layout(title = "Sales Forecast vs Actual",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Sales"))
  })
  
  output$importance_plot <- renderPlot({
    importance_matrix <- xgb.importance(feature_names = features, model = final_model)
    
    ggplot(importance_matrix, aes(x = reorder(Feature, Gain), y = Gain)) +
      geom_bar(stat = "identity", fill = "#3498db") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Feature Importance",
           x = "Features",
           y = "Gain")
  })
  
  
  # Tables
  output$sales_table <- renderDT({
    filtered_sales() %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$forecast_table <- renderDT({
    filtered_forecast() %>%
      mutate(Error = abs(Actual - Predicted),
             Error_Pct = round(Error / Actual * 100, 2)) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Model Information
  output$model_info <- renderPrint({
    params <- final_model$params
    cat("Model Parameters:\n")
    cat("Max Depth:", params$max_depth, "\n")
    cat("Learning Rate:", params$eta, "\n")
    cat("Min Child Weight:", params$min_child_weight, "\n")
    cat("Subsample:", params$subsample, "\n")
    cat("Column Sample:", params$colsample_bytree, "\n")
  })
  
  
  
  # Download handler
  output$download_predictions <- downloadHandler(
    filename = function() {
      paste("sales_forecast_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_forecast(), file, row.names = FALSE)
    }
  )
  # Add to server function:
  
  # Daily Pattern Plot
  output$daily_pattern_plot <- renderPlotly({
    daily_pattern <- filtered_sales() %>%
      group_by(weekday = weekdays(Date)) %>%
      summarise(avg_sales = mean(`Units Sold`))
    
    plot_ly(daily_pattern, x = ~weekday, y = ~avg_sales, type = "bar",
            marker = list(color = "#3498db")) %>%
      layout(title = "Average Sales by Day of Week",
             xaxis = list(title = "Day"),
             yaxis = list(title = "Average Units Sold"))
  })
  
  # Monthly Pattern Plot
  output$monthly_pattern_plot <- renderPlotly({
    monthly_pattern <- filtered_sales() %>%
      group_by(month = month(Date, label = TRUE)) %>%
      summarise(total_sales = sum(Revenue))
    
    plot_ly(monthly_pattern, x = ~month, y = ~total_sales, type = "bar",
            marker = list(color = "#2ecc71")) %>%
      layout(title = "Monthly Sales Distribution",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Total Revenue"))
  })
  
  # Region Comparison Plot
  output$region_comparison_plot <- renderPlotly({
    region_sales <- filtered_sales() %>%
      group_by(Region) %>%
      summarise(total_sales = sum(Revenue))
    
    plot_ly(region_sales, labels = ~Region, values = ~total_sales, type = "pie",
            hole = 0.4) %>%
      layout(title = "Sales by Region")
  })
  
  # Sales Distribution Plot
  output$sales_distribution_plot <- renderPlotly({
    plot_ly(filtered_sales(), type = "histogram", x = ~`Units Sold`,
            nbinsx = 30, marker = list(color = "#9b59b6")) %>%
      layout(title = "Sales Distribution",
             xaxis = list(title = "Units Sold"),
             yaxis = list(title = "Frequency"))
  })
  
  # Revenue by Category Plot
  output$revenue_by_category_plot <- renderPlotly({
    category_revenue <- filtered_sales() %>%
      group_by(`Product Category`) %>%
      summarise(total_revenue = sum(Revenue)) %>%
      arrange(desc(total_revenue))
    
    plot_ly(category_revenue, x = ~`Product Category`, y = ~total_revenue, type = "bar",
            marker = list(color = "#e74c3c")) %>%
      layout(title = "Revenue by Product Category",
             xaxis = list(title = "Category"),
             yaxis = list(title = "Total Revenue"))
  })
  
  # Seasonal Decomposition Plot
  output$seasonal_decomposition_plot <- renderPlotly({
    # Create time series object
    ts_data <- ts(filtered_sales()$`Units Sold`, frequency = 7)
    decomp <- decompose(ts_data)
    
    plot_ly() %>%
      add_lines(x = ~time(ts_data), y = ~decomp$seasonal, name = "Seasonal",
                line = list(color = "#3498db")) %>%
      add_lines(x = ~time(ts_data), y = ~decomp$trend, name = "Trend",
                line = list(color = "#e74c3c")) %>%
      layout(title = "Time Series Decomposition",
             xaxis = list(title = "Time"),
             yaxis = list(title = "Component Value"))
  })
  
  # Error Distribution Plot
  output$error_distribution_plot <- renderPlotly({
    errors <- filtered_forecast() %>%
      mutate(error = Actual - Predicted)
    
    plot_ly(errors, type = "histogram", x = ~error,
            marker = list(color = "#3498db")) %>%
      layout(title = "Forecast Error Distribution",
             xaxis = list(title = "Error"),
             yaxis = list(title = "Frequency"))
  })
  
  # Growth Rate Calculation
  output$growth_rate <- renderText({
    sales <- filtered_sales() %>%
      arrange(Date)
    
    first_value <- head(sales$Revenue, 1)
    last_value <- tail(sales$Revenue, 1)
    growth <- ((last_value - first_value) / first_value) * 100
    
    paste0(round(growth, 1), "%")
  })
  
  # Forecast Accuracy
  output$forecast_accuracy <- renderText({
    accuracy <- 100 - mean(abs((filtered_forecast()$Actual - 
                                  filtered_forecast()$Predicted) / 
                                 filtered_forecast()$Actual)) * 100
    paste0(round(accuracy, 1), "%")
  })
  
  # Predicted Growth
  output$predicted_growth <- renderText({
    forecast_data <- filtered_forecast()
    last_actual <- tail(forecast_data$Actual, 1)
    last_predicted <- tail(forecast_data$Predicted, 1)
    growth <- ((last_predicted - last_actual) / last_actual) * 100
    paste0(round(growth, 1), "%")
  })
 create_template_dataset <- function() {
    # Create sample dates
    dates <- seq(as.Date("2024-01-01"), as.Date("2024-01-05"), by="days")
    
    # Create template dataframe
    template_df <- data.frame(
        Date = dates,
        Region = rep(c("North", "South", "East", "West", "Central"), length.out = 5),
        Product = rep(c("Pain Relief", "Allergy Medication", "Antibiotics", "Vitamins", "Cold & Flu"), length.out = 5),
        Category = rep(c("OTC", "Prescription", "Supplement", "Specialty", "Generic"), length.out = 5),
        `Medication Name` = rep(c("Paracetamol", "Antihistamine", "Amoxicillin", "Multivitamin", "Decongestant"), length.out = 5),
        `Units Sold` = sample(500:1500, 5, replace = TRUE),
        Revenue = round(runif(5, 5000, 15000), 2),
        `Customer Age Group` = rep(c("18-30", "31-45", "46-60", "61+", "Under 18"), length.out = 5),
        `Customer Gender` = rep(c("Male", "Female", "Non-Binary", "Female", "Male"), length.out = 5),
        `Sales Channel` = rep(c("Online", "Pharmacy", "Hospital", "Retail", "Clinic"), length.out = 5),
        `Discount Offered` = round(runif(5, 0, 20), 2)
    )
    
    return(template_df)
}
  
 # Function to write template to temp file and get path
 get_template_path <- function() {
   # Create template
   template_df <- create_template_dataset()
   
   # Create temp file
   tmp <- tempfile(fileext = ".csv")
   write.csv(template_df, tmp, row.names = FALSE)
   
   return(tmp)
 }
 
 output$downloadTemplate <- downloadHandler(
   filename = function() {
     paste("sales_template_", format(Sys.Date(), "%Y%m%d"), ".csv", sep="")
   },
   content = function(file) {
     template_path <- get_template_path()
     file.copy(template_path, file)
     unlink(template_path)
   }
 )
 observe({
   req(user_data())
   sales_data <- uploaded_data()
 })
 # Load data from user-uploaded file
 uploaded_data <- reactive({
   if (!is.null(input$file)) {
     data <- read.csv(input$file$datapath)
     data$Date <- as.Date(data$Date)  # Ensure Date column is in Date format
     required_cols <- c("Region", "Region", "Product", "Category", "Date")
     missing_cols <- setdiff(required_cols, colnames(data))
     
     if (length(missing_cols) == 0) {
       return(data)
     } else {
       showNotification(
         paste("Uploaded file is missing required columns:", paste(missing_cols, collapse = ", ")),
         type = "error"
       )
       return(NULL)
     }
   } else {
     return(NULL)
   }
 })
 
}

# Run the application
shinyApp(ui = ui, server = server)
