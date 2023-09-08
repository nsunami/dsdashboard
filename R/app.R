library(shiny)
library(bslib)
library(here)
source(here("R/load_data.R"))

SKIP_LOGIN <- Sys.getenv("SKIP_LOGIN") |>
  as.logical()
if (is.na(SKIP_LOGIN)) {
  SKIP_LOGIN <- FALSE
}

# EUR Corporate colors
eur_pal <- list(
  deep_green = "#002328",
  bright_green = "#0c8066",
  grey = "#e3dad8"
)

# Preparing login ==========
# Login credentials
users <- tibble::tribble(
  ~username,                ~password,                    ~permission, ~name,
  Sys.getenv("ADMIN_USER"), Sys.getenv("ADMIN_PASSWORD"), "admin",     "Admin",
)
# Login tab
login_panel <- bslib::nav_panel(
  title = icon("lock"),
  value = "login",
  shinyauthr::loginUI("login")
)


dashboard <- function(...) {
  # Load requests data
  requests <- load_data()
  
  # Load faculty 
  faculties <- read.csv2(here("data/public/researchers_by_faculty.csv"))
  
  # Prepare the summary table
  summary_year <- requests |>
    dplyr::count(faculty, name = "requests") |>
    dplyr::left_join(faculties, by = "faculty") |>
    dplyr::select(-date)
  
  # Get the unique values from all the variables  
  uniques <- requests |>
    purrr::map(~unique(.))
  date_range <- c(min(uniques$date), max(uniques$date))
  
  # UI Component - Overview Panel ====
  overview_panel <- bslib::nav_panel(
    title = "Overview",
    h1("2022 Requests"),
    card(
      layout_sidebar(
        sidebar = sidebar(
          title = "Settings",
          icon = bsicons::bs_icon("gear"),
          # Dates selector
          dateRangeInput(
            "dateRange",
            label = "Dates",
            start = date_range[[1]],
            end = date_range[[2]],
            min = date_range[[1]],
            max = date_range[[2]]
          ),
          # Faculty Selector
          selectInput(
            "selectedFaculty",
            multiple = TRUE,
            label = "Faculty", 
            choices = uniques$faculty, 
            selected = uniques$faculty
          ),
          # Type selector
          selectInput(
            "selectedTypes",
            multiple = TRUE,
            label = "Request Types", 
            choices = uniques$req_type, 
            selected = uniques$req_type
          ),
        ),
        layout_columns(
          bslib::value_box(
            title = "Total",
            value = textOutput("total_requests"),
            showcase = bsicons::bs_icon("check2-all")
          ),
          bslib::value_box(
            title = "Ave. weekly requests",
            value = textOutput("weekly_average"),
            showcase = bsicons::bs_icon("speedometer2")
          ),
          bslib::value_box(
            title = "Busiest month",
            value = textOutput("busiest_month"),
            textOutput("busiest_month_requests"),
            showcase = bsicons::bs_icon("stack")
          )
        ),
        layout_columns(
          plotly::plotlyOutput("requests_over_time"),
          plotly::plotlyOutput("requests_by_type")
        ),
      ),
    )
  )
  
  # Put together the whole UI ====
  ui <- page_navbar(
    id = "navbar",
    title = "DS Dashboard",
    theme = bs_theme(version = 5) |>
      bs_theme_update(
        primary = "#0c8066",
        secondary = "#002328", 
        info = "#e3dad8",
        font_scale = NULL, 
        bootswatch = "cosmo"
      ),
    login_panel
  )
  
  server <- function(input, output, session) {
    
    # Login server functions ======
    credentials <- shinyauthr::loginServer(
      id = "login",
      data = users,
      user_col = username,
      pwd_col = password,
      log_out = reactive(logout_init())
    )
    
    logout_init <- shinyauthr::logoutServer(
      id = "logout",
      active = reactive(credentials()$user_auth)
    )
    
    # Replace tabs for login
    replace_tabs <- function(){
      # remove the login tab
      removeTab("navbar", "login")
      # add home tab 
      appendTab("navbar", overview_panel, select = TRUE)
    }
    
    # Take care the post-login behavior
    observeEvent(credentials()$user_auth, {
      # if user logs in successfully
      if(credentials()$user_auth) { 
        replace_tabs()
      }
    })
    if (SKIP_LOGIN) { replace_tabs() }
    
    # Theming =======
    # bs_themer()
    
    # Filter data based on the settings ====
    selected_requests <- reactive({
      requests |> 
        # Filter by date
        dplyr::filter(faculty %in% input$selectedFaculty) |> 
        # Filter by date
        dplyr::filter(
          date > input$dateRange[[1]],
          date < input$dateRange[[2]]
        ) |>     
        # Filter by request type
        dplyr::filter(req_type %in% input$selectedTypes)
    })
    
    # Total Requests
    output$total_requests <- renderText({
      total_requests <- selected_requests() |>
        nrow()
      paste0(total_requests, " requests")
    })
    
    # Calculating weekly average ====
    output$weekly_average <- renderText({
      weekly_average <- selected_requests() |>
        dplyr::count(week) |>
        dplyr::summarise(weekly_average = mean(n)) |>
        dplyr::pull()
      scales::label_number(suffix = " per week")(weekly_average)
    })
    
    # Busiest Month ====
    monthly_df <- reactive({
      selected_requests() |>
        dplyr::mutate(month = lubridate::floor_date(date, "month")) |>
        dplyr::count(month) |>
        dplyr::arrange(desc(n)) 
      
    })
    
    output$busiest_month <- renderText({
      monthly_df() |>
        dplyr::slice(1) |>
        dplyr::pull(month) |>
        format("%B")
    })
    
    output$busiest_month_requests <- renderText({
      num_requests <- monthly_df() |>
        dplyr::slice(1) |>
        dplyr::pull(n)
      paste0(
        "with ",
        num_requests,
        " requests"
      )
    })
    
    # Plotting requests over time ====
    output$requests_over_time <- plotly::renderPlotly({
      selected_requests() |>
        dplyr::group_by(week) |> 
        dplyr::count() |>
        plotly::plot_ly() |> 
        plotly::add_bars(
          x = ~week, 
          y = ~n,
          marker = list(color = eur_pal$bright_green),
          hovertemplate = "In the week of %{x}, we had %{y:.0f} requests<extra></extra>"
        ) |> 
        plotly::layout(
          yaxis = list(title = FALSE,
                       fixedrange = TRUE)
        ) |> 
        # Horizontal y-axis label 
        plotly::add_annotations(
          xref = "paper",
          yref = "paper",
          x = 0,
          y = 1,
          showarrow = FALSE,
          yanchor = "bottom",
          xanchor = "right",
          text = "Requests"
        ) |>
        plotly::layout(xaxis = list(title = "Week")) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    # Requests by type =======
    output$requests_by_type <- plotly::renderPlotly({
      selected_requests() |>
        dplyr::count(req_type) |>
        dplyr::arrange(n) |>
        plotly::plot_ly() |> 
        plotly::add_bars(
          y = ~req_type, 
          x = ~n,
          marker = list(color = eur_pal$bright_green),
          hovertemplate = "We had %{x:.0f} requests about \"%{y}\" <extra></extra>"
        ) |> 
        plotly::layout(
          xaxis = list(
            title = FALSE,
            fixedrange = TRUE
          ),
          yaxis = list(
            title = FALSE,
            categoryorder = "sum ascending"
          )
        ) |> 
        plotly::config(displayModeBar = FALSE)
    })
    
  }
  shinyApp(ui, server)
}

dashboard()