library(shiny)
library(bslib)
library(here)
source(here("R/load_data.R"))
source(here("R/utils-pipe.R"))
source(here("R/weekly_bar.R"))


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
login_panel <- nav_panel(
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
  overview_panel <- nav_panel(
    title = "Overview",
    h1("2022 Requests"),
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
        showcase = bsicons::bs_icon("stack")
      )
    ),
    layout_columns(
      card(
        plotly::plotlyOutput("requests_over_time"))
    )
  )
  
  # UI Component - Details Panel ====
  details_panel <- nav_panel(
    title = "Details",
    weekly_bar_UI("weeklyBar",
                  date_range = date_range,
                  uniques = uniques,
                  requests = requests)
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
    if (SKIP_LOGIN) {
      overview_panel
    } else {
      login_panel
    },
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
    # Take care the post-login behavior
    observeEvent(credentials()$user_auth, {
      # if user logs in successfully
      if(credentials()$user_auth) { 
        # remove the login tab
        removeTab("navbar", "login")
        # add home tab 
        appendTab("navbar", overview_panel, select = TRUE)
      }
    })
    
    # Theming =======
    # bs_themer()
    
    # Weekly bar server function ====
    weekly_bar_server("weeklyBar", requests)
    # Total Requests
    output$total_requests <- renderText({
      total_requests <- requests |>
        nrow()
      paste0(total_requests, " requests")
    })
    
    # Calculating weekly average ====
    output$weekly_average <- renderText({
      weekly_average <- requests |>
        dplyr::count(week) |>
        dplyr::summarise(weekly_average = mean(n)) |>
        dplyr::pull()
      
      scales::label_number(suffix = " per week")(weekly_average)
    })
    
    # Busiest Month ====
    output$busiest_month <- renderText({
      requests |>
        dplyr::mutate(month = lubridate::floor_date(date, "month")) |>
        dplyr::count(month) |>
        dplyr::arrange(desc(n)) |>
        dplyr::slice(1) |>
        dplyr::pull(month) |>
        format("%B")
    })
    
    # Plotting requests over time ====
    output$requests_over_time <- plotly::renderPlotly({
      requests |>
        dplyr::group_by(week) |> 
        dplyr::count() |>
        plotly::plot_ly() |> 
        plotly::add_bars(
          x = ~week, 
          y = ~n,
          marker = list(color = eur_pal$bright_green),
          hovertemplate = "In the week of %{x}, we had %{y:.0f} requests<extra></extra>"
        ) |> 
        plotly::layout(yaxis = list(title = FALSE,
                                    fixedrange = TRUE)) |> 
        # Horizontal y-axis label 
        plotly::add_annotations(xref = "paper",
                                yref = "paper",
                                x = 0,
                                y = 1,
                                showarrow = FALSE,
                                yanchor = "bottom",
                                xanchor = "right",
                                text = "Requests") |>
        plotly::layout(xaxis = list(title = "Week")) |>
        plotly::config(displayModeBar = FALSE)
      
    })
    
  }
  shinyApp(ui, server)
}

dashboard()