weekly_bar_UI <- function(id, date_range, uniques, requests) {
  ## Render UI
  layout_columns(
    style = "grid-template-columns: 280px 1fr",
    card(
      # Dates - Input range 
      dateRangeInput(NS(id, "dateRange"),
                     label = "Dates",
                     start = date_range[[1]],
                     end = date_range[[2]],
                     min = date_range[[1]],
                     max = date_range[[2]]),
      
      # Dates - Histogram Slider
      histoslider::input_histoslider(NS(id, "dateRangeHisto"),
                                     label = "",
                                     values = requests$date,
                                     breaks = "weeks",
                                     start = 0,
                                     end = length(requests$date),
                                     options = list(
                                       handleLabelFormat = "%b %e, %y"
                                     )),
      
      # Faculty Selector
      selectInput(NS(id, "selectedFaculty"),
                  multiple = TRUE,
                  label = h3("Faculty"), 
                  choices = uniques$faculty, 
                  selected = uniques$faculty),
      
      # Type selector
      selectInput(NS(id, "selectedTypes"),
                  multiple = TRUE,
                  label = h3("Request Types"), 
                  choices = uniques$req_type, 
                  selected = uniques$req_type),
      
      
    ),
    card(
      full_screen = TRUE,
      card_body(
        plotly::plotlyOutput(NS(id, "weeklyBar"))
      )
    )
  )
}

weekly_bar_server <- function(id, requests) {
  
  
  moduleServer(id, function(input, output, session) {
    
    # Observe changes in dates in histo-slider
    # observeEvent(input$dateRange, {
    #   selectedDates <- lubridate::date(input$dateRange)
    #   updateDateRangeInput("dateRangeHisto",
    #                        session = session,
    #                        start = selectedDates[[1]],
    #                        end = selectedDates[[2]])
    # }, ignoreInit = TRUE)
    
    selected_requests <- reactive({requests |> 
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
    
    output$weeklyBar <- plotly::renderPlotly({
      selected_requests() |>
        dplyr::group_by(week, faculty) |> 
        dplyr::count() |>
        plotly::plot_ly() |> 
        plotly::add_bars(x = ~week, y = ~n, color = ~faculty) |> 
        plotly::layout(barmode = "stack") |> 
        plotly::layout(yaxis = list(title = "Requests")) |> 
        plotly::layout(xaxis = list(title = "Week"))
    })
    
  })
}