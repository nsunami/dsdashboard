# Load requests dataset
load_data <- function(path = "data/public/2022_requests_without_names_and_notes.csv"){
  ## Load dataset
  requests_raw <- readr::read_csv2(here::here(path))
  
  # Fix date 
  requests_raw <- requests_raw |> 
    dplyr::mutate(Date = dplyr::case_when(Date == "31/08/2021" ~ "31/08/2022",
                                          TRUE ~ Date))
  
  
  # Clean up the slash dates and kebab dates
  requests_raw <- requests_raw |> 
    dplyr::mutate(date_slashes = lubridate::parse_date_time(Date, "%d/%m/%Y"),
                  date_kebab = lubridate::parse_date_time(Date, "%d-%m-%Y"),
                  date_coalesce = dplyr::coalesce(date_slashes, date_kebab))
  
  
  ## Clean up variable names
  requests <- requests_raw |> 
    dplyr::transmute(date = date_coalesce,
                     faculty = Faculty,
                     # requester = `Name researcher`,
                     req_type = `Type of request`,
                     # notes = Notes
                     )
  
  ## Date to date datatype
  # requests <- requests %>% 
  #   mutate(date = as.Date(date))
  
  ## Week column 
  requests <- requests |> 
    dplyr::mutate(week = lubridate::floor_date(date, unit = "week"))
  
  # Return dataset 
  requests
}

