#' Read local FARS data
#' 
#' \code{fars_read} reads data from the US National Highway
#' Traffic Safety Administration's Fatality Analysis Reporting
#' System (FARS), which is a nationwide census providing the
#' American public yearly data regarding fatal injuries suffered
#' in motor vehicle traffic crashes.
#' 
#' @param filename Path to a FARS csv file (optionally compressed).
#' @return A tibble representing the FARS data or an error
#'   if the file does not exist.
#' @examples
#' \dontrun{
#' fars_read("accident_2015.csv.bz2")
#' fars_read(make_filename(2015))
#' }
#'
#' @importFrom dplyr as_tibble
#' @importFrom readr read_csv
#' @export
fars_read <- function(filename) {
  print(getwd())
  if (!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::as_tibble(data)
}


#' Create a filename for FARS accident data for a given year.
#' 
#' @param year Year of accident data.
#' @return filename of accident data, e.g. "accident_2015.csv.bz2".
#' @examples
#' make_filename(2015)
#' make_filename("2015")
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read multiple years of FARS data.
#' 
#' \code{fars_read_years} will read all the given years of FARS data.
#' 
#' @param years Vector or list of years to read.
#' @return A list of tibbles representing FARS data. If data for a year is
#'   missing, an empty list element is returned together with a warning.
#' @examples
#' \dontrun{
#'   fars_read_years(c(2015, 2016))
#' }
#' 
#' @importFrom dplyr mutate select
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dat %>%
        dplyr::mutate(year = year) %>%
        dplyr::select(.data$MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Number of fatal injuries per month and year.
#' 
#' \code{fars_summarize_years} summarizes FARS data for the given years
#' into the number of fatal injuries per month and year.
#' 
#' @param years Vector or list of years to read.
#' @return A tibble of monthly accidents per month and year.
#' @examples
#' \dontrun{
#'   fars_summarize_years(c(2015, 2016))
#' }
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
#' @importFrom tidyr spread
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(.data$year, .data$MONTH) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    tidyr::spread(.data$year, .data$n)
}


#' Show locations of fatal injuries in a given state and during a given year.
#' 
#' \code{fars_map_state} shows locations of the fatal injuries for a given
#' state and year on a map.
#' 
#' @param state.num The number of the state to show locations from.
#' @param year The year for which to show locations of fatal injuries.
#' @return A map / a plot or an error if there were no fatal injuries to show.
#' @examples
#' \dontrun{
#' fars_map_state(1, 2015)
#' }
#' 
#' @importFrom dplyr filter
#' @importFrom graphics points
#' @importFrom magrittr `%>%`
#' @importFrom maps map
#' @importFrom rlang .data
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if (!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- data %>% dplyr::filter(.data$STATE == state.num)
  if (nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  maps::map("state", regions = datasets::state.name[state.num])
  data.sub %>%
    graphics::points(.data$LONGITUD, .data$LATITUDE, pch = 46)
}
