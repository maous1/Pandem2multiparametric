#' disaggregate hospitalisation files
#'
#' @param hospitalisation
#' @param start
#' @param end
#' @param case_aggregated
#'
#' @return
#' @export
#' @examples
format_hospitalisation <- function(hospitalisation, case_aggregated, start, end) {
  date <- format_date_pandem(start = start, end = end)

  Population <- case_aggregated %>%
    select(country,country_code, age_group,population) %>%
    distinct()%>%
    group_by(country,country_code)%>%
    summarise(population = sum(population))

  hospitalisation_detected <- hospitalisation %>%
    filter(indicator == "Weekly new hospital admissions per 100k") %>%
    left_join(Population, by = "country") %>%
    group_by(country_code, year_week) %>%
    summarise(new_cases = (value * population / 100000))


  hospitalisation_detected$year_week <- sub(x = hospitalisation_detected$year_week, pattern = "W", replacement = "")
  hospitalisation_detected <- right_join(x = hospitalisation_detected, y = date, "year_week")
  return(hospitalisation_detected)
}
