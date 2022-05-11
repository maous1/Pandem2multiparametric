
#' Title
#'
#' @param start
#' @param end
#'
#' @return
#' @export
#'
#' @examples
format_date_pandem <- function(start, end) {
  start <- as.Date(start)
  end <- as.Date(end)
  time <- start:end
  class(time) <- "Date"
  result <- data.frame(
    time,
    cut_Date = cut(as.Date(time), "week"),
    stringsAsFactors = FALSE
  )


  result <- result %>%
    mutate(temp = strptime(cut_Date, format = "%Y-%m-%d")) %>%
    mutate(temp2 = format(temp, format = "%Y")) %>%
    mutate(temp3 = format(temp, format = "%U")) %>%
    mutate(year_week = paste(temp2, temp3, sep = "-")) %>%
    mutate(time = cut_Date) %>%
    select(time, year_week) %>%
    distinct()

  return(result)
}
