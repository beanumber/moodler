#' Populate a calendar listing
#' @export
#' @param begin first day of class
#' @param end last day of class
#' @param days_of_week on which days does the class meet?
#' @param holidays vector of exceptions
#' @examples
#' dates()
#' dates(days_of_week = c(2, 4, 6),
#'       holidays = c("2019-10-14", "2019-10-15"))

dates <- function(begin = "2019-09-05", end = "2019-12-12",
                  days_of_week = c(3, 5),
                  holidays = NULL) {
  x <- tibble::tibble(
    date = seq(lubridate::ymd(begin), lubridate::ymd(end), by = 1),
    dow = lubridate::wday(date)
  ) %>%
    filter(dow %in% days_of_week) %>%
    filter(!date %in% lubridate::ymd(holidays))
  return(x$date)
}
