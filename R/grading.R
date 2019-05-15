globalVariables(c("Groups", "grade", "criterion", "."))

#' Generate feedback report for Moodle
#' @param x a data.frame
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' key <- "1S0vfV_xkebyTkC2bmhOv-MDNMLulb95GFwfvI9ZPtGw"
#' if (gogglesheets) {
#'   grades <- key %>%
#'     gs_key() %>%
#'     gs_read()
#' }
#' feedback(grades)
#' }

feedback <- function(x) {
  x %>%
    filter(!is.na(Groups)) %>%
    split(.$Groups) %>%
    purrr::map(display)
}


display <- function(x) {
  out <- x %>%
    tidyr::gather(key = "criterion", value = "grade", -Groups) %>%
    filter(grade == 0, !grepl("judgment", criterion)) %>%
    mutate(criterion = paste0("-", criterion)) %>%
    pull(criterion)

  c(paste("Total:", x$Total),
    paste("Comments:", x$Comments),
    paste("Discretionary Points:", x$discretionary),
    out)
}
