globalVariables(c("Groups", "grade", "criterion", "."))

#' Generate feedback report for Moodle
#' @param x key to Googlesheet
#' @param ... arguments passed to \code{\link[googlesheets]{gs_key}}
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' key <- "1FA7XrFQ1h6Lss-c8__RvQceG0-vIO2vXR1hPcdD49-o"
#' feedback(key, visibility = "private")
#' }

feedback <- function(x, ...) {
  grades <- googlesheets::gs_key(x, ...) %>%
    googlesheets::gs_read()

  grades %>%
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
    paste("Discretionary Points:", x$`0 to 2 reflects the professor’s judgment of the overall quality of your submission`),
    out)
}
