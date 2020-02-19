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


#' Report on compilation in a directory
#' @param path path to assignment directory
#' @param ... currently ignored
#' @export
#' @examples
#' dir <- "~/Dropbox/sds192-s20/SDS192- 01_202003-Homework 2 ggplot2-530786"
#' render_progress(dir)

render_progress <- function(path = ".", ...) {
  html <- fs::dir_info(path, recurse = TRUE, regexp = "*.html") %>%
    mutate(parent_dir = fs::path_dir(path)) %>%
    select(path_html = path, parent_dir)
  rmd <- fs::dir_info(path, recurse = TRUE, regexp = "*.(R|r)md") %>%
    mutate(parent_dir = fs::path_dir(path)) %>%
    select(path_rmd = path, parent_dir)

  x <- full_join(html, rmd, by = "parent_dir") %>%
    mutate(
      complete = !is.na(path_html) & !is.na(path_rmd),
      not_compile = !is.na(path_rmd) & is.na(path_html),
      not_markdown = is.na(path_rmd) & !is.na(path_html)
    )
  message(paste(nrow(filter(x, complete)), "completed."))
  x %>%
    filter(!complete) %>%
    mutate(student = fs::path_file(parent_dir)) %>%
    select(student, not_compile, not_markdown)
}
