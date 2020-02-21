globalVariables(c("Groups", "grade", "criterion", "."))

#' Generate feedback report for Moodle
#' @name feedback
#' @param x a data.frame
#' @param ... arguments passed to other functions
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' key <- "1S0vfV_xkebyTkC2bmhOv-MDNMLulb95GFwfvI9ZPtGw"
#' if (require(googlesheets4)) {
#'   # sheets_auth()
#'   grades <- key %>%
#'     read_sheet()
#' }
#' print_feedback(grades)
#' }

add_feedback <- function(x, ...) {
  feedback <- x %>%
    select(email_address, matches("\\("), ...) %>%
    tidyr::pivot_longer(-email_address, names_to = "item", values_to = "grade") %>%
    mutate(feedback_item = paste0(grade, ": ", item)) %>%
    group_by(email_address) %>%
    summarize(feedback = paste(feedback_item, collapse = "\n"))

  x %>%
    left_join(feedback, by = "email_address") %>%
    mutate(
      feedback = paste0("Comments: ", Comments, "\n", feedback),
      grade = ifelse(is.na(grade), 0, grade),
      feedback = ifelse(is.na(grade), "No submission", feedback)
    )
}


#' @rdname feedback
#' @export

read_moodle_grades <- function(...) {
  readr::read_csv(...) %>%
    janitor::clean_names() %>%
    mutate(
      moodle_name = paste(first_name, last_name, "", sep = "_"),
      moodle_name = tolower(str_remove_all(moodle_name, "['\\)\\(]")),
      moodle_name = str_replace_all(moodle_name, " ", "_")
    )
}

#' @rdname feedback
#' @export

read_moodle_sheet <- function(...) {
  googlesheets4::read_sheet(...) %>%
    mutate(
      moodle_name = tolower(str_extract(Student, "^[a-zA-Z_-]+")),
      moodle_name = str_replace_all(moodle_name, "__", "_")
    )
}

#' @rdname feedback
#' @param y other data.frame
#' @export

join_moodle <- function(x, y, ...) {
  x %>%
    full_join(y, by = "moodle_name") %>%
    arrange(...) %>%
    mutate(grade = Total)
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
