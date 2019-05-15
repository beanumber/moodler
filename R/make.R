globalVariables(c("my_group", "ext"))

#' Install makefiles
#' @inheritParams fs::is_dir
#' @export
#' @examples
#' assignment_dir <- "~/Dropbox/git/sds192/student_info/s19/SDS192- 01_201903-Mini-project #1-394031"
#' if (fs::dir_exists(assignment_dir)) {
#'   install_makefile(assignment_dir)
#'   summarize_assignment(assignment_dir)
#' }

install_makefile <- function(path) {
  if (!fs::is_dir(path)) {
    warning(paste(path, "is not a directory. Using parent directory"))
  }
  path <- fs::path(path, "makefile")
  fs::file_copy(system.file("make", "makefile", package = "sds"), path,
                overwrite = TRUE)
}

#' @rdname install_makefile
#' @export

summarize_assignment <- function(path) {
  if (!fs::is_dir(path)) {
    warning(paste(path, "is not a directory. Using parent directory"))
  }
  files <- fs::dir_info(path, recurse = TRUE)
  files %>%
    mutate(my_group = stringr::str_extract(path, "Group_[A-Z,a-z,_,\\-]+"),
           ext = tools::file_ext(path)) %>%
    group_by(my_group) %>%
    summarize(
      num_files = n(),
      zip = sum(ext == "zip"),
      rproj = sum(ext == "Rproj"),
      rmd = sum(ext == "Rmd"),
      html = sum(ext == "html")
    ) %>%
    filter(!is.na(my_group))
}
