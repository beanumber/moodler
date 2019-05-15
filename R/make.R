globalVariables(c("my_group", "ext", "value", "folder_name", "group"))

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
  fs::file_copy(system.file("makefile", package = "moodler"), path,
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

#' @rdname install_makefile
#' @export
#'
winnow_group_submissions <- function(path) {
  dirs <- fs::dir_ls(path) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::mutate(
      folder_name = fs::path_file(value),
      group = stringr::str_extract(folder_name, "Group_[A-Z]+")
    ) %>%
    group_by(group) %>%
    mutate(rank = min_rank(folder_name)) %>%
    filter(!is.na(group))

  to_delete <- dirs %>%
    filter(rank != 1) %>%
    pull(value)

  fs::file_delete(to_delete)
}
