# reworked is_installed2 from https://github.com/dataheld/elf/blob/main/R/dependencies.R
#' Checks if a package is installed and *informs* the user if not
#'
#' This is wrapper around [rlang::check_installed];
#' instead of erroring out if the check fails it returns `FALSE`.
#' However, unlike [rlang::is_installed], it emits a message to the user.
#'
#' @inheritParams rlang::check_installed
#' @inheritDotParams rlang::check_installed
#' @keywords internal
cartogram_assert_package <- function(...) {
  if (rlang::is_installed(...)) {
    return(TRUE)
  }
  
  withRestarts(
    tryCatch(
      rlang::check_installed(...),
      error = function(cnd) {
        if (inherits(cnd, "rlib_error_package_not_found")) {
          message("The required package is not installed.")
          stop(cnd)  # Re-throw the error
        }
      }
    ),
    abort = function(cnd) {
      message("The required package is not installed.")
      stop(cnd)  # Re-throw the error
    }
  )
  
  rlang::is_installed(...)
}
