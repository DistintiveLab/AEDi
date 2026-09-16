#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  .init_raispsql()
  invisible()
}

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom tibble tibble
#' @importFrom lifecycle deprecate_soft
## usethis namespace: end
NULL
