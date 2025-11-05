#' Document, build site, and install a package
#'
#' @description
#' Runs `devtools::document()`, then builds the pkg site, then installs the
#' package at `path`. Uses a temporary working directory switch so that
#' `devtools::build_site()` is called without unsupported arguments.
#' Falls back to `pkgdown::build_site()` when available.
#'
#' @param path Package root directory. Defaults to `'.'`.
#' @param do_site Logical; build the site (default `T`).
#' @param do_install Logical; install the package (default `T`).
#' @param quiet Logical; passed to underlying calls when supported (default `F`).
#' @param upgrade Passed to install step; default `'never'`.
#' @param stop_on_error Logical; if `T`, stop on first error. If `F`, continue
#' and attach errors to the returned list (default `F`).
#'
#' @return
#' Invisibly returns a list with elements `document`, `site`, and `install`,
#' each holding the underlying call result or an `error` object if that step failed.
#'
#' @examples
#' \dontrun{
#' pndr.doc_pckg('.')
#' pndr.doc_pckg(path = '.', do_site = F, quiet = T)
#' }
#'
#' @export
#' @importFrom devtools document install
pndr.doc_pckg <- function(path = '.', do_site = T, do_install = T,
                          quiet = F, upgrade = 'never', stop_on_error = F) {
  run_step <- function(expr) {
    tryCatch(force(expr), error = function(e) if (stop_on_error) stop(e) else e)
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = T)
  setwd(path)

  out <- list()

  out$document <- run_step(devtools::document(pkg = '.', quiet = quiet))

  if (do_site) {
    if ('devtools' %in% loadedNamespaces() && 'build_site' %in% getNamespaceExports('devtools')) {
      out$site <- run_step(devtools::build_site())
    } else if (requireNamespace('pkgdown', quietly = T)) {
      out$site <- run_step(pkgdown::build_site())
    } else {
      warn <- simpleWarning('No site builder found (devtools/pkgdown); skipping site.')
      out$site <- structure(warn, class = c('warning', class(warn)))
    }
  } else {
    out$site <- NULL
  }

  if (do_install) {
    out$install <- run_step(devtools::install(pkg = '.', quiet = quiet, upgrade = upgrade))
  } else {
    out$install <- NULL
  }

  invisible(out)
}
