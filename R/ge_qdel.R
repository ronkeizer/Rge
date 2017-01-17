#' Kill jobs
#'
#' @param id job id to kill
#' @param force force kill (`FALSE` by default)
#' @param flags possible vector of other flags passed to qdel command
#' @param cmd command to use (default `qdel`)
#' @param verbose `T`/`F`
#' @export
qdel <- function(
  id = NULL,
  force = FALSE,
  flags = c(),
  cmd = "qdel",
  verbose = TRUE
  ) {
    if(is.null(id)) {
      stop("`id` required.")
    }
    flags <- c()
    if(force) {
      flags <- c(flags, "-f")
    }
    flags <- c(flags, id)
    suppressWarnings({
      out <- paste(system2(cmd, args = flags, stdout=TRUE), collapse = "")
    })
    if(verbose) {
      return(out)
    }
}
