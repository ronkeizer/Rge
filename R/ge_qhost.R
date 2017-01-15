#' Return output from qhost command as data.frame
#'
#' @param cmd qstat command, default `qhost`
#' @param flags add flags
#' @param user specify user
#' @param queue specify queue
#' @param filter filter (afterwards) list of column names and filter values
#' @param exact filter using exact match or partial match? `TRUE` by default
#' @export
qhost <- function(
  cmd = "qhost",
  flags = NULL,
  filter = NULL,
  exact = TRUE,
  ext = FALSE,
  raw = FALSE) {
  flags <- paste(flags, " -xml")
  if(ext) {
    flags <- paste(flags, "-ext")
  }
  out <- paste(system(paste(cmd, flags), intern=TRUE), collapse = "")
  if(raw) {
    return(out)
  }
  data_all <- xml2::as_list(xml2::read_xml(out))
  all <- c()
  for(i in seq(data_all)) {
    tmp <- data_all[i]$host
    n <- attr(tmp, "name")
    l <- list()
    for(k in seq(tmp)) {
      l[[attr(tmp[[k]], "name")]] <- tmp[[k]][[1]][1]
    }
    all <- rbind(all, cbind(name = n, rbind(l)))
  }
  all <- data.frame(all, row.names=NULL)
  if(!is.null(filter)) {
    for(i in 1:length(names(filter))) {
      if(exact) {
        filt <- all[[names(filter)[i]]] == filter[[names(filter)[i]]]
      } else {
        filt <- !is.na(stringr::str_match(all[[names(filter)[i]]], filter[[names(filter)[i]]]))
      }
      all <- all[filt, ]
    }
  }
  return(all)
}
