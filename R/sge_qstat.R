#' Return output from qstat command as data.frame
#'
#' @param cmd qstat command, default `qstat`
#' @param flags add flags
#' @param filter filter list of column names and filter values
#' @param exact filter using exact match or partial match? `TRUE` by default
#' @export
sge_qstat <- function(
  cmd = "qstat",
  flags = "",
  filter = NULL,
  exact = TRUE) {
  cols <- c("job-ID", "prior", "name", "user", "state", "submit/start at", "queue", "slots", "ja-task-ID")

  ## RK: parsing of output and handling of colnames
  ##     is very crappy, fix!
  out <- system(paste(cmd, flags), intern=TRUE)
  idx <- data.frame(stringr::str_locate(out[1], cols))
  idx$end2 <- max(idx[,2])
  idx[1:(length(idx[,1])-1),]$end2 <- idx[2:(length(idx[,1])),1]-1
  data <- out[-c(1,2)]
  all <- c()
  for(i in seq(data)) {
    tmp <- stringr::str_trim(
      stringr::str_sub(data[i], idx$start, idx$end2)
    )
    all <- data.table::rbindlist(list(all, data.frame(rbind(tmp))))
  }
  colnames(all) <- cols
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
