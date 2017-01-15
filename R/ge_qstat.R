#' Return output from qstat command as data.frame
#'
#' @param cmd qstat command, default `qstat`
#' @param flags add flags
#' @param user specify user
#' @param queue specify queue
#' @param filter filter (afterwards) list of column names and filter values
#' @param exact filter using exact match or partial match? `TRUE` by default
#' @export
ge_qstat <- function(
  cmd = "qstat",
  flags = "",
  user = NULL,
  queue = NULL,
  filter = NULL,
  exact = TRUE,
  ext = FALSE,
  raw = FALSE) {
  flags <- paste(flags, " -xml")
  if(!is.null(user)) {
    flags <- paste(flags, "-u", user)
  }
  if(!is.null(queue)) {
    flags <- paste(flags, "-q", queue)
  }
  if(ext) {
    flags <- paste(flags, "-ext")
  }
  out <- paste(system(paste(cmd, flags), intern=TRUE), collapse = "")
  if(raw) {
    return(out)
  }
  data_all <- xml2::as_list(xml2::read_xml(out), )
  all <- c()
  for(i in seq(data_all$job_info)) {
    tmp <- data_all$job_info[i]$job_list
    for(j in tmp) {
      if(length(tmp) == 0) {
        tmp[[j]] <- ""
      }
    }
    all <- rbind(all, unlist(tmp))
  }
  all <- data.frame(all)
  all$JAT_prio <- as.numeric(all$JAT_prio)
  all$slots <- as.numeric(all$slots)
  if(!ext) {
    cols <- c("job-ID", "prior", "name", "user", "state", "t_submit_start", "queue", "slots", "ja-task-ID")
    colnames(all) <- cols[1:length(all[1,])]
  }
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
