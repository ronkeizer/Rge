#' Return output from qstat command as data.frame
#'
#' @param cmd qstat command, default `qstat`
#' @param flags add flags
#' @param user specify user
#' @param queue specify queue
#' @param all show all jobs, also finished / zombie? Default `TRUE`
#' @param filter filter (afterwards) list of column names and filter values
#' @param exact filter using exact match or partial match? `TRUE` by default
#' @export
qstat <- function(
  cmd = "qstat",
  flags = NULL,
  user = NULL,
  queue = NULL,
  all = FALSE,
  pending = TRUE,
  filter = NULL,
  exact = TRUE,
  ext = FALSE,
  raw = FALSE,
  verbose = FALSE) {
  flags <- c(flags, "-xml")
  if(!is.null(user)) {
    flags <- c(flags, paste("-u", user))
  }
  if(!is.null(queue)) {
    flags <- c(flags, paste("-q", queue))
  }
  if(pending && !all) {
    flags <- c(flags, paste("-s pr"))
  }
  if(all) {
    flags <- c(flags, paste("-s prsz"))
  }
  if(ext) {
    flags <- c(flags, "-ext")
  }
  cmd_full <- paste(cmd, flags)
  if(verbose) {
    cat(paste0(cmd_full, "\n"))
  }
  out <- paste(system2(cmd, args = flags, stdout=TRUE), collapse = "")
  if(raw) {
    return(out)
  }
  if(verbose) {
    cat(out)
  }
  data <- xml2::as_list(xml2::read_xml(out), )
  all <- c()
  if(!is.null(data$queue_info)) { # running jobs are stored in queue_info
    data$job_info <- c(data$queue_info, data$job_info)
  }
  for(i in seq(data$job_info)) {
    tmp <- data$job_info[i]$job_list
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
