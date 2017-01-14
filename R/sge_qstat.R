#' Return output from qstat command as data.frame
#'
#' @param cmd qstat command, default `qstat`
#' @param flags add flags
#' @param user show only for specific user
#' @param filter filter list of column names and filter values
#' @param exact filter using exact match or partial match? `TRUE` by default
#' @export
sge_qstat <- function(
  cmd = "qstat",
  flags = "",
  user = NULL,
  filter = NULL,
  exact = TRUE) {
  flags <- paste(flags, " -xml")
  if(!is.null(user)) {
    flags <- paste(flags, "-u", user)
  }
  out <- paste(system(paste(cmd, flags), intern=TRUE), collapse = "")
  data_all <- xml2::as_list(xml2::read_xml(out), )
  all <- c()
  for(i in seq(data_all$job_info)) {
    tmp <- data_all$job_info[i]$job_list
    if(length(tmp$queue_name) == 0) { tmp$queue_name <- "" }
    all <- rbind(all, unlist(tmp))
  }
  all <- data.frame(all)
  all$JAT_prio <- as.numeric(all$JAT_prio)
  all$slots <- as.numeric(all$slots)
  all$JB_submission_time <- as.POSIXct(all$JB_submission_time)
  cols <- c("job-ID", "prior", "name", "user", "state", "t_submit_start", "queue", "slots", "ja-task-ID")
  colnames(all) <- cols[1:length(all[1,])]
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
