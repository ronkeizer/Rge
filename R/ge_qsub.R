#' Submit job to GridEngine
#'
#' @param cmd command to execute
#' @param script_file script file called by submit command
#' @param submit_cmd command used to submit SGE job (default `qsub`)
#' @param queue queue name
#' @param jobId job name. If blank assigned automatically by grid engine
#'
#' @export
ge_qsub <- function(
  cmd,
  script_file = "job.sh",
  submit_cmd = "qsub",
  queue = NULL,
  name = NULL,
  prefix = NULL
  ) {
    script <- c("#!/bin/bash", "#", "#$ -cwd", "#$ -j y", "#$ -S /bin/bash","#",
                cmd)
    fileConn <- file(script_file)
    writeLines(script, fileConn)
    close(fileConn)
    out <- system(paste(submit_cmd = submit_cmd, script_file), intern=TRUE)
    jobId <- NULL
    if(is.null(name)) {
      random_string <- function(len = 12) { return(paste(sample(c(rep(0:9,each=5),LETTERS,letters),len,replace=TRUE),collapse='')) }
      name <- random_string(8)
      job <- stringr::str_split(cmd, " ")[[1]][1]
      name <- paste0(job, "_", random_string(8))
    }
    if(!is.null(prefix)) {
      name <- paste0(prefix, "_", job)
    }
    if(stringr::str_detect(out[1], "Your job")) {
      jobId <- stringr::str_split(
        stringr::str_split(out[1], "Your job ")[[1]][2],
        " \\("
      )[[1]][1]
    }
    return(jobId)
}
