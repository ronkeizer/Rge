#' Submit job to SGE
#'
#' @param cmd command to execute
#' @param script_file script file called by submit command
#' @param submit_cmd command used to submit SGE job (default `qsub`)
#'
#' @export
sge_qsub <- function(cmd, script_file = "job.sh", submit_cmd = "qsub") {
    script <- c("#!/bin/bash", "#", "#$ -cwd", "#$ -j y", "#$ -S /bin/bash","#",
                cmd)
    fileConn <- file(script_file)
    writeLines(script, fileConn)
    close(fileConn)
    out <- system(paste(submit_cmd = submit_cmd, script_file), intern=TRUE)
    jobId <- NULL
    if(stringr::str_detect(out[1], "Your job")) {
      jobId <- stringr::str_split(
        stringr::str_split(out[1], "Your job ")[[1]][2],
        " \\("
      )[[1]][1]
    }
    return(jobId)
}
