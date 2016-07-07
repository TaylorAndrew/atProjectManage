#' commentProjectInfo
#'
#' commentProjectInfo prints a comment to the console that includes relevant project information.
#'
#' @param ProjectName Name of the current project
#' @param Researcher Name of the researcher/client
#' @param Statistician Name of the statistician/programmer
#' @param StartDate Start date
#' @param UpdateDate Updated date
#'
#' @return A comment printed to the console
#' @export
#'
#' @examples
#' #commentProjectInfo(ProjectName="This is the project name",
#' #                  Researcher = 'John Doe',
#' #                  Statistician = 'Andrew Taylor',
#' #                  StartDate = 'December 9, 2015',
#' #                 UpdateDate = '')
commentProjectInfo <- function(ProjectName='',
                        Researcher = '',
                        Statistician = '',
                        StartDate = '',
                        UpdateDate = '') {
  getStrings <- function(string, max){
    e <- new.env()
    e$P <- list()
    get <- function(string, i=1) {
      e$P[i] <- gsub("^\\s+|\\s+$", "", substr(string, 1, max))
      leftOver <- substr(string, max+1, 10000)
      if(nchar(leftOver)>0) {
        i = i + 1
        get(string=leftOver, i = i)
      }
    }
  get(string=string)
  return(e$P)
  }
  ProjectName <- getStrings(ProjectName, max=64)
  Researcher <- getStrings(Researcher, max=61)
  Statistician <- getStrings(Statistician, max=64)
  StartDate <- getStrings(StartDate, max=66)
  UpdateDate <- getStrings(UpdateDate, max=59)
  cat(c(rep("#", 80), "\n"), sep = '')
  lapply(
  ProjectName,
  invisible(function(x) {
  cat(paste0(c("#Project Name: ",
             x,
             paste0(c(rep(" ", 80-(16+nchar(x)))), collapse = ''),
             "#\n"), collapse = ''))
  }))
  lapply(
  Researcher,
  invisible(function(x) {
  cat(paste0(c("#Researcher Name: ",
             x,
             paste0(c(rep(" ", 80-(19+nchar(x)))), collapse = ''),
             "#\n"), collapse = ''))
  }))
  lapply(
  Statistician,
  invisible(function(x) {
  cat(paste0(c("#Statistician: ",
             x,
             paste0(c(rep(" ", 80-(16+nchar(x)))), collapse = ''),
             "#\n"), collapse = ''))
  }))
  lapply(
  StartDate,
  invisible(function(x) {
  cat(paste0(c("#Start Date: ",
             x,
             paste0(c(rep(" ", 80-(14+nchar(x)))), collapse = ''),
             "#\n"), collapse = ''))
  }))
  lapply(
  UpdateDate,
  invisible(function(x) {
  cat(paste0(c("#Updated Code Date: ",
             x,
             paste0(c(rep(" ", 80-(21+nchar(x)))), collapse = ''),
             "#\n"), collapse = ''))
  }))
  cat(c(rep("#", 80), "\n"), sep = '')
}
