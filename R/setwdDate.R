#Creates a folder with today's date at the given directory and then
#sets the working directory to that folder
#' setwdDate
#'
#' setwdDate creates a folder with the current day's date (YYYYMMDD) in a given directory, and then sets the working directory to that folder.
#'
#' @param path A path to the directory in which the newly created folder will be appended to.
#'
#' @return NULL
#' @export
#'
#' @examples
#' #setwdDate("C:/MyFakePath/Output/")
#' ##Would create a folder:
#' #paste0("C:/MyFakePath/Output/", format(Sys.Date(), "%Y%m%d"))
#' ##And would set the working directory to that folder
setwdDate <- function(path) {
  wd <- ifelse(substr(path, nchar(path), nchar(path))!="/", paste0(path, "/"), path)
  if(!dir.exists(paste0(path, "/", format(Sys.Date(), "%Y%m%d")))){
     dir.create(paste0(path, "/", format(Sys.Date(), "%Y%m%d")))
  }
   setwd(paste0(path, "/", format(Sys.Date(), "%Y%m%d")))
}
