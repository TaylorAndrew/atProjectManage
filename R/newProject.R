#' newProject
#'
#' newProject creates a new project directory folder with 'Data', 'Documents', 'Output', 'Reports', and 'Syntax' subfolders.
#'
#' @param Dir Path pointing to where the new project directory should be added
#' @param FolderName Name of the new directory folder
#' @param addReportSkeleton If TRUE, an RMarkdown skeleton is added to /Reports. Note: This functionality will only work for Andrew Taylor, as the report skeleton is in my directory. If people want to use this, the report skeletons can be moved to the common M:/bioteam/ directory.
#' @param addPDAReportSkeleton If TRUE< an RMarkdown skeleton is added to /Documents, for use for planned data analyses.
#' @param recursive If TRUE, if the final folder in `Dir` does not yet exist, it will be created
#'
#' @return NULL
#' @export
#'
#' @examples
#' #newProject("C:/User/Me/", "NewProject")
newProject <- function(Dir, FolderName, addReportSkeleton=F, addPDAReportSkeleton = F, recursive = FALSE) {
  if (substr(Dir, nchar(Dir), nchar(Dir)) != "/")
    Dir <- paste0(Dir, "/")
  if (dir.exists(paste0(Dir, FolderName)))
    return(paste0(Dir, FolderName, " already exists. Project folder was not made."))
  dir.create(paste0(Dir, FolderName), recursive = recursive)
  dir.create(paste0(Dir, FolderName, "/Data"))
  dir.create(paste0(Dir, FolderName, "/Syntax"))
  dir.create(paste0(Dir, FolderName, "/Output"))
  dir.create(paste0(Dir, FolderName, "/Reports"))
  dir.create(paste0(Dir, FolderName, "/Documents"))
  if(addReportSkeleton==T) {
    file.copy("./Docs/RMarkdownSkeleton.Rmd", paste0(Dir,
                                                        FolderName,
                                                        "/Reports/",
                                                        FolderName,
                                                        "_syntax.Rmd"))
  }
  if(addPDAReportSkeleton==T) {
    file.copy("./Docs/RMarkdown_PDA_Skeleton.Rmd", paste0(Dir,
                                                        FolderName,
                                                        "/Documents/",
                                                        FolderName,
                                                        "_PDA.Rmd"))
  }
}
