#' Links installed packages to your new, empty, packrat directory
#'
#' @return NULL
#' @export symlink_packages
#' @import packrat
#' @import utils
#'
#' @examples symlink_packages()
symlink_packages <- function(){

  #Fail if it isn't in a packrat project
  if(packrat:::isPackratModeOn()==FALSE){
    stop('Packrat is not turned on, this function must be run in a packrat project')
  }


  #Grab the lists from a fresh user R session, so it'll work within a packrat
  original_dir <-
    "R  --slave --no-init-file -e \"cat(.libPaths())\""
  list_of_libs<-
    system(original_dir, intern=TRUE)
  list_of_libs<-
    unlist(strsplit(list_of_libs, split = ' '))

  #Work out the current packrat lib
  #Code from packrat package:
  getPackratLibDir <- function(projDir = NULL) {
    path <- file.path("packrat", "lib", R.version$platform, getRversion())

    if (!is.null(projDir)) {

      ## Strip trailing slashes if necessary
      projDir <- sub("/+$", "", projDir)

      ## Only prepend path if different from current working dir
      if (!identical(normalizePath(projDir), normalizePath(getwd())))
        path <- file.path(projDir, path)
    }

    path
  }

  target_lib<-
    getPackratLibDir(projDir = .rs.getProjectDirectory())

  user_packages<-
    utils::installed.packages(list_of_libs)[is.na(utils::installed.packages(list_of_libs)[,"Priority"]),]

  #Dont include packrat, or it will fail!
  user_packages<-
    user_packages[!user_packages[,'Package']=='packrat',]

  for (pkg in 1:nrow(user_packages)) {
    r_path <-user_packages[pkg,'LibPath']
    r_name <- user_packages[pkg,'Package']

    source <- file.path(r_path, r_name)
    target <- file.path(target_lib, r_name)

    if (!packrat:::ensurePackageSymlink(source, target))
      return(FALSE)

    message(c('Symlinking ',r_name))
    message(c('    from ', r_path, ' to ', target_lib))
    message('---------------------')

  }
}
