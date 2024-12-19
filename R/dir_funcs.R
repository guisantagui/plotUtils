#' Create a directory if it doesn't exist
#'
#' Creates a directory after making sure it doesn't already exist.
#'
#' @param d A path to the location of a directory
#' @param recursive Logical. If set to true all the recursive locations will
#' be created as well.
#'
#' @export
create_dir_if_not <- function(d, recursive = T){
        if (!dir.exists(d)){
                dir.create(d, recursive = recursive)
        }
}

#' Add slash to directory name if not already there
#'
#' Adds a slash to a directory location if it doesn't already finish by a slash.
#'
#' @param d A path to a directory
#'
#' @return The directory location string, finished by a `"/"`.
#'
#' @export
add_slash_if_not <- function(d){
        if (substr(d, nchar(d), nchar(d)) != "/"){
                d <- paste0(d, "/")
        }
        return(d)
}
