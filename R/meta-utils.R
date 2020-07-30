#' Get when an OSF node was last updated
#'
#' @param osf_tbl An `osf_tbl` metadata object that is associated with an object
#'                in the OSF cache.
#' @return The date when last modified
#' @export
osf_last_updated <- function(osf_tbl) {
  osf_tbl$meta[[1]]$attributes$date_modified
}

#' Get the GUID of an OSF node
#'
#' @param osf_tbl An `osf_tbl` metadata object that is associated with an object
#'                in the OSF cache.
#' @return The object's GUID
#' @export
osf_guid <- function(osf_tbl) {
  osf_tbl$meta[[1]]$attributes$guid
}

#' Get the location of a cached OSF download
#'
#' @param osf_tbl An `osf_tbl` metadata object that is associated with an object
#'                in the OSF cache.
#' @return The object's local path
#' @export
osf_path <- function(osf_tbl) {
  osf_tbl$local_path
}
