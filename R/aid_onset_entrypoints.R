# Resolve a destination through its nearest existing ancestor before legacy writes.
ao_resolve_destination <- function(path) {
  path <- path.expand(path)
  if(!grepl("^/",path)) path <- file.path(getwd(),path)
  if(file.exists(path) || dir.exists(path)) return(normalizePath(path,mustWork=TRUE))
  file.path(ao_resolve_destination(dirname(path)),basename(path))
}
