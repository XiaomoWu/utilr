#' A simplified save function using saveRDS().
#'
#' @export
#' @param dt The data.table whose variable names to be changed

names_for_py <- function(dt) {.
    nm = names(dt)
    nm_py = str_replace(nm, '\\.', '_')
    setnames(dt, nm, nm_py)
}