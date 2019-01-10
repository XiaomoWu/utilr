#' A simplified save function.
#'
#' @export
#' @param x The object to be saved
#' @param compress Whether the object should be compressed
#' @param current.dir If True, save to current folder; otherwise, create subdirectory 'Rdata'
sv <- function(x, compress = T, current.dir = F) {
    start <- Sys.time()

    # create save directory
    # if folder 'Rdata' not exists, create one
    if (current.dir == T) {
        svdir <- paste0(substitute(x), ".Rdata")
    } else {
        if (!file.exists("Rdata")) {
            dir.create("Rdata")
            message("Folder 'Rdata' successfully created")
        }
        svdir <- paste0("Rdata/", substitute(x), ".Rdata")
    }
    if (file.exists(svdir)) {
        message("'", substitute(x), "' will be OVERWRITTEN!")
    }
    # Setting compression_level from default 6 to 3 results in an increase of size by 13% but a decrease of time by 64%!!!
    save(list = deparse(substitute(x)), file = svdir, compress = compress, compression_level = 3) 
    cat("'", substitute(x), "' successfully saved", "\n", sep = '')
    end <- Sys.time()
    gap <- end - start
    cat("Use", round(gap, 2), units(gap), "\n\n")
}


#' A simplified load function.
#'
#' @export
#' @param x The object to be loaded.
#' @param force Whether the object should be reloaded if it's already in the current environment.
#' @param current.dir If True, load from current folder; otherwise, load from subdirectory 'Rdata'
ld <- function(x, force = F, current.dir = F) {
    start <- Sys.time()

    # create load dir
    if (current.dir == T) {
        lddir <- paste0(substitute(x), ".Rdata")
    } else {
        lddir <- paste0("Rdata/", substitute(x), ".Rdata")
    }

    # load data
    if (force == F) {
        if (!exists(as.character(substitute(x)))) {
            load(lddir, envir = .GlobalEnv)
            cat("'", substitute(x), "' successfully loaded", "\n")
        } else {
            message("'", substitute(x), "' already exists, will NOT load again!")
        }
    } else if (force == T) {
        if (exists(as.character(substitute(x)))) {
            message("'", substitute(x), "' will be loaded AGAIN!")
        }
        load(lddir, envir = .GlobalEnv)
        cat("'", substitute(x), "' successfully loaded", "\n", sep = '')
    }

    # output time elapsed
    end <- Sys.time()
    gap <- end - start
    cat("Use", round(gap, 2), units(gap), "\n\n")
}