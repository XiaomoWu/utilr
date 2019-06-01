#' A simplified save function.
#'
#' @export
#' @param x The object to be saved
#' @param compress Whether the object should be compressed
#' @param folder. To which subfolder the data is saved. If '.', save to current directory. 
sv <- function(x, path = "../Rdata", compress = T) {
    start <- Sys.time()

    # if folder not exists, create one
    if (!file.exists(path) & (!path %in% c('./', ''))) {
        dir.create(path)
        message(paste0("Folder ", "'", path, "' successfully created"))
    }

    # create save directory
    svdir <- paste0(path, '/', substitute(x), ".Rdata")

    if (file.exists(svdir)) {
        message("'", substitute(x), "' will be OVERWRITTEN!")
    }

    # Setting compression_level from default 6 to 3 results in an increase of size by 13% but a decrease of time by 64%!!!
    save(list = deparse(substitute(x)), file = svdir, compress = compress, compression_level = 3) 
    cat("'", substitute(x), "' successfully saved", "\n", sep = '')
    end <- Sys.time()
    gap <- end - start
    cat("Use", round(gap, 2), units(gap), "\n")
}


#' A simplified load function.
#'
#' @export
#' @param x The object to be loaded.
#' @param force Whether the object should be reloaded if it's already in the current environment.
#' @param folder To which subfolder the data is saved. If '.', save to current directory. 
ld <- function(x, path = '../Rdata', force = F) {
    start <- Sys.time()

    # create load dir
    lddir <- paste0(path, '/', substitute(x), ".Rdata")

    # check if the file exists
    if (!file.exists(lddir)) {
        stop("Object not exists!")
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
    cat("Use", round(gap, 2), units(gap), "\n")
}