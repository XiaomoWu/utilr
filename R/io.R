#' A simplified save function using saveRDS().
#'
#' @export
#' @param objname expression or char. The object to be saved, use its R object name, e.g., sv(dt).
#' @param filename expression or char. The file name of the serialized object. Default to objname, e.g., sv(dt, dt_new_name)
#' @param compress Whether the object should be compressed
#' @param path To which subfolder the data is saved. Default to '../data'. If './', save to current directory. 
#'
#' @examples
#' sv(dt) # equals to sv('dt')
#' sv(dt, dt_new_name, path='../Rdata')
sv <- function(objname, filename=NULL, path = "./data", compress = T) {
    
    start <- Sys.time()

    # if folder not exists, create one
    if (!file.exists(path) & (!path %in% c('./', ''))) {
        dir.create(path)
        message(paste0("Folder ", "-", path, "- created"))
    }

    # create save directory
    if (is.null(substitute(filename))) {
        filename = as.character(substitute(objname))
    }
    svdir <- paste0(path, '/', substitute(filename), ".rds")

    # serialize with `saveRDS`
    saveRDS(objname, file = svdir, compress = compress) 
    cat(str_c("-", substitute(filename), "- saved  "))
    end <- Sys.time()
    gap <- end - start
    cat(str_c("(", round(gap, 2), ' ', units(gap), ")\n"))
}


#' A simplified load function using readRDS().
#'
#' @export
#' @param filename expression or char. The file name to be loaded, e.g. ld(x) equals to load(file='dt.rds')
#' @param objname expression or char. The object name in R. If NULL then equals to filename
#' @param force Whether the object should be reloaded if it's already in the current environment.
#' @param path From which subfolder to read the data. Default to '../data'. If './', load from current directory.
#'
#' @examples
#' ld(dt)
#' ld(filename=dt, objname=dt_new, path='../Rdata')
ld <- function(filename, objname=NULL, path = './data', force = F) {
    start <- Sys.time()

    # create load dir
    lddir <- paste0(path, '/', substitute(filename), ".rds")

    # check if the file exists
    if (!file.exists(lddir)) {
        stop("Object not exists!")
    }

    # load data
    if (force == F) {
        if (is.null(substitute(objname))) { # if objname is null
            # if filename not exist in global, load
            if (!exists(as.character(substitute(filename)))) {
                val = readRDS(lddir)
                assign(as.character(substitute(filename)), val, env=.GlobalEnv)
                cat(str_c("-", substitute(filename), "- loaded"))
            } else {
                # if filename exist, Not load
                cat(str_c("-", substitute(filename), "- already exists, will NOT load again!"))
            }
        } else { # if objname isn't null
            # if objname not exist in global, load
            if (!exists(as.character(substitute(objname)))) {
                val = readRDS(lddir)
                assign(as.character(substitute(objname)), val, env=.GlobalEnv)
                cat(str_c("-", substitute(filename), "- loaded as -", substitute(objname), "-"))
            } else {
                # if objname exist in global, Not load
                cat(str_c("-", substitute(objname), "- already exists, will NOT load again!"))
            }
        }
    } else if (force == T) {
        val = readRDS(lddir)
        if (is.null(substitute(objname))) {
            assign(as.character(substitute(filename)), val, env=.GlobalEnv)
            cat(str_c("-", substitute(filename), "- loaded"))
        } else {
            assign(as.character(substitute(objname)), val, env=.GlobalEnv)
            cat(str_c("-", substitute(filename), "- loaded as -", substitute(objname), "-"))
        }
    }
        
    # output time elapsed
    end <- Sys.time()
    gap <- end - start
    cat(str_c("  (", round(gap, 2), ' ', units(gap), ")\n"))
}
