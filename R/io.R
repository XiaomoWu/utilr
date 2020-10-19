#' A simplified save function using saveRDS().
#'
#' @export
#' @param obj expression or char. The object to be saved, use its R object name, e.g., `sv(dt)` or `sv('data')`.
#' @param filename expression or char. The file name of the serialized object. Default to objname, e.g., sv(dt, dt_new_name)
#' @param compress Whether the object should be compressed (only when saved as `rds`)
#' @param path To which subfolder the data is saved. Default to '../data'. If './', save to current directory. 
#' @param svtype The extension of saved file. Currently only `rds` and `feather` are available. Default to `rds`. 
#' @examples
#' sv(dt) # equals to sv('dt')
#' sv(dt, dt_new_name, path='../Rdata')
sv <- function(obj, svname=NULL, svtype='rds', path = "./data", compress = T) {
    
    start <- Sys.time()

    # if folder not exists, create one
    if (!file.exists(path) & (!path %in% c('./', ''))) {
        dir.create(path)
        message(paste0("Folder ", "-", path, "- created"))
    }

    # create save directory
    if (is.null(substitute(svname))) {
        svname = as.character(substitute(obj))
    }
    svdir <- paste0(path, '/', substitute(svname), '.', svtype)

    # serialize with `saveRDS` or `write_feather`
    if (svtype=='rds') {
        saveRDS(obj, file = svdir, compress = compress) 
    } else if (svtype=='feather') {
        arrow::write_feather(obj, svdir)
    }
    cat(str_c("-", substitute(svname), "- saved  "))
    end <- Sys.time()
    gap <- end - start
    cat(str_c("(", round(gap, 2), ' ', units(gap), ")\n"))
}


#' A simplified load function using readRDS().
#'
#' @export
#' @param filename expression or char. The file name to be loaded, e.g. ld(x) equals to load(file='dt.rds')
#' @param obj expression or char. The object name in R. If NULL then equals to filename
#' @param force Whether the object should be reloaded if it's already in the current environment.
#' @param path From which subfolder to read the data. Default to '../data'. If './', load from current directory.
#' @param ldtype Either 'rds' or 'feather'. Default `rds`
#' @examples
#' ld(dt)
#' ld(filename=dt, obj=dt_new, path='../Rdata')
ld <- function(filename, obj=NULL, ldtype='rds', path = './data', force = F) {
    start <- Sys.time()

    # create load dir
    lddir <- paste0(path, '/', substitute(filename), '.', ldtype)

    # check if the file exists
    if (!file.exists(lddir)) {
        stop("Object not exists!", lddir)
    }


    # load data
    if (force == F) {
        if (is.null(substitute(obj))) { # if obj is null
            # if filename not exist in global, assign to .GlobalEnv
            if (!exists(as.character(substitute(filename)))) {
                # load with RDS or feather
                if (ldtype=='feather') {
                    val = feather::read_feather(lddir) %>% setDT()
                }               
                else if (ldtype=='rds') {
                    val = readRDS(lddir)
                } 

                assign(as.character(substitute(filename)), val, env=.GlobalEnv)
                cat(str_c("-", substitute(filename), "- loaded"))
            } else {
                # if filename exist, Not load
                cat(str_c("-", substitute(filename), "- already exists, will NOT load again!"))
            }
        } else { # if obj isn't null
            # if obj not exist in global, load
            if (!exists(as.character(substitute(obj)))) {
                # load with RDS or feather
                if (ldtype=='feather') {
                    val = arrow::read_feather(lddir) %>% setDT()
                }               
                else if (ldtype=='rds') {
                    val = readRDS(lddir)
                }            

                assign(as.character(substitute(obj)), val, env=.GlobalEnv)
                cat(str_c("-", substitute(filename), "- loaded as -", substitute(obj), "-"))
            } else {
                # if obj exist in global, Not load
                cat(str_c("-", substitute(obj), "- already exists, will NOT load again!"))
            }
        }
    } else if (force == T) {
        # assign to .GlobalEnv
        if (is.null(substitute(obj))) {
            # load with RDS or feather
            if (ldtype=='feather') {
                val = read_feather(lddir) %>% setDT()
            }               
            else if (ldtype=='rds') {
                val = readRDS(lddir)
            } 

            assign(as.character(substitute(filename)), val, env=.GlobalEnv)
            cat(str_c("-", substitute(filename), "- loaded"))
        } else {
            assign(as.character(substitute(obj)), val, env=.GlobalEnv)
            cat(str_c("-", substitute(filename), "- loaded as -", substitute(obj), "-"))
        }
    }
        
    # output time elapsed
    end <- Sys.time()
    gap <- end - start
    cat(str_c("  (", round(gap, 2), ' ', units(gap), ")\n"))
}
