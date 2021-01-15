#' Return readble file size.
#'
#' @export
#' @param file_path Path of the file to be evaluated
#' @examples
#' neat_file_size(10276) # 10.03MB
neat_file_size <- function(file_path) {
    bytes = file.size(file_path)
    if (bytes >= 1024^3) {
        gbs = round(bytes/1024^3, 1)
        sprintf('%s GB', gbs)
    } else if (1024^2<bytes & bytes<=1024^3) {
        mbs = round(bytes/1024^2, 1)
        sprintf('%s MB', mbs)
    } else if (1024<bytes & bytes<=1024^2) {
        kbs = round(bytes/1024, 1)
        sprintf('%s KB', kbs)
    }
}

#' A simplified save function using saveRDS().
#'
#' @export
#' @param obj expression or char. The object to be saved, use its R object name, e.g., `sv(dt)` or `sv('data')`.
#' @param filename expression or char. The file name of the serialized object. Default to objname, e.g., sv(dt, dt_new_name)
#' @param compress Whether the object should be compressed (only when saved as `rds`)
#' @param path To which subfolder the data is saved. Default to './data'. If './', save to current directory. 
#' @param svtype The extension of saved file. Currently only `rds` and `feather` are available. Default to `rds`. 
#' @examples
#' sv(dt) # equals to sv('dt')
#' sv(dt, dt_new_name, path='../Rdata')
sv <- function(obj, svname=NULL, svtype=NULL, path = "./data") {
    
    start <- Sys.time()

    # if folder not exists, create one
    if (!file.exists(path)) {
        dir.create(path)
        message(sprintf('Directory "%s" created', path))
    }

    # Determin svname
    svname = ifelse(is.null(substitute(svname)), 
                    as.character(substitute(obj)),
                    as.character(substitute(svname)))

    # Determin save type: "rds" or "feather"
    # Default: 'data.table' -> 'feather'
    #          others       -> 'rds'
    auto_svtype = ifelse(any(c('data.table', 'data.frame') %in% class(obj)),
                         'feather',
                         'rds')

    svtype = ifelse(is.null(substitute(svtype)),
                    auto_svtype,
                    as.character(substitute(svtype)))

    if (auto_svtype != svtype) {
        warning(sprintf('We detect the data is in "%s" format, but you are trying to save as "%s". Be careful!', class(obj), svtype),
                call.=F)
    }


    # Save with `write_feather` or `saveRDS`
    svdir = sprintf('%s/%s.%s', path, svname, svtype)

    if (svtype=='feather') {
        arrow::write_feather(obj, svdir, version=2, compression='lz4')
    } else if (svtype=='rds') {
        saveRDS(obj, file = svdir, compress=T) 
    }

    # print result
    sprintf('"%s" saved as "%s.%s"', as.character(substitute(obj)), svname, svtype) %>% cat()

    end <- Sys.time()
    gap <- end - start
    sprintf(' (%s %s)\n', round(gap, 2), units(gap)) %>% cat()
}


#' Load file into R environment.
#'
#' Currently only support "rds" and "feather" format. If only one format exists, the function will load it automatically. Otherwise, it will stop and ask you to select which format to load.
#'
#' @export
#' @param filename expression or char. The file name to be loaded, e.g. ld(x) equals to load(file='dt.rds')
#' @param ldname expression or char. The object name in R. If NULL then equals to filename
#' @param force Whether the object should be reloaded if it's already in the current environment.
#' @param path From which subfolder to read the data. Default to './data'. If './', load from current directory.
#' @examples
#' ld(dt)
#' ld(filename=dt, obj=dt_new, path='../Rdata')
ld <- function(filename, ldname=NULL, ldtype=NULL, path = './data', force = F) {
    start <- Sys.time()
    # check if ldtype is valid
    # possible value: NULL, "rds", "feather"
    if (!is.null(ldtype) && !(ldtype %in% c('rds', 'feather'))) {
        stop('`ldtype` could only be "rds", "feather" or "NULL"')
    }

    # convert filename/ldname to string
    filename = as.character(substitute(filename))

    ldname = as.character(substitute(ldname))

    # verify file type: rds or feather
    # if file doesn't exist, stop;
    # if both exists, stop and ask for clarification;
    # if only one exists, assign it to `lddir`
    hit = list.files(path, pattern=sprintf('^%s\\.(rds|feather)', filename))
    if (length(hit)==0) {
        stop(sprint('%s.rds or %s.feather does NOT exists!', filename, filename))
    } else if (length(hit)==1) {
        lddir <- sprintf('%s/%s', path, hit)
        ldtype = str_split(hit, '\\.')[[1]] %>% tail(1)
        filename_ext = hit
    } else if (length(hit)==2 & !is.null(ldtype)) {
        lddir <- sprintf('%s/%s.%s', path, filename, ldtype)
        filename_ext = hit[str_detect(hit, sprintf('%s$', ldtype))]
    } else {
        stop(sprintf('Both "%s.rds" and "%s.feather" are found, please clarify.', filename, filename))
    }

    # get file size before loading
    file_size = neat_file_size(lddir)

    # If force is F and filename/ldname already exist in .GlobalEnv, SKIP
    if (force==F & (exists(filename) | ifelse(length(ldname)==0, F, exists(ldname)))) {
        file_in_env = ifelse(length(ldname)==0, filename, sprintf('"%s" or "%s"', filename, ldname))

        sprintf('"%s" (%s) already in .GlobalEnv, will NOT load again!', file_in_env, file_size) %>% cat()

    # else, load the file
    } else {
        # first, load file as val
        if (ldtype=='feather') {
            val = arrow::read_feather(lddir) %>% setDT()
        }       
        else if (ldtype=='rds') {
            val = readRDS(lddir)
        } 

        # then assign `val` a name
        # if ldname is null, use filename as ldname
        if (length(ldname)==0) { 
            assign(filename, val, env=.GlobalEnv)
            sprintf('"%s" (%s) loaded', filename_ext, file_size) %>% cat()
        
        # if ldname is NOT null, use ldname
        } else { 
            assign(ldname, val, env=.GlobalEnv)
            sprintf('"%s" loaded as "%s"', filename_ext, ldname) %>% cat()
        }
    }
        
    # output time elapsed
    end <- Sys.time()
    gap <- end - start
    sprintf(' (%s %s)\n', round(gap, 2), units(gap)) %>% cat()
}
