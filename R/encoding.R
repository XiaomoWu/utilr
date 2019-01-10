#' @export
char2utf8 <- function(x) {
    if (is.character(x)) {
        enc2utf8(x)
    } else {
        x
    }
}

#' @export
char2native <- function(x) {
    if (is.character(x)) {
        enc2native(x)
    } else {
        x
    }
}

Sys.getlocale()
