#' Safely interpolate values into an SQL string.
#'
#' @inheritParams sqlTableCreate
#' @param sql A SQL string containing containing variables to interpolate.
#'   Variables must start with a question mark and can be any valid R
#'   identifier, i.e. it must start with a letter or \code{.}, and be followed
#'   by a letter, digit, \code{.} or \code{_}.
#' @param ... Named values to interpolate into string. All strings
#'   will be first escaped with \code{\link[DBI]{dbQuoteString}} prior
#'   to interpolation to protect against SQL interpolation attacks.
#' @export
#' @examples
#' sql <- "SELECT * FROM X WHERE name = ?name"
#' sqlInterpolate(ANSI(), sql, name = "Hadley")
#' # This is safe because the single quote has been double escaped
#' sqlInterpolate(ANSI(), sql, name = "H'); DROP TABLE--;")
setGeneric("sqlInterpolate", function(con, sql, ...) {
  standardGeneric("sqlInterpolate")
})

#' @export
#' @rdname sqlInterpolate
setMethod("sqlInterpolate", "DBIConnection", function(con, sql, ...) {
  pos <- parseSql(sql)
  vars <- substr(sql, pos$start + 1, pos$end)

  values <- list(...)
  if (!setequal(vars, names(values))) {
    stop("Supplied vars don't match vars to interpolate", call. = FALSE)
  }

  safe_values <- vapply(values, function(x) {
    if (is.character(x)) {
      dbQuoteString(con, x)
    } else {
      as.character(x)
    }
  }, character(1))

  stringi::stri_sub(sql, pos$start, pos$end) <- safe_values
  SQL(sql)
})
