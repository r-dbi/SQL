#' Safely interpolate values into an SQL string.
#'
#' @param _con A database connection.
#' @param `_sql` A SQL string containing containing variables to interpolate.
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
setGeneric("sqlInterpolate", function(`_con`, `_sql`, ...) {
  standardGeneric("sqlInterpolate")
})

#' @export
#' @rdname sqlInterpolate
setMethod("sqlInterpolate", "DBIConnection", function(`_con`, `_sql`, ...) {
  sql <- `_sql`
  pos <- parseSql(sql)

  if (length(pos$start) == 0)
    return(SQL(sql))

  vars <- substring(sql, pos$start + 1, pos$end)

  values <- list(...)
  if (!setequal(vars, names(values))) {
    stop("Supplied vars don't match vars to interpolate", call. = FALSE)
  }
  values <- values[vars]

  safe_values <- vapply(values, function(x) {
    if (is.character(x)) {
      dbQuoteString(`_con`, x)
    } else {
      as.character(x)
    }
  }, character(1))

  for (i in rev(seq_along(vars))) {
    sql <- paste0(
      substring(sql, 0, pos$start[i] - 1),
      safe_values[i],
      substring(sql, pos$end[i] + 1, nchar(sql))
    )
  }

  SQL(sql)
})
