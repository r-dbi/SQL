#' Insert rows into a table.
#'
#' @inheritParams sqlCreateTable
#' @param values A data frame. Factors will be converted to character vectors.
#'   Character vectors will be escaped with \code{\link[DBI]{dbQuoteString}}.
#' @return A character vector with one element for each row in \code{values}.
#' @export
#' @examples
#' sqlInsertInto(ANSI(), "mtcars", head(mtcars))
#' sqlInsertInto(ANSI(), "iris", head(iris))
setGeneric("sqlInsertInto", function(con, table, values, ...) {
  standardGeneric("sqlInsertInto")
})

#' @export
#' @rdname sqlInsertInto
setMethod("sqlInsertInto", "DBIConnection", function(con, table, values, ...) {
  stopifnot(is.data.frame(values))

  table <- dbQuoteIdentifier(con, table)

  # Convert factors to strings
  is_factor <- vapply(values, is.factor, logical(1))
  values[is_factor] <- lapply(values[is_factor], as.character)

  # Quote all strings
  is_char <- vapply(values, is.character, logical(1))
  values[is_char] <- lapply(values[is_char], function(x) dbQuoteString(con, x))

  # Convert fields into a character matrix
  rows <- do.call(paste, c(values, sep = ", "))
  SQL(paste0("INSERT INTO ", table, " (", rows, ")"))
})
