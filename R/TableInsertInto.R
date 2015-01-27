#' Insert rows into a table.
#'
#' @inheritParams sqlTableCreate
#' @inheritParams rownames
#' @param values A data frame. Factors will be converted to character vectors.
#'   Character vectors will be escaped with \code{\link[DBI]{dbQuoteString}}.
#' @export
#' @examples
#' sqlTableInsertInto(ANSI(), "iris", head(iris))
#'
#' sqlTableInsertInto(ANSI(), "mtcars", head(mtcars))
#' sqlTableInsertInto(ANSI(), "mtcars", head(mtcars), row.names = FALSE)
#'
setGeneric("sqlTableInsertInto", function(con, table, values, row.names = NA,
                                          ...) {
  standardGeneric("sqlTableInsertInto")
})

#' @export
#' @rdname sqlTableInsertInto
setMethod("sqlTableInsertInto", "DBIConnection",
  function(con, table, values, row.names = NA, ...) {
    stopifnot(is.data.frame(values))

    table <- dbQuoteIdentifier(con, table)

    values <- rownamesToColumn(values, row.names)
    fields <- dbQuoteIdentifier(con, names(values))

    # Convert factors to strings
    is_factor <- vapply(values, is.factor, logical(1))
    values[is_factor] <- lapply(values[is_factor], as.character)

    # Quote all strings
    is_char <- vapply(values, is.character, logical(1))
    values[is_char] <- lapply(values[is_char], function(x) dbQuoteString(con, x))

    # Convert fields into a character matrix
    rows <- do.call(paste, c(values, sep = ", "))
    SQL(paste0(
      "INSERT INTO  ", table, " ",
      "(\n  ", paste(fields, collapse = ",\n  "), "\n)\n",
      "VALUES\n",
      paste0("  (", rows, ")", collapse = ",\n")
    ))
  }
)
