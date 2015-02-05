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
setGeneric("sqlTableInsertInto", function(con, table, values, row.names = NA,
                                          ...) {
  standardGeneric("sqlTableInsertInto")
})

#' @export
#' @rdname sqlTableInsertInto
setMethod("sqlTableInsertInto", "DBIConnection",
  function(con, table, values, row.names = NA, ...) {
    stopifnot(is.data.frame(values))

    sql_values <- sqlData(con, values, row.names)
    table <- dbQuoteIdentifier(con, table)
    fields <- dbQuoteIdentifier(con, names(sql_values))

    # Convert fields into a character matrix
    rows <- do.call(paste, c(sql_values, sep = ", "))
    SQL(paste0(
      "INSERT INTO ", table, "\n",
      "  (", paste(fields, collapse = ", "), ")\n",
      "VALUES\n",
      paste0("  (", rows, ")", collapse = ",\n")
    ))
  }
)
