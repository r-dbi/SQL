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

#' Generated parameterised template for inserting rows.
#'
#' @inheritParams sqlTableCreate
#' @inheritParams sqlTableInsertInto
#' @inheritParams rownames
#' @param prefix Parameter prefix to put in front of column id.
#' @param values A data frame. Used only for the column names.
#' @export
#' @examples
#' sqlTableInsertIntoTemplate(ANSI(), "iris", iris)
#'
#' sqlTableInsertIntoTemplate(ANSI(), "mtcars", mtcars)
#' sqlTableInsertIntoTemplate(ANSI(), "mtcars", mtcars, row.names = FALSE)
sqlTableInsertIntoTemplate <- function(con, table, values, row.names = NA, prefix = "?", ...) {
  table <- dbQuoteIdentifier(con, table)

  values <- rownamesToColumn(values[0, , drop = FALSE], row.names)
  fields <- dbQuoteIdentifier(con, names(values))

  # Convert fields into a character matrix
  SQL(paste0(
    "INSERT INTO ", table, "\n",
    "  (", paste(fields, collapse = ", "), ")\n",
    "VALUES\n",
    paste0("  (", paste0(prefix, seq_along(fields), collapse = ", "), ")", collapse = ",\n")
  ))
}
