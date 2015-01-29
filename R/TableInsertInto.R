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

    sql_values <- sqlDf(con, values, row.names)
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

sqlDf <- function(con, df, row.names = NA) {
  df <- rownamesToColumn(df, row.names)

  # Convert factors to strings
  is_factor <- vapply(df, is.factor, logical(1))
  df[is_factor] <- lapply(df[is_factor], as.character)

  # Quote all strings
  is_char <- vapply(df, is.character, logical(1))
  df[is_char] <- lapply(df[is_char], function(x) dbQuoteString(con, x))

  # Convert everything to character and turn NAs into NULL
  df[] <- lapply(df, as.character)
  df[is.na(df)] <- "NULL"

  df
}
