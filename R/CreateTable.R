#' @importFrom DBI dbQuoteString dbQuoteIdentifier SQL
#' @importFrom methods setGeneric setMethod setClass new
NULL

#' Create a simple table.
#'
#' Exposes interface to simple \code{CREATE TABLE} commands. The default
#' method is ANSI SQL 99 compliant.
#'
#' @param con A database connection.
#' @param name Name of the table. Escaped with
#'   \code{\link[DBI]{dbQuoteIdentifier}}.
#' @param fields A named character vector. Names are column names, values
#'   are types. Names are escaped with \code{\link[DBI]{dbQuoteIdentifier}}.
#'   Field types are unescaped.
#' @param temporary If \code{TRUE}, will generate a temporary table statement.
#' @param ... Other arguments used by individual methods.
#' @export
#' @examples
#' sqlCreateTable(ANSI(), "my-table", c(a = "integer", b = "text"))
setGeneric("sqlCreateTable", function(con, name, fields, temporary = FALSE, ...) {
  standardGeneric("sqlCreateTable")
})

#' @export
#' @rdname sqlCreateTable
setMethod("sqlCreateTable", "DBIConnection",
  function(con, name, fields, temporary = FALSE, ...) {
    name <- dbQuoteIdentifier(con, name)

    field_names <- dbQuoteIdentifier(con, names(fields))
    field_types <- unname(fields)
    fields <- paste0(field_names, " ", field_types)

    SQL(paste0(
      "CREATE ", if (temporary) "TEMPORARY ", "TABLE ", name, " (\n",
      "  ", paste(fields, collapse = ",\n  "), "\n)\n"
    ))
  }
)
