#' Drop table.
#'
#' @inheritParams sqlCreateTable
#' @export
#' @examples
#' sqlDropTable(ANSI(), "mtcars")
#' sqlDropTable(ANSI(), "mt\nca\"rs")
setGeneric("sqlDropTable", function(con, table, ...) {
  standardGeneric("sqlDropTable")
})

#' @export
#' @rdname sqlDropTable
setMethod("sqlDropTable", "DBIConnection", function(con, table, ...) {
  table <- dbQuoteIdentifier(con, table)

  SQL(paste0("DROP TABLE ", table))
})
