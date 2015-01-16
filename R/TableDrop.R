#' Drop table.
#'
#' @inheritParams sqlTableCreate
#' @export
#' @examples
#' sqlTableDrop(ANSI(), "mtcars")
#' sqlTableDrop(ANSI(), "mt\nca\"rs")
setGeneric("sqlTableDrop", function(con, table, ...) {
  standardGeneric("sqlTableDrop")
})

#' @export
#' @rdname sqlTableDrop
setMethod("sqlTableDrop", "DBIConnection", function(con, table, ...) {
  table <- dbQuoteIdentifier(con, table)

  SQL(paste0("DROP TABLE ", table))
})
