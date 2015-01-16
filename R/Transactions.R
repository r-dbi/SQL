#' Start, commit or rollback a transaction
#'
#' @inheritParams sqlTableCreate
#' @name transactions
#' @examples
#' sqlTransactionStart(ANSI())
#' sqlTransactionCommit(ANSI())
#' sqlTransactionRollback(ANSI())
NULL

#' @export
#' @rdname transactions
setGeneric("sqlTransactionStart", function(con, ...) {
  standardGeneric("sqlTransactionStart")
})

#' @export
#' @rdname transactions
setMethod("sqlTransactionStart", "DBIConnection", function(con, ...) {
  SQL("START TRANSACTION")
})

#' @export
#' @rdname transactions
setGeneric("sqlTransactionCommit", function(con, ...) {
  standardGeneric("sqlTransactionCommit")
})

#' @export
#' @rdname transactions
setMethod("sqlTransactionCommit", "DBIConnection", function(con, ...) {
  SQL("COMMIT")
})

#' @export
#' @rdname transactions
setGeneric("sqlTransactionRollback", function(con, ...) {
  standardGeneric("sqlTransactionRollback")
})

#' @export
#' @rdname transactions
setMethod("sqlTransactionRollback", "DBIConnection", function(con, ...) {
  SQL("ROLLBACK")
})
