#' Convert a data frame into form suitable for upload to a SQL database.
#'
#' This is a generic method that coerces R objects into vectors suitable for
#' upload to the database. The output will vary a little from method to
#' method depending on whether the main upload device is through a single
#' SQL string or multiple parameterised queries.
#'
#' The default method:
#' \itemize{
#'   \item Converts factors to characters
#'   \item Quotes all strings
#'   \item Converts all columns to strings
#'   \item Replaces NA with NULL
#' }
#'
#' @inheritParams sqlTableCreate
#' @inheritParams rownames
#' @param value A data frame
#' @export
setGeneric("sqlData", function(con, value, row.names = NA, ...) {
  standardGeneric("sqlData")
})

#' @rdname sqlData
#' @export
setMethod("sqlData", "DBIConnection", function(con, value, row.names = NA, ...) {
  value <- rownamesToColumn(value, row.names)

  # Convert factors to strings
  is_factor <- vapply(value, is.factor, logical(1))
  value[is_factor] <- lapply(value[is_factor], as.character)

  # Quote all strings
  is_char <- vapply(value, is.character, logical(1))
  value[is_char] <- lapply(value[is_char], function(x) {
    enc2utf8(dbQuoteString(con, x))
  })

  # Convert everything to character and turn NAs into NULL
  value[] <- lapply(value, as.character)
  value[is.na(value)] <- "NULL"

  value
})
