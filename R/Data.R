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
#' @export
setGeneric("sqlData", function(con, values, row.names = NA, ...) {
  standardGeneric("sqlData")
})

#' @rdname sqlData
#' @export
setMethod("sqlData", "DBIConnection", function(con, values, row.names = NA, ...) {
  values <- rownamesToColumn(values, row.names)

  # Convert factors to strings
  is_factor <- vapply(values, is.factor, logical(1))
  values[is_factor] <- lapply(values[is_factor], as.character)

  # Quote all strings
  is_char <- vapply(values, is.character, logical(1))
  values[is_char] <- lapply(values[is_char], function(x) dbQuoteString(con, x))

  # Convert everything to character and turn NAs into NULL
  values[] <- lapply(values, as.character)
  values[is.na(values)] <- "NULL"

  values
})
