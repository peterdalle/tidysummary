#' Create a table in Markdown from a tidysummary
#'
#' Takes a tidysummary object and converts it into Markdown that is returned.
#' If the \code{file} argument is supplied, the Markdown is saved to a file
#' instead.
#'
#' @details
#' Sends the data frame to \code{\link[knitr:kable]{knitr::kable()}}.
#'
#' @param df a data frame.
#' @param file file name to save. If file is \code{NULL}, then the output is returned.
#' Otherwise, there is no output.
#' @param overwrite whether to overwrite the file if it already exists.
#' @param column_quotes whether to use quotes around column names.
#' @param ... other arguments passed on to \code{\link[knitr:kable]{knitr::kable()}}.
#'
#' @return A character vector of the table source code.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- tidysummary(iris)
#'
#' to_markdown(data)
#' to_markdown(data, file="summary.md")
#' }
#' @seealso
#' \code{\link[tidysummary:tidysummary]{tidysummary()}}
to_markdown <- function(df, file=NULL, overwrite=FALSE, column_quotes=FALSE, ...) {
  to_knitr_kable(df, file=file, overwrite=overwrite, format="simple",
                 column_quotes=column_quotes, caller="to_markdown()", ...)
}

#' Create a table in LaTeX from a tidysummary
#'
#' Takes a tidysummary object and converts it into LaTeX that is returned.
#' If the \code{file} argument is supplied, the LaTeX is saved to a file
#' instead.
#'
#' @details
#' Sends the data frame to \code{\link[knitr:kable]{knitr::kable()}}.
#'
#' @param df a data frame.
#' @param file file name to save. If file is \code{NULL}, then the output is returned.
#' Otherwise, there is no output.
#' @param overwrite whether to overwrite the file if it already exists.
#' @param ... other arguments passed on to \code{\link[knitr:kable]{knitr::kable()}}.
#'
#' @return A character vector of the table source code.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- tidysummary(iris)
#'
#' to_latex(data)
#' to_latex(data, file="summary.latex")
#' }
#' @seealso
#' \code{\link[tidysummary:tidysummary]{tidysummary()}}
to_latex <- function(df, file=NULL, overwrite=FALSE, ...) {
  to_knitr_kable(df, file=file, overwrite=overwrite, format="latex",
                 caller="to_latex()", ...)
}

#' Create a table in HTML from a tidysummary
#'
#' Takes a tidysummary object and converts it into HTML that is returned.
#' If the \code{file} argument is supplied, the HTML is saved to a file
#' instead.
#'
#' @details
#' Sends the data frame to \code{\link[knitr:kable]{knitr::kable()}}.
#'
#' @param df a data frame.
#' @param file file name to save. If file is \code{NULL}, then the output is returned.
#' Otherwise, there is no output.
#' @param overwrite whether to overwrite the file if it already exists.
#' @param ... other arguments passed on to \code{\link[knitr:kable]{knitr::kable()}}.
#'
#' @return A character vector of the table source code.
#' If file is not \code{NULL}, however, nothing is returned.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- tidysummary(iris)
#'
#' to_html(data)
#' to_html(data, file="summary.html")
#' }
#' @seealso
#' \code{\link[tidysummary:tidysummary]{tidysummary()}}
to_html <- function(df, file=NULL, overwrite=FALSE, ...) {
  to_knitr_kable(df, file=file, overwrite=overwrite, format="html",
                 caller="to_html()", ...)
}

to_knitr_kable <- function(df, format, column_quotes=FALSE, file=NULL, overwrite=FALSE, caller="", ...) {
  warn_if_bad_type(df, caller)
  if (column_quotes && ("column" %in% names(df))) {
    df$column <- sapply(df$column, FUN=function(x) pretty_quotes(x))
  }
  output <- knitr::kable(df, format=format, ...)
  if (!is.null(file)) {
    save_textfile(output, file=file, overwrite=overwrite)
  } else {
    output
  }
}

#' Create a table in JSON from a tidysummary
#'
#' Takes a tidysummary object and converts it into JSON that is returned.
#' If the \code{file} argument is supplied, the JSON is saved to a file
#' instead.
#'
#' @details
#' Sends the data frame to \code{\link[jsonlite:toJSON]{jsonlite::toJSON()}}.
#'
#' @param df a data frame.
#' @param file file name to save. If file is \code{NULL}, then the output is returned.
#' Otherwise, there is no output.
#' @param overwrite whether to overwrite the file if it already exists.
#' @param ... other arguments passed on to \code{\link[jsonlite:toJSON]{jsonlite::toJSON()}}.
#'
#' @return A character vector of the table source code.
#' If file is not \code{NULL}, however, nothing is returned.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- tidysummary(iris)
#'
#' to_json(data)
#' to_json(data, file="summary.json")
#' }
#' @seealso
#' \code{\link[tidysummary:tidysummary]{tidysummary()}}
to_json <- function(df, file=NULL, overwrite=FALSE, ...) {
  if (!missing(file)) {
    stop_if_file_exists(file=file, overwrite=overwrite)
    jsonlite::write_json(x=df, path=file, pretty=TRUE, ...)
  } else {
    jsonlite::toJSON(x=df, pretty=TRUE, ...)
  }
}

save_textfile <- function(text, file, overwrite=FALSE) {
  stop_if_file_exists(file=file, overwrite=overwrite)
  fileConn <- file(file, encoding="UTF-8")
  writeLines(text, fileConn)
  close(fileConn)
}

warn_if_bad_type <- function(df, caller) {
  if (!is_tidysummary(df)) {
    warning("The input is not a tidysummary object. ",
            "This might have unexpected results.\n",
            "Try using tidysummary() before ", caller, ".",
            call. = FALSE)
  }
}

stop_if_file_exists <- function(file, overwrite=FALSE) {
  if (missing(file) | is.null(file)) {
    stop('Argument "file" is missing.')
  }
  if (file.exists(file) & !overwrite) {
    stop('File "', file, '" already exists. ',
         'Use "overwrite = TRUE" to overwrite existing file.', call. = FALSE)
  }
}

is_tidysummary <- function(x) {
  "tidysummary" %in% class(x)
}

pretty_quotes <- function(x) {
  stringr::str_glue("`", x, "`")
}

#' Create an Excel file from a tidysummary
#'
#' Takes a tidysummary object and saves it into an Excel file.
#'
#' @details
#' Uses the \code{\link[openxlsx]{openxlsx}} package for saving Excel files.
#'
#' @param df a data frame.
#' @param file filename to create on disk.
#' @param as_table whether the summary should be formatted as an Excel table.
#' @param overwrite whether to overwrite the file if it already exists.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- tidysummary(iris)
#'
#' to_excel(data, file="summary.xlsx")
#' }
#' @seealso
#' \code{\link[tidysummary:tidysummary]{tidysummary()}}
to_excel <- function(df, file, as_table=TRUE, overwrite=FALSE) {
  warn_if_bad_type(df, caller="to_excel()")
  stop_if_file_exists(file=file, overwrite=overwrite)
  openxlsx::write.xlsx(x=df, file=file, asTable=as_table, overwrite=overwrite)
}

#' Create a CSV file from a tidysummary
#'
#' Takes a tidysummary object and saves it into a CSV file with UTF-8 encoding.
#'
#' @details
#' Uses the \code{\link[utils:write.csv]{write.csv}} function to save the file.
#'
#' @param df a data frame.
#' @param file filename to create on disk.
#' @param overwrite whether to overwrite the file if it already exists.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- tidysummary(iris)
#'
#' to_csv(data, file="filename.csv")
#' }
#' @seealso
#' \code{\link[tidysummary:tidysummary]{tidysummary()}}
to_csv <- function(df, file, overwrite=FALSE) {
  warn_if_bad_type(df, caller="to_csv()")
  stop_if_file_exists(file=file, overwrite=overwrite)
  utils::write.csv(x=df, file=file, fileEncoding="UTF-8", na="NA",
                   quote=FALSE, row.names=FALSE)
}
