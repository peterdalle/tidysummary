% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/converters.R
\name{to_excel}
\alias{to_excel}
\title{Create an Excel file from a tidysummary}
\usage{
to_excel(df, file, as_table = TRUE, overwrite = FALSE)
}
\arguments{
\item{df}{a data frame.}

\item{file}{filename to create on disk.}

\item{as_table}{whether the summary should be formatted as an Excel table.}

\item{overwrite}{whether to overwrite the file if it already exists.}
}
\description{
Takes a tidysummary object and saves it into an Excel file.
}
\details{
Uses the \code{\link[openxlsx]{openxlsx}} package for saving Excel files.
}
\examples{
\dontrun{
data <- tidysummary(iris)

to_excel(data, file="summary.xlsx")
}
}
\seealso{
\code{\link[tidysummary:tidysummary]{tidysummary()}}
}
