#' Create a tidy summary of a data frame
#'
#' Create a summary (small, medium or large) of a data frame.
#'
#' @param x a \code{\link[base:data.frame]{data.frame}} to generate a summary for.
#' Can also be an \link[base:atomic]{atomic} vector (numeric, factor, boolean etc.).
#' @param size how large the summary should be. Either \code{small}, \code{medium}
#' or \code{large} (or simply \code{s},  \code{m} or \code{l}).
#' @param digits the number of decimal places for rounding. Can be
#' an integer or \code{NULL} (no rounding).
#'
#' @return A data frame with one row per column.
#'
#' @details
#'
#' The returned data frame has the following columns:
#'
#' \tabular{ll}{
#'   \code{column} \tab Name of the column \cr
#'   \code{n} \tab Number of observations \cr
#'   \code{class} \tab Data type \cr
#'   \code{min} \tab Minimum value \cr
#'   \code{max} \tab Maximum value \cr
#'   \code{mean} \tab Mean value \cr
#'   \code{sd} \tab Standard deviation \cr
#'   \code{unique} \tab Number of unique values (includes \code{NA}) \cr
#'   \code{median} \tab Median value \cr
#'   \code{na} \tab Number of \code{NA}s (missing values) \cr
#'   \code{mode} \tab Most frequent value \cr
#'   \code{quant25} \tab 25 percent quantile \cr
#'   \code{quant75} \tab 75 percent quantile \cr
#'   \code{levels} \tab Number of factor levels (only relevant for factors) \cr
#'   \code{head} \tab Top (first) value of the data frame \cr
#'   \code{tail} \tab Bottom (last) value of the data frame \cr
#'   \code{se} \tab Standard error \cr
#'   \code{skewness} \tab Skewness \cr
#'   \code{kurtosis} \tab Kurtosis \cr
#'   \code{bytes} \tab Size in bytes of the memory usage of the column
#'}
#'
#' Note:
#'
#' \itemize{
#'   \item For character vectors, the text length will be used to calculate the
#'         statistics.
#'   \item Missing values will be automatically removed before any statistical
#'         summaries are calculated
#'   \item Statistical summaries are produced using base R functions with
#'         defaults. Kurtosis and skewness is produced using code adapted
#'         from the moments package
#'         (\url{https://cran.r-project.org/web/packages/moments/index.html}).
#' }
#'
#' @export
#'
#' @examples
#' # Summary for data frame
#' tidysummary(iris)
#' tidysummary(iris, "medium")
#' tidysummary(iris, digits=2)
#'
#' # Summary for atomic vector
#' tidysummary(iris$Sepal.Length, "large")
#'
#' # Convert summary to Markdown
#' output <- tidysummary(iris)
#' to_markdown(output)
#'
#' @seealso
#' \code{\link[tidysummary:to_markdown]{to_markdown()}},
#' \code{\link[tidysummary:to_latex]{to_latex()}},
#' \code{\link[tidysummary:to_html]{to_html()}},
#' \code{\link[tidysummary:to_json]{to_json()}},
#' \code{\link[tidysummary:to_csv]{to_csv()}},
#' \code{\link[tidysummary:to_excel]{to_excel()}}
#'
#' @importFrom rlang :=
tidysummary <- function(x, size="small", digits=2) {
  if (is.atomic(x)) {
    # Force atomic vectors into a single row data frame
    col_name <- deparse(substitute(x))
    x <- dplyr::rename(data.frame(var = x), "{col_name}" := "var")
  }
  validate_input_arguments(x, size, digits)
  df <- tidysummary_small(x)
  if (base::tolower(size) %in% c("medium", "m")) {
    df <- cbind(df, tidysummary_medium(x))
  } else if (base::tolower(size) %in% c("large", "l")) {
    df <- cbind(df, tidysummary_medium(x))
    df <- cbind(df, tidysummary_large(x))
  }
  if (!is.null(digits)) {
    df <- round_digits_dataframe(df, digits)
  }
  rownames(df) <- NULL
  class(df) <- c("tidysummary", "data.frame")
  df
}

validate_input_arguments <- function(x, size, digits) {
  if (!(is.numeric(x) | is.factor(x) | is.data.frame(x))) {
    stop("tidysummary() can't handle ", class(x)[1], " types, only data frames.",
         call. = FALSE)
  }
  if (!(is.null(digits) | is.numeric(digits) && digits >= 0)) {
    stop("Incorrect 'digits', must be NULL or 0 and above.", call.=FALSE)
  }
  if (base::tolower(size) %in% c("xl", "xs")) {
    stop('Argument "size" must be small, medium, or large (s, m or l), not "', size, '".\n\n',
         "Do you think that there should be extra small or extra large sizes?\n",
         "Then submit a feature request at ",
         "https://github.com/peterdalle/tidysummary",
         call.=FALSE)
  }
  if (!(base::tolower(size) %in% c("small", "medium", "large", "s", "m", "l"))) {
    stop('Argument "size" must be small, medium, or large (s, m or l), not "', size, '".',
         call.=FALSE)
  }
}

tidysummary_small <- function(x) {
  data.frame(
    column = names(x),
    n = vapply(x, get_rows, numeric(1)),
    class = vapply(x, get_class, character(1)),
    min = vapply(x, get_min, numeric(1)),
    max = vapply(x, get_max, numeric(1)),
    mean = vapply(x, get_mean, numeric(1)),
    sd = vapply(x, get_stddev, numeric(1)),
    unique = vapply(x, get_unique, numeric(1)),
    median = vapply(x, get_median, numeric(1)),
    na = vapply(x, get_na, numeric(1))
  )
}

tidysummary_medium <- function(x) {
  data.frame(
    mode = vapply(x, get_mode, numeric(1)),
    quant25 = vapply(x, get_quant, numeric(1), 2),
    quant75 = vapply(x, get_quant, numeric(1), 4),
    levels = vapply(x, get_factor_levels_count, numeric(1))
  )
}

tidysummary_large <- function(x) {
  data.frame(
    head = sapply(x, get_head, simplify="array"),
    tail = sapply(x, get_tail, simplify="array"),
    se = vapply(x, get_stderr, numeric(1)),
    skewness = vapply(x, get_skewness, numeric(1)),
    kurtosis = vapply(x, get_kurtosis, numeric(1)),
    bytes = vapply(x, get_size, numeric(1))
    #null = vapply(x, get_null, numeric(1)),
    #label = vapply(x, get_label, character(1)),
  )
}

round_digits_dataframe <- function(df, digits) {
  dplyr::mutate(df, dplyr::across(.fns = function(x) try_round(x, digits=digits)))
}

try_round <- function(x, digits) {
  if (is.numeric(x)) {
    round(x, digits)
  } else {
    x
  }
}

get_rows <- function(x) {
  length(x) - sum(is.na(x))
}

get_class <- function(x) {
  class(x)[1]
}

get_size <- function(x) {
  utils::object.size(x)
}

get_label <- function(x, max_length=NULL) {
  if (is.null(max_length)) {
    paste0(attr(x, "label"), collapse=" ")
  } else {
    substr(paste0(attr(x, "label"), collapse=" "), 1, max_length)
  }
}

get_quant <- function(x, part, na.rm=TRUE) {
  if (is.numeric(x)) {
    stats::quantile(x, names=FALSE, na.rm=na.rm)[part]
  } else if (is.character(x)) {
    stats::quantile(nchar(x), names=FALSE, na.rm=na.rm)[part]
  } else {
    NA
  }
}

get_head <- function(x, n=1) {
  if (length(x) > 0) {
    if (is.numeric(x)) {
      return(paste(x[1], collapse=" "))
    }
  }
  NA
}

get_tail <- function(x, n=1) {
  if (length(x) > 0) {
    if (is.numeric(x)) {
      return(paste(x[length(x):length(x) - n], collapse=" "))
    }
  }
  NA
}

get_na <- function(x) {
  sum(is.na(x))
}

get_null <- function(x) {
  sum(is.null(x))
}

get_unique <- function(x) {
  length(unique(x))
}

get_factor_levels_count <- function(x) {
  if (is.factor(x)) {
    length(levels(x))
  } else {
    NA
  }
}

get_min <- function(x, na.rm=TRUE) {
  if (is.numeric(x) | is.logical(x) | is_date_time(x)) {
    min(x, na.rm=na.rm)
  } else if (is.character(x)) {
    min(nchar(x), na.rm=na.rm)
  } else {
    NA
  }
}

get_max <- function(x, na.rm=TRUE) {
  if (is.numeric(x) | is.logical(x) | is_date_time(x)) {
    max(x, na.rm=na.rm)
  } else if (is.character(x)) {
    max(nchar(x), na.rm=na.rm)
  } else {
    NA
  }
}

is_date_time <- function(value) {
  any(class(value) %in% c("Date", "POSIXct", "POSIXt"))
}

get_mean <- function(x, na.rm=TRUE) {
  if (is.numeric(x) | is.logical(x)) {
    mean(x, na.rm=na.rm)
  } else if (is.character(x)) {
    mean(nchar(x), na.rm=na.rm)
  } else {
    NA
  }
}

get_median <- function(x, na.rm=TRUE) {
  if (is.numeric(x) | is.logical(x) | is_date_time(x)) {
    stats::median(x, na.rm=na.rm)
  } else if (is.character(x)) {
    stats::median(nchar(x), na.rm=na.rm)
  } else {
    NA
  }
}

get_mode <- function(x) {
  if (is.numeric(x) | is.logical(x)) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  } else {
    NA
  }
}

get_stddev <- function(x, na.rm=TRUE) {
  if (is.numeric(x) | is.logical(x)) {
    stats::sd(x, na.rm=na.rm)
  } else if (is.character(x)) {
    stats::sd(nchar(x), na.rm=na.rm)
  } else {
    NA
  }
}

get_stderr <- function(x, na.rm=TRUE) {
  if (is.numeric(x) | is.logical(x)) {
    stats::sd(x, na.rm=na.rm) / sqrt(length((x)))
  } else {
    NA
  }
}

# Implemented from the moments package
# https://github.com/cran/moments/blob/master/R/kurtosis.R
get_kurtosis <- function(x, na.rm=TRUE) {
  if (is.numeric(x) | is.logical(x)) {
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    n <- length(x)
    m <- mean(x)
    return(n * sum((x - m) ^ 4) / (sum((x - m) ^ 2) ^ 2))
  } else {
    NA
  }
}

# Implemented from the moments package
# https://github.com/cran/moments/blob/master/R/skewness.R
get_skewness <- function(x, na.rm=TRUE) {
  if (is.numeric(x) | is.logical(x)) {
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    n <- length(x)
    m <- mean(x)
    return((sum((x - m) ^ 3) / n) / (sum((x - m) ^ 2) / n) ^ (3 / 2))
  } else {
    NA
  }
}

