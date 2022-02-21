# tidysummary: a package for creating summaries of data frames

The tidysummary package creates a summary of all columns in a data frame, and returns the summary (small, medium or large) as a data frame.

## Install

```r
# install.packages("devtools")
devtools::install_github("peterdalle/tidysummary")
```

## Usage

```r
tidysummary(x, size = "small")
```

where 

- `x` is a data frame.
- `size` is a character with `small`, `medium` or `large` (or simply `s`, `m` or `l`). 

## Examples

### Small summary

```r
tidysummary(iris)
```

```
        column   n   class min max mean   sd unique median na
1 Sepal.Length 150 numeric 4.3 7.9 5.84 0.83     35   5.80  0
2  Sepal.Width 150 numeric 2.0 4.4 3.06 0.44     23   3.00  0
3 Petal.Length 150 numeric 1.0 6.9 3.76 1.77     43   4.35  0
4  Petal.Width 150 numeric 0.1 2.5 1.20 0.76     22   1.30  0
5      Species 150  factor  NA  NA   NA   NA      3     NA  0
```

### Medium summary

```r
tidysummary(iris, "m")
```

```
        column   n   class min max mean   sd unique median na mode quant25 quant75 levels
1 Sepal.Length 150 numeric 4.3 7.9 5.84 0.83     35   5.80  0  5.0     5.1     6.4     NA
2  Sepal.Width 150 numeric 2.0 4.4 3.06 0.44     23   3.00  0  3.0     2.8     3.3     NA
3 Petal.Length 150 numeric 1.0 6.9 3.76 1.77     43   4.35  0  1.4     1.6     5.1     NA
4  Petal.Width 150 numeric 0.1 2.5 1.20 0.76     22   1.30  0  0.2     0.3     1.8     NA
5      Species 150  factor  NA  NA   NA   NA      3     NA  0   NA      NA      NA      3
```

### Large summary

```r
tidysummary(iris, "l")
```

```
        column   n   class min max mean   sd unique median na mode quant25 quant75 levels head tail   se skewness kurtosis bytes
1 Sepal.Length 150 numeric 4.3 7.9 5.84 0.83     35   5.80  0  5.0     5.1     6.4     NA  5.1  6.2 0.07     0.31     2.43  1248
2  Sepal.Width 150 numeric 2.0 4.4 3.06 0.44     23   3.00  0  3.0     2.8     3.3     NA  3.5  3.4 0.04     0.32     3.18  1248
3 Petal.Length 150 numeric 1.0 6.9 3.76 1.77     43   4.35  0  1.4     1.6     5.1     NA  1.4  5.4 0.14    -0.27     1.60  1248
4  Petal.Width 150 numeric 0.1 2.5 1.20 0.76     22   1.30  0  0.2     0.3     1.8     NA  0.2  2.3 0.06    -0.10     1.66  1248
5      Species 150  factor  NA  NA   NA   NA      3     NA  0   NA      NA      NA      3 <NA> <NA>   NA       NA       NA  1248
```

### Convert to Markdown

```r
library(tidyverse)

# Use 5 digits and remove the "na" column
iris %>%
   tidysummary(digits=5) %>% 
   select(-na) %>% 
   to_markdown()
```

```
column            n  class      min   max      mean        sd   unique   median
-------------  ----  --------  ----  ----  --------  --------  -------  -------
Sepal.Length    150  numeric    4.3   7.9   5.84333   0.82807       35     5.80
Sepal.Width     150  numeric    2.0   4.4   3.05733   0.43587       23     3.00
Petal.Length    150  numeric    1.0   6.9   3.75800   1.76530       43     4.35
Petal.Width     150  numeric    0.1   2.5   1.19933   0.76224       22     1.30
Species         150  factor      NA    NA        NA        NA        3       NA
```

You can also use:

- `to_latex()`
- `to_html()`
- `to_json()`
- `to_csv(file = "summary.csv")`
- `to_excel(file = "summary.xlsx")`

## Documentation

See the full documentation by typing `?tidysummary` in the R console.

tidysummary column | Explanation
-------- | --------------------------
`column` | Name of the column
`n` | Number of observations
`class` | Data type
`min` | Minimum value
`max` | Maximum value
`mean` | Mean value
`sd` | Standard deviation
`unique` | Number of unique values (includes `NA`)
`median` | Median value
`na` | Number of `NA`s (missing values)
`mode` | Most frequent value
`quant25` | 25 percent quantile
`quant75` | 75 percent quantile
`levels` | Number of factor levels (only relevant for factors)
`head` | Top (first) value of the data frame
`tail` | Bottom (last) value of the data frame
`se` | Standard error
`skewness` | Skewness
`kurtosis` | Kurtosis
`bytes` | Size in bytes of the memory usage of the column

## Support

[Submit an issue](https://github.com/peterdalle/tidysummary/issues)

## License

[MIT](LICENSE)

## Similar packages

- [summarytools](https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html) - more comprehensive summary/codebook. Does not return a data frame.
