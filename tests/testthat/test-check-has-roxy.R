
FN_WITHOUT_ROXY <- "geomean <- function(x, na.rm = FALSE) {
exp(mean(log(x), na.rm = na.rm))
}"
FN_WITH_ROXY <- "#' Geometric mean
#' 
#' Calculate a geometric mean.
#' @param x A numeric vector of positive numbers.
#' @param na.rm Logical. If \\code{TRUE}, remove missing values before calculating.
#' @return The geometric mean of \\code{x}.
#' @examples 
#' geomean(rlnorm(100, log(5), 0.1)) # more or less 5
#' @export
geomean <- function(x, na.rm = FALSE) {
  exp(mean(log(x), na.rm = na.rm))
}"

context("check_has_roxy")


test_that(
 "test check_has_roxy() passes on a function with roxygen code", {
   lst <- list()
   lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_has_roxy()"
   lst$DC_CODE <- FN_WITH_ROXY
   output <- test_it(lst)
   passes(output)
 }
)

test_that(
  "test check_has_roxy() fails on a function without roxygen code", {
    lst <- list()
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_has_roxy()"
    lst$DC_CODE <- FN_WITHOUT_ROXY
    output <- test_it(lst)
    fails(output)
  }
)
