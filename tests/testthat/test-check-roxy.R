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

ANOTHER_FN_WITH_ROXY <- "#' Hypotenuse
#' 
#' Calculate a hypotenuse.
#' @param x A numeric vector of non-negative numbers.
#' @param y A numeric vector of non-negative numbers.
#' @return The hypotenuse of \\code{x}.
#' @examples 
#' hypotenuse(c(3, 5), c(4, 12)) 
#' hypotenuse('a', 'b')          # throws an error
#' @export
hypotenuse <- function(x, y) {
  sqrt(x ^ 2 + y ^ 2)
}"

YET_ANOTHER_FN_WITH_ROXY <- "#' Harmonic mean
#' 
#' Calculate a harmonic mean.
#' @param x A numeric vector of non-zero numbers.
#' @param na.rm Logical. If \\code{TRUE}, remove missing values before calculating.
#' @return The harmonic mean of \\code{x}.
#' @export
harmmean <- function(x, na.rm = FALSE) {
  1 / mean(1 / x, na.rm = na.rm)
}"

# check_has_roxy ----------------------------------------------------------

context("check_has_roxy")

test_that(
 "test check_has_roxy() passes on a function with roxygen code", {
   lst <- list()
   # Solution code not considered
   lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_has_roxy()"
   lst$DC_CODE <- FN_WITH_ROXY
   output <- test_it(lst)
   passes(output)
 }
)

test_that(
  "test check_has_roxy() fails on a function without roxygen code", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_has_roxy()"
    lst$DC_CODE <- FN_WITHOUT_ROXY
    output <- test_it(lst)
    fails(output)
  }
)

# check_has_roxy_element --------------------------------------------------

context("check_has_roxy_element")

test_that(
  "test check_has_roxy_element() passes on a function with that roxygen element", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_has_roxy_element('title')"
    lst$DC_CODE <- FN_WITH_ROXY
    output <- test_it(lst)
    passes(output)
  }
)

test_that(
  "test check_has_roxy_element() fails on a function without roxygen code", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_has_roxy_element('title')"
    lst$DC_CODE <- FN_WITHOUT_ROXY
    output <- test_it(lst)
    fails(output)
  }
)

test_that(
  "test check_has_roxy_element() fails on a function without that roxygen element", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_has_roxy_element('qwerty')"
    lst$DC_CODE <- FN_WITH_ROXY
    output <- test_it(lst)
    fails(output)
  }
)

# check_roxy_element_equals -----------------------------------------------

context("check_roxy_element_equals")

test_that(
  "test check_roxy_element_equals() passes on a function with a correct roxygen element", {
    lst <- list()
    lst$DC_SOLUTION <- FN_WITH_ROXY
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_element_equals('title')"
    lst$DC_CODE <- FN_WITH_ROXY
    output <- test_it(lst)
    passes(output)
  }
)

test_that(
  "test check_roxy_element_equals() fails on a function without roxygen code", {
    lst <- list()
    lst$DC_SOLUTION <- FN_WITH_ROXY
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_element_equals('title')"
    lst$DC_CODE <- FN_WITHOUT_ROXY
    output <- test_it(lst)
    fails(output)
  }
)

test_that(
  "test check_roxy_element_equals() fails on a function with an incorrect roxygen element", {
    lst <- list()
    lst$DC_SOLUTION <- FN_WITH_ROXY
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_element_equals('title', 'mean')"
    lst$DC_CODE <- ANOTHER_FN_WITH_ROXY
    output <- test_it(lst)
    fails(output)
  }
)

# check_roxy_element_matches ----------------------------------------------

context("check_roxy_element_matches")

test_that(
  "test check_roxy_element_matches() passes on a function with a regex-matching roxygen element", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_element_matches('examples', 'geomean\\\\(.*\\\\)')"
    lst$DC_CODE <- FN_WITH_ROXY
    output <- test_it(lst)
    passes(output)
  }
)

test_that(
  "test check_roxy_element_matches() passes on a function with a fixed-matching roxygen element", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_element_matches('description', 'geometric mean', fixed = TRUE)"
    lst$DC_CODE <- FN_WITH_ROXY
    output <- test_it(lst)
    passes(output)
  }
)

test_that(
  "test check_roxy_element_matches() fails on a function without roxygen code", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_element_matches('title')"
    lst$DC_CODE <- FN_WITHOUT_ROXY
    output <- test_it(lst)
    fails(output)
  }
)

test_that(
  "test check_roxy_element_matches() fails on a function with a mismatched roxygen element", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_element_matches('description', 'arithmetic mean', fixed = TRUE)"
    lst$DC_CODE <- FN_WITH_ROXY
    output <- test_it(lst)
    fails(output)
  }
)

# check_has_roxy_param ----------------------------------------------------

context("check_has_roxy_param")

test_that(
  "test check_has_roxy_param() passes on a function with that roxygen param", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_has_roxy_param('na.rm')"
    lst$DC_CODE <- FN_WITH_ROXY
    output <- test_it(lst)
    passes(output)
  }
)

test_that(
  "test check_has_roxy_param() fails on a function without roxygen code", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_has_roxy_param('na.rm')"
    lst$DC_CODE <- FN_WITHOUT_ROXY
    output <- test_it(lst)
    fails(output)
  }
)

test_that(
  "test check_has_roxy_param() fails on a function without that roxygen param", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_has_roxy_param('y')"
    lst$DC_CODE <- FN_WITH_ROXY
    output <- test_it(lst)
    fails(output)
  }
)

# check_roxy_param_matches ------------------------------------------------

context("check_roxy_param_matches")

test_that(
  "test check_roxy_param_matches() passes on a function with that roxygen param", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_param_matches('na.rm', '[lL]ogical.*remove missing')"
    lst$DC_CODE <- FN_WITH_ROXY
    output <- test_it(lst)
    passes(output)
  }
)

test_that(
  "test check_roxy_param_matches() passes on a function with a fixed-matching roxygen param", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_param_matches('na.rm', 'remove missing values', fixed = TRUE)"
    lst$DC_CODE <- FN_WITH_ROXY
    output <- test_it(lst)
    passes(output)
  }
)

test_that(
  "test check_roxy_param_matches() fails on a function without roxygen code", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_param_matches('x', 'numeric vector')"
    lst$DC_CODE <- FN_WITHOUT_ROXY
    output <- test_it(lst)
    fails(output)
  }
)

test_that(
  "test check_roxy_param_matches() fails on a function with a mismatched roxygen element", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_param_matches('x', 'character vector', fixed = TRUE)"
    lst$DC_CODE <- FN_WITH_ROXY
    output <- test_it(lst)
    fails(output)
  }
)

# check_roxy_examples_run -------------------------------------------------

context("check_roxy_examples_run")

test_that(
  "test check_roxy_examples_run() passes on a function with runnable roxygen examples", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_examples_run()"
    lst$DC_CODE <- FN_WITH_ROXY
    output <- test_it(lst)
    passes(output)
  }
)

test_that(
  "test check_roxy_examples_run() fails on a function with no roxygen examples", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_examples_run()"
    lst$DC_CODE <- YET_ANOTHER_FN_WITH_ROXY
    output <- test_it(lst)
    fails(output)
  }
)

test_that(
  "test check_roxy_examples_run() fails on a function with non-runnable roxygen examples", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_examples_run()"
    lst$DC_CODE <- ANOTHER_FN_WITH_ROXY
    output <- test_it(lst)
    fails(output)
  }
)

# check_roxy_examples_result_equals ---------------------------------------

context("check_roxy_examples_result_equals")

test_that(
  "test check_roxy_examples_result_equals() passes on a function with a correct roxygen element", {
    lst <- list()
    lst$DC_SOLUTION <- FN_WITH_ROXY
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_examples_result_equals()"
    lst$DC_CODE <- FN_WITH_ROXY
    output <- test_it(lst)
    passes(output)
  }
)

test_that(
  "test check_roxy_examples_result_equals() fails on a function with an incorrect roxygen element", {
    lst <- list()
    lst$DC_SOLUTION <- FN_WITH_ROXY
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_examples_result_equals()"
    lst$DC_CODE <- sub("log(5)", "log(10)", FN_WITH_ROXY, fixed = TRUE)
    output <- test_it(lst)
    fails(output)
  }
)

# check_roxy_example_matches ----------------------------------------------

context("check_roxy_example_matches")

test_that(
  "test check_roxy_example_matches() passes on a function with matching roxygen example", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_example_matches('geomean\\\\(.*\\\\)')"
    lst$DC_CODE <- FN_WITH_ROXY
    output <- test_it(lst)
    passes(output)
  }
)

test_that(
  "test check_roxy_example_matches() passes on a function with a fixed-matching roxygen element", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_example_matches('geomean(', fixed = TRUE)"
    lst$DC_CODE <- FN_WITH_ROXY
    output <- test_it(lst)
    passes(output)
  }
)

test_that(
  "test check_roxy_example_matches() fails on a function without roxygen examples", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_example_matches('geomean(', fixed = TRUE)"
    lst$DC_CODE <- YET_ANOTHER_FN_WITH_ROXY
    output <- test_it(lst)
    fails(output)
  }
)

test_that(
  "test check_roxy_example_matches() fails on a function with a mismatched roxygen element", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_example_matches('geomean(', fixed = TRUE)"
    lst$DC_CODE <- ANOTHER_FN_WITH_ROXY
    output <- test_it(lst)
    fails(output)
  }
)
