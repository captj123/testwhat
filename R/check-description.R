#' Check the DESCRIPTION fields
#' 
#' Check that the DESCRIPTION fields provided by the student are correct.
#' @param state The state of the exercise, as returned from \code{\link{parse_desc}}.
#' @param element String naming the element of the roxygen block to check. 
#' @param regex String providing a regular expression for the solution code to 
#' match. See \code{\link{check_code}}.
#' @param expected The expected value.
#' @param fixed Logical. If \code{TRUE}, regex is treated as a fixed string, not
#' a regular expression. See \code{\link{check_code}}.
#' @param times Positive integer. Denotes the number of times the string in 
#' \code{regex} should be matched.
#' @param missing_msg Optional string. Used to override the feedback message
#' in the event of failure.
#' @param incorrect_msg Optional string. Used to override the feedback message
#' in the event of failure.
#' @param not_typed_msg Optional string. Used to override the feedback message
#' in the event of failure.
#' @param bad_format_msg Optional string. Used to override the feedback message
#' in the event of failure.
#' incorrect_msg
#' @param append For compatibility; currently unused.
#' @return This function is invoked for the side effect of registering feedback 
#' in the event of a failed test. See \code{\link{check_that}} for details of
#' the return value and feedback mechanism.
#' \code{check_has_desc_element} checks that the \code{element} element of the 
#' DESCRIPTION is present.
#' \code{check_desc_element_equals} checks that the \code{element} element of 
#' the DESCRIPTION is equal to the value in the solution code.
#' \code{check_desc_element_matches} checks that the \code{element} element of 
#' the DESCRIPTION matches a regular expression or string.
#' @note None of these functions use solution code. In order to facilitate this,
#' the design of \code{check_desc_version},  \code{check_desc_date}, and
#'  \code{check_desc_authors_at_r} differs from other testwhat check functions
#'  in that they require an \code{expected} argument, which takes the expected
#'  value. This is coerced into a \code{package_version}, \code{Date}, or 
#'  \code{person} object.
#' @examples 
#' \dontrun{
#'   # Always begin by calling parse_desc() on the exercise state.
#'   ex() %>% parse_desc() %>% {
#'     check_has_desc_element(., 'title')
#'     check_roxy_element_matches(., 'description', 'data.+manipulation')
#'     check_desc_version(., '1.0.0')
#'     check_desc_date(., '1919-10-18')
#'     check_desc_authors_at_r(., 
#'       'Richie Cotton <richie@@datacamp.com> [cre, aut]'
#'     )
#'   }
#' }
#' @export
check_has_desc_element <- function(state, element, missing_msg = NULL, append = TRUE) {
  
  student_pd <- state$get("student_pd")
  
  if(is.null(missing_msg)) {
    missing_msg <- sprintf(
      "The '%s' element of the DESCRIPTION is `NULL` or not present.", 
      element
    )
  }
  actual <- is.null(student_pd[[element]])
  check_that(is_false(actual), feedback = missing_msg)
}

#' @rdname check_has_desc_element
#' @export
check_desc_element_matches <- function(state, element, regex, fixed = FALSE, times = 1L, not_typed_msg = NULL, append = TRUE) {
  check_has_desc_element(state, element)
  
  student_pd <- state$get("student_pd")
  
  if(is.null(not_typed_msg)) {
    not_typed_msg <- sprintf(
      "The '%s' element of the DESCRIPTION  does not match '%s'.", 
      element, regex
    )
  }
  actual <- student_pd[[element]]
  num_hits <- get_num_hits(regex = regex, x = actual, fixed = fixed)
  check_that(is_gte(num_hits, times), feedback = not_typed_msg)
}

#' @rdname check_has_desc_element
#' @export
check_desc_version <- function(state, expected, bad_format_msg = NULL, incorrect_msg = NULL, append = TRUE) {
  check_has_desc_element(state, "Version")
  
  student_pd <- state$get("student_pd")
  
  expected <- as.package_version(expected)
  
  if(is.null(bad_format_msg)) {
    bad_format_msg <- 
      "The 'Version' element of the DESCRIPTION is not in the correct format for a package version. It should be two more more integers, separated by dots or dashes."
  }
  
  if(is.null(incorrect_msg)) {
    incorrect_msg <- 
      "The 'Version' element of the DESCRIPTION does not have not the correct value."
  }
  
  actual <- student_pd[["Version"]]
  is_num_ver <- tryCatch(
    {as.package_version(); TRUE},
    error = function(e) FALSE
  )
  check_that(is_true(is_num_ver), feedback = bad_format_msg)
  
  actual <- as.package_version(actual)
  check_that(is_equal(actual, expected), feedback = incorrect_msg)
}

#' @rdname check_has_desc_element
#' @export
check_desc_date <- function(state, expected = Sys.Date(), bad_format_msg = NULL, incorrect_msg = NULL, append = TRUE) {
  check_has_desc_element(state, "Version")
  
  student_pd <- state$get("student_pd")
  
  expected <- as.Date(expected)
  
  if(is.null(bad_format_msg)) {
    bad_format_msg <- 
      "The 'Date' element of the DESCRIPTION is not in the correct format for a package date. It should be yyyy-mm-dd."
  }
  
  if(is.null(incorrect_msg)) {
    incorrect_msg <- 
      "The 'Date' element of the DESCRIPTION does not have not the correct value."
  }
  
  actual <- student_pd[["Version"]]
  is_date <- tryCatch(
    {as.Date(actual); TRUE},
    error = function(e) FALSE
  )
  check_that(is_true(is_date), feedback = bad_format_msg)
  
  actual <- as.Date(actual)
  check_that(is_equal(actual, expected), feedback = incorrect_msg)
}

#' @rdname check_has_desc_element
#' @importFrom utils as.person
#' @export
check_desc_authors_at_r <- function(state, expected, bad_format_msg = NULL, incorrect_msg = NULL, append = TRUE) {
  check_has_desc_element(state, "Version")
  
  student_pd <- state$get("student_pd")
  
  expected <- as.person(expected)
  
  if(is.null(bad_format_msg)) {
    bad_format_msg <- 
      "The 'Authors@R' element of the DESCRIPTION is not in the correct format. It should be a person object."
  }
  
  if(is.null(incorrect_msg)) {
    incorrect_msg <- 
      "The 'Authors@R' element of the DESCRIPTION does not have not the correct value."
  }
  
  actual <- student_pd[["Authors@R"]]
  is_person <- tryCatch(
    {
      authors <- eval(parse(text = actual))
      inherits(authors, "person")
    },
    error = function(e) FALSE
  )
  check_that(is_true(is_person), feedback = bad_format_msg)
  
  actual <- eval(parse(text = actual))
  check_that(is_equal(actual, expected), feedback = incorrect_msg)
}
