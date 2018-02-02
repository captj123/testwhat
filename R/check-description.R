#' Check the DESCRIPTION fields
#' 
#' Check that the DESCRIPTION fields provided by the student are correct.
#' @param state The state of the exercise, as returned from \code{\link{parse_desc}}.
#' @param element String naming the element of the roxygen block to check. 
#' @param regex String providing a regular expression for the solution code to 
#' match. See \code{\link{check_code}}.
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
#' @examples 
#' \dontrun{
#'   # Always begin by calling parse_desc() on the exercise state.
#'   ex() %>% parse_roxy() %>% {
#'     check_has_desc_element(., 'title')
#'     check_roxy_element_equals(., 'version', as.numeric_version('1.0.0'))
#'     check_roxy_element_matches(., 'description', 'data.+manipulation')
#'   }
#' }
#' @export
check_has_desc_element <- function(state, element, missing_msg = NULL, append = TRUE) {
  
  student_pd <- state$get("student_pd")
  
  if(is.null(missing_msg)) {
    missing_msg <- sprintf(
      "The '%s' element of the DESCRIPTION is `NULL` or not present.", 
      element, index
    )
  }
  actual <- is.null(student_pd[[element]])
  check_that(is_false(actual), feedback = missing_msg)
}
