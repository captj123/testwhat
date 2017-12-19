#' Check the roxygen comments
#' 
#' Check that the roxygen comments provided by the student are correct.
#' @param state The state of the exercise, as returned from \code{\link{parse_roxy}}.
#' @param element String naming the element of the roxygen block to check. 
#' @param index A positive integer or a string naming a function. This describes 
#' which roxygen element in the code to check.
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
#' @details \code{check_has_roxy} checks that the \code{index} block of roxygen
#' is present.
#' \code{check_has_roxy_element} checks that the \code{element} element of the 
#' \code{index} block of roxygen is present.
#' \code{check_roxy_element_equals} checks that the \code{element} element of 
#' the \code{index} block of roxygen is equal to the value in the solution code.
#' \code{check_roxy_element_matches} checks that the \code{element} element of 
#' the \code{index} block of roxygen matches a regular expression or string.
#' @examples 
#' \dontrun{
#'   # Always begin by calling parse_roxy() on the exercise state.
#'   ex() %>% parse_roxy() %>% {
#'     check_has_roxy(., index = 2)
#'     check_has_roxy_element(., 'title', index = 2)
#'     check_roxy_element_equals(., 'description', 'This is a mean function')
#'     check_roxy_element_matches(., 'return', 'integer +vector')
#'   }
#' }
#' @export
check_has_roxy <- function(state, index = 1L, missing_msg = NULL, append = TRUE) {
  # child_state <- ChildState$new(state)
  # child_state$add_details(type = "has_roxy", index = index, append = append)
  
  student_pd <- state$get("student_pd")
  
  if(is.null(missing_msg)) {
    missing_msg <- sprintf("The '%s' roxygen block is `NULL` or not present.", index)
  }
  if(is.numeric(index)) {
    check_that(
      is_gte(length(student_pd), index),
      feedback = missing_msg
    )
  }
  check_that(
    is_false(is.null(student_pd[[index]])),
    feedback = missing_msg
  )
}

#' @rdname check_has_roxy
#' @export
check_has_roxy_element <- function(state, element, index = 1L, missing_msg = NULL, append = TRUE) {
  check_has_roxy(state, index)
  
  student_pd <- state$get("student_pd")
  
  if(is.null(missing_msg)) {
    missing_msg <- sprintf(
      "The '%s' element of roxygen block '%s' is `NULL` or not present.", 
      element, index
    )
  }
  check_that(
    is_false(is.null(student_pd[[index]][[element]])),
    feedback = 
  )
}

check_roxy_element_equals <- function(state, element, index = 1L, incorrect_msg = NULL, append = TRUE) {
  check_has_roxy_element(state, element, index)
  
  student_pd <- state$get("student_pd")
  solution_pd <- state$get("solution_pd")
  
  if(is.null(incorrect_msg)) {
    incorrect_msg <- sprintf(
      "The '%s' element of roxygen block '%s' is not correct.", 
      element, index
    )
  }
  check_that(
    is_equal(student_pd[[index]][[element]], solution_pd[[index]][[element]]),
    feedback = 
  )
}

check_roxy_element_matches <- function(state, element, regex, fixed = FALSE, times = 1, index = 1L, not_typed_msg = NULL, append = TRUE) {
  check_has_roxy_element(state, element, index)
  
  student_pd <- state$get("student_pd")
  solution_pd <- state$get("solution_pd")
  
  if(is.null(not_typed_msg)) {
    not_typed_msg <- sprintf(
      "The '%s' element of roxygen block '%s' does not match '%s'.", 
      element, index, regex
    )
  }
  num_hits <- get_num_hits(
    regex = regex, 
    x = student_pd[[index]][[element]], 
    fixed = fixed
  )
  check_that(
    is_gte(num_hits, times), 
    feedback = not_typed_msg
  )
}

