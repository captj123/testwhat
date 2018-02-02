#' Check the roxygen comments
#' 
#' Check that the roxygen comments provided by the student are correct.
#' @param state The state of the exercise, as returned from \code{\link{parse_roxy}}.
#' @param element String naming the element of the roxygen block to check. 
#' @param index A positive integer or a string naming a function. This describes 
#' which roxygen element in the code to check.
#' @param regex String providing a regular expression for the solution code to 
#' match. See \code{\link{check_code}}.
#' @param fixed Logical. If \code{TRUE}, regex is treated as a fixed string, not
#' a regular expression. See \code{\link{check_code}}.
#' @param times Positive integer. Denotes the number of times the string in 
#' \code{regex} should be matched.
#' @param param_name String naming a parameter for the function.
#' @param missing_msg Optional string. Used to override the feedback message
#' in the event of failure.
#' @param incorrect_msg Optional string. Used to override the feedback message
#' in the event of failure.
#' @param not_typed_msg Optional string. Used to override the feedback message
#' in the event of failure.
#' @param not_runnable_msg Optional string. Used to override the feedback message
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
#' \code{check_roxy_param_equals} checks that the \code{param_name} parameter of 
#' the \code{index} block of roxygen is equal to the value in the solution code.
#' \code{check_roxy_param_matches} checks that the \code{param_name} element of 
#' the \code{index} block of roxygen matches a regular expression or string.
#' \code{check_roxy_example_results} checks that the final result of running the
#' \code{examples} in the \code{index} block of roxygen is equal to the value in
#' the solution code.
#' \code{check_roxy_example_matches} check that the \code{examples} of the 
#' \code{index} block of roxygen matches a regular expression or string.
#' 
#' Only \code{check_roxy_element_equals} and \code{check_roxy_example_results}
#' require the solution. The other checks can safely be run with 
#' '___BLOCK_SOLUTION_EXEC___'.
#' @examples 
#' \dontrun{
#'   # Always begin by calling parse_roxy() on the exercise state.
#'   ex() %>% parse_roxy() %>% {
#'     check_has_roxy(.)
#'     check_has_roxy_element(., 'title')
#'     check_roxy_element_equals(., 'description', 'This is a mean function')
#'     check_roxy_element_matches(., 'return', 'integer +vector')
#'     check_roxy_param_equals(., 'x', 'A numeric vector.')
#'     check_roxy_param_matches(., 'na.rm', '[Ll]ogical.*missing.')
#'     check_roxy_example_results(., 10)
#'     check_roxy_example_matches(., 'mean\\\\(.*\\\\)')
#'   }
#' }
#' @export
check_has_roxy <- function(state, index = 1L, missing_msg = NULL, append = TRUE) {
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
  actual <- is.null(student_pd[[index]])
  check_that(is_false(actual), feedback = missing_msg)
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
  actual <- is.null(student_pd[[index]][[element]])
  check_that(is_false(actual), feedback = missing_msg)
}

#' @rdname check_has_roxy
#' @export
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
  
  actual <- student_pd[[index]][[element]]
  expected <- solution_pd[[index]][[element]]
  
  check_that(is_equal(actual, expected), feedback = incorrect_msg)
}

#' @rdname check_has_roxy
#' @export
check_roxy_element_matches <- function(state, element, regex, fixed = FALSE, times = 1L, index = 1L, not_typed_msg = NULL, append = TRUE) {
  check_has_roxy_element(state, element, index)
  
  student_pd <- state$get("student_pd")
  
  if(is.null(not_typed_msg)) {
    not_typed_msg <- sprintf(
      "The '%s' element of roxygen block '%s' does not match '%s'.", 
      element, index, regex
    )
  }
  actual <- student_pd[[index]][[element]]
  num_hits <- get_num_hits(regex = regex, x = actual, fixed = fixed)
  check_that(is_gte(num_hits, times), feedback = not_typed_msg)
}

#' @rdname check_has_roxy
#' @export
check_has_roxy_param <- function(state, param_name, index = 1L, missing_msg = NULL, append = TRUE) {
  check_has_roxy_element(state, "param", index) 
  
  student_pd <- state$get("student_pd")
  
  if(is.null(missing_msg)) {
    missing_msg <- sprintf(
      "The '%s' param of roxygen block '%s' is `NULL` or not present.", 
      param_name, index
    )
  }
  actual <- is.null(student_pd[[index]][["param"]][[param_name]])
  check_that(is_false(actual), feedback = missing_msg)
}

#' @rdname check_has_roxy
#' @export
check_roxy_param_matches <- function(state, param_name, regex, fixed = FALSE, index = 1L, not_typed_msg = NULL, append = TRUE) {
  check_has_roxy_param(state, param_name, index)
  
  student_pd <- state$get("student_pd")
  
  if(is.null(not_typed_msg)) {
    not_typed_msg <- sprintf(
      "The '%s' param of roxygen block '%s' does not match '%s'.", 
      param_name, index, regex
    )
  }
  actual <- student_pd[[index]][["param"]][[param_name]]
  num_hits <- get_num_hits(regex = regex, x = actual, fixed = fixed)
  check_that(is_gte(num_hits, 1L), feedback = not_typed_msg)
}

#' @rdname check_has_roxy
#' @export
check_roxy_examples_run <- function(state, index = 1L, not_runnable_msg = NULL, append = TRUE) {
  check_has_roxy_element(state, "examples", index)
  
  student_pd <- state$get("student_pd")
  
  if(is.null(not_runnable_msg)) {
    not_runnable_msg <- sprintf(
      "The examples of roxygen block '%s' are not runnable.", 
      index
    )
  }
  actual <- student_pd[[index]][["examples"]]
  is_runnable <- tryCatch(
    {eval(parse(text = actual)); TRUE},
    error = function(e) FALSE
  )
  check_that(is_true(is_runnable), feedback = not_runnable_msg)
}

#' @rdname check_has_roxy
#' @export
check_roxy_examples_result_equals <- function(state, index = 1L, incorrect_msg = NULL, append = TRUE) {
  check_roxy_examples_run(state, index)
  
  student_pd <- state$get("student_pd")
  solution_pd <- state$get("solution_pd")
  
  if(is.null(incorrect_msg)) {
    incorrect_msg <- sprintf(
      "The result of running examples in roxygen block '%s' is not correct.", 
      index
    )
  }
  
  set.seed(19790801)
  actual <- eval(parse(text = student_pd[[index]][["examples"]]))
  set.seed(19790801)
  expected <- eval(parse(text = solution_pd[[index]][["examples"]]))
  
  check_that(is_equal(actual, expected), feedback = incorrect_msg)
}

#' @rdname check_has_roxy
#' @export
check_roxy_example_matches <- function(state, regex, fixed = FALSE, index = 1L, not_typed_msg = NULL, append = TRUE) {
  check_roxy_examples_run(state, index)
  
  student_pd <- state$get("student_pd")
  
  if(is.null(not_typed_msg)) {
    not_typed_msg <- sprintf(
      "The examples of roxygen block '%s' do not match '%s'.", 
      index, regex
    )
  }
  actual <- student_pd[[index]][["examples"]]
  num_hits <- get_num_hits(regex = regex, x = actual, fixed = fixed)
  check_that(is_gte(num_hits, 1L), feedback = not_typed_msg)
}
