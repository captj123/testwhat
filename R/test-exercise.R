#' Run all tests for an exercise
#'
#' Run all tests for an exercise and report the results (including feedback).
#' This function is run by R Backend and should not be used by course creators.
#'
#' @param sct Submission correctness tests as a character string.
#' @param ex_type Type of the exercise
#' @param pec pre-exercise-code
#' @param student_code character string representing the student code
#' @param solution_code character string representing the solution code
#' @param student_env environment containing the objects defined by the student.
#' @param solution_env environment containing the objects defined by solution code
#' @param output_list the output structure that is generated by RBackend
#' @param seed random seed that is used for SCTs that run expressions.
#'
#' @return A list with components \code{passed} that indicates whether all
#' tests were sucessful, and \code{feedback} that contains a feedback message.
#'
#' @export
test_exercise <- function(sct, 
                          ex_type, 
                          pec,
                          student_code,
                          solution_code,
                          student_env,
                          solution_env,
                          output_list,
                          seed = 42) {
  # backwards compatibility with older versions of RBackend
  if (missing(student_env)) {
    student_env <- globalenv()
  }
  
  # First check if parsing worked out
  if (any(sapply(output_list, `[[`, "type") == "parse-error")) {
    report <- tryCatch(do_parse(student_code),
                       error = function(e) {
                         list(message = parse_fallback_msg)
                       })
    return(c(list(correct = FALSE), report))
  } else {
    # Store everything that's needed locally (initialize does a full reset)
    tw$clear()
    state <- RootState$new(pec = pec,
                           student_code = student_code,
                           student_pd = build_pd(student_code),
                           student_env = student_env,
                           solution_code = solution_code,
                           solution_pd = build_pd(solution_code),
                           solution_env = solution_env,
                           output_list = output_list,
                           test_env = new.env(parent = environment()))
    tw$set(state = state, reporter = DC_reporter$new(), stack = TRUE, seed = seed)
    on.exit(tw$clear())
    
    # Execute sct with the DataCamp reporter such that it collects test results
    correct <- run_until_fail(parse(text = sct))
    feedback <- get_rep()$get_feedback()
    return(generate_payload(feedback = feedback,
                            correct = correct,
                            ex_type = ex_type))
  }
}

get_rep <- function() {
  tw$get("reporter")
}

run_until_fail <- function(code) {
  eval_fail <- try(eval(code, envir = ex()$get("test_env")), silent = TRUE)
  if (inherits(eval_fail, "try-error")) {
    cond <- attr(eval_fail, "condition")$message
    if (identical(cond, sct_failed_msg)) {
      # The SCT failed
      return(FALSE)
    } else {
      # Something actually went wrong, not an SCT that failed
      stop("Something went wrong in the SCT: ", attr(eval_fail, "condition"))
    }
  } else {
    # The SCT passed
    return(TRUE)
  }
}
