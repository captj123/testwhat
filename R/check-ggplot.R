#' Test ggplot call
#'
#' @param state the state to start from
#' @param index which call to check
#' @param all_fail_msg Message if all fails
#' 
#' @param check_data Whether or not to check data latyer
#' @param data_fail_msg Message in case data layer fails
#' 
#' @param check_aes Whether or not to check aes latyer
#' @param aes_fail_msg Message in case aes layer fails
#' @param exact_aes Should the aesthetics be exact?
#' 
#' @param check_geom Whether or not to check geom layer
#' @param geom_fail_msg Message in case geom layer fails
#' @param exact_geom Should the geoms be exact?
#' @param check_geom_params Should the geom parameters be checked?
#' 
#' @param check_facet Whether or not to check facet latyer
#' @param facet_fail_msg Message in case facet layer fails
#' @param check_scale Whether or not to check scale latyer
#' 
#' @param scale_fail_msg Message in case scale layer fails
#' @param exact_scale Whether or not scales should be defined exactly
#' 
#' @param check_coord Whether or not to check coord latyer
#' @param coord_fail_msg Message in case coord layer fails
#' @param exact_coord Whether or not coords should be defined exactly
#' 
#' @param check_stat Whether or not to check stat latyer
#' @param stat_fail_msg Message in case stat layer fails
#' @param exact_stat Whether or not stats should be defined exactly
#' 
#' @param check_extra Whether to check extra stuff
#' @param extra_fail_msg Message in case extra stuff fails
#' @param exact_extra Whether or not extra info should be exactly specified.
#' 
#' @param check Which layers to check
#' 
#' @export
check_ggplot <- function(state,
                         index = 1,
                         all_fail_msg = NULL,
                         check_data = TRUE, data_fail_msg = NULL,
                         check_aes = TRUE, aes_fail_msg = NULL, exact_aes = FALSE,
                         check_geom = TRUE, geom_fail_msg = NULL, exact_geom = FALSE, check_geom_params = NULL,
                         check_facet = TRUE, facet_fail_msg = NULL,
                         check_scale = TRUE, scale_fail_msg = NULL, exact_scale = FALSE,
                         check_coord = TRUE, coord_fail_msg = NULL, exact_coord = FALSE,
                         check_stat = TRUE, stat_fail_msg = NULL, exact_stat = FALSE,
                         check_extra = NULL, extra_fail_msg = NULL, exact_extra = NULL,
                         check = NULL) {
  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")
  student_code <- state$get("student_code")
  solution_code <- state$get("solution_code")
  predefined_code <- state$get("pec")
  
  layers <- c("data", "aes", "geom", "facet", "scale", "coord", "stat")
  
  sol_ggplot_info <- get_ggplot_solution_info(solution_code, predefined_code, solution_env)
  sol_ggplot_objects <- sol_ggplot_info$objects
  sol_ggplot_commands <- sol_ggplot_info$commands
  
  if (!is.null(check)) {
    for (layer in layers) {
      if (layer %in% check) {
        assign(paste0("check_", layer), TRUE)
      } else {
        assign(paste0("check_", layer), FALSE)
      }
    }
  }
  
  if (!is.null(all_fail_msg)) {
    for (layer in layers) {
      assign(paste0(layer, "_fail_msg"), all_fail_msg)
    }
    extra_fail_msg = all_fail_msg
  }
  
  sol_selected <- try(sol_ggplot_objects[[index]], silent = TRUE)
  if (inherits(sol_selected, "try-error")) {
    stop(sprintf("Could not find ggplot command %d in your solution environment.", index))
  }
  
  stud_ggplot_info <- get_ggplot_student_info(student_code, predefined_code, student_env)
  stud_ggplot_objects <- stud_ggplot_info$objects
  stud_ggplot_commands <- stud_ggplot_info$commands
  len <- length(stud_ggplot_objects)
  
  if (len < index) {
    check_that(failure(), feedback = "You didn't define enough `ggplot` commands.")
  }
  
  feedback <- sprintf("In your %s `ggplot` command,", nd(index))
  
  stud_selected <- stud_ggplot_objects[[index]]
  
  check_that(is_false(inherits(stud_selected, "try-error")), feedback = paste(feedback, "you got an error. Make sure you use the correct `ggplot` syntax. Have another look at the instructions."))
  
  sol_selected_command <- sol_ggplot_commands[[index]]
  stud_selected_command <- stud_ggplot_commands[[index]]
  
  if (check_data) {
    # Check the data
    test_data_layer(list(base = sol_selected$data), list(base = stud_selected$data), feedback, data_fail_msg)
  }
  
  if (check_aes) {
    # Check the mapping
    test_aes_layer(list(base = sol_selected$mapping), list(base = stud_selected$mapping), feedback, aes_fail_msg, exact_aes)
  }
  
  if (check_geom) {
    # Check the geom layer
    test_geom_layer(sol_selected_command, stud_selected_command, sol_selected$layers, stud_selected$layers, feedback, geom_fail_msg, exact_geom, check_geom_params)
  }
  
  if (check_stat) {
    # Check the stat layer
    test_stat_layer(sol_selected_command, stud_selected_command, feedback, stat_fail_msg, exact_coord, student_env, solution_env)
  }
  
  if (check_facet) {
    # Check the facet layer
    test_facet_layer(sol_selected$facet, stud_selected$facet, feedback, facet_fail_msg)
  }
  
  if (check_scale) {
    # Check the scale layer
    test_scale_layer(sol_selected_command, stud_selected_command, feedback, scale_fail_msg, exact_scale, student_env, solution_env)
  }
  
  if (check_coord) {
    # Check the coord layer
    test_coord_layer(sol_selected_command, stud_selected_command, feedback, coord_fail_msg, exact_coord, student_env, solution_env)
  }
  
  if (!is.null(check_extra)) {
    # Check extra layers
    for (i in 1:length(check_extra)) {
      extra <- check_extra[i]
      
      fail_msg <- try(extra_fail_msg[[i]], silent = TRUE)
      if (inherits(fail_msg, "try-error")) {
        fail_msg <- NULL
      }
      
      exact <- try(exact_extra[i], silent = TRUE)
      if (inherits(exact, "try-error")) {
        exact <- FALSE
      } else if (!is.logical(exact)) {
        exact <- FALSE
      }
      
      test_generic_part(type = extra, sol_selected_command, stud_selected_command, feedback, fail_msg, exact,
                        student_env, solution_env)
    }
  }
}

test_ggplot <- function(index = 1,
                        all_fail_msg = NULL,
                        check_data = TRUE, data_fail_msg = NULL,
                        check_aes = TRUE, aes_fail_msg = NULL, exact_aes = FALSE,
                        check_geom = TRUE, geom_fail_msg = NULL, exact_geom = FALSE, check_geom_params = NULL,
                        check_facet = TRUE, facet_fail_msg = NULL,
                        check_scale = TRUE, scale_fail_msg = NULL, exact_scale = FALSE,
                        check_coord = TRUE, coord_fail_msg = NULL, exact_coord = FALSE,
                        check_stat = TRUE, stat_fail_msg = NULL, exact_stat = FALSE,
                        check_extra = NULL, extra_fail_msg = NULL, exact_extra = NULL,
                        check = NULL) {
  ex() %>% check_ggplot(index = index,
                        all_fail_msg = all_fail_msg,
                        check_data = check_data, data_fail_msg = data_fail_msg,
                        check_aes = check_aes, aes_fail_msg = aes_fail_msg, exact_aes = exact_aes,
                        check_geom = check_geom, geom_fail_msg = geom_fail_msg, exact_geom = exact_geom, check_geom_params = check_geom_params,
                        check_facet = check_facet, facet_fail_msg = facet_fail_msg,
                        check_scale = check_scale, scale_fail_msg = scale_fail_msg, exact_scale = exact_scale,
                        check_coord = check_coord, coord_fail_msg = coord_fail_msg, exact_coord = exact_coord,
                        check_stat = check_stat, stat_fail_msg = stat_fail_msg, exact_stat = exact_stat,
                        check_extra = check_extra, extra_fail_msg = extra_fail_msg, exact_extra = exact_extra,
                        check = check)
}

test_data_layer <- function(sol_data, stud_data, feedback, data_fail_msg) {
  if (!is.null(data_fail_msg)) {
    feedback_msg <- data_fail_msg
  } else {
    feedback_msg <- paste(feedback, "you didn't get the data layer right.")
  }
  
  check_that(is_equal(sol_data$base, stud_data$base), feedback = feedback_msg)
}

test_aes_layer <- function(sol_mapping, stud_mapping, feedback, aes_fail_msg, exact_aes) {
  for (map in names(sol_mapping$base)) {
    if (!is.null(aes_fail_msg)) {
      feedback_msg <- rep_len(aes_fail_msg, 3)
    } else {
      feedback_msg <- c(paste0(feedback, " have you mapped something on the `", map, "` aesthetic?"),
                        paste0(feedback, " have you mapped `", sol_mapping$base[map] ,"` on the `", map, "` aesthetic? Instead, you got `", stud_mapping$base[map], "`."),
                        paste0(feedback, " have you mapped exactly what is asked on the aesthetics layer, no more and no less?"))
    }
    
    check_that(is_false(is.null(stud_mapping$base[map][[1]])), feedback = feedback_msg[1])
    check_that(is_equal(stud_mapping$base[map], sol_mapping$base[map]), feedback = feedback_msg[2])
    if (exact_aes) {
      check_that(is_equal(length(stud_mapping), length(sol_mapping)), feedback = feedback_msg[3])
    }
  }
}

#' @importFrom stats na.omit
test_geom_layer <- function(sol_command, stud_command, sol_layers, stud_layers, feedback, geom_fail_msg, exact_geom, check_geom_params) {
  nb_sol_layers <- length(sol_layers)
  
  exact_geom <- rep_len(exact_geom, nb_sol_layers)
  
  sol_geom_parts <- extract_parts(sol_command, "stat|geom_")
  stud_geom_parts <- extract_parts(stud_command, "stat|geom_")
  
  if (!(nb_sol_layers > 0)) {
    return()
  }
  
  if (!is.null(geom_fail_msg)) {
    geom_fail_msg <- rep_len(geom_fail_msg, 5)
  }
  
  for (i in 1:nb_sol_layers) {
    sol_layer <- sol_layers[[i]]
    sol_geom_part <- sol_geom_parts[[i]]
    
    found_geom_name <- FALSE
    found_geom_with_params <- FALSE
    found_geom_with_exact_params <- FALSE
    found_geom_with_correct_position <- FALSE
    
    sol_params <- get_geom_params(sol_layer)
    if (!is.null(check_geom_params)) {
      sol_params <- sol_params[check_geom_params]
      sol_params <- sol_params[na.omit(names(sol_params))]
    }
    
    sol_position <- extract_type_from_object(sol_layer$position)
    
    
    nb_stud_layers <- length(stud_layers)
    if (nb_stud_layers > 0) {
      for (j in 1:nb_stud_layers) {
        stud_layer <- stud_layers[[j]]
        stud_geom_part <- stud_geom_parts[[j]]
        
        sol_geom_type <- extract_type_from_object(sol_layer$geom)
        stud_geom_type <- extract_type_from_object(stud_layer$geom)
        if (sol_geom_type == stud_geom_type) {
          found_geom_name <- TRUE
          found_params <- TRUE
          
          stud_params <- get_geom_params(stud_layer)
          if (!is.null(check_geom_params)) {
            stud_params <- stud_params[check_geom_params]
            stud_params <- stud_params[na.omit(names(stud_params))]
          }
          
          stud_position <- extract_type_from_object(stud_layer$position)
          
          for (sol_param in names(sol_params)) {
            if (!(sol_param %in% names(stud_params))) {
              found_params <- FALSE
              break
            } else {
              sol_value <- sol_params[[sol_param]]
              stud_value <- stud_params[[sol_param]]
              
              if (!is_equal(sol_value, stud_value)) {
                found_params <- FALSE
                break
              }
            }
          }
          
          if (found_params) {
            found_geom_with_params <- TRUE
          }
          
          if (found_geom_with_params && (!exact_geom[i] || length(sol_params) == length(stud_params))) {
            found_geom_with_exact_params <- TRUE
          }
          
          if (found_geom_with_exact_params && compare_positions(sol_layer, stud_layer)) {
            found_geom_with_correct_position <- TRUE
          }
          
          if (found_geom_with_correct_position) {
            stud_layers[[j]] <- NULL
            stud_geom_parts[[j]] <- NULL
            break
          }
        }
        
      }
    }
    
    if (!is.null(geom_fail_msg)) {
      feedback_msg <- geom_fail_msg
    } else {
      geom_base_feedback <- paste0(feedback, " have you correctly added a `", as.character(sol_geom_part[[1]]),"()` layer")
      if (!is.null(check_geom_params)) {
        filtered_geom_params <- names(sol_params)
      } else {
        filtered_geom_params <- names(filter_standard_geom_params(as.character(sol_geom_part[[1]]), sol_params))
      }
      param_strings <- vapply(filtered_geom_params, 
                              function(x) {
                                gen_fb <- ""
                                if (isTRUE(attr(sol_params[[x]], "aes"))) {
                                  attr(sol_params[[x]], "aes") <- NULL
                                  gen_fb <- "aesthetic "
                                }
                                return(paste0(gen_fb,"`", x, "` set to `", paste0(deparse(sol_params[[x]]), collapse = " "), "`"))
                              }, character(1))
      nb_param_strings <- length(param_strings)
      if (nb_param_strings > 1) {
        param_feedback <- paste0(paste(param_strings[1:(nb_param_strings - 1)], collapse = ", "), " and ", param_strings[nb_param_strings])
      } else {
        param_feedback <- param_strings
      }
      feedback_msg <- c(paste0(geom_base_feedback, " with a `+` operator?"),
                        paste0(geom_base_feedback, " with ", param_feedback, "?"),
                        paste0(geom_base_feedback, " with ", param_feedback, "?", " It seems like you have defined too much attributes or aesthetics for this geom."),
                        paste0(geom_base_feedback, " with the `position` set correctly? Have another look at the instructions."))
      
    }
    
    check_that(is_true(found_geom_name), feedback = feedback_msg[1])
    check_that(is_true(found_geom_with_params), feedback = feedback_msg[2])
    check_that(is_true(found_geom_with_exact_params), feedback = feedback_msg[3])
    check_that(is_true(found_geom_with_correct_position), feedback = feedback_msg[4])
  }
  
  if (isTRUE(exact_geom)) {
    if (!is.null(geom_fail_msg)) {
      feedback_msg <- geom_fail_msg[5]
    } else {
      feedback_msg <- paste0(feedback, " have you added only the geom layers that are asked for? Nothing more.")
    }
    check_that(is_equal(length(stud_layers), 0), feedback = feedback_msg)
  }
}

test_facet_layer <- function(sol_facet, stud_facet, feedback, facet_fail_msg) {

  sol_type <- class(sol_facet)[1]
  stud_type <- class(stud_facet)[1]

  if (sol_type == "FacetGrid") {

    sol_cols <- names(sol_facet$params$cols)
    sol_rows <- names(sol_facet$params$rows)

    if (!is.null(facet_fail_msg)) {
      feedback_msg <- rep_len(facet_fail_msg, 3)
    } else {
      form_left <- ifelse(length(sol_rows > 0), paste(sol_rows, collapse = "+"), ".")
      form_right <- ifelse(length(sol_cols > 0), paste(sol_cols, collapse = "+"), ".")
      form_facet <- paste0(form_left, " ~ ", form_right)
      feedback_incorrect <- paste0(feedback, " did you set the correct formula for the facet: `", form_facet, "`?")
      feedback_msg <- c(paste0(feedback, " did you add the correct facet, `facet_", sol_type, "()`, using the `+` operator?"),
                        feedback_incorrect,
                        feedback_incorrect)
    }

    check_that(is_equal(sol_type, stud_type), feedback = feedback_msg[1])

    stud_cols <- names(stud_facet$params$cols)
    check_that(is_gte(length(base::intersect(sol_cols, stud_cols)), length(sol_cols)), feedback = feedback_msg[2])

    stud_rows <- names(stud_facet$params$rows)
    check_that(is_gte(length(base::intersect(sol_rows, stud_rows)), length(sol_rows)), feedback = feedback_msg[3])
  }

  if (sol_type == "FacetWrap") {
    
    sol_facets <- names(sol_facet$params$facets)

    if (!is.null(facet_fail_msg)) {
      feedback_msg <- rep_len(facet_fail_msg, 2)
    } else {
      feedback_msg <- c(paste0(feedback, " did you add the correct facet, `facet_", sol_type, "()`, using the `+` operator?"),
                        paste0(feedback, " did you correctly specify the facets: `", paste(sol_facets, collapse = "+"), "`?"))
    }

    check_that(is_equal(sol_type, stud_type), feedback = feedback_msg[1])

    stud_facets <- names(stud_facet$params$facets)
    check_that(is_gte(length(base::intersect(sol_facets, stud_facets)), length(sol_facets)), feedback = feedback_msg[2])
  }
}

test_scale_layer <- function(sol_command, stud_command, feedback, scale_fail_msg, exact_scale,
                             student_env, solution_env) {
  test_generic_part(type = "scale_", sol_command, stud_command, feedback, scale_fail_msg, exact_scale,
                    student_env, solution_env)
}

test_coord_layer <- function(sol_command, stud_command, feedback, coord_fail_msg, exact_coord,
                             student_env, solution_env) {
  test_generic_part(type = "coord_", sol_command, stud_command, feedback, coord_fail_msg, exact_coord,
                    student_env, solution_env)
}

test_stat_layer <- function(sol_command, stud_command, feedback, stat_fail_msg, exact_stat,
                            student_env, solution_env) {
  test_generic_part(type = "stat_", sol_command, stud_command, feedback, stat_fail_msg, exact_stat,
                    student_env, solution_env)
}

test_generic_part <- function(type, sol_command, stud_command, feedback, fail_msg, exact,
                              student_env, solution_env) {

  sol_parts <- extract_parts(sol_command, type)
  stud_parts <- extract_parts(stud_command, type)
  
  nb_sol_parts <- length(sol_parts)
  
  if (!(nb_sol_parts > 0)) {
    return()
  }
  
  exact <- rep_len(exact, nb_sol_parts)
  
  for (i in 1:nb_sol_parts) {
    sol_part <- sol_parts[[i]]
    if (is.call(sol_part)) {
      sol_func_name <- sol_part[[1]]
    } else {
      sol_func_name <- sol_part
    }
    
    found_name <- FALSE
    found_with_params <- FALSE
    found_with_exact_params <- FALSE
    
    sol_params <- extract_params(sol_part)
    
    nb_stud_parts <- length(stud_parts)
    if (nb_stud_parts > 0) {
      for (j in 1:nb_stud_parts) {
        stud_part <- stud_parts[[j]]
        if (is.call(stud_part)) {
          stud_func_name <- stud_part[[1]]
        } else {
          stud_func_name <- stud_part
        }
        
        if (map_synonyms(stud_func_name) == map_synonyms(sol_func_name)) {
          found_name <- TRUE
          found_params <- TRUE
          
          stud_params <- extract_params(stud_part)
          
          for (sol_param in names(sol_params)) {
            if (!(sol_param %in% names(stud_params))) {
              found_params <- FALSE
              break
            } else {
              sol_value <- sol_params[[sol_param]]
              stud_value <- stud_params[[sol_param]]
              
              eval_sol <- without_args(eval(sol_value, envir = solution_env))
              eval_stud <- without_args(try(eval(stud_value, envir = student_env), silent = TRUE))
              if (inherits(eval_stud, "try-error") ||
                  !is_equal(eval_sol, eval_stud)) {
                found_params <- FALSE
                break
              }
            }
          }
          
          if (found_params) {
            found_with_params <- TRUE
          }
          
          if (found_with_params && (!exact[i] || length(sol_params) == length(stud_params))) {
            found_with_exact_params <- TRUE
          }
          
          if (found_with_exact_params) {
            stud_parts[[j]] <- NULL
            break
          }
        }
        
      }
    }
    
    if (!is.null(fail_msg)) {
      feedback_msg <- rep_len(fail_msg, 3)
    } else {
      base_feedback <- paste0(feedback, " have you correctly added a `", sol_func_name,"()` layer")
      param_strings <- vapply(names(sol_params), 
                              function(x) {
                                if (isTRUE(attr(sol_params[[x]], "dot"))) {
                                  attr(sol_params[[x]], "dot") <- NULL
                                  return(paste0("the ", x, " set to `", deparse(sol_params[[x]]), "`")) 
                                } else {
                                  return(paste0("`", x, "` set to `", deparse(sol_params[[x]]), "`"))
                                }
                              }, character(1))
      nb_param_strings <- length(param_strings)
      if (nb_param_strings > 1) {
        param_feedback <- paste0(paste(param_strings[1:(nb_param_strings - 1)], collapse = ", "), " and ", param_strings[nb_param_strings])
      } else {
        param_feedback <- param_strings
      }
      feedback_msg <- c(paste0(base_feedback, " with a `+` operator?"),
                        paste0(base_feedback, " with ", param_feedback, "?"),
                        paste0(base_feedback, " with ", param_feedback, "?", " It seems like you have defined too much attributes."))
      
    }
    
    check_that(is_true(found_name), feedback = feedback_msg[1])
    check_that(is_true(found_with_params), feedback = feedback_msg[2])
    check_that(is_true(found_with_exact_params), feedback = feedback_msg[3])
  }
}

without_args <- function(x) {
  copy <- x
  for (a in names(attributes(copy))) {
    attr(copy, a) <- NULL
  }
  return(copy)
}

nd <- function(number) {
  switch(number, "1" = "first", "2" = "second", "3" = "third", 
                 "4" = "fourth", "5" = "fifth", "6" = "sixth", 
                 "7" = "seventh", "8" = "eighth", "9" = "ninth", 
                 "10" = "tenth")
}

#' @importFrom utils argsAnywhere
extract_params <- function(command) {
  if (!is.call(command)) {
    return(NULL)
  }
  func_def <- try(argsAnywhere(as.character(command[[1]])), silent = TRUE)
  if (!inherits(func_def, "try-error")) {
    param_list <- as.list(match.call(func_def, command))[-1]
    if (!length(param_list) > 0) {
      return(NULL)
    }
    param_names <- names(param_list)
    if (is.null(param_names)) {
      for (i in 1:length(param_list)) {
        attr(param_list[[i]], "dot") <- TRUE
        param_names[i] <- paste(nd(i), "argument")
      }
    } else {
      for (i in 1:length(param_names)) {
        if (is_equal(param_names[i], "")) {
          attr(param_list[[i]], "dot") <- TRUE
          param_names[i] <- paste(nd(i), "argument")
        }
      }
    }
    names(param_list) <- param_names
    return(param_list)
  } else {
    return(NULL)
  }
}

extract_parts <- function(command, type) {
  if (is.name(command)) {
    if (grepl(paste0("^", type), command)) {
      return(list(command))
    } else {
      return(list())
    }
  } else if (command[[1]] == "+") {
    return(c(extract_parts(command[[2]], type), extract_parts(command[[3]], type)))
  } else if (is.call(command)) {
    if (grepl(paste0("^", type), command[[1]])) {
      return(list(command))
    } else {
      return(list())
    }
  } else {
    return(list())
  }
}

extract_type_from_object <- function(object) {
  return(sub("^_", "", tolower(gsub("([A-Z])", "_\\1", class(object)[1]))))
}

compare_positions <- function(sol_layer, stud_layer) {
   sol_position <- sol_layer$position
   stud_position <- stud_layer$position

   return(is_equal(sol_position, stud_position))
}

almost_equal <- function(value1, value2) {
  if (identical(value1, value2)) {
    return(TRUE)
  } else if (is.numeric(value1) && is.numeric(value2)) {
    return(abs(value1 - value2) <= 1e-5)
  } else {
    return(FALSE)
  }
}

get_geom_params <- function(geom_layer) {
  params <- geom_layer$geom_params
  stat_params <- geom_layer$stat_params
  params[names(stat_params)] <- stat_params
  mapping_params <- lapply(geom_layer$mapping, function(x) structure(x, aes = TRUE))
  params[names(mapping_params)] <- mapping_params
  aes_params <- geom_layer$aes_params
  params[names(aes_params)] <- aes_params
  return(params)
}

filter_standard_geom_params <- function(geom_call, params) {
  standard_layer <- eval(call(geom_call))
  standard_params <- get_geom_params(standard_layer)
  ov <- base::intersect(names(params), names(standard_params))
  eq <- mapply(is_equal, standard_params[ov], params[ov])
  if (any(eq)) {
    params[names(eq[eq])] <- NULL
  } 
  return(params)
}

get_ggplot_solution_info <- function(code, predefined_code, envir) { 
  saved_solution_code <- tw$get("saved_solution_code")
  if (is.null(saved_solution_code)) saved_solution_code <- ""
  saved_solution_ggplot_info <- tw$get("saved_solution_ggplot_info")
  
  if (code != saved_solution_code || is.null(saved_solution_ggplot_info)) {
    ggplot_info <- get_ggplot_info(code, predefined_code, envir)
    tw$set(saved_solution_code = code)
    tw$set(saved_solution_ggplot_info = ggplot_info)
  }
  return(tw$get("saved_solution_ggplot_info"))
}

get_ggplot_student_info <- function(code, predefined_code, envir) {
  saved_student_code <- tw$get("saved_student_code")
  if (is.null(saved_student_code)) saved_student_code <- ""
  saved_student_ggplot_info <- tw$get("saved_student_ggplot_info")
  
  if (code != saved_student_code || is.null(saved_student_ggplot_info)) {
    ggplot_info <- get_ggplot_info(code, predefined_code, envir)
    tw$set(saved_student_code = code)
    tw$set(saved_student_ggplot_info = ggplot_info)
  }
  return(tw$get("saved_student_ggplot_info"))
}

get_ggplot_info <- function(code, predefined_code, envir) { 
  ggplot_env <- new.env()
  commands <- get_ggplot_commands(code, predefined_code, ggplot_env)
  return(list(commands = commands,
              objects = lapply(commands, function(x) try(eval(x, envir), silent = TRUE))))
}

get_ggplot_commands <- function(code, predefined_code, envir) {
  pre_parsed <- try(parse(text = predefined_code), silent = TRUE)
  parsed <- try(parse(text = code), silent = TRUE)
  
  if (inherits(pre_parsed, "try-error") || inherits(parsed, "try-error")) {
    return(list())
  }
  
  lapply(pre_parsed, extract_ggplot_command, envir = envir)
  
  extracted <- lapply(parsed, extract_ggplot_command, envir = envir)
  return(extracted[!as.logical(vapply(extracted, is.null, logical(1)))])
}

extract_ggplot_command <- function(code, envir) {
  if (any(grepl("%>%", deparse(code), fixed = TRUE))) {
    code <- unpipe(code)
  }

  if (is_ggplot_command(code, envir)) {
    return(replace_saved_ggplot_commands(code, envir))
  } else if (is.name(code)) {
    return(NULL)
  } else if (code[[1]] == "=" || code[[1]] == "<-") {
    code_is_ggplot_command <- is_ggplot_command(code[[3]], envir)
    if (code_is_ggplot_command || is_geom_command(code[[3]], envir)) {
      extract_def <- replace_saved_ggplot_commands(code[[3]], envir)
      assign(as.character(code[[2]]), extract_def, envir = envir)
      if (isTRUE(code_is_ggplot_command)) {
        return(extract_def)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

replace_saved_ggplot_commands <- function(code, envir) {
  len <- length(code)
  if (len > 1) {
    for (i in 1:len) {
      if (length(code[[i]]) > 1 && code[[i]][[1]] == "+") {
        code[[i]] = replace_saved_ggplot_commands(code[[i]], envir)
      } else if (exists(as.character(code[[i]]), envir = envir, inherits = FALSE)) {
        code[[i]] = get(as.character(code[[i]]), envir = envir, inherits = FALSE)  
      }
    }
  } else {
    if (exists(as.character(code), envir = envir, inherits = FALSE)) {
      code =  get(as.character(code), envir = envir, inherits = FALSE) 
    }
  }
  return(code)
}

is_ggplot_command <- function(code, envir) {
  if (is.null(code)) {
    return(FALSE)
  } else if (is.name(code)) {
    get_command <- try(get(as.character(code), envir = envir, inherits = FALSE))
    return(ifelse(inherits(get_command, "try-error"), FALSE, is_ggplot_command(get_command, envir)))
  } else if (code[[1]] == "ggplot") {
    return(TRUE)
  } else if (code[[1]] == "+") {
    return(is_ggplot_command(code[[2]], envir))
  } else {
    return(FALSE)
  }
}

is_geom_command <- function(code, envir) {
  if (is.name(code)) {
    get_command <- try(get(as.character(code), envir = envir, inherits = FALSE))
    return(ifelse(inherits(get_command, "try-error"), FALSE, is_geom_command(get_command, envir)))
  } else {
    return(isTRUE(grepl("^geom_", code[[1]])))
  }
}

map_synonyms <- function(fun) {
  gsub("scale_colour_", "scale_color_", fun, fixed = TRUE)
}
