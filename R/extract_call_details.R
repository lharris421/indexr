extract_call_details <- function(obj) {
  if (is.null(obj$call)) {
    stop("The object does not have a 'call' element.")
  }

  call_obj <- obj$call
  function_name <- as.character(call_obj[[1]])
  args <- as.list(call_obj)[-1]

  result <- list(function_name = function_name)


  for (arg_name in names(args)) {

    arg_value <- args[[arg_name]]
    arg_as_char <- as.character(arg_value)

    if (length(arg_as_char) == 0) next

    # Check if the argument is a call to 'list'
    if (startsWith(arg_as_char[1], "list")) {

      # Evaluate the arg_value directly
      list_to_save <- eval(arg_value)

      # Save the evaluated list to the result
      if (length(list_to_save) == 0) next
      result[[arg_name]] <- list_to_save

    } else if (length(arg_as_char) == 1 && (is.function(arg_value) || exists(arg_as_char, mode = "function", envir = parent.frame()))) {
      result[[arg_name]] <- arg_as_char  # Assign the character representation directly
    } else {
      result[[arg_name]] <- arg_value
    }
  }

  return(result)
}
get_default_arguments <- function(input) {

  if (is.list(input) && !is.null(input$function_name)) {
    function_name <- input$function_name
  } else if (!is.null(input$call)) {
    call_obj <- input$call
    function_name <- as.character(call_obj[[1]])
  } else {
    stop("Input must either be an object with a 'call' element or a list with a 'function_name' element.")
  }

  # Find the function in the environment
  func <- get(function_name, mode = "function")
  if (is.null(func)) {
    stop("Function not found in the environment.")
  }

  # Get default arguments and filter out those without defaults
  defaults <- formals(func)
  defaults <- Filter(function(x) !is.null(x), defaults)
  defaults <- lapply(defaults, convert_type)
  defaults <- Filter(function(x) x != "", defaults)
  defaults <- Filter(function(x) x != "list()", defaults)
  defaults <- Filter(function(x) !is.na(x), defaults)

  return(defaults)
}
combine_arguments_with_defaults <- function(user_args) {
  if (!is.list(user_args) || is.null(user_args$function_name)) {
    stop("The input must be a list containing a 'function_name' element.")
  }

  # Get default arguments for the specified function
  defaults <- get_default_arguments(user_args)

  # Combine the user arguments with the defaults, user values take precedence
  combined_args <- modifyList(defaults, user_args)

  return(combined_args)
}
if_vector <- function(x) {
  if (is.vector(x)) {
    return(x[1])
  } else {
    return(x)
  }
}

