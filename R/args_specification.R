args_specification <- function(e, ...)UseMethod("args_specification")

args_specification.default <- function(e, ...) {
  # in normal, interactive mode, do nothing
}

args_specification.test <- function(e, ...) {
  # Capture ... args
  targs <- list(...)
  # Check if appropriately named args exist
  if(is.null(targs$test_course) || is.null(targs$test_lesson)) {
    stop(s()%N%" 'test_course' و'test_lesson' يجب عليك ان تحدد 'test'mode حتى تعمل علي ")
  } else {
    # Make available for use in menu functions
    e$test_lesson <- targs$test_lesson
    e$test_course <- targs$test_course
  }
  # Check that 'from' is less than 'to' if they are both provided
  if(!is.null(targs$from) && !is.null(targs$to)) {
    if(targs$from >= targs$to) {
      stop(s()%N%" 'from'يجب ان تكون اكبر بشكل كبير من الحجه  'to' الحجه")
    }
  }
  if(is.null(targs$from)) {
    e$test_from <- 1
  } else {
    e$test_from <- targs$from
  }
  if(is.null(targs$to)) {
    e$test_to <- 999 # Lesson will end naturally before this
  } else {
    e$test_to <- targs$to
  }
} 