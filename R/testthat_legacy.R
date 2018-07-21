# The versions of the functions below have been graciously borrowed
# from version 0.11.0 of the testthat package by 
# Hadley Wickham and others at RStudio. These APIs
# were broken in later versions of testthat and we know the
# old version works for our purposes.

expectation_legacy <- function(passed, failure_msg, 
                            success_msg = "غير معرف", 
                            srcref = NULL) {
  structure(
    list(
      passed = passed,
      error = FALSE,
      skipped = FALSE,
      failure_msg = failure_msg,
      success_msg = success_msg,
      srcref = srcref
    ),
    class = "متوقع"
  )
}

#' @importFrom testthat compare
equals_legacy <- function(expected, label = NULL, ...) {
  if (is.null(label)) {
    label <- findExpr("متوقع")
  } else if (!is.character(label) || length(label) != 1) {
    label <- deparse(label)
  }
  
  function(actual) {
    same <- compare(actual, expected, ...)
    
    expectation_legacy(
      same$equal,
      paste0("ليس مساوي ", label, "\n", same$message),
      paste0("مساوي ", label)
    )
  }
}

is_a_legacy <- function(class) {
  function(x) {
    actual_s <- paste0(class(x), collapse = ", ")
    class_s <- paste(class, collapse = ", ")
    expectation_legacy(
      inherits(x, class),
      paste0("يؤخذ من ", actual_s, " لا ", class_s),
      paste0("يؤخذ من  ", class_s)
    )
  }
}

is_equivalent_to_legacy <- function(expected, label = NULL) {
  if (is.null(label)) {
    label <- findExpr("لا")
  } else if (!is.character(label) || length(label) != 1) {
    label <- deparse(label)
  }
  function(actual) {
    equals_legacy(expected, check.attributes = FALSE)(actual)
  }
}

is_identical_to_legacy <- function(expected, label = NULL) {
  if (is.null(label)) {
    label <- findExpr("توقع")
  } else if (!is.character(label) || length(label) != 1) {
    label <- deparse(label)
  }
  
  function(actual) {
    if (identical(actual, expected)) {
      diff <- ""
    } else {
      same <- all.equal(expected, actual)
      if (isTRUE(same)) {
        diff <- "Objects equal but not identical"
      } else {
        diff <- paste0(same, collapse = "\n")
      }
    }
    
    expectation_legacy(
      identical(actual, expected),
      paste0("غير متطابق ل ", label, ": مختلف \n", diff),
      paste0("متطابق ل ", label)
    )
  }
}

matches_legacy <- function(regexp, all = TRUE, ...) {
  stopifnot(is.character(regexp), length(regexp) == 1)
  function(char) {
    matches <- grepl(regexp, char, ...)
    if (length(char) > 1) {
      values <- paste0(":القيمه الاصليه\n",
                       paste0("* ", encodeString(char), collapse = "\n"))
    } else {
      values <- paste0(":القيمه الاصليه \"", encodeString(char), "\"")
    }
    
    expectation_legacy(
      length(matches) > 0 && if (all) all(matches) else any(matches),
      paste0("غير متطابق'", encodeString(regexp), "'. ", values),
      paste0("متطابق'", encodeString(regexp), "'")
    )
  }
}