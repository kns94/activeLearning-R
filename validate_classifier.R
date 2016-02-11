validate_classifier <- function(classifier, posterior_prob = FALSE) {
  # Tests that the specified classifier is given in 'caret', is actually a
  # classifier, and provides posterior probabilities of class membership.
  if (missing(classifier) || is.null(classifier) || is.na(classifier)) {
    stop("A classifier must be specified")
  }
  caret_lookup <- try(modelLookup(classifier), silent = TRUE)
  if (inherits(caret_lookup, "try-error")) {
    return(FALSE)
  } else if (!any(caret_lookup$forClass)) {
    return(FALSE)
  }
  
  if (posterior_prob && !any(caret_lookup$probModel)) {
    return(FALSE)
  }
  invisible(TRUE)
  return(TRUE)
}