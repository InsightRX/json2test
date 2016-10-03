#' #' Wrapper function to run tests and compare with reference
#'
#' @param module linking to test object in tests.json
#' @param package package name from where to take tests.json object from
#' @param tests a vector of tests to run
#' @param skip a vector of tests to skip (default is to run all)
#' @param reference list of reference values
#' @param equal should answers be equal? if not, specify allowed relative `delta`
#' @param delta relative allowed imprecision, default is 0.03. Only used when not specified in reference JSON.
#' @param ignore_keys ignore specific keys in reference JSON, e.g. to allow for comments
#' @param run a specific test to run only, not check. The output object will be returned.
#' @export
json_test <- function(
  module = NULL,
  package = "insightrxr",
  tests = NULL,
  reference = NULL,
  equal = TRUE,
  delta = 0.03,
  run = NULL,
  skip = c(),
  ignore_keys = c("comment", "comments")) {
  if(!is.null(run)) {
    if(length(run) > 1) {
      stop("Sorry, only a result object for a single test can be returned.")
    }
    tests <- run
  }
  if(!is.null(module)) { # then load from JSON
    all_tests <- rjson::fromJSON(file = system.file(paste0("test/", module, ".json"), package = package))
    sel_tests <- all_tests
    testit::assert("No test object found!", !is.null(sel_tests))
    if(is.null(run)) {
      all_refs <- rjson::fromJSON(file = system.file(paste0("reference/", module, ".json"), package = "insightrxr"))
      reference <- all_refs
      testit::assert("No reference object found!", !is.null(reference))
    }
  } else {
    if(is.null(sel_tests)) {
      stop("No tests found in JSON")
    }
    if(is.null(reference)) {
      stop("Either `module` or `reference`-object should be specified")
    }
  }
  func <- getExportedValue('insightrxr', module)
  if(!is.null(tests)) {
    sel_tests <- sel_tests[tests]
  }
  for(key in names(sel_tests)) {
    ## reformat covs, as conseuquence of loading JSON, not a bug in code
    for(m in names(sel_tests[[key]]$covs)) {
      if(class(sel_tests[[key]]$covs[[m]]) == "list") {
        #        print(as.data.frame(sel_tests[[key]]$covs[[m]])))
        sel_tests[[key]]$covs[[m]] <- data.frame(sel_tests[[key]]$covs[[m]])
      }
    }
    if((is.null(reference[[key]][["skip"]]) || reference[[key]][["skip"]] == FALSE) && !(key %in% skip)) {
      if(!is.null(run)) { # just return the output
        message(paste0("Returning output from ", module))
      } else {
        message(paste0("Testing ", module, "::", key))
      }
      obj  <- parse_json_test(sel_tests[[key]])
      testit::assert("Test not found!", !is.null(obj))
      tmp  <- func(args = obj)
      if(!is.null(tmp$error) && tmp$error) {
        if(is.null(reference[[key]][["error"]])) {
          stop("Unexpected error")
        }
      }
      if(!is.null(run)) { # just return the output
        return(tmp)
      } else { # run the actual tests
        testit::assert("reference for test available", length(names(reference[[key]])) > 0)
        for(refkey in names(reference[[key]])) {
          if(!refkey %in% ignore_keys) {
            calc <- get_nested_value(tmp, refkey)
            ref  <- reference[[key]][[refkey]]
            if(class(ref) == "list" && !is.null(ref$value) && !is.null(ref$delta)) {
              ref_i <- ref$value
              if(is.null(ref$delta)) {
                equal_i <- TRUE
              } else {
                equal_i <- FALSE
              }
              delta_i <- ref$delta
            } else {
              ref_i <- ref
              delta_i <- delta
              equal_i <- equal
            }
            if(is.null(calc)) {
              testit::assert("check element should be missing", ref_i == "NA")
              message(paste0(" - ", key, "::", refkey, " (NA)"))
            } else {
              if(equal_i || class(ref) == "character") {
                message(paste0(" - ", key, "::", refkey, " (", calc , " == ", ref_i,")"))
              } else {
                message(paste0(" - ", key, "::", refkey, " (", calc , " == ", ref_i,", delta=", 100*delta_i,"%)"))
              }
              if(class(ref_i) == "character") {
                testit::assert(refkey, ref_i == calc)
              }
              if(class(ref_i) %in% c("numeric", "integer")) {
                if(equal_i) {
                  testit::assert(ref_i == calc)
                } else {
                  testit::assert(refkey, abs((ref_i - calc) / ref_i) < delta_i )
                }
              }
            }
          }
        }
      }
    } else {
      message(paste0("Skipping ", module, "::", key))
    }
  }
}
