#' Wrapper function to run tests and compare with reference
#'
#' @param func linking to test/reference JSON and function in package to test
#' @param package package name from where to take tests.json object from
#' @param tests a vector of tests to run
#' @param skip a vector of tests to skip (default is to run all)
#' @param reference list of reference values
#' @param delta relative allowed imprecision, default is 0.03. Overrides value specified in reference JSON.
#' @param ignore_keys ignore specific keys in reference JSON, e.g. to allow for comments
#' @param run a specific test to run only, not check. The output object will be returned.
#' @param list_as_args should the list elements from the JSON be used as arguments to the function? TRUE by default. If FALSE, the list as a whole will be passed to the `args` argument of the function.
#' @param parse_functions list of functions to parse specific JSON tests data before calling the function. This is sometimes useful due to the back-serialization from JSON to R object.
#' @export
json_test <- function(
  func = NULL,
  package = NULL,
  tests = NULL,
  reference = NULL,
  delta = NULL,
  run = NULL,
  skip = c(),
  list_as_args = TRUE,
  ignore_keys = c("comment", "comments"),
  parse_functions = list()) {
  if(!is.null(run)) {
    if(length(run) > 1) {
      stop("Sorry, only a result object for a single test can be returned.")
    }
    tests <- run
  }
  if(is.null(package)) {
    stop("please specify package to be tested")
  }
  if(!is.null(func)) { # then load from JSON
    sel_tests <- rjson::fromJSON(file = system.file(paste0("test/", func, ".json"), package = package))
    if(is.null(sel_tests) || length(sel_tests) == 0) {
       message("No tests were found.")
       return()
    }
    if(is.null(run)) {
      all_refs <- rjson::fromJSON(file = system.file(paste0("reference/", func, ".json"), package = package))
      reference <- all_refs
      testit::assert("No reference object found!", !is.null(reference))
    }
  } else {
    stop("No function specified to test.")
  }
  fnc <- getExportedValue(package, func)
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
        message(paste0("Returning output from ", func))
      } else {
        message(paste0("Testing ", func, "::", key))
      }
      obj <- parse_json_test(sel_tests[[key]], parse_functions)
      testit::assert("Test not found!", !is.null(obj))
      if(list_as_args) {
        do.call(what = "fnc", args = obj)
      } else {
        tmp  <- fnc(args = obj)
      }
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
            equal_i <- TRUE
            ref_i <- ref
            if(class(ref) == "list" && !is.null(ref$value) && !is.null(ref$delta)) {
              ref_i <- ref$value
              if(!is.null(ref$delta)) {
                equal_i <- FALSE
              }
              delta_i <- ref$delta
            }
            if(!is.null(delta)) {
              equal_i <- FALSE
              delta_i <- delta
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
      message(paste0("Skipping ", func, "::", key))
    }
  }
}
