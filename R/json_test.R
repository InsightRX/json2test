#' Wrapper function to run tests and compare with reference
#'
#' @param func linking to test/reference JSON and function in package to test
#' @param package package name from where to take tests.json object from
#' @param test_id specifies test/reference JSON to use, i.e. /home/user/R/3.3/package_name/test/<test_id>.json. If not specified, assumes `func` is used as `test_id`.
#' @param tests a vector of tests to run
#' @param skip a vector of tests to skip (default is to run all)
#' @param reference list of reference values
#' @param delta relative allowed imprecision, default is 0.03. Overrides value specified in reference JSON.
#' @param ignore_keys ignore specific keys in reference JSON, e.g. to allow for comments
#' @param run a specific test to run only, not check. The output object will be returned.
#' @param list_as_args should the list elements from the JSON be used as arguments to the function? TRUE by default. If FALSE, the list as a whole will be passed to the `args` argument of the function.
#' @param parse_functions list of functions to parse specific JSON tests data before calling the function. This is sometimes useful due to the back-serialization from JSON to R object.
#' @param overwrite optional list specifying what keys of test JSON to manually overwrite with given values
#' @export
json_test <- function(
  func = NULL,
  package = NULL,
  test_id = NULL,
  tests = NULL,
  reference = NULL,
  delta = NULL,
  run = NULL,
  skip = c(),
  list_as_args = TRUE,
  ignore_keys = c(),
  parse_functions = list(),
  overwrite = NULL) {
  if(!is.null(run)) {
    if(length(run) > 1) {
      stop("Sorry, only a result object for a single test can be returned.")
    }
    tests <- run
  }
  if(is.null(package)) {
    stop("please specify package to be tested")
  }
  ignore_keys <- unique(c(ignore_keys, c("comment", "comments", "skip")))
  if(!is.null(func)) { # then load from JSON
    if(is.null(test_id)) {
      test_id <- func
    }
    test_file <- system.file(paste0("test/", test_id, ".json"), package = package)
    if(!file.exists(test_file)) {
      stop(paste0("File ", test_file, " not found!"))
    } else {
      message(paste0("Reading tests in file ", test_file, "..."))
    }
    suppressWarnings({
      txt <- readr::read_file(test_file)
      sel_tests <- parse_arg(txt)
      if(!is.null(overwrite)) {
        for (t in seq(sel_tests)) {
          for(w in names(overwrite)) {
            sel_tests[[t]][[w]] <- overwrite[[w]]
          }
        }
      }
    })
    if(is.null(sel_tests) || length(sel_tests) == 0) {
       message("No tests were found.")
       return()
    }
    if(is.null(run)) {
      ref_file <- system.file(paste0("reference/", test_id, ".json"), package = package)
      if(!file.exists(ref_file)) {
        stop(paste0("No reference file found for this module: ", ref_file))
      }
      txt_ref <- readr::read_file(ref_file)
      all_refs <- parse_arg(txt_ref)
      reference <- all_refs
    }
  } else {
    stop("No function specified to test.")
  }
  fnc <- getExportedValue(package, func)
  if(!is.null(tests)) {
    sel_tests <- sel_tests[tests]
  }
  for(key in names(sel_tests)) {
    if((is.null(reference[[key]][["skip"]]) || reference[[key]][["skip"]] == FALSE) && !(key %in% skip)) {
      if(!is.null(run)) { # just return the output
        message(paste0("Returning output from ", func))
      } else {
        message(paste0("Testing ", func, "::", key))
      }
      obj <- parse_json_test(sel_tests[[key]], parse_functions)
      if(list_as_args) {
        do.call(what = "fnc", args = obj)
      } else {
        tmp <- fnc(args = obj)
      }
      if(!is.null(run)) { # just return the output
        return(tmp)
      } else { # run the actual tests
        if(class(tmp) != "list") {
          stop(paste0("No list returned: ", tmp))
        }
        do_checks <- TRUE
        if(!is.null(tmp$error) && tmp$error) {
          if(is.null(reference[[key]][["error"]])) { # if error is not actually expected
            message(paste0("Unexpected error:\n", paste(tmp, collapse="\n")))
            result <- FALSE
            do_checks <- FALSE
          }
        }
        if(!is.null(reference[[key]][["comment"]])) {
          message(paste0("Comment: ", reference[[key]][["comment"]]))
        }
        for(refkey in names(reference[[key]])) {
          if(!refkey %in% ignore_keys) {
            calc <- get_nested_value(tmp, refkey)
            ref  <- reference[[key]][[refkey]]
            if(!do_checks) { # fail all checks for this test
              sign <- "   x\t"
              message(paste0(sign, key, "::", refkey, " ( ? ", " == ", ref,")"))
              json2test::assert(test_id, paste0(key,": ", refkey), FALSE)
            } else {
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
                json2test::assert(test_id, paste0(key,": ", refkey, " (NA)"), ref_i == "NA")
                message(paste0(" - ", key, "::", refkey, " (NA)"))
              } else {
                result <- FALSE
                if(class(ref_i) == "character") {
                  result <- json2test::assert(test_id, paste0(key,": ", refkey), ref_i == calc)
                }
                if(class(ref_i) %in% c("numeric", "integer")) {
                  if(equal_i) {
                    result <- json2test::assert(test_id, paste0(key,": ", refkey), ref_i == calc)
                  } else {
                    result <- json2test::assert(test_id, paste0(key,": ", refkey), abs((ref_i - calc) / ref_i) < delta_i )
                  }
                }
                sign <- ifelse(!result, "   x\t", "   ✓\t")
                if(equal_i || class(ref) == "character") {
                  message(paste0(sign, key, "::", refkey, " (", calc , " == ", ref_i,")"))
                } else {
                  message(paste0(sign, key, "::", refkey, " (", calc , " == ", ref_i,", delta=", 100*delta_i,"%)"))
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
