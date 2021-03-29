#' Wrapper function to run tests and compare with reference
#'
#' @param func linking to test/reference JSON and function in package to test
#' @param package package name from where to take tests.json object from
#' @param test_id specifies test/reference JSON to use, i.e. /home/user/R/3.3/package_name/test/<test_id>.json. If not specified, assumes `func` is used as `test_id`.
#' @param tests a vector of tests to run
#' @param skip a vector of tests to skip (default is to run all)
#' @param force a vector of test to override potential `skip`.
#' @param reference list of reference values
#' @param delta relative allowed imprecision, default is 0.03. Overrides value specified in reference JSON.
#' @param ignore_keys ignore specific keys in reference JSON, e.g. to allow for comments
#' @param run a specific test to run only, no actual check. The output object will be returned.
#' @param list_as_args should the list elements from the JSON be used as arguments to the function? TRUE by default. If FALSE, the list as a whole will be passed to the `args` argument of the function.
#' @param parse_functions list of functions to parse specific JSON tests data before calling the function. This is sometimes useful due to the back-serialization from JSON to R object.
#' @param overwrite optional list specifying what keys of test JSON to manually overwrite with given values
#' @param fail_if_not_exists fail if folder with JSON does not exist?
#' @param lib which test library to use (testit or testthat)
#' @param verbose verbosity
#' @export
json_test <- function(
  func = NULL,
  package = NULL,
  test_id = NULL,
  tests = NULL,
  reference = NULL,
  delta = NULL,
  max_time = NULL,
  run = NULL,
  skip = c(),
  force = c(),
  list_as_args = TRUE,
  ignore_keys = c(),
  parse_functions = list(),
  overwrite = NULL,
  fail_if_not_exists = TRUE,
  lib = "testit",
  verbose = FALSE) {

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
    test_dir <- system.file(paste0("test/", test_id), package=package)
    if(verbose) message(paste0("Looking in folder ", test_dir))
    if(!file.exists(test_dir)) {
      msg <- paste0("\nWarning: test folder for ", test_id, " not found!")
      if(!fail_if_not_exists) {
        message(msg)
        return()
      } else {
        stop(msg)
      }
    } else {
      last_folder <- tail(strsplit(test_dir, "[/\\]")[[1]],1)
      message(paste0("\nReading test(s) for ", last_folder, "..."))
    }
    sel_tests <- list()
    d <- system.file(paste0("test/", test_id), package=package)
    test_files <- stringr::str_replace_all(
      dir(d), "\\.json", "")
    if(verbose) message(paste0("Found tests: ", paste(test_files, collapse = ", ")))
    suppressWarnings({
      for(q in seq(test_files)) {
        txt <- readr::read_file(paste0(d, "/", test_files[q], ".json"))
        tmp <- parse_arg(txt)
        sel_tests[[test_files[q]]] <- tmp
        if(!is.null(overwrite)) {
          for (t in seq(sel_tests)) {
            for(w in names(overwrite)) {
              sel_tests[[t]][[w]] <- overwrite[[w]]
            }
          }
        }
      }
    })
    if(is.null(sel_tests) || length(sel_tests) == 0) {
       message("No tests were found.")
       return()
    }
    if(is.null(run)) {
      ref_dir <- system.file(paste0("reference/", test_id), package = package)
      sel_ref <- list()
      d <- system.file(paste0("reference/", test_id), package=package)
      ref_files <- stringr::str_replace_all(
        dir(d), "\\.json", "")
      # ref_file <- system.file(paste0("reference/", test_id, ".json"), package = package)
      for(q in seq(ref_files)) {
        if(!file.exists(paste0(d, "/", ref_files[q], ".json"))) {
          stop(paste0("Reference file missing for this test: ", ref_files[q]))
        }
        txt_ref <- readr::read_file(paste0(d, "/", ref_files[q], ".json"))
        all_refs <- parse_arg(txt_ref)
        reference[[ref_files[q]]] <- all_refs
      }
    }
  } else {
    stop("No function specified to test.")
  }
  fnc <- ifelse(is.function(func), func, getExportedValue(package, func))
  func <- ifelse(is.function(func), deparse(match.call()$func), func)
  if(!is.null(tests)) {
    sel_tests <- sel_tests[tests]
  }
  for(key in names(sel_tests)) {
    if(verbose) message(paste0("Running test: ", key))
    if(((is.null(reference[[key]][["skip"]]) || reference[[key]][["skip"]] == FALSE) || key %in% force) && !(key %in% skip)) {
      if(!is.null(run)) { # just return the output
        message(paste0("   Returning output from ", func))
      } else {
        message(paste0("\n=== Testing ", func, "::", key, " ", paste(rep("=", 10), collapse="")))
      }
      obj <- parse_json_test(sel_tests[[key]], parse_functions)
      time_a <- Sys.time()
      if(list_as_args) {
        tmp <- do.call(what = "fnc", args = obj)
      } else {
        tmp <- fnc(args = obj)
      }
      time_b <- Sys.time()
      time <- round(as.numeric(time_b - time_a), 2)
      if(!is.null(max_time)) {
        if(lib == "testthat") {
          testthat::expect(time < max_time, paste0("Time for ", test_id, " < ", max_time, " seconds"))
        } else {
          json2test::assert(test_id,  paste0("Time for ", key, " < ", max_time), time < max_time, time = time)
        }
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
            message(paste0("    Unexpected error:\n", paste(tmp, collapse="\n")))
            result <- FALSE
            do_checks <- FALSE
          } else {
            reference[[key]][["error"]] <- NULL
          }
        }
        if(!is.null(reference[[key]][["comment"]])) {
          message(paste0("  Comment: ", reference[[key]][["comment"]]))
        }
        for(refkey in names(reference[[key]])) {
          if(!refkey %in% ignore_keys) {
            calc <- get_nested_value(tmp, refkey)
            ref  <- reference[[key]][[refkey]]
            if(!do_checks) { # fail all checks for this test
              sign <- "  [ ]\t"
              message(paste0(sign, key, "::", refkey, " ( ? ", " == ", ref,")", collapse="\n"))
              if(lib == "testthat") {
                testthat::expect(FALSE, paste0(test_id, " : ", key, " / ", refkey))
              } else {
                json2test::assert(test_id, paste0(key,": ", refkey), FALSE, time = time)
              }
            } else {
              equal_i <- TRUE
              ref_i <- ref
              if(class(ref) == "list" && !is.null(ref$value)) {
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
                if(lib == "testthat") {
                  ref_i == "NA"
                  testthat::expect(ref_i == "NA", paste0(test_id, " : ", key, " / ", refkey))
                } else {
                  json2test::assert(test_id, paste0(key,": ", refkey, " (NA)"), ref_i == "NA", time = time)
                }
                message(paste0(ifelse(ref_i == "NA", "  [N]\t", "  [ ]\t"), key, "::", refkey, " (NA)"))
              } else {
                result <- FALSE
                if(class(ref_i) == "character") {
                  if(ref_i == "$exists") {
                    result <- !is.null(calc) && !is.na(calc) && length(calc) > 0
                  } else if (ref_i == "$na") {
                    result <- is.na(calc)
                  } else if (ref_i == "$null") {
                    result <- is.null(calc)
                  } else {
                    result <- ref_i == calc
                  }
                  if(lib == "testthat") {
                    testthat::expect(ref_i == calc, paste0(test_id, " : ", key, " / ", refkey))
                  } else {
                    result <- json2test::assert(test_id, paste0(key,": ", refkey), result, time = time)
                  }
                }
                if(class(ref_i) %in% c("numeric", "integer", "logical")) {
                  if(equal_i || class(ref_i) == "logical") {
                    if(lib == "testthat") {
                      result <- ref_i == calc
                      testthat::expect(ref_i == calc, paste0(test_id, " : ", key, " / ", refkey))
                    } else {
                      result <- json2test::assert(test_id, paste0(key,": ", refkey), ref_i == calc, time = time)
                    }
                  } else {
                    if(lib == "testthat") {
                      result <- abs((ref_i - calc) / ref_i) < delta_i
                      testthat::expect(abs((ref_i - calc) / ref_i) < delta_i, paste0(test_id, " : ", key, " / ", refkey))
                    } else {
                      result <- json2test::assert(test_id, paste0(key,": ", refkey), abs((ref_i - calc) / ref_i) < delta_i, time = time)
                    }
                  }
                }
                sign <- ifelse(!result, "  [ ]\t", "  [\u2713]\t")
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
  names(test_result_collector) <- c("func", "fact", "result", "time")
}
