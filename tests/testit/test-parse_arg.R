testit::assert("Strips input text of special chars and returns list", {
   input1 <- "{\"app_type\":\"patient_platform\",\"type\":\"advice\",\"module\":\"vancomycin_adults\",\"isEMR\":null,\"test_api\":true}\n"
   output1 <- parse_arg(input1)
   length(output1) == 5
   class(output1) == "list"
   is.null(output1$isEMR)
   is.logical(output1$test_api)
   is.character(output1$type)
})

testit::assert("Handles recursive lists", {
   input2 <- "{\n\t\"covs\": {\n\t\t\"height\": [\n\t\t\t{\n\t\t\t\t\"value\": 180,\n\t\t\t\t\"emrId\": null,
      \n\t\t\t\t\"emrFlagged\": null,\n\t\t\t\t\"datetime\": \"2019-05-29T18:56:43.065Z\"\n\t\t\t},
      \n\t\t\t{\n\t\t\t\t\"value\": 185,\n\t\t\t\t\"emrId\": null,\n\t\t\t\t\"emrFlagged\": null,
      \n\t\t\t\t\"datetime\": \"2019-05-30T18:56:43.065Z\"\n\t\t\t}\n\t\t]\n\t},\n\t\"hemodialysis\": [\n\n\t]\n}\n"
   output2 <- parse_arg(input2)
   class(output2) == "list"
   length(output2) == 2
   length(output2$covs) == 1
   length(output2$covs$height) == 4
   hts <- output2$covs$height
   class(hts) == "data.frame"
   hts$value[2] == 185
   is.na(hts$emrId[1])
})

testit::assert("Handles empty json", {
   class(parse_arg("{}")) == "list"
})

testit::has_error(parse_arg("{"), silent = TRUE)
