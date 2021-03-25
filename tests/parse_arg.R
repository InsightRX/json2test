library(testit)

input1 <- "{\"app_type\":\"patient_platform\",\"type\":\"advice\",\"module\":\"vancomycin_adults\",\"isEMR\":null,\"test_api\":true}\n"
output1 <- json2test:::parse_arg(input1)
assert("Strips input text of special chars and returns list",
       class(output1) == "list" && length(output1) == 5)
assert("List contains mixes of data types",
       is.null(output1$isEMR) && is.logical(output1$test_api) && is.character(output1$type))

input2 <- "{\n\t\"covs\": {\n\t\t\"height\": [\n\t\t\t{\n\t\t\t\t\"value\": 180,\n\t\t\t\t\"emrId\": null,
   \n\t\t\t\t\"emrFlagged\": null,\n\t\t\t\t\"datetime\": \"2019-05-29T18:56:43.065Z\"\n\t\t\t},
   \n\t\t\t{\n\t\t\t\t\"value\": 185,\n\t\t\t\t\"emrId\": null,\n\t\t\t\t\"emrFlagged\": null,
   \n\t\t\t\t\"datetime\": \"2019-05-30T18:56:43.065Z\"\n\t\t\t}\n\t\t]\n\t},\n\t\"hemodialysis\": [\n\n\t]\n}\n"
output2 <- json2test:::parse_arg(input2)
assert("Handles recursive lists",
       class(output2) == "list" && length(output2) == 2 && length(output2$covs) == 1 && length(output2$covs$height) == 4)
hts <- output2$covs$height
assert("Recursive list contains mixes of data types",
       class(hts) == "data.frame" && hts$value == c(180, 185) && is.na(hts$emrId[1]))

assert("Handles empty json", class(json2test:::parse_arg("{}")) == "list")
