testit::assert("detects special reference keywords", {
  (is_special_reference("$null") &&
    is_special_reference("$class:data.frame") &&
    is_special_reference("$type:double") &&
    !is_special_reference("$class:") &&
    !is_special_reference("null") &&
    !is_special_reference(c("$null", "$na")) &&
    !is_special_reference(NA_character_) &&
    !is_special_reference(1))
})

testit::assert("$null / $not_null", {
  (check_special_value("$null", NULL) &&
    !check_special_value("$null", NA) &&
    check_special_value("$not_null", NA) &&
    !check_special_value("$not_null", NULL))
})

testit::assert("$na distinguishes NA from NaN and NULL", {
  (check_special_value("$na", NA) &&
    check_special_value("$na", NA_real_) &&
    check_special_value("$na", NA_character_) &&
    check_special_value("$na", c(NA, NA)) &&
    !check_special_value("$na", NaN) &&
    !check_special_value("$na", NULL) &&
    !check_special_value("$na", 1) &&
    !check_special_value("$na", list(NA)))
})

testit::assert("$nan", {
  (check_special_value("$nan", NaN) &&
    check_special_value("$nan", 0/0) &&
    !check_special_value("$nan", NA) &&
    !check_special_value("$nan", NA_real_) &&
    !check_special_value("$nan", NULL) &&
    !check_special_value("$nan", "NaN"))
})

testit::assert("$exists", {
  (check_special_value("$exists", 1) &&
    check_special_value("$exists", c(1, NA)) &&
    check_special_value("$exists", list(a = 1)) &&
    !check_special_value("$exists", NULL) &&
    !check_special_value("$exists", NA) &&
    !check_special_value("$exists", numeric(0)))
})

testit::assert("$class", {
  (check_special_value("$class:data.frame", data.frame(a = 1)) &&
    check_special_value("$class:numeric", 1) &&
    check_special_value("$class:list", list()) &&
    check_special_value("$class:integer|numeric", 1L) &&
    check_special_value("$class:NULL|numeric", 1) &&
    !check_special_value("$class:data.frame", list(a = 1)) &&
    !check_special_value("$class:character", 1))
})

testit::assert("$type", {
  (check_special_value("$type:double", 1) &&
    check_special_value("$type:integer", 1L) &&
    check_special_value("$type:NULL", NULL) &&
    !check_special_value("$type:integer", 1))
})

testit::assert("format_value", {
  (format_value(NULL) == "NULL" &&
    format_value(c(1, NA)) == "1, NA" &&
    format_value(data.frame(a = 1)) == "<data.frame>")
})
