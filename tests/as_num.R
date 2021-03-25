library(testit)

assert("Parse characters as number - class", is.numeric(json2test:::as.num("7")))
assert("Parse characters as number - value", json2test:::as.num("7") == 7)
assert("Parse characters as number - decimal, class", is.numeric(json2test:::as.num("7.6")))
assert("Parse characters as number - decimal, value", json2test:::as.num("7.6") == 7.6)

assert("Parse numeric as number - decimal, class", is.numeric(json2test:::as.num(16.23)))
assert("Parse numeric as number - decimal, value", json2test:::as.num(16.23) == 16.23)

assert("Parse factor as number - class", is.numeric(json2test:::as.num(factor(17))))
assert("Parse factor as number - value", json2test:::as.num(factor(17)) == 17)

assert("Parse NULL as number", is.numeric(json2test:::as.num(NULL)))
assert("Parse NA as number", is.numeric(json2test:::as.num(NA)))
