library(json2test)

a <- list(b = 3, c = list(d = "test"))
b <- data.frame(c1 = c(1,2,3), c2 = c(4,5,6))

testit::assert("from list", json2test:::get_nested_value(a, "c.d") == "test")
testit::assert("from data.frame", json2test:::get_nested_value(b, "c1.2") == 2)
