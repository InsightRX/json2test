a <- list(b = 3, c = list(d = "test"))
b <- data.frame(c1 = c(1,2,3), c2 = c(4,5,6))


## Test function calls
t1 <- list(
  b = 3,
  c = list(d = 5),
  e = list(f = c(1,2,3,6,9))
)

res <- c(
  json2test::assert("from list", json2test:::get_nested_value(a, "c.d") == "test"),
  json2test::assert("from data.frame", json2test:::get_nested_value(b, "c1.2") == 2),
  json2test::assert("function call works", json2test:::get_nested_value(t1, "e.f.length") == 5)
)
if(sum(res) != length(res)) {
  stop("Error in tests!")
} else {
  message("All tests passed")
}
