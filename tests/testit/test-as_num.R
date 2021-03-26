testit::assert("Parse characters as number", {
  is.numeric(as.num("7"))
  as.num("7") == 7
})

testit::assert("Parse characters as number - decimal, class", {
  is.numeric(as.num("7.6"))
  as.num("7.6") == 7.6
})

testit::assert("Parse numeric as number - decimal",{
  is.numeric(as.num(16.23))
  as.num(16.23) == 16.23
})

testit::assert("Parse factor as number - class", {
  is.numeric(as.num(factor(17)))
  as.num(factor(17)) == 17
})

testit::assert("Parse NULL as number", {
  is.numeric(as.num(NULL))
  is.numeric(as.num(NA))
})

