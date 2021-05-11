# json2test

R library to allow tests (and reference values) for R packages to be specfied in JSON format. 

## Purpose

This library is primarily useful for functional and integration testing (but unit testing is of course possible too). Especially in the case where many different use-cases need to be tested and unit test do no fully cover the breadth of cases. Using JSON as the storage format, the module also allows for using dynamically generated of test/reference cases, or use of tests/references obtained from an external API.

## Workflow

For general use, in your R package create the `/inst` folder including the following subfolders:

```
/inst
  /test
  /reference
```

Inside the test folder, place JSON files containing the test arguments to be speficified for the functions you want to test. Make sure the JSON is valid, use e.g. [JSONlint](http://jsonlint.com/) to check. The JSON should be in the format of a named list of tests. The name of the JSON file should corresponds to the function to be tested, e.g. for `test/multiply.json`:

```
{
  "test_1": {
    "a" = 3.1415,
    "b" = 2.7183
  }
}
```

`reference/multiply.json` could e.g. look like this:

```
{
  "test_1": {
    "res" = 8.539539
  }
}
```

If the output of the function is a list or dataframe, you can access elements using dot-notation, e.g.:

```
{
  "test_1": {
    "my_result.my_list_inside.value" = 8.539539
  }
}
```

If the numeric value can vary somewhat, or you're not interested in decimals, the function allows for a `delta` to be speficied, which is the allowed relative deviance from the expected value:

```
{
  "test_1": {
    "res" = { "value": 8.5, "delta": 0.05 }
  }
}
```


