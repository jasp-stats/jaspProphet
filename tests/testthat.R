if (Sys.getenv("GITHUB_ACTIONS") == "true" && .Platform$OS.type == "unix") {
  install.packages("prophet", type = "source")
} else { # just to make sure which branch we're going into
  print("not installing prophet from source")
}

library(jaspTools)
library(testthat)

jaspTools::runTestsTravis(module = getwd())
