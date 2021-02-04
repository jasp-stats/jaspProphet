if (Sys.getenv("GITHUB_ACTIONS") == "true" && .Platform$OS.type == "unix") {
  install.packages("prophet", type = "source",
                   lib = "/Users/runner/work/jaspProphet/pkgs/Frameworks/R.framework/Versions/3.6/Resources/library")
} else { # just to make sure which branch we're going into
  print("not installing prophet from source")
}

library(jaspTools)
library(testthat)

jaspTools::runTestsTravis(module = getwd())
