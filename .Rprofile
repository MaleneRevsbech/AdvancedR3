options(
  renv.settings.snapshot.type = "explicit",
  renv.config.auto.snapshot = TRUE
)

# We need this before the dependency code below
source("renv/activate.R")

# Open up this in the interactive session when project starts
# Every time you open the project, it opens up this.
# Run the code and load this package
# Quite useful, you don't need the usethis:: anymore!!
if (interactive()) {
  suppressMessages(require(usethis))
}


# renv is a workflow dependency, not a build dependency
