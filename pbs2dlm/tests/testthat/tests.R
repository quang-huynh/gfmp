is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

# create temporary directory in file system
testing_path <- paste0(tempdir(), "/testing_directory")
#testing_path <- paste0("C:/junk/testing_directory")
dir.create(testing_path, showWarnings = FALSE)
if (getwd() != testing_path) setwd(testing_path)

## ------------------------------------------------------------------------------------------------
context("check that the package files are present")

test_that("Custom description csv is present", {
  expect_true(file.exists(file.path(system.file(package = "pbs2dlm"), "alt-slot-descriptions.csv")))
})

## ------------------------------------------------------------------------------------------------
context("create a default .rmd file")

if(file.exists("test-desc.rmd")) unlink("test-desc.rmd")
create_default_rmd("test-desc.rmd")

test_that("New .rmd description file is present", {
  expect_true(file.exists("test-desc.rmd"))
})

## ------------------------------------------------------------------------------------------------
context("Wrong slot name in description .csv file gives error")
df <- tibble(slot_type = c("Stock", "Stock"),
             slot = c("name", "nonexistent-slot-name-should-give-error"),
             use_custom_description = c(TRUE, TRUE),
             custom_description = c("example text 1", "example text 2"))

if(file.exists("test-slot-descriptions.csv")) unlink("test-slot-descriptions.csv")
readr::write_csv(df, "test-slot-descriptions.csv")

test_that("Wrong slot name in custom description file gives error", {
  expect_error(create_rmd("test-desc.rmd", "test-slot-descriptions.csv"))
})

## ------------------------------------------------------------------------------------------------
context("Check if an autogen tag is manually or accidentally removed from the file")

if(file.exists("test-desc.rmd")) unlink("test-desc.rmd")
create_default_rmd("test-desc.rmd")
rmd <- readLines("test-desc.rmd")
## Remove last autogen end tag from file
rmd <- rmd[-965]
unlink("test-desc.rmd")
conn <- file("test-desc.rmd")
write(rmd, conn)
close(conn)

test_that("Removal of an autogen tag results in error", {
  expect_error(create_rmd("test-desc.rmd", "test-slot-descriptions.csv"))
})

## ------------------------------------------------------------------------------------------------

