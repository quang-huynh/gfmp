## Create temporary directory in file system
testing_path <- paste0(tempdir(), "/testing_directory")
dir.create(testing_path, showWarnings = FALSE)
if(getwd() != testing_path){
  setwd(testing_path)
}

## ------------------------------------------------------------------------------------------------
context("Check that the package files are present")

test_that("Custom description csv is present", {
  expect_true(file.exists(file.path(system.file(package = "pbs2dlm"), "alt-slot-descriptions.csv")))
})

## ------------------------------------------------------------------------------------------------
context("Create a default .rmd file")

if(file.exists("test-desc.rmd")) unlink("test-desc.rmd")
create_default_rmd("test-desc.rmd")

test_that("New .rmd description file is present", {
  expect_true(file.exists("test-desc.rmd"))
})

## ------------------------------------------------------------------------------------------------
context("Check if wrong slot name in description .csv file gives error")
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
context("Check if a description header was manually or accidentally removed from the file")
if(file.exists("test-desc.rmd")) unlink("test-desc.rmd")
create_default_rmd("test-desc.rmd")
rmd <- readLines("test-desc.rmd")
## Remove last description from file
rmd <- rmd[-964]
unlink("test-desc.rmd")
conn <- file("test-desc.rmd")
write(rmd, conn)
close(conn)

test_that("Removal of a description results in error", {
  expect_error(create_rmd("test-desc.rmd", "test-slot-descriptions.csv"))
})

## ------------------------------------------------------------------------------------------------
context("Check if more than one description header in an autogen chunk")
if(file.exists("test-desc.rmd")) unlink("test-desc.rmd")
create_default_rmd("test-desc.rmd")
rmd <- readLines("test-desc.rmd")
## Add duplicate description to file
rmd <- append(rmd, rmd[964], after = 964)
unlink("test-desc.rmd")
conn <- file("test-desc.rmd")
write(rmd, conn)
close(conn)

test_that("Duplicate description results in error", {
  expect_error(create_rmd("test-desc.rmd", "test-slot-descriptions.csv"))
})

## ------------------------------------------------------------------------------------------------
context("Check that adding a suffix to the chunk names works")
if(file.exists("test-desc.rmd")) unlink("test-desc.rmd")
create_default_rmd("test-desc.rmd")
change_chunk_suffix("test-desc.rmd", "testme")
rmd <- readLines("test-desc.rmd")
chunk_name_regex <- "(?<=desc-)[\\w-]+(?=\\}| *,)"
val <- grep(chunk_name_regex, rmd, perl = TRUE)
mtch <- grep("testme", rmd[val])

test_that("All lines that should have had sufixes added do", {
  expect_equal(length(val), length(mtch))
})

## ------------------------------------------------------------------------------------------------
context("Check that adding a suffix with special characters or a vector of suffixes gives an error")
if(file.exists("test-desc.rmd")) unlink("test-desc.rmd")
create_default_rmd("test-desc.rmd")

test_that("All lines that should have had sufixes added do", {
  expect_error(change_chunk_suffix("test-desc.rmd", "hello?"))
  expect_error(change_chunk_suffix("test-desc.rmd", "world!"))
  expect_error(change_chunk_suffix("test-desc.rmd", "foo."))
  expect_error(change_chunk_suffix("test-desc.rmd", "."))
  expect_error(change_chunk_suffix("test-desc.rmd", "0<1"))
  expect_error(change_chunk_suffix("test-desc.rmd", "1>0"))
  expect_error(change_chunk_suffix("test-desc.rmd", "\\"))
  expect_error(change_chunk_suffix("test-desc.rmd", "\\\\"))
  expect_error(change_chunk_suffix("test-desc.rmd", "\\\\\\\\"))
  expect_error(change_chunk_suffix("test-desc.rmd", "\\\\\\\\\\"))
  expect_error(change_chunk_suffix("test-desc.rmd", "\\\\\\\\\\\\"))
  expect_error(change_chunk_suffix("test-desc.rmd", "1|2"))
  expect_error(change_chunk_suffix("test-desc.rmd", "3&4"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5:6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "7^8"))
  expect_error(change_chunk_suffix("test-desc.rmd", "hello@worldcom"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5;6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "a~b"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5(6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5)6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5[6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5]6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5{6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5}6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "hello`"))
})
