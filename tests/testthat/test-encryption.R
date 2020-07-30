test_that("Encryption works", {
  encrypted_mtcars <- password_encrypt(mtcars, "password")

  expect_error(unserialize(encrypted_mtcars))

  expect_identical(
    password_decrypt(encrypted_mtcars, "password"),
    mtcars
  )
})

test_that("File serialization works", {
  test_file <- file.path(tempdir(), "mtcars.Rds")

  password_encrypt(mtcars, "password", file = test_file)
  expect_identical(
    password_decrypt(test_file, "password"),
    mtcars
  )

  unlink(test_file)
})
