#' Encrypt an R object with a password
#'
#' This routine encrypts any R object with a password using
#' [sodium's secret key encryption](https://cran.r-project.org/web/packages/sodium/vignettes/intro.html#secret-key-encryption).
#'
#' @param obj Any object
#' @param password A character string
#' @param file If not `NULL`, a path to where the Rds object should be saved
#' @param verbose Signals to message() when/where an Rds file is written
#' @return The cipertext bytes of the encrypted object
#' @export
password_encrypt <- function(obj, password, file = NULL, verbose = TRUE) {
  stopifnot(is.character(password))

  pass_key <- passkey(password)
  obj_msg <- serialize(obj, NULL)

  ciphertext <- sodium::data_encrypt(obj_msg, pass_key)

  if (!is.null(file)) {
    stopifnot(is.character(file))

    if (verbose) {
      message(paste0("Writing '", file, "'"))
    }

    saveRDS(ciphertext, file = file)
  }

  ciphertext
}

#' Encrypt an R object with a password
#'
#' This routine encrypts any R object with a password using
#' [sodium's secret key encryption](https://cran.r-project.org/web/packages/sodium/vignettes/intro.html#secret-key-encryption).
#'
#' @param x Either
#'   * A path to an Rds file for an encrypted ciphertext
#'   * An encrypted ciphertext object
#' @param password A character string
#' @return An decrypted R object from the cipertext
#' @export
password_decrypt <- function(x, password) {
  stopifnot(is.character(password))

  if (is.character(x)) {
    x <- readRDS(x)
  }

  pass_key <- passkey(password)

  decrypted <- sodium::data_decrypt(x, pass_key)
  unserialize(decrypted)
}

passkey <- function(password) {
  sodium::hash(charToRaw(password))
}
