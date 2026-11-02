write_bytes <- function(text, encoding = "UTF-8") {
  path <- tempfile(fileext = ".csv")
  con <- file(path, "wb")
  writeBin(iconv(text, "UTF-8", encoding, toRaw = TRUE)[[1]], con)
  close(con)
  path
}

test_that("comma, semicolon, tab and pipe files read into the same table", {
  rows <- list(c("id", "locality", "country"),
               c("001", "Yangambi", "DRC"),
               c("002", "Env. Kisangani", "DRC"))
  for (d in c(",", ";", "\t", "|")) {
    path <- write_bytes(paste0(vapply(rows, paste, "", collapse = d),
                               collapse = "\n"))
    dat <- read_locality_file(path, "x.csv")
    expect_identical(names(dat), c("id", "locality", "country"), info = d)
    expect_identical(dat$id, c("001", "002"), info = d)
    expect_identical(dat$locality[2], "Env. Kisangani", info = d)
  }
})

test_that("commas inside quoted fields do not make a semicolon file comma-separated", {
  path <- write_bytes(paste(
    "id;locality;lat",
    '1;"Yangambi, 5 km N";0,8',
    '2;"Odzala, camp";1,2',
    sep = "\n"
  ))
  dat <- read_locality_file(path, "x.csv")
  expect_identical(names(dat), c("id", "locality", "lat"))
  expect_identical(dat$locality, c("Yangambi, 5 km N", "Odzala, camp"))
  expect_identical(dat$lat, c("0,8", "1,2"))
})

test_that("a Windows-1252 file from French Excel keeps its accents", {
  path <- write_bytes("id;localité\n1;Forêt de la Léfini",
                      encoding = "windows-1252")
  dat <- read_locality_file(path, "x.csv")
  expect_identical(names(dat), c("id", "localité"))
  expect_identical(dat[[2]], "Forêt de la Léfini")
})

test_that("the shipped example still reads", {
  path <- system.file("extdata", "example_localities.csv", package = "georefapp")
  dat <- read_locality_file(path)
  expect_true("locality" %in% names(dat))
  expect_gt(nrow(dat), 1)
})
