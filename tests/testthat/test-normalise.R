test_that("hedging prefixes collapse to one key", {
  keys <- normalise_locality(c("Yangambi", "Env. Yangambi", "  yangambi ", "Ca. Yangambi"))
  expect_length(unique(keys), 1L)
  expect_identical(unique(keys), "yangambi")
})

test_that("accents and punctuation do not split a locality", {
  expect_identical(
    normalise_locality("Réserve de faune de la Lopé"),
    normalise_locality("Reserve de faune de la Lope")
  )
})

test_that("displacement is preserved, because it changes the place", {
  expect_false(
    identical(normalise_locality("Makokou"), normalise_locality("5 km N of Makokou"))
  )
})

test_that("stacked prefixes are stripped", {
  expect_identical(normalise_locality("environs de ca Kribi"), "kribi")
})

test_that("empty and missing input yield NA", {
  expect_true(all(is.na(normalise_locality(c("", "   ", NA_character_)))))
})
