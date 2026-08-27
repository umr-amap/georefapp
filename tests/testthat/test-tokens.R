test_that("tokenisation normalises and deduplicates", {
  expect_equal(locality_tokens("P.N. Odzala, village Mbandza")[[1]],
               c("odzala", "village", "mbandza"))
  expect_equal(locality_tokens("Lékoli-Pandaka")[[1]], c("lekoli", "pandaka"))
  # A word twice in one name is not stronger evidence than a word once.
  expect_equal(locality_tokens("Mbomo, route de Mbomo")[[1]],
               c("mbomo", "route"))
  expect_equal(locality_tokens("")[[1]], character())
  expect_equal(locality_tokens(NA_character_)[[1]], character())
})

test_that("single characters are dropped, digits kept", {
  toks <- locality_tokens("5 km N of Yangambi")[[1]]
  expect_false("n" %in% toks)
  expect_true(all(c("km", "yangambi") %in% toks))
})

test_that("tokens agree with the grouping key", {
  # The key is the tokens of the string minus its hedging prefix, so a key and
  # its verbatim must never split on different boundaries.
  v <- "Env. Yangambi (Km 5)"
  expect_true(all(locality_tokens(normalise_locality(v))[[1]] %in%
                    locality_tokens(v)[[1]]))
})

test_that("idf ranks a rare token above a ubiquitous one", {
  corpus <- c(rep("village", 99), "odzala")
  df <- c(village = 100, odzala = 1)
  w <- token_idf(df, length(corpus))
  expect_gt(w[["odzala"]], w[["village"]])
  expect_gt(w[["village"]], 0)
})

test_that("the index posts rare tokens and withholds common ones", {
  text <- c(rep("village de la foret", 20), "village odzala", "mbandza odzala")
  ids <- seq_along(text)
  idx <- build_token_index(ids, text, df_max_frac = 0.5)

  common <- idx$tokens[idx$tokens$token == "village", ]
  rare <- idx$tokens[idx$tokens$token == "odzala", ]
  expect_equal(common$df, 21)
  expect_equal(rare$df, 2)
  expect_equal(common$indexed, 0L)   # 21 of 22 is above half the corpus
  expect_equal(rare$indexed, 1L)
  expect_false("village" %in% idx$postings$token)
  expect_true("odzala" %in% idx$postings$token)
})

test_that("locality weight counts indexed tokens only", {
  text <- c(rep("village de la foret", 20), "village odzala", "mbandza odzala")
  idx <- build_token_index(seq_along(text), text, df_max_frac = 0.5)
  # "village de la foret" is all ubiquitous words: nothing to search it on.
  expect_equal(idx$weights[1], 0)
  # "mbandza odzala" is two rare ones.
  expect_gt(idx$weights[22], idx$weights[21])
})

test_that("a query is weighed only on what can be searched", {
  text <- c(rep("village de la foret", 20), "village odzala", "mbandza odzala")
  idx <- build_token_index(seq_along(text), text, df_max_frac = 0.5)
  q <- weigh_query(c("village", "odzala", "bruxelles"), idx$tokens)

  expect_equal(q$token, "odzala")
  expect_equal(q$unknown, "bruxelles")
  expect_equal(q$too_common, "village")
  expect_equal(q$total, q$weight)
})

test_that("hedges are dropped wherever they occur, displacement is kept", {
  expect_equal(locality_tokens("env. de Kribi")[[1]], "kribi")
  expect_equal(locality_tokens("30 km env. ESE of Kribi")[[1]],
               c("30", "km", "ese", "kribi"))
  expect_equal(locality_tokens("environs de Yangambi")[[1]], "yangambi")
  # "5 km N of X" is a different place from "X" and must stay distinguishable.
  expect_true(all(c("km", "yangambi") %in% locality_tokens("5 km N of Yangambi")[[1]]))
})
