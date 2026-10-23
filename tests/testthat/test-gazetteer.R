# A miniature RAINBIO: the Mbandza cluster that motivated including
# ungeoreferenced localities, plus enough filler to make "village" and "parc"
# common words and "mbandza" a rare one.
fake_gazetteer <- function() {
  loc <- tibble::tibble(
    locality_verbatim = c(
      "P.N. Odzala. Mbandza, village sur la Lekoli, au NNW de Mbomo",
      "Parc National d'Odzala. Village Mbandza.",
      "parc national d'odzola, ancien champ du cacao, village mbandza",
      "Congo Brazzaville. Layon Mbandza (km I)",
      "Yangambi, village de la riviere",
      "Parc National de Lope, village Ipassa",
      "Kribi, village de peche",
      "Route de Ouesso, village Liouesso"
    ),
    is_georeferenced = c(1L, 1L, 0L, 0L, 1L, 1L, 1L, 0L),
    decimal_latitude = c(0.62, 0.63, NA, NA, 0.81, -0.20, 2.94, NA),
    decimal_longitude = c(14.81, 14.80, NA, NA, 24.49, 11.60, 9.91, NA),
    n_records = c(12L, 5L, 3L, 1L, 40L, 7L, 9L, 2L),
    n_distinct_coords = c(4L, 2L, NA, NA, 11L, 3L, 5L, NA),
    span_deg = c(0.05, 0.01, NA, NA, 0.4, 0.02, 0.1, NA),
    n_name_variants = 1L
  )
  loc$locality_id <- seq_len(nrow(loc))
  loc$locality_key <- normalise_locality(loc$locality_verbatim)
  loc$n_records_georef <- ifelse(loc$is_georeferenced == 1L, loc$n_records, 0L)
  loc
}

local_snapshot <- function(env = parent.frame(), ...) {
  file <- withr::local_tempfile(fileext = ".sqlite", .local_envir = env)
  unlink(file)
  suppressMessages(
    gazetteer_snapshot_build(fake_gazetteer(), file, snapshot_id = "test-1", ...)
  )
  con <- gazetteer_open(file)
  withr::defer(gazetteer_close(con), envir = env)
  con
}

test_that("a snapshot round-trips localities, tokens and metadata", {
  con <- local_snapshot()
  info <- gazetteer_info(con)
  expect_equal(unname(info[["snapshot_id"]]), "test-1")
  expect_equal(as.integer(info[["n_localities"]]), 8L)
  expect_equal(as.integer(info[["n_georeferenced"]]), 5L)
  expect_gt(as.integer(info[["n_postings"]]), 0L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM gaz_localities")$n, 8L)
})

test_that("refusing to overwrite protects an existing snapshot", {
  file <- withr::local_tempfile(fileext = ".sqlite")
  unlink(file)
  suppressMessages(gazetteer_snapshot_build(fake_gazetteer(), file))
  expect_error(gazetteer_snapshot_build(fake_gazetteer(), file), "already exists")
  expect_silent(suppressMessages(
    gazetteer_snapshot_build(fake_gazetteer(), file, overwrite = TRUE)))
})

test_that("a rare name finds its cluster, common words alone find nothing", {
  con <- local_snapshot(df_max_frac = 0.5)

  hits <- gazetteer_search(con, "Mbandza", min_coverage = 0.3)
  expect_equal(nrow(hits), 4L)
  expect_true(all(grepl("mbandza", tolower(hits$locality_verbatim))))

  # "village" is in six of eight localities: nothing to discriminate on, so the
  # search reports no candidates rather than all of them.
  none <- gazetteer_search(con, "village")
  expect_equal(nrow(none), 0L)
  expect_equal(attr(none, "query")$too_common, "village")
})

test_that("ungeoreferenced neighbours are returned and flagged", {
  con <- local_snapshot(df_max_frac = 0.5)
  hits <- gazetteer_search(con, "village Mbandza", min_coverage = 0.3)
  expect_true(any(!hits$is_georeferenced))
  expect_true(any(hits$is_georeferenced))
  expect_true(all(is.na(hits$decimal_latitude[!hits$is_georeferenced])))

  only <- gazetteer_search(con, "village Mbandza", min_coverage = 0.3,
                           georeferenced_only = TRUE)
  expect_true(all(only$is_georeferenced))
  expect_lt(nrow(only), nrow(hits))
})

test_that("a typo costs the token that carries it", {
  con <- local_snapshot(df_max_frac = 0.5)
  # "odzola" for "odzala" -- the misspelling is its own rare token, so it
  # matches only the record that shares the typo. Token rarity cannot repair
  # spelling; that is what a character-level measure would be for.
  hits <- gazetteer_search(con, "odzola", min_coverage = 0.3)
  expect_equal(nrow(hits), 1L)
  expect_match(hits$locality_verbatim, "odzola")
})

test_that("coverage ranks containment, overlap breaks the tie", {
  con <- local_snapshot(df_max_frac = 0.5)
  hits <- gazetteer_search(con, "Mbandza", min_coverage = 0.3)
  # Every hit contains the whole query, so all four are fully covered ...
  expect_true(all(abs(hits$coverage - 1) < 1e-9))
  # ... and overlap orders them by how much *else* they distinctively say.
  expect_true(!is.unsorted(rev(hits$overlap)))

  # Not by length: "Congo Brazzaville. Layon Mbandza (km I)" is the shortest of
  # the four but every word in it bar "mbandza" is unique in the corpus, which
  # makes it a heavily specified place of its own rather than a variant of the
  # query. The Odzala entries, whose other words are shared, rank above it.
  pos <- match(c("Parc National d'Odzala. Village Mbandza.",
                 "Congo Brazzaville. Layon Mbandza (km I)"),
               hits$locality_verbatim)
  expect_lt(pos[1], pos[2])
})

test_that("results honour the candidate contract", {
  con <- local_snapshot(df_max_frac = 0.5)
  hits <- gazetteer_search(con, "Mbandza")
  expect_named(hits, names(candidates_empty()))
  expect_equal(vapply(hits, class, ""), vapply(candidates_empty(), class, ""))
  expect_named(gazetteer_search(con, ""), names(candidates_empty()))
})

test_that("the provider plugs into candidates_query and stamps decisions", {
  file <- withr::local_tempfile(fileext = ".sqlite")
  unlink(file)
  suppressMessages(gazetteer_snapshot_build(fake_gazetteer(), file,
                                            snapshot_id = "rainbio-test",
                                            df_max_frac = 0.5))
  withr::local_options(georefapp.candidates_snapshot = NULL)
  p <- gazetteer_provider(file, min_coverage = 0.3)
  withr::defer(gazetteer_close(attr(p, "con")))
  withr::local_options(georefapp.candidates = p)

  expect_equal(candidates_snapshot(), "rainbio-test")
  cand <- candidates_query("mbandza", verbatim = "Village Mbandza", limit = 3L)
  expect_lte(nrow(cand), 3L)
  expect_named(cand, names(candidates_empty()))
})

test_that("a directory gets the default name written inside it", {
  dir <- withr::local_tempdir()
  expect_message(
    out <- gazetteer_snapshot_build(fake_gazetteer(), dir, overwrite = TRUE),
    "Writing to")
  expect_equal(out, file.path(dir, "rainbio-gazetteer.sqlite"))
  expect_true(file.exists(out))

  # A trailing separator means a directory even where none exists yet. Without
  # this, dirname() would strip "sub" and write the snapshot as a file of that
  # name one level up.
  sub <- paste0(file.path(dir, "sub"), "/")
  expect_error(suppressMessages(gazetteer_snapshot_build(fake_gazetteer(), sub)),
               "No such directory")
})

test_that("a bad destination fails before the corpus is read, not after", {
  dir <- withr::local_tempdir()
  # SQLite reports an unwritable destination only at the write, by which point
  # the whole dictionary has been fetched and indexed.
  expect_error(
    gazetteer_snapshot_build(fake_gazetteer(), file.path(dir, "nope", "g.sqlite")),
    "No such directory")
})

# A second, source-specific dictionary: islands placed by their distance along
# the river, as built for Mandango (1982).
fake_islands <- function() {
  tibble::tibble(
    locality_id = 1:3,
    locality_verbatim = c("Ile Mbie / Mbiye 5M, fleuve Lualaba",
                          "Ile Mafi 5M, fleuve Lualaba",
                          "Ile Kongolo 15V, fleuve Congo"),
    locality_key = normalise_locality(locality_verbatim),
    n_records = c(82L, 14L, 23L), n_records_georef = c(82L, 14L, 23L),
    n_distinct_coords = 1L, decimal_latitude = c(0.477, 0.477, 0.549),
    decimal_longitude = c(25.221, 25.221, 25.062), span_deg = NA_real_,
    n_name_variants = 1L, is_georeferenced = 1L
  )
}

local_snapshot_file <- function(src, id, label, env = parent.frame(), ...) {
  file <- withr::local_tempfile(fileext = ".sqlite", .local_envir = env)
  unlink(file)
  suppressMessages(gazetteer_snapshot_build(src, file, snapshot_id = id,
                                            label = label, ...))
  file
}

test_that("a snapshot's label is reported as the source of its candidates", {
  file <- local_snapshot_file(fake_islands(), "islands-1", "mandango-km",
                              df_max_frac = 0.5)
  withr::local_options(georefapp.candidates = NULL,
                       georefapp.candidates_snapshot = NULL)
  p <- gazetteer_provider(file, min_coverage = 0.3)
  on.exit(gazetteer_close(attr(p, "con")), add = TRUE)
  hits <- p("ile mbie 5m a 2 km de la pointe aval",
            "Ile Mbie 5M, a 2 km de la pointe aval")
  expect_equal(hits$source[1], "mandango-km")
  expect_match(hits$locality_verbatim[1], "Mbie")
})

test_that("a snapshot built without a label still reads as RAINBIO", {
  # Older snapshots have no label row at all.
  file <- withr::local_tempfile(fileext = ".sqlite")
  unlink(file)
  suppressMessages(gazetteer_snapshot_build(fake_gazetteer(), file, snapshot_id = "old"))
  rw <- DBI::dbConnect(RSQLite::SQLite(), file)
  DBI::dbExecute(rw, "DELETE FROM gaz_meta WHERE key = 'label'")
  DBI::dbDisconnect(rw)
  withr::local_options(georefapp.candidates_snapshot = NULL)
  p <- gazetteer_provider(file)
  on.exit(gazetteer_close(attr(p, "con")), add = TRUE)
  hits <- p("mbandza", "Mbandza")
  expect_true(all(hits$source == "rainbio"))
})

test_that("combined providers are listed one dictionary after the other", {
  a <- function(locality_key, verbatim = NULL, limit = 10L) {
    out <- candidates_empty()
    out[1, "candidate_id"] <- "r1"; out$source <- "rainbio"
    attr(out, "query") <- list(token = "mbie")
    out
  }
  b <- function(locality_key, verbatim = NULL, limit = 10L) {
    out <- candidates_empty()
    out[1, "candidate_id"] <- "k1"; out$source <- "mandango-km"
    out
  }
  p <- candidates_combine(a, b)
  hits <- p("ile mbie")
  expect_equal(hits$candidate_id, c("r1", "k1"))
  expect_named(hits, names(candidates_empty()))
  expect_equal(attr(hits, "query")$token, "mbie")
})

test_that("one failing dictionary does not silence the others", {
  ok <- function(locality_key, verbatim = NULL, limit = 10L) {
    out <- candidates_empty(); out[1, "candidate_id"] <- "k1"; out
  }
  broken <- function(locality_key, verbatim = NULL, limit = 10L) stop("offline")
  p <- candidates_combine(broken, ok)
  expect_warning(hits <- p("ile mbie"), "offline")
  expect_equal(hits$candidate_id, "k1")

  none <- candidates_combine(function(...) candidates_empty())("x")
  expect_equal(nrow(none), 0L)
  expect_named(none, names(candidates_empty()))
})

test_that("using two snapshots stamps decisions with both identifiers", {
  f1 <- local_snapshot_file(fake_gazetteer(), "rainbio-test", "rainbio")
  f2 <- local_snapshot_file(fake_islands(), "islands-1", "mandango-km",
                            df_max_frac = 0.5)
  withr::local_options(georefapp.candidates = NULL,
                       georefapp.candidates_snapshot = NULL)
  p <- suppressMessages(use_gazetteer(c(f1, f2), min_coverage = 0.3))
  on.exit(gazetteer_close(attr(p, "con")), add = TRUE)
  expect_equal(candidates_snapshot(), "rainbio-test + islands-1")
  expect_length(attr(p, "con"), 2L)
  hits <- candidates_query("ile kongolo 15v", "Ile Kongolo 15V")
  expect_true("mandango-km" %in% hits$source)
})
