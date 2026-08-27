# Matching locality strings on token rarity.
#
# Two locality descriptions refer to the same place when they share words that
# are rare in the corpus. "Village" and "route" are shared by tens of thousands
# of RAINBIO localities and say almost nothing; "Odzala" or "Lekoli" are shared
# by a few dozen and say almost everything. Weighting each shared word by its
# inverse document frequency turns that intuition into a number, and does so
# without a stoplist -- which matters here because the corpus mixes French,
# English, Portuguese and local languages, and no hand-written stoplist would
# cover all four. The corpus decides what is uninformative.

#' Normalise a string to lowercase unaccented alphanumerics
#'
#' The shared first step of key building and tokenisation: transliterate to
#' ASCII, lowercase, and turn every run of punctuation into a single space.
#'
#' @param x Character vector.
#'
#' @return A character vector of the same length.
#' @noRd
normalise_chars <- function(x) {
  out <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  out <- tolower(out)
  out <- gsub("[^[:alnum:][:space:]]+", " ", out)
  out <- gsub("[[:space:]]+", " ", out)
  trimws(out)
}

#' Words that hedge a locality without naming a place
#'
#' The same set [normalise_locality()] strips from the front of a key, taken
#' apart into single words and dropped wherever they occur. Their exclusion
#' rests on the definition already used there -- a word that does not change
#' which place is meant -- and not on how common they are: "env." appears in a
#' few thousand of RAINBIO's localities, rare enough that inverse document
#' frequency would treat it as strong evidence and match "env. de Kribi" to
#' "Env. de Léopoldville".
#'
#' Displacement words are deliberately absent: `km`, `north` and the rest do
#' change the place, so they stay and the corpus decides their weight.
#'
#' @return A character vector of tokens to drop.
#' @noRd
hedge_tokens <- function() {
  unique(unlist(strsplit(vague_prefixes, " ", fixed = TRUE)))
}

#' Split locality strings into matchable tokens
#'
#' Tokens are deduplicated within a locality: a locality is a name, not a
#' document, so a word occurring twice in one description is not stronger
#' evidence than a word occurring once. Single characters are dropped -- the
#' bearings and initials they usually represent (`N`, `S`, `E`) appear in so
#' many localities that their weight would be negligible anyway, and dropping
#' them keeps the index appreciably smaller. Hedges go too; see
#' [hedge_tokens()].
#'
#' @param x Character vector of locality descriptions.
#' @param min_nchar Shortest token kept.
#'
#' @return A list of character vectors, one per element of `x`, each holding
#'   that locality's distinct tokens.
#'
#' @examples
#' locality_tokens("P.N. Odzala, village Mbandza")
#' locality_tokens("30 km env. ESE of Kribi")
#'
#' @export
locality_tokens <- function(x, min_nchar = 2L) {
  parts <- strsplit(normalise_chars(x), " ", fixed = TRUE)
  drop <- hedge_tokens()
  lapply(parts, function(t) {
    # nchar(NA) is 2, so NA has to be excluded explicitly rather than by length.
    t <- t[!is.na(t) & nchar(t) >= min_nchar]
    unique(t[!t %in% drop])
  })
}

#' Inverse document frequency of a token
#'
#' `log1p(n / df)` rather than the textbook `log(n / df)`, so that a token
#' present in every locality scores 0.69 instead of 0, and still contributes
#' something when it is all a query has.
#'
#' @param df Number of localities containing the token.
#' @param n_docs Size of the corpus.
#'
#' @return A numeric vector of weights.
#' @noRd
token_idf <- function(df, n_docs) {
  log1p(n_docs / df)
}

#' Build the inverted index for a corpus of locality strings
#'
#' Pure computation over vectors, with no database involved, so the weighting
#' can be tested on a corpus of ten localities rather than a snapshot of three
#' hundred thousand.
#'
#' Tokens above `df_max_frac` of the corpus are recorded but **not** posted.
#' They are too common to discriminate, searching them would mean scanning tens
#' of thousands of postings for nothing, and their absence from the index is
#' what keeps the snapshot a manageable size. Their document frequency is kept
#' so the interface can say a query word was ignored for being ubiquitous
#' rather than for being unknown.
#'
#' @param ids Integer locality identifiers.
#' @param text Character vector of locality strings, parallel to `ids`.
#' @param df_max_frac Fraction of the corpus above which a token stops being
#'   posted.
#' @param min_nchar Passed to [locality_tokens()].
#'
#' @return A list with
#'   * `tokens`: one row per distinct token, with `df`, `idf` and `indexed`;
#'   * `postings`: `token` / `locality_id` pairs for indexed tokens only;
#'   * `weights`: total indexed weight per locality, parallel to `ids`;
#'   * `n_docs`: corpus size.
#' @noRd
build_token_index <- function(ids, text, df_max_frac = 0.05, min_nchar = 2L) {
  stopifnot(length(ids) == length(text))
  n_docs <- length(ids)

  toks <- locality_tokens(text, min_nchar = min_nchar)
  flat <- unlist(toks, use.names = FALSE)
  owner <- rep(ids, lengths(toks))

  # A factor over the flattened postings gives the vocabulary and the document
  # frequencies in one pass; table() on several million strings is markedly
  # slower.
  f <- factor(flat)
  vocab <- levels(f)
  idx <- as.integer(f)
  df <- tabulate(idx, nbins = length(vocab))
  idf <- token_idf(df, n_docs)
  indexed <- df <= max(1, floor(df_max_frac * n_docs))

  keep <- indexed[idx]
  postings <- data.frame(
    token = flat[keep],
    locality_id = owner[keep],
    stringsAsFactors = FALSE
  )

  # The candidate's own weight, used as the denominator of the overlap measure,
  # counts only indexed tokens -- the same vocabulary the query is restricted
  # to, so both sides of the comparison are measured on the same scale.
  w <- numeric(n_docs)
  if (nrow(postings) > 0) {
    per_id <- rowsum(idf[idx][keep], group = owner[keep], reorder = FALSE)
    w[match(as.integer(rownames(per_id)), ids)] <- per_id[, 1]
  }

  list(
    tokens = data.frame(token = vocab, df = df, idf = idf,
                        indexed = as.integer(indexed), stringsAsFactors = FALSE),
    postings = postings,
    weights = w,
    n_docs = n_docs
  )
}

#' Weigh a query's tokens against a built vocabulary
#'
#' Query tokens absent from the vocabulary, or present but unindexed, carry no
#' weight and are excluded from the total. Coverage is therefore measured
#' against what the corpus can actually be searched on, not against the raw
#' query, so a query of "Odzala village" is not penalised for the half of it
#' that no index could discriminate.
#'
#' @param tokens Character vector of query tokens.
#' @param vocab A data frame with `token`, `idf` and `indexed`, as returned in
#'   the `tokens` element of [build_token_index()].
#'
#' @return A list with `token`, `weight`, `total`, and the tokens dropped as
#'   `unknown` and `too_common`.
#' @noRd
weigh_query <- function(tokens, vocab) {
  hit <- match(tokens, vocab$token)
  unknown <- tokens[is.na(hit)]
  known <- tokens[!is.na(hit)]
  hit <- hit[!is.na(hit)]
  usable <- as.logical(vocab$indexed[hit])
  list(
    token = known[usable],
    weight = vocab$idf[hit][usable],
    total = sum(vocab$idf[hit][usable]),
    unknown = unknown,
    too_common = known[!usable]
  )
}
