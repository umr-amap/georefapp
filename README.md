# georefapp

Batch georeferencing of plain-text localities, aimed at Central Africa.

Existing tools such as [GeoPick](https://geopick.gbif.org) georeference one
locality at a time. `georefapp` is built for the other case: a table of
specimen records where a few hundred distinct places account for a few thousand
rows. You decide each distinct place once, and every record sharing it inherits
the result.

## What it does

1. **Import** a CSV, TSV or Excel table and say which column holds the locality
   text. Records are grouped by a normalised key, so `Yangambi`,
   `Env. Yangambi` and `yangambi` become one unit of work.
2. **Georeference** by drawing a point, circle, line, rectangle or polygon on
   the map. The coordinate and its uncertainty are derived from the minimum
   bounding circle of what you drew, using the point-radius method.
3. **Export** a Darwin Core table, plus the decision log that backs it.

## Installation

Copy the three steps below into the R console, one after the other.

```r
# 1. Allow more time for the download (useful on slow connections)
options(timeout = max(3000, getOption("timeout")))

# 2. Install the 'remotes' helper - only needed the first time
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# 3. Install georefapp from GitHub (all required packages come with it)
remotes::install_github("umr-amap/georefapp", upgrade = "never")
```

Then check that the installation worked:

```r
library(georefapp)
```

If this last line prints no error, the package is ready to use.

To install the vignette along with it, add `build_vignettes = TRUE` to step 3;
it takes a little longer and needs `knitr` and `rmarkdown`.

**Troubleshooting:**

- *"there is no package called 'remotes'"* — run step 2 again.
- *Installation stops on a slow connection* — restart R, then run the three
  steps again starting with step 1.
- If R asks `Do you want to install from sources the package which needs
  compilation?`, answer **No** (type `n` and press Enter).
- *`sf` fails to install* — it needs GDAL, GEOS and PROJ on the system. On
  Windows and macOS the binary from CRAN carries them, which is why answering
  **No** above matters. On Linux, install the system libraries first
  (`libgdal-dev`, `libgeos-dev`, `libproj-dev` on Debian and Ubuntu).

**Note:** You do not need a database, an account or a gazetteer to get started.
Running without a dictionary is fully supported, and is the normal state for
most of Central Africa — the app simply reports no similar locality. The
dictionary is an optional local file you build once; see
[The locality dictionary](#the-locality-dictionary).

## Reproducibility

A project is a single SQLite file. Decisions are **append-only**: revising a
georeference writes a new row pointing at the one it replaces, and the earlier
interpretation stays in the file. A decision is "current" when nothing
supersedes it.

The Darwin Core table is therefore *derived*, never edited — it can be
regenerated from the project file at any time. Ship the file with the dataset
and the georeferencing is auditable rather than merely asserted.

Every decision records who made it, when, against what sources, under what
protocol, with what remarks, and which dictionary snapshot was consulted.

## Uncertainty

The metric work happens in an azimuthal equidistant projection centred on the
reported coordinate, which makes the radius a true geodesic distance rather
than an approximation. Two centre rules are available:

- `mbc` (default) — the centre of the minimum bounding circle, which gives the
  smallest honest uncertainty.
- `inside` — forces the coordinate onto the footprint itself, for concave
  shapes such as a river bend where the bounding-circle centre would fall
  outside the feature. Necessarily yields a larger radius.

`pointRadiusSpatialFit` follows the Darwin Core definition: the ratio of the
circle's area to the footprint's, `NA` where the footprint has no area (a line,
or an unbuffered point).

## The locality dictionary

While deciding a locality, the app shows places elsewhere whose name resembles
it — as a list, and as markers on the map. The source is the RAINBIO locality
dictionary, migrated into the `plots_transects` database as
`rainbio_gazetteer_localities` and read through a read-only role.

Matching is on **token rarity**, not string similarity. Each word is weighted
by its inverse document frequency across the whole dictionary, so `village` is
worth almost nothing and `Odzala` almost everything — without a stoplist, which
matters for a corpus mixing French, English, Portuguese and local languages.
Two figures are reported:

- **coverage** — how much of the *query's* weight the candidate accounts for.
  The primary ranking, because the question is "where are the localities that
  mention this place", not "which localities are worded like this one".
- **overlap** — weighted Jaccard, which penalises a candidate for saying more
  than the query did. It breaks ties. Note it penalises *information*, not
  length: a short name made of otherwise-unique words ranks low, because it is
  a specific other place rather than a variant.

Candidates with no coordinates are listed too, greyed and unplottable. A
similar name that is itself ungeoreferenced is still evidence — usually that
the same place is filed under another spelling.

Token matching cannot repair spelling: `odzola` for `odzala` is its own rare
token and finds only records carrying the same typo. Closing that needs a
character-level measure (`pg_trgm`, `stringdist`), not a tuning change here.

The app never queries the database while you work. The dictionary is taken as a
local SQLite snapshot with its index built in — 360,582 localities, 126 MB,
20–70 ms per search — and every decision records which snapshot it was made
against.

Credentials come from `MYDB_USER` / `MYDB_PASS` in `~/.Renviron`, never from a
script. See `vignette("gazetteer")` for what the build does at each step.

```r
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = "dg474899-001.dbaas.ovh.net", port = 35699L, dbname = "plots_transects",
  user = Sys.getenv("MYDB_USER"), password = Sys.getenv("MYDB_PASS")
)
gazetteer_snapshot_build(con, "rainbio-gazetteer.sqlite", overwrite = TRUE)
```

Running without a dictionary is fully supported, and is the normal state for
most of Central Africa. The contract is fixed in `R/candidates.R`, so a
different dictionary means supplying a function, not changing the application:

```r
options(georefapp.candidates = function(locality_key, verbatim = NULL, limit = 10L) {
  # ... return a tibble shaped like candidates_empty()
})
```

## Running it

```r
library(georefapp)
launch()                                          # http://127.0.0.1:5792
launch(gazetteer = "rainbio-gazetteer.sqlite")    # with the dictionary
```

Or open the bundled example from the Import page to try it without data of your
own.

Closing the browser returns you to the R console; reloading the page does not.
Pass `stop_on_close = FALSE` to keep the app running and stop it with Esc.

Working on the package itself, `pkgload::load_all(".")` replaces the
`library()` call and picks up edits without reinstalling.

## Tests

```r
testthat::test_dir("tests/testthat", package = "georefapp")
```
