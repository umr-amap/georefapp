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
   bounding circle of what you drew, using the point-radius method. A locality
   that is itself an area — a park, a plot network — can instead be imported
   from a GIS file and recorded as that area; see [Areas](#areas).
3. **Export** a Darwin Core table, the decision log that backs it, and the
   footprints as a GeoPackage; see [Getting your results](#getting-your-results).

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

### The radius for a bare point

A marker on its own has no size, but a place does. **Radius for a bare point**
(default 1000 m) is drawn around each marker, and the result is treated like a
circle you drew yourself. Its consequences:

- **It becomes the uncertainty.** For a marker alone,
  `coordinateUncertaintyInMeters` is exactly this radius, and every record
  sharing the locality inherits it: the claim that the specimen was collected
  within that distance of the coordinate. Too small overstates how well the
  place is known; too large throws away precision you had.
- **It only affects markers.** Drawn circles keep their own radius; lines and
  polygons are used as drawn. A marker drawn together with other shapes gets the
  radius first, and the enclosing circle then covers everything.
- **It applies to what you save next.** Saved georeferences keep the radius they
  were saved with. The value stays set when you move to the next locality, so
  check it; the metrics under the map say whenever it is in use.
- **Zero is refused**, because it would claim the spot is known exactly.

Choose it to reach from the marker to the farthest edge of the place the text
names — the whole village, camp or town — and add the imprecision of the source,
such as a coarse map or a guessed position.

## Areas

Some localities are not a place somewhere inside a shape but the shape itself:
`Parc National d'Odzala`, a plot network, a concession. Tick **The shape is the
locality itself** and the decision is recorded with
`georefappDecisionType = "area"`. `footprintWKT` then holds the extent of the
locality, not an envelope around an uncertain position. The point and radius
are still derived from it, so the output stays GeoPick-compatible.

Boundaries rarely need drawing by hand. Under **Import an area from a file**,
load a GeoJSON, KML/KMZ, GeoPackage or shapefile (zipped, or all its parts at
once) and pick the feature or features that make up the locality. The file
stays loaded as you move between localities, so one file of parks serves them
all. Invalid boundaries are repaired, and the protocol names the file, layer
and features the footprint came from.

`footprintWKT` is repeated on every record that inherits a decision, and an
official boundary can run to a megabyte of text. The app warns when a footprint
is large; **Simplify boundary to** removes vertices within a tolerance in
metres, and the tolerance is written into the protocol.

An area needs a footprint with an area: a line or a bare point is refused.

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

Building it needs an account on the database, and the connection goes through
[CafriplotsR](https://github.com/umr-amap/cafriplotsR), which knows where the
database lives and handles credentials: it reads `MYDB_USER` / `MYDB_PASS` from
`~/.Renviron` (`CafriplotsR::setup_db_credentials()` writes them) and asks for
them otherwise. See `vignette("gazetteer")` for what the build does at each
step.

```r
# remotes::install_github("umr-amap/cafriplotsR", upgrade = "never")
con <- CafriplotsR::call.mydb()
gazetteer_snapshot_build(con, "rainbio-gazetteer.sqlite", overwrite = TRUE)
CafriplotsR::cleanup_connections()
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

## Getting your results

### Where your work is saved

Your work lives in the **project file**, a `.sqlite` file created on the Import
page, by default in R's working directory. Every decision is written to it the
moment you click *Save georeference*, so there is no "save" step at the end and
nothing is lost if you close the browser half-way. The navbar shows which file
is open, with a link to copy its full path.

To **carry on later**, launch the app again: the Import page lists the projects
in the working directory (or takes a path to one elsewhere) under *Continue a
project*. Creating a new project under a name that already exists asks whether
to open it instead.

The project file is not the result you share: it is the source the results are
built from. Keep it, though — it is what lets the results be rebuilt, checked
and revised later.

### Exporting

1. Open the **Export** page. The app offers to take you there when the last
   locality is decided, and the *Export results →* link above the locality list
   goes there at any time.
2. Check the summary at the top: how many records are georeferenced, how many
   localities are still pending or were marked unresolvable.
3. Get the files:
   - Running locally with `launch()`, click **Save all files next to the
     project**. The three files are written into the project's folder, and the
     page lists what it wrote.
   - Or use the three **download** buttons, which save to your browser's
     download folder. These are the only option when the app runs on a server.

You do not have to finish first. An export reflects the project as it stands,
and exporting again after more work or a revision simply produces up-to-date
files; nothing is changed in the project by exporting.

### What you get

Three files, named after the project and the day of export, e.g.
`odzala_dwc_20260916.csv`:

| File | What it is | Use it for |
|---|---|---|
| `<project>_dwc_<date>.csv` | The Darwin Core table: one row per imported record. | Your dataset, database or publication. |
| `<project>_log_<date>.csv` | Every decision ever made, revisions included. | The audit trail. Publish it alongside the table. |
| `<project>_footprints_<date>.gpkg` | The shapes behind the decisions, as map layers. | Checking and mapping in QGIS or ArcGIS. |

The CSV files are UTF-8, comma-separated, with empty cells for missing values.

#### The Darwin Core table

One row for **every record you imported**, including the ones not yet
georeferenced, so it always lines up with your original table.

| Column | Content |
|---|---|
| `recordID` | The identifier from the column you chose at import — or, if you chose none, `r000001`, `r000002`… in the row order of your file. |
| `verbatimLocality`, `country` | As imported. |
| `decimalLatitude`, `decimalLongitude` | The georeferenced coordinate (WGS84). |
| `coordinateUncertaintyInMeters` | Radius of the circle enclosing everything the locality could mean. |
| `pointRadiusSpatialFit` | Circle area ÷ footprint area; empty for lines. |
| `footprintWKT`, `footprintSRS` | The shape itself, as WKT in EPSG:4326. For an area, this *is* the locality. |
| `geodeticDatum`, `coordinatePrecision` | `EPSG:4326` and `1e-07`. |
| `georeferencedBy`, `georeferencedDate`, `georeferenceProtocol`, `georeferenceSources`, `georeferenceRemarks` | Who decided, when, by what method, from what sources, and why. |
| `georefappDecisionID` | The decision in the log that produced this row. |
| `georefappDecisionType` | `drawn`, `area`, `unresolvable`, or empty if still pending. |

Coordinates are empty in two cases, which `georefappDecisionType` tells apart:
empty means nobody has decided the locality yet; `unresolvable` means someone
looked and could not, with the reason in `georeferenceRemarks`.

The column names and meanings follow Darwin Core and match
[GeoPick](https://geopick.gbif.org), so the table can be loaded wherever GeoPick
output is accepted.

#### Joining the results back to your data

The export carries the georeference, not the rest of your original columns. To
put them back together, join on `recordID`:

```r
original <- readr::read_csv("my_specimens.csv")
georef   <- readr::read_csv("odzala_dwc_20260916.csv")
merged   <- dplyr::left_join(original, georef, by = c("catalog_number" = "recordID"))
```

This is why choosing your own identifier column at import matters. If none was
chosen, `recordID` follows row order, and the join must be done by position on
the unmodified file. The same happens if the chosen column has blanks or
repeated values, since those cannot tell records apart; the Import page then
shows a red warning naming the problem, before the project is created, so you
can pick another column or fix the file first.

#### The decision log

One row per decision, **including superseded ones**: when a locality is
revised, the earlier decision stays, and the new one names it in `supersedes`.
The Darwin Core table uses only the latest decision for each locality; the log
shows how it got there. Besides the georeference itself it records
`locality_key` (the normalised grouping the decision applies to), `centre_rule`,
`app_version`, `gazetteer_snapshot` (which locality dictionary was on screen)
and `created_at`.

#### The footprints

A GeoPackage with one feature per decided locality: a `polygons` layer for
areas, circles and drawn shapes, and a `lines` layer if any locality was drawn
as a line. Each feature carries the coordinate, uncertainty, decision type,
provenance, and `nRecords`, the number of records that inherit it. Unresolvable
and pending localities have no shape and are not included.

### From the R console

The same three files, without opening the app:

```r
library(georefapp)
export_project("odzala.sqlite")                  # next to the project file
export_project("odzala.sqlite", dir = "results") # or somewhere else
```

Or work with the tables directly:

```r
con <- store_open("odzala.sqlite")
dwc <- dwc_table(con)        # the Darwin Core table
log <- store_decisions(con)  # the decision log
fp  <- dwc_footprints(con)   # list of sf layers: fp$polygons, fp$lines
store_close(con)
```

## Tests

```r
testthat::test_dir("tests/testthat", package = "georefapp")
```
