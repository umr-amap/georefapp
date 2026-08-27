# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```r
# Run the app (dev)
pkgload::load_all("."); launch()          # http://127.0.0.1:5792

# Tests — always load_all() first; tests call unexported helpers
pkgload::load_all("."); testthat::test_dir("tests/testthat", package = "georefapp")

# One file
pkgload::load_all("."); testthat::test_file("tests/testthat/test-uncertainty.R")

# Regenerate man/ and NAMESPACE (roxygen owns NAMESPACE — do not hand-edit)
roxygen2::roxygenise(".")
```

`app.R` exists only for `runApp()` / shinyapps.io deployment.

## Architecture

An R package wrapping a Shiny app. Conventions follow the sibling `ConApp`
project (`../conrappli_dreams`): bslib UI, `module-*.R` files, leafpm for
drawing, reactable for tables.

### The unit of work is the locality key, not the record

`normalise_locality()` maps locality strings to a grouping key; the whole app is
organised around deciding each key once and letting every record sharing it
inherit. Import expands to records, the workbench iterates over keys,
`dwc_table()` expands back to records. When changing normalisation, note the
deliberate asymmetry: hedging prefixes (`env.`, `ca.`, `environs de`) are
stripped because they do not change which place is meant; displacement
(`5 km N of X`) is **never** stripped because it does.

Records with blank locality text get the `no_locality_key` sentinel (`"(none)"`,
unreachable by normalisation). They are excluded from `store_localities()` so
they are never offered as work, but remain in `dwc_table()` with empty
coordinates.

### The decision log is append-only and is the source of truth

`R/store.R` only ever INSERTs into `decisions`. Revising writes a new row with
`supersedes` set. "Current" means *nothing supersedes this row* — expressed as a
`NOT EXISTS` subquery, not as a timestamp — so the log survives reordering or
merging. Never add UPDATE or DELETE against `decisions`; the audit trail is the
product.

`dwc_table()` is **derived**, never stored or edited. A test asserts the output
is identical after closing and reopening the project file. Anything that makes
the output depend on state outside the SQLite file breaks the reproducibility
guarantee the app exists for.

### Uncertainty is computed twice on purpose

`georef_metrics()` in `R/uncertainty.R` projects into an azimuthal equidistant
CRS, which preserves distance **only from its origin**. Pass one is centred on
the bounding box and is used solely to locate the coordinate; pass two is
centred on that coordinate, which puts the reported centre at the origin and
makes the radius a true geodesic distance. Collapsing this to one pass silently
under-reports uncertainty.

Gotcha when writing tests here: `sf::st_distance()` uses s2, which is
**spherical** and reads ~0.09% long near the equator. The AEQD result is
ellipsoidal and matches `lwgeom` (s2 off) to ~0.001%. Compare against the
ellipsoidal reference, not the s2 default.

### Shiny wiring

- **One connection**, opened in `georef_server()` and passed down as a reactive
  (`con_r`). Modules must not open their own — that leaks handles and can hold
  a lock on the store.
- **The map is re-rendered, not proxied**, to clear drawn shapes. leafpm adds
  layers outside leaflet's group system, so `leafletProxy` cannot reliably
  remove them. The view is captured from `input$map_center` / `input$map_zoom`
  and restored on rebuild, which keeps the rebuild invisible and makes the view
  sticky between neighbouring localities.
- **leafpm circles** arrive as GeoJSON Points carrying `properties$radius` in
  metres. `draw_features_to_sfc()` carries that out of band as a `radius`
  attribute so buffering happens in a metric projection, not in degrees.
- **`stop_on_close` counts sessions and waits.** `session$onSessionEnded(stopApp)`
  on its own kills the app on every page reload, because a reload ends the old
  session before the new one connects. `georef_server()` keeps the count in its
  own frame (not the session function's) and stops after a 2 s delay only if
  nothing reconnected. It defaults to `FALSE` and `launch()` passes `TRUE`:
  `app.R` deploys the same server to shinyapps.io, where one user closing a tab
  must not take the app down for everyone.
- **reactable selection** is read with `getReactableState("localities",
  "selected")` using the *bare* id inside a module — it reads `session$input`,
  which is already unnamespaced.
- `store_localities()` sorts by `n_records` only, never by status. Status must
  not reorder rows, or the row a user just selected would move under them.

### Candidates are matched on token rarity, not string similarity

`R/tokens.R` weights each word by inverse document frequency over the whole
dictionary. This is what makes the matching work across French, English,
Portuguese and local languages without a stoplist: the corpus decides that
`village` is worthless and `Odzala` is decisive. Two figures come out, and they
answer different questions —

* **coverage**: share of the *query's* weight the candidate accounts for. The
  primary ranking signal, because the question is "where are the localities
  mentioning this place", not "which localities are worded like this one".
* **overlap**: weighted Jaccard, which penalises a candidate for saying more.
  It breaks ties. Note it penalises *information*, not length: a short name
  made entirely of corpus-unique words ranks low, because it is a specific
  other place rather than a variant.

Tokens above `df_max_frac` (5%) of the corpus are counted but **not posted**.
They cannot discriminate, searching them means scanning tens of thousands of
postings for nothing, and their absence is most of what keeps the snapshot
small. `weigh_query()` therefore excludes them from the denominator too — a
query is scored on what is searchable, not on what was typed.

Token rarity cannot repair spelling. `odzola` for `odzala` is its own rare
token and matches only other records carrying the same typo; a test asserts
this. Fixing it needs a character-level measure (`pg_trgm`, `stringdist`), not
a tuning change here.

### The gazetteer is a local snapshot, never a live query

`R/gazetteer.R` builds a SQLite file holding the dictionary and its inverted
index, and the app opens it read-only. A georeferencing session must not stall
on a remote database, and `gazetteer_snapshot` on each decision names the exact
file the candidates came from, so what was on screen is reconstructible.
Rebuild with `inst/scripts/build_gazetteer_snapshot.R`; wire in with
`use_gazetteer(path)` or `launch(gazetteer = path)`.

Snapshots include **ungeoreferenced** localities (`is_georeferenced`). A
similar name with no coordinates is still evidence, usually that the same place
is filed under another spelling. They are listed greyed and are not plotted —
candidate coordinates are medians of records, so they get bare markers and no
uncertainty circle. `candidates_empty()` deliberately has no
`coordinate_uncertainty_m` for the same reason.

`R/candidates.R` fixes the provider contract. The default returns nothing —
not a placeholder, but the state the app must stay usable in, since most of
Central Africa is in no gazetteer. A provider that errors degrades to no
candidates and never blocks the loop.

## Dependencies

`datamods` and `phosphoricons` are used by `ConApp` but are **not installed
here** — the import UI is deliberately hand-rolled with `fileInput` instead.
Check availability before reaching for a ConApp idiom.

## Domain constraints

- Output must stay field-compatible with [GeoPick](https://geopick.gbif.org):
  same Darwin Core terms, same semantics. Non-DwC columns are prefixed
  `georefapp*`.
- `pointRadiusSpatialFit` follows the DwC definition (circle area ÷ footprint
  area; `NA` where the footprint has no area). The DwC value 0 cannot arise
  here — see the note in `spatial_fit()`.
- Saving a zero radius is refused: a bare marker claims the locality is known
  exactly, which is almost never true.
- GeoPick is **AGPLv3**. Its method may be reimplemented, its Darwin Core field
  set copied; its source must not be ported into this package.
