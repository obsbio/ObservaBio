# Notices

ObservaBio is licensed under the GNU General Public License version 3. The full
text is in `LICENSE.md`. This file records what that license does **not** cover.

## Third-party R packages

ObservaBio depends on R packages that are not covered by its license and remain
under their own terms. Every package, with its version, is pinned in
`renv.lock`.

Three direct dependencies are GPL (>= 3) and are the reason ObservaBio is GPL-3:

| Package | License | Role |
| --- | --- | --- |
| `terra` | GPL (>= 3) | Raster and vector geometry |
| `florabr` | GPL (>= 3) | Flora e Funga do Brasil access |
| `faunabr` | GPL (>= 3) | Catalogo Taxonomico da Fauna do Brasil access |

`htmltools` is GPL (>= 2). `sf` is dual-licensed GPL-2 or MIT. The remaining
direct dependencies are MIT or BSD-2-Clause.

### Two GPL-2-only dependencies, and why they are not a blocker

Two packages in the dependency closure are **GPL-2 only**, which does not combine
with GPL-3 in the strict reading:

| Package | License | Required by |
| --- | --- | --- |
| `units` | GPL-2 | `sf` |
| `doSNOW` | GPL-2 | `florabr` |

**ObservaBio does not redistribute either one.** The deploy bundle carries only
this project's own source — `app.R`, `DESCRIPTION`, `NAMESPACE`, `renv.lock`,
`R/` and `inst/`. The hosting platform installs every dependency from CRAN at
build time, pinned by `renv.lock`. What ships is code that *depends on* those
packages, not the packages.

The same situation already exists upstream: `florabr` is itself GPL (>= 3) and
depends on `doSNOW`, and it is published on CRAN. This is recorded here because
it is a fact worth knowing, not because it changes the license choice. It is not
legal advice.

## Reference data

The taxonomic, conservation and geographic reference data shipped under
`inst/extdata/` comes from third-party publishers. **The GPL-3 does not apply to
that data.** It stays under the terms of its publisher and is redistributed here
for use inside this application.

| Dataset | Source |
| --- | --- |
| `florabr_observabio.rds` | Flora e Funga do Brasil (JBRJ) |
| `faunabr_observabio.rds` | Catalogo Taxonomico da Fauna do Brasil |
| `br_uf_biomes.rds` | IBGE, through `geobr` |
| `sensitive_species.rds` | Lista Nacional de Especies Ameacadas (MMA) |
| `invasive_species.rds` | Instituto Horus, GRIIS Brasil, and the federal protected-area list |

Provenance and version for each dataset are in the `*.meta.json` sidecar next to
its `.rds`. The app reports those versions on screen — Step 2 and the "Como
usar" panel.

## Live services

The app queries the GBIF backbone and the GBIF occurrence API at run time, and
the IUCN Red List API when a key is supplied. Those services have their own
terms of use, and this notice grants no rights in them.

> **Open item.** The IUCN Red List terms for commercial use are not verified.
> See `docs/DECISIONS.md` ADR-022.
