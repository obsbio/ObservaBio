#!/usr/bin/env Rscript
# Confere a resposta do app contra uma verdade de referencia.
#
#   Rscript conferir_area.R <area.kmz> "Especie um" "Especie dois" ...
#
# Referencia: consulta CADA parte do buffer com o poligono dela, que e justo e
# nao tem excesso. Isso e o que o app deveria achar. Se a consulta em blocos
# achar tudo que a referencia acha, esta correta.

args <- commandArgs(trailingOnly = TRUE)
kmz  <- args[[1]]
sp   <- args[-1]
pkgload::load_all(".", quiet = TRUE)

area <- geo_resolve_areas(unpack_area_files(kmz, basename(kmz))$areas)[[1]]$geom
buf  <- geo_buffer(area)$buffer
partes <- suppressWarnings(sf::st_cast(sf::st_union(sf::st_geometry(buf)), "POLYGON"))
blocos <- gbif_occ_query_blocks(buf)

cat(sprintf("\narea: %s | %d partes | %d blocos | %d especies\n\n",
            basename(kmz), length(partes), length(blocos), length(sp)))

# --- o que o app responde ---------------------------------------------------
.gbif_occ_cache$wkt <- NULL; .gbif_occ_cache$by_name <- NULL
app <- gbif_occ_in_buffer(sp, buf, throttle = 0.5)
app_n <- vapply(sp, function(s) sum(app$species == s), integer(1))

# --- a referencia: uma consulta por parte, poligono justo -------------------
ref_n <- integer(length(sp)); names(ref_n) <- sp
for (i in seq_along(partes)) {
    parte <- partes[i]
    wkt <- gbif_occ_wkt(parte)
    for (s in sp) {
        pts <- gbif_occ_fetch_species(s, wkt)
        if (isTRUE(attr(pts, "gbif_error"))) {
            cat("  !! falha na referencia:", s, "parte", i, "\n"); next
        }
        if (nrow(pts) > 0L) pts$species <- s
        ref_n[[s]] <- ref_n[[s]] + nrow(.gbif_occ_refine(pts, parte))
        Sys.sleep(0.5)
    }
}

cat(sprintf("%-32s %8s %10s   %s\n", "especie", "app", "referencia", "veredito"))
for (s in sp) {
    a <- app_n[[s]]; r <- ref_n[[s]]
    v <- if (r == 0 && a == 0) "ok (ambos vazios)"
         else if (a > 0 && r > 0) "ok (ambos acharam)"
         else if (a == 0 && r > 0) "FALHA: app perdeu registro"
         else "app achou a mais (ok, refino e exato)"
    cat(sprintf("%-32s %8d %10d   %s\n", s, a, r, v))
}
cat("\n")
