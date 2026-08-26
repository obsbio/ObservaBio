#!/usr/bin/env Rscript
# Gera um .kmz de area fragmentada e mostra o diagnostico dos blocos.
#
#   Rscript area_fragmentada.R [n_parcelas] [espalhamento_graus] [saida.kmz]
#
# Exemplos:
#   Rscript area_fragmentada.R 12 1.5 area12.kmz
#   Rscript area_fragmentada.R 40 3.0 area40.kmz

args <- commandArgs(trailingOnly = TRUE)
n      <- as.integer(if (length(args) >= 1) args[[1]] else 12)
spread <- as.numeric(if (length(args) >= 2) args[[2]] else 1.5)
out    <- if (length(args) >= 3) args[[3]] else sprintf("area_%dparcelas.kmz", n)

pkgload::load_all(".", quiet = TRUE)
set.seed(7)

# Talhoes de ~4 km de lado, sorteados no Cerrado a partir de Brasilia.
ring <- function(cx, cy, h = 0.02) sprintf(
  "%f,%f,850 %f,%f,850 %f,%f,850 %f,%f,850 %f,%f,850",
  cx - h, cy - h, cx + h, cy - h, cx + h, cy + h, cx - h, cy + h, cx - h, cy - h)
marks <- vapply(seq_len(n), function(i) {
  cx <- -48.5 + stats::runif(1, 0, spread)
  cy <- -16.0 + stats::runif(1, 0, spread)
  sprintf(paste0("<Placemark><name>Talhao %d</name><Polygon><outerBoundaryIs>",
                 "<LinearRing><coordinates>%s</coordinates></LinearRing>",
                 "</outerBoundaryIs></Polygon></Placemark>"), i, ring(cx, cy))
}, character(1))

tmp <- tempfile("kmzsrc"); dir.create(tmp)
half <- ceiling(n / 2)
writeLines(c('<?xml version="1.0" encoding="UTF-8"?>',
  '<kml xmlns="http://www.opengis.net/kml/2.2"><Document>',
  '<Folder><name>Talhoes</name>', marks[seq_len(half)], '</Folder>',
  '<Folder><name>Reserva</name>', marks[-seq_len(half)], '</Folder>',
  '</Document></kml>'), file.path(tmp, "doc.kml"))
unlink(out)
old <- setwd(tmp); utils::zip(normalizePath(out, mustWork = FALSE), "doc.kml", flags = "-qj"); setwd(old)

# ---- diagnostico ----------------------------------------------------------
area <- geo_resolve_areas(unpack_area_files(out, basename(out))$areas)[[1]]$geom
buf  <- geo_buffer(area)$buffer
exact <- as.numeric(sf::st_area(sf::st_union(sf::st_geometry(buf))))
umso  <- as.numeric(sf::st_area(sf::st_as_sfc(gbif_occ_wkt(buf), crs = 4326))) / exact
blocos <- gbif_occ_query_blocks(buf)
dividido <- sum(vapply(blocos, function(b)
  as.numeric(sf::st_area(sf::st_as_sfc(gbif_occ_wkt(b), crs = 4326))), numeric(1))) / exact

cat(sprintf("\narquivo   : %s\n", out))
cat(sprintf("parcelas  : %d desenhadas -> %d partes no buffer (10 km funde vizinhas)\n",
            n, length(unclass(sf::st_geometry(buf)[[1]]))))
cat(sprintf("um poligono so : 1 requisicao, excesso %6.2fx%s\n", umso,
            if (umso > .GBIF_QUERY_MAX_OVERFETCH) "   <- arriscado" else ""))
cat(sprintf("em blocos      : %d requisicoes, excesso %6.2fx\n\n", length(blocos), dividido))
