# Title: ZHOUSE DwC extraction and standardization — Base-09-05-26.xlsx
# Author: Rogerio Nunes Oliveira
# Date: 2026-05-12
# Version: 1.0

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(stringi)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(writexl)
  library(florabr)
  library(faunabr)
  library(taxadb)
  library(rredlist)
})

input_path <- "Base-09-05-26.xlsx"
template_path <- "Template_lista_especies.xlsx"
criteria_path <- "docs/criterio_species_brasil.md"
output_dir <- "outputs"
output_path <- file.path(output_dir, "dwc_zhouse_090526.xlsx")
audit_output_path <- file.path(output_dir, "dwc_zhouse_090526_auditoria.xlsx")

# ── Helper functions (identical to zhouse_dwc_minimax.R) ─────────────────────

ensure_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      df[[col]] <- NA_character_
    }
  }
  df
}

coalesce_columns <- function(df, target, candidates) {
  keep <- intersect(candidates, names(df))
  if (length(keep) == 0) {
    df[[target]] <- NA_character_
    return(df)
  }
  out <- as.character(df[[keep[[1]]]])
  if (length(keep) > 1) {
    for (k in keep[-1]) {
      out <- dplyr::coalesce(out, as.character(df[[k]]))
    }
  }
  df[[target]] <- out
  df
}

to_ascii <- function(x) {
  stringi::stri_trans_general(as.character(x), "Latin-ASCII")
}

norm_key <- function(x) {
  x %>%
    as.character() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("[ ]", " ") %>%
    str_squish()
}

is_non_empty <- function(x) {
  !is.na(x) & nzchar(str_squish(as.character(x)))
}

split_semicolon <- function(x) {
  if (!is_non_empty(x)) {
    return(character(0))
  }
  str_split(as.character(x), "\\s*;\\s*")[[1]] %>%
    str_squish() %>%
    discard(~ !nzchar(.x))
}

combine_pipe <- function(x) {
  x <- as.character(x)
  x <- x[is_non_empty(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }
  x <- unique(x)
  paste(x, collapse = " | ")
}

combine_pipe_expanded <- function(x) {
  x <- as.character(x)
  x <- x[is_non_empty(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }
  parts <- unlist(str_split(x, "\\s*\\|\\s*"))
  parts <- str_squish(parts)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) {
    return(NA_character_)
  }
  parts <- unique(parts)
  paste(parts, collapse = " | ")
}

first_non_empty <- function(x) {
  x <- as.character(x)
  x <- x[is_non_empty(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }
  x[[1]]
}

pt_title_case <- function(x) {
  if (!is_non_empty(x)) {
    return(NA_character_)
  }
  x <- str_squish(as.character(x))
  low <- str_to_lower(x)
  words <- str_split(low, "\\s+")[[1]]
  small_words <- c("de", "da", "do", "dos", "das", "e")
  words <- map_chr(seq_along(words), function(i) {
    w <- words[[i]]
    if (i > 1 && w %in% small_words) {
      return(w)
    }
    str_to_title(w)
  })
  out <- paste(words, collapse = " ")
  out <- str_replace_all(out, "\\bAs\\b", "AS")
  out <- str_replace_all(out, "\\bObsbio\\b", "ObsBio")
  out
}

clean_scientific_name <- function(x) {
  if (!is_non_empty(x)) {
    return(NA_character_)
  }
  x %>%
    as.character() %>%
    str_replace_all("[ ]", " ") %>%
    str_squish()
}

has_uncertainty_marker <- function(x) {
  if (!is_non_empty(x)) {
    return(FALSE)
  }
  x_norm <- norm_key(x)
  str_detect(x_norm, "\\b(cf|aff|gr)\\.?\\b") ||
    str_detect(x_norm, "\\bsp\\.?\\d*\\b") ||
    str_detect(x_norm, "\\bspp\\.?\\b") ||
    str_detect(x_norm, "\\bsp\\.?\\s*nov\\.?\\b")
}

has_placeholder_marker <- function(x) {
  if (!is_non_empty(x)) {
    return(FALSE)
  }
  x_norm <- norm_key(x)
  str_detect(x_norm, "^\\S+\\s+(sp\\.?\\d*|spp\\.?)\\b")
}

is_lower_taxon_token <- function(token) {
  token_norm <- token %>%
    to_ascii() %>%
    str_to_lower()
  str_detect(token_norm, "^[a-z][a-z\\-]+$")
}

canonical_name <- function(x) {
  x <- clean_scientific_name(x)
  if (!is_non_empty(x)) {
    return(NA_character_)
  }

  x_clean <- x %>%
    str_replace_all("(?i)\\b(cf|aff|gr)\\.?\\b", " ") %>%
    str_replace_all("(?i)\\bsp\\.?\\s*nov\\.?\\b", " ") %>%
    str_squish()

  tokens <- str_split(x_clean, "\\s+")[[1]]
  if (length(tokens) == 0) {
    return(NA_character_)
  }

  genus <- tokens[[1]]
  if (length(tokens) == 1) {
    return(genus)
  }

  second <- tokens[[2]]
  if (str_detect(str_to_lower(second), "^sp\\.?\\d*$|^spp\\.?$")) {
    return(genus)
  }

  if (!is_lower_taxon_token(second)) {
    return(genus)
  }

  if (length(tokens) >= 4 && str_to_lower(tokens[[3]]) %in% c("subsp.", "ssp.", "var.", "f.", "forma")) {
    marker <- str_to_lower(tokens[[3]])
    marker <- case_when(
      marker %in% c("subsp.", "ssp.") ~ "subsp.",
      marker == "var." ~ "var.",
      TRUE ~ "f."
    )
    if (is_lower_taxon_token(tokens[[4]])) {
      return(paste(genus, second, marker, tokens[[4]]))
    }
    return(paste(genus, second))
  }

  if (length(tokens) >= 3 && is_lower_taxon_token(tokens[[3]])) {
    return(paste(genus, second, tokens[[3]]))
  }

  paste(genus, second)
}

safe_rank_name <- function(x) {
  can <- canonical_name(x)
  if (!is_non_empty(can)) {
    return(NA_character_)
  }
  if (has_uncertainty_marker(x) || has_placeholder_marker(x)) {
    return(str_split(can, "\\s+")[[1]][1])
  }
  can
}

normalize_rank <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
  case_when(
    x %in% c("species", "especie", "especie.") ~ "species",
    x %in% c("subspecies", "subspecie", "subespecie") ~ "subspecies",
    x %in% c("variety", "variedade", "var.") ~ "variety",
    x %in% c("form", "forma", "f.") ~ "form",
    x %in% c("genus", "genero") ~ "genus",
    TRUE ~ x
  )
}

normalize_taxonomic_status <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
  case_when(
    x %in% c("valid", "accepted", "nome_aceito", "aceito") ~ "accepted",
    x %in% c("synonym", "sinonimo") ~ "synonym",
    TRUE ~ x
  )
}

parse_rank_from_name <- function(x) {
  if (!is_non_empty(x)) {
    return(list(
      taxonRank = NA_character_,
      genus = NA_character_,
      specificEpithet = NA_character_,
      infraspecificEpithet = NA_character_
    ))
  }

  tokens <- str_split(str_squish(as.character(x)), "\\s+")[[1]]
  genus <- tokens[[1]]

  if (length(tokens) == 1) {
    return(list(
      taxonRank = "genus",
      genus = genus,
      specificEpithet = NA_character_,
      infraspecificEpithet = NA_character_
    ))
  }

  if (length(tokens) >= 4 && str_to_lower(tokens[[3]]) %in% c("subsp.", "ssp.", "var.", "f.")) {
    rank <- case_when(
      str_to_lower(tokens[[3]]) %in% c("subsp.", "ssp.") ~ "subspecies",
      str_to_lower(tokens[[3]]) == "var." ~ "variety",
      TRUE ~ "form"
    )
    return(list(
      taxonRank = rank,
      genus = genus,
      specificEpithet = tokens[[2]],
      infraspecificEpithet = tokens[[4]]
    ))
  }

  if (length(tokens) >= 3 && is_lower_taxon_token(tokens[[3]])) {
    return(list(
      taxonRank = "subspecies",
      genus = genus,
      specificEpithet = tokens[[2]],
      infraspecificEpithet = tokens[[3]]
    ))
  }

  list(
    taxonRank = "species",
    genus = genus,
    specificEpithet = tokens[[2]],
    infraspecificEpithet = NA_character_
  )
}

status_rank <- function(x) {
  map <- c(
    "EX" = 8,
    "EW" = 7,
    "CR (PEX)" = 6,
    "CR" = 5,
    "EN" = 4,
    "VU" = 3,
    "NT" = 2,
    "LC" = 1
  )
  unname(map[str_trim(as.character(x))])
}

pick_most_threatened <- function(x) {
  x <- as.character(x)
  x <- x[is_non_empty(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }
  rk <- status_rank(x)
  if (all(is.na(rk))) {
    return(NA_character_)
  }
  x[[which.max(rk)]]
}

parse_status_string <- function(x) {
  if (!is_non_empty(x)) {
    return(NA_character_)
  }
  parts <- str_split(as.character(x), "\\s*[;|,]\\s*")[[1]]
  parts <- str_trim(str_to_upper(parts))
  parts <- str_replace_all(parts, "\\s+", " ")
  parts <- if_else(parts == "CR(PEX)", "CR (PEX)", parts)
  parts <- parts[parts %in% c("EX", "EW", "CR (PEX)", "CR", "EN", "VU", "NT", "LC")]
  if (length(parts) == 0) {
    return(NA_character_)
  }
  pick_most_threatened(parts)
}

format_status_label <- function(x) {
  if (length(x) == 0) {
    return(NA_character_)
  }
  x <- as.character(x)
  x <- str_trim(str_to_upper(x))
  x <- str_replace_all(x, "\\s+", " ")
  x[x == "CR(PEX)"] <- "CR (PEX)"

  map <- c(
    "EX" = "(EX) Extinta",
    "EW" = "(EW) Extinta na Natureza",
    "CR (PEX)" = "(CR (PEX)) Criticamente Em Perigo (Possivelmente Extinta)",
    "CR" = "(CR) Criticamente Em Perigo",
    "EN" = "(EN) Em Perigo",
    "VU" = "(VU) Vulneravel",
    "NT" = "(NT) Quase Ameacada",
    "LC" = "(LC) Pouco Preocupante"
  )

  unname(map[x])
}

parse_criteria_md <- function(path) {
  if (!file.exists(path)) {
    return(tibble(species = character(0), category = character(0)))
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  tbl_lines <- lines[str_detect(lines, "^\\|")]
  if (length(tbl_lines) == 0) {
    return(tibble(species = character(0), category = character(0)))
  }

  out <- map_dfr(tbl_lines, function(line) {
    parts <- str_split(line, "\\|")[[1]] %>% str_trim()
    parts <- parts[parts != ""]
    if (length(parts) < 3) {
      return(NULL)
    }
    cat_idx <- which(str_detect(parts, "^(CR \\(PEX\\)|CR|EN|VU|NT|LC|EW|EX)$"))
    if (length(cat_idx) == 0) {
      return(NULL)
    }
    idx <- cat_idx[[length(cat_idx)]]
    if (idx <= 1) {
      return(NULL)
    }
    species <- parts[[idx - 1]]
    if (!is_non_empty(species)) {
      return(NULL)
    }
    if (str_detect(norm_key(species), "familia|especie|anexo|categoria")) {
      return(NULL)
    }
    tibble(species = species, category = parts[[idx]])
  })

  out
}

map_origin_to_establishment <- function(x) {
  x_norm <- norm_key(x)
  case_when(
    x_norm %in% c("native", "nativo", "nativa") ~ "native",
    x_norm %in% c("naturalized", "naturalizado", "naturalizada") ~ "naturalized",
    x_norm %in% c("cultivated", "cultivado", "cultivada") ~ "cultivated",
    x_norm %in% c("introduced", "introduzido", "introduzida") ~ "introduced",
    x_norm %in% c("invasive", "invasora", "invasor") ~ "invasive",
    x_norm %in% c("domesticated", "domesticaded", "domesticado", "domesticada") ~ "domesticated",
    x_norm %in% c("cryptogenic", "criptogenico", "criptogenica") ~ "cryptogenic",
    TRUE ~ ""
  )
}

normalize_endemism <- function(x) {
  x_norm <- norm_key(x)
  case_when(
    x_norm %in% c("endemic", "endemica", "endemico") ~ "Endemic",
    x_norm %in% c("non-endemic", "nao endemica", "nao endemico", "non endemic") ~ "Non-endemic",
    TRUE ~ ""
  )
}

get_iucn_assessment <- function(genus, species, infra, key) {
  empty <- tibble(statusIUCN = NA_character_, criteria = NA_character_)
  if (!is_non_empty(genus) || !is_non_empty(species) || !is_non_empty(key)) {
    return(empty)
  }

  res <- tryCatch(
    rredlist::rl_species_latest(
      genus = genus,
      species = species,
      infra = if (is_non_empty(infra)) infra else NULL,
      key = key,
      parse = TRUE
    ),
    error = function(e) NULL
  )

  if (is.null(res)) {
    return(empty)
  }

  if (is.data.frame(res) && nrow(res) > 0) {
    return(tibble(
      statusIUCN = if ("category" %in% names(res)) as.character(res$category[[1]]) else NA_character_,
      criteria = if ("criteria" %in% names(res)) as.character(res$criteria[[1]]) else NA_character_
    ))
  }

  if (is.list(res)) {
    status_iucn <- NA_character_
    if (!is.null(res$category)) {
      status_iucn <- as.character(res$category[[1]])
    } else if (!is.null(res$red_list_category) &&
      !is.null(res$red_list_category$code)) {
      status_iucn <- as.character(res$red_list_category$code[[1]])
    }
    return(tibble(
      statusIUCN = status_iucn,
      criteria = if (!is.null(res$criteria)) as.character(res$criteria[[1]]) else NA_character_
    ))
  }

  empty
}

normalize_group <- function(x) {
  if (!is_non_empty(x)) {
    return("unknown")
  }
  x_norm <- norm_key(x)
  if (str_detect(x_norm, "fung")) {
    return("fungi")
  }
  if (str_detect(x_norm, "flora|planta")) {
    return("planta")
  }
  "fauna"
}

normalize_operation_key <- function(x) {
  norm_key(x)
}

# ── Data loading ─────────────────────────────────────────────────────────────

base_raw <- readxl::read_excel(input_path)
names(base_raw) <- names(base_raw) %>%
  to_ascii() %>%
  str_replace_all("[ ]", " ") %>%
  str_squish()

# "Grupo alvo" (sem hífen) → "Grupo-alvo" para reutilizar normalize_group
if ("Grupo alvo" %in% names(base_raw) && !"Grupo-alvo" %in% names(base_raw)) {
  base_raw <- dplyr::rename(base_raw, `Grupo-alvo` = `Grupo alvo`)
}

required_cols <- c("Nome cientifico", "Grupo-alvo", "locality")

missing_cols <- setdiff(required_cols, names(base_raw))
if (length(missing_cols) > 0) {
  stop("Missing columns: ", paste(missing_cols, collapse = ", "))
}

# Remove linhas que sejam cabeçalho duplicado
header_row <- norm_key(base_raw$`Grupo-alvo`) == "grupo" &
  norm_key(base_raw$`Nome cientifico`) == "nome cientifico"
base_raw <- base_raw %>% filter(!header_row)

op_to_state <- c(
  "fazenda bananal" = "RJ",
  "rppn rio do brasil" = "BA",
  "fazenda trijuncao" = "BA",
  "trijuncao" = "BA"
)

# ── Column mapping adaptado para Base-09-05-26.xlsx ──────────────────────────
# locality, datasetName e informationWithheld já vêm prontos neste arquivo.
# Não há: Operacao, Projeto, Nome popular, Status conservacao nacional,
#          Nativa BR, Endemica BR.

base <- base_raw %>%
  mutate(
    row_id = row_number(),
    locality_raw     = as.character(.data$locality),
    datasetName_raw  = if ("datasetName" %in% names(.)) as.character(.data$datasetName) else NA_character_,
    infoWithheld_raw = if ("informationWithheld" %in% names(.)) as.character(.data$informationWithheld) else NA_character_,
    name_raw         = as.character(`Nome cientifico`),
    name_clean       = map_chr(name_raw, clean_scientific_name),
    name_query       = map_chr(name_raw, canonical_name),
    safe_name        = map_chr(name_raw, safe_rank_name),
    uncertainty_flag = map_lgl(name_raw, has_uncertainty_marker),
    placeholder_flag = map_lgl(name_raw, has_placeholder_marker),
    group_type       = map_chr(`Grupo-alvo`, normalize_group),
    name_key         = norm_key(name_clean)
  ) %>%
  mutate(
    datasetName = map_chr(datasetName_raw, ~ if (is_non_empty(.x)) str_squish(.x) else NA_character_),
    informationWithheld = map_chr(infoWithheld_raw, ~ if (is_non_empty(.x)) str_squish(.x) else NA_character_),
    locality = map_chr(locality_raw, ~ if (is_non_empty(.x)) str_squish(.x) else NA_character_),
    stateProvince = map_chr(locality_raw, function(loc) {
      key   <- normalize_operation_key(loc)
      state <- unname(op_to_state[key])
      if (all(is.na(state))) NA_character_ else state[!is.na(state)][[1]]
    }),
    # Colunas ausentes no novo arquivo
    status_base   = NA_character_,
    nativa_flag   = FALSE,
    endemica_flag = NA
  )

# ── Critérios MMA (se existir docs/criterio_species_brasil.md) ───────────────

criteria_raw <- parse_criteria_md(criteria_path)
criteria_lookup <- criteria_raw %>%
  mutate(species_key = norm_key(species)) %>%
  group_by(species_key) %>%
  summarise(category = pick_most_threatened(category), .groups = "drop")

# ── Flora BR ─────────────────────────────────────────────────────────────────

flora_source_path <- "data/florabr/393.422/CompleteBrazilianFlora.rds"
if (file.exists(flora_source_path)) {
  flora_src <- readRDS(flora_source_path)
} else {
  data("bf_data", package = "florabr")
  flora_src <- bf_data
}
flora_src <- as_tibble(flora_src)
flora_src <- ensure_cols(
  flora_src,
  c(
    "id", "taxonID", "species", "scientificName", "acceptedName",
    "kingdom", "phylum", "class", "order", "family", "genus",
    "specificEpithet", "infraspecificEpithet", "taxonRank",
    "scientificNameAuthorship", "taxonomicStatus", "vernacularName",
    "origin", "endemism"
  )
)
flora_src <- flora_src %>%
  mutate(
    id = as.character(id),
    taxonID = coalesce(as.character(taxonID), as.character(id)),
    taxonRank = normalize_rank(taxonRank),
    taxonomicStatus = normalize_taxonomic_status(taxonomicStatus),
    scientificNameAuthorship = as.character(scientificNameAuthorship)
  )

data("fauna_data", package = "faunabr")
fauna_src <- as_tibble(fauna_data)
if (!"taxonID" %in% names(fauna_src) && "id" %in% names(fauna_src)) {
  fauna_src$taxonID <- fauna_src$id
}
fauna_src <- ensure_cols(
  fauna_src,
  c(
    "id", "taxonID", "species", "subspecies", "scientificName", "validName",
    "kingdom", "phylum", "class", "order", "family", "genus",
    "specificEpithet", "infraspecificEpithet", "taxonRank",
    "scientificNameAuthorship", "taxonomicStatus", "vernacularName",
    "origin"
  )
)
fauna_src <- fauna_src %>%
  mutate(
    id = as.character(id),
    taxonID = coalesce(as.character(taxonID), as.character(id)),
    taxonRank = normalize_rank(taxonRank),
    taxonomicStatus = normalize_taxonomic_status(taxonomicStatus),
    scientificNameAuthorship = as.character(scientificNameAuthorship)
  )

flora_lookup <- bind_rows(
  flora_src %>% transmute(
    match_name = species,
    taxonID, scientificName, taxonRank, scientificNameAuthorship,
    taxonomicStatus, kingdom, phylum, class = .data$class, order, family, genus,
    specificEpithet, infraspecificEpithet, origin, endemism
  ),
  flora_src %>% transmute(
    match_name = scientificName,
    taxonID, scientificName, taxonRank, scientificNameAuthorship,
    taxonomicStatus, kingdom, phylum, class = .data$class, order, family, genus,
    specificEpithet, infraspecificEpithet, origin, endemism
  ),
  flora_src %>% transmute(
    match_name = acceptedName,
    taxonID, scientificName, taxonRank, scientificNameAuthorship,
    taxonomicStatus, kingdom, phylum, class = .data$class, order, family, genus,
    specificEpithet, infraspecificEpithet, origin, endemism
  )
) %>%
  filter(is_non_empty(match_name)) %>%
  mutate(match_key = norm_key(match_name)) %>%
  arrange(desc(taxonomicStatus %in% c("accepted", "valid"))) %>%
  distinct(match_key, .keep_all = TRUE)

fauna_lookup <- bind_rows(
  fauna_src %>% transmute(
    match_name = species,
    taxonID, scientificName, taxonRank, scientificNameAuthorship,
    taxonomicStatus, kingdom, phylum, class = .data$class, order, family, genus,
    specificEpithet, infraspecificEpithet, origin
  ),
  fauna_src %>% transmute(
    match_name = scientificName,
    taxonID, scientificName, taxonRank, scientificNameAuthorship,
    taxonomicStatus, kingdom, phylum, class = .data$class, order, family, genus,
    specificEpithet, infraspecificEpithet, origin
  ),
  fauna_src %>% transmute(
    match_name = validName,
    taxonID, scientificName, taxonRank, scientificNameAuthorship,
    taxonomicStatus, kingdom, phylum, class = .data$class, order, family, genus,
    specificEpithet, infraspecificEpithet, origin
  )
) %>%
  filter(is_non_empty(match_name)) %>%
  mutate(match_key = norm_key(match_name)) %>%
  arrange(desc(taxonomicStatus %in% c("accepted", "valid"))) %>%
  distinct(match_key, .keep_all = TRUE)

# ── Name index & validation ───────────────────────────────────────────────────

name_index <- base %>%
  filter(is_non_empty(name_key)) %>%
  distinct(name_key, group_type, .keep_all = TRUE) %>%
  select(
    name_key, group_type, name_clean, name_query, safe_name,
    uncertainty_flag, placeholder_flag
  )

run_florabr_check <- function(name_vec, kingdom, source_data) {
  name_vec <- unique(name_vec[is_non_empty(name_vec)])
  if (length(name_vec) == 0) {
    return(tibble())
  }
  out <- tryCatch(
    florabr::check_names(
      data = source_data,
      species = name_vec,
      include_subspecies = TRUE,
      include_variety = TRUE,
      kingdom = kingdom,
      parallel = FALSE,
      progress_bar = FALSE
    ),
    error = function(e) tibble(input_name = name_vec)
  )
  as_tibble(out) %>% mutate(kingdom_checked = kingdom)
}

flora_idx <- name_index %>%
  filter(group_type %in% c("planta", "fungi"), is_non_empty(name_query))

flora_check <- bind_rows(
  run_florabr_check(
    flora_idx %>% filter(group_type == "planta") %>% pull(name_query),
    kingdom = "Plantae",
    source_data = flora_src
  ),
  run_florabr_check(
    flora_idx %>% filter(group_type == "fungi") %>% pull(name_query),
    kingdom = "Fungi",
    source_data = flora_src
  )
) %>%
  rename_with(~ str_replace_all(.x, " ", "_"))

flora_check <- ensure_cols(flora_check, c("input_name", "Spelling", "Suggested_name", "acceptedName"))
flora_check <- flora_check %>%
  mutate(
    suggested_first = map_chr(Suggested_name, function(x) {
      if (!is_non_empty(x)) {
        return(NA_character_)
      }
      str_split(as.character(x), "\\s*[;|,]\\s*")[[1]][1] %>% str_squish()
    }),
    validator_candidate = case_when(
      is_non_empty(acceptedName) ~ as.character(acceptedName),
      Spelling %in% c("Correct", "Probably_correct") ~ as.character(input_name),
      Spelling == "Probably_incorrect" & is_non_empty(suggested_first) ~ suggested_first,
      TRUE ~ NA_character_
    )
  )

flora_decisions <- flora_idx %>%
  left_join(flora_check, by = c("name_query" = "input_name")) %>%
  mutate(
    exact_match = norm_key(validator_candidate) == norm_key(name_query),
    final_scientificName = case_when(
      (uncertainty_flag | placeholder_flag) & exact_match & is_non_empty(validator_candidate) ~ validator_candidate,
      (uncertainty_flag | placeholder_flag) ~ safe_name,
      is_non_empty(validator_candidate) ~ validator_candidate,
      TRUE ~ coalesce(name_query, safe_name)
    ),
    validator_used = "florabr",
    match_type = case_when(
      (uncertainty_flag | placeholder_flag) & exact_match ~ "exact_promoted",
      (uncertainty_flag | placeholder_flag) & !exact_match ~ "safe_rank",
      is_non_empty(validator_candidate) & norm_key(validator_candidate) == norm_key(name_query) ~ "exact",
      is_non_empty(validator_candidate) & norm_key(validator_candidate) != norm_key(name_query) ~ "corrected",
      TRUE ~ "not_found"
    ),
    decision_reason = case_when(
      (uncertainty_flag | placeholder_flag) & exact_match ~ "uncertain_exact_match_promoted",
      (uncertainty_flag | placeholder_flag) & !exact_match ~ "uncertain_no_exact_match_safe_rank",
      is_non_empty(validator_candidate) & norm_key(validator_candidate) != norm_key(name_query) ~ "corrected_by_florabr",
      is_non_empty(validator_candidate) ~ "validated_exact_florabr",
      TRUE ~ "not_found_kept_input"
    ),
    final_key = norm_key(final_scientificName)
  ) %>%
  left_join(
    flora_lookup %>%
      select(
        match_key, taxonID, scientificName, taxonRank, scientificNameAuthorship,
        taxonomicStatus, kingdom, phylum, class, order, family, genus,
        specificEpithet, infraspecificEpithet, origin, endemism
      ),
    by = c("final_key" = "match_key")
  )

flora_tax_cols <- c(
  "taxonID", "taxonRank", "scientificNameAuthorship", "taxonomicStatus",
  "kingdom", "phylum", "class", "order", "family", "genus",
  "specificEpithet", "infraspecificEpithet", "origin", "endemism"
)
for (col in flora_tax_cols) {
  flora_decisions <- coalesce_columns(
    flora_decisions,
    col,
    c(col, paste0(col, ".y"), paste0(col, ".x"))
  )
}

taxadb_lookup <- tibble()
taxadb_best <- tibble()

nonflora_idx <- name_index %>%
  filter(!group_type %in% c("planta", "fungi"), is_non_empty(name_query))

if (nrow(nonflora_idx) > 0) {
  gbif_db <- tryCatch(taxadb::td_create("gbif"), error = function(e) NULL)

  if (!is.null(gbif_db)) {
    taxadb_raw <- map_dfr(unique(nonflora_idx$name_query), function(nm) {
      out <- tryCatch(
        taxadb::filter_name(name = nm, provider = "gbif", db = gbif_db, collect = TRUE),
        error = function(e) tibble()
      )
      if (!is.data.frame(out) || nrow(out) == 0) {
        return(tibble(input_name = nm))
      }
      as_tibble(out) %>% mutate(input_name = nm)
    })

    taxadb_raw <- ensure_cols(
      taxadb_raw,
      c(
        "input_name", "taxonID", "acceptedNameUsageID", "scientificName",
        "taxonRank", "scientificNameAuthorship", "taxonomicStatus",
        "kingdom", "phylum", "class", "order", "family", "genus",
        "specificEpithet", "infraspecificEpithet"
      )
    )

    taxadb_raw <- taxadb_raw %>%
      mutate(
        taxonRank = normalize_rank(taxonRank),
        taxonomicStatus = normalize_taxonomic_status(taxonomicStatus),
        input_key = norm_key(input_name),
        sci_key = norm_key(scientificName),
        exact_match = sci_key == input_key,
        status_pref = taxonomicStatus %in% c("accepted", "valid")
      )

    taxadb_best <- taxadb_raw %>%
      group_by(input_name) %>%
      arrange(desc(exact_match), desc(status_pref), desc(is_non_empty(taxonID))) %>%
      slice(1) %>%
      ungroup()

    ids_to_resolve <- taxadb_best %>%
      filter(
        !taxonomicStatus %in% c("accepted", "valid") | !is_non_empty(scientificName),
        is_non_empty(acceptedNameUsageID)
      ) %>%
      pull(acceptedNameUsageID) %>%
      unique()

    if (length(ids_to_resolve) > 0) {
      acc_rows <- map_dfr(ids_to_resolve, function(idv) {
        out <- tryCatch(
          taxadb::filter_id(
            id = idv,
            provider = "gbif",
            type = "acceptedNameUsageID",
            db = gbif_db,
            collect = TRUE
          ),
          error = function(e) tibble()
        )
        if (!is.data.frame(out) || nrow(out) == 0) {
          return(tibble(req_id = idv))
        }
        as_tibble(out) %>% mutate(req_id = idv)
      })

      acc_rows <- ensure_cols(
        acc_rows,
        c(
          "req_id", "taxonID", "scientificName", "taxonRank",
          "scientificNameAuthorship", "taxonomicStatus", "kingdom",
          "phylum", "class", "order", "family", "genus",
          "specificEpithet", "infraspecificEpithet"
        )
      ) %>%
        mutate(
          taxonRank = normalize_rank(taxonRank),
          taxonomicStatus = normalize_taxonomic_status(taxonomicStatus),
          status_pref = taxonomicStatus %in% c("accepted", "valid")
        ) %>%
        group_by(req_id) %>%
        arrange(desc(status_pref), desc(is_non_empty(taxonID))) %>%
        slice(1) %>%
        ungroup()

      taxadb_best <- taxadb_best %>%
        left_join(acc_rows, by = c("acceptedNameUsageID" = "req_id"), suffix = c("", "_acc")) %>%
        mutate(
          use_acc = !taxonomicStatus %in% c("accepted", "valid") | !is_non_empty(scientificName),
          scientificName = if_else(use_acc & is_non_empty(scientificName_acc), scientificName_acc, scientificName),
          taxonID = if_else(use_acc & is_non_empty(taxonID_acc), taxonID_acc, taxonID),
          taxonRank = if_else(use_acc & is_non_empty(taxonRank_acc), taxonRank_acc, taxonRank),
          scientificNameAuthorship = if_else(
            use_acc & is_non_empty(scientificNameAuthorship_acc),
            scientificNameAuthorship_acc,
            scientificNameAuthorship
          ),
          taxonomicStatus = if_else(
            use_acc & is_non_empty(taxonomicStatus_acc),
            taxonomicStatus_acc,
            taxonomicStatus
          ),
          kingdom = if_else(use_acc & is_non_empty(kingdom_acc), kingdom_acc, kingdom),
          phylum = if_else(use_acc & is_non_empty(phylum_acc), phylum_acc, phylum),
          class = if_else(use_acc & is_non_empty(class_acc), class_acc, class),
          order = if_else(use_acc & is_non_empty(order_acc), order_acc, order),
          family = if_else(use_acc & is_non_empty(family_acc), family_acc, family),
          genus = if_else(use_acc & is_non_empty(genus_acc), genus_acc, genus),
          specificEpithet = if_else(
            use_acc & is_non_empty(specificEpithet_acc),
            specificEpithet_acc,
            specificEpithet
          ),
          infraspecificEpithet = if_else(
            use_acc & is_non_empty(infraspecificEpithet_acc),
            infraspecificEpithet_acc,
            infraspecificEpithet
          )
        ) %>%
        select(-ends_with("_acc"), -use_acc)
    }

    taxadb_best <- ensure_cols(
      taxadb_best,
      c(
        "input_name", "taxonID", "scientificName", "taxonRank", "scientificNameAuthorship",
        "taxonomicStatus", "kingdom", "phylum", "class", "order", "family",
        "genus", "specificEpithet", "infraspecificEpithet"
      )
    )

    taxadb_lookup <- taxadb_raw %>%
      mutate(match_key = norm_key(scientificName)) %>%
      filter(is_non_empty(match_key)) %>%
      select(
        match_key, taxonID, scientificName, taxonRank, scientificNameAuthorship,
        taxonomicStatus, kingdom, phylum, class, order, family, genus,
        specificEpithet, infraspecificEpithet
      ) %>%
      arrange(desc(taxonomicStatus %in% c("accepted", "valid"))) %>%
      distinct(match_key, .keep_all = TRUE)
  } else {
    taxadb_best <- tibble(input_name = unique(nonflora_idx$name_query))
  }
}

taxadb_best <- ensure_cols(
  taxadb_best,
  c(
    "input_name", "scientificName", "taxonID", "taxonRank", "scientificNameAuthorship",
    "taxonomicStatus", "kingdom", "phylum", "class", "order", "family",
    "genus", "specificEpithet", "infraspecificEpithet"
  )
)

fauna_check <- tibble()
if (nrow(nonflora_idx) > 0) {
  fauna_check <- tryCatch(
    faunabr::check_fauna_names(
      data = fauna_data,
      species = unique(nonflora_idx$name_query),
      include_subspecies = TRUE
    ),
    error = function(e) tibble(input_name = unique(nonflora_idx$name_query))
  ) %>%
    as_tibble() %>%
    rename_with(~ str_replace_all(.x, " ", "_"))

  fauna_check <- ensure_cols(fauna_check, c("input_name", "Spelling", "Suggested_name", "validName"))
  fauna_check <- fauna_check %>%
    mutate(
      suggested_first = map_chr(Suggested_name, function(x) {
        if (!is_non_empty(x)) {
          return(NA_character_)
        }
        str_split(as.character(x), "\\s*[;|,]\\s*")[[1]][1] %>% str_squish()
      }),
      validator_candidate_fauna = case_when(
        is_non_empty(validName) ~ as.character(validName),
        Spelling %in% c("Correct", "Probably_correct") ~ as.character(input_name),
        Spelling == "Probably_incorrect" & is_non_empty(suggested_first) ~ suggested_first,
        TRUE ~ NA_character_
      ),
      spell_score = case_when(
        Spelling == "Correct" ~ 3L,
        Spelling == "Probably_correct" ~ 2L,
        Spelling == "Probably_incorrect" ~ 1L,
        TRUE ~ 0L
      )
    ) %>%
    arrange(desc(spell_score), desc(is_non_empty(validName)), desc(is_non_empty(suggested_first))) %>%
    group_by(input_name) %>%
    slice(1) %>%
    ungroup() %>%
    select(-spell_score)
}

nonflora_decisions <- nonflora_idx %>%
  left_join(taxadb_best, by = c("name_query" = "input_name"), suffix = c("", "_taxadb")) %>%
  left_join(
    fauna_check %>% select(input_name, Spelling, suggested_first, validator_candidate_fauna),
    by = c("name_query" = "input_name")
  ) %>%
  mutate(
    taxadb_candidate = scientificName,
    exact_taxadb = norm_key(taxadb_candidate) == norm_key(name_query),
    exact_fauna = norm_key(validator_candidate_fauna) == norm_key(name_query),
    final_scientificName = case_when(
      (uncertainty_flag | placeholder_flag) & exact_taxadb & is_non_empty(taxadb_candidate) ~ taxadb_candidate,
      (uncertainty_flag | placeholder_flag) & exact_fauna & is_non_empty(validator_candidate_fauna) ~ validator_candidate_fauna,
      (uncertainty_flag | placeholder_flag) ~ safe_name,
      is_non_empty(taxadb_candidate) ~ taxadb_candidate,
      is_non_empty(validator_candidate_fauna) ~ validator_candidate_fauna,
      TRUE ~ coalesce(name_query, safe_name)
    ),
    validator_used = case_when(
      (uncertainty_flag | placeholder_flag) & exact_taxadb ~ "taxadb",
      (uncertainty_flag | placeholder_flag) & exact_fauna ~ "faunabr",
      is_non_empty(taxadb_candidate) ~ "taxadb",
      is_non_empty(validator_candidate_fauna) ~ "faunabr",
      TRUE ~ "none"
    ),
    match_type = case_when(
      (uncertainty_flag | placeholder_flag) & (exact_taxadb | exact_fauna) ~ "exact_promoted",
      (uncertainty_flag | placeholder_flag) ~ "safe_rank",
      is_non_empty(taxadb_candidate) & norm_key(taxadb_candidate) == norm_key(name_query) ~ "exact",
      is_non_empty(taxadb_candidate) & norm_key(taxadb_candidate) != norm_key(name_query) ~ "corrected",
      is_non_empty(validator_candidate_fauna) & norm_key(validator_candidate_fauna) == norm_key(name_query) ~ "exact",
      is_non_empty(validator_candidate_fauna) & norm_key(validator_candidate_fauna) != norm_key(name_query) ~ "corrected",
      TRUE ~ "not_found"
    ),
    decision_reason = case_when(
      (uncertainty_flag | placeholder_flag) & (exact_taxadb | exact_fauna) ~ "uncertain_exact_match_promoted",
      (uncertainty_flag | placeholder_flag) ~ "uncertain_no_exact_match_safe_rank",
      is_non_empty(taxadb_candidate) & norm_key(taxadb_candidate) != norm_key(name_query) ~ "corrected_by_taxadb",
      is_non_empty(validator_candidate_fauna) & norm_key(validator_candidate_fauna) != norm_key(name_query) ~ "corrected_by_faunabr",
      is_non_empty(taxadb_candidate) | is_non_empty(validator_candidate_fauna) ~ "validated_exact",
      TRUE ~ "not_found_kept_input"
    ),
    final_key = norm_key(final_scientificName)
  ) %>%
  left_join(
    taxadb_lookup %>%
      rename_with(~ paste0(.x, "_tx"), -match_key),
    by = c("final_key" = "match_key")
  ) %>%
  left_join(
    fauna_lookup %>%
      rename_with(~ paste0(.x, "_fa"), -match_key),
    by = c("final_key" = "match_key")
  ) %>%
  mutate(
    taxonID = coalesce(taxonID_tx, taxonID_fa, taxonID),
    scientificName = coalesce(scientificName_tx, scientificName_fa, final_scientificName),
    taxonRank = coalesce(taxonRank_tx, taxonRank_fa, taxonRank),
    scientificNameAuthorship = coalesce(scientificNameAuthorship_tx, scientificNameAuthorship_fa, scientificNameAuthorship),
    taxonomicStatus = coalesce(taxonomicStatus_tx, taxonomicStatus_fa, taxonomicStatus),
    kingdom = coalesce(kingdom_tx, kingdom_fa, kingdom),
    phylum = coalesce(phylum_tx, phylum_fa, phylum),
    class = coalesce(class_tx, class_fa, class),
    order = coalesce(order_tx, order_fa, order),
    family = coalesce(family_tx, family_fa, family),
    genus = coalesce(genus_tx, genus_fa, genus),
    specificEpithet = coalesce(specificEpithet_tx, specificEpithet_fa, specificEpithet),
    infraspecificEpithet = coalesce(infraspecificEpithet_tx, infraspecificEpithet_fa, infraspecificEpithet),
    origin = origin_fa,
    endemism = NA_character_
  )

name_resolution <- bind_rows(
  flora_decisions %>%
    transmute(
      name_key, group_type, name_query, safe_name,
      uncertainty_flag, placeholder_flag,
      final_scientificName,
      taxonID, taxonRank, scientificNameAuthorship, taxonomicStatus,
      kingdom, phylum, class, order, family, genus,
      specificEpithet, infraspecificEpithet, origin, endemism,
      validator_used, match_type, decision_reason
    ),
  nonflora_decisions %>%
    transmute(
      name_key, group_type, name_query, safe_name,
      uncertainty_flag, placeholder_flag,
      final_scientificName,
      taxonID, taxonRank, scientificNameAuthorship, taxonomicStatus,
      kingdom, phylum, class, order, family, genus,
      specificEpithet, infraspecificEpithet, origin, endemism,
      validator_used, match_type, decision_reason
    )
) %>%
  mutate(
    taxonRank = normalize_rank(taxonRank),
    taxonomicStatus = normalize_taxonomic_status(taxonomicStatus),
    scientificNameAuthorship = as.character(scientificNameAuthorship)
  ) %>%
  distinct(name_key, group_type, .keep_all = TRUE)

base_final <- base %>%
  left_join(name_resolution, by = c("name_key", "group_type"), suffix = c("", "_res")) %>%
  mutate(
    scientificName = coalesce(final_scientificName, safe_name_res, name_query_res, name_query, safe_name, name_clean)
  )

parsed_tbl <- map(base_final$scientificName, parse_rank_from_name)
parsed_rank <- map_chr(parsed_tbl, "taxonRank")
parsed_genus <- map_chr(parsed_tbl, "genus")
parsed_specific <- map_chr(parsed_tbl, "specificEpithet")
parsed_infra <- map_chr(parsed_tbl, "infraspecificEpithet")

base_final <- base_final %>%
  mutate(
    taxonRank = coalesce(normalize_rank(taxonRank), parsed_rank),
    genus = coalesce(genus, parsed_genus),
    specificEpithet = coalesce(specificEpithet, parsed_specific),
    infraspecificEpithet = coalesce(infraspecificEpithet, parsed_infra),
    taxonomicStatus = normalize_taxonomic_status(taxonomicStatus),
    specificEpithet = if_else(is_non_empty(genus), specificEpithet, NA_character_),
    infraspecificEpithet = if_else(is_non_empty(genus), infraspecificEpithet, NA_character_),
    taxonRank = case_when(
      taxonRank %in% c("species", "subspecies", "variety", "form") & !is_non_empty(specificEpithet) & is_non_empty(genus) ~ "genus",
      taxonRank %in% c("species", "subspecies", "variety", "form") & !is_non_empty(genus) ~ NA_character_,
      TRUE ~ taxonRank
    )
  )

species_key <- norm_key(base_final$scientificName)
base_final$status_from_criteria <- criteria_lookup$category[match(species_key, criteria_lookup$species_key)]
base_final <- base_final %>%
  mutate(
    status = format_status_label(case_when(
      is_non_empty(status_from_criteria) ~ status_from_criteria,
      TRUE ~ status_base
    )),
    statusSource = case_when(
      is_non_empty(status_from_criteria) ~ "MMA Portaria 148/2022",
      is_non_empty(status_base) ~ "Base-09-05-26.xlsx",
      TRUE ~ NA_character_
    )
  )

base_final <- base_final %>%
  mutate(
    establishmentMeans = case_when(
      is_non_empty(origin) ~ map_origin_to_establishment(origin),
      nativa_flag ~ "native",
      TRUE ~ ""
    ),
    endemism_from_validators = normalize_endemism(endemism),
    endemism_from_base = case_when(
      isTRUE(endemica_flag) ~ "Endemic",
      identical(endemica_flag, FALSE) ~ "Non-endemic",
      TRUE ~ ""
    ),
    endemism_final = case_when(
      is_non_empty(endemism_from_validators) ~ endemism_from_validators,
      is_non_empty(endemism_from_base) ~ endemism_from_base,
      TRUE ~ ""
    ),
    taxonRemarks = case_when(
      is_non_empty(endemism_final) ~ paste0("endemism=", endemism_final),
      TRUE ~ NA_character_
    )
  )

iucn_key <- Sys.getenv("IUCN_KEY")
criteria_tbl <- tibble(
  scientificName = character(),
  statusIUCN = character(),
  criteria = character()
)
if (nzchar(iucn_key)) {
  taxa_for_iucn <- base_final %>%
    filter(
      taxonRank %in% c("species", "subspecies", "variety", "form"),
      is_non_empty(genus),
      is_non_empty(specificEpithet)
    ) %>%
    distinct(scientificName, genus, specificEpithet, infraspecificEpithet)

  if (nrow(taxa_for_iucn) > 0) {
    iucn_values <- pmap_dfr(
      list(taxa_for_iucn$genus, taxa_for_iucn$specificEpithet, taxa_for_iucn$infraspecificEpithet),
      function(g, s, i) get_iucn_assessment(g, s, i, iucn_key)
    )
    criteria_tbl <- bind_cols(taxa_for_iucn %>% select(scientificName), iucn_values)
  }
}

criteria_tbl <- criteria_tbl %>% select(scientificName, statusIUCN, criteria)

base_final <- base_final %>%
  left_join(criteria_tbl, by = "scientificName") %>%
  mutate(
    institutionCode = "ZHOUSE",
    license = "CC-BY-NC",
    rightsHolder = "ZHOUSE",
    eventDate = ""
  )

cols_to_remove <- names(base_final)[str_detect(names(base_final), "\\.(x|y)$")]
if (length(cols_to_remove) > 0) {
  base_final <- base_final %>% select(-all_of(cols_to_remove))
}

# ── Montagem DwC ─────────────────────────────────────────────────────────────

dwc <- base_final %>%
  filter(is_non_empty(scientificName)) %>%
  group_by(scientificName) %>%
  summarise(
    datasetName = combine_pipe_expanded(datasetName),
    institutionCode = first_non_empty(institutionCode),
    taxonID = first_non_empty(taxonID),
    scientificName = first_non_empty(scientificName),
    taxonRank = first_non_empty(taxonRank),
    scientificNameAuthorship = first_non_empty(scientificNameAuthorship),
    kingdom = first_non_empty(kingdom),
    phylum = first_non_empty(phylum),
    class = first_non_empty(class),
    order = first_non_empty(order),
    family = first_non_empty(family),
    genus = first_non_empty(genus),
    subgenus = NA_character_,
    specificEpithet = first_non_empty(specificEpithet),
    infraspecificEpithet = first_non_empty(infraspecificEpithet),
    vernacularName = NA_character_,
    establishmentMeans = first_non_empty(establishmentMeans),
    taxonRemarks = combine_pipe(taxonRemarks),
    taxonomicStatus = first_non_empty(taxonomicStatus),
    status = format_status_label(if (any(is_non_empty(status_from_criteria))) {
      pick_most_threatened(status_from_criteria)
    } else {
      pick_most_threatened(status_base)
    }),
    statusSource = case_when(
      any(is_non_empty(status_from_criteria)) ~ "MMA Portaria 148/2022",
      any(is_non_empty(status_base)) ~ "Base-09-05-26.xlsx",
      TRUE ~ NA_character_
    ),
    statusIUCN = first_non_empty(statusIUCN),
    criteria = first_non_empty(criteria),
    eventDate = "",
    locality = combine_pipe_expanded(locality),
    stateProvince = combine_pipe_expanded(stateProvince),
    license = first_non_empty(license),
    rightsHolder = first_non_empty(rightsHolder),
    informationWithheld = combine_pipe_expanded(informationWithheld),
    .groups = "drop"
  )

if (file.exists(template_path)) {
  template_cols <- names(readxl::read_excel(template_path))
} else {
  template_cols <- names(dwc)
}

if (!"establishmentMeans" %in% template_cols) {
  insert_at <- match("vernacularName", template_cols)
  if (is.na(insert_at)) {
    template_cols <- c(template_cols, "establishmentMeans")
  } else {
    template_cols <- append(template_cols, "establishmentMeans", after = insert_at)
  }
}

if (!"statusIUCN" %in% template_cols) {
  insert_at <- match("statusSource", template_cols)
  if (is.na(insert_at)) {
    template_cols <- c(template_cols, "statusIUCN")
  } else {
    template_cols <- append(template_cols, "statusIUCN", after = insert_at)
  }
}

for (col in template_cols) {
  if (!col %in% names(dwc)) {
    dwc[[col]] <- NA_character_
  }
}

dwc <- dwc %>%
  select(all_of(template_cols)) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~ tidyr::replace_na(.x, "")))

# ── Auditoria ─────────────────────────────────────────────────────────────────

name_examples <- base %>%
  group_by(name_key, group_type) %>%
  summarise(
    originalName = combine_pipe(name_clean),
    .groups = "drop"
  )

audit_tbl <- name_resolution %>%
  left_join(name_examples, by = c("name_key", "group_type")) %>%
  transmute(
    originalName,
    queryName = name_query,
    groupType = group_type,
    validator = validator_used,
    matchType = match_type,
    finalScientificName = final_scientificName,
    decisionReason = decision_reason,
    taxonRank,
    taxonomicStatus,
    taxonID,
    scientificNameAuthorship,
    kingdom,
    phylum,
    class,
    order,
    family,
    genus,
    specificEpithet,
    infraspecificEpithet
  ) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~ tidyr::replace_na(.x, "")))

audit_unresolved <- audit_tbl %>%
  filter(
    matchType %in% c("safe_rank", "not_found") |
      !is_non_empty(taxonID) |
      !is_non_empty(kingdom)
  )

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

writexl::write_xlsx(dwc, output_path)
writexl::write_xlsx(
  list(
    auditoria = audit_tbl,
    nao_resolvidos = audit_unresolved
  ),
  audit_output_path
)

cat("Process finished.\n")
cat("DwC output:", output_path, "\n")
cat("Audit output:", audit_output_path, "\n")
