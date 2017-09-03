source("src/R/init.R")

OUTPUT <- project_path("data", "glottolog.csv")
INPUTS <- list("tree_glottolog" =
                 project_path("external", "glottolog", "tree-glottolog.json"))

wals <- IO[["wals"]]

glottolog_tree <-
  read_json(INPUTS[["tree_glottolog"]], simplifyVector = FALSE)

simplify_tree <- function(x) {
  list(x[["glottocode"]], map(sort_glotto(x[["children"]]), simplify_tree))
}

# Add additional information
languoid_info <- select(IO$glottolog_languoids,
                        glottocode = id,
                        iso_639_3 = hid,
                        latitude, longitude,
                        level, status, bookkeeping)

macroarea_lookup <- select(IO$glottolog_lang_geo, glottocode, macroarea) %>%
  filter(!is.na(macroarea)) %>%
  (function(x) set_names(x$macroarea, x$glottocode))()

geo_lookup <- select(languoid_info, glottocode, latitude, longitude) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  group_by(glottocode) %>%
  summarise(xy = list(matrix(c(longitude, latitude), ncol = 2))) %>%
  (function(x) set_names(x$xy, x$glottocode))()

# wals_genus_path <- function(x) {
#   replacements <- list(
#     "genus/bororoan" = "genus/bororo",
#     "genus/jingpho" =  "genus/jinghpo",
#     "genus/guaymiic" = "genus/guaymi",
#     "genus/coresiouan" = "genus/siouan",
#     "genus/warayic" = "genus/warayic"
#   )
#
#   iconv(x, to = "ASCII//TRANSLIT") %>%
#     str_to_lower() %>%
#     str_replace_all("[^a-z]", "") %>%
#     str_c("genus", ., sep = "/") %>%
#     recode(splice(replacements))
# }
#
# wals_genus <- IO$wals %>%
#   select(genus, wals_code) %>%
#   mutate(identifier = wals_genus_path(genus)) %>%
#   select(-genus)
#
# wals_family_path <- function(x) {
#   iconv(x, to = "ASCII//TRANSLIT") %>%
#   str_to_lower() %>%
#     str_replace_all("[^a-z]", "") %>%
#     str_c("family", ., sep = "/")
# }
#
# wals_family <- IO$wals %>%
#   select(family, wals_code) %>%
#   mutate(identifier = wals_family_path(family)) %>%
#   select(-family)

# Remove some incorrect WALS identifiers
# I submitted PRs to fix these
resourcemap_rm <- tribble(
  ~glottocode, ~type,
  "yana1270", "wals",
  "baii1247", "wals",
  "baii1251", "wals",
  "boro1274", "wals",
  "mura1277", "wals",
  "kwaa1264", "wals",
  "kwaa1262", "wals",
  "nucl1372", "wals"
)

# Lookup Identifiers: WALS_Codes, ISO 639-3
resourcemap <- IO$glottolog_resourcemap %>%
  anti_join(resourcemap_rm, by = c("glottocode", "type"))

wals_lookup <-
  filter(resourcemap, type == "wals") %>%
  filter(str_detect(identifier, "^[a-z]{2,3}$")) %>%
  select(-type) %>%
  #left_join(bind_rows(wals_family, wals_genus),
  #          by = "identifier") %>%
  #mutate(wals_code = coalesce(wals_code, identifier)) %>%
  #select(-identifier) %>%
  rename(wals_code = identifier) %>%
  group_by(glottocode) %>%
  summarise(wals_codes = list(sort(unique(wals_code)))) %>%
  (function(x) {set_names(x$wals_codes, x$glottocode)})()

iso_lookup <- filter(resourcemap, type == "iso639-3",
                      str_detect(identifier, "^[a-z]{3}$")) %>%
  group_by(glottocode) %>%
  summarise(identifier = list(identifier)) %>%
  (function(x) {set_names(x$identifier, x$glottocode)})()

# sort list by glottocode
sort_glotto <- function(x) x[order(map_chr(x, "glottocode"))]

# Walk the glottolog tree to build data
env <- rlang::new_environment()
env$.i <- 0
walk_glottolog <- function(x,
                           depth = 1,
                           ancestors = character(),
                           family = NULL) {
  glottocode <- x[["glottocode"]]
  if (depth == 1) {
    family <- glottocode
  }
  # Save and iterate depth first indicator
  i <- env$.i <- env$.i + 1
  descendants <- map(sort_glotto(x[["children"]]),
                     walk_glottolog,
                     depth = depth + 1,
                     family = family,
                     ancestors = c(glottocode, ancestors))

  descendants_glotto <- flatten_chr(map(descendants, "glottocodes"))

  # For these codes, do not concatenate descendant values
  wals_codes <- (wals_lookup[[glottocode]] %||%
      flatten_chr(map(descendants, "wals_codes"))) %>%
    sort()
  iso_codes <- (iso_lookup[[glottocode]] %||%
                  flatten_chr(map(descendants, "iso_codes"))) %>%
    sort()

  macroarea <- c(macroarea_lookup[glottocode],
                 flatten_chr(map(descendants, "macroarea"))) %>%
    na.omit() %>%
    unique() %>%
    sort()

  if (length(descendants) > 0) {
    subtree_depth <- max(map_int(descendants, "subtree_depth")) + 1L
  } else {
    subtree_depth <- 0L
  }



  geo <- geo_lookup[[glottocode]]
  if (is.null(geo)) {
    desc_geo <- invoke(rbind, map(descendants, "geo"))
    if (!is.null(desc_geo)) {
      if (nrow(desc_geo) > 1) {
        geo <- geosphere::geomean(desc_geo)
      }
    }
  }

  env[[glottocode]] <-
    tibble(glottocode = glottocode,
           parent = if (length(ancestors)) ancestors[[1]] else NA_character_,
           children = list(map_chr(x[["children"]], "glottocode")),
           ancestors = list(ancestors),
           descendants = list(descendants_glotto),
           iso_639_3 = list(iso_codes),
           wals_codes = list(wals_codes),
           longitude = if (!is.null(geo)) geo[1, 1] else NA_real_,
           latitude = if (!is.null(geo)) geo[1, 2] else NA_real_,
           macroarea = list(macroarea),
           i = i,
           depth = depth,
           subtree_depth = subtree_depth,
           family = family)

  list(glottocodes = c(glottocode, descendants_glotto),
       iso_codes = iso_codes,
       wals_codes = wals_codes,
       macroarea = macroarea,
       geo = geo,
       subtree_depth = subtree_depth)
}

# Walk tree using DFS.
# - Fill in data in env()
# - Add data from descendants
walk(glottolog_tree, walk_glottolog)
rm(.i, envir = env)

# A second pass through the tree to fill in missing values
# inherited from ancestors
fill_descendants <- function(x, parent = NULL) {
  glottocode <- x[["glottocode"]]
  if (!is.null(parent)) {
    parent_data <- env[[parent]]
    .data <- env[[glottocode]]

    for (i in c("latitude", "longitude")) {
      if (is.na(.data[[i]])) {
        .data[[i]] <- parent_data[[i]]
      }
    }
    for (i in c("iso_639_3", "wals_codes", "macroarea")) {
      if (!length(.data[[i]][[1]])) {
        .data[[i]] <- parent_data[[i]]
      }
    }
    env[[glottocode]] <- .data
  }
  walk(x[["children"]], fill_descendants, parent = glottocode)
}
walk(glottolog_tree, fill_descendants)

#' Merge other info
languoid_meta <- select(IO$glottolog_languoids,
                        name,
                        glottocode = id,
                        level,
                        bookkeeping)

languoids <-
  as.list(env) %>%
  bind_rows() %>%
  left_join(languoid_meta, by = "glottocode") %>%
  mutate_at(vars(iso_639_3, wals_codes, ancestors, children, descendants),
            funs(map_chr(., paste0, collapse = " "))) %>%
  # since macroarea can have spaces, use ; delim
  mutate_at(vars(macroarea),
            funs(map_chr(., paste0, collapse = ";"))) %>%
  select(glottocode, name, level, depth, family, parent, ancestors,
         children, descendants, subtree_depth, iso_639_3, wals_codes,
         macroarea, latitude, longitude, bookkeeping)

write_csv(languoids, OUTPUT, na = "")
