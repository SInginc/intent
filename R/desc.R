check_path_to_description <- function(path_to_description) {
  # We don't want to aggressively fall back to renv::project() DESCRIPTION
  # if the user specifically provided a path that doesn't exist yet (like in init).
  path_to_description
}


#' Read and parse custom Config/intent fields from DESCRIPTION
#'
#' @param path_to_description Path to the DESCRIPTION file.
#' @param mandatory Character vector of mandatory top-level settings (e.g., "repos").
#' @param permissive Character vector of permissive top-level settings. If NULL, all are allowed.
#'   Permissive fields are optional; they are only used to filter out fields not in
#'   `union(mandatory, permissive)`.
#'
#' @return A nested list of settings.
#' @keywords internal
read_intent_config <- function(
  path_to_description = file.path(getwd(), "DESCRIPTION"),
  mandatory = NULL,
  permissive = NULL
) {
  path_to_description <- check_path_to_description(path_to_description)
  desc_obj <- desc::description$new(path_to_description)
  all_fields <- desc_obj$fields()

  # Filter for Config/intent/ fields
  intent_prefix <- "Config/intent/"
  intent_fields <- all_fields[startsWith(all_fields, intent_prefix)]

  if (length(intent_fields) == 0) {
    stop("No configuration found with prefix 'Config/intent/'.")
  }

  # Simplified 2-level parser for better readability and reliability
  parsed_config <- list()
  for (field in intent_fields) {
    suffix <- substring(field, nchar(intent_prefix) + 1)
    parts <- strsplit(suffix, "/")[[1]]
    if (length(parts) >= 1) {
      category <- parts[1]
      if (!category %in% names(parsed_config)) {
        parsed_config[[category]] <- list()
      }
      if (length(parts) > 1) {
        # e.g. repos/cran -> parsed_config$repos$cran
        key <- paste(parts[-1], collapse = "/")
        parsed_config[[category]][[key]] <- desc_obj$get_field(field)
      } else {
        # e.g. Config/intent/mode -> parsed_config$mode
        parsed_config[[category]] <- desc_obj$get_field(field)
      }
    }
  }

  # Validation: Mandatory fields must exist
  if (!is.null(mandatory)) {
    missing <- setdiff(mandatory, names(parsed_config))
    if (length(missing) > 0) {
      stop(sprintf(
        "Mandatory Config/intent fields missing: %s",
        paste(missing, collapse = ", ")
      ))
    }
  }

  # Filtering: Keep only mandatory and permissive fields if permissive is specified
  if (!is.null(permissive)) {
    allowed <- union(mandatory, permissive)
    extra <- setdiff(names(parsed_config), allowed)
    if (length(extra) > 0) {
      parsed_config <- parsed_config[intersect(
        names(parsed_config),
        allowed
      )]
    }
  }

  parsed_config
}


#' @keywords internal
get_repos <- function(
  path_to_description = file.path(getwd(), "DESCRIPTION")
) {
  config <- tryCatch(
    read_intent_config(
      path_to_description = path_to_description,
      mandatory = "repos"
    ),
    error = function(e) list(repos = list())
  )
  repos <- unlist(config$repos)
  if (length(repos) == 0) {
    # Don't stop, just return empty if not found, unless mandatory was truly needed
    return(character())
  }
  repos
}


#' Read dependency overrides from DESCRIPTION
#'
#' @param path_to_description Path to the DESCRIPTION file.
#'
#' @return A list with two elements: `overrides` (named list of parsed override objects)
#'   and `extra_repos` (named vector of additional repositories).
#' @keywords internal
get_intent_overrides <- function(
  path_to_description = file.path(getwd(), "DESCRIPTION")
) {
  # We use a tryCatch because Config/intent/Imports or Suggests might not exist
  config <- tryCatch(
    read_intent_config(
      path_to_description = path_to_description,
      permissive = c("Imports", "Suggests")
    ),
    error = function(e) list()
  )

  overrides <- list()
  extra_repos <- character()

  # Process Imports and Suggests
  for (type in c("Imports", "Suggests")) {
    if (type %in% names(config)) {
      items <- config[[type]]
      if (is.list(items)) {
        # e.g. Config/intent/Imports/pkg: ...
        items <- unlist(items)
      }
      for (item in items) {
        parsed <- parse_override(item)
        overrides[[parsed$package]] <- parsed
        if (!is.null(parsed$repo)) {
          repo_name <- paste0("override_", parsed$package)
          extra_repos[repo_name] <- parsed$repo
        }
      }
    }
  }

  list(overrides = overrides, extra_repos = extra_repos)
}


#' Parse a dependency override string
#'
#' @param override_str String in format "package@version@source"
#'
#' @return A list with `package`, `version`, `ref` (pak reference), and `repo` (optional).
#' @keywords internal
parse_override <- function(override_str) {
  parts <- strsplit(override_str, "@", fixed = TRUE)[[1]]
  if (length(parts) != 3 || any(parts == "")) {
    stop(sprintf(
      "Invalid override format: %s. Expected package@version@source",
      override_str
    ))
  }

  pkg <- parts[1]
  ver <- parts[2]
  src <- parts[3]

  # Extract package name if it's user/repo
  pkg_name <- basename(pkg)

  ref <- NULL
  repo <- NULL

  if (grepl("^https?://", src)) {
    # URL source
    repo <- src
    ref <- sprintf("%s@%s", pkg_name, ver)
  } else if (src %in% c("cran", "standard")) {
    ref <- sprintf("%s@%s", pkg_name, ver)
  } else if (src %in% c("github", "bioc", "local", "url")) {
    ref <- sprintf("%s::%s@%s", src, pkg, ver)
  } else {
    stop(sprintf(
      "Invalid override source: %s. Supported sources are cran, standard, github, bioc, local, url, or an http(s) repository URL",
      src
    ))
  }

  list(package = pkg_name, version = ver, ref = ref, repo = repo)
}
