check_path_to_description <- function(path_to_description) {
  if (!file.exists(path_to_description)) {
    message("Provided DESCRIPTION does not exist.")
    path_to_description_renv <- file.path(renv::project(), "DESCRIPTION")
    if (file.exists(path_to_description_renv)) {
      message("Will use DESCRIPTION under active renv project.")
      path_to_description <- path_to_description_renv
    } else {
      stop("No valid DESCRIPTION found.")
    }
  }
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
  config <- read_intent_config(
    path_to_description = path_to_description,
    mandatory = "repos"
  )
  repos <- unlist(config$repos)
  if (length(repos) == 0) {
    stop("No repositories found in Config/intent/repos/.")
  }
  repos
}
