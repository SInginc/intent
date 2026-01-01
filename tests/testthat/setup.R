if (any(grepl("^_R_CHECK_", names(Sys.getenv())))) {
  Sys.setenv(R_USER_CACHE_DIR = tempdir())
}
