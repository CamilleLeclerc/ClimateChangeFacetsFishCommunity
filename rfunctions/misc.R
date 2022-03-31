#' Save objects
#'
#' As devtools::use_data but choosing dest_dir 
#'  
#' @export
mysave <- function (..., dir = ".", overwrite = FALSE,
    compress = "bzip2") {

  objs <- eval(substitute(alist(...)))
  objs <- vapply(objs, as.character, character(1))
  paths <- file.path(dir, paste0(objs, ".rda"))

  if (any(file.exists(paths)) & !overwrite) {
    existing_file <- objs[file.exists(paths)]
    stop(paste0(existing_file, " already exists.\n", "Use overwrite = TRUE."))
  }
  message("Saving ", paste(unlist(objs), collapse = ", "),
    " as ", paste(basename(paths), collapse = ", "), " to ",
    dir)
  envir <- parent.frame()
  mapply(save, list = objs, file = paths, MoreArgs = list(envir = envir,
      compress = compress))
  invisible()
}

myload <- function (..., dir = ".") {

  objs <- eval(substitute(alist(...)))
  objs <- vapply(objs, as.character, character(1))
  paths <- file.path(dir, paste0(objs, ".rda"))

  if (any(!file.exists(paths))) {
    stop(paste0(existing_file, " does not exist\n"))
  }
  lapply(paths, load, .GlobalEnv)
  invisible()
}

#' Make summary table
#'
#' @param ci obj from bootMer
#' @param mod linear model 
#' @param term_rp named character vector
#' @param group_rp named character vector
clean_summary_mod <- function (ci = NULL, mod = NULL, term_rp = NULL,
  group_rp = NULL) {


  # Make clean ci table
  ci_tbl <- ci %>%
    as.data.frame() %>%
    rownames_to_column(var = "term") %>%
    as_tibble() %>% 
    mutate(
      term = str_replace_all(term, "beta\\.", ""),
      term = str_replace_all(term, term_rp)) %>%
    mutate_at(c("2.5 %", "97.5 %"),~ round(., 2)) %>%
    unite(CI, `2.5 %`, `97.5 %`, sep = ":") %>%
    mutate(CI = paste0("[", CI,"]"))

  # Make clean estimate table
  est_tbl <- broom::tidy(mod) %>%
    mutate(
      term = str_replace_all(term, term_rp)
      ) %>%
  mutate_at(c("estimate", "std.error"), ~round(., 2)) %>%
  left_join(ci_tbl) %>% 
  select(term, group, estimate, std.error, CI)

  return(est_tbl)
}

source_dir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "*.R")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
