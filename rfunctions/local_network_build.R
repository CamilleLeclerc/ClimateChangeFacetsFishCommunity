################################################################################
#                      Functions to build local networks                       #
################################################################################

#' Local network building
#' 
#' 
#' @inheritParams get_size_class 
#' @param group_var variable characterizing different communities.
#'
#' @details 
#'
#' @return data.frame containing local network code and interaction matrix.
#' @export
build_local_network <- function(
  data,
  species,
  var,
  group_var,
  metaweb,
  classes = NULL, ...) {

  species <- rlang::enquo(species)
  var <- rlang::enquo(var)
  var_chr <- rlang::quo_name(var)
  group_var <- rlang::enquo(group_var)

  if (is.null(classes)) {
    stopifnot("size_class" %in% names(metaweb))
    classes <- metaweb$size_class
  }
  if (any(is.na(data[[var_chr]]))) {
    msg <- paste0(length(which(is.na(data[[var_chr]]))), " NA in ", var_chr, "\n",
    "They have removed by na.omit")
    message(msg)
    data %<>% na.omit
  }

  #data <- sanatize_metaweb(data = data,
    #species = !!species, fish_diet_shift = classes,
    #nb_class = max(unique(classes$class_id))
  #)

  # Compute the class_id
  classes_assigned <- assign_size_class(data, !!species, !!var, classes)
  #Subset the matrix
  output <- classes_assigned %>%
    dplyr::group_by(!!group_var) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      network = purrr::map(data, extract_network,
        species = !!species, var = !!var, metaweb = metaweb,
        classes = classes, ...),
      data = purrr::map(data, na.omit)
    )

    return(output)
}

#' Extract network
#'
#' Extract local network from a list of species and size
#'
#' @inheritParams assign_size_class
#' @param metaweb an object created by the build_metaweb function.
#'
#' @return a matrix containing the species class and links between them
#' @export
extract_network <- function (data, species, var, metaweb, classes = NULL, link = "flux", out_format = "adjacency") {

  species <- rlang::enquo(species)
  var <- rlang::enquo(var)

  if (is.null(classes)) {
    stopifnot("size_class" %in% names(metaweb))
    classes <- metaweb$size_class
  }
  stopifnot(link %in% c("flux", "pred"))
  stopifnot(out_format %in% c("adjacency", "igraph", "edge"))

  # Idea if class_id present in data, do not do it build_local_network.
  if ("class_id" %in% colnames(data)) {
    classes_assigned <- data
  } else {
    classes_assigned <- assign_size_class(data, !!species, !!var, classes)
  }

  if (any(is.na(classes_assigned$class_id))) {
    unassigned <- filter(classes_assigned, is.na(class_id)) %>%
      select(!!species) %>% unlist
    message(
      "Unassigned individuals (marked as NAs) found in species: ", cat(unassigned), "\n",
    "they have been excluded with na.omit"
    )
    classes_assigned %<>% na.omit
  }

  species_class <- classes_assigned %>%
    tidyr::unite(sp_class, !!species, class_id, sep = "_") %>%
    dplyr::select(sp_class) %>%
    unlist
  to_select <- c(species_class, metaweb$resource) %>%
    unique

  full_meta <- metaweb$metaweb
  # If it is a square matrix
  if (is.matrix(full_meta) & nrow(full_meta) == ncol(full_meta)) {
    #pos_to_select <- which(colnames(full_meta) %in% to_select)
    output <- full_meta[to_select, to_select]
  } else {
    stop("Metaweb should be a squared matrix.")
  }

  ## May be that the format should be done in the upper function (faster)  
  # Transform to edge dataframe:
  if (link == "pred") {
    output <- t(output)
  }
  if (out_format == "igraph") {
    output %<>% igraph::graph_from_adjacency_matrix(., mode = "directed") %>%
      igraph::as_data_frame()
  } else if (out_format == "edge") {
    output %<>% igraph::graph_from_adjacency_matrix(., mode = "directed") %>%
      igraph::as_edgelist()
  }

  output
}

#' Assign size classes, for all species 
#' 
#' @param data a data.frame containing species and size variable.
#' @param species species variable name in the dataset.
#' @param var variable characterizing the size.
#' @param classes data.frame created by the compute_classes function.
#'
#' @return data.frame containing class_id for each indivual.
#' @export
assign_size_class <- function (data, species, var, classes) {

  species <- rlang::enquo(species)
  var <- rlang::enquo(var)

  #Attribute size class for each fish
  classes_assigned <- data %>%
    dplyr::group_by(!!species) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      class_id = purrr::pmap(
        list(data = data, species_name = !!species),
        get_size_class,
        var = !!var, classes = classes, species_var = !!species)
    )

  classes_assigned %>%
    tidyr::unnest(cols = c(data, class_id)) %>%
    dplyr::mutate(class_id = as.integer(class_id)) %>%
    dplyr::select(!!species, class_id, !!var, tidyselect::everything()) %>%
    dplyr::ungroup()

}

#' Get size classes for a given species
#'
#' @param data a data.frame containing species and size variable.
#' @param species data.frame containing species and the predation window.
#' @param var variable characterizing the size.
#' @param classes data.frame created by the compute_classes function.
#'
#' @return data.frame containing local network code and interaction matrix.
#' @export
get_size_class <- function (data, species_name, var, classes, species_var = NULL) {
  
  var <- rlang::enquo(var)
  species_name <- rlang::enquo(species_name)
  species_var <- rlang::enquo(species_var)
  #if (!is.null(species)) {
    #species <- rlang::quo(species)
  #} else {
    #species <- "species"
    #species <- rlang::quo(species)
  #}

  #Get species classes
  if (!rlang::quo_is_null(species_name)) {
    classes <- classes[classes[[rlang::quo_name(species_var)]] == rlang::quo_name(species_name), ] 
  }
  classes %<>%
  dplyr::select(lower, upper)

  # Get species size
  data %<>% dplyr::select(!!var) %>%
    unlist(.)

  # Use findInterval
  # see https://rpubs.com/josephuses626/findInterval to improve
  mat_match <- apply(classes, 1, findInterval, x = data, left.open = TRUE) == 1
  #correct for first interval which is left close if one of the data is equal to
  #the lower bound of the the first interval
  if (any(data == min(classes$lower))) {
    row_min <- which(data == min(classes$lower))
    mat_match[row_min, 1]  <- TRUE
  }
  apply(mat_match, 1, function(x) {
    int <- which(x)
    if (all(x == FALSE | is.na(x))) {
      NA
    } else {
      int
    }
      }
    ) %>%
  unlist(.)
}