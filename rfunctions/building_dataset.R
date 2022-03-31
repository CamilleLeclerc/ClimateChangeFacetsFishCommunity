#Get size from lot (AFB)
#@param lot data.frame
#@inheritParams gen_fish_from_lot
#@export
get_size_from_lot <- function(
  lot = NULL, id_var = NULL, type_var = NULL, nb_var = NULL,
  min_var = NULL, max_var = NULL, species = NULL,
  measure = NULL, measure_id_var = NULL, size_var = NULL,
  future_enabled = FALSE, ...){
  
  id_var <- rlang::enquo(id_var)
  id_var_chr <- rlang::quo_name(id_var)
  type_var <- rlang::enquo(type_var)
  type_var_chr <- rlang::quo_name(type_var)
  nb_var <- rlang::enquo(nb_var)
  nb_var_chr <- rlang::quo_name(nb_var)
  species <- rlang::enquo(species)
  max_var <- rlang::enquo(max_var)
  max_var_chr <- rlang::quo_name(max_var)
  min_var <- rlang::enquo(min_var)
  min_var_chr <- rlang::quo_name(min_var)
  
  measure_id_var <- rlang::enquo(measure_id_var)
  measure_id_var_chr <- rlang::quo_name(measure_id_var)
  size_var <- rlang::enquo(size_var)
  size_var_chr <- rlang::quo_name(size_var)
  
  # Filter surnumerous variable:
  lot <- lot %>%
    dplyr::select(!!id_var, !!type_var, !!nb_var,
                  !!species, !!min_var, !!max_var
    )
  
  measure <- measure %>%
    dplyr::select(!!measure_id_var, !!size_var)
  
  # Filter surnumerous id in measure:
  if (any(! measure[[measure_id_var_chr]] %in% lot[[id_var_chr]])) {
    measure <- measure %>%
      dplyr::filter(!!measure_id_var %in% lot[[id_var_chr]])
    message("surnumerous lot in measure were removed")
  }
  
  # Filter incorrect lot:
  diff_lot_type <- c("G", "S/L", "N", "I")
  if (any(is.na(lot[[type_var_chr]]) |
          any(!lot[[type_var_chr]] %in% diff_lot_type))
  ) {
    lot <- lot %>%
      dplyr::filter(! is.na(!!type_var) & !!type_var %in% diff_lot_type)
    message("NA lot id and lot type has been filtered")
  }
  
  # Filter if effectif is not present:
  if (any(is.na(lot[[nb_var_chr]])) | any(!lot[[nb_var_chr]] > 0)) {
    lot <- lot %>%
      dplyr::filter( (!is.na(!!nb_var)) & !!nb_var > 0)
    message("Incorrect effectif has been filtered")
  }
  
  na_G <- lot %>%
    dplyr::filter(!!type_var == "G" & (is.na(!!min_var) | is.na(!!max_var)))
  incorrect_G <-  lot %>%
    dplyr::filter(!!type_var == "G" & !!min_var >= !!max_var)
  low_n_G <-  lot %>%
    dplyr::filter(!!type_var == "G" & !!nb_var < 5)
  
  if (any(c(nrow(na_G), nrow(incorrect_G)) != 0)) {
    G_bad_id <- c(na_G[[id_var_chr]], incorrect_G[[id_var_chr]], low_n_G[[id_var_chr]])
    
    lot <- lot %>%
      dplyr::filter(!( !!id_var %in% G_bad_id))
    message("incorrect lot G have been filtered")
  }
  
  
  gen_fish_from_lot <- compiler::cmpfun(gen_fish_from_lot)
  lot$fish <- parallel::mcMap(gen_fish_from_lot,
                              id = lot[[id_var_chr]],
                              type = lot[[type_var_chr]],
                              min_size = lot[[min_var_chr]],
                              max_size = lot[[max_var_chr]],
                              nb = lot[[nb_var_chr]],
                              MoreArgs = list( 
                                ind_measure = measure,
                                ind_size = size_var_chr,
                                ind_id = measure_id_var_chr
                              )
  )
  
  # Filter and extract:
  output <- lot %>%
    dplyr::select(!!id_var, !!species, fish) 
  
  output
}

#Generate fish from fishing lot (AFB) 
#@param id int id of the lot  
#@param type character type of the lot (N, G, S/L, I)
#@param min_size dbl minimum size of the lot 
#@param max_size dbl maximum size of the lot 
#@param nb int effectif of the lot 
#@param ind_measure data.frame individual measurement of the fish 
#@param ind_id variable name id of the lot in ind_measure 
#@param ind_size variable name for the size in ind_measure 
#@details From fishing lot, we build generate the fish size individual
#@export
gen_fish_from_lot <- function (
  id = NULL, type = NULL,  min_size = NULL, max_size = NULL, nb = NULL,
  ind_measure = NULL, ind_id = NULL, ind_size = NULL, ...) {
  
  # Promise:
  ind_id <- rlang::enquo(ind_id)
  ind_size <- rlang::enquo(ind_size)
  
  # Build by lot
  if (type == "G") {
    if (any(is.na(c(min_size, max_size)))) {
      warning_msg <- paste(
        "NA min_size or max_size in lot of type G number ",id,
        ", lot put as NA\n", sep = ""
      )
      warning(warning_msg)
      lot <- rep(NA, nb)
    } else if (min_size >= max_size) {
      warning_msg <- paste(
        "min_size >= max_size in lot of type G number ",id,", lot put as NA\n",
        sep = "")
      warning(warning_msg)
      lot <- NA
    } else if (nb < 5) {
      warning_msg <- paste(
        "# of obs is inferior to 10 (actual # is,", length(nb),
        ") in Lot type G number", id,".\n", "Lot put as NA\n", sep = "")
      warning(warning_msg)
      lot <- rep(NA, nb)
    } else {
      avg <- (min_size + max_size) / 2
      sdt <- (max_size - min_size) * 1 / 4
      
      lot <- truncdist::rtrunc(n = nb, spec = "norm", a = min_size, b = max_size,
                               mean = avg, sd = sdt)
      stopifnot(length(lot) == nb)
    }
  } else if (type == "S/L") {
    #Get size:
    mask <- which(ind_measure[[rlang::quo_name(ind_id)]] == id)
    size <- ind_measure[mask, ][[rlang::quo_name(ind_size)]]
    size <- na.omit(size)
    stopifnot(is.na(size) | nrow(size) == 0)
    # Sanity check:
    if (length(size) < 10) {
      warning_msg <- paste(
        "# of obs is inferior to 10 (actual # is,", length(size),
        ") in Lot type S/L number ", id,".\n", "Lot put as NA\n", sep = "")
      warning(warning_msg)
      lot <- rep(NA, nb)
    } else {
      #Distribution parameters:
      avg <- mean(size)
      sdt <- sd(size)
      
      # Sample inside the 90% of the distribution probability:
      p05 <- quantile(size, 0.05)
      p95 <- quantile(size, 0.95)
      
      # Error if p05 >= p95
      if (p05 >= p95) {
        warning_msg <- paste(
          "p05 is equal or greater than p95 (p05 = ", p05, ", P95 = ", p95,
          ") in Lot type S/L number ", id,".\n", "Lot put as NA\n", sep = "")
        warning(warning_msg)
        lot <- rep(NA, nb)
      } else {
        lot <- truncdist::rtrunc(n = nb, spec = "norm", a = p05, b = p95,
                                 mean = avg, sd = sdt)
      }
    }
  } else if (type == "I") {
    # All individuals have been measured:
    mask <- which(ind_measure[[rlang::quo_name(ind_id)]] == id)
    lot <- ind_measure[mask, ][[rlang::quo_name(ind_size)]]
    stopifnot(length(lot) == nb)
  } else if (type == "N") {
    # One big individual:
    mask <- which(ind_measure[[rlang::quo_name(ind_id)]] == id)
    lot <- ind_measure[mask, ][[rlang::quo_name(ind_size)]]
    stopifnot(length(lot) == 1)
  }
  # Round to milimeters:
  round(lot)
}

#Main function for lot check
#return a data.frame with lot id, species and the  
#@export
get_check_lot <- function(
  lot = NULL, id_var = NULL, type_var = NULL, nb_var = NULL,
  min_var = NULL, max_var = NULL, species = NULL,
  measure = NULL, measure_id_var = NULL, size_var = NULL,
  future_enabled = FALSE, ...){
  
  id_var <- rlang::enquo(id_var)
  id_var_chr <- rlang::quo_name(id_var)
  type_var <- rlang::enquo(type_var)
  type_var_chr <- rlang::quo_name(type_var)
  nb_var <- rlang::enquo(nb_var)
  nb_var_chr <- rlang::quo_name(nb_var)
  species <- rlang::enquo(species)
  max_var <- rlang::enquo(max_var)
  min_var <- rlang::enquo(min_var)
  
  measure_id_var <- rlang::enquo(measure_id_var)
  measure_id_var_chr <- rlang::quo_name(measure_id_var)
  size_var <- rlang::enquo(size_var)
  size_var_chr <- rlang::quo_name(size_var)
  
  # Filter surnumerous variable:
  lot <- lot %>%
    dplyr::select(!!id_var, !!type_var, !!nb_var, !!species, !!min_var, !!max_var)
  measure <- measure %>%
    dplyr::select(!!measure_id_var, !!size_var)
  
  # Filter surnumerous id in measure:
  if (any(! measure[[measure_id_var_chr]] %in% lot[[id_var_chr]])) {
    measure <- measure %>%
      dplyr::filter(!!measure_id_var %in% lot[[id_var_chr]])
    message("surnumerous lot in measure were removed")
  }
  
  check_lot <- compiler::cmpfun(check_lot)
  output <- lot %>%
    dplyr::mutate(
      fish =
        parallel::mcMap(check_lot,
                        id = !!id_var,
                        type = !!type_var,
                        min_size = !!min_var,
                        max_size = !!max_var,
                        nb = !!nb_var,
                        MoreArgs = list( 
                          ind_measure = measure,
                          ind_size = size_var_chr,
                          ind_id = measure_id_var_chr
                        )
        )
    ) %>%
    dplyr::select(!!id_var, !!species, fish) %>%
    tidyr::unnest(fish)
  
  output
}

#Check the quality of the lot 
#@inheritParams: gen_fish_from_lot
#@details: The function check if there are missing datas that can make the lot
#invalid. The check are specific of each lot type. For lot "G", we check if
#min or max size are missing, if min >= max size and the number of fish in the
#lot is superior or equal to 10. For lot "S/L", we check if the number of measured
#fishes is superior to 20 (standard protocol propose 30 be measured). In lot
#"I", we check that the number of measured fishes correspond to the number of
#fish recorded in the lot. For lot "N", we check if the number of measured
#fish is equal to one.    
#@return a list: Each element of the list correspond to a lot of a fishing
#operation. If there is no problem in the lot, it returns NA. In the opposite
#case, the function returns a character containing the type of error.
check_lot <- function (
  id = NULL, type = NULL,  min_size = NULL, max_size = NULL, nb = NULL,
  ind_measure = NULL, ind_id = NULL, ind_size = NULL, ...) {
  
  
  # Build by lot
  if (type == "G") {
    if (any(is.na(c(min_size, max_size)))) {
      status <- paste("min_size or/and max_size is NA")
    } else if (min_size >= max_size) {
      status <- paste("min_size >= max_size")
    } else if (nb < 10) {
      status <- paste("# < 10")
    } else {
      status <- "good"
    }
  } else if (type == "S/L") {
    #Get size:
    mask <- which(ind_measure[[ind_id]] == id)
    size <- ind_measure[mask, ][[ind_size]]
    size <- na.omit(size)
    stopifnot(is.na(size) | nrow(size) == 0)
    # Sanity check:
    if (length(size) < 20) {
      status <- paste("# < 20 (excluding NA)")
    } else {
      status <- "good"
    }
  } else if (type == "I") {
    # All individuals have been measured:
    mask <- which(ind_measure[[ind_id]] == id)
    lot <- ind_measure[mask, ][[ind_size]]
    if (length(lot) == nb) {
      status <- "good"
    } else {
      status <- "# of measured fish do not match sample size"
    }
  } else if (type == "N") {
    # One big individual:
    mask <- which(ind_measure[[ind_id]] == id)
    lot <- ind_measure[mask, ][[ind_size]]
    if (length(lot) == nb) {
      status <- "good"
    } else {
      status <- "# of measured fish do not match sample size"
    }
  }
  status 
}


#Remove fishing operation that are too close in time 
#@param op data.frame generated by the vignette `aspe_database.Rmd`
#@param sep_threshold int. Number of days minimum between two operations
# @param nb_sampling int. Minimum number of the operation for a station to be
#kept in the dataset
#@details sep_threshold and nb_sampling are set to 270 and 10 by default
#respectively.
rm_dbl_fishing_op <- function (op = NULL, sep_threshold = 270, nb_sampling = 10) {
  
  # Set consecutive days between operation by station 
  int_op <- op %>%
    dplyr::group_by(station) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      point = seq(1, length(station)),
      sample_sep = c(NA, date[-1] - date[-length(station)])
    ) %>%
    dplyr::arrange(station)
  
  # Keep the most complete sampling for each op
  clean_dbl <- int_op %>%
    dplyr::group_by(station) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = 
                    purrr::map(data, ~keep_most_complete_sampling(
                      station = .x, threshold = sep_threshold))) %>%
    tidyr::unnest()
  #
  # Numbering the sampling events by station
  op_hist <- clean_dbl %>%
    dplyr::group_by(station) %>%
    dplyr::summarise(freq = dplyr::n())
  
  good_station <- op_hist %>%
    dplyr::filter(freq >= nb_sampling)
  
  
  op <- op %>%
    dplyr::filter(station %in% good_station$station, opcod %in% clean_dbl$opcod) %>%
    dplyr::mutate(year = lubridate::year(date))
  
  return(op)
}

##################################
#  Filter double fish operation  #
##################################

del_dbl_op <- function (point, tot, ...) {
  # The targeted op have likely less species
  point_to_select <- tot$point %in% c(point, point - 1)
  to_compare <- tot[point_to_select, ]
  kept_station <- arrange(to_compare, desc(nb_sp), desc(nb_ind)) %>%
    slice(1)
  kept_station
}
keep_most_complete_sampling <- function (station = NULL, threshold = 270) {
  
  ##Any double fishing within two month
  dbl_op <- filter(station, sample_sep < threshold)
  # if it is ok:
  if (nrow(dbl_op) == 0) {
    return(station)
  }
  #For the op that have short time_sep:
  low_row <- FALSE
  while (nrow(dbl_op) > 0) {
    
    temp_pt <- dbl_op$point[1]
    selected_op <- del_dbl_op(temp_pt, tot = station)
    removed_dbl <- station[!station$point %in% c(temp_pt, temp_pt - 1),]
    
    station <- bind_rows(removed_dbl, selected_op) %>%
      arrange(date) %>%
      mutate(
        point = seq(1, n()),
        sample_sep = c(NA, date[-1] - date[-n()])
      )
    dbl_op <- filter(station, sample_sep < threshold)
  }
  return(station)
}

#Filter fish op to get op dataset without holes
get_longest_consecutive_time_series <- function (x) {
  
  s <- split(x, cumsum(c(TRUE, diff(x) != 1)))
  s[[which.max(lengths(s))]] 
  
}
get_op_wo_holes <- function (.op = NULL) {
  
  sep_y <- .op %>%
    group_by(station) %>%
    mutate(year = as.integer(year)) %>%
    filter(year %in% get_longest_consecutive_time_series(year)) %>%
    filter(n() >= 10)
  
  sep_y <- sep_y %>%
    ungroup()
  
  message(".op had ", length(unique(.op$station)), " stations.")
  message("filtered dataset contains ", length(unique(sep_y$station)), " stations.")
  
  return(sep_y)
  
}