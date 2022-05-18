#' From a given name (typically, an institution), generates
#' possible variant names (by removing letters, duplicating letters or syllables)
#' @export
#' @param name character
#' @return a list of variant names
generate_variant_names <- function(name) {
    variants <- c()

    # prepare
    chars <- str_split(name, "")[[1]]
    # remove one letter
    for(i in seq_along(chars)) {
        variants <- c(variants, paste0(chars[-i], collapse=""))
    }
    # abbreviate
    sizes <- seq_len(nchar(name) - 2)
    for(i in sizes[3:length(sizes)]) {
        variants <- c(variants, substr(name, 1, i))
    }
    variants <- c(variants, substr(name, 2, nchar(name)))
    # double some letters
    for(i in seq_along(chars)) {
        variants <- c(variants, paste0(append(chars, chars[[i]], after = i), collapse = ""))
    }
    # duplicate syllables
    # TODO

    # we can also recursively do it on all the variants and then
    # only keep the unique ones
    variants
}


build_school_pattern <- function(name, patterns) {
    paste0("(^|\\s)", paste0(c(generate_variant_names(name), patterns), collapse = "|"), "(\\s|$)")
}

#' Normalize a school name into a canonical form
#' @param school_name character
#' @return canonical school name character
#' @export
normalize_school_name <- function(school_name) {
  str_to_upper(school_name) %>%
    str_replace_all(build_school_pattern("INSTITUT", "I\\.?N?\\.?S\\.?T\\.?"),
                    "\\1INSTITUT\\2") %>%
    str_squish()
}

#' Matches schools across fields and the years, attributing them a unique id
#' @param years a list of exetat results. The first table of the list will be the reference
#' @export
#' @return a list of the data frames with each an additional column: school_id
match_schools <- function(years) {
  stopifnot(length(years) > 0)
  # TODO: check if there all the column names

  # Filter by provinces

  # First match schools across fields
}
