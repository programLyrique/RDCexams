#' split the school name into tokens
#' @param school character vector
tokens_from_school <- function(school) {
    str_split(school, pattern=boundary("word"))[[1]]
}


#' find the institute, school, keywords
#' @param tokens character vector
find_school_type <- function(tokens) {
    for(school_type in c("INSTITUT", "ECOLE", "ITA", "INST", "LYCEE", "COLLEGE", "COLL", "GROUPE", "SCOLAIRE")) {
        distances <- stringdist(tokens, school_type, method = "osa")

        # Is there a token that is similar?
        similar_pos <- Position(function(d) d <= 2, distances)

        # There is one matching token
        if(!is.na(similar_pos) ) {
            school_type_short <- if(school_type == "INSTITUT") {
                "INST"
            }
            else if(school_type %in% c("GROUPE", "SCOLAIRE")) {
                school_type_short <- "C.S"
                if(school_type == "GROUPE") {
                    similar_pos <- c(similar_pos, similar_pos + 1)
                }
                else {
                    similar_pos <- c(similar_pos - 1, similar_pos)
                }
            }
            else if(school_type == "COLLEGE") {
                school_type_short <- "COLL"
            }
            return(c(similar_pos, school_type_short))
        }
    }
    return(NA)
}

#' Normalization
#' @param school character vector of the school name
#' @export
#' @return a normalized version of the school name
normalize_schools <- function(school) {
    tokens <- tokens_from_school(school)
    school_type <- find_school_type(tokens)

    if(length(school_type) > 1) {
        # remove the token that corresponded to the school type
        school_name <- tokens[-strtoi(school_type[[1]])]
        # always append the school type at the beginning
        school_name <- c(school_type[[2]], school_name)

        return(paste(school_name, collapse = " "))
    }

    return(paste(tokens, collapse = " "))
}
