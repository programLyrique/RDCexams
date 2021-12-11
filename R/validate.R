#' Validate the information extracted from the results of the exam
#'
#'  @export
#' @import dplyr assertr
validate <- function(extracted) {
    extracted %>%
        verify(has_all_names("option", "code_option", "province", "code_province",
                             "year", "page", "school_index", "school", "code_school",
                             "nb_participants", "nb_females", "nb_success", "nb_success_females",
                             "ranking", "name", "gender", "mark")) %>%
        #verify(page - lag(page, default = 0) <= 1) %>% # There are no gaps in the pages
        # If they have a name, they also have a mark
        verify(if_else(not_na(name), not_na(mark), TRUE)) %>%
        #if female student number is NA, there are successful participants, then all genders are NA
        verify(if_else(is.na(nb_females) & nb_success > 0, is.na(gender), TRUE)) %>%
        # if there are at leats one success, then no names should be NA
        verify(nb_success == 0 | !is.na(name)) %>%
        # every school has a school code (and there are no province without school)
        verify(not_na(school) & not_na(code_school)) %>%
        # there are no more successful female students than there are females
        verify(if_else(not_na(nb_females) & not_na(nb_success_females), nb_females >= nb_success_females, TRUE)) %>%
        # There are as many successful female participants as the lines with
        # gender "F"
        # Same for the overall number of successes
        # There are sometimes duplicated students also
        group_by(province, school, code_school) %>% # assertr does not do grouped assertion :()
        mutate(unique_students = n_distinct(name, gender, mark), n_displayed = n()) %>%
        mutate(school_female_witness = nb_success == 0 | sum(gender == "F", na.rm =  TRUE) == first(nb_success_females)) %>%
        mutate(school_success_witness = nb_success == 0 | n() == first(nb_success)) %>%
        verify(nb_participants == 0 | unique_students == n_displayed) %>%
        verify(school_female_witness) %>%
        verify(school_success_witness) %>%
        select(-school_female_witness, -school_success_witness) %>%
        ungroup() %>%
        verify(year == first(year)) %>%
        assert(within_bounds(0, 100), mark)
}


validate_file <- function(filename) {
    csv <- read_csv(filename)

    res <- tryCatch(validate(csv),
        error = function(e) conditionMessage(e))

    if(is.character(res)) {
        return(res)
    } else {
        return(NA_character_)
    }
}

validate_folder <- function(foldername) {
    if(!dir.exists(foldername)) {
        stop("Directory does not exist or you do not have rights to read in it. Check the spelling for it: ", foldername)
    }
    files <-  list.files(foldername, pattern = ".*\\.csv", recursive = TRUE, full.names = TRUE)

    report <- tibble(file = files)

    report %>% mutate(error = purrr::map_chr(file, validate_file))
}
