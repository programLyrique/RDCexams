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
        verify(page - lag(page, default = 0) <= 1) %>% # There are no gaps in the pages
        # If they have a name, they also have a gender and a mark
        verify(if_else(not_na(name), not_na(gender) & not_na(mark), TRUE)) %>%
        # every school has a school code (and there are no province without school)
        verify(not_na(school) & not_na(code_school)) %>%
        # There are as many successful female participants as the lines with
        # gender "F"
        # Same for the overall number of participants
        group_by(school, code_school) %>% # assertr does not do grouped assertion :()
        mutate(school_female_witness = nb_success == 0 | sum(gender == "F") == first(nb_success_females)) %>%
        mutate(school_success_witness = nb_success == 0 | n() == first(nb_success)) %>%
        verify(school_female_witness) %>%
        verify(school_success_witness) %>%
        select(-school_female_witness, -school_success_witness) %>%
        ungroup()
}
