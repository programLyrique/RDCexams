# A very simple fixed point algorithm
# the goal is to adjust the y coordinate of items that are close together
# to coalesce them into their proper row
stabilize_rows <- function(df, tolerance = 2, n = 3) {
    res <- mutate(df, y = if_else(abs(y - lag(y, default = df[1,]$y))  <= tolerance, lag(y,  default = df[1,]$y), y))
    if(n == 0 || any(df$y != res$y)) {
        stabilize_rows(res, tolerance, n - 1)
    }
    else {
        res
    }
}


#' @export
#' @import dplyr tidyr stringr lubridate readr
extract_results <- function(pdf_pages) {
    # For documentation purposes
    # results <- tibble(
    #     option = character(0),
    #     code_option = integer(0),
    #     province = character(0),
    #     code_province = integer(0),
    #     year = Date(0),
    #     page = integer(0),
    #     school = character(0),
    #     school_code = character(0), # we can also divide it into its 4 components
    #     nb_participants = integer(0),
    #     nb_females = integer(0),
    #     nb_success = integer(0),
    #     nb_success_females = integer(0),
    #     # is it really the ranking or just an useless index we do not need?
    #     ranking = integer(0),
    #     name = character(0),
    #     gender = factor(levels = c("M", "F")),
    #     mark = integer(0)
    # )

    page_infos <- list()

    option <- character(0)
    code_option <- integer(0)
    province <- character(0)
    code_province <- integer(0)
    year <- Date(0)

    for(page_num in seq_along(pdf_pages)) {
        # sort rows by increasing y (they are not!)
        page <- arrange(pdf_pages[[page_num]], y)

        # Extract option, province, year
        # No need to extract page number as it alreaady corresponds to
        # the index in the pdf_pages list
        y_coord_first <- page[1,]$y
        y_coord_last <-page[nrow(page),]$y

        if(page_num == 1) {
            first_line <- page %>% filter(y == y_coord_first) %>% pull(text)
            last_line <- page %>% filter(y == y_coord_last) %>% pull(text)
            # Might be a bit fragile if the option or the province
            # are composed of several words.
            option <- first_line[[3]]
            code_option <- first_line[[7]]
            province <- first_line[[10]]
            code_province <- first_line[[15]]
            year <- last_line[[8]]
        }

        page <- page %>% filter(!y %in% c(y_coord_first, y_coord_last))

        # Detect schools

        # First we separate columns
        start_column2 <- 221 + 12 # 228
        start_column3 <- 374 + 12 # 391

        column1 <- page %>% filter(x < start_column2)
        column2 <- page %>% filter(x > start_column2, x < start_column3)
        column3 <- page %>% filter(x > start_column3)

        # we manually pile up the columns to make only one column
        offset <- y_coord_last - y_coord_first
        column2 <- column2 %>% mutate(y = y + offset)
        column3 <- column3 %>% mutate(y = y + 2 * offset)

        schools <- bind_rows(column1, column2, column3) %>%
            arrange(y, x) %>% # it was not always sorted by x also!!
            stabilize_rows() # correct subtle difference in y in rows

        # A school block ends just before a new block start, or
        # at the end of the document
        start_school_block <- schools %>%
            filter(text %in% c("INSTITUT", "COLLEGE", "AUTODIDACTES", "SEMINAIRE")) %>%
            select(y) %>%
            mutate(school_index = row_number())

        schools <- schools %>% left_join(start_school_block, by="y")
        schools <- tidyr::fill(schools, school_index)

        grouped_schools <- schools %>% group_by(school_index, y)

        # Extract school information
        # the 1st 4 lines of a block
        # Some blocks have Participants and Réussites misaligned by one pixel...
        # We need to correct that first
        school_info <- grouped_schools %>%
            summarize(text = paste0(text, collapse = " ")) %>%
            #stabilize_rows() %>%
            slice_head(n = 4) %>%
            summarise(
                school = cur_data()[1,]$text,
                code_school = str_remove(cur_data()[2, "text"], fixed("Code : ")),
                # we assume that there is at least one participant
                nb_participants = as.integer(str_match(cur_data()[3, "text"], "Participants? : ((?:\\d)+)")[2]),
                # Slightly trickier because they write "Zéro" instead of "0"!
                nb_females = replace_na(str_match(cur_data()[3, "text"], "Dont : ((?:\\d)+|Zéro) F")[2], "0"),
                nb_success = replace_na(str_match(cur_data()[4, "text"], "Réussites?\\s+:\\s*((?:\\d)+|Zéro)")[2], "0"),
                nb_success_females = replace_na(str_match(cur_data()[3, "text"], "Dont : ((?:\\d)+|Zéro) F")[2], "0"),
                end_block_y = max(y)
                ) %>%
            # replace by a map on the columns?
            mutate(nb_females = if_else(nb_females == "Zéro", 0L, as.integer(nb_females)),
                   nb_success = if_else(nb_success == "Zéro", 0L, as.integer(nb_success)),
                   nb_success_females = if_else(nb_success == "Zéro", 0L, as.integer(nb_success)))

        end_block <- school_info %>% select(school_index, end_block_y)
        # Extract student information
        # The remaining lines
        student_info <- grouped_schools %>% left_join(end_block) %>%
            # Add some tolerance
            filter(y > end_block_y + 5) %>%
            summarize(text = paste0(text, collapse = " ")) %>%
            extract(text, c("ranking", "name", "gender", "mark"), regex = "((?:\\d)+)\\s+(.*)\\s+(M|F)\\s+((?:\\d)+)", convert = TRUE) %>%
            select(-y)

        # Now let's join everything together!!
        school_info <- select(school_info, -end_block_y)
        page_infos[[page_num]] <- school_info %>% left_join(student_info)
    }

    bind_rows(page_infos) %>%
        mutate(option = option, code_option = code_option,
               province = province, code_province = code_province,
               year = year,
               page = page_num,
               before = school_index)
}


#'export
extract_from_file <- function(filename) {
    # A list of tibbles, one per page
    pdf_pages <- pdftools::pdf_data(filename)
    extract_results(pdf_pages)
}

#'export
extract_from_folder <- function(foldername) {
    files <- list.files(foldername, pattern = ".*\\.pdf", recursive = TRUE, full.names = TRUE)

    # we might want to parallelize that
    for(file in files) {
        res <- extract_from_file(file)
        if(nrow(res) > 0) {
            readr::write_csv(res, str_replace(file, "\\.pdf$", ".csv"))
        }
    }
}
