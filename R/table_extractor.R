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
extract_results <- function(pdf_pages, correct = FALSE) {
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

    student_cols <- cols(
        ranking = col_integer(),
        name = col_character(),
        gender = col_factor(levels = c("M", "F")),
        mark = col_integer()
    )

    page_infos <- list()

    # compute scale factor in x
    x_min_max <- range(pdf_pages[[1]]$x)
    width_real <- x_min_max[[2]] - x_min_max[[1]]
    x_min_usual <- 84
    x_max_usual <- 553
    width_usual <- x_max_usual - x_min_usual
    x_scale <- width_real / width_usual

    # column separations fore the usual ones
    start_column2 <- x_min_max[[1]] + (221 - x_min_usual + 12) * x_scale # 228
    start_column3 <- x_min_max[[1]] + (380 - x_min_usual + 12) * x_scale # 391

    for(page_num in seq_along(pdf_pages)) {
        # sort rows by increasing y (they are not!)
        page <- arrange(pdf_pages[[page_num]], y)

        # Extract option, province, year
        # No need to extract page number as it already corresponds to
        # the index in the pdf_pages list
        y_coord_first <- page[1,]$y + 3
        y_coord_last <-page[nrow(page),]$y - 3

        first_line <- page %>% filter(y < y_coord_first) %>%
            stabilize_rows() %>%
            arrange(x)

        start_province_block <- first_line %>% filter(text == "Province") %>% pull(x)

        option_line <- first_line %>% filter(x < start_province_block - 10) %>% pull(text) %>% paste0(collapse = " ")
        province_line <-first_line %>% filter(x >= start_province_block - 10) %>% pull(text) %>% paste0(collapse = " ")

        last_line <- page %>% filter(y > y_coord_last) %>%
            stabilize_rows() %>% arrange(x) %>% pull(text)

        res_option <- str_match(option_line, "Option\\s+:\\s+(.*)\\s+-\\s+Code\\s+:\\s+(\\d+)")
        res_province <- str_match(province_line, "Province\\s+:\\s+(.*)\\s+-\\s+Code\\s+:\\s+(\\d+)")
        option <- res_option[[2]]
        code_option <- as.integer(res_option[[3]])
        province <- res_province[[2]]
        code_province <- as.integer(res_province[[3]])
        year <- last_line %>% paste0(collapse = " ") %>% str_match("(\\d+)\\s+http://") %>% .[,2] %>% as.integer

        page <- page %>% filter(y > y_coord_first + 2, y < y_coord_last - 2)

        if(nrow(page) == 0) {
            # the page is empty
            # See Kinshasa Est 2016

            # we do not insert a row here (but we could...)
            # it will make one of the assertion in validate fail...
            next
        }

        # Detect schools

        # First we separate columns
        #start_column2 <- 221 + 12 # 228
        #start_column3 <- 374 + 12 # 391

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



        # The school starts with their name in bold font (but not italic)
        start_school_block <- schools %>%
            group_by(y) %>%
            summarize(font_name = first(font_name)) %>%
            filter(str_detect(font_name, "Bold$|\\+F1$")) %>%
            select(y) %>%
            mutate(school_index = row_number())


        schools <- schools %>% left_join(start_school_block, by="y")
        schools <- tidyr::fill(schools, school_index) %>%
            mutate(school_index = replace_na(school_index, 0))



        # Extract school information
        # the 1st 4 lines of a block
        # Some blocks have Participants and Réussites misaligned by one pixel...
        # We need to correct that first
        school_info <- schools %>%
            filter(school_index != 0) %>%
            group_by(school_index, y) %>%
            summarize(text = paste0(text, collapse = " ")) %>%
            #stabilize_rows() %>%
            slice_head(n = 4) %>%
            summarise(
                school = cur_data()[1,]$text,
                code_school = str_remove(cur_data()[2, "text"], fixed("Code : ")),
                # we assume that there is at least one participant
                nb_participants = as.integer(str_match(cur_data()[3, "text"], "Participants? : ((?:\\d)+)")[2]),
                # Slightly trickier because they write "Zéro" instead of "0"!
                # Sometimes, there is no gender in the data. So we just keep NA there
                nb_females = replace_na(str_match(cur_data()[3, "text"], "Dont : ((?:\\d)+|Zéro) F")[2], "0"),
                nb_success = replace_na(str_match(cur_data()[4, "text"], "Réussites?\\s+:\\s*((?:\\d)+|Zéro)")[2], "0"),
                nb_success_females = str_match(cur_data()[4, "text"], "Dont : ((?:\\d)+|Zéro) F")[2],
                end_block_y = max(y)
                ) %>%
            # replace by a map on the columns?
            mutate(nb_females = if_else(nb_females == "Zéro", 0L, as.integer(nb_females)),
                   nb_success = if_else(nb_success == "Zéro", 0L, as.integer(nb_success)),
                   nb_success_females = if_else(nb_success_females == "Zéro" | nb_females == 0, 0L, as.integer(nb_success_females)))


        # Is there any continuation of a school on the previous page?
        # For that, we just need to detect if there are any rows before the first
        # start_school_block (remember we already remove the line with the option and province)
        # that is to say, does the first row has a first row with a NA school_index
        if(schools[1,"school_index"] == 0) {
            if(page_num == 1) {
                warning("Detected students without a school on the first page!")
            } else {
                prev_school_info <- tail(page_infos[[page_num - 1]], n =1) %>%
                    select(contains("school"), starts_with("nb")) %>%
                    mutate(school_index = 0, end_block_y = schools[1,]$y - 15)
                school_info <- bind_rows(prev_school_info, school_info)

                # correct the previous page information
                page_infos[[page_num - 1]] <- page_infos[[page_num - 1]] %>%
                    # if the last school on the page states there are participants, but
                    # they are no one here, it means there is a continuation with the student name
                    # later on. So remove that line with the NA.
                    filter(!(school_index == max(school_index) & nb_success > 0 & is.na(name)))
                    # We need to remove it here and not while parsing the page, otherwise,
                    # the continuation is not detected properly for the next page.
            }
        }


        end_block <- school_info %>% select(school_index, end_block_y)

        # Extract student information
        # The remaining lines
        student_info <- schools %>%
            group_by(school_index, y) %>%
            left_join(end_block, by = "school_index") %>%
            # Add some tolerance
            filter(y > end_block_y + 5) %>%
            summarize(text = paste0(text, collapse = " ")) %>%
            # Gender is sometimes omitted
            extract(text, c("ranking", "name", "gender", "mark"), regex = "((?:\\d)+)?\\s+(.*?)\\s+(?:(M|F|m|f)\\s+)?((?:\\d)+)$") %>%
            mutate(gender = str_to_upper(gender)) %>%
            type_convert(student_cols) %>%
            select(-y)

        # Now let's join everything together!!
        school_info <- select(school_info, -end_block_y) %>%
            mutate(option = option, code_option = code_option,
                   province = province, code_province = code_province,
                   year = year,
                  page = page_num, .before = school_index)
        page_infos[[page_num]] <- school_info %>% left_join(student_info, by = "school_index")
        }

    res <- bind_rows(page_infos) %>%
        group_by(province, school, code_school) %>%
        # Correct the number of female successes if it is NA but the number of female
        # participants is not NA
        mutate(nb_success_females =
                   if_else(is.na(nb_success_females) & !is.na(nb_females),
                           sum(gender == "F"),
                           nb_success_females)) %>%
        mutate(female_success_discrepancy = nb_success!= 0 & sum(gender == "F", na.rm =  TRUE) != first(nb_success_females)) %>%
        mutate(school_success_discrepancy = nb_success!= 0 & n() != first(nb_success))


    if(correct) {
     res%>%
        # Remove duplicated students
        group_modify(~ distinct(.x, name, gender, mark, .keep_all = TRUE)) %>%
        ungroup()
    }
    else {
        # Add some columns to warn about possible errors
        res %>%
            mutate(duplicated_students = nb_participants !=0 & n() != n_distinct(name, gender, mark)) %>%
            ungroup()
    }
}

#' Extract the exam information
#'
#' @param filename character vector. The filename (or path)
#' @param pages NULL for the whole pdf. A range for a part of the pdf
#'
#' @return  a tibble
#'
#' @export
extract_from_file <- function(filename, pages=NULL, correct = FALSE) {
    # A list of tibbles, one per page
    message("Loading data from the pdf.\n")
    pdf_pages <- pdftools::pdf_data(filename, font_info = TRUE)
    if(!is.null(pages)) {
        pdf_pages <- pdf_pages[pages]
    }
    message("Extracting the data.\n")
    extract_results(pdf_pages, correct = correct)
}

#' @export
extract_from_folder <- function(foldername, destdir =".", only_missing=FALSE, correct = FALSE) {
    if(!dir.exists(destdir)) {
        stop("Destination directory does not exist or you do not have rights to write in it. Check the spelling for it: ", destdir)
    }

    files <- list.files(foldername, pattern = ".*\\.pdf", recursive = TRUE, full.names = TRUE)


    furrr::future_map_chr(files,
                          .options = furrr::furrr_options(packages = "RDCexams"),
                          .progress = TRUE,
                          function(file) {
        if(only_missing) {
            year <- basename(dirname(file))
            filename <- paste0(destdir, "/", str_replace(basename(file), "\\.pdf$", paste0("-", year, ".csv")))
            if(file.exists(filename)) {
                return(filename)
            }
        }
        cat("Extracting", file, "\n")
        res <- extract_from_file(file, correct = correct)
        year <- pull(res[1,], year)
        if(nrow(res) > 0) {
            dest_file <- paste0(destdir, "/", str_replace(basename(file), "\\.pdf$", paste0("-", year, ".csv")))
            readr::write_csv(res, dest_file)
            dest_file
        }
        else {
            ""
        }
    })
}
