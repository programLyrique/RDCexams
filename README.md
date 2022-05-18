# Table extractor for the exam results in RDC

This defines a function `extract_from_file` which takes a pdf (as filename) as input.

The function outputs a tibble with the following columns:

```R
tibble(
    option = character(0),
    code_option = integer(0),
    province = character(0),
    code_province = integer(0),
    year = Date(0),
    school = character(0),
    school_code = character(0), 
    nb_participants = integer(0),
    nb_females = integer(0),
    nb_success = integer(0),
    nb_success_females = integer(0),
    ranking = integer(0),
    name = character(0),
    gender = factor(levels = c("M", "F")),
    mark = integer(0)
)
```

Then, you can save the tibble as a CSV file for instance.


You can also use `extract_from_folder` to process all the pdfs in a folder (and its sub-folders). This one requires `furrr` and can extract the files in parallel:

```R
library(furrr)

plan(multisession, workers = parallel::detectCores() - 1)

# To extract only years 2015 to 2021
folders <- paste0("ExamenEtatDRC/", 2015:2021)

RDCexams::extract_from_folder(folders, destdir = "ExamenEtatDRC/extracted/")
```

## Installing

```R
devtools::install_github("programLyrique/RDCexams")
```
