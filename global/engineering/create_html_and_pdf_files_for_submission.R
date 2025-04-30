# Copy staging repo to student repo ----
## GRAPH Courses team
## 2021-03-27

#' Copies internal staging repo to a repo hosted on GitHub pages. Lessons are then embedded in our workspace

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages and functions ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, fs, cli, glue, xfun, parsermd)
pacman::p_load_gh("rstudio/pagedown", 
                  "rfortherestofus/pagedreport")

blue_print <- function(x) cat(cli::bg_blue(cli::col_white(cli::style_bold(x))))

##  render duplicate
# some tibble print options for the dfs
options(pillar.width = 60) # avoid overflow of tibbles
options(pillar.min_title_chars = 15,
        pillar.max_footer_lines = 2,
        pillar.min_chars = 15)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Render Rmds to regular HTML ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

current_dir <- here::here("lessons")

# List all files in the directory that start with 'ls0' and end with '.Rmd'
file_names <- list.files(path = current_dir, 
                         pattern = "^ls0.*\\.Rmd$")
file_names

selected_lessons <- file_names[!grepl("TEACHER", file_names)]

rmds_to_render <- 
  fs::dir_ls(current_dir, 
             regexp = paste0(selected_lessons, collapse = "|"),
             recurse = T)

# Render documents
for (rmd in rmds_to_render[7:length(rmds_to_render)]) {
  
  blue_print(paste0("Rendering: \n", rmd, 
                    "\n(", which(rmd == rmds_to_render), " of ", length(rmds_to_render), ")"
  ))
  rmarkdown::render(rmd)
}

 ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Render Rmds to PDF ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Start loop----

for (rmd in rmds_to_render[1:8]) {
  blue_print(paste0("Rendering: \n", rmd, 
                    "\n(", which(rmd == rmds_to_render), " of ", length(rmds_to_render), ")"
  ))
  
  # Add yaml to PDF
  yaml_to_append <- glue::glue('credits: "This document is a draft of a lesson made by the GRAPH Network,
                               a non-profit headquartered at the University of Geneva Global Health Institute, 
                               in collaboration with the Bill and Melinda Gates Foundation,
                               under a Bill and Melinda Gates Foundation 2024-2025 grant to create e-learning modules to build in-country capacity for data analysis and infectious disease modelling.
                               date: "`r format(Sys.Date(), "%B %Y")`"
                               author: "GRAPH Network, supported by the Bill and Melinda Gates Foundation"')
  
  # duplicate rmd
  duplicate_rmd <- str_replace(rmd, ".Rmd", "-duplicate-for-pagedown.Rmd")
  fs::file_copy(path = rmd, new_path = duplicate_rmd, overwrite = T)
  
  # modify duplicate
  rmd_modified <- 
    read_lines(rmd) %>% 
    str_replace_all("render = reactable_5_rows", "render = head_5_rows") %>%  # reactable does not work in this context it seems. Replace with regular renders
    str_replace_all("render = reactable_10_rows", "render = head_10_rows") # reactable does not work in this context it seems. Replace with regular renders
  
  
  # append then write
  write_lines(x = c(rmd_modified,"\n","---", yaml_to_append, "---"), 
              file = duplicate_rmd)
  
  
  output_html <- str_replace(rmd, ".Rmd", "-pagedown.html")
  rmarkdown::render(duplicate_rmd, 
                    output_file = output_html, 
                    output_format = "pagedown::html_paged",
                    output_yaml = here("global/style/_output_pagedown.yml")) 
  

  # convert pagedown html to a pdf
  output_pdf <- str_replace(rmd, ".Rmd", ".pdf")
  chrome_print(output_html, 
               output = output_pdf, wait = 10)
  
  # delete duplicate rmd & html
  unlink(duplicate_rmd)
  unlink(output_html)
  
  
}


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Copy the lessons folder into wp repo  ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

from <- here::here()
to <- stringr::str_replace(from, "staging", "wp")

# list files to be copied
search_string <- paste(c("lessons"),collapse = "|")
folders_to_copy <- dir_ls(from)[str_detect(dir_ls(from), search_string)]

fs::dir_copy(folders_to_copy, 
             str_replace(folders_to_copy, "staging", "wp"), 
             overwrite = TRUE)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Delete non HTML and non PDF stuff from the target repo  ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Now delete non HTML files (there is smarter way to do this [we should be able to copy JUST the html files], but I leave like this for now)
all_copied_files <- dir_ls(to, recurse = T, all = T)
files_to_delete <- all_copied_files[!str_ends(all_copied_files, "\\.pdf|\\.html|\\.Rproj|README.md") & 
                                      !str_detect(all_copied_files, "\\.git")]
file.remove(files_to_delete)

## finally delete empty folders with terminal command (as at 2022-03-17, this does not seem to delete everything)
system2(
  command = "find",
  args = c(to, "-empty", "-type d", "-delete")
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Delete PDFs stuff from the source repo  ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Now delete non HTML files (there is smarter way to do this [we should be able to copy JUST the html files], but I leave like this for now)
all_files <- dir_ls(from, recurse = T, all = T)
files_to_delete <- all_files[str_ends(all_files, "\\.pdf")]
file.remove(files_to_delete)

## finally delete empty folders with terminal command (as at 2022-03-17, this does not seem to delete everything)
system2(
  command = "find",
  args = c(to, "-empty", "-type d", "-delete")
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TODO: Pull then push -wp repo  ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TODO: Squash all but most recent commit in order to minimize space  ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



