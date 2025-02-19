# This file contains all the code needed to parse and print various sections of your CV
# from data. Feel free to tweak it as you desire!


#' Create a CV_Printer object.
#'
#' @param data_location Path of the spreadsheets holding all your data. This can be
#'   either a URL to a google sheet with multiple sheets containing the four
#'   data types or a path to a folder containing four `.csv`s with the neccesary
#'   data.
#' @param source_location Where is the code to build your CV hosted?
#' @param pdf_mode Is the output being rendered into a pdf? Aka do links need
#'   to be stripped?
#' @param sheet_is_publicly_readable If you're using google sheets for data,
#'   is the sheet publicly available? (Makes authorization easier.)
#' @return A new `CV_Printer` object.
create_CV_object <-  function(data_location,
                              pdf_mode = FALSE,
                              sheet_is_publicly_readable = TRUE) {

  cv <- list(
    pdf_mode = pdf_mode,
    links = c()
  )

  is_google_sheets_location <- stringr::str_detect(data_location, "docs\\.google\\.com")

  if(is_google_sheets_location){
    if(sheet_is_publicly_readable){
      # This tells google sheets to not try and authenticate. Note that this will only
      # work if your sheet has sharing set to "anyone with link can view"
      googlesheets4::gs4_deauth()
    } else {
      # My info is in a public sheet so there's no need to do authentication but if you want
      # to use a private sheet, then this is the way you need to do it.
      # designate project-specific cache so we can render Rmd without problems
      options(gargle_oauth_cache = ".secrets")
    }

    read_gsheet <- function(sheet_id){
      googlesheets4::read_sheet(data_location, sheet = sheet_id, skip = 1, col_types = "c")
    }
    cv$entries_data  <- read_gsheet(sheet_id = "entries")
    cv$lang          <- read_gsheet(sheet_id = "language_skills")
    cv$text_blocks   <- read_gsheet(sheet_id = "text_blocks")
    cv$contact_info  <- read_gsheet(sheet_id = "contact_info")
    cv$skills        <- read_gsheet(sheet_id = "skills")
    cv$references    <- read_gsheet(sheet_id = "references")
    cv$publications  <- read_gsheet(sheet_id = "publications")
    cv$presentations <- read_gsheet(sheet_id = "presentations")
  } else {
    # Want to go old-school with csvs?
    cv$entries_data <- readr::read_csv(paste0(data_location, "entries.csv"), skip = 1)
    cv$lang       <- readr::read_csv(paste0(data_location, "language_skills.csv"), skip = 1)
    cv$text_blocks  <- readr::read_csv(paste0(data_location, "text_blocks.csv"), skip = 1)
    cv$contact_info <- readr::read_csv(paste0(data_location, "contact_info.csv"), skip = 1)
    cv$skills        <- read_gsheet(sheet_id = "skills.csv")
    }


  extract_year <- function(dates){
    date_year <- stringr::str_extract(dates, "(20|19)[0-9]{2}")
    date_year[is.na(date_year)] <- lubridate::year(lubridate::ymd(Sys.Date())) + 10

    date_year
  }

  parse_dates <- function(dates){

    date_month <- stringr::str_extract(dates, "(\\w+|\\d+)(?=(\\s|\\/|-)(20|19)[0-9]{2})")
    date_month[is.na(date_month)] <- "1"

    paste("1", date_month, extract_year(dates), sep = "-") %>%
      lubridate::dmy()
  }

  # Clean up entries dataframe to format we need it for printing
  cv$entries_data %<>%
    tidyr::unite(
      tidyr::starts_with('description'),
      col = "description_bullets",
      sep = "\n- ",
      na.rm = TRUE
    ) %>%
    dplyr::mutate(
      description_bullets = ifelse(description_bullets != "", paste0("- ", description_bullets), ""),
      start = ifelse(start == "NULL", NA, start),
      end = ifelse(end == "NULL", NA, end),
      start_year = extract_year(start),
      end_year = extract_year(end),
      no_start = is.na(start),
      has_start = !no_start,
      no_end = is.na(end),
      has_end = !no_end,
      timeline = dplyr::case_when(
        no_start  & no_end  ~ "N/A",
        no_start  & has_end ~ as.character(end),
        has_start & no_end  ~ paste("Present", "-", start),
        TRUE                ~ paste(end, "-", start)
      )
    ) %>%
    dplyr::arrange(desc(parse_dates(end))) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .)) %>%
    dplyr::mutate(timeline = ifelse(timeline == 1999, "N/A", timeline))
  
  cv$publications %<>%
    dplyr::mutate(
      doi = ifelse(doi == "NULL", NA, doi),
      link = ifelse(link == "NULL", NA, link),
      no_doi = is.na(doi),
      has_doi = !no_doi,
      no_link = is.na(link),
      has_link = !no_link,
      doiPrint = dplyr::case_when(
        no_doi  ~ " ",
        has_doi ~ as.character(doi)
      ),
      linkPrint = dplyr::case_when(
        no_link ~ " ",
        has_link ~ as.character(link)
      )
    ) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), ' ', .))
  
  cv
}


#' @description Take a position data frame and the section id desired and prints the section to markdown.
#' @param section_id ID of the entries section to be printed as encoded by the `section` column of the `entries` table
print_section <- function(cv, section_id, glue_template = "default"){

  if(glue_template == "default"){
    glue_template <- "
### {title}

{institution}

{loc}

{timeline}

{description_bullets}
\n\n\n"
  }

  section_data <- dplyr::filter(cv$entries_data, section == section_id)

  print(glue::glue_data(section_data, glue_template))

  invisible(cv)
}

######

#' @description Take a position data frame and the section id desired and prints the section to markdown.
#' @param section_id ID of the entries section to be printed as encoded by the `section` column of the `entries` table
new_link = "{target=_blank}"
print_publication <- function(cv, section_id, glue_template = "pub"){
  if(glue_template == "pub"){
    glue_template <- "
### {title}
    
*{journal}* ({year}). [{doi}]({link}){new_link}
    
N/A
    
{year2}
    
{authors}
\n\n\n"
  }
  
  section_data <- dplyr::filter(cv$publications, section == section_id)
  
  print(glue::glue_data(section_data, glue_template))
  
  invisible(cv)
}


print_publication_prep <- function(cv, section_id, glue_template = "pub"){
  if(glue_template == "pub"){
    glue_template <- "
### {title}

*{journal}*

N/A

N/A

{authors} 
\n\n\n"
  }
  
  section_data <- dplyr::filter(cv$publications, section == section_id)
  
  print(glue::glue_data(section_data, glue_template))
  
  invisible(cv)
}

#######

#' @description Take a position data frame and the section id desired and prints the section to markdown.
#' @param section_id ID of the entries section to be printed as encoded by the `section` column of the `entries` table
print_presentation <- function(cv, section_id, glue_template = "pres"){
  
  if(glue_template == "pres"){
    glue_template <- "
### {title}
    
{name}
    
{loc}
    
{year}
    
{authors}<br>
{type}
\n\n\n"
  }
  
  section_data <- dplyr::filter(cv$presentations, section == section_id)
  
  print(glue::glue_data(section_data, glue_template))
  
  invisible(cv)
}


#######

#' @description Prints out text block identified by a given label.
#' @param label ID of the text block to print as encoded in `label` column of `text_blocks` table.
print_text_block <- function(cv, label){
  text_block <- dplyr::filter(cv$text_blocks, loc == label) %>%
    dplyr::pull(text)

  # strip_res <- sanitize_links(cv, text_block)

  # cat(cv$text)

  invisible(cv)
  # cat(strip_res$text)
  # 
  # invisible(strip_res$cv)
}



#' @description Construct a bar chart of skills
#' @param out_of The relative maximum for skills. Used to set what a fully filled in skill bar is.
print_skill_bars <- function(cv, out_of = 5, bar_color = "#83DDE0", bar_background = "#d9d9d9", glue_template = "default"){

  if(glue_template == "default"){
    glue_template <- "
<div
  class = 'skill-bar'
  style = \"background:linear-gradient(to right,
                                      {bar_color} {width_percent}%,
                                      {bar_background} {width_percent}% 100%)\"
>{language}</div>"
  }
  cv$lang %>%
    dplyr::mutate(width_percent = round(100*as.numeric(level)/out_of)) %>%
    glue::glue_data(glue_template) %>%
    print()

  invisible(cv)
}


#' @description List of all links in document labeled by their superscript integer.
print_links <- function(cv) {
  n_links <- length(cv$links)
  if (n_links > 0) {
    cat("
Links {data-icon=link}
--------------------------------------------------------------------------------

<br>


")

    purrr::walk2(cv$links, 1:n_links, function(link, index) {
      print(glue::glue('{index}. {link}'))
    })
  }

  invisible(cv)
}


#' @description Contact information section with icons
print_contact_info <- function(cv){
  glue::glue_data(
    cv$contact_info,
    "- <i class='fa fa-{icon}'></i> {contact}"
  ) %>% print()
  
  invisible(cv)
}


#' @description Skills information section 
print_skills <- function(cv){
  glue::glue_data(
    cv$skills,
    "- {skill}"
  ) %>% print()
  
  invisible(cv)
}

#' @description Reference contacts section 
print_references <- function(cv){
    glue::glue_data(
      cv$references,
      "### {person}

      {position}

      {institution}

      N/A

      ::: concise
      - {phone}<br>
      - <br>
      - <span style='padding-left:62px'>**{email}**</span>
      :::"
      ) %>% print()
  
  invisible(cv)
  }