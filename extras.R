library(shiny)
library(shinyjs)
library(dplyr)
library(readr)
library(tidyr)

# this makes a red star next to questions
labelMandatory <- function(label) {
  tagList(
    span("*", class = "mandatory_star"), label
  )
}
appCSS <- ".mandatory_star { color: red; }"

# all the survey field ids
fieldsAll <- c("career_stage", "funding", "other_funding", "institution_type", "other_inst_type", 
               "institution_location", "h_index", "gender", "other_gender",
               "n_species", "n_genera", "n_families", "n_phyla",
               "molecular_imp", "physiological_imp", "individual_imp", "species_imp",
               "mechanism_imp", "ontology_imp", "adaptive_imp", "phylogeny_imp",
               "email", "comments")

humanTime <- function() format(Sys.time(), "%Y-%m-%d_%H.%M.%OS")

saveData <- function(data) {
  fileName <- sprintf("%s_%s.txt",
                      humanTime(),
                      digest::digest(data))
  
  write.table(x = data, file = file.path('responses', fileName), sep = "|")
}

loadData <- function() {
  files <- list.files('responses', full.names = TRUE)
  data <- lapply(files, read.delim, sep = "|", stringsAsFactors = FALSE, na.strings = c("NA", ""))
  data <- dplyr::bind_rows(data)
  data.frame(data, stringsAsFactors = FALSE) %>%
    mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"))
}

group_div <- function(data) {
  for (i in 1:nrow(data)) {
    data$tax_score[i] <- diversity(data[i, c("n_species", "n_genera", "n_families", "n_phyla")])
    data$bio_score[i] <- diversity(data[i, c("molecular_imp", "physiological_imp", "individual_imp", "species_imp")])
    data$tin_score[i] <- diversity(data[i, c("mechanism_imp", "ontology_imp", "adaptive_imp", "phylogeny_imp")])
  }
  data
}


updateData <- function() {
  all_data <<- loadData() %>%
    unique() %>%
    arrange(timestamp) %>%
    mutate(index = 1:n()) %>%
    group_div() %>%
    select(career_stage, funding, institution_type, institution_location, h_index, gender, tax_score, bio_score, tin_score) %>%
    filter(!is.na(tax_score), !is.na(bio_score), !is.na(tin_score)) %>%
    mutate(person_color = factor(c(rep("Older entries", nrow(.) - 1), "Most recent entry")),
           career_stage = factor(career_stage,
                                 levels = c('undergrad',
                                            'grad_stud',
                                            'postdoc',
                                            'assist_prof',
                                            'assoc_prof',
                                            'full_prof'),
                                 labels = c('Undergraduate',
                                            'Graduate student',
                                            'Postdoctoral researcher',
                                            'Assistant professor',
                                            'Associate professor',
                                            'Full professor')),
           institution_type = factor(institution_type,
                                     levels = c('research',
                                                'med_school',
                                                'lib_arts',
                                                'non_teaching',
                                                'other'),
                                     exclude = 'other',
                                     labels = c('R1 - research institution (basic research)',
                                                'R1 - medical school campus',
                                                'Liberal arts college',
                                                'Non-teaching research institute')),
           institution_location = factor(institution_location,
                                         levels = c('africa',
                                                    'asia',
                                                    'europe',
                                                    'north_america',
                                                    'oceania',
                                                    'south_america'),
                                         labels = c("Africa",
                                                    "Asia",
                                                    "Europe",
                                                    "North America",
                                                    "Oceania",
                                                    "South America")),
           gender = factor(gender,
                           levels = c('female',
                                      'male',
                                      'trans_woman',
                                      'trans_man',
                                      'gnc',
                                      'dna',
                                      'other'),
                           labels = c('Female',
                                      'Male',
                                      rep("Other categories", 5))))
  for_plotting <<- mutate(all_data,
                         tax_score = ifelse(tax_score == 0, tax_score, jitter(tax_score, amount = .1)),
                         bio_score = ifelse(bio_score == 0, bio_score, jitter(bio_score, amount = .1)),
                         tin_score = ifelse(tin_score == 0, tin_score, jitter(tin_score, amount = .1)))
}