library(here)
library(tidyverse)
library(jsonlite)

processed_data_directory <- here("..","data","processed_data")
file_name <- "massive_memory"

#read experiment data
exp_data <- read_csv(here(processed_data_directory,paste0(file_name,"-alldata.csv")))

exp_data <- exp_data %>%
  #clean up participant ids
  rename(participant_id = participant) %>%
  mutate(
    participant_id = case_when(
      participant_id == "9252" ~ "parrot",
      participant_id == "A18534325" ~ "moose",
      TRUE ~ trimws(tolower(participant_id))
    )
  )

#double check that participant ids are unique
counts_by_random_id <- exp_data %>%
  group_by(random_id,participant_id) %>%
  count()
#output to track participants
write_csv(counts_by_random_id,here(processed_data_directory,paste0(file_name,"-participant-list.csv")))

#extract final questionnaire responses
questionnaire_responses <- exp_data %>% 
  filter(trial_type == "survey-html-form") %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) 

#final debrief questions
debrief_responses <- exp_data %>% 
  filter(trial_type == "survey-text" & trial_index>1) %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json)

#join into exp_data
exp_data <- exp_data %>%
  left_join(select(questionnaire_responses,participant_id,age:memory_experiment_experience)) %>%
  left_join(debrief_responses %>% select(participant_id,task_guess) %>% filter(!is.na(task_guess))) %>%
  left_join(debrief_responses %>% select(participant_id,technical_issues_question) %>% filter(!is.na(technical_issues_question)))

#filter dataset
exp_data <- exp_data %>%
  filter(!is.na(trial_phase))

#filter participant ids
filter_ids <- c()

#identify participants from the experiment group
group_members <- c("camel","ladybug","alpaca","lamb","zebra")

processed_data <- exp_data %>%
  filter(!(participant_id %in% filter_ids)) %>%
  #flag for group participants
  mutate(participant_is_group_member = case_when(
    participant_id %in% group_members ~ TRUE,
    TRUE ~ FALSE
  
  )) %>%
  #remove unneeded columns
  select(-c(success,plugin_version,timeout:failed_video)) %>%
  #add trial_number
  group_by(participant_id) %>%
  mutate(trial_number = row_number()) %>%
  relocate(trial_number,.after=trial_index) %>%
  #convert reaction time to numeric
  mutate(rt = as.numeric(rt))
  
#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
