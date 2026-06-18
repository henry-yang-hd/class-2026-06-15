library(tidyverse)
library(rvest)

# 1. Read the Wikipedia page for 2026 World Cup Squads
url <- "https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_squads"
webpage <- read_html(url)

# 2. Extract all country names from the <h3> headings
countries <- webpage %>% 
  html_nodes("h3") %>% 
  html_text() %>% 
  str_remove("\\[edit\\]") %>% 
  str_trim()

# 3. Locate all the squad tables on the page
all_tables <- webpage %>% html_nodes("table.wikitable")

compiled_squads <- list()
table_counter <- 1

for (i in seq_along(countries)) {
  if (table_counter > length(all_tables)) break
  
  potential_table <- all_tables[[table_counter]]
  headers <- potential_table %>% html_nodes("th") %>% html_text() %>% str_trim()
  
  if (any(str_detect(headers, "Player")) && any(str_detect(headers, "Club"))) {
    
    squad_df <- html_table(potential_table, fill = TRUE)
    
    # Standardize column headers to lowercase and clean symbols
    colnames(squad_df) <- tolower(gsub("[[:punct:] ]+", "_", colnames(squad_df)))
    
    # Check if necessary columns exist inside the parsed data
    if ("player" %in% colnames(squad_df)) {
      
      cleaned_df <- squad_df %>%
        rename(position_raw = matches("^pos")) %>%
        mutate(
          country = countries[i],
          player_name = str_trim(gsub("\\(captain\\)", "", player)),
          
          position = case_when(
            str_detect(position_raw, "GK") ~ "Goalkeeper",
            str_detect(position_raw, "DF") ~ "Defender",
            str_detect(position_raw, "MF") ~ "Midfielder",
            str_detect(position_raw, "FW") ~ "Forward",
            TRUE ~ as.character(position_raw)
          ),
          
          # Handle the age/birth column safely
          age_column = coalesce(!!!select(., matches("date_of_birth|age"))),
          
          # Extract numerical age
          age = as.numeric(str_extract(age_column, "(?<=aged\\s)\\d+")),
          
          # Extract the YYYY-MM-DD date format inside the parentheses
          birthday = str_extract(age_column, "\\d{4}-\\d{2}-\\d{2}"),
          
          # FIX: Safely pull the club column and map it directly to club_team
          club_raw = coalesce(!!!select(., matches("^club$|^club_"))),
          club_team = str_trim(gsub(".*Association|.*Federation|.*Union", "", club_raw))
        ) %>%
        select(country, player_name, position, age, birthday, club_team)
      
      compiled_squads[[length(compiled_squads) + 1]] <- cleaned_df
    }
    
    table_counter <- table_counter + 1
  }
}

# 4. Bind all data frames together
final_world_cup_roster <- bind_rows(compiled_squads)

# 5. Export to CSV
write_csv(final_world_cup_roster, "all_world_cup_2026_squads.csv")

message("Process complete! All rosters combined and saved to 'all_world_cup_2026_squads.csv'.")
