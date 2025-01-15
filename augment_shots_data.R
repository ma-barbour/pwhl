# Function to augment shot data in raw play-by-play

# Load libraries

library(tidymodels)
library(readr)
library(jsonlite)
library(httr)
library(lubridate)

# Load xG model

xg_model <- read_rds("pwhl_xg_model.rds")

# Function for augmenting the PBP shots data

augment_shots <- function(raw_pbp_data) {
        
        # Add helper functions
        
        adjust_event_location <- function(pbp_data) {
                
                pbp_data <- pbp_data |>
                        mutate(x_location = (x_location - 300) * 0.3333,
                               y_location = (y_location - 150) * 0.2833)
                
                return(pbp_data)
                
        }
        
        add_shot_distance <- function(pbp_data) {
                
                find_sides <- pbp_data |>
                        filter(event == "shot") |>
                        group_by(game_id, 
                                 event_team) |>
                        summarize(mean_shot = mean(x_location,
                                                   na.rm = TRUE),
                                  .groups = "drop") 
                
                pbp_data <- pbp_data |>
                        left_join(find_sides, 
                                  by = join_by(game_id, 
                                               event_team))
                
                pbp_data <- pbp_data |>
                        mutate(distance = case_when(
                                mean_shot > 0 & event == "shot" ~ round(abs(sqrt((x_location - 89)^2 + (y_location)^2)), 1),
                                mean_shot < 0 & event == "shot" ~ round(abs(sqrt((x_location - (-89))^2 + (y_location)^2)), 1)),
                               .after = y_location)
                
                pbp_data <- pbp_data |>
                        select(-mean_shot)
                
                return(pbp_data)
                
        }
        
        add_shot_angle <- function(pbp_data) {
                
                find_sides <- pbp_data |>
                        filter(event == "shot") |>
                        group_by(game_id, 
                                 event_team) |>
                        summarize(mean_shot = mean(x_location,
                                                   na.rm = TRUE),
                                  .groups = "drop") 
                
                pbp_data <- pbp_data |>
                        left_join(find_sides, 
                                  by = join_by(game_id, 
                                               event_team))
                
                pbp_data <- pbp_data |>
                        mutate(angle = case_when(
                                mean_shot > 0 & event == "shot" ~ round(abs(atan((0-y_location) / (89-x_location)) * (180 / pi)), 1),
                                mean_shot < 0 & event == "shot" ~ round(abs(atan((0-y_location) / (-89-x_location)) * (180 / pi)), 1)),
                               .after = distance) |>
                        mutate(angle = ifelse((mean_shot > 0 & x_location > 89) | (mean_shot < 0 & x_location < -89), 180 - angle, angle))
                
                pbp_data <- pbp_data |>
                        select(-mean_shot)
                
                return(pbp_data)
                
        }
        
        add_rebound <- function(pbp_data) {
                
                pbp_data <- pbp_data |>
                        mutate(is_rebound = if_else((event == "shot" & lag(event) == "shot") & (game_seconds - lag(game_seconds)) < 3, TRUE, FALSE),
                               .after = is_goal)
                
                return(pbp_data)
                
        }
        
        # Prepare the data for xG model
        
        indexed_data <- raw_pbp_data |>
                mutate(index = row_number()) |>
                adjust_event_location() |>
                add_shot_distance() |>
                add_shot_angle() |>
                add_rebound()
        
        model_data <- indexed_data |>
                mutate(en = if_else(lead(goal_is_en) == TRUE, TRUE, FALSE)) |>
                mutate(en = if_else(is.na(en), FALSE, en)) |>
                mutate(is_rebound = if_else(is.na(is_rebound), FALSE, is_rebound)) |>
                filter(event == "shot" & en == FALSE)

        model_data$is_goal <- factor(model_data$is_goal, levels = c("TRUE", "FALSE"))
        
        model_data$is_rebound <- as.factor(model_data$is_rebound)
        
        # Get indexed xG
        
        xg_data <- augment(xg_model, model_data)
        
        xg_data <- xg_data |>
                rename(xg = .pred_TRUE) |>
                select(index,
                       xg)
        
        # Join xG to indexed data
        
        indexed_data <- indexed_data |>
                left_join(xg_data, by = "index")
        
        # Tidy up
        
        pbp_length <- length(indexed_data)
        cols <- c(1:16, pbp_length, 17:(pbp_length -2))
        
        indexed_data <- indexed_data |>
                select(c(cols))

        return(indexed_data)        
        
} 

# Pull sample data

get_pbp <- read_rds("pbp_function.rds")

raw_data <- get_pbp(88) 

augmented_data <- augment_shots(raw_data)

# Summarize sample data by team

augmented_data |>
        filter(xg > 0) |>
        group_by(event_team) |>
        summarize(xg = sum(xg),
                  goals = sum(is_goal == TRUE),
                  .groups = "drop")

# Summarize sample data by skater

augmented_data |>
        filter(xg > 0) |>
        group_by(event_player_id, event_player, event_team) |>
        summarize(xg = sum(xg),
                  goals = sum(is_goal == TRUE),
                  .groups = "drop") |>
        select(-event_player_id) |>
        arrange(desc(xg))

# Save augment function

write_rds(augment_shots, "augment_shots_function.rds")


