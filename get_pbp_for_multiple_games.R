# Get play-by-play data for multiple games

library(tidyverse)
library(jsonlite)
library(httr)

get_pbp <- read_rds("pbp_function.rds")
get_pbp_old <- read_rds("pbp_function_old.rds")

# There are some anomalies in the game_ids

# Loop through early season game_ids

game_ids_1 <- c(2:6,8:27)

loop_list <- list()

for (i in 1:length(game_ids_1)) {
        
        loop_data <- get_pbp_old(game_ids_1[i])
        
        loop_list[[i]] <- loop_data
        
        Sys.sleep(15)
        
}

season_one_pbp_1 <- bind_rows(loop_list)

# Fix the plus|minus columns

season_one_pbp_1 <- season_one_pbp_1 |>
        select(c(1:36, 43, 37:42))

# Loop through game_ids 28:73 plus game 83

game_ids_2 <- c(28:73, 83)

loop_list <- list()

for (i in 1:length(game_ids_2)) {
        
        loop_data <- get_pbp(game_ids_2[i])
        
        loop_list[[i]] <- loop_data
        
        Sys.sleep(15)
        
}

season_one_pbp_2 <- bind_rows(loop_list)

# Fix the plus|minus columns

season_one_pbp_2 <- season_one_pbp_2 |>
        select(c(1:42, 49, 43:48))

# Combine the data

season_one_pbp <- season_one_pbp_1 |>
        bind_rows(season_one_pbp_2)

# Save the season one data

write_rds(season_one_pbp, "season_one_pbp.rds")
