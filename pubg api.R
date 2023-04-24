library(httr)
library(jsonlite)

#restriction on url means only matches from previous 14 days available
#Gets a sample of match ids from the steam platform started from 2023-04-19 (24hrs before specified)
url <- "https://api.pubg.com/shards/steam/samples?filter[createdAt-start]=2023-04-20T00:00:00Z"

#api key
api_key <- Sys.getenv("PUBG_KEY")

#Make the request
res <- GET(url, add_headers(Authorization=api_key), accept_json(), config(verbose = TRUE))

#Get the match ids into a usable format
raw <- rawToChar(res$content)
data <- fromJSON(rawToChar(res$content))
match_ids <- data$data$relationships$matches$data

#Request 1 match id info
url_match <- paste0("https://api.pubg.com/shards/steam/matches/",match_ids$id[1])

#make the new request for match data
res_match <- GET(url_match, add_headers(Authorization=api_key), accept_json(), config(verbose = TRUE))

#Make it readable
raw_match <- rawToChar(res_match$content)
data_match <- fromJSON(rawToChar(res_match$content))

match_data <- data_match$data$attributes

#Get match data
data_match_df <- data.frame(matchid = match_ids$id[1], duration = match_data$duration)

#Get player data
player_data <- cbind(data_match$included$type, data_match$included$id, data_match$included$attributes$won, data_match$included$attributes$shardId, data_match$included$attributes$stats, data_match$included$attributes$actor, data_match$included$attributes$name, data_match$included$attributes$description, data_match$included$attributes$createdAt, data_match$included$attributes$URL, data_match$included$relationships)
colnames(player_data)
colnames(player_data) <- stringr::str_remove_all(colnames(player_data), "data_match\\$included")
colnames(player_data) <- stringr::str_remove_all(colnames(player_data), "\\$attributes")
colnames(player_data) <- stringr::str_remove_all(colnames(player_data), "\\$")
player_data <- dplyr::select(player_data, -name)
player_data <- dplyr::filter(player_data, !is.na(playerId))

#Join match and player data
match_player_data <- cbind(data_match_df, player_data)
