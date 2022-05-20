# https://themockup.blog/posts/2020-05-22-parsing-json-in-r-with-jsonlite/
# helpful source here

# this script requests the api and returns the hole by hole scores per player. 

pacman::p_load(httr, jsonlite, listviewer, googlesheets4)
res = GET("https://site.api.espn.com/apis/site/v2/sports/golf/pga/scoreboard")
data = fromJSON(rawToChar(res$content))

# Can we try to loop through the values of the scores for each player?
# Amazing way of viewing the JSON 
listviewer::jsonedit(data, height = "800px", mode = "view")

#TODO: probably need a better way to handle if the rows are missing
# flatten seems to work well

score_data = data$events$competitions[[1]]$competitors

sd = data.frame(score_data)
sd = flatten(sd, recursive = TRUE)
# now we just need to work with the line scores list in the sd dataframe

N = nrow(sd)

# parse info from JSON
#TODO: update to parse scores from each round
hole_scores = list()
hole_list_10 = c(seq(10, 18, 1), seq(1, 9, 1))
hole_list = seq(1, 18, 1)
for(i in 1:N) {
  
  start_hole = sd[i, ]$linescores[[1]]$startTee[[2]]
  player = sd[i, ]$athlete.displayName
  scores = sd[i, ]$linescores[[1]]$linescores[[1]]
  
  
  # Note: hole start seems to be reverse
  if(!is.null(scores)){
    if(start_hole == 1){
      holes_played = hole_list_10[1:nrow(scores)]
    } else {
      holes_played = hole_list[1:nrow(scores)]
    }
    
    hole_scores[[length(hole_scores) + 1]] = data.frame( player = player, 
                                                         holes = holes_played,
                                                         scores = scores)
  }
  
  
  
}
hole_scores = do.call(rbind.data.frame, hole_scores)
hole_scores = hole_scores[order(hole_scores$player, hole_scores$holes), ]
holes_wide = tidyr::pivot_wider(hole_scores, id_cols = player, names_from = holes, values_from = value, values_fill = NULL)


# # do horse race calculation here ---- 
# #TODO: would need to read in the picks by the guys, and only keep these players. 
# 
# players = unique(hole_scores$player)
# holes = unique(hole_scores$holes)
# race = hole_scores
# 
# race_results = list()
# 
# for(hole in sort(holes)){
#   
#   tmp = race[race$holes == hole, ]
#   max_score = max(tmp$value)
#   eliminations = race[race$holes == hole & race$value == max_score, ]$player
#   race = race[race$holes > hole & race$value < max_score, ]
# 
#   race_results[[length(race_results) + 1]] = data.frame(hole = hole, 
#                                                         max_score = max_score, 
#                                                         eliminations = paste(eliminations, collapse = ", "))
# 
#   
# }
# 
# race_results = do.call(rbind.data.frame, race_results)



# -----

# write to googlesheet
ss = "https://docs.google.com/spreadsheets/d/1RazFqjnhSs1QDmiBBShmkv2XNt2YlKHk6SOmLdg-xhY/edit#gid=621170165"

holes_wide %>%
  sheet_write(ss,
              sheet = "skins+hr")

# pacman::p_load(cronR)
# 
# wd = getwd()
# script = file.path(wd, "scratch.R")
# cmd = cron_rscript(script, 
#                    log_append = TRUE, 
#                    log_timestamp = TRUE)
# cron_add(command = cmd, 
#          frequency ='*/15 * * * *', 
#          id = "PGA-Championship", 
#          description = "Sending ESPN Scores",
#          tags = "golf")
