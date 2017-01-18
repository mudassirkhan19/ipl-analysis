ipl_list <- readCricsheetCsv("./csv_files/")
match_data <- ipl_list[[1]]
info <- ipl_list[[2]]
match_data <- match_data[L3 == "1st innings" | L3 == "2nd innings"]
rm(ipl_list)

changecols <- setdiff(names(match_data),c("L6","L3","batsman_NA","bowler_NA","non_striker_NA","wicket_fielders","wicket_kind","wicket_player_out"))
match_data[, (changecols) := lapply(.SD, as.integer), .SDcols = changecols]
str(match_data)

match_data[,unique(wicket_kind)]

# No of innings batted
total_batted <- nrow(match_data[batsman_NA == "AB de Villiers", .N , by = match_no])

# No of times dismissed
total_dismissed <- match_data[wicket_player_out == "AB de Villiers" & wicket_kind != "retired hurt", .N]

# Not outs
total_batted - total_dismissed

# Total runs
total_runs <- match_data[batsman_NA == "AB de Villiers", sum(runs_batsman)]

# Highest Score
max(match_data[batsman_NA == "AB de Villiers", sum(runs_batsman), by = match_no]$V1)

# Batting Avg
match_data[batsman_NA == "AB de Villiers" , sum(runs_batsman)/total_dismissed]

# Balls Faced
balls_faced <- match_data[batsman_NA == "AB de Villiers" & is.na(extras_wides) , .N]

# Strike Rate
format(round(total_runs/balls_faced * 100,2),nsmall = 2) 

# No of 100's
sum(match_data[batsman_NA == "AB de Villiers", sum(runs_batsman), by = match_no]$V1 >= 100)

# No of 50's
sum(match_data[batsman_NA == "AB de Villiers", sum(runs_batsman), by = match_no]$V1 %in% c(50:99))

# No of 4's
match_data[batsman_NA == "AB de Villiers" & runs_batsman == 4 & is.na(runs_non_boundary), .N]

# No of 6's
match_data[batsman_NA == "AB de Villiers" & runs_batsman == 6, .N]

# No of catches
match_data[grepl("AB de Villiers(?! \\(sub)", wicket_fielders, perl = T) & grepl("caught", wicket_kind), .N]

# No of Stumpings
match_data[grepl("AB de Villiers(?! \\(sub)", wicket_fielders, perl = T) & grepl("stumped", wicket_kind), .N]

# No of Balls
balls_bowled <- match_data[bowler_NA == "SR Watson" & is.na(extras_wides) & is.na(extras_noballs), .N]

# Runs Conceded
runs_conceded <- match_data[bowler_NA == "SR Watson", sum(runs_total)-sum(extras_byes,na.rm = T)-sum(extras_legbyes,na.rm = T)]

# Wickets
bowler_dismissal <- setdiff(unique(match_data$wicket_kind), c("obstructing the field","run out","retired hurt",NA))
total_wickets <- match_data[bowler_NA == "SR Watson" & wicket_kind %in% bowler_dismissal, .N]

# Best Bowling Figures
most_wickets <- match_data[bowler_NA == "SR Watson" & wicket_kind %in% bowler_dismissal, .(wickets = .N), by = match_no][wickets == max(wickets),.(match_no), key = wickets]
match_data[bowler_NA == "SR Watson", .(runs = sum(runs_total)-sum(extras_byes,na.rm = T)-sum(extras_legbyes,na.rm = T)), by = match_no][most_wickets, on = "match_no"][runs == min(runs),.(wickets),key=runs]

# Average
round(runs_conceded/total_wickets,2)

# Economy
round(runs_conceded/(balls_bowled/6), 2)

# Strike Rate
round(balls_bowled/total_wickets,2)

# 4 Wickets
match_data[bowler_NA == "SR Watson" & wicket_kind %in% bowler_dismissal, .(wickets = .N), by = match_no][wickets == 4,.N, key = wickets]$N

# 5 Wickets
match_data[bowler_NA == "SR Watson" & wicket_kind %in% bowler_dismissal, .(wickets = .N), by = match_no][wickets == 5,.N, key = wickets]$N

# Dot Balls
match_data[bowler_NA == "SR Watson" & runs_total == 0, .N]
