# # MVP calculator
# 2.5 points for each four
# 3.5 points for each six
# 3.5 points for each wicket taken
# 1 point for each dot ball bowled
# 2.5 points for each catch or stumping taken

# Shortlisting Matches
# i <- 0
# all_auct <- ipl_auction[,unique(PLAYER)]
# ipl_players <- ipl_data[,.(c(bowler_NA,batsman_NA,wicket_fielders))][,unique(V1)]
# rTable <- data.table()
# for (auct_player in all_auct) {
#     current <- unlist(strsplit(auct_player,split = " "))
#     current <- current[length(current)]
#     poss_matches <- grep(current, ipl_players, ignore.case = T, value = T)
#     if (length(poss_matches) == 0) {
#         poss_matches <- "No matches"
#     }
#     poss_table <- data.table(player = auct_player, matches = poss_matches)
#     rTable <- rbind(rTable, poss_table)
# }
# rTable <- rTable[grepl("^[^\\/]+$",rTable$matches)]
# write.csv(rTable, "possible matches.csv")

calculateMvpStats <- function(DT,info,player,period){
    require(data.table)
    
    if (missing(period)){
        time <- "AllTime"
        matches <- DT[, unique(match_no)]
    }
    else{
        matches <- grep(period, info$value[info$L2 == "dates"])
        time <- period
    }
    
    fours <- DT[batsman_NA == player & runs_batsman == 4 & is.na(runs_non_boundary) & match_no %in% matches, .N]
    sixes <- DT[batsman_NA == player & runs_batsman == 6 & match_no %in% matches, .N]
    
    bowler_dismissal <- setdiff(unique(DT$wicket_kind), c("obstructing the field","run out","retired hurt",NA))
    wickets <- DT[bowler_NA == player & wicket_kind %in% bowler_dismissal & match_no %in% matches, .N]
    dots <- DT[bowler_NA == player & runs_total == 0 & match_no %in% matches, .N]
    
    catches <- DT[grepl(paste0(player,"(?! \\(sub)"), wicket_fielders, perl = T) & grepl("caught", wicket_kind) & match_no %in% matches, .N]
    stumpings <- DT[grepl(paste0(player,"(?! \\(sub)"), wicket_fielders, perl = T) & grepl("stumped", wicket_kind) & match_no %in% matches, .N]
    ct_st <- catches + stumpings
    
    score <- (2.5 * fours) + (3.5 * sixes) + (3.5 * wickets) + (1 * dots) + (2.5 * ct_st)
    
    data.table(Player = player, Period = time, Fours = fours, Sixes = sixes, Wickets = wickets, Dots = dots, Catches = catches, Stumpings = stumpings, Points = score)
}