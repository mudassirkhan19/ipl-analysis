readCricsheetCsv <- function(directory = "."){
    require(data.table)
    all.data.files <- list.files(path = directory, 
                                pattern = "^[^info].*csv$", full.names = TRUE)
    all.info.files <- list.files(path = directory, 
                                 pattern = "^[info].*csv$", full.names = TRUE)
    match_data <- data.table()
    info <- data.table()
    for (i in 1:length(all.data.files)) {
        new_data <- fread(all.data.files[i])
        match_data <- rbind(match_data, new_data,fill=TRUE)
    }
    for (i in 1:length(all.info.files)) {
        new_info <- fread(all.info.files[i])
        info <- rbind(info, new_info,fill=TRUE)
    }
    return(list(match_data,info))
}