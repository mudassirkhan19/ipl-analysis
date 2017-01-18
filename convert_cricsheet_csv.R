aggr_fielder <- function(x) {
    paste0(x, collapse="/")
}

convertCricsheetData <- function(source = ".",destination = ""){
    require(yaml)
    require(reshape2)
    require(data.table)
    all.files <- list.files(path = source,
                            pattern = ".yaml$",
                            full.names = TRUE)
    
    for (i in 1:length(all.files)) {
        data = yaml.load_file(all.files[i])
        x = melt(data)
        y = data.table(x)
        fileName <- gsub("\\.yaml$","",all.files[i])
        matches <- regmatches(fileName, gregexpr("[[:digit:]]+$", fileName))
        fileName <- unlist(matches)
        while (nrow(y[(grepl("\\.9$|\\.10$",shift(L6))) & grepl("\\.1$",L6) & (substring(L6,1,1) == substring(shift(L6),1,1)), ]) > 0) {
            y[ grepl("\\.9$|\\.10$",shift(L6)) & grepl("\\.1$",L6) & (substring(L6,1,1) == substring(shift(L6),1,1)),
               L6 := paste0(L6,"0")]
        }
        
        meta = y[y$L1 == 'meta',]
        meta = meta[, colSums(is.na(meta)) != nrow(meta), with=FALSE]
        data_meta = reshape(meta,direction = 'wide',timevar = 'L2',idvar = 'L1')
        
        info = y[y$L1 == 'info',]
        info = info[, colSums(is.na(info)) != nrow(info), with=FALSE]
        info[, L1 := NULL]
        info[,match_no := i]
        
        data_innings = y[(y$L1 == 'innings') & (y$L4 == 'deliveries'),]
        data_innings[, new := paste(data_innings$L7,data_innings$L8,sep="_")]
        data_innings [, c("L7","L8","L4","L1","L5") := NULL]
        data_innings = dcast(data_innings, L2+L3+L6 ~ new, fun.aggregate = aggr_fielder,fill = NA)
        data_innings[,match_no := i]
        write.csv(data_innings,paste0(destination,paste(c(fileName,info[info$L2 == "dates",]$value,info[info$L2 == "teams",]$value), collapse = "-"),".csv"),row.names = F)
        write.csv(info,paste0(destination,paste(c("info",fileName,info[info$L2 == "dates",]$value,info[info$L2 == "teams",]$value), collapse = "-"),".csv"),row.names = F)
    }
    
}
