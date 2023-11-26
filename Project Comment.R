data1 = read.csv("D:/IIT Chicago/Fall 23/DPAA/Project/SP3.csv")
filter_and_format_dates <- function(data, date_col, cutoff_date) {


  data <- data[as.Date(data[[date_col]]) > as.Date(cutoff_date),]


  data[[date_col]] <- ifelse(nchar(as.character(data[[date_col]])) == 8,
                             as.character(data[[date_col]]),


                             paste0(substr(data[[date_col]], 1, 5), "20", substr(data[[date_col]], 6,7))

  )

  return(data)

}
filtered_data <- filter_and_format_dates(
  data = data1,
  date_col = "Date",
  cutoff_date = "01/01/2022"
)
split_dates <- strsplit(as.character(filtered_data$Date), "/")
years <- sapply(split_dates, function(x) x[3])
years <- paste0("20", years)
filtered_data$Date <- sapply(seq_along(split_dates), function(i) {
  paste(split_dates[[i]][1], split_dates[[i]][2], years[i], sep="/")
})
write.csv(filtered_data, file = "SPF3.csv", row.names = FALSE)
z=unique(a$Date)
y=unique(b$Date)



result <- identical(z,y)

if (identical(z,y)) {
  cat("TRUE\n")
} else {
  cat("FALSE\n")
  cat("Unique values in a1$Home that are not in b1$Home:\n")
  cat(setdiff(z,y), "\n")
  cat("Unique values in b1$Home that are not in a1$Home:\n")
  cat(setdiff(y,z), "\n")
}

