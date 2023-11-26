
a=read.csv("D:/IIT Chicago/Fall 23/DPAA/Project/Olympic_Athlete_Bio.csv")
summary(a)
a=a[1:8]
a$height=as.numeric(a$height)
a$weight=as.numeric(a$weight)
replace_na_with_random <- function(x) {
  is_na <- is.na(x)
  min_val <- min(x, na.rm = TRUE)
  mean_val <- mean(x, na.rm = TRUE)
  random_vals <- runif(sum(is_na), min_val, mean_val)
  x[is_na] <- random_vals
  return(x)
}
a$height=replace_na_with_random(a$height)
a$weight=replace_na_with_random(a$weight)
a=a[a$born != "na", ]
summary(a)

b=read.csv("D:/IIT Chicago/Fall 23/DPAA/Project/Olympic_Athlete_Event_Results.csv")
summary(b)
b$medal=ifelse(b$medal == "na", 0, b$medal)
summary(b)

c=read.csv("D:/IIT Chicago/Fall 23/DPAA/Project/Olympic_Results.csv")
summary(c)
columns_to_remove=c("sport_url","result_detail","result_description","result_format","time")
c=c[, !names(c) %in% columns_to_remove]
calculate_date_difference1 <- function(data, start_col, end_col, new_col) {
  data$start_date <- as.Date(data[[start_col]])
  data$end_date <- as.Date(data[[end_col]])
  data <- data[!(is.na(data$start_date) & is.na(data$end_date)), ]
  data[[new_col]] <- as.integer(data$end_date - data$start_date)
  return(data)
}
c=calculate_date_difference1(c,"start_date", "end_date","Date_Difference")
c$Date_Difference=replace_na_with_random(c$Date_Difference)
c$Date_Difference=as.integer(c$Date_Difference)
c$end_date=c$start_date + c$Date_Difference
summary(c)

d=read.csv("D:/IIT Chicago/Fall 23/DPAA/Project/Olympics_Games.csv")
summary(d)
columns_to_remove1=c("edition_url","country_flag_url","isHeld")
d=d[, !names(d) %in% columns_to_remove1]
z=c(6,12,13,33,34,35,40,41)
d=d[-z, ]
row.names(d)=NULL
d$start_date[2]="14 May"
d$start_date[29]="23 July"
d$end_date[2:4]=c("28 October","26 November","31 October")
d$end_date[29]= "8 August"
d$year[29]=c(2021)
d$competition_date[29]="21 July – 8 August"

e=read.csv("D:/IIT Chicago/Fall 23/DPAA/Project/Olympic_Games_Medal_Tally.csv")
summary(e)
View(e)
library(dplyr)
ab=inner_join(a,b,by="athlete_id")
remove1=c("athlete")
ab=ab[, !names(ab) %in% remove1]
abc=inner_join(ab,c,by="result_id")
remove2=c("country_noc.y","edition.y","edition_id.y","sport.y")
abc=abc[, !names(abc) %in% remove2]
colnames(abc)[colnames(abc) == "country_noc.x"] = "country_noc"
colnames(abc)[colnames(abc) == "edition.x"] = "edition"
colnames(abc)[colnames(abc) == "edition_id.x"] = "edition_id"
colnames(abc)[colnames(abc) == "sport.x"] = "sport"
abcd=inner_join(abc,d,by="edition_id")
remove2=c("country_noc.y","start_date.y","end_date.y","edition.y")
abcd=abcd[, !names(abcd) %in% remove2]
colnames(abcd)[colnames(abcd) == "country_noc.x"] = "country_noc"
colnames(abcd)[colnames(abcd) == "edition.x"] = "edition"
colnames(abc)[colnames(abc) == "start_date.x"] = "start_date"
colnames(abc)[colnames(abc) == "end_date.x"] = "end_date"
View(abcd)



abcd4 <- abcd %>%
  group_by(athlete_id) %>%
  filter(n() >= 10)
View(abcd4)

# Load the required packages
library(TraMineR)
library(dplyr)

# Convert to sequence data
seq_df <- abcd4 %>%
  group_by(athlete_id) %>%
  summarise(sequence = paste(medal, collapse = "-"))

# Mine frequent sequences
seq_matrix <- seqdef(seq_df$sequence)
# Mine frequent sequences
# Mine frequent sequences
freq_patterns <- seqiplot(seq_matrix, supp = 0.05)

# Print the first few rows of the frequent sequences
print(head(freq_patterns))



# Cluster athletes
# Define a substitution cost matrix with zeros on the diagonal
# Define a substitution cost matrix with zeros on the diagonal
substitution_matrix <- matrix(0, nrow = 4, ncol = 4)
rownames(substitution_matrix) <- colnames(substitution_matrix) <- c("0", "Bronze", "Gold", "Silver")

# Define a positive indel cost
indel_cost <- 1  # You can adjust this value based on your requirements

# Calculate sequence distances using the OM method with the substitution cost matrix and indel cost
seq_dist <- seqdist(seq_matrix, method = "OM", sm = substitution_matrix, indel = indel_cost)

# Print the sequence distances
print(seq_dist)

# Print the sequence distances
print(seq_dist)

# Print the sequence distances
print(seq_dist)



clusters <- seqclust(seq_dist, method = "ward")

# Analyze clusters
plot(clusters)

