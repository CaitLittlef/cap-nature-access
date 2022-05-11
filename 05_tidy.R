##################################
## TIDY AND COMBINE ALL RESULTS ##
##################################

# List and load all output files with stats for CR/AKA/random and buffers.
files <- list.files(out.dir, pattern = "220511.csv", full.names = TRUE)
l <- list()
for (i in 1:length(files)){
  l[[i]] <- read.csv(files[i])
}

# Combine all tables and add row to specify real versus random sample
data <- bind_rows(l) ; data$X <- NULL ; data$unit_type <- NULL
data$source <- ifelse(data$loc == "AKA_randSample", "random sample",
                      ifelse(data$loc == "CR_randSample", "random sample", "proposed area"))
data <- data %>% dplyr::select(loc, source, buffer_mi, stat_grp, everything())
data$loc <- ifelse(data$loc == "AKA_randSample", "AKA",
                      ifelse(data$loc == "CR_randSample", "CR", data$loc))


# Load and sub-in tidier names for status grps
library(openxlsx)
lu <- read.xlsx(paste0(data.dir,"lu_status_grps.xlsx"))
data <- left_join(data, lu, by = c("stat_grp" = "code"))
data$stat_grp <- data$name ; data$name <- NULL

# Sort by status group
data <- data %>% arrange(stat_grp)

# Replace all NaN with NA
data[data == "NaN"] <- NA

# Nicer column names
colnames(data)
n <- c("Proposed national monument",
  "Source",
  "Buffer (miles)",
  "Status group",
  "Total number of tracts",
  "Proportion of tracts with hm > nat'l avg",
  "Proportion of tracts with hm-e > nat'l avg",
  "Number of people in tracts with hm > nat'l avg",
  "Number of people in tracts with hm-e > nat'l avg",
  "Miles to nearest PA for tracts with hm > nat'l avg",
  "Miles to nearest PA for tracts with hm-e > nat'l avg")
colnames(data) <- n


# Write to Excel w multiple sheets
# ?write.xlsx
cr_10 <- data %>% filter(`Proposed national monument` == "CR" & `Buffer (miles)` == 10)
cr_25 <- data %>% filter(`Proposed national monument` == "CR" & `Buffer (miles)` == 25)
cr_50 <- data %>% filter(`Proposed national monument` == "CR" & `Buffer (miles)` == 50)
aka_10 <- data %>% filter(`Proposed national monument` == "AKA" & `Buffer (miles)` == 10)
aka_25 <- data %>% filter(`Proposed national monument` == "AKA" & `Buffer (miles)` == 25)
aka_50 <- data %>% filter(`Proposed national monument` == "AKA" & `Buffer (miles)` == 50)

names <- list("CR_10mi" = cr_10,
              "CR_25mi" = cr_25,
              "CR_50mi" = cr_50,
              "AKA_10mi" = cr_10,
              "AKA_25mi" = cr_25,
              "AKA_50mi" = cr_50)

write.xlsx(names, paste0(out.dir,"CR-AKA_summary_stats_",today,".xlsx"))
