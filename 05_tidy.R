##################################
## TIDY AND COMBINE ALL RESULTS ##
##################################

# Large random sample (n=500) does not compute dist; results on 220519
# ^ This also has newly added column of total number of tracts within buffer.
# ^ This also divides track tally by n for average across random samples.
# Small random sample (n=100) does compute dist; results on 220511


# List and load all output files with SMALL n & with dist stats for CR/AKA/random and buffers.
files <- list.files(out.dir, pattern = "220511.csv", full.names = TRUE)
l <- list() 
for (i in 1:length(files)){
  l[[i]] <- read.csv(files[i])
}
files

# Combine all tables and add row to specify real versus random sample
data <- bind_rows(l) ; data$X <- NULL ; data$unit_type <- NULL
data$source <- ifelse(data$loc == "AKA_randSample", "random sample",
                      ifelse(data$loc == "CR_randSample", "random sample", "proposed area"))
data <- data %>% dplyr::select(loc, source, buffer_mi, stat_grp, everything())
data$loc <- ifelse(data$loc == "AKA_randSample", "AKA",
                   ifelse(data$loc == "CR_randSample", "CR", data$loc))
# Hold as data w dist
data_w_dist <- data

# List and load all output files with LARGE n & withOUT dist stats for CR/AKA/random and buffers.
files <- list.files(out.dir, pattern = "220519.csv", full.names = TRUE)
l <- list()
for (i in 1:length(files)){
  l[[i]] <- read.csv(files[i])
}
files

## CONFIRM THEY DATASETS ARE ALL IN THE CORRECT ORDER BASED ON FILE NAMSE!! ##

# Combine all tables and add row to specify real versus random sample
data <- bind_rows(l) ; data$X <- NULL ; data$unit_type <- NULL
data$source <- ifelse(data$loc == "AKA_randSample", "random sample",
                      ifelse(data$loc == "CR_randSample", "random sample", "proposed area"))
data <- data %>% dplyr::select(loc, source, buffer_mi, stat_grp, everything())
data$loc <- ifelse(data$loc == "AKA_randSample", "AKA",
                   ifelse(data$loc == "CR_randSample", "CR", data$loc))


# Tack dist from smaller n onto larger n
data$pa_dist_mi_gt_hm <- data_w_dist$pa_dist_mi_gt_hm
data$pa_dist_mi_gt_hme <- data_w_dist$pa_dist_mi_gt_hme

# Should have 100 for each buffer
data %>% group_by(buffer_mi) %>% tally()
data %>% group_by(loc) %>% tally()
data %>% group_by(source) %>% tally()


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
  "Total number of status group tracts",
  "Proportion of tracts with hm > nat'l avg",
  "Proportion of tracts with hm-e > nat'l avg",
  "Number of people in tracts with hm > nat'l avg",
  "Number of people in tracts with hm-e > nat'l avg",
  "Miles to nearest PA for tracts with hm > nat'l avg",
  "Miles to nearest PA for tracts with hm-e > nat'l avg")
colnames(data) <- n

# Spread wider so proposed values are next to random sample values
data <- data %>% pivot_wider(names_from = c(Source),
                            values_from = c(`Total number of tracts`,
                                            `Total number of status group tracts`,
                                            `Proportion of tracts with hm > nat'l avg`,
                                            `Proportion of tracts with hm-e > nat'l avg`,
                                            `Number of people in tracts with hm > nat'l avg`,
                                            `Number of people in tracts with hm-e > nat'l avg`,
                                            `Miles to nearest PA for tracts with hm > nat'l avg`,
                                            `Miles to nearest PA for tracts with hm-e > nat'l avg`),
                            names_sep = " - ")


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
              "AKA_10mi" = aka_10,
              "AKA_25mi" = aka_25,
              "AKA_50mi" = aka_50)

write.xlsx(names, paste0(out.dir,"CR-AKA_summary_stats_",today,".xlsx"))



