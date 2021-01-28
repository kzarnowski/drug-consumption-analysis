# DATA PREPARATION

data <- read.delim("drug_consumption.data", header = FALSE, sep = ",", fill = TRUE)
data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
#str(data)
colnames(data) <- c(
  "id", #1
  "age", #2
  "gender", #3
  "education", #4
  "country", #5
  "ethnicity", #6
  "nscore", #7
  "escore", #8
  "oscore", #9
  "ascore", #10
  "cscore", #11
  "impulsive", #12
  "sensationSeeing", #13
  "alcohol", #14
  "amphetamine", #15
  "amyl", #16
  "benzos", #17
  "caffeine", #18
  "cannabis", #19
  "chocolate", #20
  "cocaine", #21
  "crack", #22
  "ecstasy", #23
  "heroin", #24
  "ketamine", #25
  "legalHighs", #26
  "lsd", #27
  "meth", #28
  "mushrooms", #29
  "nicotine", #30
  "semer", #31
  "vsa" #32
) 

# quantitive data mapping
#drugs
usage <- c(
  "Never Used",
  "Used over a Decade Ago", 
  "Used in Last Decade",
  "Used in Last Year",
  "Used in Last Month",
  "Used in Last Week",
  "Used in Last Day"
)
names(usage) <- c("CL0", "CL1", "CL2", "CL3", "CL4", "CL5", "CL6")
names
for (i in 14:32) {
  tmp <- usage[data[ ,i]]
  data[ ,i] <- as.factor(tmp)
  levels(data[ ,i]) <- usage
}

# age
ageLevels <- c("18-24","25-34","35-44","45-54","55-64","65+")
names(ageLevels) <- c(
  -0.95197, # 18-24
  -0.07854, # 25-34
  0.49788,  # 35-44
  1.09449,  # 45-45
  1.82213,  # 55-64
  2.59171   # 65+
)  
tmp <- ageLevels[data$age]
data$age <- as.factor(tmp)
levels(data$age) <- ageLevels

# gender
genderLevels <- c("M", "F")
names(genderLevels) <- c(-0.48246, 0.48246)
tmp <- genderLevels[data$gender]
data$gender <- as.factor(tmp)

#education
educationLevels <- c(
  "Left school before 16 years",
  "Left school at 16 years",
  "Left school at 17 years",
  "Left school at 18 years",
  "Some college or university, no certificate or degree",
  "Professional certificate/ diploma",
  "University degree",
  "Masters degree","Doctorate degree"
)
names(educationLevels) <- c(
  -2.43591, # Left school before 16 years
  -1.73790, # Left school at 16 years
  -1.43719, # Left school at 17 years
  -1.22751, # Left school at 18 years
  -0.61113, # Some college or university, no certificate or degree
  -0.05921, # Professional certificate/ diploma
  0.45468,  # University degree
  1.16365,  # Masters degree  
  1.98437   # Doctorate degree
)
tmp <- educationLevels[data$education]
data$education <- as.factor(tmp)
levels(data$education) <- educationLevels

#country
countryLevels <- c("Australia", "Canada", "New Zealand", "Other",
                   "Ireland", "UK", "USA")
names(countryLevels) <- c(
  -0.09765, # Australia
  0.24923,  # Canada
  -0.46841, # New Zealand
  -0.28519, # Other
  0.21128,  # Ireland
  0.96082,  # UK
  -0.57009  # USA
)
tmp <- countryLevels[data$country]
data$country <- as.factor(tmp)

#ethnicity
ethnicityLevels <- c("Asian", "Black", "Mixed-Black/Asian", "Mixed-White/Asian",
                     "Mixed-White/Black", "Other", "White")
names(ethnicityLevels) <- c(
  -0.50212, # Asian
  -1.10702, # Black
  1.90725,  # Mixed-Black/Asian
  0.12600,  # Mixed-White/Asian
  -0.22166, # Mixed-White/Black
  0.11440,  # Other
  -0.31685  # White
)
tmp <- ethnicityLevels[data$ethnicity]
data$ethnicity <- as.factor(tmp)

plot(data$id, data$nscore)

if (sum(is.na(data)) > 0) {
  data <- na.omit(data)
} else {
  print("No missing values")
}

summary(data) # for each category

write.csv(data, "output.csv", row.names = FALSE)
saveRDS(data, "output.Rda")

# WORKING WITH TIDYVERSE

library(tidyverse)


