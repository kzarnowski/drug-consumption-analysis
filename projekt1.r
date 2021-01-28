# DATA PREPARATION


data <- read.delim("drug_consumption.data", header = FALSE, sep = ",", fill = TRUE)
  
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
data[ ,c(2:11)] <- lapply(data[ ,c(2:11)], as.character)

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


nscoreLevels <- c(12:60)
escoreLevels <- c(16,18:59)
oscoreLevels <- c(24,26,28:60)
ascoreLevels <- c(12,16,18,23:60)
cscoreLevels <- c(17,19:57,59)

#neoac <- c("n", "e", "o", "a", "c")

#nscores
scores <- data %>%
  distinct(nscore) %>%
  arrange(nscore) %>%
  pull(nscore)

names(nscoreLevels) <- scores
tmp <- nscoreLevels[data$nscore]
data$nscore <- as.factor(tmp)

#escores
scores <- data %>%
  distinct(escore) %>%
  arrange(escore) %>%
  pull(escore)

names(escoreLevels) <- scores
tmp <- escoreLevels[data$escore]
data$escore <- as.factor(tmp)

#oscores
scores <- data %>%
  distinct(oscore) %>%
  arrange(oscore) %>%
  pull(oscore)

names(oscoreLevels) <- scores
tmp <- oscoreLevels[data$oscore]
data$oscore <- as.factor(tmp)

#ascores
scores <- data %>%
  distinct(ascore) %>%
  arrange(ascore) %>%
  pull(ascore)

names(ascoreLevels) <- scores
tmp <- ascoreLevels[data$ascore]
data$ascore <- as.factor(tmp)

#cscores
scores <- data %>%
  distinct(cscore) %>%
  arrange(cscore) %>%
  pull(cscore)

names(cscoreLevels) <- scores
tmp <- cscoreLevels[data$cscore]
data$cscore <- as.factor(tmp)







#nscore
nscoreLevels <- c(12:60)
names(nscoreLevels) <- c(
  -3.46436, -3.15735, -2.75696, -2.52197, -2.42317, -2.34360, -2.21844, -2.05048, 
  -1.86962, -1.69163, -1.55078, -1.43907, -1.32828, -1.19430, -1.05308, -0.92104, 
  -0.79151, -0.67825, -0.58016, -0.46725, -0.34799, -0.24649, -0.14882, -0.05188, 
   0.04257,  0.13606,  0.22393,  0.31287,  0.41667,  0.52135,  0.62967,  0.73545,
   0.82562,  0.91093,  1.02119,  1.13281,  1.23461,  1.37297,  1.49158,  1.60383, 
   1.72012,  1.83990,  1.98437,  2.12700,  2.28554,  2.46262,  2.61139,  2.82196,
   3.27393
)
tmp <- nscoreLevels[data$nscore]
data$nscore <- as.factor(tmp)

#escore
escoreLevels <- c(16:59)
names(escoreLevels) <- c(
  -3.27393, -3.11111, -3.00537, -2.72827, -2.53830, -2.44904, -2.32338, -2.21069, 
  -2.11437, -2.03972, -1.92173, -1.76250, -1.63340, -1.50796, -1.37639, -1.23177,
  -1.09207, -0.94779, -0.80615, -0.69509, -0.57545, -0.43999, -0.30033, -0.15487,
   0.00332,  0.16767,  0.32197,  0.47617,  0.63779,  0.80523,  0.96248,  1.11406,
   1.28610,  1.45421,  1.58487,  1.74091,  1.93886,  2.12700,  2.32338,  2.57309,
   2.85950,  3.00537,  3.27393
)
tmp <- escoreLevels[data$escore]
data$escore <- as.factor(tmp)

#oscore
oscoreLevels <- c(24:60)
names(oscoreLevels) <- c(
  -3.27393, -3.11111, -2.8595, -2.63199, -2.39883, -2.21069, -2.09015, -1.97495, -1.82919, -1.68062, -1.55521, -1.42424, -1.27553, -1.11902, -0.97631, -0.84732, -0.71727, -0.58331, -0.45174, -0.31776, -0.17779, -0.01928, 0.14143, 0.29338, 0.44585, 0.58331, 0.7233, 0.88309, 1.06238, 1.24033, 1.43533, 1.65653, 1.88511, 2.15324, 2.44904, 2.90161
)




if (sum(is.na(data)) > 0) {
  drop_na(data)
} else {
  print("No missing values")
}






summary(data) # for each category





write.csv(data, "output.csv", row.names = FALSE)
saveRDS(data, "output.Rda")

# WORKING WITH TIDYVERSE
library(tidyverse)
str(data)

data %>% group_by(age)





