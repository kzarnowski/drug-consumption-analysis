# DATA PREPARATION

drugs <- read.delim("drug_consumption.data", header = FALSE, sep = ",", fill = TRUE)
  
#str(data)
colnames(drugs) <- c(
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
drugs[ ,c(2:11)] <- lapply(drugs[ ,c(2:11)], as.character)

#usage
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
usage
for (i in 14:32) {
  tmp <- usage[drugs[ ,i]]
  drugs[ ,i] <- as.factor(tmp)
  levels(drugs[ ,i]) <- usage
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
tmp <- ageLevels[drugs$age]
drugs$age <- as.factor(tmp)
levels(drugs$age) <- ageLevels

# gender
genderLevels <- c("M", "F")
names(genderLevels) <- c(-0.48246, 0.48246)
tmp <- genderLevels[drugs$gender]
drugs$gender <- as.factor(tmp)

#education
educationLevels <- c(
  "Left school before 16 years",
  "Left school at 16 years",
  "Left school at 17 years",
  "Left school at 18 years",
  "Some college, no degree",
  "Professional certificate",
  "University degree",
  "Masters degree",
  "Doctorate degree"
)
names(educationLevels) <- c(
  -2.43591, # Left school before 16 years
  -1.73790, # Left school at 16 years
  -1.43719, # Left school at 17 years
  -1.22751, # Left school at 18 years
  -0.61113, # Some college, no degree
  -0.05921, # Professional certificate
  0.45468,  # University degree
  1.16365,  # Masters degree  
  1.98437   # Doctorate degree
)
tmp <- educationLevels[drugs$education]
drugs$education <- as.factor(tmp)
levels(drugs$education) <- educationLevels

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
tmp <- countryLevels[drugs$country]
drugs$country <- as.factor(tmp)

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
tmp <- ethnicityLevels[drugs$ethnicity]
drugs$ethnicity <- as.factor(tmp)


# WORKING WITH TIDYVERSE
library(tidyverse)

nscoreLevels <- c(12:60)
escoreLevels <- c(16,18:59)
oscoreLevels <- c(24,26,28:60)
ascoreLevels <- c(12,16,18,23:60)
cscoreLevels <- c(17,19:57,59)

#nscores
scores <- drugs %>%
  distinct(nscore) %>%
  arrange(nscore) %>%
  pull(nscore)

names(nscoreLevels) <- scores
tmp <- nscoreLevels[drugs$nscore]
drugs$nscore <- as.factor(tmp)

#escores
scores <- drugs %>%
  distinct(escore) %>%
  arrange(escore) %>%
  pull(escore)

names(escoreLevels) <- scores
tmp <- escoreLevels[drugs$escore]
drugs$escore <- as.factor(tmp)

#oscores
scores <- drugs %>%
  distinct(oscore) %>%
  arrange(oscore) %>%
  pull(oscore)

names(oscoreLevels) <- scores
tmp <- oscoreLevels[drugs$oscore]
drugs$oscore <- as.factor(tmp)

#ascores
scores <- drugs %>%
  distinct(ascore) %>%
  arrange(ascore) %>%
  pull(ascore)

names(ascoreLevels) <- scores
tmp <- ascoreLevels[drugs$ascore]
drugs$ascore <- as.factor(tmp)

#cscores
scores <- drugs %>%
  distinct(cscore) %>%
  arrange(cscore) %>%
  pull(cscore)

names(cscoreLevels) <- scores
tmp <- cscoreLevels[drugs$cscore]
drugs$cscore <- as.factor(tmp)

drugs[ ,c(7:11)] <- lapply(drugs[ ,c(7:11)], as.numeric)

if (sum(is.na(drugs)) > 0) {
  drop_na(drugs)
} else {
  print("No missing values")
}

summary(drugs) # for each category

write.csv(drugs, "output.csv", row.names = FALSE)
saveRDS(drugs, "output.Rda")

# DATA VISUALIZATION


# VISUALIZATION 4

library(lattice)
library(latticeExtra)
library(tidyverse)
# uzycie nielegalnych narkotykow w przedziale wiekowym 18-24
# active-users : <= last_month
# past_users: >last_month & != never
# non_users: never

sample_drugs <- c(21,22,24,28,29)
youth <- drugs %>%
  filter(age == "18-24" | age == "25-34") %>%
  select(all_of(sample_drugs))

df <- count(youth, cocaine)
df <- subset(df, select = n)
rownames(df) <- usage
df <- rename(df, cocaine = n )
df <- mutate(df, crack = count(youth, crack)$n)
df <- mutate(df, heroin = count(youth, heroin)$n)
df <- mutate(df, meth = count(youth, meth)$n)
df <- mutate(df, mushrooms = count(youth, mushrooms)$n)

users <- df %>%
    slice(4:7) %>%
    summarise_all(sum)
past_users <- df %>%
  slice(2:3) %>%
  summarise_all(sum)
non_users <- df[1, ]

df <- bind_rows(users, past_users, df[1, ])
df <- as.data.frame(t(df))

names <- c("cocaine", "crack", "heroin", "meth", "mushrooms")

df <- add_column(df, .before = 1, drugs = c("cocaine", "crack", "heroin", "meth", "mushrooms"))
rownames(df) <- c(1:5)
colnames(df) <- c("Substance", "ActiveUsers", "PastUsers", "NonUsers")

barchart(ActiveUsers ~ Substance, data = df,
         main = "Active 18-34 years old users of some drugs",
         xlab = "Substance",
         ylab = "Users",
         col = "chocolate",
         panel = function(...) {
           panel.fill(col = "gray4")
           panel.barchart(...)
         }
         )


# VISUALIZATION 5
library(plotly)



























# VISUALIZATION 1
library(ggplot2)

plot1 <- ggplot(data = drugs, aes(x = cannabis, fill = age)) +
  geom_bar() +
  theme_bw() + 
  facet_wrap(~ gender) +
  ggtitle("cannabis") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        axis.text.y = ) +
  coord_flip()

plot2 <- ggplot(data = drugs, aes(x = amphetamine, fill = age)) +
  geom_bar() +
  theme_bw() + 
  facet_wrap(~ gender) +
  ggtitle("amphetamine") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  coord_flip()

plot3 <- ggplot(data = drugs, aes(x = nicotine, fill = age)) +
  geom_bar() +
  theme_bw() + 
  facet_wrap(~ gender) +
  ggtitle("nicotine") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none") +
  coord_flip()

plot4 <- ggplot(data = drugs, aes(x = ecstasy, fill = age)) +
  geom_bar() +
  theme_bw() + 
  facet_wrap(~ gender) +
  ggtitle("ecstasy") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y=element_blank()) +
  coord_flip()

library(gridExtra)
grid.arrange(plot1,plot2,plot3,plot4, ncol = 2, nrow = 2,
             top = "Some drugs usage broken down by gender and age",
             left = "Usage",
             bottom = "Number of respondens")

# VISUALIZATION 2
library(graphics)

neoac <- function(substance, active) {
  if(active) {
    df = users(substance)
  } else {
    df = nonusers(substance)
  }
  
  var <- df %>%
    summarise(
      n = mean(nscore),
      e = mean(escore),
      o = mean(oscore),
      a = mean(ascore),
      c = mean(cscore))
  
  var <- as.numeric(var[1, ])
  names(var) <- c("n", "e", "o", "a", "c")
  return(var)
}

users <- function(substance) {
  drugs %>% 
    filter(
      substance == "Used in Last Day" 
      | substance == "Used in Last Week"
      | substance == "Used in Last Month"
      | substance == "Used in Last Year"
    )
}

nonusers <- function(substance) {
  drugs %>% 
    filter(
      substance == "Never Used" 
      | substance == "Used over a Decade Ago"
      | substance == "Used in Last Decade"
    )
}

#
par(mfrow=c(2,2))

# cocaine
cocaine_yes = neoac(drugs$cocaine, T)
cocaine_no = neoac(drugs$cocaine, F)

scores_range <- range(12, cocaine_yes, cocaine_no, 25)
plot(cocaine_yes, type="b", col="red", pch = 19, ylim=scores_range, axes=FALSE, ann=FALSE)
axis(1, at=1:5, lab=c("n", "e", "o", "a", "c"))
axis(2, las=1, at=12:scores_range[2])
lines(cocaine_no, type="b", pch=15, lty=2, col="blue")
title(main = "Cocaine",
      xlab = "NEO PI-R personality dimensions",
      ylab = "Average score")
box()

# ecstasy
ecstasy_yes = neoac(drugs$ecstasy, T)
ecstasy_no = neoac(drugs$ecstasy, F)

scores_range <- range(12, ecstasy_yes, ecstasy_no, 25)
plot(ecstasy_yes, type="b", col="red", pch = 19, ylim=scores_range, axes=FALSE, ann=FALSE)
axis(1, at=1:5, lab=c("n", "e", "o", "a", "c"))
axis(2, las=1, at=12:scores_range[2])
lines(ecstasy_no, type="b", pch=15, lty=2, col="blue")
title(main = "Ecstasy",
      xlab = "NEO PI-R personality dimensions",
      ylab = "Average score")
box()

# meth
meth_yes = neoac(drugs$meth, T)
meth_no = neoac(drugs$meth, F)

scores_range <- range(12, meth_yes, meth_no, 25)
plot(meth_yes, type="b", col="red", pch = 19, ylim=scores_range, axes=FALSE, ann=FALSE)
axis(1, at=1:5, lab=c("n", "e", "o", "a", "c"))
axis(2, las=1, at=12:scores_range[2])
lines(meth_no, type="b", pch=15, lty=2, col="blue")
title(main = "Meth",
      xlab = "NEO PI-R personality dimensions",
      ylab = "Average score")
box()

# crack
crack_yes = neoac(drugs$meth, T)
crack_no = neoac(drugs$meth, F)

scores_range <- range(12, crack_yes, crack_no, 25)
plot(crack_yes, type="b", col="red", pch = 19, ylim=scores_range, axes=FALSE, ann=FALSE)
axis(1, at=1:5, lab=c("n", "e", "o", "a", "c"))
axis(2, las=1, at=12:scores_range[2])
lines(crack_no, type="b", pch=15, lty=2, col="blue")
title(main = "Crack",
      xlab = "NEO PI-R personality dimensions",
      ylab = "Average score")
box()

mtext("Average personality profiles for users and non-users of some drugs",
      side = 3, line = -1.2, outer = TRUE)

par(mfrow=c(1,1))


# VISUALIZATION 3
illegal <- c(15,19,21:25,27:29,31)
illegal_count <- rowSums(drugs[ ,illegal] != "Never Used", na.rm = TRUE)

drugs <- mutate(drugs, illegal_active = illegal_count)
ggplot(data = drugs, aes(x = illegal_active, y = education)) + 
  geom_count(aes(size = ..n..), color='royalblue3', shape = 15) +
  ggtitle("Correlation between illegal drugs usage and education") +
  labs(x = "Number of illegal drugs ever used", y = "Education level") +
  scale_size("Group", range = c(1,8)) 




