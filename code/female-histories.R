#######################################
#  ANALYSES OF REPRODUCTIVE HISTORIES #
#  OF ADULT FEMALES ALIVE FROM YEARS  #
#  1990-2017.                         #
#-------------------------------------#
#             Tim Frasier             #
#   Last Updated: April 12, 2023      #
#-------------------------------------#
# Data files  "calvingEvent.csv",     #
#             "whale.csv.", and       #
#             "whaleAge.csv"          #
#                                     #
# were obtained with permission from  #
# the North Atlantic Right Whale      #
# Consortium (https://www.narwc.org/) #
# specifically for these analyses.    #
# They are provided here strictly for #
# reproducibility purposes, but they  #
# should not be used for any other    #
# purposes (i.e., any other analyses).#
# If desired, permission to use them  #
# for other purposes should be        #
# requested from the NARWC.           #
#######################################

#--------------------#
# Load Libraries     #
#--------------------#
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(cowplot)


#--------------------#
# Load the data      #
#--------------------#
calvingEvent = read.table("../data/calvingEvent.csv", header = TRUE, sep = ",")
whale = read.table("../data/whale.csv", header = TRUE, sep = ",")
whaleAge = read.table("../data/whaleAge.csv", header = TRUE, sep = ",")


#--- Parse out females ---#
females = whale[whale$sex_either == "F", 2]

#--- Whales alive in 2017 ---#
alive = whaleAge[whaleAge$year == 2017 & (whaleAge$alive_status == "A" | whaleAge$alive_status == "R" | whaleAge$alive_status == "OP" | whaleAge$alive_status == "OSP"), ]

#--- Females alive in 2017 ---#
fAlive = alive[alive$nea %in% females, ]

#--- Adult females alive in 2017 ---#
afAlive = fAlive[fAlive$age_class == "A", ]
length(afAlive[, 1])


#------------------------------------------------------------------------------#



#----------------------------------#
# Calving History of Females Adult #
# And Alive in 2017                #
#----------------------------------#
nFemUsed = length(afAlive[, 1])
nCalvingEvents = length(calvingEvent[, 1])

nCalves = rep(NA, times = nFemUsed)

for (i in 1:nFemUsed) {
  
  counter = 0
  for (j in 1:nCalvingEvents) {
    if (calvingEvent[j, 2] == afAlive[i, 2]) {
      counter = counter + 1
    }
  }
  
  nCalves[i] = counter
}

femalesCalves = data.frame(afAlive$nea, nCalves)
colnames(femalesCalves) = c("female", "calves")

fem0 = sum(femalesCalves$calves == 0)
fem1 = sum(femalesCalves$calves == 1)
femMulti = sum(femalesCalves$calves > 1)


#--- Plot the data ---#
# Make Tidy
numbers = c(femMulti, fem1, fem0)
percent = c(round(100 * femMulti/(femMulti + fem1 + fem0), 0), round(100 * fem1/(femMulti + fem1 + fem0), 0), round(100 * fem0/(femMulti + fem1 + fem0), 0))
Category = c(">1 Calf", "One Calf", "No Calves")
Year = c(2017, 2017, 2017)
data = data.frame(numbers, Category, Year, percent)
colnames(data) = c("Females", "Category", "2017", "Percent")
ggplot(data, aes(x = 2017, y = Females, fill = Category)) +
  theme_bw() +
  geom_col() +
  geom_text(aes(label = paste(Females, " (", Percent, "%", ")", sep = "")), position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("2017") +
  ylab("Number of Females")


#----------------------------------------------------#
# FEMALES IN EACH REPRODUCTIVE CATEGORY ACROSS YEARS #
#----------------------------------------------------#
years = 1990:2017
nYears = length(years)

no.calves = rep(NA, times = nYears)
one.calf = rep(NA, times = nYears)
multi.calves = rep(NA, times = nYears)
total.adult.females = rep(NA, times = nYears)

# For each year...
for (y in 1:nYears) {
  
  #--- Whales alive in that year ---#
  alive = whaleAge[whaleAge$year == years[y] & (whaleAge$alive_status == "A" | whaleAge$alive_status == "R" | whaleAge$alive_status == "OP" | whaleAge$alive_status == "OSP"), ]

  #--- Females alive in that year ---#
  fAlive = alive[alive$nea %in% females, ]
  
  #--- Adult females alive in that year ---#
  afAlive = fAlive[fAlive$age_class == "A", ]
  length(afAlive[, 1])
  
  #--- Calving histories of these females ---#
  nFemUsed = length(afAlive[, 1])
  nCalvingEvents = length(calvingEvent[, 1])
  
  nCalves = rep(NA, times = nFemUsed)
  
  for (i in 1:nFemUsed) {
    
    counter = 0
    for (j in 1:nCalvingEvents) {
      if (calvingEvent[j, 2] == afAlive[i, 2]) {
        counter = counter + 1
      }
    }
    
    nCalves[i] = counter
  }
  
  femalesCalves = data.frame(afAlive$nea, nCalves)
  colnames(femalesCalves) = c("female", "calves")
  
  no.calves[y] = sum(femalesCalves$calves == 0)
  one.calf[y] = sum(femalesCalves$calves == 1)
  multi.calves[y] = sum(femalesCalves$calves > 1)
  total.adult.females[y] = no.calves[y] + one.calf[y] + multi.calves[y]
}


#--- Get Mean Values ---#
mean(no.calves)
mean(one.calf)
mean(multi.calves)


#--- Combine data and organize ---#
# Percentages
perc.no.calves = no.calves / total.adult.females
perc.one.calf = one.calf / total.adult.females
perc.multi.calves = multi.calves / total.adult.females

# Make data tidy
years.combined = rep(years, times = 3)
female.numbers = c(no.calves, one.calf, multi.calves)
percentages = c(perc.no.calves, perc.one.calf, perc.multi.calves)
female.histories = c(rep("No Calves", times = nYears), rep("One Calf", times = nYears), rep(">1 Calf", times = nYears))

calving.histories = data.frame(years.combined, female.numbers, percentages, female.histories)
colnames(calving.histories) = c("Year", "Number", "Percentage", "Category")

#--- Plot the data ---#
ggplot(calving.histories) + 
  theme_bw() +
  geom_point(aes(x = Year, y = Percentage, color = Category)) +
  geom_line(aes(x = Year, y = Percentage, color = Category)) +
  scale_color_brewer(palette = "Dark2") +
  ylab("Percentage of Females")


# Plot one
p1 = ggplot(data, aes(x = 2017, y = Females, fill = Category)) +
  theme_bw() +
  geom_col() +
  geom_text(aes(label = paste(Females, " (", Percent, "%", ")", sep = "")), position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("2017") +
  ylab("Number of Females")

# Plot two
p2 = ggplot(calving.histories) + 
  theme_bw() +
  geom_point(aes(x = Year, y = Percentage, color = Category), show.legend = FALSE) +
  geom_line(aes(x = Year, y = Percentage, color = Category), show.legend = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  ylab("Percentage of Females")

# Combine
combined = plot_grid(p2, p1, labels = "AUTO")
ggsave("../plots/females.png", combined, width = 183, height = 80, units = "mm", dpi = 300)
