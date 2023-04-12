#######################################
# ANALYSES FOR CALCULATING THE        #
# "EXPECTED" NUMBER OF CALVES         #
# BORN IN YEARS 1990-2017.            #
#-------------------------------------#
#           Tim Frasier               #
#     Last updated: April 12, 2023    #
#-------------------------------------#
# Data file "calvingEvent.csv" was    #
# obtained with permission from the   #
# North Atlantic Right Whale          #
# Consortium (https://www.narwc.org/) #
# specifically for these analyses.    #
# Similarly, data file "PaceAFW.csv"  #
# was obtained from Dr. Richard Pace  #
# specifically for these analyses.    #
# They are provided here strictly for #
# reproducibility purposes, but they  #
# should not be used for any other    #
# purposes (i.e., any other analyses).#
#######################################

#----------------------#
#  Load Libraries      #
#----------------------#
library(ggplot2)
library(cowplot)

#--------------------#
# Load the data      #
#--------------------#
females = read.table("../data/PaceAFW.csv", header = TRUE, sep = ",")
calvingEvent = read.table("../data/calvingEvent.csv", header = TRUE, sep = ",")


#-----------------------#
# Parse out Data I Want #
#-----------------------#

#--- Available Females ---#
available = females[129:156, ]

#--- # of calves by year ---#
years = 1990:2017
nYears = length(years)
nCalves = rep(NA, times = nYears)
for (i in 1:nYears) {
  counter = 0
  for (j in 1:length(calvingEvent[, 1])) {
    if (calvingEvent[j, 3] == years[i]) {
      counter = counter + 1
    }
  }
  nCalves[i] = counter
}


#----------------------------------#
#  Plot Results                    #
#----------------------------------#

#--- Make Data Tidy ---#
available$nCalves = nCalves
expected = ggplot(available) +
  theme_bw() +
  geom_point(aes(x = Year, y = Median), size = 1) +
  geom_line(aes(x = Year, y = Median)) +
  geom_segment(aes(x = Year, y = Lower95, xend = Year, yend = Upper95)) +
  geom_point(aes(x = Year, y = nCalves), size = 1, color = "red") +
  geom_line(aes(x = Year, y = nCalves), color = "red") +
  ylab("# of Calves") +
  xlab("Year")



#---------------------------------------------#
# Plot Difference Between Observed & Expected #
#---------------------------------------------#
percExp = nCalves / available$Median
meanPerc = mean(percExp)
meanPerc

#--- Make Tidy ---#
percentages = data.frame(percExp, available$Year)
colnames(percentages) = c("percExp", "Year")

percentPlot = ggplot(percentages) +
  theme_bw() +
  geom_point(aes(x = Year, y = percExp), size = 1) +
  geom_line(aes(x = Year, y = percExp)) +
  geom_smooth(aes(x = Year, y = percExp), method = "lm") +
  geom_hline(yintercept = meanPerc, color = "red", linetype = "dashed", linewidth = 1) +
  ylab("Percent Expected")

# Combine
combined = plot_grid(expected, percentPlot, labels = "AUTO")
ggsave("../plots/expected.png", combined, width = 183, height = 80, units = "mm", dpi = 300)
  
#--- Regression Values ---#
model1 = lm(percentages$percExp ~ percentages$Year)
summary(model1)
