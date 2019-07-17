##########################################################################
#                                                                        #
# Simulation                                                             #
#                                                                        #
##########################################################################
# general hints cor coding:
# monitor time each chunk code took, 
# dont have to optimise chunks that used insignificant amount of time 
# use for loop if it doesnt take too long, 
# use interaction function
# debug and testing codes are contained within an empty  functions
#
# V1: rand.day.time
#     simdata (sim.number, time.start, time.end, cat1n, cat2n)
#     check simulation with cumulative plot and count plot
#
# V2: Fixed the bug that caused as.POSIXct to outout different time zone
#     simdata (cat2.val:Able to add custome matrix as terminal roots)
#
# V3: rand.day
#     simdata(added leaf variable, various debugging, cleanning)
#     contain test and debug codes in an empty function
#     cumdata
#     tabulatedata 
#
# V4: fig bug in tabulatedata 
#     Anomaly
#     
# V5: time.int
#     ggplot for cumulative counts
#     ggplot for daily counts
#
# V6: Bug in simdata
#     Export data
#
# V7: make ggdailyplot and ggcumplot into functions
#
# V8: Add anomalies
#
# V9: Almost done, organise each function in separate R files
#
# V10: 

# Premeables

library(dplyr)
library(tidyr)
library(ggplot2)
library(qpcR)
library(zoo)


##########################################################################