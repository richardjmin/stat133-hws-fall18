#=======================================================================
# Title: Data Preparation 
# Description: 
#   I will be making a nba2018-teams.csv that will contain the required
#   required variables to be used in the ranking analysis. It will contain
#   new variables such as missed_fg, missed_ft, rebounds, and efficiency.
#   There will be preprocessing on some of the current data.
# Input(s): data file 'nba2018.csv'
# Output(s): data file 'nba2018-teams.csv
#=======================================================================

library(dplyr)
library(readr)
dat = read_csv('Documents/GitHub/stat133-hws-fall18/workout1/data/nba2018.csv')

dat$experience[dat$experience == 'R'] = 0
dat$experience = as.integer(dat$experience)

dat$salary = dat$salary / 1000000

dat$position = revalue(dat$position, replace = c("C" = 'center', "PF" = 'power_fwd', "PG" = 'point_guard', "SF" = 'small_fwd', "SG" = 'shoot_guard'))

dat = mutate(dat, missed_fg = dat$field_goals_atts - dat$field_goals)
dat = mutate(dat, missed_ft = dat$points1_atts - dat$points1)
dat = mutate(dat, rebounds = dat$off_rebounds + dat$def_rebounds)
dat = mutate(dat, efficiency = (dat$points + dat$rebounds + dat$assists + dat$steals + dat$blocks - dat$missed_fg - dat$missed_ft - dat$turnovers) / dat$games)

sink(file = 'Documents/GitHub/stat133-hws-fall18/workout1/output/efficiency-summary.txt')
summary(dat)
sink()


library(dplyr)
x = group_by(dat, team)
y = summarise(x, experience = sum(experience))




teams = summarise(group_by(dat, team), experience = sum(experience))



                   



