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
dat = read_csv('../data/nba2018.csv')

dat$experience[dat$experience == 'R'] = 0
dat$experience = as.integer(dat$experience)

dat$salary = dat$salary / 1000000

dat$position = revalue(dat$position, replace = c("C" = 'center', "PF" = 'power_fwd', "PG" = 'point_guard', "SF" = 'small_fwd', "SG" = 'shoot_guard'))

dat = mutate(dat, missed_fg = dat$field_goals_atts - dat$field_goals)
dat = mutate(dat, missed_ft = dat$points1_atts - dat$points1)
dat = mutate(dat, rebounds = dat$off_rebounds + dat$def_rebounds)
dat = mutate(dat, efficiency = (dat$points + dat$rebounds + dat$assists + dat$steals + dat$blocks - dat$missed_fg - dat$missed_ft - dat$turnovers) / dat$games)
dat = mutate(dat, my_efficiency = (dat$points + dat$rebounds + dat$assists + dat$steals + dat$blocks - dat$points1 - dat$turnovers) / dat$games)

sink(file = '../output/efficiency-summary.txt')
summary(dat)
sink()




teams = summarise(group_by(dat, team),
                  experience = sum(round(experience, 2)),
                  salary = sum(round(salary, 2)),
                  points3 = sum(points3),
                  points2 = sum(points2),
                  points1 = sum(points1),
                  points = sum(points),
                  off_rebounds = sum(off_rebounds),
                  def_rebounds = sum(def_rebounds),
                  assists = sum(assists),
                  steals = sum(steals),
                  blocks = sum(blocks),
                  turnovers = sum(turnovers),
                  fouls = sum(fouls),
                  efficiency = sum(efficiency),
                  my_efficiency = sum(my_efficiency))

sink(file = '../output/teams-summary.txt')
summary(teams)
sink()

write_csv(teams, '../data/nba2018-teams.csv')

getwd()                



