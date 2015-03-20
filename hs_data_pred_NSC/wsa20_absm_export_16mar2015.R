##########################################################################
###
### Read Gwinnett Blockgroup ABSM Data 
### Export ABSMs for Join w/ WSA data
### Created on 16mar2015 by Roland Richard
### RUN SUCCESSFULLY IN R VERSION 3.1.3 ON 
###
###########################################################################

require(GISTools)
require(foreign)

gc.absm <- readShapePoly("H:/spatial/data/polygons/gwinnett/gwinnett_bg_absm_walk_07mar2015.shp")

head(data.frame(gc.absm))

gc <- data.frame(gc.absm)

gc.dep <- gc[,c(14, 36:37)]

write.csv(gc.dep, "..\\student.success.factor\\data\\prep\\gwinnett_census_deprivation_mar2015.csv")
write.dta(gc.dep, "..\\student.success.factor\\data\\prep\\wsa_census_dep.dta")
