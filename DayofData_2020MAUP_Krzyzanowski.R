#######################################################

#DAY OF DATA 2020 MAUP ACTIVITY
#KRZYZANOWSKI B.

########################################################
########################################################

#Load the Libraries
require(spdep)
require(spatstat)
require(GWmodel)


#Let's look at the pretty strong and stable relationship between income and air quality
#Set the Directory
setwd("C:\\Users\\krzyzanowski\\Desktop\\Roger\\DayofData2020\\final")
#Name your data so you can call on it.  We will call it "spdat"
#If you want to run the script on another data set, or different set of units, all you need to change is the last bit of the line below.
spdat<-readShapePoly("C:\\Users\\krzyzanowski\\Desktop\\Roger\\DayofData2020\\final\\blockgroup.shp")


#View the data so that you can see the variable names
names (spdat) #note that some variables are averages and others are sums. Variables that start with "P" are porportions
# Avg_MEDHIN = Median Household Income
# Avg_Avg_NO = Average Air Quality (higher is worse)
# Pedu = Porportion of people with a BA degree or higher
# Pminori = Porportion of non-white
# Sum_TOTPOP = total number of people per unit
# Sum_MINORI = total number of non-white
# Sum_BA_HIG = total number of people with BA degree or higher

#Regressing Air Quality on Porportion Minority
spdat.lm<-lm(spdat$Avg_Avg_NO~spdat$Pminori, data=spdat)
summary.lm(spdat.lm)

#Scatter Plot of Results with Regression Line
plot(spdat$Avg_Avg_NO, spdat$Pminori, ylab = "Percent NonWhite", xlab = "NO2 parts per billion", pch=21, bg="royalblue")
abline(lm(spdat$Pminori~ spdat$Avg_Avg_NO), col="red", lwd=2, lty=2)


#############################################################
#############################################################

#Let's look at a different relationship...one that DOES show MAUP effects
#Try county first...then schooldistrict...then censustract
#Both Zoning and Aggregation Effects are Involved with MAUP. More observations...
setwd("C:\\Users\\krzyzanowski\\Desktop\\Roger\\DayofData2020\\final")
spdat<-readShapePoly("C:\\Users\\krzyzanowski\\Desktop\\Roger\\DayofData2020\\final\\county.shp")

#Regressing Porportion Minority on Education
spdat.lm<-lm(spdat$Pedu~spdat$Pminori, data=spdat)
summary.lm(spdat.lm)

#Scatter Plot of Results with Regression Line
plot(spdat$Pedu, spdat$Pminori, ylab = "Porportion of NonWhite", xlab = "Porportion of Population with BA Degree or Higher", pch=21, bg="royalblue")
abline(lm(spdat$Pminori~ spdat$Pedu), col="red", lwd=2, lty=2)



