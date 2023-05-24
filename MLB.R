# Notes & Lecture by Akhona Njeje.
# Date 24 May 2023.
# Topic : Data Science in Sports aka Sports Analytics.
# Solution : The aim is too sign the most undervalued players using Statistical & Analysis.
# Part 2 : Major League Baseball(MLB), Moneyball. 

# df = mtcars
# df



bat = read.csv("D:/Users/NjejeA/Downloads/My Research Projects/Raw Solutions/R Solutions/Machine Learning/Part 2/Batting.csv",header = TRUE)
bat
print(head(bat))

# Lets check the structure of the dataset.
str(bat)   # Less than 100k rows.Not that big of a dataset. 24 Variables = Columns.

# Call the head() of the first five rows of AB (At Bats) column.
head(bat$AB)

# Call the head of the doubles (X2B) column.
head(bat$X2B)

# Feature Engineering
# We need to add three more statistics that were used in Moneyball! These are:

   # Batting Average.
   # On Base Percentage.
   # Slugging Percentage.

# AVB = (H / AB) ---> Batting Average = (Hits / At Base).

bat$BA <- bat$H / bat$AB   # Our formular.

tail(bat$BA,5)   # Checking the last 5 entries of the BA column IN BAT dataset.

# On Base Percentage:

   # Create an OBP Column
   # Create an SLG Column
bat$OBP <- (bat$H + bat$BB + bat$HBP)/(bat$AB + bat$BB + bat$HBP + bat$SF)

# Creating X1B (Singles)
bat$X1B <- bat$H - bat$X2B - bat$X3B - bat$HR

# Creating Slugging Average (SLG)
bat$SLG <- ((1 * bat$X1B) + (2 * bat$X2B) + (3 * bat$X3B) + (4 * bat$HR) ) / bat$AB

# Check the structure of your data frame using str()
str(bat)



# Merging Salary Data with Batting Data.

# We know we don't just want the best players, we want the most undervalued players, meaning we will also need to know current salary information! We have salary information in the csv file 'Salaries.csv'.

# Complete the following steps to merge the salary data with the player stats!

sal <- read.csv('D:/Users/NjejeA/Downloads/My Research Projects/Raw Solutions/R Solutions/Machine Learning/Part 2/Salaries.csv')

print(head(sal))

# Use summary to get a summary of the batting data frame and notice the minimum year in the yearID column.
# Our batting data goes back to 1871! Our salary data starts at 1985, meaning we need to remove the batting data that occured before 1985.
# Use subset() to reassign batting to only contain data from 1985 and onwards.

summary(bat)

bat = subset(bat,yearID >= 1985)

# Now use summary again to make sure the subset reassignment worked, your yearID min should be 1985.

summary(bat)

# Now it is time to merge the batting data with the salary data! Since we have players playing multiple years, 
# we'll have repetitions of playerIDs for multiple years, meaning we want to merge on both players and years.
# Use the merge() function to merge the batting and sal data frames by c('playerID','yearID'). Call the new data frame combo.

combo = merge(bat,sal,by=c('playerID','yearID'))

# Use summary to check the data

summary(combo)

# Analyzing the Lost Players.

# As previously mentioned, the Oakland A's lost 3 key players during the off-season. 
# We'll want to get their stats to see what we have to replace.
# Use the subset() function to get a data frame called lost_players from the combo data frame consisting of those 3 players.

lost_players = subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01') )
print(head(lost_players))

# Since all these players were lost in after 2001 in the offseason, let's only concern ourselves with the data from 2001.
# Use subset again to only grab the rows where the yearID was 2001.

lost_players <- subset(lost_players,yearID == 2001)

# Reduce the lost_players data frame to the following columns: playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB.
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]
head(lost_players)

# Replacement Players:

# Find Replacement Players for the key three players we lost! However, you have three constraints:

# The total combined salary of the three players can not exceed 15 million dollars.
# Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
# Their mean OBP had to equal to or greater than the mean OBP of the lost players.

library(dplyr)
avail.players <- filter(combo,yearID==2001)

library(ggplot2)
ggplot(avail.players,aes(x=OBP,y=salary)) + geom_point()

# Looks like there is no point in paying above 8 million or so (I'm just eyeballing this number). 
# I'll choose that as a cutt off point. 
# There are also a lot of players with OBP==0. Let's get rid of them too.
avail.players <- filter(avail.players,salary<8000000,OBP>0)

# The total AB of the lost players is 1469. 
# This is about 1500, meaning I should probably cut off my avail.players at 1500/3= 500 AB.
avail.players <- filter(avail.players,AB >= 500)

# Now let's sort by OBP and see what we've got!
possible <- head(arrange(avail.players,desc(OBP)),10)

# Grab columns I'm interested in:
possible <- possible[,c('playerID','OBP','AB','salary')]
possible

# Can't choose giambja again, but the other ones look good (2-4). I choose them!
possible[2:4,]
