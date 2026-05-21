library(readxl)
library(ggplot2)
library(dplyr)


# Data-------------------------------------------------------------------------

data<-read_excel("data/Collating the data_final.xlsx")
from_93<-data[7:38,]|>
  mutate(Year=as.numeric(Year))

from_05<-from_93|>
  filter(Year>=2005)

from_15<-from_93|>
  filter(Year>2015)

from_10<-from_93|>
  filter(Year>2010)

--------------------------------------------------------------------------------

# Plot of best fit line through full data set from 1994
ggplot(from_93, aes(x = Year, y = avgLoS)) +
  geom_point(size = 3, color = "darkgray") +
  geom_smooth(method = "glm", 
              method.args = list(family = gaussian(link = "log")), 
              color = "blue", 
              se = TRUE) 

# Plotting on a log scale full data set from 1994 
ggplot(from_93, aes(x = Year, y = log(avgLoS))) +
  geom_point(size = 3) +
  geom_smooth(method = "glm", color = "blue") 

# Does look like decrease is slowing

# Full data fit

model <- lm(log(avgLoS) ~ I(Year - min(Year)), data =from_93)
summary(model)

slope_93 <- coef(model)[2]
annual_growth_pct_93 <- (exp(slope_93) - 1) * 100


## What about using just the last 15 years

# Fitting line to last 15 points from 2010
from_10|>
  ggplot(aes(x = Year, y = avgLoS)) +
  geom_point(size = 3, color = "darkgray") +
  geom_smooth(method = "glm", 
              method.args = list(family = gaussian(link = "log")), 
              color = "blue", 
              se = TRUE) 


# Straight line through last 15 points
model <- lm(log(avgLoS) ~ I(Year - min(Year)), data =from_10)

slope_10 <- coef(model)[2]
annual_growth_pct_10 <- (exp(slope_10) - 1) * 100

