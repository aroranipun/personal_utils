#Name: GG PLOT
#Date:Nov-2018
#Author: Nipun Arora
#-----------------------------------------------------------------------
#SOURCES: 
https://www.r-bloggers.com/a-simple-introduction-to-the-graphing-philosophy-of-ggplot2/
http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html
.libPaths()
library("psych")
library("ggplot2")

SYNTAX: 
data
+ geometry to represent the data,
+ aesthetic mappings of data to plot coordinates like position, color and size
+ scaling of ranges of the data to ranges of the aesthetics

#1. AESTHETICSL aes() 
#	Aesthetics of the geometric and statistical objects, such as color, size, shape and position.
# aes() function is used to specify the X and Y axes. That's because, any information that is 
# part of the source dataframe has to be specified inside the aes() function. 

#2. GEOMETRICS: geom_
# The geometric shapes that will represent the data


3. SCALES: scale_
Maps between the data and the aesthetic dimensions, such as data range to plot width or factor values to colors
Examples
scale_x_continuous(breaks=seq(0, 0.1, 0.01)) can be continuous, date, etc.
scale_x_reverse(),scale_y_reverse()

4 Coordinate System: coord_
The transformation used for mapping data coordinates into the plane of the data rectangle.

  e.g You can set limits by using coord_cartesian(xlim(c(,)),ylim(c())) and cut of points outside the limit from the graph. 
  All points are still considered for statitical analysis

4. LABELS: labs(title = , x= ,y= ,subtitle = )
  in labels arguement, you can give a function that works on out of the label processing e.g. function(x){paste0(x/1000, 'K')
    
5. Theme and inheritance
Theme elements inherit properties from other theme elements heirarchically. For example, axis.title.x.bottom inherits from axis.title.x 
which inherits from axis.title, which in turn inherits from text. All text elements inherit directly or indirectly from text; all lines 
inherit from line, and all rectangular objects inherit from rect. This means that you can modify the appearance of multiple elements by 
setting a single high-level component.

6. Facet Wrap: The facet_wrap() is used to break down a large plot into multiple small plots for individual categories. 
It takes a formula as the main argument. The items to the left of ~ forms the rows while those to the right form the columns
Example: facet_wrap( ~ class, nrow=3,scales = "free") - scale="free" implies different scales for each facet. 


#----------------------------------------------------------------
midwest <- read.csv("http://goo.gl/G1K41K") 
str(midwest)
g_main<-ggplot(midwest,(aes(x=area,y=poptotal)))+
  geom_point(aes(col=state),size=3)+
  geom_smooth(method=lm)
labels<-labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
coordinate_transfrom<-coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) 
scale<- scale_x_reverse(breaks=seq(0, 0.1, 0.01))

g_main+labels+coordinate_transfrom+scale_y_continuous(breaks=seq(0,10000000,200000),labels = function(x){paste0(x/1000, 'K')})

