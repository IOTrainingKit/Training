rm(list = ls())
options(scipen=999)

####################################
#             PART 1               #
####################################
#link for the videos: https://www.youtube.com/playlist?list=PL1NulKAe6lOwV4yvRT7evM-Asxuw4FTue

library(readxl)
# loading the data 
path <- "ADD FILE PATH"
wiot <- as.matrix(read_excel(paste(path,"/BRA_NIOT_ROW_Sep12.xlsx",sep=""), sheet = "2010")) ## UPDATE THE PATH OF THE FILE IN YOUR COMPUTER
# Change the year in "sheet = YEAR" for different years

# extracting the labels 
labels.IO <- wiot[6:39,2]
labels.FD <- wiot[3,40:44]
labels.VA <- wiot[77:80,2]

# extracting Matrices Z, FD, VA and x
no.sectors <- 34

Z <- matrix(as.numeric(wiot[6:39,5:38]),no.sectors) # extracting the domestic intermediate consumption matrix (excluding the last sector)
Zm <- matrix(as.numeric(wiot[41:74,5:38]),no.sectors) # extracting the imported intermediate consumption matrix
v <- as.matrix(as.numeric(wiot[81,5:38])) # extracting the value-added vector
VA <- matrix(as.numeric(wiot[77:80,5:38]),4) # extracting the adjustments
FD <- matrix(as.numeric(wiot[6:39,40:45]),no.sectors) # extracting final demand (domestic (C+I+G)+exp)
x.in <- as.matrix(as.numeric(wiot[83,5:38])) # output as Z + FD
x.out <- as.matrix(as.numeric(wiot[6:39,46])) # output as Z + VA

y <- apply(FD,1,sum) # creating the total final demand vector (categories are aggregated)


####################################
#             PART 2               #
####################################


# comparing two outputs to see if they match:
sum((x.in-x.out)^2)==0 #if TRUE, the output vectors are equal 
x <- x.out # establishing the x vector of total output
rm(x.in,x.out)


# calculating A, L and G matrices
x <- x + 0.00001 # we add this so we don't operate the matrix with zeroes, the stronger your PC the smaller you could make this number
X <- diag(as.vector(x)) # diagonalize and invert the x vector

A <- Z %*% solve(X) # A matrix: we are dividing every cell in Z by a total column value in X 
B <- solve(X) %*% Z #B matrix: we are dividing each row of Z by its total value in x


I <- diag(length(labels.IO)) # Creating an Identity matrix with the same size as the IO table

L <- solve(I-A) # L matrix: the leontief inverse with direct and indirect inputs needed for the production of 1 unit
G <- solve(I-B) # G matrix: the Ghosh inverse or Output inverse, shows where production is distributed directly and indirectly


# calculating multipliers
Multipliers <- cbind(apply(A,2,FUN=sum),apply(L,2,FUN=sum),apply(B,1,FUN=sum),apply(G,1,FUN=sum)) # here we aggregate the A and L matrices vertically, and the B and G matrices horizontally
colnames(Multipliers) <- c("A", "L", "B", "G") # we name the columns
rownames(Multipliers) <- labels.IO # we name the rows with sector labels
View(Multipliers) # visualize results - you can order them with R visualization tool

# averaged multipliers/linkages (backward and forward)
L.mean <- sum(apply(L,2,FUN=sum)/nrow(L))
G.mean <- sum(apply(G,1,FUN=sum)/nrow(G))

BL <- as.data.frame(apply(L,2,FUN=sum)/L.mean)
FL <- as.data.frame(apply(G,1,FUN=sum)/G.mean)

Linkages <- cbind(BL,FL)
colnames(Linkages) <- c("BL", "FL") # we name the columns
rownames(Linkages) <- labels.IO # we name the rows with sector labels
View(Linkages) # visualize results - you can order them with R visualization tool

# you can see that the order of the sectors don't change, but this method averages the multiplier effects of a sector and, hence, values are lower than in the "Multipliers"
# there is no better method, you can choose which measure of linkages/multipliers to use depending on your goals


####################################
#             PART 3               #
####################################

# Now let's plot and visualize the results 

install.packages("ggrepel")
library(ggrepel)
install.packages("tidyr")
library(tidyr)
install.packages("dplyr")
library(dplyr)

# preparation:
Linkages$x <- x # we add our output vector to compare output size 
  
# we prepare an industry vector
ind <- c("AGRICULTURE","MINING", rep("MANUFACTURING",15),"CONSTRUCTION",rep("TRADE",4),
         rep("TRANSPORT",4),rep("SERVICES",8))

# we prepare a color vector - you can chose different ones
colors <- c("#486856","#a4b8e1","#2b637b","darkorange3" ,
            "#b33b6b","darkred","#70446f")


# plotting a scatterplot 
ggplot(Linkages,aes(BL,FL,size=x))+
  geom_point()+
  theme_bw()+labs(x="Backward Linkages",y="Forward linkages",size="Output")+
  geom_text_repel(label=labels.IO,size=3)+
  geom_hline(yintercept=1,linetype="dashed", color = "grey")+
  geom_vline(xintercept=1,linetype="dashed", color = "grey")

# plotting a scatterplot with colours 
Linkages$Cluster <- with(Linkages, ifelse(BL > 1 & FL > 1, "Key sectors",
                                            ifelse(BL > 1 & FL < 1, "Propulsive sectors",
                                                   ifelse(BL < 1 & FL > 1, "Critical supplies",
                                                          "Less integrated sectors"))))
ggplot(Linkages,aes(BL,FL,size=x, color=Cluster))+
  geom_point()+
  theme_bw()+labs(x="Backward Linkages",y="Forward linkages")+
  geom_text_repel(label=labels.IO,size=2)+
  geom_hline(yintercept=1,linetype="dashed", color = "grey")+
  geom_vline(xintercept=1,linetype="dashed", color = "grey")+
  scale_size_continuous(range = c(2, 6),
                        guide = "none")+
  scale_color_manual(values=colors[c(3,4,5,6)])+
  ggtitle("Backward and Forward Linkages")
# In the plot you can see all the Brazilian sectors positioned according to their BL and FL


# Now let's plot some data from the Ghosh and Leontief Matrices 
# Bar chart for Ghosh matrix
# We trace the flows of our Ghosh matrix: How is the output for each sector i distributed across sectors j?

colnames(G) <- labels.IO 
G.long <- as.data.frame(G-I) %>% #we subtract I to look only at sector's connection with its own allocation
  mutate(i = factor(labels.IO,rev(labels.IO))) %>% 
  pivot_longer(
    cols = c(1:34),
    names_to = "j",
    values_to = "flow"
  )

G.long$industry <- rep(factor(ind,levels=unique(ind)),times=34)

ggplot(G.long, aes(x = i, y = flow, fill = industry)) +
  geom_bar(stat = "identity") +
  theme_bw()+
  scale_fill_manual(values=colors)+
  ggtitle("Interindustrial flows (Ghosh Matrix)")+
  coord_flip()
# tip: use the zoom option to open a new window with a larger plot
# In this plot you can how is the output of each sector allocated to major industrial groups of the Brazilian economy


# Now we do the same for the Leontief Matrix
# We trace the flows of our Leontief Matrix: How is the demand of sector j distributed across sectors i?
colnames(L) <- labels.IO
L.long <- as.data.frame(L-I) %>% #we subtract I to look only at sector's connection with its own inputs
  mutate(i = labels.IO) %>%
  pivot_longer(
    cols = c(1:34),
    names_to = "j",
    values_to = "flow"
  )

L.long$j <- factor(L.long$j, levels = rev(labels.IO))
L.long$industry <- rep(factor(ind,levels=unique(ind)),each=34)

ggplot(L.long, aes(x = j, y = flow, fill = industry)) +
  geom_bar(stat = "identity") +
  theme_bw()+
  scale_fill_manual(values = colors)+
  ggtitle("Interindustrial flows (Leontief Matrix)")+
  coord_flip()

#  Lastly, we build a treemap graph to assess the Brazilian export structure 

install.packages("treemapify") # installing packages for treemap
library(treemapify)

data <- as.data.frame(exp)
data$perc <- round(data$V1/sum(data$V1)*100,2)
data$ind <- factor(labels.IO,levels=labels.IO)
data$industry <- factor(ind)


# Create treemap
ggplot(data, aes(area = perc, fill = industry, label = paste(ind, "\n", perc, "%"),subgroup=industry)) +
  geom_treemap() +
  geom_treemap_text(color = "white", place = "bottom", grow = F) +
  theme(legend.position = "none") +
  ggtitle("Export Categories")+
  scale_fill_manual(values = colors)

# In the plot the whole exports represent the total square. 
# Each small square is adjusted to the relative size of the exports of each industry to the total exports.
# Colors follow the classification of major industries

