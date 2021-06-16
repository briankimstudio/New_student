library(xlsx)
library(tidyverse)
library(ggplot2)
library(htmlwidgets)
library(webshot)
library(networkD3)

dataset <- read.xlsx("application_data.xlsx","Sheet1",header=TRUE)

str(dataset)

dataset %>%
  count(Nationality, sort=TRUE) %>%
  ggplot(aes(x=Nationality, y=n, fill=Nationality)) +
  geom_bar(stat="identity", width=1) 
  
dataset %>%
  count(Nationality, sort=TRUE) %>%
  top_n(20)

dataset %>%
  group_by(Nationality) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate( proportion = count / sum(count) * 100 )

library(treemap)
a<-dataset %>%
  count(Nationality, sort=TRUE) 

dataset %>%
  count(Nationality, sort=TRUE) %>%
  mutate(Nationality=paste(Nationality,n,sep="\n")) %>%
  treemap(index="Nationality",
      vSize="n", title="By nationality"
  )

dataset %>%
  count(Department, sort=TRUE) %>%
  mutate(Department=paste(Department,n,sep="\n")) %>%
  treemap(index="Department",
      vSize="n", title="By department"
  )

dataset %>%
  count(Department, sort=TRUE) %>%
  mutate(Department=str_remove(Department,"Department of ")) %>%
  mutate(Department=paste(Department,n,sep="\n")) %>%
  treemap(index="Department",
      vSize="n", title="By department"
  )

dataset %>%
  count(Department, sort=TRUE) %>%
  mutate(Department = str_remove(Department,"Department of "))

dataset %>%
  count(Department, sort=TRUE) %>%
  filter ( str_detect(Department,"Technology") )

dataset %>%
  filter ( str_detect(Department,"Technology") ) %>%
  select(-c(Department,College))

dataset %>%
  filter (str_detect(Degree,"Ph")) %>%
  count(Department,sort=T)

dataset %>%
  filter (str_detect(Degree,"Ph")) %>%
  count(Nationality,sort=T)
  
dataset %>%
  filter (str_detect(Degree,"Ph") & str_detect(Nationality,"India")) %>%
  select(Department)

dataset %>%
  filter (str_detect(Degree,"Ph") & str_detect(Nationality,"Pakistan")) %>%
  select(Department)



dataset %>%
  count(College, sort=TRUE) %>%
  mutate(College=paste(College,n,sep="\n")) %>%
  treemap(index="College",
          vSize="n", title="By college"
  )

dataset %>%
  count(Degree, sort=TRUE) %>%
  mutate(Degree=paste(Degree,n,sep="\n")) %>%
  treemap(index="Degree",
      vSize="n", title="By degree"
  )

dataset %>%
  count(Degree, sort=TRUE) %>%
ggplot(aes(x="", y=n, fill=Degree)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)


# N_Degree
links <- dataset %>%
  group_by(Nationality,Degree) %>%
  count()
names(links) <- c("source","target","value")
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   fontSize = 14,fontFamily ="sans-serif",
                   sinksRight=FALSE)
p

# N_C
links <- dataset %>%
  group_by(Nationality,College) %>%
  count()
names(links) <- c("source","target","value")
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   fontSize = 14,fontFamily ="sans-serif",
                   sinksRight=FALSE)
p

# N_Dept
links <- dataset %>%
  group_by(Nationality,Department) %>%
  count()
names(links) <- c("source","target","value")
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   fontSize = 14,fontFamily ="sans-serif",
                   sinksRight=FALSE)
p
saveWidget(p, file="N_Dept.html")
webshot("N_Dept.html","N_Dept.png", vwidth = 1000, vheight = 900)

# D_Dept
links <- dataset %>%
  group_by(Department,Degree) %>%
  count()
names(links) <- c("source","target","value")
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   fontSize = 14,fontFamily ="sans-serif",
                   sinksRight=FALSE)
p
saveWidget(p, file="D_Dept.html")
webshot("D_Dept.html","D_Dept.png", vwidth = 1000, vheight = 900)
