library(xlsx)
library(tidyverse)
library(ggplot2)
library(networkD3)
library(htmlwidgets)
library(webshot)
library(treemap)



drawTreemap <- function(ds, prefix, item) {
  svg(filename=paste0(prefix,"_",item,".svg"))
  ds %>%
    count(!!!sym(item), sort=TRUE) %>%
    mutate( tmp = paste(!!sym(item),n,sep="\n")) %>%
    treemap(index="tmp",vSize="n", title=item)
  dev.off()
}
mSankey <- function(x,prefix,...) {
  vList<-list(...)
  end <- length(vList)-1
  results <- x %>%
    group_by(!!sym(vList[[1]]),!!sym(vList[[2]])) %>%
    count()
  names(results) <- c("source","target","value")
  # show(results)  
  if ( end >= 2) {
    for (i in 2:end) {
      show(paste(vList[[i]],vList[[i+1]]))
      tmp <- x %>%
        group_by(!!sym(vList[[i]]),!!sym(vList[[i+1]])) %>%
        count()
      names(tmp) <- c("source","target","value")
      results <-bind_rows(results,tmp)
    }
  }
  # show(results)
  links <- results
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
                     LinkGroup="source",NodeGroup="name",
                     JS("d3.scaleOrdinal(d3.schemeCategory20);"),
                     sinksRight=FALSE)
  
  p$x$nodes$source <- nodes$name %in% links$source
  
  p <- onRender(p,
                '
  function(el) {
    d3.select(el)
      .selectAll(".node text")
      .filter(d => d.source)
      .attr("x",-6)
      .attr("text-anchor", "end");
  }
  '
  )
  show(p)
  # p <- htmlwidgets::prependContent(p, htmltools::tags$h4(paste(psrc,"~",pdst)))
  filename <- ""
  for (i in list(...))
    filename <- paste0(filename,i,"_")
  # show(filename)
  filename <- paste0(prefix,"_",filename)
  saveWidget(p, file=paste0(filename,".html"))
  webshot(paste0(filename,".html"),paste0(filename,".png"), vwidth = 1500, vheight = 900,cliprect = "viewport")
}

dataset <- read.xlsx("application_data.xlsx","Sheet1",header=TRUE)
dataset <- dataset %>%
  mutate(College = str_remove(College,"College of ")) %>%
  mutate(Department = str_remove(Department,"Department of "))
str(dataset)

prefix <- "NCHU"
drawTreemap(dataset,prefix,"Nationality")
drawTreemap(dataset,prefix,"College")
drawTreemap(dataset,prefix,"Department")
drawTreemap(dataset,prefix,"Degree")

mSankey(dataset,prefix,"Nationality","College")
mSankey(dataset,prefix,"Nationality","Department")
mSankey(dataset,prefix,"Nationality","Degree")
mSankey(dataset,prefix,"Department","Degree")
mSankey(dataset,prefix,"Nationality","College","Department")
mSankey(dataset,prefix,"Degree","Nationality","College")
mSankey(dataset,prefix,"Degree","Nationality","Department")

dataset <- read.xlsx("fcu-2020.xlsx","Sheet1",header=TRUE)
dataset <- dataset %>%
  mutate(College = str_remove(College,"College of ")) %>%
  mutate(Department = str_remove(Department,"Department of "))
str(dataset)

prefix <- "FCU"
drawTreemap(dataset,prefix,"Nationality")
drawTreemap(dataset,prefix,"College")
drawTreemap(dataset,prefix,"Department")
drawTreemap(dataset,prefix,"Degree")

mSankey(dataset,prefix,"Nationality","College")
mSankey(dataset,prefix,"Nationality","Department")
mSankey(dataset,prefix,"Nationality","Degree")
mSankey(dataset,prefix,"Department","Degree")
mSankey(dataset,prefix,"Nationality","College","Department")
mSankey(dataset,prefix,"Degree","Nationality","College")
mSankey(dataset,prefix,"Degree","Nationality","Department")



# dataset %>%
#   count(Nationality, sort=TRUE) %>%
#   ggplot(aes(x=Nationality, y=n, fill=Nationality)) +
#   geom_bar(stat="identity", width=1) 

# dataset %>%
#   count(Nationality, sort=TRUE) %>%
#   top_n(20)

# dataset %>%
#   group_by(Nationality) %>%
#   summarise(count = n()) %>%
#   arrange(desc(count)) %>%
#   mutate( proportion = count / sum(count) * 100 )

# a<-dataset %>%
#   count(Nationality, sort=TRUE) 

# dataset %>%
#   count(Nationality, sort=TRUE) %>%
#   mutate(Nationality=paste(Nationality,n,sep="\n")) %>%
#   treemap(index="Nationality",
#       vSize="n", title="By nationality"
#   )

# svg(filename="faithful.svg")
# 
# dataset %>%
#   count(Nationality, sort=TRUE) %>%
#   mutate(Nationality=paste(Nationality,n,sep="\n")) %>%
#   treemap(index="Nationality",
#           vSize="n", title="By nationality"
#   )
# dev.off()

# dataset %>%
#   count(Department, sort=TRUE) %>%
#   mutate(Department=paste(Department,n,sep="\n")) %>%
#   treemap(index="Department",
#       vSize="n", title="By department"
#   )
# 
# dataset %>%
#   count(College, sort=TRUE) %>%
#   mutate(College=paste(College,n,sep="\n")) %>%
#   treemap(index="College",
#           vSize="n", title="By college"
#   )
# 
# dataset %>%
#   count(Degree, sort=TRUE) %>%
#   mutate(Degree=paste(Degree,n,sep="\n")) %>%
#   treemap(index="Degree",
#           vSize="n", title="By degree"
#   )


# dataset %>%
#   count(Department, sort=TRUE) %>%
#   mutate(Department=str_remove(Department,"Department of ")) %>%
#   mutate(Department=paste(Department,n,sep="\n")) %>%
#   treemap(index="Department",
#       vSize="n", title="By department"
#   )

# dataset %>%
#   count(Department, sort=TRUE) %>%
#   mutate(Department = str_remove(Department,"Department of "))
# 
# dataset %>%
#   count(Department, sort=TRUE) %>%
#   filter ( str_detect(Department,"Technology") )
# 
# dataset %>%
#   filter ( str_detect(Department,"Technology") ) %>%
#   select(-c(Department,College))

# dataset %>%
#   filter (str_detect(Degree,"Ph")) %>%
#   count(Department,sort=T)
# 
# dataset %>%
#   filter (str_detect(Degree,"Ph")) %>%
#   count(Nationality,sort=T)
#   
# dataset %>%
#   filter (str_detect(Degree,"Ph") & str_detect(Nationality,"India")) %>%
#   select(Department)

# dataset %>%
#   filter (str_detect(Degree,"Ph") & str_detect(Nationality,"Pakistan")) %>%
#   select(Department)


# dataset %>%
#   count(Degree, sort=TRUE) %>%
# ggplot(aes(x="", y=n, fill=Degree)) +
#   geom_bar(stat="identity", width=1) +
#   coord_polar("y", start=0)


# linkColor <- '3.scaleOrdinal().range(["#7d3945", "#e0677b", "#244457"])'
# 
# # N_Degree
#   group_by(Nationality,Degree) %>%
#   count()
# links <- dataset %>%
# names(links) <- c("source","target","value")
# nodes <- data.frame(
#   name=c(as.character(links$source), 
#          as.character(links$target)) %>% unique()
# )
# links$IDsource <- match(links$source, nodes$name)-1 
# links$IDtarget <- match(links$target, nodes$name)-1
# p <- sankeyNetwork(Links = links, Nodes = nodes,
#                    Source = "IDsource", Target = "IDtarget",
#                    Value = "value", NodeID = "name", 
#                    fontSize = 14,fontFamily ="sans-serif",
#                    LinkGroup="source",NodeGroup="name",
#                    JS("d3.scaleOrdinal(d3.schemeCategory20);"),
#                    sinksRight=FALSE)
# p
# 
# # N_C
# links <- dataset %>%
#   group_by(Nationality,College) %>%
#   count()
# names(links) <- c("source","target","value")
# nodes <- data.frame(
#   name=c(as.character(links$source), 
#          as.character(links$target)) %>% unique()
# )
# links$IDsource <- match(links$source, nodes$name)-1 
# links$IDtarget <- match(links$target, nodes$name)-1
# p <- sankeyNetwork(Links = links, Nodes = nodes,
#                    Source = "IDsource", Target = "IDtarget",
#                    Value = "value", NodeID = "name", 
#                    fontSize = 14,fontFamily ="sans-serif",
#                    sinksRight=FALSE)
# p

#
# Two variable sankey diagram
#

# twoSankey <- function (ds, psrc, pdst) {
#     group_by(!!sym(psrc),!!sym(pdst)) %>%
#   links <- ds %>%
#     count()
#   names(links) <- c("source","target","value")
#   nodes <- data.frame(
#     name=c(as.character(links$source),
#            as.character(links$target)) %>% unique()
#   )
#   links$IDsource <- match(links$source, nodes$name)-1
#   links$IDtarget <- match(links$target, nodes$name)-1
#   p <- sankeyNetwork(Links = links, Nodes = nodes,
#                      Source = "IDsource", Target = "IDtarget",
#                      Value = "value", NodeID = "name",
#                      fontSize = 14,fontFamily ="sans-serif",
#                      LinkGroup="source",NodeGroup="name",
#                      JS("d3.scaleOrdinal(d3.schemeCategory20);"),
#                      sinksRight=FALSE)
# 
#   p$x$nodes$source <- nodes$name %in% links$source
# 
#   p <- onRender(p,
#                 '
#   function(el) {
#     d3.select(el)
#       .selectAll(".node text")
#       .filter(d => d.source)
#       .attr("x",-6)
#       .attr("text-anchor", "end");
#   }
#   '
#   )
#   show(p)
#   # p <- htmlwidgets::prependContent(p, htmltools::tags$h4(paste(psrc,"~",pdst)))
#   saveWidget(p, file=paste0(psrc,pdst,".html"))
#   webshot(paste0(psrc,pdst,".html"),paste0(psrc,pdst,".png"), vwidth = 1500, vheight = 900,cliprect = "viewport")
# }
# 
# twoSankey(dataset, 'Nationality','College')
# twoSankey(dataset, 'Nationality','Department')
# twoSankey(dataset, 'Nationality','Degree')
# twoSankey(dataset, 'Department','Degree')

# N_Dept

# links <- dataset %>%
#   group_by(Nationality,Department) %>%
#   mutate(Department=str_remove(Department,"Department of ")) %>%
#   count()
# names(links) <- c("source","target","value")
# nodes <- data.frame(
#   name=c(as.character(links$source), 
#          as.character(links$target)) %>% unique()
# )
# links$IDsource <- match(links$source, nodes$name)-1 
# links$IDtarget <- match(links$target, nodes$name)-1
# p <- sankeyNetwork(Links = links, Nodes = nodes,
#                    Source = "IDsource", Target = "IDtarget",
#                    Value = "value", NodeID = "name", 
#                    fontSize = 14,fontFamily ="sans-serif",
#                    LinkGroup="source",NodeGroup="name",
#                    JS("d3.scaleOrdinal(d3.schemeCategory20);"),
#                    sinksRight=FALSE)
# 
# p$x$nodes$source <- nodes$name %in% links$source
# 
# p <- onRender(p,
#                '
#   function(el) {
#     d3.select(el)
#       .selectAll(".node text")
#       .filter(d => d.source)
#       .attr("x",-6)
#       .attr("text-anchor", "end");
#   }
#   '
# )
# p
# saveWidget(p, file="N_Dept.html")
# webshot("N_Dept.html","N_Dept.png", vwidth = 1500, vheight = 900,cliprect = "viewport")

# D_Dept
# links <- dataset %>%
#   group_by(Department,Degree) %>%
#   count()
# names(links) <- c("source","target","value")
# nodes <- data.frame(
#   name=c(as.character(links$source), 
#          as.character(links$target)) %>% unique()
# )
# links$IDsource <- match(links$source, nodes$name)-1 
# links$IDtarget <- match(links$target, nodes$name)-1
# p <- sankeyNetwork(Links = links, Nodes = nodes,
#                    Source = "IDsource", Target = "IDtarget",
#                    Value = "value", NodeID = "name", 
#                    fontSize = 14,fontFamily ="sans-serif",
#                    sinksRight=FALSE)
# p
# saveWidget(p, file="D_Dept.html")
# webshot("D_Dept.html","D_Dept.png", vwidth = 1000, vheight = 900)



# Department_Degree_Country
# a<-dataset %>%
#   group_by(Degree,Nationality) %>%
#   count()
# names(a) <- c("source","target","value")
# 
# b<-dataset %>%
#   group_by(Nationality,College) %>%
#   count()
# names(b) <- c("source","target","value")
# 
# c<-dataset %>%
#   group_by(College,Department) %>%
#   count()
# names(c) <- c("source","target","value")
# 
# z<-bind_rows(a,b,c)
# 
# links<-z
# nodes <- data.frame(
#   name=c(as.character(links$source), 
#          as.character(links$target)) %>% unique()
# )
# links$IDsource <- match(links$source, nodes$name)-1 
# links$IDtarget <- match(links$target, nodes$name)-1
# p <- sankeyNetwork(Links = links, Nodes = nodes,
#                    Source = "IDsource", Target = "IDtarget",
#                    Value = "value", NodeID = "name", 
#                    fontSize = 14,fontFamily ="sans-serif",
#                    sinksRight=TRUE)
# p
