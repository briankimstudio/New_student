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
                     fontSize = 13,fontFamily ="sans-serif",
                     LinkGroup="source",NodeGroup="name",
                     JS("d3.scaleOrdinal(d3.schemeCategory20);"),
                     width=1000,height=900,
                     sinksRight=T,iterations = 0)
  
  p$x$nodes$source <- nodes$name %in% links$source
  
  # p <- onRender(p,
  #               '
  # function(el) {
  #   d3.select(el)
  #     .selectAll(".node text")
  #     .filter(d => d.source)
  #     .attr("x",-6)
  #     .attr("text-anchor", "end");
  # }
  # '
  # )
  p
}