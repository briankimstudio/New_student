library(ggplot2)
library(showtext)
library(hrbrthemes)
library(readxl)
showtext_auto()

data <- read_excel("Degree.xlsx",sheet="Sheet1",col_names = T)
ggplot(data, aes(fill=Degree, y=N, x=University)) + 
  labs(title="The number of students by degree") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(position="stack", stat="identity") +
  coord_flip()
ggplot(data, aes(fill=Degree, y=N, x=University)) + 
  labs(title="Relative proportion by degree",y="Proportion") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(position="fill", stat="identity") +
  coord_flip()
