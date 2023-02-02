# Combining-plots
Combining plots

#Plotting NTerminalSNP

library(ggplot2)
library(dplyr)
library(tidyr)

df2<-read.csv("NTerminalSNP.csv")
head(df2)
df2=ggplot(df2, aes(Country, Percentage, fill=SNP))+
  geom_bar(stat ="identity", color="black", fill="#000075", width = 0.7, position = position_dodge())+
  #coord_flip()+
  ggtitle("N-terminal SNP (G98A)")+
  theme_classic()+
  theme(axis.text.x = element_text(color = "black", angle = 45, vjust = 1, hjust = 1, size = 8))+
  theme(axis.text.y = element_text(color = "black", size = 8))+ 
  theme(axis.title = element_text(size = 10, face = "bold"))+
  theme(plot.title = element_text(hjust =0.5, face = "bold", size = 10))
df2

#Plotting NTerminalIndels frequency

library(ggplot2)
library(dplyr)
library(tidyr)
df1<-read.csv("NTerminalIndels.csv")
head(df1)
df1=ggplot(df1, aes(Indel, Percentage, fill=Country))+
  geom_bar(stat ="identity", width = 0.8, position = position_dodge() )+
  #coord_flip()+
  ggtitle("N-terminal INDELs")+
  theme_classic()+
  theme(axis.title = element_text(size = 10, face = "bold"))+
  theme(plot.title = element_text(hjust =0.5, face = "bold", size = 10))+
  theme(axis.text.x = element_text(color = "black", size = 8))+
  theme(axis.text.y = element_text(color = "black", size = 8))+ 
  theme(legend.position='none')+
  scale_fill_manual(values=c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#800000', '#911eb4', '#46f0f0', '#f032e6', '#000075', '#808000', '#008080', '#000000'))
df1

library(plotly)

x <- c("Bioko", "Cameroon", "Gambia", "Ghana", "Iran", "Kenya", "Malawi", "Phillipiens", "Solomon Island", "Tanzania", "Vanuatu", "Venezuela")
y1 <- c(0, 0, 100, 0, 100, 0, 0, 0, 0, 0, 0, 0)
y2 <- c(49, 100, 100, 26, 100, 31, 0, 100, 100, 100, 100, 100)
data <- data.frame(x, y1, y2)

#The default order will be alphabetized unless specified as below:
data$x <- factor(data$x, levels = data[["x"]])

fig <- plot_ly(data, x = ~x, y = ~y1, type = 'bar', name = '1MMRKLAILSVSSFLF15', marker = list(color = 'rgb(49,130,189)'))
fig <- fig %>% add_trace(y = ~y2, name = '76DGNNNNGDNGREGKDEDKR77', marker = list(color = '#EC8FA3'))
fig <- fig %>% layout(xaxis = list(title = "Country", size = 100, face = "bold", tickangle = -45),
                      yaxis = list(title = "Prevalence(%)"),
                      margin = list(b = 100),
                      barmode = 'group')

fig



#Plotting NaturalTheoryofNaturalSelection

library(ggplot2)
library(dplyr)
library(tidyr)
df<-read.csv("NaturalTheoryofNaturalSelection.csv")
head(df)
df=ggplot(df, aes(Region, Value, fill=Parameter))+
  geom_bar(stat ="identity", width = 0.7, position = position_dodge() )+
  #coord_flip()+
  ggtitle("Natural Theory of Natural Selection")+
  theme_classic()+
  theme(axis.title = element_text(size = 10, face = "bold"))+
  theme(plot.title = element_text(hjust =0.5, face = "bold", size = 10))+
  theme(legend.position = c(0.4, 0.3))+
  theme(axis.text.x = element_text(color = "black", size = 10))+
  theme(axis.text.y = element_text(color = "black", size = 10))+ 
  theme(legend.title = element_text(face = "bold"))+
  scale_fill_manual(values=c('#e6194b', '#000075', '#f032e6', '#4363d8', '#800000', '#911eb4', '#46f0f0', '#f032e6', '#000075', '#808000', '#008080', '#000000'))

df

#Plotting Cterminal-SNP frequency

library(ggplot2)
library(dplyr)
library(tidyr)
df3<-read.csv("CTerminalSNPs.csv")
head(df3)
df3=ggplot(df3, aes(SNP, Frequency, fill=Country))+
  geom_bar(stat ="identity", width = 0.7, position = position_dodge() )+
  #coord_flip()+
  ggtitle("C-Terminal SNPs")+
  theme_classic()+
  theme(axis.title = element_text(size = 10, face = "bold"))+
  theme(plot.title = element_text(hjust =0.5, face = "bold", size = 10))+
  theme(legend.position="bottom", legend.key.size = unit(0.4, 'cm'),)+
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_manual(values=c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#800000', '#911eb4', '#46f0f0', '#f032e6', '#000075', '#808000', '#008080', '#000000'))
df3
library(ggpubr)

ggarrange(df2, df1, ncol = 2, nrow = 2, labels = c("A", "B", "C"), common.legend = TRUE, legend = "right")

ggarrange(df3,                                                 # First row with scatter plot
          ggarrange(df2, df1, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
          nrow = 2, 
          labels = "A", common.legend = TRUE, legend = "right"                                        # Labels of the scatter plot
) 

library("gridExtra")
library("cowplot")

grid.arrange(df3,                             # First row with one plot spaning over 2 columns
             arrangeGrob(df2, df1, ncol = 2), # Second row with 2 plots in 2 different columns
             nrow = 2)                       # Number of rows
grid.arrange(bp,                                    # bar plot spaning two columns
             bxp, sp,                               # box plot and scatter plot
             ncol = 2, nrow = 2, 
             layout_matrix = rbind(c(1,1), c(2,3)))
