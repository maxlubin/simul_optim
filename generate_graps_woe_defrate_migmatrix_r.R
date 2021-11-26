

# install.packages("haven","trmatplot")

library(haven)
library(trmatplot)
library(knitr)
library(pander)
library(gridExtra)
library(xtable)

install.packages("stargazer")   # Install & load gridExtra
library("stargazer")

# importing the SAS file:
df <- read_sas("C:/Users/a96h4va/OneDrive - Erste Group/R/data/migmatrix_02.sas7bdat")
head(df)

df1 <- data.frame(df[ ,-2])
head(df1)






pdf(file = "C:/Users/a96h4va/OneDrive - Erste Group/OD4BMigration/Validation/Analysis/final_reports/migmatrxPlot.pdf", width = 7, height = 7) 

#
# Plot graph matrix
#

variabls <- unique(df1$variable);

for(i in 1:length(variabls)){

  dat <- df1[df1$variable==variabls[i], ];
  transi <- data.frame(dat[ ,c("to_1","to_2","to_3","to_4","to_5","to_6","to_7","to_8","to_9","to_10")]);
  
  if(nrow(transi)==10 & ncol(transi)==10){
    
  try({  

  mat = as.matrix(transi);
  rownames(mat) = colnames(mat) 
  
  m.size <- nrow(mat)
  m.size1 <- (m.size+1)
  
  par(mfrow=c(1,1))
  j <- matrix(rep(1:m.size, m.size), m.size)
  i <- as.vector(t(j))
  j <- m.size1 - as.vector(j)
  q.scale <- (mat-min(mat))/diff(range(mat))
  plot(c(0,m.size1), c(0,m.size1), type="n", 
       main=gsub("x","",unique(dat$variable)),
       xlab="To", ylab="From", asp=1,
       bty="n", xaxt="n", yaxt="n")
  points(i, j, col="#e0e0e0")
  points(i, j, pch=22, cex=6, col="#d0d0d0", bg=hsv(q.scale, abs(mat), 0.9))
  text(i, j, paste(round(mat, 3)), cex=0.75)
  text(0, 1:m.size, paste(m.size:1))
  text(1:m.size, 0, paste(1:m.size)); 

  }, silent=TRUE)}; 
  # else { print(i) }
}

dev.off()



#
# Plot tables
#


library(grid)

l.df <- with(df1, split(df1, variable))

# l <- list(l.df[[1]],l.df[[2]],l.df[[3]])

pdf(file = "C:/Users/a96h4va/OneDrive - Erste Group/OD4BMigration/Validation/Analysis/final_reports/migmatrxTable.pdf", width = 10, height = 7) 
lapply(l.df, function(x){
  grid.newpage()
  grid.table(x[ ,c("LB","UB","Range","Bin","EventRate","WOE","IV")])})
dev.off()




# grid.arrange(g)

# stargazer(dat[ ,c("LB","UB","Range","Bin","EventRate","WOE","IV")],                 #
#           summary = FALSE,
#           type = "text",
#           out = "C:/Users/a96h4va/OneDrive - Erste Group/OD4BMigration/Validation/Analysis/final_reports/tablemig.txt")

# print(xtable(dat[ ,c("LB","UB","Range","Bin","EventRate","WOE","IV")]))

# knitr::kable(dat[ ,c("LB","UB","Range","Bin","EventRate","WOE","IV")])
# pander::pander(dat[ ,c("LB","UB","Range","Bin","EventRate","WOE","IV")])









# library(knitr)
# knit2pdf("path_to_your_Rnw_file")




# library(circlize)
# circos.par(gap.degree = 8)
# chordDiagram(mat, grid.col = 1:nrow(transi), directional = TRUE, annotationTrack = "grid",
#              preAllocateTracks = list(list(track.height = 0.05),
#                                       list(track.height = 0.05)))
# 
# circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
#   xlim = get.cell.meta.data("xlim")
#   ylim = get.cell.meta.data("ylim")
#   sector.index = get.cell.meta.data("sector.index")
#   circos.text(mean(xlim), mean(ylim), sector.index, facing = "inside", niceFacing = TRUE)
# }, bg.border = NA)
# circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
#   circos.axis("bottom", major.tick.percentage = 0.2, labels.cex = 0.4)
# }, bg.border = NA)
# circos.clear()



with(df1[ ,c("to_1","to_2","to_3","to_4","to_5","to_6","to_7","to_8","to_9","to_10","variable")], by())

