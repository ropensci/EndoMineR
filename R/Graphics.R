if (getRversion() >= "2.15.1")
  utils::globalVariables(
    c(
      "PatientID",
      ".SD",
      "CStage",
      "NumbOfBx",
      "Years",
      "Difference",
      "barplot",
      "head",
      "read.table",
      "eHospitalNum",
      "pHospitalNum",
      ".",
      "EVENT",
      "MonthYear",
      "freq",
      "Endoscopist",
      "avg",
      "v",
      "destination",
      "dcast",
      "complete.cases",
      "g",
      "gvisSankey",
      "head",
      "pHospitalNum",
      "par",
      "plot",
      "r",
      "read.table",
      "region",
      "rgb",
      "setDT",
      "Myendo",
      "Mypath",
      "im",
      "manual_pal"
    )
  )

########### General graph tidy up functions ##############


#' #' Sets the publication theme for all the ggplots
#' #'
#' #' 
#' @import grid 
#' @keywords ggplot themes
#' @param base_size
#' @param base_family
#' @export
#' @examples #None needed

theme_Publication <- function(base_size=14, base_family="helvetica") {
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            axis.text.x = element_text(angle = -90),
            axis.text.y = element_text(angle = -90),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}


#' #' Sets the publication theme for all the ggplots
#' #'
#' #' 
#' @keywords ggplot themes
#' @export
#' @examples #None needed


scale_fill_Publication <- function(...){
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}


#' Sets the publication theme for all the ggplots
#'
#' 
#' @keywords ggplot themes
#' @export
#' @examples #None needed

scale_colour_Publication <- function(...){
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

# EndoSubsetEMR <- EndoSubsetEMR[EndoSubsetEMR$EVENT == "EMR", ]
# 
# # Create the matrix
# df3 <-
#   data.frame(EndoSubsetEMR$ParisClass, EndoSubsetEMR$IMorNoIM)
# # Reorganise the column names and rows Get rid of no Paris EMR's
# dfy <- df3[!df3$EndoSubsetEMR.ParisClass == "No_Paris", ]
# # Get the histology proportions by the Paris grade
# tr4 <- as.data.frame.matrix(prop.table(table(dfy), 1))
# 
# tr5 <- as.matrix(tr4)
# tr5 <- head(tr5, -1)
# # Create the heatmap par(oma = c(4, 0, 0, 4))
# 
# tr5 <- tr5[!!rowSums(!is.na(tr5)), ]
# tr5 <- t(tr5)
# tr5 <- tr5[!!rowSums(!is.na(tr5)), ]
# tr5 <- t(tr5)
# if (nrow(tr5) > 2 & ncol(tr5) > 2) {
#   colors <- c(seq(-1, 0.2, length = 100),
#               seq(0.21, 0.8, length = 100),
#               seq(0.81, 1, length = 100))
#   
#   gplots::heatmap.2(
#     tr5,
#     trace = "none",
#     breaks = colors,
#     density.info = "none",
#     dendrogram = "none",
#     Rowv = FALSE,
#     Colv = FALSE,
#     cexRow = 3.5,
#     cexCol = 1.5
#   )
# }


########################################## Patient flow functions#######




#' #' Determine the patient metric of choice over time WORK IN PROGRESS
#' #'
#' #' This function aims to show what is happening over time to a metric
#' #' of choice on a per patient basis.
#' #' An example might be to demonstrate the worst grade of histopathology
#' #' on repeated endoscopic biopsies eg for Barrett's oesophagus
#' #' @param theframe the dataframe,
#' #' @param EndoReportColumn the column of interest,
#' #' @param myNotableWords list of words you are interested in
#' #' @import ggplot2 
#' #' @import stringr
#' #' @import ggplus
#' #' @keywords patient flow
#' #' @export
#' #' @examples #The function relies on defined a list of
#' #' # words you are interested in and then choosing the column you are
#' #' # interested in looking in for these words. This can be for histopathology
#' #' # free text columns or endoscopic. In this example it is for endoscopic
#' #' # columns
#' #' v<-HistolAll(Mypath)
#' #' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure','HospitalNumber')
#' #' b<-Barretts_PathStage(v,'Histology')
#' #' aa<-Barretts_PragueScore(b,'Findings')
#' #' aa<-SurveilTimeByRow(aa,'pHospitalNum','Date.y')
#' #' myNotableWords<-c("No_IM","IM","LGD","HGD","T1a)
#' #' PatientFlowBasic(aa,"IMorNoIM",myNotableWords)
#' 
#' 
#' 
#' PatientFlowBasic <- function(theframe, EndoReportColumn, myNotableWords) {
#' 
#'   theframe["RecodedColumn"] <- as.integer(factor(theframe[,EndoReportColumn], myNotableWords, ordered = TRUE))
#'   
#'   #Now develop the patient specific journey with faceted plot in ggplot2
#'   f<-ggplot(theframe) +
#'     geom_point(aes(Date.x,type),shape=16,size=1) +
#'     xlab("Date") + 
#'     ylab("Histopathological State") +
#'     theme(axis.text.x=element_text(angle=-90)) 
#'   
#'   
#'   
#'   t<-facet_multiple(plot = f, 
#'                  facets = 'pHospitalNum', 
#'                  ncol = 2, 
#'                  nrow = 2)
#'   mylist<-as.list(aa,t)
#'   
#'   return(mylist)
#' }










#' Create a Sankey plot for patient flow
#'
#' This creates a Sankey plot to see the order of tests for all patients:
#' dfw is the dataframe, y is the value of in this case
#' the procedure type (eg EMR,
#'  radiofrequency ablation for Barrett's but can be
#'  any description of a procedure you desire)
#'  Note the Hospital Number column MUST be called PatientID.
#' @param dfw the dataframe extracted using the standard cleanup scripts
#' @param ProcPerformedColumn the column containing the test like P
#' rocPerformed for example
#' @param PatientID the column containing the patients unique identifier
#' eg hostpital number
#' @importFrom dplyr group_by
#' @importFrom magrittr '%>%'
#' @importFrom data.table 'setDT' 'rowid'
#' @keywords Sankey
#' @export
#' @examples # The purpose of the function is to
#' # provide a Sankey plot which allows the analyst to see the proportion
#' # of patients moving from one state (in this case type of Procedure) to
#' # another. This allows us to see for example how many EMRs are done after
#' #RFA. For further patient flow examples see PatientFlow_CircosPlots
#' names(Myendo)[names(Myendo) == 'HospitalNumber'] <- 'PatientID'
#' gg<-SurveySankey(Myendo,"ProcedurePerformed","PatientID")

SurveySankey <- function(dfw, ProcPerformedColumn, PatientID) {
  # Create the Sankey diagrams
  Sankey <-
    reshape2::dcast(setDT(dfw)[, .SD, PatientID],
                    PatientID ~ rowid(PatientID),
                    value.var = ProcPerformedColumn)
  PtFlow <- Sankey
  PtFlow <- data.frame(PtFlow)
  PtFlow <- PtFlow[!is.na(names(PtFlow))]
  r <- c()
  
  #names(PtFlow)<-gsub("X(\\d+)","Event\\1",names(PtFlow))
  for (i in seq_along(PtFlow)) {
    t <- paste("ord", i, sep = "")
    r <- c(r, t)
    names(PtFlow) <- r
  }
  orders <- PtFlow %>% select(names(PtFlow))
  orders.plot <- data.frame()
  for (i in 3:ncol(orders)) {
    ord.cache <-
      orders %>% group_by(orders[, i - 1], orders[, i]) %>% summarise(n = n())
    
    colnames(ord.cache)[1:2] <- c("from", "to")
    
    # adding tags to carts
    ord.cache$from <-
      paste(ord.cache$from, "(", i - 1, ")", sep = "")
    ord.cache$to <- paste(ord.cache$to, "(", i, ")", sep = "")
    
    ord.cache <- data.frame(ord.cache)
    orders.plot <- rbind(orders.plot, ord.cache)
    
  }
  
  
  orders.plot <- data.frame(orders.plot)
  orders.plot <-
    orders.plot[grepl("[A-Z]", orders.plot$from) &
                  grepl("[A-Z]", orders.plot$to),]
  orders.plot <-
    orders.plot[!grepl("NA", orders.plot$from) &
                  !grepl("NA", orders.plot$to),]
  plot(
    googleVis::gvisSankey(
      orders.plot,
      from = "from",
      to = "to",
      weight = "n",
      options = list(
        height = 900,
        width = 1800,
        sankey = "{link:{color:{fill:'black',stroke: 'black', strokeWidth: 1 }},
        node: { color: { fill: '#a61d4c' },
        label: { color: '#871b47',fontName: 'Open Sans',fontSize: 35 } }}"
      )
      )
      )
}


#' Create a Circos plot for patient flow
#'
#' This allows us to look at the overall flow from one
#' type of procedure to another using circos plots.
#' @param dataframe dataframe
#' @param Endo_ResultPerformed the column containing the date of the procedure
#' @param ProcPerformed The procedure that you want to plot (eg EMR,
#'  radiofrequency ablation for Barrett's but can be
#'  any dscription of a procedure you desire)
#' @param HospNum_Id Column with the patient's unique hospital number
#' @importFrom dplyr arrange group_by mutate select summarise lag ungroup rename
#' @importFrom tidyr separate
#' @importFrom magrittr '%>%'
#' @importFrom rlang sym
#' @keywords Circos
#' @export
#' @examples # This function builds a circos plot which gives a more aggregated
#' # overview of how patients flow from one state to another than the
#' # SurveySankey function
#' # Build a list of procedures
#' Event <- list(x1 = "Therapeutic- Dilatation",
#' x2 = "Other-", x3 = "Surveillance",
#' x4 = "APC", x5 = "Therapeutic- RFA TTS",
#' x5 = "Therapeutic- RFA 90",
#' x6 = "Therapeutic- EMR", x7 = "Therapeutic- RFA 360")
#' EndoEvent<-replicate(2000,sample(Event,1, replace = FALSE))
#' # Merge the list with the Myendo dataframe
#' fff<-unlist(EndoEvent)
#' fff<-data.frame(fff)
#' names(fff)<-"col1"
#' Myendo<-cbind(fff$col1,Myendo)
#' names(Myendo)[names(Myendo) == 'HospitalNumber'] <- 'PatientID'
#' names(Myendo)[names(Myendo) == 'fff$col1'] <- 'EndoEvent'
#' # Myendo$EndoEvent<-as.character(Myendo$EndoEvent)
#' # Run the function using the procedure information (the date of the
#' # procedure, the Event type and the individual patient IDs)
#' hh<-PatientFlow_CircosPlots(Myendo,"Dateofprocedure","PatientID","EndoEvent")
#' rm(Myendo)
#' rm(EndoEvent)


PatientFlow_CircosPlots <-
  function(dataframe,
           Endo_ResultPerformed,
           HospNum_Id,
           ProcPerformed) {
    Endo_ResultPerformeda <- rlang::sym(Endo_ResultPerformed)
    HospNum_Ida <- rlang::sym(HospNum_Id)
    ProcPerformeda <- rlang::sym(ProcPerformed)
    
    mydf <-
      dataframe %>% arrange(!!Endo_ResultPerformeda) %>%
      group_by(!!HospNum_Ida) %>%
      mutate(origin = lag(!!ProcPerformeda, 1),
             destination = !!ProcPerformeda) %>%
      select(origin, destination, PatientID) %>%
      group_by(origin, destination, PatientID) %>%
      summarise(n = n()) %>% ungroup()
    
    mydf <- data.frame(reshape2::dcast(mydf, origin ~ destination))
    
    # Get rid of NA's
    mydf <- mydf[complete.cases(mydf),]
    
    V1 <- c("2", "7", "3", "10")
    V2 <- c("210,150,12", "110,255,233", "125,175,0", "255,219,0")
    
    mydf <- cbind(V1, V2, mydf)
    
    df_format <-
      mydf %>% select(1:3) %>% rename(order = V1,
                                      rgb = V2,
                                      region = origin) %>%
      mutate(region = gsub("_", " ", region))
    # flow matrix. Need to add V1 and V2 to the matrix here
    
    matmydf <- as.matrix(mydf[,-(1:3)])
    dimnames(matmydf) <-
      list(orig = df_format$region, dest = df_format$region)
    # library('tidyr')
    df_format <-
      df_format %>% dplyr::arrange(order) %>%
      separate(rgb, c("r", "g", "b")) %>%
      mutate(col = rgb(r, g, b, max = 255),
             max = rowSums(matmydf) + colSums(matmydf))
    
    
    
    circlize::circos.clear()
    par(mar = rep(0, 4), cex = 0.9)
    circlize::circos.par(start.degree = 90, gap.degree = 4)
    par(cex = 0.8, mar = c(0, 0, 0, 0))
    circlize::chordDiagram(
      x = matmydf,
      directional = 1,
      order = df_format$region,
      grid.col = df_format$col,
      annotationTrack = "grid",
      transparency = 0.25,
      annotationTrackHeight = c(0.1, 0.1),
      diffHeight = -0.04
    )
    
    
    circlize::circos.trackPlotRegion(
      track.index = 1,
      panel.fun = function(x, y) {
        xlim <- circlize::get.cell.meta.data("xlim")
        ylim <- circlize::get.cell.meta.data("ylim")
        sector.index <- circlize::get.cell.meta.data("sector.index")
        circlize::circos.text(
          mean(xlim),
          mean(ylim),
          sector.index,
          col = "black",
          cex = 0.6,
          facing = "inside",
          niceFacing = TRUE
        )
      },
      bg.border = NA
    )
  }



##################### Exploratory data analysis #########################

#Make sure the data is inputted in the correct format ie one column is the endoscopist
#And the other column is the metric. This can also be used for Time vs numeric eg how many tests
#have been done etc.
#' Create a Circos plot for patient flow
#'
#' This allows us to look at the overall flow from one
#' type of procedure to another using circos plots.
#' @param dataframe dataframe
#' @param Title The plot title
#' @import GGally 
#' @return Myplot the EDA final plot
#' @keywords Circos
#' @export
#' @examples # This function does EDA



EndoDataVizEDA <-
  function(dataframe,Title) {
Myplot <-ggpairs(data=sample,title=Title) # title of the plot
return(Myplot)
}



##################### Non-numeric x Numeric y points #########################

#Make sure the data is inputted in the correct format ie one column is the endoscopist

#' Create non Numeric x versus y
#'
#' This allows us to look at the overall flow from one
#' type of procedure to another using circos plots.
#' @param dataframe dataframe
#' @param Prop The proportion column
#' @param xdata The proportion column
#' @import ggplot2
#' @return Myplot the EDA final plot
#' @keywords Circos
#' @export
#' @return Myplot
#' @examples # This function plot numeric y vs non-numeric x


# # Then just plot it:

EndoDataVizNum_y <-
  function(dataframe,xdata,Prop) {
Myplot <-
  ggplot(data = dataframe, aes(x = xdata, y = Prop ,group=1)) +
  geom_point() +
  labs(title="Number of procedures per year")+
  scale_colour_Publication()+
  scale_fill_Publication()+
  theme_Publication()

return(Myplot)
}

##################### Time x Numeric y points #########################


#Make sure the data is inputted in the correct format ie one column is the endoscopist

#' Create non Numeric x versus y
#'
#' This allows us to look at the overall flow from one
#' type of procedure to another using circos plots.
#' @param dataframe dataframe
#' @param number The numeric column
#' @param xdata The Time column
#' @import ggplot2
#' @return Myplot the EDA final plot
#' @keywords Time plots
#' @export
#' @return Myplot
#' @examples # This function plot numeric y vs non-numeric x
#' 
#' 
EndoDataVizTime_xNum_y <-
  function(dataframe,xdata,number) {
Myplot <-
  ggplot(data = dataframe, aes(x = xdata, y = number)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title="Number per year")+
  scale_colour_Publication()+
  scale_fill_Publication()+
  theme_Publication()
}

##################### Time series plots  #########################





##################### Non-numeric x Numeric y bar #########################

#Make sure the data is inputted in the correct format ie one column is the endoscopist

#' Create non Numeric x versus y
#'
#' This allows us to look at the overall flow from one
#' type of procedure to another using circos plots.
#' @param dataframe dataframe
#' @param b The numeric column
#' @param Proportion The Proportion column
#' @import ggplot2
#' @keywords proportions plot
#' @export
#' @return Myplot
#' @examples # This function plot numeric y vs non-numeric x
#' 
#' 
EndoDataVizNum_xProp_y <-
  function(dataframe,b,Proportion) {
##Input needs to be two columns. x is the term and y is the proportion that have it.
t <-ggplot(dataframe, aes(x=b, y=Proportion)) + 
  geom_bar(stat="identity")+
  xlab("Documentation")+
  labs(title="Proportion of Reports Containing Terms")+
  scale_colour_Publication()+
  theme_Publication()

return(t)
}
#################### Grouped distributions #######################
#Use violin/boxplot/ridgeplot
#This function will produce all of them so any can be used


##################### Numeric x Numeric y  #########################
#Make sure the data is inputted in the correct format ie one column is the endoscopist

#' Create  Numeric x versus y
#'
#' @param dataframe dataframe
#' @param Numx The numeric x column
#' @param Numy The numeric y column
#' @import ggplot2
#' @importFrom stats lm
#' @keywords basic numeric vs numeric plot
#' @export
#' @return Myplot
#' @examples # This function plot numeric y vs non-numeric x
#' 
#' 
EndoDataVizNum_xNum_y <-
  function(dataframe,Numx,Numy) {
    
    Myplot <-
  ggplot(data = dataframe, aes(x = Numx, y = Numy)) +
  geom_point() +
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(title="Number of procedures per year")+
  scale_colour_Publication()+
  scale_fill_Publication()+
  theme_Publication()
    
    return(Myplot)
}
##################### Grouped numeric vs numeric ####################
#This function will produce all permutations so you can choose between them

##################### Time Series ####################
#This function will produce all permutations so you can choose between them

##########Tables###########
#library(stargazer)
#stargazer(iris,type = "html",style = "qje",out="star_descriptive.doc")
