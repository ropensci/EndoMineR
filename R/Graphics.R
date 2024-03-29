if (getRversion() >= "2.15.1") {
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
      "manual_pal",
      "RecodedColumn"
    )
  )
}

########### General graph tidy up functions ##############


#' Set the publication theme for all the ggplots
#' 
#' This standardises the theme for any ggplot plot produced.
#' If you do use it, like all ggplots it can be extended using the 
#' "+" to add whatever else is necessary
#' 
#' 
#' @import grid
#' @import scales
#' @import ggthemes
#' @keywords ggplot themes
#' @param base_size the base size
#' @param base_family the base family
#' @export
#' @family Data Presentation helpers
#' @examples
#' # None needed
theme_Publication <- function(base_size = 14, base_family = "Helvetica") {
  (theme_foundation(base_size = base_size, base_family = base_family)
  + theme(
      plot.title = element_text(
        face = "bold",
        size = rel(1.2), hjust = 0.5
      ),
      text = element_text(),
      panel.background = element_rect(colour = NA),
      plot.background = element_rect(colour = NA),
      panel.border = element_rect(colour = NA),
      axis.title = element_text(face = "bold", size = rel(1)),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text = element_text(),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(),
      axis.text.x = element_text(angle = -90),
      axis.text.y = element_text(angle = -90),
      panel.grid.major = element_line(colour = "#f0f0f0"),
      panel.grid.minor = element_blank(),
      legend.key = element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(0.2, "cm"),
      legend.margin = unit(0, "cm"),
      legend.title = element_text(face = "italic"),
      plot.margin = unit(c(10, 5, 5, 5), "mm"),
      strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = element_text(face = "bold")
    ))
}


#' Set the fills for all the ggplots
#' 
#' This standardises the fills for any ggplot plot produced.
#' If you do use it, like all ggplots it can be extended using the 
#' "+" to add whatever else is necessary
#' @keywords ggplot themes
#' @import grid
#' @import ggthemes
#' @import scales
#' @export
#' @family Data Presentation helpers
#' @examples
#' # None needed
scale_fill_Publication <- function() {
  discrete_scale("fill", "Publication", manual_pal(values = c("#386cb0", "#fdb462", "#7fc97f", "#ef3b2c", "#662506", "#a6cee3", "#fb9a99", "#984ea3", "#ffff33")))
  
  }


#' Set the colour theme for all the ggplots
#' 
#' This standardises the colours for any ggplot plot produced.
#' If you do use it, like all ggplots it can be extended using the 
#' "+" to add whatever else is necessary
#'
#' @import grid
#' @import scales
#' @import ggthemes
#' @keywords ggplot themes
#' @export
#' @family Data Presentation helpers
#' @examples
#' # None needed
scale_colour_Publication <- function() {
  discrete_scale("colour", "Publication", manual_pal(values = c("#386cb0", "#fdb462", "#7fc97f", "#ef3b2c", "#662506", "#a6cee3", "#fb9a99", "#984ea3", "#ffff33")))
}




########################################## Patient flow functions#######


#' Create a Sankey plot for patient flow
#'
#' The purpose of the function is to provide a Sankey plot 
#' which allows the analyst to see the proportion
#' of patients moving from one state (in this case type of Procedure) to
#' another. This allows us to see for example how many EMRs are done after
#' RFA.
#' @param dfw the dataframe extracted using the standard cleanup scripts
#' @param ProcPerformedColumn the column containing the test like P
#' rocPerformed for example
#' @param PatientID the column containing the patients unique identifier
#' eg hostpital number
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr '%>%'
#' @importFrom data.table 'setDT' 'rowid'
#' @keywords Sankey
#' @family Patient Flow functions
#' @export
#' @examples names(Myendo)[names(Myendo) == "HospitalNumber"] <- "PatientID"
#' gg <- SurveySankey(Myendo, "ProcedurePerformed", "PatientID")
#' 
SurveySankey <- function(dfw, ProcPerformedColumn, PatientID) {
  # Create the Sankey diagrams
  Sankey <-
    reshape2::dcast(setDT(dfw)[, .SD, PatientID],
      PatientID ~ rowid(PatientID),
      value.var = ProcPerformedColumn
    )
  PtFlow <- Sankey
  PtFlow <- data.frame(PtFlow)
  PtFlow <- PtFlow[!is.na(names(PtFlow))]
  r <- c()

  for (i in seq_along(PtFlow)) {
    t <- paste("ord", i, sep = "")
    r <- c(r, t)
    names(PtFlow) <- r
  }
  orders <- PtFlow %>% select(names(PtFlow))
  orders.plot <- data.frame()
  for (i in 3:ncol(orders)) {
    ord.cache <-
      orders %>%
      dplyr::group_by(orders[, i - 1], orders[, i]) %>%
      dplyr::summarise(n = dplyr::n())

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
      grepl("[A-Z]", orders.plot$to), ]
  orders.plot <-
    orders.plot[!grepl("NA", orders.plot$from) &
      !grepl("NA", orders.plot$to), ]
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
#' type of procedure to another using circos plots. A good example of it's 
#' use might be to see how patients move from one state (e.g. having an
#' EMR), to another state (e.g. undergoing RFA)
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
#'  @family Patient Flow functions
#' @export
#' @examples
#' # This function builds a circos plot which gives a more aggregated
#' # overview of how patients flow from one state to another than the
#' # SurveySankey function
#' # Build a list of procedures
#' Event <- list(
#'   x1 = "Therapeutic- Dilatation",
#'   x2 = "Other-", x3 = "Surveillance",
#'   x4 = "APC", x5 = "Therapeutic- RFA TTS",
#'   x5 = "Therapeutic- RFA 90",
#'   x6 = "Therapeutic- EMR", x7 = "Therapeutic- RFA 360"
#' )
#' EndoEvent <- replicate(2000, sample(Event, 1, replace = FALSE))
#' # Merge the list with the Myendo dataframe
#' fff <- unlist(EndoEvent)
#' fff <- data.frame(fff)
#' names(fff) <- "col1"
#' Myendo$EndoEvent<-fff$col1
#' names(Myendo)[names(Myendo) == "HospitalNumber"] <- "PatientID"
#' names(Myendo)[names(Myendo) == "fff$col1"] <- "EndoEvent"
#' # Myendo$EndoEvent<-as.character(Myendo$EndoEvent)
#' # Run the function using the procedure information (the date of the
#' # procedure, the Event type and the individual patient IDs)
#' hh <- PatientFlow_CircosPlots(Myendo, "Dateofprocedure", "PatientID", "EndoEvent")
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
      dataframe %>%
      dplyr::arrange(!!Endo_ResultPerformeda) %>%
      dplyr::group_by(!!HospNum_Ida) %>%
      dplyr::mutate(
        origin = lag(!!ProcPerformeda, 1),
        destination = !!ProcPerformeda
      ) %>%
      dplyr::select(origin, destination, PatientID) %>%
      dplyr::group_by(origin, destination, PatientID) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      ungroup()

    mydf <- data.frame(reshape2::dcast(mydf, origin ~ destination))

    # Get rid of NA's
    mydf <- mydf[complete.cases(mydf), ]

    V1 <- c("2", "7", "3", "10")
    V2 <- c("210,150,12", "110,255,233", "125,175,0", "255,219,0")

    mydf <- cbind(V1, V2, mydf)

    df_format <-
      mydf %>%
      dplyr::select(1:3) %>%
      dplyr::rename(
        order = V1,
        rgb = V2,
        region = origin
      ) %>%
      dplyr::mutate(region = gsub("_", " ", region))
    # flow matrix. Need to add V1 and V2 to the matrix here

    matmydf <- as.matrix(mydf[, -(1:3)])
    dimnames(matmydf) <-
      list(orig = df_format$region, dest = df_format$region)
    # library('tidyr')
    df_format <-
      df_format %>%
      dplyr::arrange(order) %>%
      tidyr::separate(rgb, c("r", "g", "b")) %>%
      dplyr::mutate(
        col = rgb(r, g, b, max = 255),
        max = rowSums(matmydf) + colSums(matmydf)
      )



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

#' Create a plot over time of patient categorical findings as a line chart
#'
#' This plots the findings at endoscopy (or pathology) over time for individual
#' patients. An example might be with worst pathological grade on biopsy for 
#' Barrett's oesophagus over time 
#' @param theframe dataframe
#' @param EndoReportColumn the column containing the date of the procedure
#' @param myNotableWords The terms from a column with categorical variables
#' @param DateofProcedure Column with the date of the procedure
#' @param PatientID Column with the patient's unique identifier
#' @importFrom dplyr group_by filter n
#' @import ggplot2 
#' @importFrom rlang sym
#' @keywords Patient flow
#' @family Patient Flow functions
#' @export
#' @examples
#' # This function builds chart of categorical outcomes for individal patients over time
#' # It allows a two dimensional visualisation of patient progress. A perfect example is 
#' # visualising the Barrett's progression for patients on surveillance and then
#' # therapy if dysplasia develops and highlighting recurrence if it happens
#' # Barretts_df <- BarrettsAll(Myendo, "Findings", "OGDReportWhole", Mypath, "Histology")
#' # myNotableWords<-c("No_IM","IM","LGD","HGD","T1a","IGD","SM1","SM2")
#' # PatientFlowIndividual(Barretts_df,"IMorNoIM",myNotableWords,DateofProcedure,"HospitalNumber")
#' # Once the function is run you should always call dev.off()




PatientFlowIndividual<-
  function(theframe,EndoReportColumn,myNotableWords,DateofProcedure,PatientID) {
  
PatientIDa <- rlang::sym(PatientID)
theframe$DateofProcedure<-as.Date(gsub("[\r\n].*","",theframe$Dateofprocedure),"%Y-%m-%d")

theframe["RecodedColumn"] <- as.integer(factor(theframe[,EndoReportColumn], myNotableWords, ordered = TRUE))

#Now for rows where there the hospital number is in more than one row:
theframe<-theframe %>% group_by(!!PatientIDa) %>% filter(n() > 2)

#Now develop the patient specific journey with faceted plot in ggplot2
f<-ggplot(theframe) +
  geom_line(aes(DateofProcedure,RecodedColumn),shape=11,size=1) +
  geom_point(aes(DateofProcedure,RecodedColumn),shape=11,colour="red",size=1) +
  xlab("Date") + 
  ylab("Histopathological State") +
  theme(axis.text.x=element_text(angle=-90))

return(f)

#t<-facet_multiple(plot = f, facets = PatientID ,ncol = 1, nrow = 5,scales = "free_x")

}
##################### Basic Graph #########################


#' Basic graph creation using the template specified in theme_Publication.
#' 
#' This creates a basic graph using the template specified in theme_Publication.
#' It takes a numeric column and plots it against any non-numeric x axis in a ggplot
#' @param dataframe dataframe
#' @param number The numeric column
#' @param xdata The x column
#' @import ggplot2
#' @return Myplot This is the final plot
#' @keywords Time plots
#' @export
#' @family Data Presentation helpers
#' @return Myplot
#' @examples
#' # This function plots numeric y vs non-numeric x
#' # Get some numeric columns e.g. number of biopsies and size
#' Mypath$Size <- HistolBxSize(Mypath$Macroscopicdescription)
#' Mypath$NumBx <- HistolNumbOfBx(Mypath$Macroscopicdescription, "specimen")
#' Mypath2 <- Mypath[, c("NumBx", "Size")]
#' EndoBasicGraph(Mypath, "Size", "NumBx")
EndoBasicGraph <-
  function(dataframe, xdata, number) {
    Myplot <-
      ggplot(data = dataframe, aes(x = dataframe[, xdata], y = dataframe[, number], group = 1)) +
      geom_point() +
      geom_smooth(method = "loess") +
      labs(title = paste0(xdata, " vs ", number)) +
      xlab(xdata) +
      ylab(number) +
      scale_colour_Publication() +
      scale_fill_Publication() +
      theme_Publication()
    return(Myplot)
  }
