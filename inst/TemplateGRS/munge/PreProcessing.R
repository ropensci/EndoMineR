

##### Import ####
## @knitr dataImport






##### Clean ####
## @knitr dataClean
#Prepare the text for OGD
mywordsOGD<-c("hospital number:","patient name:","general practitioner","date of procedure:","endoscopist:","2nd endoscopist:","medications:","instrument:","extent of exam:","indications:", "procedure performed:","findings:","diagnosis:")
#MyOGD<-textPrep(TheOGDReportFinal$OGDReportWhole,mywordsOGD)
MyOGD<-textPrep(TheOGDReportFinal$OGDReportWhole,mywordsOGD,NegEx="TRUE",Extractor="1")

#Prepare the text for Pathology
mywordsPath<-c("hospital number:","patient name:","dob:","general practitioner:","date received:","nature of specimen:","macroscopic description:" ,"diagnosis:")
#MyPath<-textPrep(PathDataFrameFinal$PathReportWhole,mywordsPath)
MyPath<-textPrep(PathDataFrameFinal$PathReportWhole,mywordsPath,NegEx="TRUE",Extractor="1")







##### Merge ####
## @knitr dataMerge 
FinalDataset<-Endomerge2(MyOGD,"dateofprocedure","hospitalnumber",MyPath,"datereceived","hospitalnumber")







##### Clean2 ####
## @knitr dataClean2
FinalDataset$endoscopist<-EndoscEndoscopist(FinalDataset$endoscopist)
FinalDataset$instrument<-EndoscInstrument(FinalDataset$instrument)







##### Extrapolate ####
## @knitr dataExtrapolate
#Extract and clean the meds (need to bind to the existing dataframe)
FinalDataset<-cbind(EndoscMeds(FinalDataset$medications),FinalDataset)
#Now extrapolation with histology
FinalDataset$NumBx<-HistolNumbOfBx(FinalDataset$macroscopicdescription,'specimen')
FinalDataset$BxSize<-HistolBxSize(FinalDataset$macroscopicdescription)
HistolTypeAndSiteToJoin<-HistolTypeAndSite(FinalDataset$procedureperformed,FinalDataset$macroscopicdescription,FinalDataset$Original.x)
FinalDataset<-cbind(FinalDataset,HistolTypeAndSiteToJoin)







##### Extrapolate2 ####
## @knitr dataExtrapolate2
FinalDataset$EndoscopyEvent<-EndoscopyEvent(FinalDataset,"findings","procedureperformed","macroscopicdescription","Original.x")








##### Analysis Disease Specific ####
## @knitr dataAnalysis_DisSpecific
FinalDatasetBarr<-Barretts_PragueScore(FinalDataset,'findings','Original.x')
FinalDatasetBarr$IMorNoIM<-Barretts_PathStage(FinalDatasetBarr,"Original.x")
FinalDatasetBarr$FUType<-Barretts_FUType(FinalDatasetBarr,"CStage","MStage","IMorNoIM")
FinalDatasetBarr$Paris<-BarrettsParisEMR(FinalDatasetBarr$procedureperformed,FinalDatasetBarr$findings)
#BarrettsBxQual2<-BarrettsBxQual(FinalDatasetBarr,'Date.x','pHospitalNum','endoscopist')










##### Analysis Generic ####
## @knitr dataAnalysis_Generic
FinalDatasetBarr<-SurveilTimeByRow(FinalDataset,'pHospitalNum','Date.x')
#Gives you all the last tests done:
LastTestsDone<-SurveilLastTest(FinalDataset,'pHospitalNum','Date.x')
FirstTestsDone<-SurveilFirstTest(FinalDataset,'pHospitalNum','Date.x')
#Need to sort this one out
ff<-HowManyOverTime(FinalDataset,'indications','Date.x','.*')

kk<-MetricByEndoscopist(FinalDataset,'endoscopist','Fent')
EndoBasicGraph(kk, "endoscopist", "avg")









#### Consort ####
## @knitr dataDiagrammeR

nodes <- create_node_df(n=9, 
                        nodes=c("TheOGDReportFinal", "PathDataFrameFinal", "MyOGD","MyPath","FinalDataset","FinalDatasetBarr","LastTestsDone","FirstTestsDone","ff"),
                        label=c(stringr::str_wrap(paste0("TheOGDReportFinal: ",nrow(TheOGDReportFinal)),5),
                                stringr::str_wrap(paste0("PathDataFrameFinal: ",nrow(PathDataFrameFinal)),5), 
                                stringr::str_wrap(paste0("MyOGD: ",nrow(MyOGD)),5),
                                stringr::str_wrap(paste0("MyPath:",nrow(MyPath)),5),
                                stringr::str_wrap(paste0("FinalDataset:",nrow(FinalDataset)),5),
                                stringr::str_wrap(paste0("FinalDatasetBarr: ",nrow(FinalDatasetBarr)),5),
                                stringr::str_wrap(paste0("LastTestsDone: ",nrow(LastTestsDone)),5),
                                stringr::str_wrap(paste0("FirstTestsDone: ",nrow(FirstTestsDone)),5),
                                stringr::str_wrap(paste0("ff: ",nrow(ff)),5)),
                        shape = "rectangle",
                        fontsize=10)

edges <-
  create_edge_df(
    from = c(1,2,3,4,5,6,6,6),
    to =   c(3,4,5,5,6,7,8,9))


g <- create_graph(nodes_df=nodes, 
                  edges_df=edges)%>%
  add_global_graph_attrs(
    attr = c("layout", "rankdir", "splines"),
    value = c("dot", "TB", "false"),
    attr_type = c("graph", "graph", "graph"))
render_graph(g)










##### CodeDepends ####
## @knitr codeDepends


sc = readScript(here("inst","TemplateProject","munge", "PreProcessing.R"))
g = makeVariableGraph( info =getInputs(sc))
require(Rgraphviz)
edgemode(g) <- "directed"
x <- layoutGraph(g, layoutType="neato")
zz = layoutGraph(g)
graph.par(list(nodes = list(fontsize = 25)))
renderGraph(zz)
