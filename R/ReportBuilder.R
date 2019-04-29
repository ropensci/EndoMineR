#The idea of the package is that it creates a consort diagram using diagrammeR but all the dataframes are automatically filled.

#' Number of tests done per month and year by indication
#'
#' Get an overall idea of how many endoscopies have been done for an indication
#' by year and month. This is a more involved version of
#' SurveilCapacity function. It takes string for
#' the Indication for the test
#'
#' This returns a list which contains a plot (number of tests for that
#' indication over time and a table with the same information broken down
#' by month and year).
#' @param dataframe dataframe
#' @param Indication Indication column
#' @param Endo_ResultPerformed column containing date the Endoscopy was
#' performed
#' @param StringToSearch The string in the Indication to search for
#' @import EndoMineR
#' @importFrom here here
#' @import DiagrammeR
#' @importFrom magrittr '%>%'
#' @importFrom stringr str_wrap
#' @importFrom stringr str_extract
#' @keywords consort
#' @export
#' @examples
#' pathName<-"/home/rstudio/EndoMineR_devSite/inst/TemplateGRS/munge/PreProcessing.R"
#' sanity(pathNames)
#' # This creates a consort diagram from any R script (not Rmd). It
#' basically tells you how all the dataframes are related and how many
#' rows each dataframe has so you can see if any data has been lost
#' on the way.


pathName<-paste0(here::here(),"/inst/TemplateGRS/munge/PreProcessing.R")


sanity<-function(pathName){

#source the script first
env <- new.env()
source(pathName, local = env)

###Construct the nodes
listOfdf<-names(Filter(function(x) x, eapply(env, is.data.frame)))
#Create the find and replace dataframe with the number representing each dataframe:
listOfdf<-data.frame(listOfdf,stringsAsFactors = FALSE)
names(listOfdf)<-c("name")

#Get the nrows for all the dataframes and label each dataframe with a number and a name
mySizes<-data.frame(sapply(mget(listOfdf$name, envir = env), nrow),stringsAsFactors = FALSE)
mySizes$name<-row.names(mySizes)
names(mySizes)<-c("size","name")
mySizes<-merge(listOfdf,mySizes,by="name")
mySizes$Number<-rownames(mySizes)


nodes <- create_node_df(n=nrow(mySizes),
                        nodes=mySizes$name,
                        label=stringr::str_wrap(paste0(mySizes$name,":",mySizes$size),5),
                        shape = "rectangle",
                        fontsize=10)




#4. Then extract all lines that have a any dataframe mentioned on both side of a '<-' as that gives the directionality of the edges.
#Extract the arrows.
#read the text of the script in too for the arrow extraction.


#### Construct the edges:

my_data <- read.delim(path,stringsAsFactors = FALSE)
names(my_data)<-"here"
myattrib<-data.frame(my_data[grepl("<-",my_data$here),],stringsAsFactors = FALSE)
names(myattrib)<-"here"
#Exclude commented out lines
myattrib<-data.frame(myattrib[!grepl("#",myattrib$here),],stringsAsFactors = FALSE)
names(myattrib)<-"here"
#Get rid of all numbers so that they dont get confused:
myattrib$here<-gsub("[0-9]+","",myattrib$here)

#Create a colum with each side of the assignment operator:
myattrib$left<-stringr::str_extract(myattrib$here,".*<-")
myattrib$right<-stringr::str_extract(myattrib$here,"<-.*")

#Now lookup which dataframes are related to which.
#Create a named list from this for the lookup:
numberReplaceList<-split(mySizes$Number,paste0(mySizes$name,"[^a-zA-Z]"))
myattrib$left<-DictionaryInPlaceReplace(myattrib$left,numberReplaceList)
myattrib$right<-DictionaryInPlaceReplace(myattrib$right,numberReplaceList)

#Get into numbers only
myattrib$leftNum<-str_extract_all(myattrib$left,"[0-9]")
myattrib$rightNum<-str_extract_all(myattrib$right,"[0-9]")
myattrib<-separate_rows(myattrib, rightNum, convert = TRUE)

myattrib$leftNum<-as.integer(str_extract(myattrib$leftNum,"[1-9]"))
myattrib$rightNum<-as.integer(str_extract(myattrib$rightNum,"[1-9]"))

#Only select columns which have both a left and right:
myattrib<-myattrib[!is.na(myattrib$leftNum),]
myattrib<-myattrib[!is.na(myattrib$rightNum),]

#Remove any row that refers to itself
myattrib<-myattrib[myattrib$leftNum!=myattrib$rightNum,]


#Now contruct the edges:
edges <-
  create_edge_df(
    from = myattrib$rightNum,
    to =   myattrib$leftNum)


g <- create_graph(nodes_df=nodes,
                  edges_df=edges)%>%
  add_global_graph_attrs(
    attr = c("layout", "rankdir", "splines"),
    value = c("dot", "TB", "false"),
    attr_type = c("graph", "graph", "graph"))
render_graph(g)

return(g)

}




