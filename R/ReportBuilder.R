

#' The idea of the package is that it creates a consort diagram using 
#' diagrammeR but all the dataframes are automatically filled with the dataframes
#' in the script. The user just provides a pathname for the script
#' @param pathName The string in the Indication to search for
#' @importFrom here here
#' @import DiagrammeR
#' @importFrom magrittr '%>%'
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_wrap str_extract str_extract_all
#' @keywords consort
#' @export
#' @examples
#' #pathName<-paste0(here::here(),"/inst/TemplateProject/munge/PreProcessing.R")
#' #sanity(pathName)
#' # This creates a consort diagram from any R script (not Rmd). It
#' # basically tells you how all the dataframes are related and how many
#' # rows each dataframe has so you can see if any data has been lost
#' # on the way.


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
                        fontsize=5,
                        label=stringr::str_wrap(paste0(mySizes$name,":",mySizes$size),5),
                        shape = "rectangle")

#4. Then extract all lines that have a any dataframe mentioned on both side of a '<-' as that gives the directionality of the edges.
#Extract the arrows.
#read the text of the script in too for the arrow extraction.


#### Construct the edges:

my_data <- read.delim(pathName,stringsAsFactors = FALSE)
names(my_data)<-"here"
myattrib<-data.frame(my_data[grepl("<-",my_data$here),],stringsAsFactors = FALSE)
names(myattrib)<-"here"
#Exclude commented out lines
myattrib<-data.frame(myattrib[!grepl("#",myattrib$here),],stringsAsFactors = FALSE)
names(myattrib)<-"here"
#Get rid of all numbers so that they dont get confused:
myattrib$here<-gsub("[0-9]+","",myattrib$here)
#And get rid of brackets as seems to confuse things too
myattrib$here<-gsub("\\(","_",myattrib$here)
myattrib$here<-gsub("\\)","_",myattrib$here)

#Create a colum with each side of the assignment operator:
myattrib$left<-str_extract(myattrib$here,".*<-")
myattrib$right<-str_extract(myattrib$here,"<-.*")

#Now lookup which dataframes are related to which.
#Create a named list from this for the lookup:
mySizes$Number<-paste0(mySizes$Number,",")
numberReplaceList<-split(mySizes$Number,paste0(mySizes$name,"[^a-zA-Z]"))

myattrib$left<-DictionaryInPlaceReplace(myattrib$left,numberReplaceList)
myattrib$right<-DictionaryInPlaceReplace(myattrib$right,numberReplaceList)

#Get into numbers only
myattrib$leftNum<-str_extract_all(myattrib$left,"[0-9]+")
myattrib$rightNum<-str_extract_all(myattrib$right,"[0-9]+")
myattrib<-separate_rows(myattrib, rightNum, convert = TRUE)
myattrib<-separate_rows(myattrib, leftNum, convert = TRUE)

myattrib$leftNum<-as.integer(str_extract(myattrib$leftNum,"[0-9]+"))
myattrib$rightNum<-as.integer(str_extract(myattrib$rightNum,"[0-9]+"))

myattrib<-myattrib[grepl("[0-9]+",myattrib$leftNum),]
myattrib<-myattrib[grepl("[0-9]+",myattrib$rightNum),]

#Only select columns which have both a left and right:
myattrib<-myattrib[!is.na(myattrib$leftNum),]
myattrib<-myattrib[!is.na(myattrib$rightNum),]

myattrib<-myattrib[myattrib$leftNum>0,]
myattrib<-myattrib[myattrib$rightNum>0,]

#Remove any row that refers to itself
myattrib<-myattrib[myattrib$leftNum!=myattrib$rightNum,]
myattrib<-data.frame(myattrib$leftNum,myattrib$rightNum,stringsAsFactors = FALSE)
myattrib<-unique(myattrib)
names(myattrib)<-c("leftNum","rightNum")
  
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

}




