########## Lexicons ##########

#' Extract pathology type
#'
#' This standardizes terms to describe the pathology tissue type being exmained
#' @param dataframe dataframe with column of interest
#' @keywords Pathology type
#' @export
#' @examples #No examples as just returns a list



HistolType <- function() {
  
  #First standardise the terms
  
  tissue<-list("Resection" = "Resection", 
               "bx|biopsies"="Biopsy",
               "(endoscopic mucosal resection)|(endoscopic mucosectomy)"="EMR",
               "endoscopic submucosal (dissection|resection)"="ESD",
               "nodul"="nodul",
               "polyp "="polyp")
  
  
  #To get the list as a list of values only in a regex use
  #paste0(unlist(HistolType(),use.names=F),collapse="|")
  
  return(tissue)
}



#' Use list of standard locations
#'
#' The is a list of standard locations at endoscopy that is used in the TermStandardLocator as well
#' as extraction of the site of biopsies/EMRs and potentially in functions looking at the site of a 
#' therapeutic event. It just returns the list in the function
#'
#'
#' @keywords Location
#' @export
#' @examples #No example needed

LocationList<-function(){
  
  All<-append(LocationListLower(), LocationListUpper())
  All<-append(All,LocationListUniversal())
  
  #To get the list as a list of values only in a regex use
  #paste0(unlist(All,use.names=F),collapse="|")
  
  return(All)
  
}

#' Use list of standard locations for upper GI endoscopy
#'
#' The is a list of standard locations at endoscopy that is used in the TermStandardLocator as well
#' as extraction of the site of biopsies/EMRs and potentially in functions looking at the site of a 
#' therapeutic event. It just returns the list in the function
#'
#'
#' @keywords Location
#' @export
#' @examples #No example needed

LocationListUpper<-function(){
  
  
  upper<-list("duodenum|d2|D2|duodenal" = "Duodenum", 
              "gastric|stomach"="Stomach",
              "antrum|antral"="Antrum",
              "ogj|goj|gastrooesophageal"="GOJ",
              "fundal|fundus"="stomach",
              "pyloric"="Pylorus",
              "gastric cardia"="Cardia",
              "oesophagus|oesophageal"="Oesophageal")
  
  
  #To get the list as a list of values only in a regex use
  #paste0(unlist(upper,use.names=F),collapse="|")
  
  return(upper)
  
}

#' Use list of standard locations for upper GI endoscopy
#'
#' The is a list of standard locations at endoscopy that is used in the TermStandardLocator as well
#' as extraction of the site of biopsies/EMRs and potentially in functions looking at the site of a 
#' therapeutic event. It just returns the list in the function
#'
#'
#' @keywords Location
#' @export
#' @examples #No example needed

LocationListUniversal<-function(){
  universal<-list("Anastomosis" = "Ascending ")
  return(universal)
  
  #If need to use then use
  #paste0(unlist(universal,use.names=F),collapse="|")
  
}


#' Use list of standard locations for lower GI endoscopy
#'
#' The is a list of standard locations at endoscopy that is used in the TermStandardLocator as well
#' as extraction of the site of biopsies/EMRs and potentially in functions looking at the site of a 
#' therapeutic event. It just returns the list in the function
#'
#'
#' @keywords Location
#' @export
#' @examples #No example needed

LocationListLower<-function(){
  
  lower<-list("asce |ascending|(ascend[^a-z])|( colon r )|(r colon)|(asc )|(right colon)" = "Ascending ",
              "descending|(descen[^a-z])|(desc[^a-z])|(des[^a-z])|(colon l)|(l colon)|(left colon)" = "Descending ",
              "sigmoid|(sigm[^a-z])|sigmo "= "Sigmoid ",
              "rectal|rectum|(rectum[a-z])|rect "="Rectum ",
              "transverse|(transv[^a-z])|tranv |trans "="Transverse ",
              "caecum|caecal"="Caecum ",
              "splenic"="Splenic ",
              "(ileum )|( ileal )"="Ileum ",
              "rectosigmoid"="Rectosigmoid ",
              "(ileocaecal\\s*)|icv|(ileo-caecum)"="Ileocaecal", 
              "(hep[^a-z])|([Hh]epatic)"="Hepatic ",
              "colonic|colon |(col[^a-z])"="Colon ",
              "term |terminal"="Terminal",
              "TI "="Terminal",
              "caec"="Caecum ",
              "[Ss]ig "="Sigmoid ",
              "ileo\\s*-\\s*anal|ileo\\s*anal "="Ileoanal ",
              "(pre\\s*pouch)|(pre-[Pp]ouch)"="PrePouch",
              "pouch"="Pouch",
              "term |terminal"="Terminal")
  
  
  #To get the list as a list of values only in a regex use
  #paste0(unlist(lower,use.names=F),collapse="|")
  
  return(lower)
  
}


#' Use list of catheters used in radiofrequency ablation
#'
#' The takes a list of catheters used in radiofrequency ablation
#'
#' @keywords RFA
#' @export
#' @examples #No example needed

RFACath<-function(){
  
  tofind <-
    paste(
      c(
        "90","360","HALO60"," 60","TTS",
        "[Cc]hannel","APC"
      ),
      collapse = "|"
    )
  
  return(tofind)
  
}

#' Event list
#'
#' This function returns all the conversions from common version of events to 
#' a standardised event list, much like the Location standardidastion function
#' It is used in the Barretts_EventType. This does not include EMR as this is 
#' extracted from the pathology so is part of pathology type.
#' @keywords Event extraction
#' @examples # unique(unlist(EventList(), use.names = FALSE))
#' 
EventList<-function(){
  
  Event <- list("radiofrequency ablation" = "RFA", 
                "(argon plasma coagulation)|apc" = "APC",
                "halo| rfa" = "RFA",
                "dilatation|dilated"="dilat",
                " emr"="EMR",
                "(endoscopic mucosal resection)|(endoscopic mucosectomy)"="EMR",
                "clip"="clip",
                "grasp"="grasp",
                "iodine"="iodine", 
                "acetic"="acetic",
                "NAC"="NAC",
                "Brushings"="brushings"
  )
  
  #To get the list as a list of values only in a regex use
  #paste0(unlist(Event,use.names=F),collapse="|")
  
  return(Event)
}


#' Biopsy index list
#'
#' This function returns all the conversions from common version of events to 
#' a standardised event list, much like the Location standardidastion function
#' It is used in the Barretts_EventType. This does not include EMR as this is 
#' extracted from the pathology so is part of pathology type.
#' @keywords Event extraction
#' @examples # unique(unlist(EventList(), use.names = FALSE))
#' 
BiopsyIndex<-function(){
  
  BiopsyIndexList <- list("ileum:biopsy"="C11",
                          "ileocaecal:biopsy"="C10",
                          "caecum:biopsy"="C9",
                          "ascending:biopsy"="C8",
                          "hepatic:biopsy"="C7",
                          "transverse:biopsy"="C6",
                          "splenic:biopsy"="C5",
                          "descending:biopsy"="C4",
                          "sigmoid:biopsy"="C3",
                          "rectosigmoid:biopsy"="C2",
                          "rectum:biopsy"="C1",
                          "ileoanal:biopsy"="S1",
                          "prepouch:biopsy"="S2",
                          "pouch:biopsy"="S3",
                          "duodenum:biopsy"="O5",
                          "antrum:biopsy"="O4",
                          "stomach:biopsy"="O3",
                          "goj:biopsy"="O1",
                          "cardia:biopsy"="O2",
                          "oesophagus:biopsy"="O1",
                          "colon:biopsy"="colon",
                          "oesophagus:emr"="O1",
                          "goj:emr"="O1",
                          "stomach:emr"="O3",
                          "duodenum:emr"="O5")
  
  #To get the list as a list of values only in a regex use
  #paste0(unlist(BiopsyIndex,use.names=F),collapse="|")
  
  return(BiopsyIndexList)
}



#' GISymptoms
#'
#' This function returns all the Common GI symptoms. They are simply listed as is without grouping
#' or mapping. They have been derived from a manual list with synonyms derived from the UMLS Methatharus
#' using the browser.
#' @keywords Event extraction
#' @examples # unique(unlist(EventList(), use.names = FALSE))
#' 
GISymptomsList<-function(){
  
  Symptoms <- list("abdomen pain"="abdomen pain",
                     "abdomen; pain, epigastric"="abdomen; pain, epigastric",
                     "abdominal pain"="abdominal pain",
                     "abdominal pain epigastric"="abdominal pain epigastric",
                     "abdominal pain in the central upper belly"="abdominal pain in the central upper belly",
                     "abdominal pain nos"="abdominal pain nos",
                     "abdominal pain, epigastric"="abdominal pain, epigastric",
                     "abdominal pain, unspecified site"="abdominal pain, unspecified site",
                     "abdominal pains"="abdominal pains",
                     "abdominal wind"="abdominal wind",
                     "abdominal; pain"="abdominal; pain",
                     "abdominalgia"="abdominalgia",
                     "acid reflux"="acid reflux",
                     "acidity"="acidity",
                     "alteration in bowel elimination: incontinence"="alteration in bowel elimination: incontinence",
                     "altered blood in stool"="altered blood in stool",
                     "altered blood passed per rectum"="altered blood passed per rectum",
                     "anal incontinence"="anal incontinence",
                     "anal pain"="anal pain",
                     "anus pain"="anus pain",
                     "anus pains"="anus pains",
                     "anus; pain"="anus; pain",
                     "ap - abdominal pain"="ap - abdominal pain",
                     "bad breath"="bad breath",
                     "bad breath - halitosis"="bad breath - halitosis",
                     "bad breath halitosis"="bad breath halitosis",
                     "bad or foul breath"="bad or foul breath",
                     "bad/foul;breath"="bad/foul;breath",
                     "being sick"="being sick",
                     "belch"="belch",
                     "belching"="belching",
                     "belching symptom"="belching symptom",
                     "belching symptoms"="belching symptoms",
                     "belching, burping"="belching, burping",
                     "belchings"="belchings",
                     "bellyache"="bellyache",
                     "black stool"="black stool",
                     "black stools"="black stools",
                     "black, tarry stool"="black, tarry stool",
                     "bleeding per rectum"="bleeding per rectum",
                     "bleeding rectal"="bleeding rectal",
                     "bleeding rectum"="bleeding rectum",
                     "bleeding-rectal"="bleeding-rectal",
                     "bleeding;anal"="bleeding;anal",
                     "bleeding;rectal"="bleeding;rectal",
                     "blood per rectum"="blood per rectum",
                     "blood; vomiting"="blood; vomiting",
                     "bowel incontinena"="bowel incontinena",
                     "bowel incontinence"="bowel incontinence",
                     "bowel incontinent"="bowel incontinent",
                     "bowel loose movements"="bowel loose movements",
                     "bowels loose movement"="bowels loose movement",
                     "bowels: incontinent"="bowels: incontinent",
                     "brash"="brash",
                     "breath odor"="breath odor",
                     "breath odor abnormal"="breath odor abnormal",
                     "breath odor nos"="breath odor nos",
                     "breath odors"="breath odors",
                     "breath odour"="breath odour",
                     "breath odour nos"="breath odour nos",
                     "breath smells offensive"="breath smells offensive",
                     "breath smells unpleasant"="breath smells unpleasant",
                     "breath; foul"="breath; foul",
                     "burning reflux"="burning reflux",
                     "burp"="burp",
                     "burping"="burping",
                     "burps"="burps",
                     "c/o - cough"="c/o - cough",
                     "can't get food down"="can't get food down",
                     "cannot get food down"="cannot get food down",
                     "chest pain"="chest pain",
                     "chest pain nos"="chest pain nos",
                     "chest pain or discomfort"="chest pain or discomfort",
                     "chest pain or discomfort reported as pain"="chest pain or discomfort reported as pain",
                     "chest pain, nos"="chest pain, nos",
                     "chest pain, pleuritic"="chest pain, pleuritic",
                     "chest pain, unspecified"="chest pain, unspecified",
                     "chest pains"="chest pains",
                     "chest; pain"="chest; pain",
                     "clear throat"="clear throat",
                     "clearing throat"="clearing throat",
                     "clearings throat"="clearings throat",
                     "complaining of cough"="complaining of cough",
                     "cough"="cough",
                     "cough symptom"="cough symptom",
                     "cough symptoms"="cough symptoms",
                     "cough, nos"="cough, nos",
                     "coughing"="coughing",
                     "coughing - function"="coughing - function",
                     "coughing symptom"="coughing symptom",
                     "coughs"="coughs",
                     "d - diarrhea"="d - diarrhea",
                     "d - diarrhoea"="d - diarrhoea",
                     "dark stools"="dark stools",
                     "deglutition disorder"="deglutition disorder",
                     "deglutition disorders"="deglutition disorders",
                     "diarrhea"="diarrhea",
                     "diarrhea nos"="diarrhea nos",
                     "diarrhea running"="diarrhea running",
                     "diarrhea symptom"="diarrhea symptom",
                     "diarrhea symptoms"="diarrhea symptoms",
                     "diarrhea, nos"="diarrhea, nos",
                     "diarrhea, unspecified"="diarrhea, unspecified",
                     "diarrheas"="diarrheas",
                     "diarrhoea"="diarrhoea",
                     "diarrhoea nos"="diarrhoea nos",
                     "diarrhoea symptom"="diarrhoea symptom",
                     "diarrhoea symptoms"="diarrhoea symptoms",
                     "difficult swallowing"="difficult swallowing",
                     "difficult; swallowing"="difficult; swallowing",
                     "difficulties swallowing"="difficulties swallowing",
                     "difficulty in swallowing"="difficulty in swallowing",
                     "difficulty in swallowing nos"="difficulty in swallowing nos",
                     "difficulty swallowing"="difficulty swallowing",
                     "difficulty;swallowing"="difficulty;swallowing",
                     "disorder deglutition"="disorder deglutition",
                     "disorders, deglutition"="disorders, deglutition",
                     "dry heave"="dry heave",
                     "dry heaves"="dry heaves",
                     "dysphagia"="dysphagia",
                     "dysphagia, nos"="dysphagia, nos",
                     "dysphagia, unspecified"="dysphagia, unspecified",
                     "dysphagias"="dysphagias",
                     "emesis"="emesis",
                     "emesis bloody"="emesis bloody",
                     "epigastralgia"="epigastralgia",
                     "epigastric abdominal pain"="epigastric abdominal pain",
                     "epigastric ache"="epigastric ache",
                     "epigastric pain"="epigastric pain",
                     "epigastric; pain"="epigastric; pain",
                     "epigastrium pain"="epigastrium pain",
                     "eructate"="eructate",
                     "eructation"="eructation",
                     "eructation, function"="eructation, function",
                     "eructations"="eructations",
                     "excessive farting"="excessive farting",
                     "excessive passing of gas"="excessive passing of gas",
                     "faecal incontinence"="faecal incontinence",
                     "fart"="fart",
                     "farting"="farting",
                     "farts"="farts",
                     "faucitis"="faucitis",
                     "fecal incontinence"="fecal incontinence",
                     "fecal incontinence nos"="fecal incontinence nos",
                     "feces incontinence"="feces incontinence",
                     "feces incontinence of"="feces incontinence of",
                     "feces; incontinence"="feces; incontinence",
                     "fetor ex ore"="fetor ex ore",
                     "fetor oris"="fetor oris",
                     "finding of vomiting"="finding of vomiting",
                     "flatulence"="flatulence",
                     "flatulence symptom"="flatulence symptom",
                     "flatus"="flatus",
                     "flatus, function"="flatus, function",
                     "foetor ex ore"="foetor ex ore",
                     "foul breath"="foul breath",
                     "foul; breath"="foul; breath",
                     "frequent stools"="frequent stools",
                     "full of wind"="full of wind",
                     "gas passing"="gas passing",
                     "gas raising"="gas raising",
                     "gaseous regurgitation"="gaseous regurgitation",
                     "gaseousness"="gaseousness",
                     "gastric contents; regurgitation"="gastric contents; regurgitation",
                     "gastrointestinal disorder hematemesis"="gastrointestinal disorder hematemesis",
                     "gut pain"="gut pain",
                     "haematemesis"="haematemesis",
                     "haematemesis/vomiting blood"="haematemesis/vomiting blood",
                     "haemorrhage of rectum"="haemorrhage of rectum",
                     "haemorrhage rectal"="haemorrhage rectal",
                     "haemorrhage rectum"="haemorrhage rectum",
                     "halitoses"="halitoses",
                     "halitosis"="halitosis",
                     "heart burn"="heart burn",
                     "heartburn"="heartburn",
                     "heartburn symptom"="heartburn symptom",
                     "heaving"="heaving",
                     "hematemeses"="hematemeses",
                     "hematemesis"="hematemesis",
                     "hematemesis was observed"="hematemesis was observed",
                     "hematemesis/vomiting blood"="hematemesis/vomiting blood",
                     "hemorrhage of rectum"="hemorrhage of rectum",
                     "hemorrhage rectal"="hemorrhage rectal",
                     "hemorrhage rectum"="hemorrhage rectum",
                     "hemorrhage; rectum"="hemorrhage; rectum",
                     "incontinence (fecal)"="incontinence (fecal)",
                     "incontinence bowel"="incontinence bowel",
                     "incontinence faecal"="incontinence faecal",
                     "incontinence fecal"="incontinence fecal",
                     "incontinence of bowel"="incontinence of bowel",
                     "incontinence of faeces"="incontinence of faeces",
                     "incontinence of feces"="incontinence of feces",
                     "incontinence of feces (finding)"="incontinence of feces (finding)",
                     "incontinence of stool"="incontinence of stool",
                     "incontinence stool"="incontinence stool",
                     "incontinence, bowel"="incontinence, bowel",
                     "incontinence, fecal"="incontinence, fecal",
                     "incontinence; fecal"="incontinence; fecal",
                     "incontinence;bowel"="incontinence;bowel",
                     "incontinence;faeces"="incontinence;faeces",
                     "incontinent bowel"="incontinent bowel",
                     "incontinent of bowels"="incontinent of bowels",
                     "incontinent of faeces"="incontinent of faeces",
                     "incontinent of feces"="incontinent of feces",
                     "incontinent of stool"="incontinent of stool",
                     "intestinal gas excretion"="intestinal gas excretion",
                     "involuntary stool"="involuntary stool",
                     "loose bowel motion"="loose bowel motion",
                     "loose bowel motions"="loose bowel motions",
                     "loose bowel movement"="loose bowel movement",
                     "loose stools"="loose stools",
                     "loss of bowel control"="loss of bowel control",
                     "melaena"="melaena",
                     "melena"="melena",
                     "melena (disorder)"="melena (disorder)",
                     "melena/black tarry stools"="melena/black tarry stools",
                     "melenas"="melenas",
                      "nausea"="nausea",
                     "nonspecific chest pain"="nonspecific chest pain",
                     "observation of cough"="observation of cough",
                     "observation of diarrhea"="observation of diarrhea",
                     "observation of diarrhoea"="observation of diarrhoea",
                     "observation of vomiting"="observation of vomiting",
                     "odor breath"="odor breath",
                     "odor of breath"="odor of breath",
                     "odor of breath fetor oris"="odor of breath fetor oris",
                     "odour breath"="odour breath",
                     "of bowel incontinence"="of bowel incontinence",
                     "ozostomia"="ozostomia",
                     "pain abdominal"="pain abdominal",
                     "pain anal"="pain anal",
                     "pain anus"="pain anus",
                     "pain chest"="pain chest",
                     "pain epigastric"="pain epigastric",
                     "pain in chest"="pain in chest",
                     "pain in stomach"="pain in stomach",
                     "pain in the pharynx"="pain in the pharynx",
                     "pain in throat"="pain in throat",
                     "pain pharynx"="pain pharynx",
                     "pain pleural"="pain pleural",
                     "pain thoracic"="pain thoracic",
                     "pain throat"="pain throat",
                     "pain, abdominal"="pain, abdominal",
                     "pain, chest"="pain, chest",
                     "pain, pleuritic"="pain, pleuritic",
                     "pain; abdominal"="pain; abdominal",
                     "pain; anus"="pain; anus",
                     "pain; chest"="pain; chest",
                     "pain; epigastric"="pain; epigastric",
                     "pain; pharynx"="pain; pharynx",
                     "pain; pleura"="pain; pleura",
                     "pain; thorax"="pain; thorax",
                     "pain; throat"="pain; throat",
                     "pain;abdominal"="pain;abdominal",
                     "pain;anal"="pain;anal",
                     "pain;chest"="pain;chest",
                     "pain;epigastric"="pain;epigastric",
                     "pain;throat"="pain;throat",
                     "pains, abdominal"="pains, abdominal",
                     "pains, chest"="pains, chest",
                     "pass wind"="pass wind",
                     "passage of gas by anus"="passage of gas by anus",
                     "passing flatus"="passing flatus",
                     "passing wind"="passing wind",
                     "pharyngeal pain"="pharyngeal pain",
                     "pharynx discomfort"="pharynx discomfort",
                     "pharynx; pain"="pharynx; pain",
                     "pleura; pain"="pleura; pain",
                     "pleural pain"="pleural pain",
                     "pleuralgia"="pleuralgia",
                     "pleuritic chest pain"="pleuritic chest pain",
                     "pleuritic pain"="pleuritic pain",
                     "pleurodynia"="pleurodynia",
                     "pleurodynia nos"="pleurodynia nos",
                     "poor swallowing"="poor swallowing",
                     "pr - bleeding per rectum"="pr - bleeding per rectum",
                     "pr - blood per rectum"="pr - blood per rectum",
                     "prb - rectal bleeding"="prb - rectal bleeding",
                     "proctorrhagia"="proctorrhagia",
                     "pyroses"="pyroses",
                     "pyrosis"="pyrosis",
                     "rb - rectal bleeding"="rb - rectal bleeding",
                     "rectal bleed"="rectal bleed",
                     "rectal bleeding"="rectal bleeding",
                     "rectal blood loss"="rectal blood loss",
                     "rectal discharge incontinent of stool"="rectal discharge incontinent of stool",
                     "rectal discharge incontinent of stool (physical finding)"="rectal discharge incontinent of stool (physical finding)",
                     "rectal haemorrhage"="rectal haemorrhage",
                     "rectal hemorrhage"="rectal hemorrhage",
                     "rectal pain"="rectal pain",
                     "rectorrhagia"="rectorrhagia",
                     "rectum bleeding"="rectum bleeding",
                     "rectum hemorrhage"="rectum hemorrhage",
                     "rectum; hemorrhage"="rectum; hemorrhage",
                     "reflux"="reflux",
                     "reflux, nos"="reflux, nos",
                     "refluxed"="refluxed",
                     "refluxing"="refluxing",
                     "refluxs"="refluxs",
                     "regurgitation"="regurgitation",
                     "regurgitation of gastric content"="regurgitation of gastric content",
                     "regurgitation; gastric contents"="regurgitation; gastric contents",
                     "reported chest pain"="reported chest pain",
                     "retch"="retch",
                     "retching"="retching",
                     "rndx bowel incontinence"="rndx bowel incontinence",
                     "rndx bowel incontinence (diagnosis)"="rndx bowel incontinence (diagnosis)",
                     "rndx diarrhea"="rndx diarrhea",
                     "ructus"="ructus",
                     "runs"="runs",
                     "smelly breath"="smelly breath",
                     "sore symptom throat"="sore symptom throat",
                     "sore symptoms throat"="sore symptoms throat",
                     "sore throat"="sore throat",
                     "sore throat nos"="sore throat nos",
                     "sore throat symptom"="sore throat symptom",
                     "sore throat, nos"="sore throat, nos",
                     "sore throat;"="sore throat;",
                     "sore throats"="sore throats",
                     "sorethroat"="sorethroat",
                     "spitting up"="spitting up",
                     "stomach pain"="stomach pain",
                     "stomatodysodia"="stomatodysodia",
                     "stool black"="stool black",
                     "stool color black"="stool color black",
                     "stool incontinence"="stool incontinence",
                     "stool soiling"="stool soiling",
                     "stool tarry"="stool tarry",
                     "stool(s);black"="stool(s);black",
                     "stool(s);tarry"="stool(s);tarry",
                     "stools loose"="stools loose",
                     "stools.incontinent"="stools.incontinent",
                     "swallowing difficult"="swallowing difficult",
                     "swallowing difficulties"="swallowing difficulties",
                     "swallowing difficulty"="swallowing difficulty",
                     "swallowing disorder"="swallowing disorder",
                     "swallowing disorders"="swallowing disorders",
                     "swallowing; difficult"="swallowing; difficult",
                     "symptoms diarrhea"="symptoms diarrhea",
                     "symptoms vomiting"="symptoms vomiting",
                     "tarry stool"="tarry stool",
                     "tarry stools"="tarry stools",
                     "the runs"="the runs",
                     "the trots"="the trots",
                     "thoracalgia"="thoracalgia",
                     "thoracic pain"="thoracic pain",
                     "thoracodynia"="thoracodynia",
                     "thorax pain"="thorax pain",
                     "thorax painful"="thorax painful",
                     "thorax; pain"="thorax; pain",
                     "throat clearing"="throat clearing",
                     "throat discomfort"="throat discomfort",
                     "throat pain"="throat pain",
                     "throat sore"="throat sore",
                     "throat soreness"="throat soreness",
                     "throat, sore"="throat, sore",
                     "throw up"="throw up",
                     "throwing up"="throwing up",
                     "unable to restrain bowel movement"="unable to restrain bowel movement",
                     "unable to restrain bowel movement (symptom)"="unable to restrain bowel movement (symptom)",
                     "unspecified abdominal pain"="unspecified abdominal pain",
                     "unspecified chest pain"="unspecified chest pain",
                     "vomit"="vomit",
                     "vomited"="vomited",
                     "vomiting"="vomiting",
                     "vomiting blood"="vomiting blood",
                     "vomiting disorder"="vomiting disorder",
                     "vomiting nos"="vomiting nos",
                     "vomiting of blood"="vomiting of blood",
                     "vomiting out blood"="vomiting out blood",
                     "vomiting symptom"="vomiting symptom",
                     "vomiting symptoms"="vomiting symptoms",
                     "vomiting was observed"="vomiting was observed",
                     "vomiting, nos"="vomiting, nos",
                     "vomiting, unspecified"="vomiting, unspecified",
                     "vomiting; blood"="vomiting; blood",
                     "vomiting;blood"="vomiting;blood",
                     "vomits"="vomits",
                     "vomitting blood"="vomitting blood",
                     "vomiturition"="vomiturition",
                     "waterbrash"="waterbrash",
                     "waterbrash symptom"="waterbrash symptom",
                     "watery stool"="watery stool",
                     "watery stools"="watery stools",
                     "wind"="wind",
                     "wind - flatus"="wind - flatus",
                     "wind symptom"="wind symptom",
                     "winding"="winding",
                   "c/o - nausea"="c/o - nausea",
                   "feel sick"="feel sick",
                   "feeling bilious"="feeling bilious",
                   "feeling queasy"="feeling queasy",
                   "feeling sick"="feeling sick",
                   "nausea nos"="nausea nos",
                   "nausea symptom"="nausea symptom",
                   "nausea symptoms"="nausea symptoms",
                   "nausea"="nausea",
                   "nauseas"="nauseas",
                   "nauseated"="nauseated",
                   "nauseating"="nauseating",
                   "nauseous"="nauseous",
                   "observation of nausea"="observation of nausea",
                   "queasy"="queasy",
                   "rndx nausea"="rndx nausea",
                   "sick feeling"="sick feeling",
                   "symptom nausea"="symptom nausea",
                   "symptoms nausea"="symptoms nausea")
  
  #To get the list as a list of values only in a regex use
  #paste0(unlist(WordsToNumbers,use.names=F),collapse="|")
  
  return(Symptoms)
}


#' Words to numbers
#'
#' This function returns all the conversions from common version of events to 
#' a standardised event list, much like the Location standardidastion function
#' It is used in the Barretts_EventType. This does not include EMR as this is 
#' extracted from the pathology so is part of pathology type.
#' @keywords Event extraction
#' @examples # unique(unlist(EventList(), use.names = FALSE))
#' 
WordsToNumbers<-function(){
  
  NumberConv <- list("[Oo]ne "="1",
                     "[Ss]ingle "="1",
                     "[Tt]wo "="2",
                     "[Tt]hree "= "3",
                     "[Ff]our "= "4",
                     "[Ff]ive "= "5",
                     "[Ss]ix "="6",
                     "[Ss]even "= "7",
                     "[Ee]ight "= "8",
                     "[Nn]ine "= "9",
                     "[Tt]en "= "10",
                     "[Ee]leven "= "11",
                     "[Tt]welve "= "12",
                     "[Tt]hirteen "= "13",
                     "[Ff]ourteen "= "14",
                     "[Ff]ifteen "= "15",
                     "[Ss]ixteen "= "16"
                     )
  
  #To get the list as a list of values only in a regex use
  #paste0(unlist(WordsToNumbers,use.names=F),collapse="|")
  
  return(NumberConv)
}

#' #' Validate columns
#' #'
#' #' This is an attempt to determine whether functions are accurate in their extraction
#' #' It is part of a validation pipeline to make sure the data outputted from a function
#' #' is accurate
#' #' It essentially is a comparison of actual vs expected output. The intention is to 
#' #' then quantify the output of a function for a specific dataset.
#' #'
#' #' @param dataframe dataframe
#' #' @param testcolumn The dataframe column to run the function on
#' #' @param pHospitalNum the hospital number of the patient so can be identified
#' #' @importFrom openxlsx write.xlsx
#' #' @keywords validation
#' #' @export
#' #' @examples #ValidationR(HistolBxSize,myHistol$histology_report)
#' 
#' sensAndSpecificMultip<-function(FUN =funct,dataframe,Column,pHospitalNum){
#'   dd<-funct(dataframe,Column)
#'   #Now just want the columns that have changed or have been added from the original as well
#'   #as well as the columns that sound very similar to the original
#'   #ie need to get the original column called Column as well as the new column created (can
#'   # I get this from the diff between two data frames? dd-dataframe??)
#'   #This gets the new column name from the dataframe that the function has run
#'   MyNegsFiltered$FindingsAlgoNegs<-as.list(strsplit(MyNegsFiltered$FindingsAlgoNegs, ";"))
#'   MyNegsFiltered$negativeresults<-as.list(strsplit(MyNegsFiltered$negativeresults, ";"))
#'   #pHospitalNum
#'   ff<-setdiff(union(names(dataframe), names(dd)), names(dataframe))
#'   ff<-data.frame(dd[,Column],dd[,ff],dd[,pHospitalNum])
#'   write.xlsx(ff, paste0(getwd(),"/MyValidation.xlsx"))
#' }
#' #dataframe[,pHospitalNum],


#' #' Validate columns
#' #'
#' #' This is an attempt to determine whether functions are accurate in their extraction
#' #' It is part of a validation pipeline to make sure the data outputted from a function
#' #' is accurate
#' #' It essentially is a comparison of actual vs expected output where there are multiple 
#' #' outputs in the column
#' #' The intention is to then quantify the output of a function for a specific dataset.
#' #'
#' #' @param dataframe dataframe
#' #' @param testcolumn The dataframe column to run the function on
#' #' @param pHospitalNum the hospital number of the patient so can be identified
#' #' @importFrom openxlsx write.xlsx
#' #' @keywords validation
#' #' @export
#' #' @examples #ValidationR(HistolBxSize,myHistol$histology_report)
#' 
#' ValidationR<-function(FUN =funct,dataframe,Column,pHospitalNum){
#'   dd<-funct(dataframe,Column)
#'   #Now just want the columns that have changed or have been added from the original as well
#'   #as well as the columns that sound very similar to the original
#'   #ie need to get the original column called Column as well as the new column created (can
#'   # I get this from the diff between two data frames? dd-dataframe??)
#'   #This gets the new column name from the dataframe that the function has run
#'   MyNegsFiltered$FindingsAlgoNegs<-as.list(strsplit(MyNegsFiltered$FindingsAlgoNegs, ";"))
#'   MyNegsFiltered$negativeresults<-as.list(strsplit(MyNegsFiltered$negativeresults, ";"))
#'   #pHospitalNum
#'   ff<-setdiff(union(names(dataframe), names(dd)), names(dataframe))
#'   ff<-data.frame(dd[,Column],dd[,ff],dd[,pHospitalNum])
#'   write.xlsx(ff, paste0(getwd(),"/MyValidation.xlsx"))
#' }
#' #dataframe[,pHospitalNum],
#' 
