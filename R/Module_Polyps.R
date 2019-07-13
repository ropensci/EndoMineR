########################### Diagnostic yield functions #######

#' Create GRS metrics by endoscopist (X-ref with pathology)
#'
#' This extracts the polyps types from the data
#' (for colonoscopy and flexible sigmoidosscopy data)
#' and outputs the adenoma,adenocarcinoma and
#' hyperplastic detection rate by endoscopist as well
#' as overall number of colonoscopies.
#' This will be extended to other GRS outputs in the future.
#' @param dataframe The dataframe
#' @param ProcPerformed The column containing the Procedure type performed
#' @param Endo_Endoscopist column containing the Endoscopist name
#' @param Dx The column with the Histological diagnosis
#' @param Histol The column with the Histology text in it
#' @importFrom dplyr group_by_ do full_join
#' @keywords Withdrawal
#' @export
#' @examples
#' nn <- GRS_Type_Assess_By_Unit(
#'   vColon, "ProcedurePerformed",
#'   "Endoscopist", "Diagnosis", "Histology"
#' )
GRS_Type_Assess_By_Unit <-
  function(dataframe,
             ProcPerformed,
             Endo_Endoscopist,
             Dx,
             Histol) {
    dataframe <- data.frame(dataframe)
    dataframe <- dataframe[grepl("[Cc]olonoscopy", dataframe[, ProcPerformed]), ]

    # Function should get proportions of a procedure that result in a finding:

    Adenoma <- dataframe %>% group_by_(Endo_Endoscopist) %>% summarise(Adenoma = (sum(grepl("[Aa]denoma", Histol)) / dplyr::n()) * 100)
    Adenocarcinoma <- dataframe %>% group_by_(Endo_Endoscopist) %>% summarise(Adenocarcinoma = (sum(grepl(".*denoca.*", Histol)) / dplyr::n()) * 100)
    HGD <- dataframe %>% group_by_(Endo_Endoscopist) %>% summarise(HGD = (sum(grepl(".*[Hh]igh [Gg]rade.*", Histol)) / dplyr::n()) * 100)
    LGD <- dataframe %>% group_by_(Endo_Endoscopist) %>% summarise(LGD = (sum(grepl(".*[Ll]ow [Gg]rade.*", Histol)) / dplyr::n()) * 100)
    Serrated <- dataframe %>% group_by_(Endo_Endoscopist) %>% summarise(Serrated = (sum(grepl(".*[Ss]errated.*", Histol)) / dplyr::n()) * 100)
    Hyperplastic <- dataframe %>% group_by_(Endo_Endoscopist) %>% summarise(Hyperplastic = (sum(grepl(".*yperplastic.*", Histol)) / dplyr::n()) * 100)

    FinalTable <-
      full_join(Adenoma, Adenocarcinoma, by = Endo_Endoscopist)
    FinalTable <-
      full_join(FinalTable, HGD, by = Endo_Endoscopist)
    FinalTable <-
      full_join(FinalTable, LGD, by = Endo_Endoscopist)
    FinalTable <-
      full_join(FinalTable, Serrated, by = Endo_Endoscopist)
    FinalTable <-
      full_join(FinalTable, Hyperplastic, by = Endo_Endoscopist)

    # Need to add the total colonoscopy count in here
    FinalTable <- data.frame(FinalTable)
    return(FinalTable)
  }
