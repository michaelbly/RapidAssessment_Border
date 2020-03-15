#IMPORT KOBO FILE TO GENERATE DUMMY DATA
kobochoices<-read.xlsx("input/KoboFile_border.xlsx", sheet = "choices")
koboquestions <-read.xlsx("input/KoboFile_border.xlsx", sheet="survey")
#dummy <- as.data.frame(xlsform_fill(koboquestions, kobochoices,200))
raw_data_first <- read.xlsx("Input/raw_cleaned_dataset_border_2.xlsx")


#IMPORT DAP 
dap<-read.xlsx("input/dap.xlsx", sheet = 1)


#IMPORT QUESTIONNAIRE
questionnaire<-load_questionnaire(data = dap,
                                  questions = koboquestions,
                                  choices = kobochoices,
                                  choices.label.column.to.use = "label::English")



#EXPORT LIST WITH THE NAMES OF THE VARIABLES FOR THE SELECT MULTIPLE (TO BE ADDED TO THE DAP)
#names(raw_data_first)
#vec <-as.vector(names(raw_data_first[,c(195:209, 211:222, 183:193)]))
#names_multiple<- as.data.frame(vec)
#write.csv(names_multiple, "output/names_multiple.csv")

