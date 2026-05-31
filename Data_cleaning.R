library(readxl)
library(dplyr)
library(ggplot2)
data<-read.csv("C:/Users/DELL/OneDrive/Desktop/Student personality DS_project/Dataset/_BCA_ BTech Degree Efficiency vs Student Personality Fit Questionnaire  (Responses) - Form responses 1 (1).csv")
head(data)
View(data)
str(data)
summary(data) 
dim(data)
colnames(data)
ncol(data) 
data_original <- data
#----------Data Cleaning & preparation--------------------

#---removing the timestamp extra column from dataset
data <- data[, !(colnames(data) == "Timestamp")]
View(data)
#---------Renaming the columns
colnames(data) <- c(
  "Email.address",
  "Student_Name",
  "Student_Age",
  "Course_year",
  "Student_CGPA",
  "Struggle_with_subjects",
  "Practical_confidence",
  "Coding_without_help",
  "Academic_satisfaction",
  "Solving_logical_analytical_problem",
  "Understanding_memorizing",
  "Task_focus",
  "Curiosity_for_technologies",
  "Beyond_classroom",
  "Study_schedule",
  "Completing_assignment",
  "Anxiety_towards_errors",
  "Academicpresurre_learning_relation",
  "Asking_doubts",
  "Group_learning",
  "Personality_Course_learning",
  "Programming_Student_personality",
  "Theoretical_practical",
  "Learning_environment",
  "Task_motivation",
  "Difficult_aspect_ofprogram",
  "In_face_of_difficulty",
  "Pace_of_curriculum",
  "Personality_match_with_program",
  "Choice",
  "MBTI",
  "Opinion",
  "Learning_style",
  "Student_Domain",
  "Employability_confidence"
)  

View(data)
#--------Checking for missing values
colSums(is.na(data))  
#--------Replacing missing numeric values with the mean value in the dataset--------
data$Struggle_with_subjects[is.na(data$Struggle_with_subjects)] <- mean(data$Struggle_with_subjects, na.rm = TRUE)
data$Personality_Course_learning[is.na(data$Personality_Course_learning)] <- mean(data$Personality_Course_learning, na.rm = TRUE)
data$Programming_Student_personality[is.na(data$Programming_Student_personality)]<- mean(data$Programming_Student_personality, na.rm = TRUE)
data$Task_motivation[is.na(data$Task_motivation)]<-mean(data$Task_motivation, na.rm = TRUE)
data$Pace_of_curriculum[is.na(data$Pace_of_curriculum)]<-mean(data$Pace_of_curriculum, na.rm = TRUE)
data$Personality_match_with_program[is.na(data$Personality_match_with_program)]<- mean(data$Personality_match_with_program, na.rm = TRUE)
View(data)
#----------Replacing missing values for categorical values by converting them into likert scale first------
data$Choice <- case_when(
  data$Choice == "Yes" ~ 1,
  data$Choice == "No" ~ 2,
  data$Choice== "Unsure" ~ 3
)  
data$Choice[is.na(data$Choice)]<- mean(data$Choice, na.rm = TRUE) 
#-------------------------------------------------------------------
data$Group_learning <- case_when(
  data$Group_learning == "yes" ~ 1,
  data$Group_learning == "no" ~ 2,
  data$Group_learning == "doesn't matter, I am okay with both." ~ 3
)  
data$Group_learning[is.na(data$Group_learning)]<- mean(data$Group_learning, na.rm =TRUE)
#----------------------------------------------------------------------
data$Student_Domain <- case_when(
  data$Student_Domain == "IOT" ~ 1,
  data$Student_Domain == "Accountancy/ Finance" ~ 2,
  data$Student_Domain == "Law / Taxation" ~ 3,
  data$Student_Domain == "Digital marketing" ~ 4,
  data$Student_Domain == "Business Management" ~ 5,
  data$Student_Domain == "Gaming" ~ 6,
  data$Student_Domain == "Cloud Computing" ~ 7,
  data$Student_Domain == "AI / ML" ~ 8
)  

data$Student_Domain[is.na(data$Student_Domain)]<- mean(data$Student_Domain, na.rm = TRUE)
#------------------------------------------------------------------------
data$Learning_style <- case_when(
  data$Learning_style == "I best learn in class , through faculty." ~ 1,
  data$Learning_style == "Youtube videos are my go to choice." ~ 2,
  data$Learning_style == "Nptel/ Moocs , I like to get certifications." ~ 3
)  

data$Learning_style[is.na(data$Learning_style)]<- mean(data$Learning_style, na.rm = TRUE)
#------------------------------------------------------------------------
data$Understanding_memorizing <- case_when(
  data$Understanding_memorizing == "Agree" ~ 1,
  data$Understanding_memorizing == "Disagree" ~ 2,
  data$Understanding_memorizing == "depends upon the subject and my interest in it" ~ 3
)  
data$Understanding_memorizing[is.na(data$Understanding_memorizing)]<- mean(data$Understanding_memorizing, na.rm = TRUE)
#------------------------------------------------------------------------
data$Study_schedule <- case_when(
  data$Study_schedule == "Agree" ~ 1,
  data$Study_schedule == "Disagree" ~ 2,
  data$Study_schedule == "I try best to follow a routine but can't." ~ 3,
  data$Study_schedule == "Somedays I follow routine and somedays I don't." ~ 4
)  
data$Study_schedule[is.na(data$Study_schedule)]<- mean(data$Study_schedule, na.rm = TRUE)
#--------------------------------------------------------------------------
data$Completing_assignment<- case_when(
  data$Completing_assignment == "Agree" ~ 1,
  data$Completing_assignment == "Disagree, always at the last moment" ~ 2,
  data$Completing_assignment == "Sometimes yes, sometimes at the last moment" ~ 3
)  
data$Completing_assignment[is.na(data$Completing_assignment)]<- mean(data$Completing_assignment, na.rm = TRUE)
#---------------------------------------------------------------------------
data$Anxiety_towards_errors <- case_when(
  data$Anxiety_towards_errors == "Yes" ~ 1,
  data$Anxiety_towards_errors == "No" ~ 2,
  data$Anxiety_towards_errors == "I just want some guidance while doing practical." ~ 3,
  data$Anxiety_towards_errors == "I am comfortable with errors as it helps me to learn by doing mistakes." ~ 4
)   
data$Anxiety_towards_errors[is.na(data$Anxiety_towards_errors)]<- mean(data$Anxiety_towards_errors, na.rm = TRUE)
#-----------------------------------------------------------------------------
data$Academicpresurre_learning_relation <- case_when(
  data$Academicpresurre_learning_relation == "Yes , It demotivates me a lot" ~ 1,
  data$Academicpresurre_learning_relation == "No , It doesn't impact me much." ~ 2,
  data$Academicpresurre_learning_relation == "I am okay as I know learning something takes time and patience." ~ 3
) 
data$Academicpresurre_learning_relation[is.na(data$Academicpresurre_learning_relation)]<- mean(data$Academicpresurre_learning_relation, na.rm = TRUE)
#------------------------------------------------------------------------------
data$Asking_doubts <- case_when(
  data$Asking_doubts == "Yes" ~ 1,
  data$Asking_doubts == "No" ~ 2,
  data$Asking_doubts == "I sometimes don't ask as I would like to search and understand the doubt and answers better by myself." ~ 3
)  
data$Asking_doubts[is.na(data$Asking_doubts)]<- mean(data$Asking_doubts, na.rm = TRUE)
#-------------------------------------------------------------------------------
data$Learning_environment <- case_when(
  data$Learning_environment == "Structured leactures" ~ 1,
  data$Learning_environment == "Hands-on labs" ~ 2,
  data$Learning_environment == "Self learning" ~ 3,
  data$Learning_environment == "Group discussions" ~ 4
) 
data$Learning_environment[is.na(data$Learning_environment)]<- mean(data$Learning_environment, na.rm = TRUE)
#-------------------------------------------------------------------------------
data$Difficult_aspect_ofprogram <- case_when(
  data$Difficult_aspect_ofprogram == "Programming logic" ~ 1,
  data$Difficult_aspect_ofprogram == "Mathematics / Algorithms" ~ 2,
  data$Difficult_aspect_ofprogram == "Theory- heavy subjects" ~ 3,
  data$Difficult_aspect_ofprogram == "Time management" ~ 4,
  data$Difficult_aspect_ofprogram == "Stress / Anxiety" ~ 5,
  data$Difficult_aspect_ofprogram == "Lack of guidance" ~ 6,
  data$Difficult_aspect_ofprogram == "To much to learn / keeping up with growing tech skills." ~ 7
)
data$Difficult_aspect_ofprogram[is.na(data$Difficult_aspect_ofprogram)]<- mean(data$Difficult_aspect_ofprogram, na.rm = TRUE)
#-------------------------------------------------------------------------------
data$In_face_of_difficulty<- case_when(
  data$In_face_of_difficulty == "Persist and try alternatives" ~ 1,
  data$In_face_of_difficulty == "Seek help" ~ 2,
  data$In_face_of_difficulty == "Delay or avoid the task" ~ 3,
  data$In_face_of_difficulty == "Feel demotivated" ~ 4 
) 
data$In_face_of_difficulty[is.na(data$In_face_of_difficulty)]<- mean(data$In_face_of_difficulty, na.rm = TRUE)
#-------------------------------------------------------------------------------
data$MBTI <- case_when(
  data$MBTI == "The Analysts ( INTJ, INTP, ENTJ, ENTP)" ~ 1,
  data$MBTI == "The Diplomats ( INFJ, INFP, ENFJ, ENFP)" ~ 2,
  data$MBTI == "he Sentinels ( ISTJ, ISFJ, ESTJ, ESFJ)" ~ 3,
  data$MBTI == "The Explorers (ISTP, ISFP, ESTP, ESFP)" ~ 4
) 
data$MBTI[is.na(data$MBTI)]<- mean(data$MBTI, na.rm = TRUE)
#--------------Handling the open ended questions---------

data$Opinion <- tolower(trimws(data$Opinion))

library(tm)

# Create corpus
corpus <- Corpus(VectorSource(data$Opinion))

# Clean text
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Create matrix
dtm <- DocumentTermMatrix(corpus)

# Convert to matrix
matrix <- as.matrix(dtm)

# Word frequency
word_freq <- sort(colSums(matrix), decreasing = TRUE)

head(word_freq, 10)

data$Opinion_Category <- case_when(
  grepl("coding|programming|skills", data$Opinion) ~ "Coding Difficulty",
  grepl("struggle", data$Opinion) ~ "Academic Difficulty",
  grepl("dont|not able|confuse", data$Opinion) ~ "Lack of Confidence",
  grepl("interest|passion", data$Opinion) ~ "Interest/Motivation",
  grepl("practice|practical", data$Opinion) ~ "Need for Practical Learning",
  TRUE ~ "Other"
)  

table(data$Opinion_Category) 

library(ggplot2)

ggplot(data, aes(x = Opinion_Category)) +
  geom_bar() +
  labs(title = "Student Opinion Categories",
       x = "Category",
       y = "Count")  

library(wordcloud)

wordcloud(names(word_freq), word_freq, max.words = 50)   

#------------------------------------------------------------------------------
#-------Exploratory Analysis---------------------------------------------------

# Structure of dataset
str(data)

# Summary statistics
summary(data)

# Check data types
sapply(data, class)

colSums(is.na(data))

#-------------Demographics-------
str(data$Student_Age)
str(data$Course_year)
table(data$Course_year)

ggplot(data, aes(x = Course_year, y = Student_Age)) +
  geom_boxplot() +
  labs(title = "Age vs Course Year",
       x = "Course Year",
       y = "Age")  

ggplot(data, aes(x = Course_year, y = Student_Age)) +
  geom_jitter(width = 0.2) +
  labs(title = "Age Distribution Across Course Years",
       x = "Course Year",
       y = "Age")  

table(data$Student_Age, data$Course_year)
#-------------------------------------------------------------------------
table(data$MBTI, data$Learning_style)  
#---------Correlation between Curiosity vs Confidence-------------------------
ggplot(data, aes(x = Curiosity_for_technologies, fill = Curiosity_for_technologies)) +
  geom_bar(color = "black") +
  labs(title = "Curiosity for Technologies vs Employability Confidence",
       x = "Curiosity Level",
       y = "Number of Students") +
  scale_fill_manual(values = c(
    "No" = "#FF6B6B",        # Red
    "Sometimes" = "#FFD93D", # Yellow
    "Yes" = "#6BCB77"        # Green
  )) +
  theme_minimal() 
#------------Correlation between Study_schedule and Employability confidence--------

ggplot(data, aes(x = Study_schedule, fill = Employability_confidence)) +
  geom_bar(position = "dodge", color = "blue") +
  labs(title = "Study Schedule vs Employability Confidence",
       x = "Study Schedule Consistency",
       y = "Number of Students",
       fill = "Employability Confidence") +
  scale_fill_manual(values = c(
    "Low" = "#FF4E50",
    "Medium" = "#FFA600",
    "High" = "#2ECC71"
  )) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#---------------Correlation between Group learning vs Employability confidence-----------
ggplot(data, aes(x = Group_learning, fill = Employability_confidence)) +
  geom_bar(position = "dodge", color = "black", width = 0.7) +
  labs(
    title = "Group Learning vs Employability Confidence",
    x = "Preference for Group Learning",
    y = "Number of Students",
    fill = "Employability Confidence"
  ) +
  scale_fill_manual(values = c(
    "Very Low" = "#E63946",   # Deep Red
    "Low"      = "#F4A261",   # Orange
    "Medium"   = "#FFD93D",   # Yellow
    "High"     = "#6BCB77",   # Green
    "Very High"= "#1D7A46"    # Dark Green
  )) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text     = element_text(color = "black"),
    legend.position = "right",
    panel.grid.major.x = element_blank()
  )

#-------------Correlation of Anxiety towards errors and Employability confidence---------------

ggplot(data, aes(x = Anxiety_towards_errors, fill = Employability_confidence)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Anxiety Towards Errors vs Employability Confidence",
       x = "Anxiety Level",
       y = "Number of Students",
       fill = "Employability Confidence") +
  scale_fill_manual(values = c(
    "Low" = "#FF6B6B",      # Red
    "Medium" = "#FFD93D",   # Yellow
    "High" = "#6BCB77"      # Green
  )) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#-------------Correlation between beyond classroom learning and Pace of curriculum-----------

ggplot(data, aes(x = Beyond_classroom, fill = Pace_of_curriculum)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Pace of Curriculum Vs Beyond Classroom learning",
       x = "Beyond classroom learning",
       y = "No of students",
       fill = "Pace of curriculum") +
  scale_fill_manual(values = c(
    "No" = "#FF6B6B",        # Red
    "Sometimes" = "#FFD93D", # Yellow
    "Yes" = "#6BCB77"        # Green
  )) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
#----------------- Correlation between struggle with subjects and learning styles-------

ggplot(data, aes(x = Learning_style, fill = Struggle_with_subjects)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Struggle with Subjects vs Learning Style",
       x = "Learning Style",
       y = "Number of students",
       fill = "Struggle with subjects") +
  scale_fill_manual(values = c(
    "No" = "#2ECC71",        # Green
    "Sometimes" = "#F1C40F", # Yellow
    "Yes" = "#E74C3C"        # Red
  )) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#-------------Correlation between logical problem solving and coding without help------------------
ggplot(data, aes(x = Solving_logical_analytical_problem, 
                 fill = Coding_without_help)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Logical Problem Solving vs Coding Without Help",
       x = "Logical & Analytical Ability",
       y = "Number of Students",
       fill = "Coding Without Help") +
  scale_fill_manual(values = c(
    "No" = "#E74C3C",       # Red
    "Sometimes" = "#F1C40F",# Yellow
    "Yes" = "#2ECC71"       # Green
  )) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#--------------Correlation between personality match with program and academic satisfaction--------

ggplot(data, aes(x = Academic_satisfaction, 
                 fill = Personality_match_with_program)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Academic Satisfaction vs Personality Match",
       x = "Academic satisafaction",
       y = "Number of Students",
       fill = "personality match with program") +
  scale_fill_manual(values = c(
    "Low" = "#E74C3C",
    "Medium" = "#F1C40F",
    "High" = "#2ECC71"
  )) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))






















