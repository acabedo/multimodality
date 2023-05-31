library(readxl)
library(tidyverse)

discourse_markers <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Publicaciones/Llopis_Hubschner_Cabedo/discourse_markers.xlsx")

dmarkers <- discourse_markers %>%mutate(filename = gsub("A.Movie_conversation_","",file),filename = gsub(" m_friend-new.eaf","",filename),filename = gsub(" m_friend-new_cab.eaf","",filename),filename = gsub(" f_friend-new.eaf","",filename),filename = gsub(" f_friend-new_cab.eaf","",filename),filename = gsub(" m-friend-new.eaf","",filename))

dmarkers <- dmarkers%>%select(-part)%>%mutate(
  
  tier = tolower(tier),
  type = tolower(type),
  speaker = ifelse(grepl("eye_contact",tier),NA,speaker),
  tier = gsub("H1_eye_contact","eye_contact",tier),
  tier = gsub("H2_eye_contact","eye_contact",tier),
  tier = gsub("adapter","adaptor",tier),
  tier = gsub("eyebrows","eyebrow_movement",tier),
  tier = gsub("fron/_back","frontback",tier),
  tier = gsub("front/_back","frontback",tier),
  tier = gsub("front/back","frontback",tier),
  tier = gsub("getsure_type","gesture_type",tier),
  tier = gsub("hand_shape","handshape",tier),
  tier = gsub("head_gestures","head_gesture",tier),
  tier = gsub("marker__function","marker_function",tier),
  tier = gsub("marker_postition","marker_position",tier),
  tier = gsub("comment$","comments",tier),
  type = ifelse(type == "sí sí sí","sí",type),
  type = ifelse(type == "sí","sí",type),
  type = ifelse(type == "sí sí","sí",type),
  type = ifelse(type == "i bueno","bueno",type),
  type = ifelse(type == "aa","pausa_llena",type),
  type = ifelse(type == "ee","pausa_llena",type),
  type = ifelse(type == "eh","eh?",type),
  type = ifelse(type == "ehh","eh",type),
  type = ifelse(type == "tia","tio-tia",type),
  type = ifelse(type == "tio","tio-tia",type),
  type = ifelse(type == "vale vale","vale",type),
  type = ifelse(type == "bua","buah",type),
  type = ifelse(type == "¿vale?","vale?",type),
  type = ifelse(type == "i no sé","no sé",type),
  type = ifelse(type == "jaa","ja",type),
  type = ifelse(type == "ja ja","ja",type),
  type = ifelse(type == "ja ja ja","ja",type),
  type = ifelse(type == "no no","no",type),
  type = ifelse(type == "peró","però",type)
 
  
)%>%group_by(filename)%>%mutate(dur_conv=max(tmax,na.rm=TRUE))

i_mar <- dmarkers%>%filter(tier=="i_mar")%>%mutate(id = paste(type,speaker,tmin,sep="_"))
position <- dmarkers %>% filter(tier=="marker_position")
functions <- dmarkers %>% filter(tier=="marker_function")
functions$type <- gsub("1.turn taking","Tt", functions$type)
functions$type <- gsub("2.turn keeping","keeping", functions$type)
functions$type <- gsub("8.phatic","phatic", functions$type)
functions$type <- gsub("7.recognition of listener/solidarity","solidarity", functions$type)
functions$type <- gsub("expressing attitudes","emotions", functions$type)
functions$type <- gsub("9.expressing emotions","emotions", functions$type)
functions$type <- gsub("partial-disagreement","disagreement", functions$type)
functions$type <- gsub("^justification$","juex", functions$type)
functions$type <- gsub("^explanation$","juex", functions$type)
functions$type <- gsub("^justification-Explanation$","juex", functions$type)
functions$type <- gsub("partial agreement","partial-agr", functions$type)
functions$type <- gsub("partial-agreement","Pagr", functions$type)
functions$type <- gsub("Phatic","Ph", functions$type)
functions$type <- gsub("Contrast","Con", functions$type)
functions$type <- gsub("Partial disagreement","Pdis", functions$type)
functions$type <- gsub("Partial-Disagreement","Pdis", functions$type)
functions$type <- gsub("turn keeping","Tk", functions$type)
functions$type <- gsub("turn yielding","Ty", functions$type)
functions$type <- gsub("turn taking","Tt", functions$type)
functions$type <- gsub("showing comprehension","shcom", functions$type)
functions$type <- gsub("recognition of listener/solidarity","sol", functions$type)
functions$type <- gsub("partial-agr","agr", functions$type)
functions$type <- gsub("partial disagr","disagr", functions$type)
functions$type <- gsub("solidarity","sol", functions$type)
functions$type <- gsub("justification-explanation","juex", functions$type)
functions$type <- gsub("9.emotions","exem", functions$type)
functions$type <- gsub("seeking for confirmation","skcf", functions$type)
functions$type <- gsub("expressing emotions","exem", functions$type)
functions$type <- gsub("remembering sth","rmsth", functions$type)
functions$type <- gsub("drawing attention","drwat", functions$type)
functions$type <- gsub("agreement","agr", functions$type)

functions$type <- gsub("confirmation","con", functions$type)
adaptor <- dmarkers %>% filter(tier=="adaptor")
both_hands <- dmarkers %>% filter(tier=="both_hands")
eye_contact <- dmarkers %>% filter(tier=="eye_contact")
head_gesture <- dmarkers %>% filter(tier=="head_gesture")
head_gesture<- head_gesture%>%mutate(type = ifelse(grepl("nod",type),"nod",type),type = ifelse(grepl("tilt",type),"tilt",type),type = ifelse(grepl("shake",type),"shake",type),type = ifelse(grepl("turn",type),"turn",type), type = ifelse(type%in%c("nod","shake","tilt","turn"),type,"no"))
manual_gesture <- dmarkers %>% filter(tier=="manual_gesture")
manual_gesture <- manual_gesture%>%mutate(type=ifelse(type=="bh"|type=="lh"|type=="rh",type,"no"))
gaze <- dmarkers %>% filter(tier=="gaze")
frontback <- dmarkers %>% filter(tier=="frontback")
gesture_type <-dmarkers %>% filter(tier=="gesture_type")
gesture_type<- gesture_type%>%mutate(type = ifelse(type=="beat"|type=="iconic",type,"no"))
handshape <-dmarkers %>% filter(tier=="handshape")
handshape <- handshape%>%mutate(type=ifelse(type=="closed"|type=="open",type,"no"))
horizontal <-dmarkers %>% filter(tier=="horizontal")
vertical <-dmarkers %>% filter(tier=="vertical")
eyebrow_movement<-dmarkers %>% filter(tier=="eyebrow_movement")

library(sqldf)

i_mar0 <- sqldf("select x.id, x.tier, x.tmin,x.tmax, x.dur,x.file,x.filename,x.speaker,x.type,x.dur_conv, y.type as position from i_mar as x left join position as y on x.filename = y.filename and x.speaker = y.speaker and ((x.tmin >= y.tmin and x.tmax<=y.tmax)|(x.tmin<=y.tmin and x.tmax >y.tmax)|(x.tmin>=y.tmin and x.tmin <=y.tmax and x.tmax>=y.tmax)|(x.tmin<=y.tmin and x.tmax >=y.tmin and x.tmax <=y.tmax)) ")

i_mar1 <- sqldf("select x.id, x.tier, x.tmin,x.tmax, x.dur,x.file,x.filename,x.speaker,x.dur_conv,x.type as marker, x.position,y.type as functions from i_mar0 as x left join functions as y on x.filename = y.filename and x.speaker = y.speaker and ((x.tmin >= y.tmin and x.tmax<=y.tmax)|(x.tmin<=y.tmin and x.tmax >y.tmax)|(x.tmin>=y.tmin and x.tmin <=y.tmax and x.tmax>=y.tmax)|(x.tmin<=y.tmin and x.tmax >=y.tmin and x.tmax <=y.tmax)) ")

i_mar3 <- sqldf("select x.id, x.tier, x.tmin,x.tmax, x.dur,x.file,x.filename,x.speaker,x.marker,x.dur_conv, x.position,x.functions,  y.type as adaptor from i_mar1 as x left join adaptor as y on x.filename = y.filename and x.speaker = y.speaker and ((x.tmin >= y.tmin and x.tmax<=y.tmax)|(x.tmin<=y.tmin and x.tmax >y.tmax)|(x.tmin>=y.tmin and x.tmin <=y.tmax and x.tmax>=y.tmax)|(x.tmin<=y.tmin and x.tmax >=y.tmin and x.tmax <=y.tmax)) ")

i_mar4 <- sqldf("select x.id, x.tier, x.tmin,x.tmax, x.dur,x.file,x.filename,x.speaker,x.marker,x.dur_conv, x.position,x.functions, x.adaptor, y.type as both_hands from i_mar3 as x left join both_hands as y on x.filename = y.filename and x.speaker = y.speaker and ((x.tmin >= y.tmin and x.tmax<=y.tmax)|(x.tmin<=y.tmin and x.tmax >y.tmax)|(x.tmin>=y.tmin and x.tmin <=y.tmax and x.tmax>=y.tmax)|(x.tmin<=y.tmin and x.tmax >=y.tmin and x.tmax <=y.tmax))")

i_mar5 <- sqldf("select x.id, x.tier, x.tmin,x.tmax, x.dur,x.file,x.filename,x.speaker,x.marker,x.dur_conv, x.position,x.functions, x.adaptor,both_hands, y.type as eye_contact from i_mar4 as x left join eye_contact as y on x.filename = y.filename  and ((x.tmin >= y.tmin and x.tmax<=y.tmax)|(x.tmin<=y.tmin and x.tmax >y.tmax)|(x.tmin>=y.tmin and x.tmin <=y.tmax and x.tmax>=y.tmax)|(x.tmin<=y.tmin and x.tmax >=y.tmin and x.tmax <=y.tmax)) ")

i_mar6 <- sqldf("select x.id, x.tier, x.tmin,x.tmax, x.dur,x.file,x.filename,x.speaker,x.marker,x.dur_conv, x.position,x.functions, x.adaptor,both_hands,eye_contact, y.type as head_gesture from i_mar5 as x left join head_gesture as y on x.filename = y.filename and x.speaker = y.speaker and ((x.tmin >= y.tmin and x.tmax<=y.tmax)|(x.tmin<=y.tmin and x.tmax >y.tmax)|(x.tmin>=y.tmin and x.tmin <=y.tmax and x.tmax>=y.tmax)|(x.tmin<=y.tmin and x.tmax >=y.tmin and x.tmax <=y.tmax)) ")

i_mar7 <- sqldf("select x.id, x.tier, x.tmin,x.tmax, x.dur,x.file,x.filename,x.speaker,x.marker,x.dur_conv, x.position,x.functions, x.adaptor,both_hands,eye_contact,head_gesture, y.type as gaze from i_mar6 as x left join gaze as y on x.filename = y.filename and x.speaker = y.speaker and ((x.tmin >= y.tmin and x.tmax<=y.tmax)|(x.tmin<=y.tmin and x.tmax >y.tmax)|(x.tmin>=y.tmin and x.tmin <=y.tmax and x.tmax>=y.tmax)|(x.tmin<=y.tmin and x.tmax >=y.tmin and x.tmax <=y.tmax)) ")

i_mar8 <- sqldf("select x.id, x.tier, x.tmin,x.tmax, x.dur,x.file,x.filename,x.speaker,x.marker,x.dur_conv, x.position,x.functions, x.adaptor,both_hands,eye_contact,head_gesture,gaze, y.type as frontback from i_mar7 as x left join frontback as y on x.filename = y.filename and x.speaker = y.speaker and ((x.tmin >= y.tmin and x.tmax<=y.tmax)|(x.tmin<=y.tmin and x.tmax >y.tmax)|(x.tmin>=y.tmin and x.tmin <=y.tmax and x.tmax>=y.tmax)|(x.tmin<=y.tmin and x.tmax >=y.tmin and x.tmax <=y.tmax)) ")

i_mar9 <- sqldf("select x.id, x.tier, x.tmin,x.tmax, x.dur,x.file,x.filename,x.speaker,x.marker,x.dur_conv, x.position,x.functions, x.adaptor,both_hands,eye_contact,head_gesture,gaze, frontback,y.type as gesture_type from i_mar8 as x left join gesture_type as y on x.filename = y.filename and x.speaker = y.speaker and ((x.tmin >= y.tmin and x.tmax<=y.tmax)|(x.tmin<=y.tmin and x.tmax >y.tmax)|(x.tmin>=y.tmin and x.tmin <=y.tmax and x.tmax>=y.tmax)|(x.tmin<=y.tmin and x.tmax >=y.tmin and x.tmax <=y.tmax)) ")

i_mar10 <- sqldf("select x.id, x.tier, x.tmin,x.tmax, x.dur,x.file,x.filename,x.speaker,x.marker,x.dur_conv, x.position,x.functions, x.adaptor,both_hands,eye_contact,head_gesture,gaze, frontback,gesture_type, y.type as handshape from i_mar9 as x left join handshape as y on x.filename = y.filename and x.speaker = y.speaker and ((x.tmin >= y.tmin and x.tmax<=y.tmax)|(x.tmin<=y.tmin and x.tmax >y.tmax)|(x.tmin>=y.tmin and x.tmin <=y.tmax and x.tmax>=y.tmax)|(x.tmin<=y.tmin and x.tmax >=y.tmin and x.tmax <=y.tmax)) ")

i_mar11 <- sqldf("select x.id, x.tier, x.tmin,x.tmax, x.dur,x.file,x.filename,x.speaker,x.marker,x.dur_conv, x.position,x.functions, x.adaptor,both_hands,eye_contact,head_gesture,gaze, frontback,gesture_type,handshape, y.type as horizontal from i_mar10 as x left join horizontal as y on x.filename = y.filename and x.speaker = y.speaker and ((x.tmin >= y.tmin and x.tmax<=y.tmax)|(x.tmin<=y.tmin and x.tmax >y.tmax)|(x.tmin>=y.tmin and x.tmin <=y.tmax and x.tmax>=y.tmax)|(x.tmin<=y.tmin and x.tmax >=y.tmin and x.tmax <=y.tmax)) ")

i_mar12 <- sqldf("select x.id, x.tier, x.tmin,x.tmax, x.dur,x.file,x.filename,x.speaker,x.marker,x.dur_conv, x.position,x.functions, x.adaptor,both_hands,eye_contact,head_gesture,gaze, frontback,gesture_type,handshape,horizontal, y.type as vertical from i_mar11 as x left join vertical as y on x.filename = y.filename and x.speaker = y.speaker and ((x.tmin >= y.tmin and x.tmax<=y.tmax)|(x.tmin<=y.tmin and x.tmax >y.tmax)|(x.tmin>=y.tmin and x.tmin <=y.tmax and x.tmax>=y.tmax)|(x.tmin<=y.tmin and x.tmax >=y.tmin and x.tmax <=y.tmax)) ")

i_mar13 <- sqldf("select x.id, x.tier, x.tmin,x.tmax, x.dur,x.file,x.filename,x.speaker,x.marker,x.dur_conv, x.position,x.functions, x.adaptor,both_hands,eye_contact,head_gesture,gaze, frontback,gesture_type,handshape,horizontal, vertical, y.type as manual_gesture from i_mar12 as x left join manual_gesture as y on x.filename = y.filename and x.speaker = y.speaker and ((x.tmin >= y.tmin and x.tmax<=y.tmax)|(x.tmin<=y.tmin and x.tmax >y.tmax)|(x.tmin>=y.tmin and x.tmin <=y.tmax and x.tmax>=y.tmax)|(x.tmin<=y.tmin and x.tmax >=y.tmin and x.tmax <=y.tmax)) ")

i_mar14 <- sqldf("select x.id, x.tier, x.tmin,x.tmax, x.dur,x.file,x.filename,x.speaker,x.marker,x.dur_conv, x.position,x.functions, x.adaptor,both_hands,eye_contact,head_gesture,gaze, frontback,gesture_type,handshape,horizontal, vertical,manual_gesture, y.type as eyebrow_movement from i_mar13 as x left join eyebrow_movement as y on x.filename = y.filename and x.speaker = y.speaker and ((x.tmin >= y.tmin and x.tmax<=y.tmax)|(x.tmin<=y.tmin and x.tmax >y.tmax)|(x.tmin>=y.tmin and x.tmin <=y.tmax and x.tmax>=y.tmax)|(x.tmin<=y.tmin and x.tmax >=y.tmin and x.tmax <=y.tmax)) ")

i_mar15 <- i_mar14%>%select(-gaze)%>%mutate(quantity_kinesic =  11-rowSums(is.na(.)))%>%mutate(sex = ifelse(grepl(" f_",file),"female","male"),spk=paste(speaker,filename,sep="_"))%>%distinct(id,.keep_all = TRUE)%>%mutate(eye_contact=ifelse(!is.na(eye_contact),"yes",eye_contact),adaptor = ifelse(grepl("touching",adaptor),"touching",NA),
  eyebrow_movement = ifelse(grepl("nod",eyebrow_movement)|is.na(eyebrow_movement),NA,"raised"),gesture_type = ifelse(grepl("iconic",gesture_type),"iconic",gesture_type), manual_gesture = ifelse(manual_gesture=="BH (beat)","BH",manual_gesture),manual_gesture = ifelse(manual_gesture=="RH (beat)","RH",manual_gesture), manual_gesture = ifelse(manual_gesture=="RH (beat","RH",manual_gesture))

library(tidyr)
i_nona <- i_mar15%>%mutate(across(everything(), ~replace_na(.x, "no")))

ggplot(i_mar15, aes(y=quantity_kinesic, x="corpus",fill="boxplot")) + 
  geom_boxplot(
    size = 1, outlier.shape = NA
  ) +
  geom_jitter(width = .1, size = 2, alpha = .9)+coord_flip()

ggplot( i_nona,aes(x=quantity_kinesic)) +
  geom_histogram( aes(fill = quantity_kinesic ),binwidth=1, fill="orange", color="#e9ecef", alpha=0.9)  +
  theme(
    plot.title = element_text(size=15)
  )+
  stat_bin(binwidth=1, geom='text', color='black', size=4,
           aes(label=..count..), position=position_stack(vjust=0.5))

ggplot(i_mar15%>%arrange(desc(sex)), aes(fill=sex, y=quantity_kinesic, x=spk)) + 
  geom_boxplot(
    size = 1, outlier.shape = NA
  ) +
  geom_jitter(width = .1, size = 0.9, alpha = .5)

## ANOVA

paov <- TukeyHSD(aov(quantity_kinesic~sex*position*functions,data = i_mar15))
paov2 <- as.data.frame(paov)
View(paov2%>%filter(`p adj`<0.05))



library(FactoMineR)
p<-FactoMineR::MCA(i_nona[,-c(1:8,10,24,25,26)])
factoextra::fviz_eig(p)
factoextra::fviz_mca_var(p,repel = TRUE,col.var = "cos2",gradient.cols = c("#00AFBB", "#E7B800","#FC4E07"))
s<-FactoMineR::HCPC(p,nb.clust = 4)
factoextra::fviz_cluster(s,ellipse = TRUE)


# library(rpart)
# library(rpart.plot)
# d <- rpart(position~.,data=i_nona%>%select(-marker,-id,-speaker,-spk,-file,-filename,-dur_conv,-tmin,-tmax)%>%mutate_if(is.character,as.factor),cp = 0.01, minsplit=10, minbucket=6, maxdepth=10)
# rpart.plot::rpart.plot(d,type=4,cex = .7 )
library(randomForest)
nona2<-randomForest(quantity_kinesic~.,i_nona%>%select(-marker,-id,-speaker,-spk,-file,-filename,-dur_conv,-tmin,-tmax))
varImpPlot(nona2)
library(party)
d <- ctree(quantity_kinesic~.,data=i_nona%>%select(-marker,-id,-speaker,-spk,-file,-filename,-dur_conv,-tmin,-tmax)%>%mutate_if(is.character,as.factor),controls = ctree_control(maxdepth = 4))
plot(d)

