#### read data set ####
music <- read.csv("master_ALL_repository.csv",sep = "\t")

str(music)
summary(music)

library(gdata) # for drop.levels() function
library(RVAideMemoire) # for reliability
library(irr) # for calculating kappa
library(vcd) # for mosaic plots

#### INSPECT DATA FRAME ####
str(music)
summary(music)

##VARIABLES / INFORMATION in data frame:
#The data frame contains the following information / variables:
#NO = assigns a number to each item as an index
#CORPUS.FILE = indicates the source text in the corpus from which the passage was taken
#ITEM = the actual words (lexical units) that were annotated for metaphor
#POS = part of speech
#POS_BROAD = more coarse-grained classification of part-of-speech (e.g. all noun takes receive one tag and are not distinguished into singular and plural use) (categorical variable)
#contextual meaning = of the item in the present context, taken from Macmillan dictionary
#basic meaning = more basic meaning of the item in other contexts, taken from the Macmillan dictionary
#METAPHOR = indicates whether the item is used metaphorically
  ##categorical variable
   ###levels: no (not metaphorically used), DFMA ("discarded for metaphor analysis"), DFMA_Punct (discarded for metaphor analyses because it is a punctuation item), MRW-PP ("possible personification"), MRW-direct (direct use of a metaphor-related word), MRW-implicit (implicit use of a metaphor-related word), MRW-indirect (indirect use of a metaphor-related word), Mflag (metaphor flag), WIDLII ("when in doubt, leave it in)
#MUSIC.TARGET.DOMAIN = only assigned to cases of metaphor, indicates whether the metaphor refers to the target domain of music; levels: yes, no
#SD = only assigned to cases of metaphor and MUSIC.TARGET.DOMAIN = yes, indicates potential source domain reflected by item
#CONCEPTUAL_METAPHOR_BASIS = only assigned to cases of metaphor, with music as target domain, and motion as source domain
#DEICTIC_EGO = only assigned to cases of metaphor, with music as target domain, and motion as source domain, indicates whether the motion is used deictically or not (levels: neutral, deictic)
#Context = provides context of use for selected items
#comment = provides comments for some annotations


#### CLEAN UP ####
### change vector types
music$ITEM <- as.character(music$ITEM)

#in ITEM, delete underscore + POS tags
music$ITEM <- gsub("_...","",music$ITEM)

### delete punctuation
##  delete DFMA_Punct (because CLAWS regards punctuation as individual units) *DFMA = discard for metaphor analysis (cf. Steen et al 2010a)
music <- music[music$METAPHOR!="DFMA_Punct", ]

#now we can identify the number of lexical units in each corpus file (cf. Table 7.2)
table(music$CORPUS.FILE)
length(music$CORPUS.FILE)

## now delete DFMA # note that this is already part of MIPVU!, i.e. we are deleting words which are not suitable for metaphor analysis (foreign words and "for" and "of", cf. Steen et al. 2010)
music <- music[music$METAPHOR!="DFMA", ]
length(music$CORPUS.FILE)

### create a column "genre" with only two levels: aca & news
news <- c("CT284", "CT348", "GLR1282", "GLR1347", 
          "GLR900", "IR1149", "IR1196", "IR1447", "LAT19","MN91", "NYT109", "NYT12", "TT34", "TT351", "TT626", "WP409", "WP69", "WSJ115")
music$GENRE <- ifelse(music$CORPUS.FILE %in% news, "news", "aca")

## replace original column
music$CORPUS.FILE <- music$GENRE

## delete newly created column
a <- which(colnames(music)=="GENRE")
music <- music[-a]

## rename original column
colnames(music)[2] <- "GENRE"

### drop empty factor levels (DFMA, DFMA_Punct)
summary(music$METAPHOR)
music$METAPHOR <- drop.levels(music$METAPHOR)
summary(music$METAPHOR)

### create another colum for METAPHOR TYPE, so that the variable METAPHOR only has two levels: yes or no

yes <- c("MRW-direct", "MRW-implicit", "MRW-indirect", "MRW-PP", "WIDLII")
music$metaphor.yes.no <- ifelse(music$METAPHOR %in% yes, "yes", "no")

music$metaphor.yes.no <- as.factor(music$metaphor.yes.no)

## rename columns accordingly
colnames(music)[which(colnames(music)=="METAPHOR")] <- "METAPHOR.TYPE"
colnames(music)[which(colnames(music)=="metaphor.yes.no")] <- "METAPHOR"

#### STATISTICAL ANALYSIS ####

### overall metaphor frequency ####
##for metaphor (yes, no) (cf. Table 8.1)
met.all <- xtabs(~METAPHOR, music); addmargins(met.all)
met.all.prop <- prop.table(xtabs(~METAPHOR, music)); addmargins(met.all.prop)

## Is this significantly different from Steen's findings
#load data from Steen
Steen.register <- read.csv("Music.vs.Steen.Register.csv") # frequencies from Steen et al. 2010: 195
#clean (row names, total sums)
rownames(Steen.register) <- Steen.register$X
Steen.register <- Steen.register[c(1,2),c(2:5)]
#now subset according to register
Steen.register.aca <- Steen.register[,c(1,3)]
Steen.register.news <- Steen.register[,c(2,4)]

#perform chi2 for given probabilies in aca + news
Steen.register$Steen.aca.news <- Steen.register$Steen_aca + Steen.register$Steen_news
Steen.register$Music.aca.news <- Steen.register$music_aca + Steen.register$music_news

chisq.test(Steen.register[, "Music.aca.news"], p=Steen.register[, "Steen.aca.news"]/sum(Steen.register[, "Steen.aca.news"])) # X-squared = 88.579, df = 1, p-value < 2.2e-16

#result: metaphor frequency in music criticsm differs signicantly from Steen et al.'s observed frequencies for aca and news (cf. Table 8.2)

#additional: compare metaphor frequency  between registers
#perform chi2 for academic register
Steen.chi2.aca <- chisq.test(Steen.register.aca$music_aca, p=Steen.register.aca$Steen_aca/sum(Steen.register.aca$Steen_aca)); Steen.chi2.aca # X-squared = 83.953, df = 1, p-value < 2.2e-16
#Result: metaphor frequency in the academic part of the music criticsm corpus is significantly different from metaphor frequeny in the academic register in Steen et al (which is taken to be representative for the academic register in general)

#perform chi2 for news register
chisq.test(Steen.register.news$music_news, p=Steen.register.news$Steen_news/sum(Steen.register.news$Steen_news)) # X-squared = 20.231, df = 1, p-value = 6.862e-06
#Result: metaphor frequency in the news part of the music criticsm corpus is significantly different from metaphor frequeny in the news register in Steen et al (which is taken to be representative for the news register in general)


#### reliability ####
#load data (Desktop>Reli>Reli2_all.csv)
rel2 <- read.csv(file = file.choose(), header = TRUE, dec = ",")

#subset (to only include raw data)
rel2 <- rel2[c(1:1020),c(1:2)]
rel2 <- drop.levels(rel2)

### calculate KAPPA ###
#Kappa2 = Cohen's Kappa for two raters (Fleiss Kappa for more than two raters) cf. MIPVU p. 150
kappa2(rel2)
#Result: Kappa = 0.732, p < 0.001, sample size = 1020 items, raters = 2 (cf. Table 8.3)

### test for coder bias: calculare Cochran's Q berechnen (in line with MIPVU, Steen et al. 2010, p. 150ff.) ###
#for that, the data format has to be re-organised
rel2.nina <- rel2[,1]
nina <- data.frame(rel2.nina, coder="Nina")
rel2.steffi <- rel2[,2]
steffi <- data.frame(rel2.steffi, coder="Steffi")
colnames(nina) <- c("code", "coder")
colnames(steffi) <- c("code", "coder")
nina$item <- 1:1020
steffi$item <- 1:1020
#add both data frames
rel3 <- rbind(nina, steffi)
head(rel3)
tail(rel3)

cochran.qtest(code~coder|item,data=rel3,p.method="none")
#results: the difference between coders is significant indicating coder bias (Cochran's Q (df=1 ) = 62.7451, p<0.001) (cf. Table 8.3)

### alternative McNemar Test ###
summary(rel2)
#create a summary of reliability data and assign it to the matrix "mat"
mat <- matrix(c(730,290,810,210), nrow = 2, ncol = 2)

mcnemar.test(mat)
#result: the test is equally significant, indicating that the coders behaved significantly different form one another, indicating coder bias (McNemar's chi-squared = 244.87, df = 1, p-value < 2.2e-16) (cf. footnote 69)


#### metaphor frequency and GENRE ####
music$GENRE <- as.factor(music$GENRE)

met.genre <- xtabs(~METAPHOR+GENRE, music); addmargins(met.genre)
#see Table 8.4

met.genre.prop <- round(prop.table(xtabs(~METAPHOR+GENRE, music), margin=2),2); met.genre.prop
#see Table 8.4

# Is the difference in metaphor frequency between aca and news significant?
met.genre.chi2 <- chisq.test(xtabs(~METAPHOR+GENRE, music)); met.genre.chi2 #chi2 bc the DV is categorical (metaphor yes or no)
#output: X-squared = 36.083, df = 1, p-value = 1.891e-09

#additional:
#residuals
resi1 <- met.genre.chi2$residuals; resi1

#effect size
sqrt(met.genre.chi2$statistic/sum(met.genre)*(min(dim(met.genre))-1))

mosaicplot(xtabs(~METAPHOR+GENRE, music), shade = T)


## metaphor type ####
met.type.all <-  xtabs(~METAPHOR.TYPE, music); met.type.all
#percentage (100% = all metaphors (N = 2134) + mflags (N = 17))
met.type.all/(2134+17)*100 
#see Table 8.5

## metaphor.type in relation to genre ####
met.type <- xtabs(~METAPHOR.TYPE+GENRE, music); met.type
#see table 8.6

#100% in column
met.type.prop1 <- round(prop.table(xtabs(~METAPHOR.TYPE+GENRE, music), margin=2)*100,2); met.type.prop1

#100% in row
met.type.prop2 <- round(prop.table(xtabs(~METAPHOR.TYPE+GENRE, music), margin=1)*100,2); met.type.prop2

#Is the different distribution of metaphor types across genres significant?
chisq.test(met.type) #X-squared = 57.987, df = 7, p-value = 3.803e-10
chisq.test(met.type)$expected
chisq.test(met.type)$observed
resi2 <-  round(chisq.test(met.type)$residuals,1); resi2
#see Table 8.6

#result: the distributions of metaphor types in the two genres differ significantly from one another (X2(7)=57.987, p<0.001). In the news texts more words are not metaphorically used. The indirect metaphors, implicit metaphors and cases labeled as WIDLII are overrepresented in the aca genre. 

#### conceptual basis of motion metaphors ####

#subset: only cases of metaphor + music as target domain
music1 <- music[music$METAPHOR=="yes" & music$MUSIC_TARGET.DOMAIN=="yes",]
length(music1[,1])/2134
#of all metaphorical expressions, 68% have music as target domain (N = 1,442)

## source domains ####
music1 <- drop.levels(music1)
summary(music1$SD)
sort(summary(music1$SD), decreasing = T)
sort(summary(music1$SD), decreasing = T)/length(music1$SD)*100

## motion expressions, conceptual metaphor basis ##
music2 <- music1[music1$SD=="motion",] # N = 244
music2 <- drop.levels(music2)

sort(summary(music2$CONCEPTUAL_METAPHOR_BASIS), decreasing = T)
sort(summary(music2$CONCEPTUAL_METAPHOR_BASIS), decreasing = T)/length(music2$CONCEPTUAL_METAPHOR_BASIS)*100

plot(sort(summary(music2$CONCEPTUAL_METAPHOR_BASIS), decreasing = T))

#other: bring out, movement, abandon, retrograde, swell, heave, conducted, surging, came at, helmed, catching, moving, got away from

## deixis ####
# are the motion expressions deictic (as Johnson and Larson suggest?)
summary(music2$DEICTIC_EGO)
summary(music2$DEICTIC_EGO)/length(music2$DEICTIC_EGO)*100

table(music2$CONCEPTUAL_METAPHOR_BASIS, music2$DEICTIC_EGO)
prop.table(table(music2$CONCEPTUAL_METAPHOR_BASIS, music2$DEICTIC_EGO),1)
