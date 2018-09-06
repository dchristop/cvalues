#' A full data set created for the 60 countries of World Value Survey Wave 6
#'
#' @format A data frame with 60 rows and 10 variables:
#' \describe{
#' \item{name}{ country short name} 
#' \item{ISOAlpha2}{ country ISOAlpha2 code} 
#' \item{ISOAlpha3}{ country ISOAlpha3 code} 
#' \item{long}{ Longitude of country centroid} 
#' \item{lat}{ Latitude of country centroid} 
#' \item{D1}{ Dimension 1 of MDS} 
#' \item{D2}{ Dimension 2 of MDS} 
#' \item{D3}{ Dimension 3 of MDS} 
#' \item{Retreater}{ Country percentage for Retreaters} 
#' \item{Social_Hedonist}{ Country percentage for Social Hedonists} 
#' \item{Social_Conservative}{ Country percentage for Social Conservative} 
#' \item{Maximalist}{ Country percentage for Maximalists} 
#' \item{Social_Innovator}{ Country percentage for Social Innovator} 
#' \item{HH}{ HH Index} 
#' \item{NHH}{ NHH Index} 
#' \item{MI}{ MI Index} 
#' \item{GDP_Constant_$}{ GDP Constant $  Index} 
#' \item{Economic_Freedom}{ Economic Freedom  Index} 
#' \item{Political_Rights}{ Political Rights  Index} 
#' \item{Civil_Liberties}{ Civil Liberties  Index} 
#' \item{Overall_Freedom}{ Overall Freedom  Index} 
#' \item{Global_Competitiveness}{ Global Competitiveness  Index} 
#' \item{Human_Development}{ Human Development  Index} 
#' \item{Global_Innovation}{ Global Innovation  Index} 
#' \item{Rule_of_Law_1}{ Rule of Law 1  Index} 
#' \item{Control_of_corruption}{ Control of corruption  Index} 
#' \item{Government_Effectiveness}{ Government Effectiveness  Index} 
#' \item{Political_Stability}{ Political Stability  Index} 
#' \item{Regulatory_Quality}{ Regulatory Quality  Index} 
#' \item{Rule_of_Law_2}{ Rule of Law 2  Index} 
#' \item{Voice_and_Accountability}{ Voice and Accountability  Index} 
#' \item{Current_Account_Balance}{ Current Account Balance  Index} 
#' \item{Agricultural_Workers}{ Agricultural Workers  Index} 
#' \item{Self_Employed}{ Self Employed  Index} 
#' \item{Service_Workers}{ Service Workers  Index} 
#' \item{GDP_PP}{ GDP PP  Index} 
#' \item{gini_disp}{ gini disp  Index} 
#' \item{Females_in_Laborforce}{ Females in Laborforce  Index} 
#' \item{Land_Area}{ Land Area  Index} 
#' \item{Female_Life_Expectancy}{ Female Life Expectancy  Index} 
#' \item{Male_Life_Expectancy}{ Male Life Expectancy  Index} 
#' \item{Total_Life_Expectancy}{ Total Life Expectancy  Index} 
#' \item{Female_Literacy}{ Female Literacy  Index} 
#' \item{Male_Literacy}{ Male Literacy  Index} 
#' \item{Total_Literacy}{ Total Literacy  Index} 
#' \item{Defense_Spending}{ Defense Spending  Index} 
#' \item{Population_0_to_14}{ Population 0 to 14  Index} 
#' \item{Population_15_to_64}{ Population 15 to 64  Index} 
#' \item{Population_65_plus}{ Population 65 plus  Index} 
#' \item{Female_Population}{ Female Population  Index} 
#' \item{Total_Population}{ Total Population  Index} 
#' \item{Ratio_Females_to_Males_in_Laborforce}{ Ratio Females to Males in Laborforce  Index} 
#' \item{Rural_Population}{ Rural Population  Index} 
#' \item{Ratio_School_to_Tertiary_Enrolment}{ Ratio School to Tertiary Enrolment  Index} 
#' \item{Tax}{ Tax  Index} 
#' \item{Urban_Population}{ Urban Population  Index} 
#' \item{Happiness}{ Happiness  Index} 
#' \item{Homicide}{ Homicide  Index} 
#' }
"FullCountryDataFrameForPlots"


#' The final data set for archetypal mixture for the 5 archetypes created in WVS wave 6
#'
#' It contains the components of archetypes for the 10 Schwartz values
#'
#' @format A data frame with 5 rows and 11 variables:
#' \describe{
#' \item{B_Archetype}{Alpha label of archetype} 
#' \item{Universalism}{score of Schwartz value} 
#' \item{Benevolence}{score of Schwartz value} 
#' \item{Tradition}{score of Schwartz value} 
#' \item{Conformity}{score of Schwartz value} 
#' \item{Security}{score of Schwartz value} 
#' \item{Power}{score of Schwartz value} 
#' \item{Achievement}{score of Schwartz value} 
#' \item{Hedonism}{score of Schwartz value} 
#' \item{Stimulation}{score of Schwartz value} 
#' \item{Self-direction}{score of Schwartz value} 
#' }
"Final_W6_Archetypes"


#' The complete cases data set of World Value Survey Wave 6
#' after reversing Scwartz values to the scale 
#' "Not at all like me"->1
#' ...
#' "Very much like me"->6
#'
#' @format A data frame with 83526 rows and 14 variables:
#' \describe{
#' \item{id}{identity stamp for each respondent} 
#' \item{desc_gender}{gender of respondent: 0=male, 1=female} 
#' \item{desc_country}{country numeric code} 
#' \item{cyname}{short country name} 
#' \item{Universalism}{name of Schwartz value} 
#' \item{Benevolence}{name of Schwartz value} 
#' \item{Tradition}{name of Schwartz value} 
#' \item{Conformity}{name of Schwartz value} 
#' \item{Security}{name of Schwartz value} 
#' \item{Power}{name of Schwartz value} 
#' \item{Achievement}{name of Schwartz value} 
#' \item{Hedonism}{name of Schwartz value} 
#' \item{Stimulation}{name of Schwartz value} 
#' \item{Self-direction}{name of Schwartz value} 
#' }
"W6"

