This folder contains supplementary files accompanying the manuscript Self-reported Social Media Use Does Not Affect Cross-Cultural Consensus in First Impressions (working title, after Revision 1, as of 12-09-2025).
----

R Scripts

*1_Processing_Raw_Data_REVISION.R*
This script shows how the data were processed and how the final table, provided to the reader, was created.
Note: The raw data used to generate the datasets reported below are not included here but can be obtained from the authors upon a justified request.

*2_Underlying_Factors_REVISION.R*
This script computes the underlying factor used for the final split of participants, as reported in the revised version of the manuscript.
All necessary data are included in the folder Data.rar, so the script should run without issues.

*3_Main_Models_Sampling_REVISION.R*
Contains the main models reported in the manuscript, as well as additional models excluding participants within the 25% band around the median SMU_score.

*4_Processing_Posterior_Plotting_Main_Models_REVISION.R*
Used to assemble Figures 2–4 in the revised manuscript.

*5_Processing_Posterior_Plotting_Separate_Models_REVISION.R*
Remember how we also ran models predicting only one of the three rating scales at a time?
This script creates figures based on those separate models.

*6_Processing_Posterior_Plotting_Models_with_Centre_Scores_Excluded_REVISION.R*
For these models, participants within the 25% band around the median SMU_score were excluded.
This script generates the corresponding figures.

*7_Models_Travel_Childhood_SES_REVISION.R*
Samples models where participants were split based on travel abroad frequency and childhood socioeconomic status (SES).

*8_Plotting_Travel_Abroad_Models_REVISION.R*
Generates figures based on the travel abroad models created in script 7.

*9_Plotting_Childhood_SES_REVISION.R*
Generates figures based on the childhood SES models from script 7.

Datasets

(All files are stored in the folder Data.rar — please download and unzip before use.)

*Yufe_rating_study_long_data_SM_factor_25_08_25.csv*
This is the main dataset after revision. 
The file should work directly in R. However, if there are problems, try the alternative version below:

*2Yufe_rating_study_long_data_SM_factor_25_08_25.csv*
In some language environments, the comma is also used as a decimal separator, which can cause trouble with CSV files.
If one version misbehaves, try the other.

Additional Files for Script 2

The following files are needed to rerun script 2 and compute your own underlying factor if you wish to use a different approach than ours:

*AUS_USE_THIS_FOR_LONG_TABLE.Rdata*

*COL_USE_THIS_FOR_LONG_TABLE.Rdata*

*CZ_USE_THIS_FOR_LONG_TABLE.Rdata*

*RSA_USE_THIS_FOR_LONG_TABLE.Rdata*

*TUR_USE_THIS_FOR_LONG_TABLE.Rdata*

*VN_USE_THIS_FOR_LONG_TABLE.Rdata*


*OLDEMOG.Rdata* contains all the demographic information we consider relevant. ID is just a code assigned by labvanced. No personal data whatsoever. These data will be subject of a further analysis (together with an equally large unpublished dataset that cotains the same variables). It is likely (unless you are the reviewer / journal editor) that when you read this text, the data has been processed into a stand alone short report and it may not be the best idea to use them for a publication without letting us know. 

*Supplementary_Materials.docx* Supplementary materials. It's 44 pages, but it's mostly Figure 1, 2, 3, and 4, when different datasets are considered, plus the detailed characterisation of the sample (Ethnicity, Languages, Passport Nationality) and description of how the SMU_score was calculated. Analogue of "Supplementary_Materials.pdf", a file that was uploaded together with the manuscript in journal submission interface. 

*Supplementary_Materials_OLD_BEFORE_REVISION.docx* These are old supplementary materials, as they were submitted together with the initial submission of the manuscript. It's not kept here because we are sentimental. In our opinion, it is not completely unreasonable to anticipate that someone would like to compare the old and the new version of the supplementary materials. 

*YUFE_Rating_Study_Old_Analyses.pdf*. Originally (as we predicted) we wanted to include Czech control sample, which was tempting, since the dataset was large (N=777 raters). Nevertheless, the raters only rated Attactiveness and Trustworthiness. To include Dominance, we had to go for another "Czech control sample", in which participants rated on Likert scale 1-7. Moreover, in neither of the two "Czech control sets" were the participants asked on how / if they use social media. Therefore, we decided to exclude the Czech control samples. However, we decided so only after the study was preregistered. To show that we eare not hiding anything else than our bad design decision, we provide the full results (also to somehow justify the weeks of work that would otherwise be wasted). 

*YUFE_Rating_Study_Rev2_Visual_SM_analyses.rar* Following justified feedback from the reviewer, we, during the 2nd round of revision, introduced new type of median split. It was based exclusively on the frequency of use of three social media that were, at the time of the submission, considered mostly or exclusively visually oriented (YouTube, Instagram, Tiktok), while the factor analysis revealed that there is much more shared variance between intensity of using of TikTok and Instagram, the data generally support our selection. This compressed file contains script in which the underlying factor is calculated as well as subsequent analyses. We only wanted to prevent misunderstanding (and avoid uncessary but very likely errors) by updating all the scripts and the content of the folder as a whole.  
