This is a supplementary folder of the files that come with a manuscript "Self-reported Social Media Use Does Not Affect Cross-Cultural Consensus in First Impressions" (working tittle, after Revision 1, as of 12-09-2025). 
----
You find here the following files here: 
R Scripts: 

1_Processing_Raw_Data_REVISION.R It shows how data were processed and how the final table, provided to the reader, was created. Please note that the data we use to create the datasets reported below are not provided. They may be obtained from the authors upon justified request. 

2_Underlying_Factors_REVISION.R We computed underlying factor for the final split of the participants, as reported in a revised version of the manuscript. The script should work as all the necessary data have been added in the folder Data.Rar. 

3_Main_Models_Sampling_REVISION.R Script that contain the models, as reported in the manuscript, plus models based on the dataset with participants within 25% of median band, based on SMU_score, excluded.

4_Processing_Posterior_Plotting_Main_Models_REVISION.R In this script, Figures 2-4 of the revised manuscript were assembled. 

5_Processing_Posterior_Plotting_separate_models_REVISION.R Do you remember we also did models with only one of the three rating scales' ratings predicted? We did, and this is to the script where we create figures based on these models. 

6_Processing_Posterior_Plotting_Models_with_Centre_Scores_Excluded_REVISION.R We also excluded participants who were in the 25% band around Median for at least two of the three scales based on which the split was done. In this script, we show how the figures were created for these models. 

7_Models_Travel_Childhood_SES_REVISION.R We also splitted participants based on their frequency of traveling abroad and their socioeconomic status during childhood. The models are sampled through this script. 

8_Plotting_Travel_Abroad_Models_REVISION.R Here we create figures based on models from script 7.

9_Plotting_Childhood_SES_REVISION.R Here we also create figures based on models from script 7.

Datasets: 
(all are stored in the folder Data.rar, please download and unzip):
"Yufe_rating_study_long_data_SM_factor_25_08_25.csv" These are the data after revision. No older datasets are provided. We are not that sentimental. The dataset should be fine for being uploaded straight into R and run the analyses. However, if there is something wrong, try the 2nd data file of the same name: 

"2Yufe_rating_study_long_data_SM_factor_25_08_25.csv" Why this? Well, csv is "comma separated values". In some language environment, comma is also a marker of decimal place. One of those files may misbehave in your case. Just try them both. 

The other files: "AUS_USE_THIS_FOR_LONG_TABLE.Rdata", "COL_USE_THIS_FOR_LONG_TABLE.Rdata", "CZ_USE_THIS_FOR_LONG_TABLE.Rdata", "RSA_USE_THIS_FOR_LONG_TABLE.Rdata", "TUR_USE_THIS_FOR_LONG_TABLE.Rdata", and "VN_USE_THIS_FOR_LONG_TABLE.Rdata" allow reruring the script 2. Should you disagree with our underlying factor, run your own. 

"OLDEMOG.Rdata" contains all the demographic information we consider relevant. ID is just a code assigned by labvanced. No personal data whatsoever. These data will be subject of a further analysis. It is likely (unless you are the reviewer / journal editor) that when you read this text, the data has been processed into a stand alone short report and it may not be the best idea to use them for a publication without letting us know. 

"Supplementary_Materials.docx" Supplementary materials. It's 44 pages, but it's mostly Figure 1, 2, 3, and 4, when different datasets are considered, plus the detailed characterisation of the sample (Ethnicity, Languages, Passport Nationality) and description of how the SMU_score was calculated. Analogue of "Supplementary_Materials.pdf", a file that was uploaded together with the manuscript in journal submission interface. 

"Supplementary_Materials_OLD_BEFORE_REVISION.docx" These are old supplementary materials, as they were submitted together with the initial submission of the manuscript. It's not kept here because we are sentimental. In our opinion, it is not completely unreasonable to anticipate that someone would like to compare the old and the new version of the supplementary materials. 

"YUFE_Rating_Study_Old_Analyses.pdf". Originally (as we predicted) we wanted to include Czech control sample, which was tempting, since the dataset was large (N=777) raters. Nevertheless, the raters only rated Attactiveness and Trustworthiness. To include Dominance, we had to go for another "Czech control sample", in which participants rated on Likert scale 1-7. Moreover, in neither of the two "Czech control sets" were the participants asked on how / if they use social media. Therefore, we decided to exclude the Czech control samples. However, we decided so only after the study was preregistered. To show that we eare not hiding anything else than our bad design decision, we provide the full results (also to somehow justify the weeks that would otherwise be lost). 
