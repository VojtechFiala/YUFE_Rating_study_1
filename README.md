This is a supplementary folder of the files that come with a manuscript "Self-reported Social Media Use Does Not Affect Cross-Cultural Consensus in First Impressions" (working tittle, after Revision 1, as of 12-09-2025). 
----
You find here the following files here: 
R Scripts: 

1_Processing_Raw_Data_REVISION.R It shows how data were processed and how the final table, provided to the reader, was created. Please note that the data we use to create the datasets reported below are not provided. They may be obtained from the authors upon justified request. 

2_Underlying_Factors_REVISION.R We computed underlying factor for the final split of the participants, as reported in a revised version of the manuscript. The script should work as all the necessary data have been added in the folder Data.Rar. 

3_Main_Models_Sampling_REVISION.R Script that contain the models, as reported in the manuscript.

4_Processing_Posterior_Plotting_Main_Models_REVISION.R In this script, Figures 2-4 of the revised manuscript were assembled. 

4_Plotting_Separate_Models.R Do you remember we also did models with only one of the three rating scales' ratings predicted? We did, and this is to the script where we create figures based on these models. 

5_PlottingModels_with_Centre_Scores_Excluded.R We also excluded participants who were in the 25% band around Median for at least two of the three scales based on which the split was done. In this script, we show how the figures were created for these models. 

6_Models_Travel_Childhood_SES.R We also splitted participants based on their frequency of traveling abroad and their socioeconomic status during childhood. The models are sampled through this script. 

7_Plotting_Travel_Abroad_Models.R Here we create figures based on models from script 6.

8_Plotting_Childhood_SES.R Here we also create figures based on models from script 6.

Datasets: 
(all are stored in the folder Data.rar, please download and unzip):
"Yufe_rating_study_long_data_SM_factor_25_08_25.csv" These are the data after revision. No older datasets are provided. We are not that sentimental. The dataset should be fine for being uploaded straight into R and run the analyses. However, if there is something wrong, try the 2nd data file of the same name: 

"2Yufe_rating_study_long_data_SM_factor_25_08_25.csv" Why this? Well, csv is "comma separated values". In some language environment, comma is also a marker of decimal place. One of those files may misbehave in your case. Just try them both. 

The other files: "AUS_USE_THIS_FOR_LONG_TABLE.Rdata", "COL_USE_THIS_FOR_LONG_TABLE.Rdata", "CZ_USE_THIS_FOR_LONG_TABLE.Rdata", "RSA_USE_THIS_FOR_LONG_TABLE.Rdata", "TUR_USE_THIS_FOR_LONG_TABLE.Rdata", and "VN_USE_THIS_FOR_LONG_TABLE.Rdata" allow reruring the script 2. Should you disagree with our underlying factor, run your own. 

"OLDEMOG.Rdata" contains all the demographic information we consider relevant. ID is just a code assigned by labvanced. No personal data whatsoever. These data will be subject of a further analysis. It is likely (unless you are the reviewer / journal editor) that when you read this text, the data has been processed into a stand alone short report and it may not be the best idea to use them for a publication without letting us know. 

"Supplementary_Materials.docx" Supplementary materials. It's 38 pages, but it's mostly Figure 1, 2, 3, and 4, when different datasets are considered. Analogue of "Supplementary_Materials.pdf" that were uploaded together with the paper.  

"YUFE_Rating_Study_Old_Analyses.pdf". Originally (as we predicted) we wanted to include Czech control sample, which was tempting, since the dataset was large (N=777) raters. Nevertheless, the raters only rated Attactiveness and Trustworthiness. To include Dominance, we had to go for another "Czech control sample", in which participants rated on Likert scale 1-7. Moreover, in neither of the two "Czech control sets" were the participants asked on how / if they use social media.Therefore, we decided to exclude the Czech control samples. However, to avoid accusition we are hiding something else than our bad design decision, we provide the full results (also to somehow justify the weeks that would otherwise be lost). 
