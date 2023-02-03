# MetaAnalyses_STARprotocols
Protocol for meta-analysis of LC-MS metabolomics data using R to understand how metabolites affect disease

To explore the associations among clinical factors, metabolites and disease, 4 parts of analysis were performed: 1) Spearman correlation to measure associations between metabolites and major clinical parameters; 2) conditional logistic regression analysis to quantify the association between metabolites and disease’s onset; 3) causal mediation analysis to evaluate potential causal impact of metabolites on the disease; 4) variance contribution analysis to explore the factors affecting metabolite levels. The 4 layers of in-depth analysis could give us a comprehensive view of relationships among clinical risk factor-metabolite-disease, which lead to a better understanding of the development and progression of human diseases.

The users must prepare 3 datasets before analysis, including clinical characteristics data at baseline, metabolomics data at baseline and disease condition data at follow-up. The first column of all 3 datasets should be unified as “ID”, the unique identifier of the participants in the study. Example datasets were provided as follows: 
(1) Clinical.txt: clinical characteristics data, 
(2) meta.txt: metabolomics data, and 
(3) outcome.txt: disease information. 
Example inputs were provided.

13 files including 10 .csv files and 3 plots  were expected through these 4 parts of analysis. 
(1)normality.test.csv 
(2)normality.test.pdf 
(3)SCmatrix.csv
(4)SCpmatrix.csv
(5)SC_heatmap.pdf
(6)OR.csv
(7)dataset1_OR.csv
(8)dataset2_OR.csv
(9)randomdataset_OR.csv
(10)causal mediation.csv
(11)VP_corr_type.csv
(12)VP_var.csv
(13)VPplot.pdf
Example outputs were provided.

For any questions, please contact Hong Lin (lh12263@rjh.com.cn).
