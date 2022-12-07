# MetaAnalyses_STARprotocols
Protocol for the processing of targeted metabolomics data in prospective nested case-control studies with R
To explore the associations among clinical factors, metabolites and disease outcome, we performed 4 steps for data analysis: 
(1) spearman correlation to measure relationships between metabolites and major clinical parameters; 
(2) conditional logistic regression analysis to quantify the association between metabolites and onset of disease; 
(3) casual mediation analysis to evaluate potential causal impact of metabolites on the disease; 
(4) variance contribution analysis to explore the factors affecting metabolite levels. 
For each part of analysis, R functions were provided, which allow user to call directly for data analysis in other studies with similar study design.

The users must prepare 3 datasets before analysis. including (1) clinical characteristics data, (2) levels of metabolites and (3) disease information for all the participants in the study. The first column of all 3 datasets should be unified as “ID”, the unique identifier of the participants in the study. Example datasets were provided as follows: 
(1) Clinical.txt: clinical characteristics data, 
(2) meta.txt: metabolomics data, and 
(3) outcome.txt: disease information. 
Example inputs were provided.

11 files including 9 .csv files and 2 plots were expected through these 4 parts of analysis. 
(1)SCmatrix.csv
(2)SCpmatrix.csv
(3)SC_heatmap.pdf
(4)OR.csv
(5)train_OR.csv
(6)valid_OR.csv
(7)randomdataset_OR.csv
(8)causal mediation.csv
(9)VP_corr_type.csv
(10)VP_var.csv
(11)VPplot.pdf
Example outputs were provided.

For any questions, please contact Hong Lin (lh12263@rjh.com.cn).
