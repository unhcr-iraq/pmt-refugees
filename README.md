# Refugee protection Monitoring

This project is an analysis of the protection monitoring dataset. The project is developped with [Rstudio & Github](http://happygitwithr.com/index.html)

The code is commented but [post issues](https://github.com/unhcr-iraq/pmt-refugees/issues) if needed. Note that you can highlight one element of the code using the following syntax. All you need to do is append something like #L17-L24 to specify highlighting lines 17 to 24.  For instance, this [URL](https://github.com/unhcr-iraq/pmt-refugees/blob/master/code/1-loaddata.R#L18-L20) highlight the loop used to rename the variable and their label based on a codebok the . 

To get ready, position your data set (in csv format, call it data.csv) and the form definition in [xlsform](http://xlsform.org/) format.

Please go to [code/1-loaddata.R](https://github.com/unhcr-iraq/pmt-refugees/blob/master/code/1-loaddata.R) to load the data.

This script will load:

 * [0-packages.R](https://github.com/unhcr-iraq/pmt-refugees/blob/master/code/0-packages.R) This will activate - and install if not yet installed in your R environment the list of necessary packages for the analysis

 * [1-get-labels-from-form.R](https://github.com/unhcr-iraq/pmt-refugees/blob/master/code/1-get-labels-from-form.R) in order to recode the full dataset with orginal variable name & lablel based on the xlsform definition of the form. It creates a kind of codebook called datalabel
 
From there a series of script are looking at the exploration: 

 * Preparing graphs using loop to generate them in batch mode: [2-create-graph.R](https://github.com/unhcr-iraq/pmt-refugees/blob/master/code/2-create-graph.R)
 
 * Preparing maps in the same way: [3-create-maps.R](https://github.com/unhcr-iraq/pmt-refugees/blob/master/code/3-create-maps.R)
 
 * Testing correlations and independance between variable: [4-testing_correlation.R](https://github.com/unhcr-iraq/pmt-refugees/blob/master/code/4-testing_correlation.R) 
 
 * Exploration multivariate analysis: [5-correspondance-analysis.R](https://github.com/unhcr-iraq/pmt-refugees/blob/master/code/5-correspondance-analysis.R)
 
 
 Then a report using Rmarkdown will allow to compile the results: [6-protection-monitoring-report.Rmd](https://github.com/unhcr-iraq/pmt-refugees/blob/master/code/6-protection-monitoring-report.Rmd)