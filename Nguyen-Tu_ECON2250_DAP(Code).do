*Set Working Directory
cd "\\prism.nas.gatech.edu\enguyentu3\vlab\desktop\ECON2250\Effect of Sleep on Marriage"


*Select Dataset
use "SLEEP75.dta"
//removed commas from variable labels
use "SLEEP75 (labels no comma).dta"



////////////////////////////////////////////////////////////////////////////////
*Introduction*
////////////////////////////////////////////////////////////////////////////////

*Checking if Variables of Interest are Continuous or Discrete by tabulating frequencies
tab slpnaps
tab yrsmarr
//both variables are discrete

*Description of Variables of Interest
describe slpnaps yrsmarr

*Table 1 - Data Summary of All Variables in SLEEP75.dta
estpost summarize
esttab using "Nguyen-Tu_DAP-Milestone1_Summarize.csv", cells("count(fmt(0))mean(fmt(2))Var(fmt(2))sd(fmt(2))max(fmt(2))min(fmt(2))sum(fmt(2))") title("SLEEP75") nonumbers label replace
eststo clear
//The labels of the variables were altered to replace "," with "()" and the variable names were added separately



////////////////////////////////////////////////////////////////////////////////
*Graphical Representations of Data*
////////////////////////////////////////////////////////////////////////////////


*Table 2 - Correlation Matrix

//cosider observations with missing values
pwcorr yrsmarr slpnaps

//consider observations without missing values
correlate yrsmarr slpnaps

//both showed the same results, so only one will be included in the report store results
estpost correlate yrsmarr slpnaps , matrix listwise
esttab using "correlation_esttab.csv", unstack not noobs compress replace
eststo clear

*Frequency Table

//Table 3 - yrsmarr

estpost tab yrsmarr
esttab using "yrsmarr_frequency_esttab.csv", cells ("b(label(freq)fmt(0)) pct (fmt(2))") nomtitle nonumber replace
eststo clear

//Table 4 - slpnaps
estpost tab slpnaps
esttab using "slpnaps_frequency_esttab.csv", cells ("b(label(freq)fmt(0)) pct (fmt(2))") nomtitle nonumber replace
eststo clear


*Box Plot to Find Outliers

//Figure 1 - slpnaps
graph hbox slpnaps

//Figure 2 - yrsmarr
graph hbox yrsmarr


*Histogram with Density Overlay and Identified Mean

//Figure 3 - slpnaps
sum slpnaps, mean
twoway (histogram slpnaps, discrete xline(`r(mean)'))(kdensity slpnaps)

//Figure 4 - yrsmarr
sum yrsmarr, mean
twoway (histogram yrsmarr, discrete xline(`r(mean)'))(kdensity yrsmarr)


*Kernel Diagram

//Figure 5 - slpnaps
kdensity slpnaps , kernel(epanechnikov) normal

//Figure 6 - yrsmarr
kdensity yrsmarr , kernel(epanechnikov) normal


*Scatter Plot

//Figure 7 - with unmarried
twoway (scatter yrsmarr slpnaps)(lfit yrsmarr slpnaps)

//Figure 9 - without unmarried
twoway (scatter yrsmarr slpnaps if marr==1)(lfit yrsmarr slpnaps if marr==1)

*Pie Chart
//Figure 8- Checking percent of sample that is married
graph pie, over(marr) plabel(_all percent)



////////////////////////////////////////////////////////////////////////////////
*Inferential Statistics*
////////////////////////////////////////////////////////////////////////////////


*T-tests

//Table 6
ttest yrsmarr == slpnaps, unpaired


*Simple Linear Regression: reg [dependent variable] [independent variable]

//Table 7
reg yrsmarr slpnaps


*Confidence Interval

//Table 5
ci means yrsmarr slpnaps
//Figure 10 - Plotting simple regression using 95% CI
graph twoway (lfitci yrsmarr slpnaps) (scatter yrsmarr slpnaps)


*Linear regression line with 95% confidence interval

//Table 8 - storing regression results using esttab
eststo M1: reg yrsmarr slpnaps, vce(robust)
esttab M1 using "Effect of Sleep on Years Married.csv", b(%4.3f) se(%4.3f) sfmt(%12.0fc) se ar2 stats(N r2 ,fmt(%11.0gc) labels("Obs." "R-squared" )) label nonumber title ("Effect of Sleep on Years Married") mtitle("OLS") replace
eststo clear

//Table 9 - reporting with confidence interval
eststo M1: reg yrsmarr slpnaps, vce(robust)
esttab M1 using "Effect of Sleep on Years Married CI.csv", cells(b(fmt(3)) ci(fmt(3) par) p(fmt(3) par)) collabels(none) label nonumbers mtitles("M1") addnotes("95% confidence intervals in brackets; p-values in parentheses.") replace
eststo clear
