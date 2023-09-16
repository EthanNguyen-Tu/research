/*****************************************************************************************

ECON4803: Behavioral Economics - Scope Sensitivity Semester Project
@authors Ethan Nguyen-Tu and Jacqueline Chambers
@version 1.0.2
@date 22 November 2022

*****************************************************************************************/
clear
set more off
*Working Directory
capture cd "\\Client\C$\Users\jacki\OneDrive\Desktop\Documents\Georgia Tech\Fall 2022\ECON 4803 - Behavioral Econ\Project\STATA"
use ScopeSensitivityData.dta, clear

clear
capture cd "C:\Users\enguyentu3\Downloads\ScopeSensitivity"
import excel "ScopeSensitivityResults.xls", firstrow case(preserve)





**# CLEAN DATA #**
describe
summarize
* check gender count
count if black_or_africanamerican == 1 // 6
count if hispanic_or_latnix == 1 // 5
count if asian_or_pacificislander == 1 // 12
count if white  == 1 // 46
* check education count
count if completed_graduate_degree == 1 // 20
count if completed_bachelors_degree == 1 // 19
count if current_undergrad == 1 // 23
count if high_school_diploma == 1 // 4
count if incomplete_high_school == 1 // 1
count if current_graduatestu == 1 // 2
count if other_education == 1 // 5
// Combine high_school_diploma, incomplete_high_school, and current_graduatestu with other_education
generate other_education2 = other_education + incomplete_high_school + current_graduatestu + high_school_diploma
count if other_education2 == 1 // 12
* check charity count
count if charity_past == 1 // 66
count if charity_future == 1 // 53





**# REGRESSION #**



** Overall Accuracy Score Regression Framework **

/* Figure 1
DEPENDENT VARIABLE: accuracy_score
Variables Left Out:
   Form Type: sml
   Gender: other_gender
   Ethnicity: other_race
   Education: other_education
*/
reg accuracy_score math_activity activity_score msl slm lsm lms mls age male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad high_school_diploma incomplete_high_school current_graduatestu annual_income ess_spend noness_spend charity_past charity_future

* Test form type significance
test msl slm lsm lms mls // msl slm lsm lms mls are are not jointly significant
di invFtail(5, 30, .05) // 2.5335545


/* Figure 2
DEPENDENT VARIABLE: accuracy_score
Variables Left Out:
   Form Type: all
   Gender: other_gender
   Ethnicity: other_race
   Education: other_education
*/
reg accuracy_score math_activity activity_score male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad high_school_diploma incomplete_high_school current_graduatestu annual_income ess_spend noness_spend charity_past charity_future


/* Figure 3
DEPENDENT VARIABLE: accuracy_score
Variables Left Out:
   Form Type: all
   Gender: other_gender
   Ethnicity: other_race
   Education: other_education2
*/
reg accuracy_score math_activity activity_score male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad annual_income ess_spend noness_spend charity_past charity_future

* Test charity variables significance
test charity_past charity_future
di invFtail(2, 39, .05) // 3.2380961


/* Figure 4
DEPENDENT VARIABLE: accuracy_score
Variables Left Out:
   Form Type: all
   Gender: other_gender
   Ethnicity: other_race
   Education: other_education2
   Charity: charity_past & charity_future
*/
reg accuracy_score math_activity activity_score male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad annual_income ess_spend noness_spend

test ess_spend noness_spend
di invFtail(2, 52, .05) // 3.175141


/* Figure 5
DEPENDENT VARIABLE: accuracy_score
Variables Left Out:
   Form Type: all
   Gender: other_gender
   Ethnicity: other_race
   Education: other_education2
   Charity: charity_past & charity_future
   Spending: ess_spend & noness_spend
*/
reg accuracy_score math_activity activity_score male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad annual_income

* Test Ethnicity
test black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white 
di invFtail(4, 54, .05) // 2.5429175

* Test Education
test completed_graduate_degree completed_bachelors_degree current_undergrad
di invFtail(3, 54, .05) // 2.7757624

* Test Gender
test male female
di invFtail(2, 54, .05) // 3.168246


/* Check Regression 1
DEPENDENT VARIABLE: accuracy_score
Variables Left Out:
   Form Type: all
   Gender: all
   Ethnicity: other_race
   Education: other_education2
   Charity: all
   Spending: all
*/
reg accuracy_score math_activity activity_score black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad annual_income


/* Figure 6
DEPENDENT VARIABLE: accuracy_score
Variables Left Out:
   Form Type: all
   Gender: all
   Ethnicity: other_race
   Education: all
   Charity: all
   Spending:all
*/
reg accuracy_score math_activity activity_score black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white annual_income

* Test Ethnicity
test black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white 
di invFtail(4, 59, .05) // 2.5279066


* Figure 7 - Base Regression
reg accuracy_score math_activity activity_score

/* Overall Accuracy Score Conclusion
Cannot conclude significance.
*/



** Bird Regression Framework **

* linear-linear model
* variables left out: sml, other_gender, other_race, other_education
* dependent var: accuracy_bird
reg accuracy_bird math_activity activity_score msl slm lsm lms mls age male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad high_school_diploma incomplete_high_school current_graduatestu annual_income ess_spend noness_spend charity_past charity_future

* Test 'form type' significance
test msl slm lsm lms mls // msl slm lsm lms mls are not jointly significant
di invFtail(5, 30, .05) // 2.5335545
// form type is not significant in this regression


* log-linear model
* independent vars: no change
* dependent var: logAccBird
generate logAccBird = ln(accuracy_bird)
reg logAccBird math_activity activity_score msl slm lsm lms mls age male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad high_school_diploma incomplete_high_school current_graduatestu annual_income ess_spend noness_spend charity_past charity_future

* Test 'form type' significance
test msl slm lsm lms mls // msl slm lsm lms mls are not jointly significant
di invFtail(5, 30, .05) // 2.5335545
// form type is not significant in this regression


* linear-log model & log-log model
/* independent vars:
logActSco - newly generated
logAnnInc - newly generated
logEssSpe - newly generated
logNEssSpe - newly generated
*/ 
generate logActSco = ln(activity_score)
generate logAnnInc = ln(annual_income)
generate logEssSpe = ln(ess_spend)
generate logNEssSpe = ln(noness_spend)
* dependent var: accuracy_bird
reg accuracy_bird math_activity logActSco msl slm lsm lms mls age male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad high_school_diploma incomplete_high_school current_graduatestu logAnnInc logEssSpe logNEssSpe charity_past charity_future

* Test 'form type' significance
test msl slm lsm lms mls // msl slm lsm lms mls are not jointly significant
di invFtail(5, 30, .05) // 2.5335545
// form type is not significant in this regression

* dependent var: logAccBird
reg logAccBird math_activity logActSco msl slm lsm lms mls age male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad high_school_diploma incomplete_high_school current_graduatestu logAnnInc logEssSpe logNEssSpe charity_past charity_future

* Test 'form type' significance
test msl slm lsm lms mls // msl slm lsm lms mls are not jointly significant
di invFtail(5, 30, .05) // 2.5335545
// form type is not significant in this regression



** Turtle Regression Framework **

* linear-linear model
* independent variables left out: sml, other_gender, other_race, other_education
* dependent var: accuracy_turtle
reg accuracy_turtle math_activity activity_score msl slm lsm lms mls age male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad high_school_diploma incomplete_high_school current_graduatestu annual_income ess_spend noness_spend charity_past charity_future

* Test 'form type' significance
test msl slm lsm lms mls // msl slm lsm lms mls are not jointly significant
di invFtail(5, 30, .05) // 2.5335545
// form type is not significant in this regression


* log-linear model
* independent vars: no change
* dependent var: logAccTurtle
generate logAccTurtle = ln(accuracy_turtle)
reg logAccTurtle math_activity activity_score msl slm lsm lms mls age male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad high_school_diploma incomplete_high_school current_graduatestu annual_income ess_spend noness_spend charity_past charity_future

* Test 'form type' significance
test msl slm lsm lms mls // msl slm lsm lms mls are not jointly significant
di invFtail(5, 30, .05) // 2.5335545
// form type is not significant in this regression


* linear-log & log-log
* independent vars: no change
* dependent var: accuracy_turtle
reg accuracy_turtle math_activity logActSco msl slm lsm lms mls age male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad high_school_diploma incomplete_high_school current_graduatestu logAnnInc logEssSpe logNEssSpe charity_past charity_future

* Test 'form type' significance
test msl slm lsm lms mls // msl slm lsm lms mls are not jointly significant
di invFtail(5, 30, .05) // 2.5335545
// form type is not significant in this regression

* dependent var: logAccTurtle
reg logAccTurtle math_activity logActSco msl slm lsm lms mls age male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad high_school_diploma incomplete_high_school current_graduatestu logAnnInc logEssSpe logNEssSpe charity_past charity_future

* Test 'form type' significance
test msl slm lsm lms mls // msl slm lsm lms mls are not jointly significant
di invFtail(5, 30, .05) // 2.5335545
// form type is not significant in this regression



** Human Regression Framework **

* linear-linear model
* variables left out: sml, other_gender, other_race, other_education
* dependent var: accuracy_human
reg accuracy_human math_activity activity_score msl slm lsm lms mls age male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad high_school_diploma incomplete_high_school current_graduatestu annual_income ess_spend noness_spend charity_past charity_future

* Test 'form type' significance
test msl slm lsm lms mls // msl slm lsm lms mls are not jointly significant
di invFtail(5, 30, .05) // 2.5335545
// form type is not significant in this regression


* log-linear model
* independent vars: no change
* dependent var: logAccHuman
generate logAccHuman = ln(accuracy_human)
reg logAccHuman math_activity activity_score msl slm lsm lms mls age male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad high_school_diploma incomplete_high_school current_graduatestu annual_income ess_spend noness_spend charity_past charity_future

* Test 'form type' significance
test msl slm lsm lms mls // msl slm lsm lms mls are jointly significant
di invFtail(5, 30, .05) // 2.5335545
// form type is significant in this regression!


* linear-log & log-log
* independent vars: no change
* dependent var: accuracy_human
reg accuracy_human math_activity logActSco msl slm lsm lms mls age male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad high_school_diploma incomplete_high_school current_graduatestu logAnnInc logEssSpe logNEssSpe charity_past charity_future

* Test 'form type' significance
test msl slm lsm lms mls // msl slm lsm lms mls are not jointly significant
di invFtail(5, 30, .05) // 2.5335545
// form type is not significant in this regression


* dependent var: logAccHuman
reg logAccHuman math_activity logActSco msl slm lsm lms mls age male female black_or_africanamerican hispanic_or_latnix asian_or_pacificislander white completed_graduate_degree completed_bachelors_degree current_undergrad high_school_diploma incomplete_high_school current_graduatestu logAnnInc logEssSpe logNEssSpe charity_past charity_future

* Test 'form type' significance
test msl slm lsm lms mls // msl slm lsm lms mls are jointly significant
di invFtail(5, 30, .05) // 2.5335545
// form type is significant in this regression!



// Uncomment below if new variables have been added or variables have been modified
*export excel using "ScopeSensitivityResults.xls", firstrow(variables) keepcellfmt replace

// END OF DOCUMENT