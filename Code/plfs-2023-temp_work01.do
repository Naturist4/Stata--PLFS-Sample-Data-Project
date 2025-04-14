// Start with clean slate
cls

* ---------------------------------------------
* Name: Prabodh GEDAM
* Project: PLFS 2023 for code sample
* 2025-02-05, ver. 01
* Dataset: PLFS 2023 (Jan 2023 - Dec 2023)
* Descriptive Statistics
* plfs-2023-temp_work01.do
* ---------------------------------------------

* ---------------------------------------------
* INDEX
* 
* Line 61
* 	Eduactional attainment by Gender
*		- using Descriptive table
*		- using graph visualization
* Line 106
*	Educational attainment by Gender, comparison across select States
*		- using Loop method for generating descriptive tables
*		- using Loop method for generating graph visualization
* Line 133 
* 	Impact of select independent variable on Employment (dependent variable)
*		- recoding variables for proper analysis
*		- Probit model utilization for binary variable Employment
*		- examining interaction effect
* Line 180
*	Impact of various factors on Earnings
*		- 6 regression models testing various situations
* Line 250
*	Appendix
*		- Data Cleaning code
* ---------------------------------------------

* ---------------------------------------------
* PROGRAM SETUP
* Stata/MP 14.0 (32-bit)
* Windows 10 in Virtual Machine
* Host: Fedora Linux Workstation 41
* Host: GNOME Boxes

// Line size limit to make output more readable
set linesize 80

// Setting up Directory
cd "C:\Users\pgeda\Documents\stata-appdata"

// Starting Log File
log using "C:\Users\pgeda\Documents\stata-appdata\plfs23-temp_work.smcl", append

// Importing dataset PLFS 2023 (Jan 2023 - Dec 2023)
use "C:\Users\pgeda\Documents\stata-appdata\plfs-2023-23.dta", clear
* ---------------------------------------------





* ========== Education Status by Gender (Sex) ==========

// Observing B4Q8 (General Education Level). it has 13 categories.
// this makes anlysis cumbersome to handle.
label list		b4q8_label

// Generating new variable by combining similar categories of B4Q8
generate	educat = . 								// null "." is default		
replace 	educat = 1 if inlist(b4q8, 1, 2, 3, 4, 5) 	
replace 	educat = 2 if inlist(b4q8, 6, 7) 		
replace 	educat = 3 if inlist(b4q8, 8, 10) 		
replace 	educat = 4 if inlist(b4q8, 11, 12, 13)

label variable		educat		"Education Level (recategorized)"
label define		educat_label	1 "Illiterate & Below Primary" ///
		2 "Primary & Middle School"	3 "Secondary & Higher School" ///
		4 "Higher Education"
label values		educat		educat_label


// Observing level of education trends across sex
tabulate	educat	b4q5,	row column


// Plotting a bar graph for comparison
graph hbar, 		/// hbar plots the bars horizontally
	  over(b4q5) 		/// b4q5(:Sex) is plotted first
	  over(educat) 	/// educat(: Education Level) is larger category
	  asyvars 		/// treats first over() as yvars. splits education per sex
	  nofill 			/// removes category if has no value
	  blabel(bar, format(%9.1g) gap(1.2) color(black)) ///
	  title("Educational Attainment as per Sex")
	  /*stack 		// this stacks the bars in single line */


/* ~~~~~~~~~~ attempted vertical bars. Not clean. ~~~~~~~~~~
graph bar, 			///
	over(educat) 	///
	over(b4q5) 		///
	asyvars 		///
	nofill 			///
	blabel(bar, format(%9.1g) gap(1.2)) //
~~~~~~~~~~ */


* ========== Education Status by State (Sex) ==========
codebook state_ut1
label list state_ut_label

// Maharastra (27) in comparison to nearby states Madhya Pradesh (23),
//						Karnataka (29), and Gujarat (24)
foreach s in 27 23 29 24 {
    di "State: `s'"
    tabulate educat b4q5 if state_ut1 == `s', row column
}


// Graphical visualization of the same in 4 separate graphs
foreach s in 27 23 29 24 {
    di "State: `s'"
    graph hbar (percent) if state_ut1 == `s', 	///
	over(b4q5) over(educat) 					///
	asyvars										///
	blabel(bar, format(%9.1g) gap(1.2) color(black))	///
	title("Education Level by Gender - State `s'") 		///
    ytitle("Percentage") 								///
	name(state_`s', replace) 		// every graph needs unique title to keep each graph window open 
}




* ========== Impact of Education, Sex, Marital Status, Sector on Employment ==========

// generating new variable `EMP_STATUS` based on variable B6Q5 (Current Weekly Status)
// B6Q5 has 21 categories that can be combined into new variable

label list b6q5_label		// observing variable and categories


generate byte	emp_status = .
replace 		emp_status = 0 if inlist(b6q5, 61, 62, 71, 72, 81, 82, 91, 98, 94, 95)	// Unemployed
replace 		emp_status = 1 if inlist(b6q5, 11, 12, 21, 31, 41, 42, 51, 92, 93, 97)	// Employed

label variable	emp_status			"Employment Status"
label define	emp_status_label	0 "Unemployed"	1 "Employed"
label values	emp_status 			emp_status_label

// generating new variable 'TRAINING' based on variable `B4Q12`
// 'B4Q12' has 6 categories which can be combined meaningfully

label list		b4q12_label

generate byte	training	= .
replace			training	= 1 if inlist(b4q12, 1, 2, 3, 4, 5)		// Yes
replace			training	= 2 if inlist(b4q12, 6)					// No

label variable	training			"Training Received or Not"
label define	training_label		1 "Yes"	2 "No"
label values	training			training_label


// Dependent `EMP_STATUS` is binary variable.
// Independent `EDUCAT` (education level), `B4Q5` (gender), B4Q7 (Sex),
//		and `TRAINING` (training received or not) are all categorical variables
// hence we use Probit model to determine the effect of independent variables
probit	emp_status	educat	b4q5	b4q7	training, robust

margins, dydx(*)			// exploring marginal effects per variable


// training might have some interaction with level of education on employement
// 		adding interaction term `C.EDUCAT#C.TRAINING`
probit emp_status educat b4q5 b4q7 training c.educat#c.training, robust





* ========== Impact of various factors on Earnings ==========

// Model 1
regress b6q9	educat	training	b4q5	b4q6	b4q7, robust


// Model 2
// Incorporating non-linear effect on B4Q6 (Age)
gen age2 = b4q6^2


// Model 3
// Incorporating interaction effect between educational level & training received
gen educat_training = educat * training


// Model 4
regress b6q9 educat training b4q5 b4q6 age2 b4q7 educat_training, robust
/* ~~~~~~~~~~ 	
	b6q9: Earnings;			educat: level of education;
	training: Yes or No;
	b4q5: Sex (1:male, 2:female, 3:transgender)
	b4q6: age;				b4q7: Marital status
~~~~~~~~~~ */


// Model 5
regress b6q9 b4q10 b4q5 b4q6 age2 b4q7, robust
/* ~~~~~~~~~~ 	
	b6q9: Earnings;			b4q10: No of Years in Formal Education;
	b4q5: Sex (1:male, 2:female, 3:transgender)
	b4q6: age;				b4q7: Marital status
~~~~~~~~~~ */

// Model 6 - Incorporating Fixed Effects by States
regress b6q9 b4q10 b4q5 b4q6 age2 b4q7 i.state_ut1, robust


































* ========== Appendix - Data Cleaning ==========


* ---------------------------------------------
* Name: Prabodh GEDAM
* Project: PLFS 2023 for code sample
* 2025-02-03, ver. 01
* Dataset: PLFS 2023 (Jan 2023 - Dec 2023)
* Data cleaning
* plfs-2023-datacleaning.do
* Initial Variables 140
* Final Variables 55
* ---------------------------------------------


* ---------------------------------------------
* PROGRAM SETUP
* Stata/MP 14.0 (32-bit)
* Windows 10 in Virtual Machine
* Host: Fedora Linux Workstation 41
* Host: GNOME Boxes

// Start with clean slate
cls

// Setting up Directory
cd "C:\Users\pgeda\Documents\stata-appdata"

// Starting Log File
log using "C:\Users\pgeda\Documents\stata-appdata\plfs23-datacleaning.smcl"

// Importing dataset PLFS 2023 (Jan 2023 - Dec 2023)
use "C:\Users\pgeda\Documents\stata-appdata\plfs-2023-23.dta", clear
* ---------------------------------------------


// Browsing dataset
browse					// lot of variables with string-type values

// Browsing available labels
label	list


* ---------------------------------------------
* NOTES ON FIRST OBSERVATION
* almost all variables and their observations are coded as string type by Stata
* during import.
* out of 140 variables, many variables has nature of "microdata" and are
* not required for my personal purpose.
* the export procedure faced issues and the labele values were not
* exported/imported for almost all variables.
* the associated data dictionary has detailed information about label values.
* the data disctionary is not exportable into csv or any other format.
* only option is to create lable values.
* ---------------------------------------------


* ========== Reworking variable PANEL_CPERV1 (Panel) ==========
describe	panel_cperv1
tab			panel_cperv1

// Converting PANEL_CPERV1 from String to byte, by GENERATING clone variable
gen byte	panel_num = .				// null (.) default
replace		panel_num = 1 	if panel_cperv1 == "P3"
replace 	panel_num = 2	if panel_cperv1 == "P4"

label variable 	panel_num "Survey Panel"	// Labeling variable
label define	panel_label 1 "P3" 2 "P4"	// Defining value label
label values 	panel_num panel_label		// applying value label

tabulate	panel_cperv1	panel_num		// Cross-examining data integrity
codebook	panel_num
drop		panel_cperv1

// Dropping variables (FI_cperv1 :File Identification) (B1q2_cperv1 :Schedule)
drop	FI_cperv1 B1q2_cperv1		


* ========== Reworking variable QTR_CPERV1 (Quarter) ==========
// Converting from String to byte first, by GENERATING clone
gen byte	qtr = .		// null (.) default
replace 	qtr = 1 	if qtr_cperv1 == "Q1"
replace 	qtr = 2		if qtr_cperv1 == "Q2"
replace 	qtr = 7		if qtr_cperv1 == "Q7"
replace 	qtr = 8		if qtr_cperv1 == "Q8"

label variable		qtr			"Quarter"
label define		qtr_label	1 "Q1" 2 "Q2" 7 "Q7" 8 "Q8"
label values 		qtr 		qtr_label

tab		qtr_cperv1	qtr		// Cross-examining data integrity
drop 	qtr_cperv1			// Dropped String QTR_CPERV1


* ========== Reworking variable VISIT_CPERV1 (Visit) ==========
codebook	visit_cperv1

gen byte	visit = .
replace		visit = 1	if visit_cperv1 == "V1"

label variable	visit			"Visit"
label define	visit_label		1 "V1"
label values 	visit 			visit_label

tab 	visit_cperv1	visit
drop 	visit_cperv1


* ========== Reworking variable B1Q3_CPERV1 (Sector: Rural & Urban) ==========
codebook b1q3_cperv1

/* ~~~~~~~~~~ Commented Out Code-block ~~~~~~~~~~
encode		b1q3_cperv1,	gen(sector)		// generated 'long' type data.
drop 		sector
~~~~~~~~~~ */

gen byte	sector = .
replace 	sector = 1		if b1q3_cperv1 == "1"
replace 	sector = 2 		if b1q3_cperv1 == "2"

label variable	sector			"Sector"
label define	sector_label 	1 "Rural" 2 "Urban"
label values 	sector			sector_label

tab b1q3_cperv1 sector
drop b1q3_cperv1


* ========== Reworking variable STATE_CPERV1 (State/UT Codes) ==========
codebook	state_cperv1

/* ~~~~~~~~~~ Commented Out Code-block ~~~~~~~~~~
// Encoding the String variables into a Numberic variable
encode	state_cperv1,	gen(state_ut1)

// Recasting the new variable into byte
recast	byte	state_ut1

// ========== ERROR NOTES ==========
// This method is messing up everything. STATE_CPERV1 does NOT have VALUE`==26`
// ENCODE command is shifting all values from 27 onwards onto 26 and ahead.
// better to use DESTRING
~~~~~~~~~~ */

destring	state_cperv1,	generate(state_ut1) force

label variabl	state_ut1		"State and UT Code"
label define	state_ut_label		1 "Jammu & Kashmir"	2 "Himachal Pradesh" /// 
	3 "Punjab"		4 "Chandigarh"	5 "Uttarakhand"		6 "Haryana" ///
	7 "Delhi"		8 "Rajasthan"	9 "Uttar Pradesh"	10 "Bihar" ///
	11 "Sikkim" 	12 "Arunachal Pradesh"				13 "Nagaland" ///
	14 "Manipur" 	15 "Mizoram"	16 "Tripura"		17 "Meghalaya" ///
	18 "Assam"		19 "West Bengal" 20 "Jharkhand"		21 "Odisha" ///
	22 "Chhattisgarh"				23 "Madhya Pradesh" 24 "Gujarat" ///
	25 "D & N. Haveli & Daman & Diu" 26 "none"			27 "Maharashtra" ///
	28 "Andhra Pradesh"				29 "Karnaaka" 		30 "Goa" ///
	31 "Lakshadweep" 32 "Kerala"	33 "Tamilnadu" 		34 "Puduchery" ///
	35 "Andaman & N. Island"		36 "Telangana"		37 "Ladakh"
label values	state_ut		state_ut_label

tabulate	state_cperv1	state_ut
drop		state_cperv1


* ========== Reworking multiple variable ==========
// Destring into Numeric varialbes
destring	distcode_cperv1,	gen(distcode) force
destring	nss_region_cperv1, 	gen(nss_region) force

destring	b1q5_cperv1,		gen(stratum) force
destring	b1q6_cperv1,		gen(substratum) force
destring	b1q11_cperv1,		gen(subsample) force
destring	b1q12_cperv1,		gen(fod_subregion) force
destring	b1q1_cperv1,		gen(fsu) force
destring	b1q13_cperv1,		gen(sample_sgsb) force
destring	b1q14_cperv1,		gen(st2stratum) force
destring	b1q15_cperv1,		gen(sample_hhid) force

// Dropping unusable String variables
drop	distcode_cperv1		nss_region_cperv1	b1q5_cperv1		b1q6_cperv1 ///
		b1q11_cperv1		b1q12_cperv1		b1q1_cperv1		b1q13_cperv1 ///
		b1q14_cperv1		b1q15_cperv1


* ========== Reworking variable B4PT1Q3_CPERV1 ==========
* 		(Whether Training complete during last 365 Days)
codebook		b4pt1q3_cperv1

destring		b4pt1q3_cperv1,		gen(b4pt1q3) force

label define	b4pt1q3_label		1 "Yes" 2 "No"
label values	b4pt1q3				b4pt1q3_label

tabulate		b4pt1q3_cperv1		b4pt1q3
codebook		b4pt1q3
drop 			b4pt1q3_cperv1


* ========== Reworking variable B4Q1_CPERV1 (Person Serial No.) ==========
codebook		b4q1_cperv1
destring		b4q1_cperv1,		gen(b4q1) force
drop			b4q1_cperv1


* ========== Reworking variable B4Q4_CPERV1 (Relationship to Head) ==========
codebook		b4q4_cperv1
destring		b4q4_cperv1,		gen(b4q4) force

label define	b4q4_label	1 "self"	2 "spouse of head" /// 
	3 "married child"		4 "spouse of married child" /// 
	5 "unmarried child"		6 "grandchild" ///
	7 "father/mother/father-in-law/mother-in-law" /// 
	8 "brother/sister/brother-in-law/ sister-in-law/other relatives" ///
	9 "servants/employees/other non-relatives"
label values	b4q4		b4q4_label

tabulate	b4q4_cperv1	b4q4
drop		b4q4_cperv1


* ========== Reworking variable B4Q5_CPERV1 (Sex) ==========
codebook		b4q5_cperv1
destring		b4q5_cperv1,		gen(b4q5) force

label define	b4q5_label	1 "Male" 2 "Female" 3 "Transgender"
label values	b4q5		b4q5_label

tabulate	b4q5_cperv1	b4q5
drop		b4q5_cperv1


* ========== Reworking variable B4Q6_CPERV1 (Age) ==========
codebook		b4q6_perv1
destring		b4q6_perv1,		gen(b4q6) force
drop			b4q6_perv1


* ========== Reworking variable B4Q7_CPERV1 (Marital Status) ==========
codebook		b4q7_cperv1
destring		b4q7_cperv1,		gen(b4q7) force

label define	b4q7_label	1 "Never Married" 2 "Currently Married" ///
							3 "Widowed" 4 "Divorced/Separated"
label values	b4q7		b4q7_label

tabulate	b4q7_cperv1	b4q7
drop		b4q7_cperv1


* ========== Reworking variable B4Q8_CPERV1 (General Education Level) ==========
codebook		b4q8_cperv1
destring		b4q8_cperv1,		gen(b4q8) force

label define	b4q8_label	1 "Not Literate" /// 
	2 "Literate without formal schooling (EGS/NFEC/AEC)" /// 
	3 "Literate without formal schooling (TLC)" 4 "Others" ///
	5 "Below Primary" 6 "Primary" 7 "Middle" 8 "Secondary" ///
	10 "Higher Secondary" 11 "Diploma/Certificate Course" ///
	12 "Graduate" 13 "Postgraduate and above"
label values	b4q8		b4q8_label

tabulate	b4q8_cperv1	b4q8
drop		b4q8_cperv1


* ========== Reworking variable B4Q9_CPERV1 (Technical Education Level) ==========
codebook		b4q9_cperv1
destring		b4q9_cperv1,		gen(b4q9) force

label define	b4q9_label	1 "no technical education" ///
	2 "technical degree in agriculture" ///
	3 "technical degree in engineering/technology" ///
	4 "technical degree in medicine" ///
	5 "technical degree in crafts" ///
	6 "technical degree in other subjects" ///
	7 "diploma or certificate (below graduate level) in agriculture" ///
	8 "diploma or certificate (below graduate level) in engineering/technology" ///
	9 "diploma or certificate (below graduate level) in medicine" ///
	10 "diploma or certificate (below graduate level) in crafts" ///
	11 "diploma or certificate (below graduate level) in other subjects" ///
	12 "diploma or certificate (graduate and above level) in agriculture" ///
	13 "diploma or certificate (graduate and above level) in engineering/technology" ///
	14 "diploma or certificate (graduate and above level) in medicine" /// 
	15 "diploma or certificate (graduate and above level) in crafts" /// 
	16 "diploma or certificate (graduate and above level) in other subjects"
label values	b4q9		b4q9_label

tabulate	b4q9_cperv1	b4q9
drop		b4q9_cperv1


* ========== Reworking variable B4Q10_CPERV1 ==========
*					(No. of years in formal education)
codebook	b4q10_cperv1
rename 		b4q10_cperv1	b4q10


* ========== Reworking variable B4Q11_CPERV1 ==========
* (Status of Current Attendance in Educational Institution)
codebook		b4q11_cperv1
destring		b4q11_cperv1,	gen(b4q11) force

label define	b4q11_label ///
	1 "currently not attending, never attended: school too far" ///
	2 "currently not attending, never attended: to supplement household income" /// 
	3 "currently not attending, never attended: education not considered necessary" ///
	4 "currently not attending, never attended: to attend domestic chores" ///
	5 "currently not attending, never attended: others" /// 
	11 "ever attended but currently not attending: school too far" /// 
	12 "ever attended but currently not attending: to supplement household income" /// 
	13 "ever attended but currently not attending: education not considered necessary" /// 
	14 "ever attended but currently not attending: to attend domestic chores" /// 
	15 "ever attended but currently not attending: others" /// 
	21 "currently attending in: EGS/NFEC/AEC" /// 
	22 "currently attending in: TLC" ///
	23 "currently attending in: pre-primary (nursery/Kindergarten, etc.)" /// 
	24 "currently attending in: primary (class I to IV/V)" /// 
	25 "currently attending in: middle" /// 
	26 "currently attending in: secondary" /// 
	27 "currently attending in: higher secondary" ///
	28 "graduate in: agriculture" ///
	29 "graduate in: engineering/technology" /// 
	30 "graduate in: medicine" ///
	31 "graduate in: other subjects" /// 
	32 "graduate in: post graduate and above" /// 
	33 "diploma or certificate (below graduate level) in: agriculture" /// 
	34 "diploma or certificate (below graduate level) in: engineering/technology" /// 
	35 "diploma or certificate (below graduate level) in: medicine" /// 
	36 "diploma or certificate (below graduate level) in: crafts" /// 
	37 "diploma or certificate (below graduate level) in: other subjects" ///
	38 "diploma or certificate (graduate level) in: agriculture" /// 
	39 "diploma or certificate (graduate level) in: engineering/technology" /// 
	40 "diploma or certificate (graduate level) in: medicine" /// 
	41 "diploma or certificate (graduate level) in:crafts" ///
	42 "diploma or certificate (graduate level) in: other subjects" /// 
	43 "diploma or certificate in post graduate and above level-"
label values	b4q11		b4q11_label

tabulate	b4q11_cperv1	b4q11
drop		b4q11_cperv1


* ========== Reworking variable B4Q12_CPERV1 ==========
*	(Whether received any Vocational/Technical Training)
codebook	b4q12_cperv1
destring	b4q12_cperv1,	gen(b4q12) force

label define	b4q12_label 1 "yes: received formal vocational/technical training" ///
	2 "received vocational/technical training other than formal vocational/technical training hereditary" ///
	3 "received vocational/technical training other than formal vocational/technical training self-learning" ///
	4 "received vocational/technical training other than formal vocational/technical training learning on the job" ///
	5 "received vocational/technical training other than formal vocational/technical trainingothers" ///
	6 "did not receive any vocational/technical training"
label values	b4q12		b4q12_label

tabulate	b4q12_cperv1	b4q12
drop		b4q12_cperv1


* ========== Reworking variable B4PT1Q4_CPERV1 (Field of Training) ==========
codebook	b4pt1q4_cperv1
destring	b4q12_cperv1,	gen(b4q12) force

label define	b4q12_label 1 "yes: received formal vocational/technical training" ///
	2 "received vocational/technical training other than formal vocational/technical training hereditary" ///
	3 "received vocational/technical training other than formal vocational/technical training self-learning" ///
	4 "received vocational/technical training other than formal vocational/technical training learning on the job" ///
	5 "received vocational/technical training other than formal vocational/technical trainingothers" ///
	6 "did not receive any vocational/technical training"
label values	b4q12		b4q12_label

tabulate	b4q12_cperv1	b4q12
drop		b4q12_cperv1


* ========== Dropping multiple unrequired variable ==========
drop	nss_region					/// NSS-region
		b1q5						/// Stratum
		b1q6						/// Sub-stratum
		b1q11						/// Sub-sample
		b1q12						/// Fod Sub-region
		b1q1						/// FSU
		b1q13						/// Sample Sg/Sb No.
		b1q14						/// Second State Stratum No
		visit						/// Visit
		no_qtr_cperv1				/// Occurance of FSUs in State * Sector * Stratum * SubStratum in 4 Quarters
		mult_cperv1					/// Subsample wise Multiplier
		nsc_cperv1					/// Ns count for sector * stratum * substratum
		nss_cperv1					/// Ns count for sector * stratum * substratum * sub-sample
		b5pt2q13					///		
		b6q4_3pt1_cperv1			///
		b6q4_3pt2_cperv1			/// 
		b6q4_3pt3_cperv1 			///
		b6q4_3pt4_cperv1 			/// 
		b6q4_3pt5_cperv1 			/// 
		b6q4_3pt6_cperv1 			/// 
		b6q4_3pt7_cperv1 			///
		b6q4_act2_3pt1_cperv1 		/// 
		b6q4_act2_3pt2_cperv1 		///
		b6q4_act2_3pt3_cperv1		/// 
		b6q4_act2_3pt4_cperv1		/// 
		b6q4_act2_3pt5_cperv1 		/// 
		b6q4_act2_3pt6_cperv1 		/// 
		b6q4_act2_3pt7_cperv1		/// 
		b6q5_3pt1_cperv1			///
		b6q5_3pt2_cperv1			///
		b6q5_3pt3_cperv1			///
		b6q5_3pt4_cperv1			///
		b6q5_3pt5_cperv1			///
		b6q5_3pt6_cperv1			///
		b6q5_3pt7_cperv1			///
		b6q5_act2_3pt1_cperv1		///
		b6q5_act2_3pt2_cperv1		///
		b6q5_act2_3pt3_cperv1		///
		b6q5_act2_3pt4_cperv1		///
		b6q5_act2_3pt5_cperv1		///
		b6q5_act2_3pt6_cperv1		///
		b6q5_act2_3pt7_cperv1		///
		b6q6_3pt1_cperv1			///
		b6q6_3pt2_cperv1			///
		b6q6_3pt3_cperv1			///
		b6q6_3pt4_cperv1			///
		b6q6_3pt5_cperv1			///
		b6q6_3pt6_cperv1			///
		b6q6_3pt7_cperv1			///
		b6q6_act2_3pt1_cperv1		///
		b6q6_act2_3pt2_cperv1		///
		b6q6_act2_3pt3_cperv1		///
		b6q6_act2_3pt4_cperv1		///
		b6q6_act2_3pt5_cperv1		///
		b6q6_act2_3pt6_cperv1		///
		b6q6_act2_3pt7_cperv1		///
		b6q7_3pt2_cperv1			///
		b6q7_3pt3_cperv1			///
		b6q7_3pt4_cperv1			///
		b6q7_3pt5_cperv1			///
		b6q7_3pt6_cperv1			///
		b6q7_act2_3pt1_cperv1		///
		b6q7_act2_3pt7_cperv1		///
		b6q8_3pt1_cperv1			///
		b6q8_3pt2_cperv1			///
		b6q8_3pt3_cperv1			///
		b6q8_3pt4_cperv1			///
		b6q8_3pt5_cperv1			///
		b6q8_3pt6_cperv1			///
		b6q8_act2_3pt7_cperv1		///
		b6q9_3pt1_cperv1			///
		b6q9_3pt2_cperv1			///
		b6q9_3pt3_cperv1			///
		b6q9_3pt4_cperv1			///
		b6q9_3pt5_cperv1			///
		b6q9_3pt6_cperv1			///
		b6q9_3pt7_cperv1			///
		b6q9_act2_3pt1_cperv1		///
		b6q9_act2_3pt2_cperv1		///
		b6q9_act2_3pt3_cperv1		///
		b6q9_act2_3pt4_cperv1		///
		b6q9_act2_3pt5_cperv1		///
		b6q9_act2_3pt6_cperv1		///
		b6q9_act2_3pt7_cperv1		// Wage earning for activity 2 on 1st day


* ========== Reqorking B4PT1Q4_CPERV1 (Field of Training) ==========
codebook	b4pt1q4_cperv1
list 		b4pt1q4_cperv1		in 1/5		// tried to see value. try BROWSE
destring	b4pt1q4_cperv1, 	gen(b4pt1q4) force

label define	b4pt1q4_label 1 "aerospace and aviation" ///
	2 "agriculture, non-crop based agriculture, food processing" ///
	3 "allied manufacturing- gems and jewellery, leather, rubber, furniture and fittings, printing" ///
	4 "artisan/craftsman/handicraft/creative arts and cottage based production" ///
	5 "automotive"		6 "beauty and wellness" ///
	7 "chemical engineering, hydrocarbons, chemicals and petrochemicals" ///
	8 "civil engineering- construction, plumbing, paints and coatings" ///
	9 "electrical, power and electronics"		10 "healthcare and life sciences" ///
	11 "hospitality and tourism" ///
	12 "iron and steel, mining, earthmoving and infra building" ///
	13 "IT-ITeS"		14 "logistics" ///
	15 "mechanical engineering-capital goods, strategic manufacturing" ///
	16 "media-journalism, mass communication and entertainment" ///
	17 "office and business related work" ///
	18 "security"		19 "telecom"		20 "textiles and handlooms, apparels" ///
	21 "work related to childcare, nutrition, pre-school and crèche" ///
	99 "other"
label values	b4pt1q4		b4pt1q4_label

tabulate	b4pt1q4_cperv1	b4pt1q4
drop		b4pt1q4_cperv1


* ========== Reqorking B4PT1Q5_CPERV1 (Duration of Training) ==========
codebook	b4pt1q5_cperv1
rename		b4pt1q5_cperv1		b4pt1q5


* ========== Reqorking B4PT1Q6_CPERV1 (Type of Training) ==========
codebook	b4pt1q6_cperv1
destring	b4pt1q6_cperv1,		gen(b4pt1q6) force

label define	b4pt1q6_label		1 "on the job" ///
		2 "other than on the job: part time" ///
		3 "other than on the job: full time"
label values	b4pt1q6		b4pt1q6_label

tabulate	b4pt1q6_cperv1	b4pt1q6
drop		b4pt1q6_cperv1


* ========== Reqorking B4PT1Q7_CPERV1 (Source of Funding of Training) ==========
codebook	b4pt1q7_cperv1
destring	b4pt1q7_cperv1,		gen(b4pt1q7) force

label define	b4pt1q7_label		1 "Govt"	2 "Own funding" ///
									3 "Other"
label values	b4pt1q7		b4pt1q7_label

tabulate	b4pt1q7_cperv1	b4pt1q7
drop		b4pt1q7_cperv1


* ========== Reqorking B5PT1Q3_CPERV1 (Status Code) ==========
codebook	b5pt1q3_cperv1
list		b5pt1q3_cperv1	in 1/5
destring	b5pt1q3_cperv1,		gen(b5pt1q3) force

label variable	b5pt1q3				"Status Code (Principal)"
label define	b5pt1q3_label		11 "worked in h.h. enterprise (self-employed): own account worker" ///
		12 "worked in h.h. enterprise (self-employed): employer" ///
		21 "worked as helper in h.h. enterprise (unpaid family worker)" ///
		31 "worked as regular salaried/ wage employee" ///
		41 "worked as casual wage labour: in public works" ///
		51 "in other types of  work" ///
		81 "did not work but was seeking and/or available for work" ///
		91 "attended educational institution" ///
		92 "attended domestic duties only" ///
		93 "attended domestic duties and was also engaged in free collection of goods (vegetables, roots, firewood, cattle feed, etc.), sewing, tailoring, weaving, etc. for household use -" ///
		94 "rentiers, pensioners , remittance recipients, etc." ///
		95 "not able to work due to disability" ///
		97 "others (including begging, prostitution,  etc.)" ///
		99 ""
label values	b5pt1q3		b5pt1q3_label

tabulate	b5pt1q3_cperv1	b5pt1q3
drop		b5pt1q3_cperv1


* ========== Reqorking B5PT1Q5_CPERV1 (Industry Code) ==========
codebook	b5pt1q5_cperv1
destring	b5pt1q5_cperv1,		gen(b5pt1q5) force

label variable	b5pt1q5			"Industry Code-NIC (Principal)"

drop		b5pt1q5_cperv1


* ========== Reqorking B5PT1Q6_CPERV1 (Occupation Code) ==========
codebook	b5pt1q6_cperv1
destring	b5pt1q6_cperv1,		gen(b5pt1q6) force

label variable	b5pt1q6			"Occupation Code-NCO (Principal)"

drop			b5pt1q6_cperv1


* ========== Reqorking B5PT1Q7_CPERV1 ==========
*			(Whether engaged in any work in Subsidiary Capacity)
codebook	b5pt1q7_cperv1
destring	b5pt1q7_cperv1,		gen(b5pt1q7) force

label define	b5pt1q7_label	1 "Yes" 2 "No"
label value		b5pt1q7			b5pt1q7_label

tabulate		b5pt1q7_cperv1	b5pt1q7
drop			b5pt1q7_cperv1


* ========== Reqorking B5PT1Q8_CPERV1 (Location of Workplace Code (Principal)) ==========
codebook	b5pt1q8_cperv1
list		b5pt1q8_cperv1		in 1/5
destring	b5pt1q8_cperv1,		gen(b5pt1q8) force

label variable	b5pt1q8		"Location of Workplace Code (Principal)"
label define	b5pt1q8_label	10 "workplace in rural areas and located in dwelling unit" ///
		11 "workplace in rural areas and located in structure attached to own dwelling unit" ///
		12 ""	13 "workplace in rural areas and located in detached structure adjacent to own dwelling unit" ///
		14 "workplace in rural areas and located in own enterprise/unit/office/shop but away from own dwelling" ///
		15 "workplace in rural areas and located in employer’s dwelling unit" ///
		16 "workplace in rural areas and located in employer’s enterprise/unit/office/shop but outside employer’s dwelling" ///
		17 "workplace in rural areas and located in street with fixed location" ///
		18 "workplace in rural areas and located in construction site" ///
		19 "workplace in rural areas and located in others" ///
		20 ""	21 "workplace in urban areas and located in: dwelling unit" ///
		22 "workplace in urban areas and located in: structure attached to own dwelling unit" ///
		23 "workplace in urban areas and located in: detached structure adjacent to own dwelling unit" ///
		24 "workplace in urban areas and located in: own enterprise/unit/office/shop but away from own dwelling" ///
		25 "workplace in urban areas and located in: employer’s dwelling unit" ///
		26 "workplace in urban areas and located in: employer’s enterprise/unit/office/shop but outside employer’s dwelling" ///
		27 "workplace in urban areas and located in: street with fixed location" ///
		28 "workplace in urban areas and located in: construction site" ///
		29 "workplace in urban areas and located in: others" ///
		99 "no fixed workplace"
label value		b5pt1q8			b5pt1q8_label

tabulate		b5pt1q8_cperv1	b5pt1q8
drop			b5pt1q8_cperv1


* ========== Reqorking B5PT1Q9_CPERV1 (Enterprise Type Code (Principal)) ==========
codebook	b5pt1q9_cperv1
destring	b5pt1q9_cperv1,		gen(b5pt1q9) force

label variable	b5pt1q9			"Enterprise Type Code (Principal)"
label define	b5pt1q9_label	1 "proprietary: male"	2 "proprietary: female" ///
		3 "partnership: with members from same household" ///
		4 "partnership: with members from different household" ///
		5 "partnership: Government/local body" ///
		6 "partnership: Public Sector Enterprises" ///
		7 "partnership: Autonomous Bodies" ///
		8 "partnership: Public/Private limited company" ///
		10 "partnership: Co-operative societies" ///
		11 "partnership: trust/other non-profit institutions" ///
		12 "partnership: employer’s households(i.e., private households employing  maid servant, watchman, cook, etc.)" ///
		19 "partnership: others"
label value		b5pt1q9			b5pt1q9_label

tabulate	b5pt1q9_cperv1		b5pt1q9				
drop		b5pt1q9_cperv1


* ========== Reqorking B5PT1Q10_CPERV1 ==========
* 			(No of Wokrers in Enterprise (Principal))
codebook	b5pt1q10_cperv1
destring	b5pt1q10_cperv1,		gen(b5pt1q10) force

label variable	b5pt1q10			"No of Workers in Enterprise (Principal)"
label define	b5pt1q10_label		1 "less than 6" ///
		2 "6 and above but less than 10" ///
		3 "10 and above but less than 20" ///
		4 "20 and abobe"			9 "not known"
label value		b5pt1q10			b5pt1q10_label

tabulate	b5pt1q10_cperv1		b5pt1q10
drop		b5pt1q10_cperv1


* ========== Reqorking B5PT1Q11_CPERV1 (Type of Job Contract (Principal)) ==========
codebook	b5pt1q11_cperv1
destring	b5pt1q11_cperv1,		gen(b5pt1q11) force

label variable	b5pt1q11			"Type of Job Contract (Principal)"
label define	b5pt1q11_label		1 "no written  job contract" ///
		2 "written job contract for 1  year or less" ///
		3 "written job contract more than 1 year to 3  years" ///
		4 "written job contract more than 3 years"
label value		b5pt1q11			b5pt1q11_label

tabulate	b5pt1q11_cperv1		b5pt1q11
drop		b5pt1q11_cperv1


* ========== Reqorking B5PT1Q12_CPERV1 (Eligible of Paid Leave (Principal)) ==========
codebook	b5pt1q12_cperv1
destring	b5pt1q12_cperv1,		gen(b5pt1q12) force

label variable	b5pt1q12			"Eligible of Paid Leave (Principal)"
label define	b5pt1q12_label		1 "Yes"		2 "No"
label value		b5pt1q12			b5pt1q12_label

tabulate	b5pt1q12_cperv1		b5pt1q12
codebook	b5pt1q12
drop		b5pt1q12_cperv1


* ========== Reqorking B5PT1Q13_CPERV1 (Social Security Benefits (Principal)) ==========
codebook	b5pt1q13_cperv1
destring	b5pt1q13_cperv1,		gen(b5pt1q13) force

label variable	b5pt1q13			"Social Security Benefits (Principal)"
label define	b5pt1q13_label		1 "only PF/pension (i.e., GPF, CPF, PPF, pension, etc.)" ///
		2 "only gratuity"		3 "only health care &  maternity benefits" ///
		4 "only PF/pension  and gratuity" ///
		5 "only PF/pension and health care & maternity benefits" ///
		6 "only gratuity and health care & maternity benefits" ///
		7 "PF/pension, gratuity, health care  & maternity benefits" ///
		8 "not  eligible for  any of above social security benefit" ///
		9 "not known"
label value		b5pt1q13			b5pt1q13_label

tabulate	b5pt1q13_cperv1		b5pt1q13
codebook	b5pt1q13
drop		b5pt1q13_cperv1


* ========== Reqorking B5PT1Q14_CPERV1 ==========
*			(Usage of product of the economic activity (Principal))
codebook	b5pt1q14_cperv1
destring	b5pt1q14_cperv1,		gen(b5pt1q14) force

label variable	b5pt1q14			"Usage of product of the economic activity (Principal)"
label define	b5pt1q14_label		1 "for own consumption only and did not intend to sell any part of it" ///
		2 "for own consumption and intended to sell some part of it (less than 50%)" ///
		3 "for own consumption and intended to sell major part of it (more than or equal 50%)" ///
		4 "the entire produce is for selling"
label value		b5pt1q14			b5pt1q14_label

tabulate	b5pt1q14_cperv1		b5pt1q14
codebook	b5pt1q14
drop		b5pt1q14_cperv1


* ========== Reqorking B5PT2Q3_CPERV1 (Status Code) ==========
codebook	b5pt2q3_cperv1
list		b5pt2q3_cperv1	in 1/5
destring	b5pt2q3_cperv1,		gen(b5pt2q3) force

label variable	b5pt2q3				"Status Code (Subsidiary)"
label define	b5pt2q3_label		11 "worked in h.h. enterprise (self-employed): own account worker" ///
		12 "worked in h.h. enterprise (self-employed): employer" ///
		21 "worked as helper in h.h. enterprise (unpaid family worker)" ///
		31 "worked as regular salaried/ wage employee" ///
		41 "worked as casual wage labour: in public works" ///
		51 "in other types of work" 
label values	b5pt2q3		b5pt2q3_label

tabulate	b5pt2q3_cperv1	b5pt2q3
drop		b5pt2q3_cperv1


* ========== Reqorking B5PT2Q5_CPERV1 (Industry Code) ==========
codebook	b5pt2q5_cperv1
destring	b5pt2q5_cperv1,		gen(b5pt2q5) force

label variable	b5pt2q5			"Industry Code-NIC (Subsidiary)"

drop		b5pt2q5_cperv1


* ========== Reqorking B5PT2Q6_CPERV1 (Occupation Code) ==========
codebook	b5pt2q6_cperv1
destring	b5pt2q6_cperv1,		gen(b5pt2q6) force

label variable	b5pt2q6			"Occupation Code-NCO (Subsidiary)"

drop			b5pt2q6_cperv1


* ========== Reqorking B5PT2Q7_CPERV1 (Location of Workplace Code (Subsidiary)) ==========
codebook	b5pt2q7_cperv1
list		b5pt2q7_cperv1		in 1/5
destring	b5pt2q7_cperv1,		gen(b5pt2q7) force

label variable	b5pt2q7		"Location of Workplace Code (Subsidiary)"
label define	b5pt2q7_label	10 "workplace in rural areas and located in dwelling unit" ///
		11 "workplace in rural areas and located in structure attached to own dwelling unit" ///
		12 ""	13 "workplace in rural areas and located in detached structure adjacent to own dwelling unit" ///
		14 "workplace in rural areas and located in own enterprise/unit/office/shop but away from own dwelling" ///
		15 "workplace in rural areas and located in employer’s dwelling unit" ///
		16 "workplace in rural areas and located in employer’s enterprise/unit/office/shop but outside employer’s dwelling" ///
		17 "workplace in rural areas and located in street with fixed location" ///
		18 "workplace in rural areas and located in construction site" ///
		19 "workplace in rural areas and located in others" ///
		20 ""	21 "workplace in urban areas and located in: dwelling unit" ///
		22 "workplace in urban areas and located in: structure attached to own dwelling unit" ///
		23 "workplace in urban areas and located in: detached structure adjacent to own dwelling unit" ///
		24 "workplace in urban areas and located in: own enterprise/unit/office/shop but away from own dwelling" ///
		25 "workplace in urban areas and located in: employer’s dwelling unit" ///
		26 "workplace in urban areas and located in: employer’s enterprise/unit/office/shop but outside employer’s dwelling" ///
		27 "workplace in urban areas and located in: street with fixed location" ///
		28 "workplace in urban areas and located in: construction site" ///
		29 "workplace in urban areas and located in: others" ///
		99 "no fixed workplace"
label value		b5pt2q7			b5pt2q7_label

tabulate		b5pt2q7_cperv1	b5pt2q7
drop			b5pt2q7_cperv1


* ========== Reqorking B5PT2Q8_CPERV1 (Enterprise Type Code (Subsidiary)) ==========
codebook	b5pt2q8_cperv1
destring	b5pt2q8_cperv1,		gen(b5pt2q8) force

label variable	b5pt2q8			"Enterprise Type Code (Subsidiary)"
label define	b5pt2q8_label	1 "proprietary: male"	2 "proprietary: female" ///
		3 "partnership: with members from same household" ///
		4 "partnership: with members from different household" ///
		5 "partnership: Government/local body" ///
		6 "partnership: Public Sector Enterprises" ///
		7 "partnership: Autonomous Bodies" ///
		8 "partnership: Public/Private limited company" ///
		10 "partnership: Co-operative societies" ///
		11 "partnership: trust/other non-profit institutions" ///
		12 "partnership: employer’s households(i.e., private households employing  maid servant, watchman, cook, etc.)" ///
		19 "partnership: others"
label value		b5pt2q8			b5pt2q8_label

tabulate	b5pt2q8_cperv1		b5pt2q8
drop		b5pt2q8_cperv1


* ========== Reqorking B5PT2Q9_CPERV1 ==========
* 			(No of Wokrers in Enterprise (Subsidiary))
codebook	b5pt2q9_cperv1
rename		b5pt2q9_cperv1		b5pt2q9

label variable	b5pt2q9			"No of Workers in Enterprise (Subsidiary)"


* ========== Reqorking B5PT2Q10_CPERV1 (Type of Job Contract (Subsidiary)) ==========
codebook	b5pt2q10_cperv1
destring	b5pt2q10_cperv1,		gen(b5pt2q10) force

label variable	b5pt2q10			"Type of Job Contract (Subsidiary)"
label define	b5pt2q10_label		1 "no written  job contract" ///
		2 "written job contract for 1 year or less" ///
		3 "written job contract more than 1 year to 3  years" ///
		4 "written job contract more than 3 years"
label value		b5pt2q10			b5pt2q10_label

tabulate	b5pt2q10_cperv1		b5pt2q10
drop		b5pt2q10_cperv1


* ========== Reqorking B5PT2Q11_CPERV1 (Eligible of Paid Leave (Subsidiary)) ==========
codebook	b5pt2q11_cperv1
destring	b5pt2q11_cperv1,		gen(b5pt2q11) force

label variable	b5pt2q11			"Eligible of Paid Leave (Subsidiary)"
label define	b5pt2q11_label		1 "Yes"		2 "No"
label value		b5pt2q11			b5pt2q11_label

tabulate	b5pt2q11_cperv1		b5pt2q11
drop		b5pt2q11_cperv1


* ========== Reqorking B5PT2Q12_CPERV1 (Social Security Benefits (Subsidiary)) ==========
codebook	b5pt2q12_cperv1
destring	b5pt2q12_cperv1,		gen(b5pt2q12) force

label variable	b5pt2q12			"Social Security Benefits (Subsidiary)"
label define	b5pt2q12_label		1 "only PF/pension (i.e., GPF, CPF, PPF, pension, etc.)" ///
		2 "only gratuity"		3 "only health care &  maternity benefits" ///
		4 "only PF/pension  and gratuity" ///
		5 "only PF/pension and health care & maternity benefits" ///
		6 "only gratuity and health care & maternity benefits" ///
		7 "PF/pension, gratuity, health care  & maternity benefits" ///
		8 "not  eligible for  any of above social security benefit" ///
		9 "not known"
label value		b5pt2q12			b5pt2q12_label

tabulate	b5pt2q12_cperv1		b5pt2q12
drop		b5pt2q12_cperv1


* ========== Reqorking B5PT2Q13_CPERV1 ==========
*			(Usage of product of the economic activity (Subsidiary))
codebook	b5pt2q13_cperv1
destring	b5pt2q13_cperv1,		gen(b5pt2q13) force

label variable	b5pt2q13			"Usage of product of the economic activity (Principal)"
label define	b5pt2q13_label		1 "for own consumption only and did not intend to sell any part of it" ///
		2 "for own consumption and intended to sell some part of it (less than 50%)" ///
		3 "for own consumption and intended to sell major part of it (more than or equal 50%)" ///
		4 "the entire produce is for selling"
label value		b5pt2q13			b5pt2q13_label

tabulate	b5pt2q13_cperv1		b5pt2q13
codebook	b5pt2q13
drop		b5pt2q13_cperv1


* ========== Reqorking B5PT3Q5_CPERV1 ==========
*			(Ever Worked Prior to last 365 days)
codebook	b5pt3q5_cperv1
destring	b5pt3q5_cperv1,		gen(b5pt3q5) force

label define	b5pt3q5_label	1 "Yes"		2 "No"
label value		b5pt3q5			b5pt3q5_label

tabulate	b5pt3q5_cperv1	b5pt3q5
drop		b5pt3q5_cperv1


* ========== Reqorking B5PT3Q6_CPERV1 ==========
*	(Duration of engagement in the economic activity in usual Principal activity status)
codebook	b5pt3q6_cperv1
rename		b5pt3q6_cperv1		b5pt3q6

label copy		b5pt3q6_cperv1		b5pt3q6_label
label values 	b5pt3q6				b5pt3q6_label
label drop 		b5pt3q6_cperv1

codebook	b5pt3q6


* ========== Reqorking B5PT3Q7_CPERV1 ==========
*	(Duration of engagement in the economic activity in Subsidiary activity status)
codebook	b5pt3q7_cperv1
rename		b5pt3q7_cperv1		b5pt3q7

label copy		b5pt3q7_cperv1		b5pt3q7_label
label values 	b5pt3q7				b5pt3q7_label
label drop 		b5pt3q7_cperv1

codebook	b5pt3q7


* ========== Reqorking B5PT3Q8_CPERV1 ==========
*			(Efforts undertaken to search work)
codebook	b5pt3q8_cperv1
rename		b5pt3q8_cperv1		b5pt3q8

label copy		b5pt3q8_cperv1		b5pt3q8_label
label values 	b5pt3q8				b5pt3q8_label
label drop 		b5pt3q8_cperv1

codebook	b5pt3q8


* ========== Reqorking B5PT3Q9_CPERV1 ==========
*			(Duration of spell of Unemployment)
codebook	b5pt3q9_cperv1
rename		b5pt3q9_cperv1		b5pt3q9

label copy		b5pt3q9_cperv1		b5pt3q9_label
label values 	b5pt3q9				b5pt3q9_label
label drop 		b5pt3q9_cperv1

codebook	b5pt3q9


* ========== Reqorking B5PT3Q10_CPERV1 ==========
*			(Whether Ever worked)
codebook	b5pt3q10_cperv1
rename		b5pt3q10_cperv1		b5pt3q10

label copy		b5pt3q10_cperv1		b5pt3q10_label
label values 	b5pt3q10				b5pt3q10_label
label drop 		b5pt3q10_cperv1

codebook	b5pt3q10


* ========== Reqorking B5PT3Q11_CPERV1 ==========
*			(Reason for not working in last 365 days)
codebook	b5pt3q11_cperv1
rename		b5pt3q11_cperv1		b5pt3q11

list	b5pt3q11	in 1/10

label list		b5pt3q11_cperv1
label copy		b5pt3q11_cperv1		b5pt3q11_label
label values 	b5pt3q11			b5pt3q11_label
label drop 		b5pt3q11_cperv1

codebook	b5pt3q11


* ========== Reqorking B5PT3Q12_CPERV1 ==========
*			(Main reason for being in Principal activity status (91 to 97))
codebook	b5pt3q12_cperv1
rename		b5pt3q12_cperv1		b5pt3q12

label copy		b5pt3q12_cperv1		b5pt3q12_label
label value		b5pt3q12			b5pt3q12_label
label drop		b5pt3q12_cperv1

codebook		b5pt3q12


* ========== Reqorking B6Q5_CPERV1 (Current Weekly Status (CWS)) ==========
codebook	b6q5_cperv1
destring	b6q5_cperv1,		gen(b6q5) force

label variable	b6q5			"Current Weekly Status (CWS)"
label define	b6q5_label		11 "worked in h.h. enterprise (self-employed): own account worker" ///
	12 "worked in h.h. enterprise (self-employed): employer" ///
	21 "worked as helper in h.h. enterprise (unpaid family worker)" ///
	31 "worked as regular salaried/wage employee" ///
	41 "worked as casual wage labour in public works other than MGNREG works" ///
	42 "worked as casual wage labour in MGNREG works" ///
	51 "in other types of work" ///
	61 "had work in h.h. enterprise but did not work due to: sickness" ///
	62 "had work in h.h. enterprise but did not work due to other reasons" ///
	71 "had regular salaried/wage employment but did not work due to sickness" ///
	72 "had regular salaried/wage employment but did not work due to other reasons" ///
	81 "had regular salaried/wage employment but did not work due tosought work" ///
	82 "had regular salaried/wage employment but did not work due to did not seek but was available for work" ///
	91 "attended educational institution" ///
	92 "attended domestic duties only" ///
	93 "attended domestic duties and was also engaged in free collection of goods (vegetables, roots, firewood, cattle feed, etc.), sewing, tailoring, weaving, etc. for household use -" ///
	94 "rentiers, pensioners, remittance recipients, etc." ///
	95 "not able to work due to disability" ///
	97 "others (including begging, prostitution, etc.)" ///
	98 "had regular salaried/wage employment but did not work due to did not work due to temporary sickness (for casual workers only)" ///
	99 ""
label value		b6q5			b6q5_label

tabulate	b6q5_cperv1		b6q5
drop		b6q5_cperv1


* ========== Reqorking B6Q6_CPERV1 (Industry Code (CWS)) ==========
codebook	b6q6_cperv1
destring	b6q6_cperv1,		gen(b6q6) force

drop		b6q6_cperv1


* ========== Reqorking B6Q7_CPERV1 (Occupation Code (CWS)) ==========
codebook	b6q7_cperv1
destring	b6q7_cperv1,		gen(b6q7) force

drop		b6q7_cperv1


* ========== Reqorking B6Q9_CPERV1 ==========
*			(Earnings for Regular Salaried/Wage Activity)
codebook	b6q9_cperv1
destring	b6q9_cperv1,		gen(b6q9) force

drop		b6q9_cperv1


* ========== Reqorking B6Q10_CPERV1 ==========
*			(Earnings for Self Employed)
codebook	b6q10_cperv1
destring	b6q10_cperv1,		gen(b6q10) force

drop		b6q10_cperv1


* ========== Reqorking Consistency in select labels ==========
codebook	b4pt1q5		b5pt2q9

label copy		b4pt1q5_cperv1	b4pt1q5_label
label value		b4pt1q5			b4pt1q5_label
label drop		b4pt1q5_cperv1

label copy		b5pt2q9_cperv1	b5pt2q9_label
label value		b5pt2q9			b5pt2q9_label
label drop		b5pt2q9_cperv1

codebook	b4pt1q5		b5pt2q9





save "C:\Users\pgeda\Documents\stata-appdata\plfs-2023-23.dta", replace




