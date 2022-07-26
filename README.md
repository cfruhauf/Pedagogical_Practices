# PEDAGOGICAL PRACTICES AND SCHOOL PERFOMANCE IN BRAZIL

## Research objectives
- Verify if the pedagogical practices used by teachers in classrooms are determinants of the quality of Brazilian education, denoted by the performance of students in the 9th grade of Elementary School and 3rd grade of High School in Portuguese Language and Mathematics in 2017 SAEB test.
- Compare the impacts of pedagogical practices teachers use in the classroom on student performance among students in the 9th grade of Elementary School and the 3rd grade of High School, as well as between the subjects of Portuguese Language and Mathematics.

## Data source
Microdata from the National Assessment of Basic Education of 2017 obtained from the Brazilian Basic Education Assessment System (in Portuguese SAEB) was used as a database. The exam is carried out biannually in public and private schools across the country, with two assessment instruments: parametric tests applied to students and socioeconomic questionnaires answered by students, teachers, and directors of the evaluated schools. The data refer to students in the 9th year of Elementary School and 3rd year of High School, their respective teachers, principals, and schools.
**Download page**: [Inep: SAEB microdata](https://dados.gov.br/dataset/inep-microdados-do-saeb)

## Methodology
The school system is a typical example in which the data is organized in a nested way, where variables represent the students, who are grouped into classes, and where you can find variables that describe the teacher. Then the classes can be grouped into schools, and these turn into school districts, forming a hierarchical system. As a result, it was decided to use a hierarchical model divided into three levels: level 1 represents the characteristics of students, level 2 of teachers, and level 3 of schools. Also, different models will be estimated for students in the 9th grade of Elementary School and 3rd grade of High School, as well as for Portuguese Language and Mathematics subjects.

To verify the degree of differentiation within the levels (1st, 2nd, and 3rd) and thus verify if the model was adequately estimated, the intra-class correlation coefficient is used.

The scores obtained by students in the 9th grade of Elementary School and 3rd grade of High School in Portuguese Language and Mathematics in the 2017 SAEB Exam are used as dependent variables. The main explanatory variables of the model consist of three indices to be built: General Pedagogical Practices Index (IPP - General), Portuguese Language Teachers' Pedagogical Practices Index (IPP - Portuguese Language), and the Mathematics Teachers' Pedagogical Practices Index (IPP - Mathematics).

Based on the literature on the subject, 13 control variables were defined to be used. Regarding the student's level, we have Gender, Race, Mother's Education, Father's Education, Student Socioeconomic Level, Failure, and Motivation. Related teachers' characteristics were Training (postgraduate), Experience at school, and Remuneration. Finally, linked to the school's characteristics are the Principal's experience, Type of administrative dependency, and Location.

If you are interested in checking the format of the questions used related to the mentioned variables, I added a folder with all the [questionnaires](https://github.com/cfruhauf/Pedagogical_Practices/tree/master/Questionnaires).

## Code

The code has been split into nine files. I started by performing the previous organization and cleaning the data in RStudio ([Dissertation](https://github.com/cfruhauf/Pedagogical_Practices/blob/master/Codes/R%20Script/Dissertation.R)). After that, four new R Scripts were created, where students, teachers, and schools information were merged, with one script for each model to be estimated: [9th grade of Elementary School Portuguese Language](https://github.com/cfruhauf/Pedagogical_Practices/blob/master/Codes/R%20Script/9th%20grade%20of%20Elementary%20School%20Portuguese%20Language.R), [9th grade of Elementary School Mathematics](https://github.com/cfruhauf/Pedagogical_Practices/blob/master/Codes/R%20Script/9th%20grade%20of%20Elementary%20School%20Mathematics.R), [3rd grade of High School Portuguese Language](https://github.com/cfruhauf/Pedagogical_Practices/blob/master/Codes/R%20Script/3rd%20grade%20of%20High%20School%20Portuguese%20Language.R), and [3rd grade of High School Mathematics](https://github.com/cfruhauf/Pedagogical_Practices/blob/master/Codes/R%20Script/3rd%20grade%20of%20High%20School%20Mathematics.R).

After the variables of each model were aggregated in a data frame and exported to the computer. So, the analysis continued in Stata, where a do-file was created for each model to create the index and estimate the models ([9th grade of Elementary School Portuguese Language](https://github.com/cfruhauf/Pedagogical_Practices/blob/master/Codes/Do-File/9th%20grade%20of%20Elementary%20School%20Portuguese%20Language.do); [9th grade of Elementary School Mathematics](https://github.com/cfruhauf/Pedagogical_Practices/blob/master/Codes/Do-File/9th%20grade%20of%20Elementary%20School%20Mathematics.do); [3rd grade of High School Portuguese Language](https://github.com/cfruhauf/Pedagogical_Practices/blob/master/Codes/Do-File/3rd%20grade%20of%20High%20School%20Portuguese%20Language.do); [3rd grade of High School Mathematics](https://github.com/cfruhauf/Pedagogical_Practices/blob/master/Codes/Do-File/3rd%20grade%20of%20High%20School%20Mathematics.do)). Finally, we returned to R with the complete files for estimating statistics and creating graphs.

###**Observation**: All code step-by-step observations are written in English. The code observations related to questionnaires were kept in Portuguese, as well as the name of the variables.

## Finished dissertation

This dissertation was concluded and defended by February 2021. If you want to read a copy, to check the results, the statistics and/or the graphics generated, it is disposal in Portuguese [here](https://www.researchgate.net/publication/352092774_Praticas_pedagogicas_e_desempenho_escolar_no_Brasil).
