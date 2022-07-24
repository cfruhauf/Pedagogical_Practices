# PEDAGOGICAL PRACTICES AND SCHOOL PERFOMANCE IN BRAZIL

## Research objectives
- Verify if the pedagogical practices used by teachers in classrooms are determinants of the quality of Brazilian education, denoted by the performance of students in the 9th grade of Elementary School and 3rd grade of High School in Portuguese Language and Mathematics in the 2017 SAEB test.
- Compare the impacts of pedagogical practices used by teachers in the classroom on student performance among students in the 9th grade of Elementary School and the 3rd grade of High School, as well as between the subjects of Portuguese Language and Mathematics.

## Data source
As a database, microdata from the National Assessment of Basic Education of 2017 obtained from the brazilian Basic Education Assessment System were used. The exam is carried out biannually in public and private schools across the country, with two assessment instruments: parametric tests applied to students and socioeconomic questionnaires answered by students, teachers and directors of the evaluated schools. The data refer to students in the 9th year of Elementary School and 3rd year of High School, their respective teachers, principals and schools.
**Download page**: [Inep: SAEB microdata](https://dados.gov.br/dataset/inep-microdados-do-saeb)

## Methodology
The school system is a typical example in which the data is organized in a nested way, where there are variables representing the students, who are grouped into classes; and where you can find variables that describe the teacher. Then the classes can be grouped into schools, and these in turn into school districts and so on, forming a hierarchical system. As a result, it was decided to use a hierarchical model divided into three levels: level 1 represents the characteristics of students, level 2 of teachers and level 3 of schools. Also, different models will be estimated for students in the 9th grade of Elementary School and 3rd grade of High School, as well as for the subjects of Portuguese Language and Mathematics.
In order to verify the degree of differentiation within the levels (1st, 2nd and 3rd) and thus verify if the model was adequately estimated, the intra-class correlation coefficient is used.

As dependent variables, the scores obtained by students in the 9th grade of Elementary School and 3rd grade of High School in Portuguese Language and Mathematics in the 2017 SAEB Exam are used. The main explanatory variables of the model consist of three indices to be build: General Pedagogical Practices Index (IPP - General), Portuguese Language Teachers' Pedagogical Practices Index (IPP - Portuguese Language), and the Mathematics Teachers' Pedagogical Practices Index (IPP - Mathematics).

Based on the literature on the subject, 13 control variables were defined to be used. Regarding the student's level, we have: Gender, Race, Mother's Education, Father's Education, Student's Socioeconomic Level, Failure and Motivation. Related to the characteristics of the teachers, the following were used: Training (postgraduate), Experience at school and Remuneration. Finally, linked to the characteristics of the school are: Principal's experience, Type of administrative dependency and Location.

If you are interested in checking the format of the questions used related to the mentioned variables, I added a folder with all the [questionnaires](https://github.com/cfruhauf/Pedagogical_Practices/tree/master/Questionnaires).

## Code

The code has been split into nine files. I started by performing the previous organization and cleaning of the data in RStudio (Dissertation). After that, four new R Scripts were created, where students, teachers and schools informations were merged, one script for each model to be estimated: 9th grade of Elementary School Portuguese Language, 9th grade of Elementary School Mathematics, 3rd grade of High School Portuguese Language, and 3rd grade of High School Mathematics.

After the variables of each model were aggregated in a dataframe, they were exported to the computer. The analysis continued in Stata, where also were created a do-file for each model to criated the index and estimate the models.

Finally, we returned to R with the complete files for estimating statistics and creating graphs.
