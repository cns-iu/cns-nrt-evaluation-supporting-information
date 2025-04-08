Data Manifest:

/data/

This directory contains three groups of data that are processed by R scripts to generate assessment results and figures: 
* ../membership/ includes data related to CNS NRT membership groups.
	* CNS NRT faculty roster and department affiliations.
	* Proxy data for CNS NRT membership (i.e., doctoral students, student affiliates, faculty and staff). 
	* Note the original student demographics data is not provided, to preserve students' right to privacy under FERPA.
* ../surveys/ includes data related to the CNS NRT annual survey.
	* 
	* Collected survey responses, from 2019-2023, which have been prepared to preserve the privacy of participants. 
		* Identifying information has been removed from responses, including names, email addresses, geolocation data, and IP addresses. 
		* Names provided in qualitative responses have been de-identified.
	* Survey questions sets and responses, based on the respondent pools (i.e., faculty, doctoral fellows, student affiliates).
*../publications/ includes publication records that have been reported to NSF by CNS NRT members.
	* Bibtex data set of CNS NRT publications collected from Web of Science, Google Scholar, and NSF Reporter. 
	* Prepared bibliometric data, using EndNote document variable sets.
		* Author names were disambiguated using Open Refine. 
	* An index of authors and publication identifiers, and author's CNS NRT role used in the scientometric analysis.
	* An index of group authors and publication identifiers excluded from analysis.

/analysis/

This directory mirrors the /data/ directory organizational structure of .../membership/, .../surveys/, & .../publications/. The directory is where analysis results (by R scripts) that used in the publication text. 

* ../membership/ provides demographic and education roll-up statistics for the annual cohorts of CNS NRT doctoral fellows, including gender, ethnicity, prior education and the secondary doctoral program selected by the fellows in the program.
* ../surveys/ provides:
	* survey response rate calculations for each year & by CNS NRT group
	* results data for the survey questions analyzed in the paper, including self-assessment that CNS NRT program has achieved its goals; doctoral satisfaction with their academic progress, the programs impact on research skills, and interdisciplinary orientation; the quality and impact of mentorship relationships on doctoral fellows. 
* ../publications/ provides the results of the scientometric analysis of CNS NRT publications, and includes:
	* co-authorship network analysis, generated using R, igraph package, and layout using Gephi.
		* temporal analysis network development in two periods (2019-2021; 2019-2024).
			* group level annualized publication count statistics.
		* author publication count, with annual cumulative counts.
		* author-group relationships analysis of collaborations identified during the co-authorship network analysis. 
	* Web of Science analysis results
		* author affiliations identified by Web of Science
		* journal Web of Science subject categorization to describe disciplinary scope of CNS NRT research.
		* citation analysis from Web of Science for publications, includes the Sum of the Times Cited, Average Citations per Item, and h-index for 64 publications.

