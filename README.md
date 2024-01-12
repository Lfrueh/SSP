<html>
<h1>Spatial Social Polarization Database: <br>An Interactive Mapping Tool</h1>
<p>Lisa Frueh,<sup>1</sup> Sam Jaros,<sup>2</sup> Dr. Hoda Abdel Magid,<sup>3</sup>  Dr. Gina Lovasi<sup>4</sup> 
<small>
<br><sup>1</sup>Drexel University Dornsife School of Public Health, Department of Environmental and Occupational Health
<br><sup>2</sup>Stanford University School of Medicine, Departmetn of Epidemiology and Clinical Research
<br><sup>3</sup>University of Southern California Keck School of Medicine, Departmnet of Population and Public Health Sciences
<br><sup>4</sup>Drexel University Dornsife School of Public Health, Department of Epidemiology and Biostatistics
<br><br>
<b>Contact:</b> Lisa Frueh at <a href="mailto:lisa.frueh@drexel.edu">lisa.frueh@drexel.edu</a>

</p>
<h3>Introduction</h3>
<p>This interactive mapping app is a companion to the <a href="https://github.com/samjaros-stanford/spatial_social_polarization_database/tree/main">spatial social polarization database</a>, which compiles pre-calculated spatial social polarization variables and the code used to calculate them. The purpose of the database is to remove computational barriers which prevent the wider use of useful indices in health research. The purpose of this interactive mapping platform is to create an accessible entry point into the visualization and comparison of spatial social polarization variables.</p>

<h3>What is the Index of Concentration at the Extremes?</h3>
<p>The Index of Concentration at the Extremes (ICE), developed by sociologist <a href="https://www.researchgate.net/publication/312987867_The_Prodigal_Paradigm_Returns_Ecology_Comes_Back_to_Sociology">Douglas Massey</a>, compares the percent of population in an area that are in a &apos;privileged&apos; group and the percent of a population in the same area that are in the &apos;deprived&apos; group. ICE was first applied to public health research by <a href = "https://ajph.aphapublications.org/doi/full/10.2105/AJPH.2015.302955">Dr. Nancy Krieger in 2016 </a>.
</p>

<p dir="auto">ICE can be calculated for any definition of privilege and deprivation. For example, an ICE for income could compare wealthy to poor population, an ICE for race might compare Black to white population, and an ICE for race/ethnicity/income might compare the concentration of high-income non-Hispanic whites to low-income people of color. This flexibility makes ICE a highly versatile measure of segregation. For any ICE variable, values range from -1 to 1, with values close to -1 indicating a high concentration of the disadvantaged population, and values close to 1 indicating a high concentration of the privileged population. </p>

<p>For a given area, <em>i</em>, an ICE value is calculated as:</p>
<p>ICE<sub>i</sub> = (A<sub>i</sub> - P<sub>i</sub>)/T<sub>i</sub></p>
<p>Where A<sub>i</sub> is the number of people in an area that belong to the advantaged group, and is the number of people in the area that belong to the P<sub>i</sub> deprived group, and T<sub>i</sub> is the total number of people in area <em>i</em>. ICE values range from -1 (most concentrated deprivation) to +1 (most concentrated privilege).</p>

<p dir="auto">In this interactive web tool, we define 7 ICE variables (see details in app) across 3 different geographies (county, zip code tabulation area [ZCTA], and census tract). We used 2010 tract, ZCTA, and county boundaries and data are from the US Census Bureau American Community Survey (ACS) 5-year estimates. Each year corresponds to the 5-year estimates ending on that year; for example, selecting year = 2010 pulls ACS 5-year estimates from 2006-2010. 

Geographic areas are identified using the variable GEOID, which refers to the census <a href="https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html">Geographic Identifier</a>.</p>
</body>
</html>
