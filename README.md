# traits and species associations with Hmsc, INCLINE data and ordination methods

This readme file was generated on [2023-01-13] by Eva Lieungh

Contact:
Eva Lieungh
https://orcid.org/0000-0003-4009-944X
Natural History Museum, University of Oslo
eva.lieungh[at]nhm.uio.no
evaleriksen[at]gmail.com

## NB! this is a work in progress. The scripts are not ready, and results may not be reproducible. Some scripts have been copied and modified from other online sources.

## Related resources:

- Otso Ovaskainen, & Eva Lieungh. (2023). Model output for five Hmsc models of alpine grassland communities (1.0) [Data set]. Zenodo. <https://doi.org/10.5281/zenodo.8064101>
- Eva Lieungh, & Rune Halvorsen. (2023). DCA and GNMDS output for 4640 subplots and 95 vascular plant species in four alpine grasslands (1.0) [Data set]. Zenodo. <https://doi.org/10.5281/zenodo.8064319>
- INCLINE data repository (to be published)

### Contents

| Folder or file name | Description       |
| ------------------- | ----------- |
| archive/ | files made redundant, old versions | 
| data/ | raw and input data |
| data_processed/ | modified data, intermediate files |
| writing | manuscript drafts and backups, other texts |
| results | output files, models, figures etc. |
| resources | external resources not to be uploaded (add to .gitignore) |
| src   | code, notebooks etc for analysis |
| .gitignore | file specifying folders and files Git should not track or upload |
| README.md | this file, read as the front page of GitHub repository |

### Funding sources that supported this project

PhD project funded by the Norwegian Ministry of Education and Research, through the Natural History Museum, University of Oslo. Vascular plant community data were collected for the INCLINE project, Research Council of Norway FRIMEDBIO project 274712. PI: Vandvik, Töpper. 2018–2021. 

### Sharing and access

Copy snippets freely, but please refer to this repository if you use large parts of several scripts (that is, if you are doing most of the same analyses and your starting point is a copy of these scripts). See for instance the [ICMJE authorship guidelines (Vancouver convention)](https://www.icmje.org/recommendations/browse/roles-and-responsibilities/defining-the-role-of-authors-and-contributors.html) to help you decide which kind of acknowledgement is appropriate. 

### Help and useful tricks

Questions? Contact me, or post an issue in this repository. 

A neat trick to find the answer to questions like "what script creates and saves the important_data.Rds file?" is to open a terminal in the ´/src´ folder and enter this command to search (grep) inside all files (-e) inside that folder and subfolders (-r): ´grep -r -e "important_data.Rds"´
