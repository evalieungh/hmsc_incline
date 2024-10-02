# traits and species co-occurrences with Hmsc and INCLINE data

This repository contains scripts for a manuscript about traits and species co-occurrences, using [Hmsc](https://www.helsinki.fi/en/researchgroups/statistical-ecology/software/hmsc) and INCLINE data. 

## NB! this is a work in progress. The scripts are not 100% clean and finished, and results may not be reproducible. Some scripts have been heavily inspired by other sources.

This readme file was generated on [2023-01-13] by Eva Lieungh

Contact:
Eva Lieungh,
https://orcid.org/0000-0003-4009-944X,
Natural History Museum, University of Oslo,
eva.lieungh[at]nhm.uio.no /
evaleriksen[at]gmail.com

## Related resources:

- Otso Ovaskainen, & Eva Lieungh. (2023). Model output for five Hmsc models of alpine grassland communities (1.0) [Data set]. Zenodo. <https://doi.org/10.5281/zenodo.8064101>
- INCLINE data repository will be published on OSF along with a data paper by Gya et al. (in prep): https://osf.io/zhk3m/ 
- HMSC pipeline example scripts that follow the book. Scripts available from the [hmsc webpage](https://www.helsinki.fi/en/researchgroups/statistical-ecology/software/hmsc). Book: Ovaskainen, O., & Abrego, N. (2020). Joint Species Distribution Modelling (1st ed.). Cambridge University Press. ISBN: 9781108716789.

### Contents

This is the folder structure in my local copy of the directory. Most of these folders are listed in `.gitignore` and will not be uploaded to GitHub. See Related resources list above for the necessary data.

| Folder or file name | Description       |
| ------------------- | ----------- |
| archive/ | files made redundant, old versions | 
| data/ | raw and input data |
| data_processed/ | modified data, intermediate files |
| writing | manuscript drafts and backups, other texts |
| results | output files, models, figures etc. |
| resources | external resources not to be uploaded (add to .gitignore) |
| src   | scripts/code/notebooks |
| .gitignore | file specifying folders and files Git should not track or upload |
| README.md | this file, read as the front page of GitHub repository |

### Funding sources that supported this project

PhD project funded by the Norwegian Ministry of Education and Research, through the Natural History Museum, University of Oslo. Vascular plant community data were collected for the INCLINE project, Research Council of Norway FRIMEDBIO project 274712. PI: Vandvik, Töpper. 2018–2021. 

### Sharing and access

See license. Copy snippets freely, but please refer to this repository if you use large parts of several scripts (that is, if you are doing most of the same analyses and your starting point is a copy of these scripts). See for instance the [ICMJE authorship guidelines (Vancouver convention)](https://www.icmje.org/recommendations/browse/roles-and-responsibilities/defining-the-role-of-authors-and-contributors.html) to help you decide which kind of acknowledgement is appropriate. 

### Help and useful tricks

Questions? Contact me, or post an issue in this repository. 

A neat trick to find the answer to questions like "what script creates and saves the important_data.Rds file?" is to open a terminal in the ´/src´ folder and enter this command to search (grep) inside all files (-e) inside that folder and subfolders (-r): ´grep -r -e "important_data.Rds"´
