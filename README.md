# traits and species associations with Hmsc, INCLINE data and ordination methods

This readme file was generated on [2023-01-13] by Eva Lieungh

Contact:
Eva Lieungh
https://orcid.org/0000-0003-4009-944X
Natural History Museum, University of Oslo
eva.lieungh[at]nhm.uio.no
evaleriksen[at]gmail.com

More info to come... 

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

PhD project funded by the Norwegian Ministry of Education and Research, through the Natural History Museum, University of Oslo.

### Sharing and access

This code will have a permissive license (likely MIT). Copy snippets freely, but please refer to this repository if you use large parts of several scripts (that is, if you are doing most of the same analyses and your starting point is a copy of these scripts). 

### Help and useful tricks

Questions? Contact me, or post an issue in this repository. 

A neat trick to find the answer to questions like "what script creates and saves the important_data.Rds file?" is to open a terminal in the ´/src´ folder and enter this command to search (grep) inside all files (-e) inside that folder and subfolders (-r): ´grep -r -e "important_data.Rds"´
