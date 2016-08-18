# Mathematical Sciences Editorial Board Dataset

This dataset was collected and analyzed for Topaz and Sen's analysis of editorial boards in the Mathematical Sciences (2016).
If you use this dataset, please cite the following publication:

*Topaz CM, Sen S (2016) Gender Representation on Journal Editorial Boards in the Mathematical Sciences. PLoS ONE 11(8): e0161357. doi:10.1371/journal.pone.0161357*

If you have questions about this dataset let use this dataset in your own work, you can contact us at topaz@macalester.edu and ssen@macalester.edu. Pull requests are encouraged and gratefully accepted.

Our dataset contains the following:

1. **Journal Information** - *journal_data.csv*: Includes the source abbreviations from Thomson / Reuters Journal Citation Reports (JCR), the Turker responses for Journal Title and Journal URLs, an our corrected journal titles.
2. **Editor Information** - *editors.csv*: Information from Turkers about editors for each Journal. A simple name-matching algorithm was used to join editors for the same journal.
3. **Gender Information** - *finalData.csv*: Full dataset of editorships with gender information. "gender" is the raw inferred gender, probF, probM, and probNA are the adjusted calibrations as described in the paper. "TurkGender" is the Turker gender score as described in the paper. "inferredGender" is the gender as inferred by genderize.io. "inferredGenderScore" is the genderize.io score as described in the paper.
4. **Validation dataset** - *validation.csv*: Human-annotated validation dataset. Genders as coded by the authors appear in the "Expert Gender" column.

We have also included our three main R analysis scripts. Note that these will not run without adjusting filename information, but they will provide insight into our analysis details. Please let us know if you have questions on these.
