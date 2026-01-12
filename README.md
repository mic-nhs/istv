# istv

This package provides functions that geocode a short text description of a location. This is to allow the text descriptions collected as part of the assault location description field of the Emergency Care Data Set (ECDS) to be converted into locations identifiers free of any potential personal identifiable data, described by Lower Super Output Areas (LSOA). It does so without exposing the data involved to any external geocoding service and is designed to handle issues that are common in the way administrative and clinical staff capture this data.

Install with:

`devtools::install_github("mic-nhs/istv")`

If you need to use the `assign_home()` function you will need the associated word2vec model file: contact michael.cheetham@nhs.net to discuss this.

Assault location description is a 255 character field in ECDS that is captured when a patient attends as the result of an assault. It may be collected at reception, triage or elsewhere in the patient journey by administrative or clinical staff. It is intended to indicate the location of an assault so that hotspots can be identified and preventative measures put in place (e.g. additional CCTV, police patrols etc.). It should be in a form similar to this:

> "Black Lion pub on Commercial Road"

If the patient was assaulted at their home address the text should be similar to:

> "Patient's home"

Because the field is free text it frequently contains other information which is not useful or contains PID.

The name istv comes from a previous name for this standard: Information sharing to tackle violence.
