# Ebola linelist data from Fang et al. (2016)

Linelist data for the Ebola virus collected in Sierra Leone. If you use
this data in your work, please cite the corresponding paper.

## Usage

``` r
sierra_leone_ebola_data
```

## Format

A `tibble` with 8,358 rows and 8 columns:

- id:

  Unique identification number for the case

- name:

  Name as character, omitted

- age:

  Age as numeric

- sex:

  Sex as character, either "F", "M" or NA

- date_of_symptom_onset:

  The date symptoms began

- date_of_sample_tested:

  The date the sample was tested

- district:

  The district (ADM2)

- chiefdom:

  The chiefdom (ADM3)

## Source

<https://www.pnas.org/doi/full/10.1073/pnas.1518587113>
