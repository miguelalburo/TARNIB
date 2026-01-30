# TARNIB's Predictive Modeling for Clinical Outcomes

Hi 👋,

We are the *Team for Advanced Research in Neural Inference for Biostatistics* a.k.a **TARNIB**, a group of postgraduate bioinformatics students at the University of Birmingham. Our project focuses on building and comparing ML to predict patient in-hospital mortality risk and length of stay using synthetic clinical data inspired by the [MIMIC-III/IV datasets](https://physionet.org/content/mimiciv/3.1/). We also aim to investigate whether ethnic group impacts model performance.

## Data Preprocessing Pipeline 

flowchart TB

    diagnoses["Diagnoses Data"]
    admissions["Admission Data"]
    patients["Patient Data"]
    icd10["ICD 10 Codes"]
    ccs["Clinical Classification CCS"]
    feature["Input Feature Matrix"]
    long[ ]:::empty
    merged["Cleaned Aggregate"]
    wide[ ]:::empty

    
    admissions --- long
    patients --- long
    long --"Merging, Cleaning & Feature Engineering"--> merged
    diagnoses --"Unification"--> icd10
    icd10 --"Cardinality Reduction"-->ccs
    ccs --- wide
    merged --- wide
    wide --"Multihot & Embedding"--> feature
    

    classDef empty width:1px,height:1px,fill:transparent,stroke:transparent;

## 👥 TARNIB Members

- Abd Alhalem Abboud

- Zubair Khan

- Dylan le

- Miguel Alburo
