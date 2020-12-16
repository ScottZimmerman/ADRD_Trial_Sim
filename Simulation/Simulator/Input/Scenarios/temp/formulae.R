list(demographics = "JD", cohort = "JD", arm = "JD", cses = "cses ~ base.age60 + raceBlack", 
    educ_HRS = "educ_HRS ~ base.age60 + sexMale + raceBlack + cses", 
    educ_ARIC = "educ_ARIC ~ base.age60 + sexMale + raceBlack", 
    educ = "combineCohorts", diet_p = "diet_p ~ base.age60 + sexMale + raceBlack + educ", 
    diet_w = "diet_w ~ base.age60 + sexMale + raceBlack + educ", 
    hba1c = "setValue", mem.s = "mem.s ~ cohort + cohort*age60*(raceBlack + educ + sexMale + cses + diet_p + diet_w)+cohort * (hba1c_U65 + hba1c_O65 + raceBlack + educ + sexMale + cses + diet_p + diet_w)+age60 * (hba1c_U65 + hba1c_O65 + raceBlack + educ + sexMale + cses + diet_p + diet_w) + (1 + age60 | ID)")
