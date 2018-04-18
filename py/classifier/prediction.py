import pandas as pd
import numpy as np
from sklearn import linear_model

class predictInfo:

    def __init__(self):
        p_train = pd.read_csv('/Users/josh/Dropbox/@PAPERS/2017/CSCW/data/classification/provideinfo.csv', index_col = 0)
        self.p_model = linear_model.LogisticRegression(class_weight={0:1, 1: 2}, penalty='l2')
        self.p_model.fit(p_train.ix[:,0:-1], p_train.ix[:,-1])
        del(p_train)
        print("Learned provide info model")

        r_train = pd.read_csv('/Users/josh/Dropbox/@PAPERS/2017/CSCW/data/classification/receivedinfo.csv', index_col = 0)
        self.r_model =  linear_model.LogisticRegression(class_weight={0:1, 1: 2}, penalty='l2')
        self.r_model.fit(r_train.ix[:,0:-1], r_train.ix[:,-1])
        del(r_train)
        print("Learned receive info model")

    def processCorpus(self,corpus):
        print("Predict %s"%(corpus))
        topredict =  pd.read_csv('/Users/josh/Dropbox/@PAPERS/2017/CSCW/data/classification/%s.topredict.csv'%(corpus), index_col = 0)
        p =  self.p_model.predict(topredict)
        r =  self.r_model.predict(topredict)
        out_p = pd.DataFrame(p,index=topredict.index.values, columns=['pi'])
        out_r = pd.DataFrame(r,index=topredict.index.values, columns=['ri'])
        final = out_p.join(out_r)
        final.to_csv('/Users/josh/Dropbox/@PAPERS/2017/CSCW/data/classification/%s.labelled.csv'%(corpus))



def main():
    corpora = ["anxiety_and_panic_disorders_exchange","bipolar_disorder_exchange","cancer_community","colorectal_cancer_exchange","depression_exchange","erectile_dysfunction_exchange","eye_health_community","food_and_cooking_exchange","gynecology_exchange","heart_disease_exchange","hypertension_and_high_blood_pressure_exchange","infertility_and_reproduction_exchange","knee_and_hip_replacement_exchange","lupus_exchange","mens_health_community","migraines_and_headaches_exchange","newborn_and_baby_exchange","oral_health_exchange","osteoarthritis_exchange","pet_health_exchange","pregnancy_exchange","prostate_cancer_exchange","raising_fit_kids_community","rheumatoid_arthritis_exchange","skin_and_beauty_exchange","skin_problems_and_treatments_exchange","sleep_disorders_exchange","smoking_cessation_exchange","sports_medicine_exchange","stroke_exchange","substance_abuse_exchange"]
    p = predictInfo()
    for corpus in corpora:
        p.processCorpus(corpus)



if __name__ == "__main__":
    main()




