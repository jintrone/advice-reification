import pandas as pd
import numpy as np
from sklearn import linear_model
from sklearn.ensemble import RandomForestClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.svm import SVC
from sklearn.tree import DecisionTreeClassifier
from sklearn import cross_validation
from sklearn.metrics import cohen_kappa_score, make_scorer

#input = pd.read_csv('features_final/Community_features.csv', index_col = 0)
#input = pd.read_csv('features_final/ProvideEmotion_features.csv', index_col = 0)
#input = pd.read_csv('features_final/ProvideInformation_features.csv', index_col = 0)
input = pd.read_csv('/Users/josh/Dropbox/@PAPERS/2017/CSCW/data/classification/receivedinfo.csv', index_col = 0)
#input = pd.read_csv('/Users/josh/Dropbox/@PAPERS/2017/CSCW/data/classification/provideinfo.csv', index_col = 0)
#input = pd.read_csv('/Users/josh/Dropbox/@PAPERS/2017/CSCW/data/classification/test.csv', index_col = 0, nrows=1000)
#input = pd.read_csv('features_updated/r_original_2_python_5463_post and question mark.csv', index_col = 0)

shuffled_input = input.reindex(np.random.permutation(input.index))
#print shuffled_input

#print(input.columns)
neg_proportion = len(input[input.ix[:,-1]==0])/float(len(input))

print('Proportion of negative class: '+str(neg_proportion))

#kappa = make_scorer(cohen_kappa_score)

########Logistic Regression###############
#logistic = linear_model.LogisticRegression(class_weight='balanced') # assign class_weight actually make accuracy lower, because more positive class are being identified
logistic = linear_model.LogisticRegression(class_weight={0:1, 1: 2}, penalty='l2')
print('Logistic Regression L2:')
scores = cross_validation.cross_val_score(logistic, shuffled_input.ix[:,0:-1], y=shuffled_input.ix[:,-1],cv = 10, scoring='accuracy')
print("Mean accuracy Logistic Regression from 10-fold cross-validation:")
print(scores.mean())

########Decision Tree###############
# dt = DecisionTreeClassifier(class_weight={0:1, 1:1}, max_depth=10)
# print 'Decision Tree:'
# scores_dt = cross_validation.cross_val_score(dt, shuffled_input.ix[:,0:5452], shuffled_input.ix[:,5452],cv = 10, scoring='accuracy')
# print "Mean accuracy Decision Tree from 10-fold cross-validation:"
# print scores_dt.mean()



##########Support Vector Machine###############
# svm = SVC(class_weight={0:1, 1:1}, kernel='linear')
# print 'SVM Linear Kernel:'

#svm = OneVsRestClassifier(BaggingClassifier(SVC(kernel='linear', probability=True, class_weight='auto'), max_samples=1.0 / n_estimators, n_estimators=n_estimators))

# scores_svm = cross_validation.cross_val_score(svm, shuffled_input.ix[:,0:5452], shuffled_input.ix[:,5452],cv = 10, scoring='accuracy')
# print "Mean accuracy SVM from 10-fold cross-validation:"
# print scores_svm.mean()



##########Gaussian Naive Bayes###############
# gnb = GaussianNB()
# print 'Gaussian Naive Bayes:'
#
# scores_gnb = cross_validation.cross_val_score(gnb, shuffled_input.ix[:,0:5452], shuffled_input.ix[:,5452],cv = 10, scoring='accuracy')
# print "Mean accuracy Gaussian NB from 10-fold cross-validation:"
# print scores_gnb.mean()