import numpy as np
import matplotlib.pyplot as plt
import math
import pandas as pd
from sklearn import preprocessing
from sklearn.model_selection import train_test_split
from sklearn.linear_model import Lasso
from sklearn.linear_model import ElasticNet
from sklearn.model_selection import KFold
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import cross_val_score
import statsmodels.api as sm
import sklearn

def step_cross_validation(x,y,features):
	kf = KFold(n_splits= 10,random_state=None, shuffle=False)
	data =[]
	for var1 in range(len(features)):
		data.append(x[:,features[var1]])
	data = np.array(data)
	data = data.transpose()
	po = preprocessing.PolynomialFeatures(degree = 2)
	x_train = po.fit_transform(data)
	y_train = po.fit_transform(y)
	lm=sklearn.linear_model.LinearRegression()
	model = lm.fit(x_train,y_train)
	mse = cross_val_score(model,x_train,y_train,scoring = 'neg_mean_squared_error',cv = kf)
	mse = np.abs(mse)
	error = np.mean(mse)
	return error



train_data = pd.read_csv("kc_house_train_data.csv")
test_data = pd.read_csv("kc_house_test_data.csv")
f = [train_data,test_data]
train_data = pd.concat(f,axis =0)
train_data = train_data.drop(labels = ['id','date'],axis = 1)
y=train_data['price']
y = y.to_numpy()
y = y.reshape(-1,1)
x =train_data.drop(labels = ['price'],axis = 1)
st = sklearn.preprocessing.StandardScaler()
newdata_x = st.fit_transform(x)
newdata_y = st.fit_transform(y)
print(newdata_x)
print(newdata_y)
features = []
coeff = []
count = newdata_x.shape[1]
dim = 0
co = np.zeros(count)
coeff.append(co)
while count>0:
	dict1 = {}
	dim = dim + 1
	co = np.zeros(count)
	print("dim = " ,dim)
	if dim > 18:
		break
	if not features:
		for var1 in range(count):
			temp = newdata_x[:,var1].reshape(-1,1)
			model1 = sm.OLS(newdata_y,temp)
			model2 = model1.fit()
			dict1[var1] =model2.pvalues[0]                             		
		tem = min(dict1,key = dict1.get)
		if dict1[tem] < 0.000001:
			features.append(tem)
			model1 = sm.OLS(newdata_y,newdata_x[:,tem].reshape(-1,1))
			model2 = model1.fit()
			co[tem] = model2.params
			print(co)
			coeff.append(co)
		else:
			break
	else:
		data =[]
		for var1 in range(len(features)):
			data.append(newdata_x[:,features[var1]])
		for var1 in range(count):
			data1 = data
			if var1 not in features:
				data1.append(newdata_x[:,var1])
				data1 = np.array(data1)
				data1 = data1.transpose()
				last = data1.shape[1]-1
				#print(last+1)
				model1 = sm.OLS(newdata_y,data1)
				model2 = model1.fit()
				dict1[var1] = model2.pvalues[last]
		print(dict1)
		tem = min(dict1,key = dict1.get)
		data.append(newdata_x[:,tem])
		data = np.array(data)
		data = data.transpose()
		if dict1[tem] < 0.005:
			features.append(tem)
			model1 = sm.OLS(newdata_y,data)
			model2 = model1.fit()
			temp = model2.params
			t1 =0
			for var1 in features:
				co[var1] = temp[t1]
				t1 = t1+1
			print(co)
			coeff.append(co)
		else:
			break

error = step_cross_validation(newdata_x,newdata_y,features)
print(error)
w_total_ar = np.array(coeff)
w_total_var = w_total_ar.var(axis = 0)
ind = np.argsort(w_total_var)
ink = ind[-5:]
last = ind.shape[0]
temp =[]
for var1 in range(0,5):
	temp.append(w_total_ar[:,ink[var1]])
temp = np.array(temp)
lab = list(x.columns.values)
for var1 in range(temp.shape[0]):
	plt.plot(temp[var1,:],label = lab[ink[var1]])
plt.xlabel("iterations")
plt.ylabel("coeff. values")
plt.legend(loc = 'upper right',bbox_to_anchor=(1.0, 1.02))
plt.show()