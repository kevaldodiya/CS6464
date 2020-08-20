import numpy as np
import matplotlib.pyplot as plt
import math
import pandas as pd
from sklearn import preprocessing
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.linear_model import Lasso
from sklearn.linear_model import ElasticNet
from sklearn.model_selection import KFold
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import cross_val_score
from sklearn.linear_model import SGDRegressor
import sklearn

def poly_cross_validation(x,y,it):
	
	kf = KFold(n_splits= 10,random_state=None, shuffle=False)
	po = preprocessing.PolynomialFeatures(degree = 2)
	x_train = po.fit_transform(x)
	y_train = y
	reg = SGDRegressor(max_iter = it,penalty = "elasticnet",loss = 'huber',tol = 1e-3, average = True)
	model = reg.fit(x_train,y_train)
	mse = cross_val_score(model,x_train,y_train,scoring = 'neg_mean_squared_error',cv = kf)
	mse = np.abs(mse)
	error = np.mean(mse)
	#print(model.coef_)
	return error, model.coef_ , x_train.shape[1]
	'''
	kf = KFold(n_splits= 10,random_state=None, shuffle=False)
	for train,test in kf.split(x):
		x_train =[]
		y_train =[]
		mse =[]
		for var1 in train:
			x_train.append(x[var1,:])
			y_train.append(y[var1,:])
		x_test =[]
		y_test=[]
		for var1 in test:
			x_test.append(x[var1,:])
			y_test.append(y[var1,:])
		x_train = np.array(x_train)
		x_test = np.array(x_test)
		y_train = np.array(y_train).reshape(-1,1)
		y_test = np.array(y_test).reshape(-1,1)
		po = preprocessing.PolynomialFeatures(degree = 2)
		x_train = po.fit_transform(x_train)
		y_train = po.fit_transform(y_train)
		x_test = po.fit_transform(x_test)
		y_test = po.fit_transform(y_test)
		lm=sklearn.linear_model.LinearRegression()
		model = lm.fit(x_train,y_train)	
		mse.append(sklearn.metrics.mean_squared_error(model.predict(x_test),y_test))

	mse = np.array(mse)
	error = np.mean(mse)
	return(error)
	'''
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
num_iteration = 100

it = [100,1000,1500,3000]
w_total =[]
for i in it:
	error,cof,fes = poly_cross_validation(newdata_x,newdata_y,i)
	#print(cof)
	w_total.append(cof)	




#co = np.zeros(newdata_x.shape[1])
#w_total.append(co)
w_total_ar = np.array(w_total)
w_total_var = w_total_ar.var(axis = 0)
ind = np.argsort(w_total_var)
ink = ind[-5:]
last = ind.shape[0]
temp =[]
for var1 in range(0,5):
	temp.append(w_total_ar[:,ink[var1]])
temp = np.array(temp)
lab = list(range(fes))
for var1 in range(temp.shape[0]):
	plt.plot(temp[var1,:],label = lab[ink[var1]])
plt.xlabel("iterations")
plt.ylabel("coeff. values")
plt.legend(loc = 'upper right',bbox_to_anchor=(1.0, 1.02))
plt.show()
