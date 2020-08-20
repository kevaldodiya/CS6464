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
import sklearn

def selection(rho,lambada):
	if rho < -lambada:
		return lambada + rho
	elif rho>lambada:
		return rho-lambada
	else:
		return 0

def coordinate(x_train,y_train,lambada,num_iteration):
	w_total = []
	
	x_train,x_test,y_train,y_test = train_test_split(x,y,test_size = 0.2,random_state = None)
	x_train = x_train.values
	y_train = y_train.values
	y_test = y_test.values
	x_test = x_test.values
	w = np.ones((x_train.shape[1],1))
	for var1 in range(num_iteration):
		print("round" , var1)
		for var2 in range(x_train.shape[1]):
			col= x_train[:,var2].reshape(x_train.shape[0],1)
			row = col.transpose()
			y_pred = x_train @ w
			ytrain = y_train.reshape(x_train.shape[0],1)
			sum = row @ (ytrain - y_pred  + w[var2]*col)
			w[var2] = selection(sum,lambada)
		w_total.append(w.transpose())
	return w,w_total
def lasso_cross_validation(x,y,lambada):
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
		lasso = Lasso(alpha = lambada,max_iter = 100)	
		lasso.fit(x_train,y_train)
		mse.append(sklearn.metrics.mean_squared_error(lasso.predict(x_test),y_test))
	mse = np.array(mse)
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
num_iteration = 100
lambada = 0.0001

error = lasso_cross_validation(newdata_x,newdata_y,lambada)
print('\n')
print('\n')
print(error)
print('\n')
print('\n')

w_total =[]
for var1 in range(1,100):
	lasso = Lasso(alpha = lambada,max_iter = var1)	
	lasso.fit(newdata_x,newdata_y)
	w_total.append(lasso.coef_)

w_total_ar = np.array(w_total)
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
