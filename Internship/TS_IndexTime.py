#!/usr/bin/env python
# coding: utf-8

# In[1]:


import os
import pandas as pd
import numpy as np
import cx_Oracle
import matplotlib.pyplot as plt
import seaborn as sns
import plotly.express as px
from sqlalchemy import create_engine
from sqlalchemy.engine import url
from scipy.stats import skew, mode
from fitter import Fitter, get_common_distributions, get_distributions
import warnings 
warnings.filterwarnings('ignore')


# In[2]:


try:
    oracle_connection_string = 'oracle+cx_oracle://{username}:{password}@{hostname}:{port}/?service_name={database}'
    engine = create_engine(
        oracle_connection_string.format(
            username='gpib_read',
            password='read',
            hostname='sindb003-db.sin.infineon.com',
            port='1523',
            database='sindb003.sin.infineon.com'
        )
    )
except Exception as err:
    print(str(err))


# In[3]:


df = pd.read_sql("SELECT TEMPERATURE, TESTMODE, HANDLERTYPE,LOADBOARD, TESTPROGRAM, DEVICE, TESTER,LOT_ID, HANDLER, TESTERTYPE, TESTSITES, TESTCYCLE, TIMESTAMPSTARTLOT, TIMESTAMPENDLOT, PKG_CLASS, PKG_GROUP, PKG_NAME, pgsmaxsite, CYCLE_START, CYCLE_END,TESTPROGRAM_START, TESTPROGRAM_END, BASICTYPE, cycle.CYCLE_NUM, cycle.DT_START, cycle.INDEX_TIME, cycle.START_DATE_PARTITION, LEAD(cycle.DT_START) OVER (PARTITION BY cycle.GPIB_ID ORDER BY cycle.CYCLE_NUM) DT, summ.maxsite, summ.ett_reference,summ.INDEX_REFERENCE, summ.AVERAGE_TEST_SITE, (summ.INDEX_REFERENCE* ROUND(summ.AVERAGE_TEST_SITE)) AS TOTAL_INDEXREF FROM gpib.TESTER_GPID_DATA gpid LEFT JOIN gpib.TESTER_CYCLE_TEST cycle on gpid.GPIB_ID = cycle.GPIB_ID LEFT JOIN gpib.TESTER_SUMMARY summ on gpid.GPIB_ID = summ.GPIB_ID WHERE SITE = 'SIN' AND TESTTYPE = 'N' AND HANDLER LIKE 'DM%' AND TESTER LIKE 'SIV93K%' AND TIMESTAMPSTARTLOT>=20220201000000 AND TIMESTAMPSTARTLOT<=20220430000000", engine) #pgsmaxsite, summ.ett_reference


# In[4]:


# Ett parameter
ett_ref = 1.625


# In[5]:


# Separating data by <testmode_names, pkg_names, basictype, pgsmaxsite>
names = list(df.columns)
ett = df[df['ett_reference']==ett_ref]
testmode_names = ett['testmode'].unique()
pkg_names = ett['pkg_name'].unique()
basictype = ett['basictype'].unique()
pgsmaxsite = ett['pgsmaxsite'].unique()


# In[6]:


print(testmode_names)
print(pkg_names)
print(basictype)
print(pgsmaxsite)
# Data of ett_ref = 1.625 belongs to the same <testmode_names, pkg_names, basictype, pgsmaxsite> group


# In[7]:


dfe = ett[(ett['testmode']==testmode_names[0])&(ett['pkg_name']==pkg_names[0])&(ett['basictype']==basictype[0])]


# In[8]:


df1 = dfe


# In[9]:


# Filtering out data with index_time more than 60 seconds
df1 = df1[(df1.index_time<=60)&(df1.index_time>0)]
df1_1 = df1[df1.index_time>60]

# Getting different index_ref values
indexref = df1['total_indexref'].unique().tolist()
indexref


# In[50]:


# Before filter
y = df1.index_time
plt.hist(y, bins=100)
plt.xlabel('Index Time')
# plt.semilogy()
df2 = df1.loc[:,['index_time']]


# In[51]:


# After filter
y = df2.index_time
plt.hist(y, bins=100)
plt.xlabel('Index Time')


# In[52]:


# Skewness method corrects skewed data by removing either ends of the tails according to the skew score.
# Removes maximum 5% of data (threshold can be adjusted)

from scipy.stats import skew

def skewness(data):
    score = skew(data)
    Qhigh = 100
    Qlow = 0
    while (score>1)|(score<-1):
        if score>1:
            Qhigh = Qhigh-0.5
        else:
            Qlow = Qlow+0.5
        temp = data[data['index_time']>=np.percentile(data['index_time'], Qlow)]
        temp = temp[temp['index_time']<=np.percentile(data['index_time'], Qhigh)]
        score = skew(temp)
        if (Qhigh-Qlow)<=95:
            break
    print((Qlow, Qhigh))
    high = np.percentile(data, Qhigh)
    low = np.percentile(data, Qlow)
    data = data[data['index_time']>=low]
    data = data[data['index_time']<=high]
    return data


# In[53]:


df3 = skewness(df2)
df3


# In[54]:


# Before adjusting for skewness
plt.hist(df2, bins=100)
plt.xlabel('Index Time')


# In[55]:


# After adjusting for skewness
plt.hist(df3, bins=100)
plt.xlabel('Index Time')


# In[73]:


# By CLT theory, large sample data follows a normal istribution approximately.
# Fitting data to variations of normal distribution to find best Mu and sigma
time = df3['index_time']
dist = ['lognorm','norm','skewnorm','exponnorm','t','powerlognorm', 'powernorm', 'chi2', 'chi']


# In[74]:


f = Fitter(time,
           distributions=dist,
           timeout=60)
f.fit()
f.summary()


# In[75]:


r = f.get_best(method = 'sumsquare_error')
bestfit = list(r.keys())[0]


# In[76]:


Mu = f.fitted_param[bestfit][0]
Sig = f.fitted_param[bestfit][1]
bestfit


# In[77]:


# Hypothesis testing to check applicability of different index_ref values

import scipy.stats
def hypothesis_testing(Val, Mu, Sigma, n, alpha):
    print('Conducting',(1-alpha)*100,'% confidence interval hypothesis test against IndexRef of', 
          Val)
    print('Mu =', Mu, ' and Sigma =', Sigma)
    print("-"*75)
    print('H0: Mu == ', Val)
    print('H1: Mu =/= ', Val)
    print("-"*75)
    if (Sigma==0) or (n==0):
        print('Zero Denominator Warning')
        return
    Z = abs((Val-Mu)/Sigma*np.sqrt(n))
    Critical = abs(scipy.stats.norm.ppf(alpha))
    print('Critical value =', Critical)
    print('Z value =', Z)
    print("-"*75)
    if Z>Critical:
        print('Reject H0, IndexRef of ', Val, ' is insufficient at the ',(1-alpha)*100,'% confidence interval')
    else:
        print('Do not reject H0, IndexRef of ', Val, ' is sufficient at the ',(1-alpha)*100,'% confidence interval')
    print(" "*75)
    print(" "*75)
    print(" "*75)


# In[78]:


for i in indexref:
    hypothesis_testing(i, Mu, Sig, len(df2), 0.05)


# In[ ]:




