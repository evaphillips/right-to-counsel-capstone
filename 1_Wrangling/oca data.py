#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Dec  9 19:07:34 2023

@author: lynnmiao
"""

import sqlite3
import random
import numpy as np
import pandas as pd
from scipy.stats import bootstrap, permutation_test
import matplotlib.pyplot as plt
import seaborn as sns
sns.set() #adds a style to seaborn plots
import scipy.stats as stats
import re
import os
import plotly.express as px

# read in the data



oca_index = pd.read_csv('/Users/lynnmiao/Library/CloudStorage/OneDrive-nyu.edu/right-to-counsel-capstone-data/01_Eviction/OCA/oca_index.csv')
oca_addresses = pd.read_csv('/Users/lynnmiao/Library/CloudStorage/OneDrive-nyu.edu/right-to-counsel-capstone-data/01_Eviction/OCA/oca_addresses.csv')
nyc_zips = pd.read_csv('/Users/lynnmiao/Library/CloudStorage/OneDrive-nyu.edu/right-to-counsel-capstone-data/01_Eviction/OCA/nyc_zip.csv')

# join oca_index with zip code

oca_index_zip = pd.merge(oca_index, oca_addresses, on='indexnumberid', how='left')

# convert the zips to be 5 digit and integer (some values are 11694-2802)
oca_index_zip['postalcode'] = oca_index_zip['postalcode'].astype(str).str[:5].astype(int)

# join to list of nyc zip codes - if null, not in nyc
oca_index_zip = pd.merge(oca_index_zip, nyc_zips[['Neighborhood','ZipCode']], left_on='postalcode', right_on='ZipCode', how='left')

# separate into nyc and non-nyc

non_nyc = oca_index_zip[oca_index_zip['ZipCode'].isnull()]

nyc = oca_index_zip[oca_index_zip['ZipCode'].notnull()]

# convert to datetime
non_nyc['fileddate'] = pd.to_datetime(non_nyc['fileddate']) 
nyc['fileddate'] = pd.to_datetime(nyc['fileddate']) 


# find average # of eviction filings by month
monthly_count = non_nyc.resample('M', on='fileddate').size()
monthly_count = pd.DataFrame(monthly_count)
monthly_count.reset_index(inplace=True)
monthly_count.rename(columns={0: 'filings'}, inplace=True)


# find average # of eviction filings by month NYC
nyc_count = nyc.resample('M', on='fileddate').size()
nyc_count = pd.DataFrame(nyc_count)
nyc_count.reset_index(inplace=True)
nyc_count.rename(columns={0: 'filings'}, inplace=True)


plt.figure(figsize=(10,4))
plt.plot(monthly_count['fileddate'], monthly_count['filings']
         ,marker='o', linestyle='-', color='b', label='Eviction Filings')
plt.title('Eviction Filings NON NYC by Month')
plt.grid(True)
plt.legend()
plt.show()


plt.figure(figsize=(10,4))
plt.plot(nyc_count['fileddate'], nyc_count['filings']
         ,marker='o', linestyle='-', color='b', label='Eviction Filings')
plt.title('Eviction Filings NYC by Month')
plt.grid(True)
plt.legend()
plt.show()