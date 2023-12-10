#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov  2 21:26:01 2023

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

# read in the data

county = pd.read_csv('/Users/lynnmiao/Library/CloudStorage/OneDrive-nyu.edu/right-to-counsel-capstone-data/01_Eviction/baseline_2000-2018/county_court-issued_2000_2018.csv')
county_proprietary = pd.read_csv('/Users/lynnmiao/Library/CloudStorage/OneDrive-nyu.edu/right-to-counsel-capstone-data/01_Eviction/baseline_2000-2018/county_proprietary_valid_2000_2018 (1).csv')
tract_proprietary = pd.read_csv('/Users/lynnmiao/Library/CloudStorage/OneDrive-nyu.edu/right-to-counsel-capstone-data/01_Eviction/baseline_2000-2018/tract_proprietary_valid_2000_2018.csv')

#ohio_tract = tract_proprietary[tract_proprietary['county'] == 'Cuyahoga County']

##############################
#CLEVELAND 
##############################
ohio = tract_proprietary[tract_proprietary['cofips'] == 39035]

# only 5 years of data - 2002, 2003, 2014, 2017, 2018 from tract level
ohio_year_fips_summary = ohio.groupby('year').agg({'fips':'nunique'
                                                   ,'filing_rate':'mean'
                                                   ,'filings':'sum'
                                                   ,'threatened':'sum'
                                                   ,'threatened_rate':'mean'
                                                   ,'judgements':'sum'
                                                   ,'judgement_rate':'mean'})

# COUNTY data from 2002 to 2018
ohio_county = county[county['county'] == 'Cuyahoga County']

ohio_county_summary = ohio_county.groupby('year').agg({'filings_observed':'sum'
                                                   ,'hh_threat_observed':'sum'
                                                   ,'ind_filings_court_issued_lt':'sum'}).reset_index()


plt.figure(figsize=(10,4))
plt.plot(ohio_county_summary['year'], ohio_county_summary['filings_observed']
         ,marker='o', linestyle='-', color='b', label='Eviction Filings')
plt.title('Eviction Filings Cleveland by Year')
plt.grid(True)
plt.legend()
plt.show()

# COUNTY PROPRIETARY data from 2004 to 2016

ohio_county_p = county_proprietary[county_proprietary['county'] == 'Cuyahoga County']

ohio_county_summary = ohio_county.groupby('year').agg({'filings_observed':'sum'
                                                   ,'hh_threat_observed':'sum'
                                                   ,'ind_filings_court_issued_lt':'sum'}).reset_index()

##############################
#Kansas City
##############################

kc = tract_proprietary[tract_proprietary['cofips'] == 29095]

kc_year_fips_summary = kc.groupby('year').agg({'fips':'nunique','filing_rate':'mean','filings':'sum'
                                                   ,'threatened':'sum'
                                                   ,'threatened_rate':'mean'
                                                   ,'judgements':'sum'
                                                   ,'judgement_rate':'mean'})

kc_county = county[county['fips_county'] == 29095]


kc_county_summary = kc_county.groupby('year').agg({'filings_observed':'sum'
                                                   ,'hh_threat_observed':'sum'
                                                   ,'ind_filings_court_issued_lt':'sum'}).reset_index()

plt.figure(figsize=(10,4))
plt.plot(kc_county_summary['year'], kc_county_summary['filings_observed']
         ,marker='o', linestyle='-', color='b', label='Eviction Filings')
plt.title('Eviction Filings KC by Year')
plt.grid(True)
plt.legend()
plt.show()


##############################
#New Orleans
##############################
# no tract data at all
nola = tract_proprietary[tract_proprietary['county'] == 'Orleans Parish']

nola_county = county[county['county'] == 'Orleans Parish']

nola_county_summary = nola_county.groupby('year').agg({'filings_observed':'sum'
                                                   ,'hh_threat_observed':'sum'
                                                   ,'ind_filings_court_issued_lt':'sum'}).reset_index()

plt.figure(figsize=(10,4))
plt.plot(nola_county_summary['year'], nola_county_summary['filings_observed']
         ,marker='o', linestyle='-', color='b', label='Eviction Filings')
plt.title('Eviction Filings NOLA by Year')
plt.grid(True)
plt.legend()
plt.show()

##############################
#Philadelphia
##############################

phil = tract_proprietary[tract_proprietary['county'] == 'Philadelphia County']

phil_year_fips_summary = phil.groupby('year').agg({'fips':'nunique','filing_rate':'mean','filings':'sum'
                                                   ,'threatened':'sum'
                                                   ,'threatened_rate':'mean'
                                                   ,'judgements':'sum'
                                                   ,'judgement_rate':'mean'})

phil_year_fips_summary['filings'].mean()

phil_county = county[county['county'] == 'Philadelphia County']

phil_county_summary = phil_county.groupby('year').agg({'filings_observed':'sum'
                                                   ,'hh_threat_observed':'sum'
                                                   ,'ind_filings_court_issued_lt':'sum'}).reset_index()

plt.figure(figsize=(10,4))
plt.plot(phil_county_summary['year'], phil_county_summary['filings_observed']
         ,marker='o', linestyle='-', color='b', label='Eviction Filings')
plt.title('Eviction Filings Philadelphia by Year')
plt.grid(True)
plt.legend()
plt.gca().set_xticks(phil_county_summary['year'].unique())
plt.show()


##############################
#DC
##############################

dc = tract_proprietary[tract_proprietary['cofips'] == 11001]

phil_year_fips_summary['filings'].mean()

dc_county = county[county['fips_county'] == 11001]




##############################
#NBoston
##############################


boston = tract_proprietary[tract_proprietary['cofips'] == 	25025]


boston_year_fips_summary = boston.groupby('year').agg({'fips':'nunique','filing_rate':'mean','filings':'sum'
                                                   ,'threatened':'sum'
                                                   ,'threatened_rate':'mean'
                                                   ,'judgements':'sum'
                                                   ,'judgement_rate':'mean'})

##############################
#NYC
##############################


nyc = tract_proprietary[tract_proprietary['county'] == 'New York County']


nyc_year_fips_summary = nyc.groupby('year').agg({'fips':'nunique','filing_rate':'mean','filings':'sum'
                                                   ,'threatened':'sum'
                                                   ,'threatened_rate':'mean'
                                                   ,'judgements':'sum'
                                                   ,'judgement_rate':'mean'}).reset_index()


nyc_year_fips_summary['filing_rate'][1]

plt.figure(figsize=(10,4))
plt.plot(nyc_year_fips_summary['year'], nyc_year_fips_summary['filing_rate']
         ,marker='o', linestyle='-', color='b', label='Eviction Filings')
for i in range(len(nyc_year_fips_summary['year'])):
    plt.text(nyc_year_fips_summary['year'][i], nyc_year_fips_summary['filing_rate'][i], f'{nyc_year_fips_summary["filing_rate"][i]:.1f}%', ha='right', va='bottom')
plt.title('Eviction Filing Rate NYC by Year')
plt.grid(True)
plt.legend()
plt.gca().set_xticks(nyc_year_fips_summary['year'].unique())
plt.show()


plt.figure(figsize=(10,4))
plt.plot(nyc_year_fips_summary['year'], nyc_year_fips_summary['filings']
         ,marker='o', linestyle='-', color='b', label='Eviction Filings')
for i in range(len(nyc_year_fips_summary['year'])):
    plt.text(nyc_year_fips_summary['year'][i], nyc_year_fips_summary['filings'][i], f'{nyc_year_fips_summary["filings"][i]:.0f}', ha='right', va='bottom')
plt.title('Eviction Filings NYC by Year')
plt.grid(True)
plt.legend()
plt.gca().set_xticks(nyc_year_fips_summary['year'].unique())
plt.show()

