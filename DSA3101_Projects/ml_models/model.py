import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.tree import DecisionTreeClassifier
from scipy import stats
import joblib

def bin_time(crs_time):
    bin_dict = {0: '00-06', 1: '06-12', 2: '12-18', 3: '18-00'}
    if crs_time == 2400:
        return '18-00'
    return bin_dict[crs_time // 600]

def get_season(month):
    season_dict = {(1,2,3): 'spring', (4,5,6): 'summer', (7,8,9): 'autumn', (10,11,12): 'winter'}
    for key, val in season_dict.items():
        if month in key:
            return val
            
def create_lm_arr_model(result, year):

    try:
        arr_use_cols = ['month', 'day_of_week', 'crs_arr_time', 'distance', 'prcp_dest', 'snow_dest', 'snwd_dest', 'tmax_dest', 'tmin_dest', 'arr_delay']
        arr_df = result[arr_use_cols]

    except Exception as e:
        print(e)
        print("result from db is not a dataframe")
            
    arr_df['crs_arr_bin'] = arr_df['crs_arr_time'].apply(bin_time)
    arr_df['season'] = arr_df['month'].apply(get_season)
    arr_df = arr_df.astype({'day_of_week': 'category'})

    arr_regression_cols = ['season', 'day_of_week', 'crs_arr_bin', 'distance', 'prcp_dest', 'snow_dest', 'snwd_dest', 'tmax_dest', 'tmin_dest', 'arr_delay']
    arr_regression_df = arr_df[arr_regression_cols]
    arr_regression_df = arr_regression_df.dropna()
    arr_regression_df = pd.get_dummies(arr_regression_df, columns=['season', 'day_of_week', 'crs_arr_bin'])
    #Standardisation
    std_cols = ['distance', 'prcp_dest', 'snow_dest', 'snwd_dest', 'tmax_dest', 'tmin_dest', 'arr_delay']
    arr_regression_df[std_cols] = arr_regression_df[std_cols].apply(stats.zscore)

    arr_X = arr_regression_df.drop('arr_delay', axis=1)
    arr_y = arr_regression_df['arr_delay']

    lm_arr = LinearRegression()
    lm_arr.fit(arr_X, arr_y)

    file_to_save = f"lm_arr_{year}.pkl"

    joblib.dump(lm_arr, file_to_save, compress = 3)
    print(f'---Successfully dumped {file_to_save}---')

    return lm_arr

def create_lm_dep_model(result, year):

    try:
        dep_use_cols = ['month', 'day_of_week', 'crs_dep_time', 'distance', 'prcp_origin', 'snow_origin', 'snwd_origin', 'tmax_origin', 'tmin_origin', 'dep_delay']
        dep_df = result[dep_use_cols]
        
    except Exception as e:
        print(e)
        print("result from db is not a dataframe")
            
    dep_df['crs_dep_bin'] = dep_df['crs_dep_time'].apply(bin_time)        
    dep_df['season'] = dep_df['month'].apply(get_season)
    dep_df = dep_df.astype({'day_of_week': 'category'})

    dep_regression_cols = ['season', 'day_of_week', 'crs_dep_bin', 'distance', 'prcp_origin', 'snow_origin', 'snwd_origin', 'tmax_origin', 'tmin_origin', 'dep_delay']
    dep_regression_df = dep_df[dep_regression_cols]
    dep_regression_df = dep_regression_df.dropna()
    dep_regression_df = pd.get_dummies(dep_regression_df, columns=['season', 'day_of_week', 'crs_dep_bin'])
    #Standardisation
    std_cols = ['distance', 'prcp_origin', 'snow_origin', 'snwd_origin', 'tmax_origin', 'tmin_origin', 'dep_delay']
    dep_regression_df[std_cols] = dep_regression_df[std_cols].apply(stats.zscore)

    dep_X = dep_regression_df.drop('dep_delay', axis=1)
    dep_y = dep_regression_df['dep_delay']

    lm_dep = LinearRegression()
    lm_dep.fit(dep_X, dep_y)

    file_to_save = f"lm_dep_{year}.pkl"

    joblib.dump(lm_dep, file_to_save, compress = 3)
    print(f'---Successfully dumped {file_to_save}---')

    return lm_dep

def create_dt_arr_model(result, year):

    try:
        arr_use_cols = ['month', 'day_of_week', 'crs_arr_time', 'distance', 'prcp_dest', 'snow_dest', 'snwd_dest', 'tmax_dest', 'tmin_dest', 'arr_delay']
        arr_df = result[arr_use_cols]

    except Exception as e:
        print(e)
        print("result from db is not a dataframe")
            
    arr_df['crs_arr_bin'] = arr_df['crs_arr_time'].apply(bin_time)
    arr_df['season'] = arr_df['month'].apply(get_season)
    arr_df = arr_df.astype({'day_of_week': 'category'})
    arr_df['has_arr_delay'] = arr_df['arr_delay'].apply(lambda x: 1 if x >= 60 else 0)

    arr_regression_cols = ['season', 'day_of_week', 'crs_arr_bin', 'distance', 'prcp_dest', 'snow_dest', 'snwd_dest', 'tmax_dest', 'tmin_dest', 'has_arr_delay']
    arr_regression_df = arr_df[arr_regression_cols]
    arr_regression_df = arr_regression_df.dropna()
    arr_regression_df = pd.get_dummies(arr_regression_df, columns=['season', 'day_of_week', 'crs_arr_bin'])
    arr_X = arr_regression_df.drop('has_arr_delay', axis=1)
    arr_y = arr_regression_df['has_arr_delay']

    dt_arr = DecisionTreeClassifier()
    dt_arr.fit(arr_X, arr_y)

    file_to_save = f"dt_arr_{year}.pkl"

    joblib.dump(dt_arr, file_to_save, compress = 3)
    print(f'---Successfully dumped {file_to_save}---')

    return dt_arr


def create_dt_dep_model(result, year):

    try:
        dep_use_cols = ['month', 'day_of_week', 'crs_dep_time', 'distance', 'prcp_origin', 'snow_origin', 'snwd_origin', 'tmax_origin', 'tmin_origin', 'dep_delay']
        dep_df = result[dep_use_cols]
        
    except Exception as e:
        print(e)
        print("result from db is not a dataframe")
            
    dep_df['crs_dep_bin'] = dep_df['crs_dep_time'].apply(bin_time)        
    dep_df['season'] = dep_df['month'].apply(get_season)
    dep_df = dep_df.astype({'day_of_week': 'category'})
    dep_df['has_dep_delay'] = dep_df['dep_delay'].apply(lambda x: 1 if x >= 60 else 0)

    dep_regression_cols = ['season', 'day_of_week', 'crs_dep_bin', 'distance', 'prcp_origin', 'snow_origin', 'snwd_origin', 'tmax_origin', 'tmin_origin', 'has_dep_delay']
    dep_regression_df = dep_df[dep_regression_cols]
    dep_regression_df = dep_regression_df.dropna()
    dep_regression_df = pd.get_dummies(dep_regression_df, columns=['season', 'day_of_week', 'crs_dep_bin'])
    dep_X = dep_regression_df.drop('has_dep_delay', axis=1)
    dep_y = dep_regression_df['has_dep_delay']

    dt_dep = DecisionTreeClassifier()
    dt_dep.fit(dep_X, dep_y)

    file_to_save = f"dt_dep_{year}.pkl"

    joblib.dump(dt_dep, file_to_save, compress = 3)
    print(f'---Successfully dumped {file_to_save}---')

    return dt_dep