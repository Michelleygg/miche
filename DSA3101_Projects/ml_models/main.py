import pandas as pd
import database
import model

def load_and_train(mode, ml_model):

    yr = mode[-4:]
    dir = mode[3:6]

    try:
        db = database.connect_db()

        print('---Successfully connected to db---')

        cursor = db.cursor()
        
        if dir == 'arr':
            cursor.execute(f"""SELECT month, day_of_week, crs_arr_time, distance, prcp_dest, 
            snow_dest, snwd_dest, tmax_dest, tmin_dest, arr_delay FROM year_{yr}""")
            result = cursor.fetchall()
            df = pd.DataFrame(result, columns=[desc[0] for desc in cursor.description])
            if ml_model == 'lm':
                myModel = model.create_lm_arr_model(df, yr)
                print('---Successfully created lm model---')
            elif ml_model == 'dt':
                myModel = model.create_dt_arr_model(df, yr)
                print('---Successfully created dt model---')

        
        elif dir == 'dep':
            cursor.execute(f"""SELECT month, day_of_week, crs_dep_time, distance, prcp_origin, 
            snow_origin, snwd_origin, tmax_origin, tmin_origin, dep_delay FROM year_{yr}""")
            result = cursor.fetchall()
            df = pd.DataFrame(result, columns=[desc[0] for desc in cursor.description])
            if ml_model == 'lm':
                myModel = model.create_lm_dep_model(df, yr) 
                print('---Successfully created lm model---')
            elif ml_model == 'dt':
                myModel = model.create_dt_dep_model(df, yr) 
                print('---Successfully created dt model---')
        
        return myModel
        
    except Exception as e:
        print(e)
        print("Cannot load_and_train")
    