import mysql.connector

def connect_db():

    try:
        db = mysql.connector.connect(
            host="mysql",
            port= "3306",
            user="root",
            password="Kktctyt123!",
            database="airlines"
        )
        return db
    
    except Exception as e:
        print(e)
        print("Cannot connect_db")