from flask import Flask, request, render_template
from email_validator import validate_email, EmailNotValidError
import pandas as pd
import datetime
import mysql.connector

app = Flask(__name__)

def establish_sql_connection():
    db = mysql.connector.connect(
    host="db",
    user="user",
    password="pass",
    database="mydb"
    )
    return db

@app.route("/", methods=["GET"])
def get_users_table():

    # Get users from db
    db = establish_sql_connection()
    cursor = db.cursor()
    cursor.execute("SELECT * FROM users_info")
    result = cursor.fetchall()
    df = pd.DataFrame(result, columns=['Name','Email Address','Created At'])

    return render_template('users_table.html', users=df.to_html(), url='register')

@app.route("/register", methods=["POST"])
def register_user():
    return render_template('registration_input.html')

def check_email(email):
    try:
        v = validate_email(email)
        email = v["email"] # replace with normalized form
        return 'OK'
    except EmailNotValidError as e:
        return str(e)

@app.route("/result")
def show_result():
    new_username = request.form['username']
    new_email = request.form['email_address']

    # check if new_username is valid
    if new_username == '' or new_username.isspace():
        string = "Name of user must not be blank."
        return render_template('result_unsuccessful.html', email=new_email, error_string=string)

    # check if new_email is a valid email address
    string = check_email(new_email)
    if string != 'OK':
        return render_template('result_unsuccessful.html', email=new_email, error_string=string)

    # check if email exists in db
    db = establish_sql_connection()
    cursor = db.cursor()
    cursor.execute("SELECT * FROM users_info WHERE email='{}'".format(new_email))
    result = cursor.fetchall()
    df = pd.DataFrame(result, columns=['Name','Email Address','Created At'])

    if len(df) > 0:
        # Render template for unsuccessful registration
        string = "This user has already been registered."
        return render_template('result_unsuccessful.html', email=new_email, error_string=string)
    else:
        # add new user to db
        timenow = datetime.datetime.utcnow() + datetime.timedelta(hours=8)
        timenow = timenow.strftime("%Y-%m-%d %H:%M:%S")
        db = establish_sql_connection()
        cursor = db.cursor()
        cursor.execute("INSERT INTO users_info (name,email,creation_time) values ('{}','{}','{}')".format(new_username, new_email, timenow))
        db.commit()

        # Render template for successful registration
        return render_template('result_successful.html', username=new_username, email=new_email)

if __name__ =="__main__":
    app.run(port=5000, host="0.0.0.0", debug=True)
