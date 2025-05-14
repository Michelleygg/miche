from flask import Flask, request
import joblib
import numpy as np
import pandas as pd


all_pairs = pd.read_csv("pairwise_flights.csv")

class Cascade:
    def __init__(self, data):
        self.data = data
        
    #method that returns the day with most delays from origin to destination in that year
    def query(self, origin, dest, year): 
        target = self.data[(self.data["ORIGIN"] == origin) & (self.data["DEST"] == dest) & (self.data["YEAR"] == year)]
        most_dep_delay = target.sort_values("delayed_dep", ascending = False).head(1)
        if most_dep_delay.empty:
            return most_dep_delay
        month = most_dep_delay.loc[:, "MONTH"].item()
        day = most_dep_delay.loc[:, "DAY_OF_MONTH"].item()
        final = pd.concat([most_dep_delay, self.data[(self.data["MONTH"] == month) & (self.data["DAY_OF_MONTH"] == day) & (self.data["YEAR"] == year) & (self.data["ORIGIN"] == dest)]])
        return final

model = Cascade(all_pairs)

app = Flask(__name__)

@app.route("/")
def app_page():
    return "This flask app allows get requests to query for cascading delays"

@app.route("/query")
def query():
    origin = request.args.get('origin')
    dest = request.args.get('dest')
    year = int(request.args.get('year'))
    pred = model.query(origin, dest, year)
    return pred.to_json(orient = "records")