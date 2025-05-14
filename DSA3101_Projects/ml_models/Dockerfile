# syntax=docker/dockerfile:1

FROM python:3.10.6-slim-buster

WORKDIR /app

COPY requirements.txt requirements.txt
COPY app.py app.py 
COPY model.py model.py
COPY database.py database.py
COPY main.py main.py

RUN pip3 install --upgrade pip
RUN pip3 install -r requirements.txt

ENV FLASK_APP app
EXPOSE 5000

ENTRYPOINT [ "python", "-u", "-m", "flask", "run", "--host=0.0.0.0"]
