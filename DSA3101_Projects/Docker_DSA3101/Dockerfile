# syntax=docker/dockerfile:1
FROM python:3.10.6-slim-buster

## Add Python script, requirements file and templates folder to /app 
## directory on the image:
WORKDIR /app
COPY . /app
RUN apt-get update && apt-get install -y gcc build-essential python-dev default-libmysqlclient-dev

RUN pip3 install --upgrade pip
RUN pip3 install -r requirements.txt

## Set the environment variable for the flask application here:
ENV FLASK_APP=users.py
EXPOSE 5000
EXPOSE 3306

ENTRYPOINT [ "python", "-u", "-m", "flask", "run", "--host=0.0.0.0"]
