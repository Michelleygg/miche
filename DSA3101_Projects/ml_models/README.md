## Machine learning models
- linear regression (lm)
- decision tree (dt)

## Execute these steps on bash shell to run the flask app
1. docker pull swamyhn/mysqldb
2. cd to this folder
3. docker compose up --build (this will take around 5mins)
4. The homepage will be on localhost:1000
5. Model outputs will be on localhost:1000/coefficients?mode=<ml_model>_<direction>_<year> where
    - ml_model: lm, dt
    - direction: arr, dep
    - year: 1989, 1990, 2000, 2001, 2006, 2007
6. If the input for 'mode' is new, the loading time will be around 1-2 mins. Otherwise, the output is instantaneous.