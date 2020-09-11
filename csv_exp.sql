/* Import csv */
.mode csv
.import "C:/Users/wb519128/WBG/Sveta Milusheva - COVID 19 Results/proof-of-concept/panel_indicators/comparisson/i10_3.csv" i10
.import "C:/Users/wb519128/WBG/Sveta Milusheva - COVID 19 Results/proof-of-concept/panel_indicators/comparisson/i5_3.csv" i5

/* Test if it worked */
.schema i10
.schema i5

/* ------------------------------------------------------------- */
/*SELECT DISTINCT day FROM i10; */

SELECT COUNT(*) 
FROM (SELECT day, region, region_lag FROM i10)
GROUP BY day;

SELECT day, COUNT(DISTINCT (region || region_lag)) FROM i10 GROUP BY day;
SELECT connection_date, COUNT(DISTINCT (region_from || region_to)) FROM i5 GROUP BY connection_date;

/* ------------------------------------------------------------- */
/* Export 
.headers on
.mode csv
.output "C:/Users/wb519128/Desktop/i10.csv"
SELECT * FROM i10 WHERE day = '2020-03-21T00:00:00.000Z';
.quit
*/