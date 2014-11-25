spool /home/anitavd/PhD/Sub-daily/data/tipping/hourly_AM_2014.txt

set NULL 'NA'
set trimspool on
column stnr format 99999 heading STNR
column utm_east format 9999999 heading EAST
column utm_north format 9999999 heading NORTH
column lon format 99.9999 heading LON
column lat format 99.9999 heading LAT
column year format 9999 heading YEAR
column month format 99 heading MONTH
column day format 99 heading DAY
column rr_1 format 99.9 heading RR_1
set pagesize 50000 
SET LONG 32000
set lines 180
set heading on

select stnr, to_number(substr(year,1,4)), max(rr_1) from (select stnr, year, sum(mm) RR_1 
from (select stnr, to_char(dato,'YYYY.MM.DD HH24') year, rr_01 mm from t_pdata t1 
where dato between '01.01.2013' and '31.12.2013') where mm>0 group by stnr, year order by stnr, year) group by stnr, to_number(substr(year,1,4)) 
order by  stnr, to_number(substr(year,1,4)); 

  3015			      2013	 13,6
  3019			      2013	  8,4
  3030			      2013	  9,9
  3120			      2013	  7,5
 11450			      2013	  4,3
 12290			      2013	 11,3
 17005			      2013	  8,3
 17030			      2013	 15,3
 17870			      2013	  9,9
 17980			      2013	  9,8
 18020			      2013	  8,8
 18210			      2013	 11,2
 18269			      2013	 14,3
 18270			      2013	   10
 18320			      2013	 11,5
 18420			      2013	 15,2
 18701			      2013	 17,4
 18815			      2013	 16,6
 18920			      2013	 15,8
 18980			      2013	 11,9
 27270			      2013	   11
 39150			      2013	 16,3
 39160			      2013	 12,7
 39165			      2013	 17,6
 39200			      2013	    9
 39210			      2013	 11,1
 41090			      2013	 18,2
 44190			      2013	 13,5
 44640			      2013	 13,7
 44730			      2013	  8,6
 50480			      2013	 12,7
 60945			      2013	  4,9
 61340			      2013	 46,3
 64300			      2013	  5,6
 68050			      2013	 30,2
 68120			      2013	 30,2
 68125			      2013	 30,2
 68230			      2013	  8,9
 69020			      2013	 28,3
 82310			      2013	  6,5
 90451			      2013	  9,5
 90495			      2013	   12
 90510			      2013	 13,7
 90560			      2013	  8,7

 11450			      2014	 11,4
 12290			      2014	 24,4
 17980			      2014	 18,7
 18020			      2014	 12,1
 18162			      2014	  5,2
 18165			      2014	  5,6
 18170			      2014	  5,9
 18180			      2014	  6,8
 18195			      2014	  6,4
 18205			      2014	  5,7
 18210			      2014	   10
 18215			      2014	  6,2
 18225			      2014	  6,5
 18233			      2014	    7
 18235			      2014	  8,2
 18245			      2014	  6,6
 18260			      2014	    8
 18269			      2014	  8,6
 18310			      2014	  6,6
 18390			      2014	  5,1
 18420			      2014	  9,6
 18440			      2014	  5,9
 18645			      2014	  7,1
 18701			      2014	   40
 18815			      2014	 19,6
 18920			      2014	 13,2
 18980			      2014	 10,6
 19490			      2014	  3,9
 19510			      2014	  4,6
 27270			      2014	 13,1
 39160			      2014	 18,7
 39165			      2014	 16,6
 39200			      2014	 17,1
 39210			      2014	   10
 41090			      2014	 21,8
 44640			      2014	 15,5
 52860			      2014	 18,8
 61340			      2014	  6,2
 68050			      2014	 30,6
 68120			      2014	 30,8
 68125			      2014	 30,7
 69020			      2014	 10,4
 90451			      2014	  9,8
 90495			      2014	  6,4
 90510			      2014	 10,1
 90560			      2014	  8,8

