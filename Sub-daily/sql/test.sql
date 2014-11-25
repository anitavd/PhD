spool /home/anitavd/PhD/Sub-daily/data/tipping/max10/99370_hourly_AM10.txt

set NULL 'NA'
set trimspool on
column stnr format 99999 heading STNR
column utm_east format 9999999 heading EAST
column utm_north format 9999999 heading NORTH
column lon format 99.9999 heading LON
column lat format 99.9999 heading LAT
column year format a20 heading YEAR
column month format 99 heading MONTH
column day format 99 heading DAY
column time format 99 heading TIME
column rr_1 format 99.9 heading RR_1
set pagesize 50000 
SET LONG 32000
set lines 180
set heading on


select distinct stnr, utm_east, utm_north, lon, lat, year, month, day, time, rr_1 
from (select * from (select row_number() over (partition by t1.stnr, to_char(dato,'YYYY') order by rr_1 desc ) r, t1.stnr, utm_east, utm_north, lon, lat, to_char(dato,'YYYY') year, to_char(dato,'MM') month, to_char(dato,'DD') day, to_char(dato,'HH24') time, rr_1 
from t_adata t1, v_st_info t2 where t1.stnr=t2.stnr and rr_1 is not null and t1.stnr in (99370) ) where r<=10) 
order by stnr, year;

spool off;


