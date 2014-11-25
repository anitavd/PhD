spool /home/anitavd/PhD/Sub-daily/data/tipping/series/new/68230_hourly_AM.txt

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


select distinct t1.stnr, utm_east, utm_north, lon, lat, to_char(dato,'YYYY') year,to_char(dato,'MM') month,to_char(dato,'DD') day, rr_1 
from t_adata t1, v_st_info t2 where t1.stnr=t2.stnr and rr_1>-1 and rr_1 is not null and t1.stnr in (68230)
order by stnr,year, month, day;

spool off;

