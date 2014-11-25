spool /home/anitavd/PhD/Sub-daily/data/tipping/11620_RR24_AM.txt

set NULL 'NA'
set trimspool on
column stnr format 99999 heading STNR
column utm_east format 9999999 heading EAST
column utm_north format 9999999 heading NORTH
column lon format 99.9999 heading LON
column lat format 99.9999 heading LAT
column year format a20 heading YEAR
column rr_24 format 99.9 heading RR_24
set pagesize 50000 
SET LONG 32000
set lines 180
set heading on


select distinct stnr, utm_east, utm_north, lon, lat, year, rr_24 
from (select * from (select rank() over (partition by t1.stnr, to_char(dato,'YYYY') order by rr_24 desc ) r, t1.stnr, utm_east, utm_north, lon, lat, to_char(dato,'YYYY') year, rr_24 
from t_adata t1, v_st_info t2 where t1.stnr=t2.stnr and rr_24 is not null and t1.stnr in (11620) ) where r = 1) 
order by stnr, year;

spool off;

