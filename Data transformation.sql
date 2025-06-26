--data cleaning 

--checking the total number of rows
SELECT COUNT(*)
FROM "tripdata"


--Checking if duplicate values
SELECT
  COUNT(ride_id) - DISTINCT COUNT( ride_id) AS duplicate_rows
FROM "tripdata";


--identifying any null values present in dataset
SELECT 
 COUNT(*) - COUNT(ride_id) as ride_id,
 COUNT(*) - COUNT(rideable_type) as rideable_type,
 COUNT(*) - COUNT(started_at) as started_at,
 COUNT(*) - COUNT(ended_at) as ended_at,
 COUNT(*) - COUNT(start_station_name) as start_station_name,
 COUNT(*) - COUNT(start_station_id) as start_station_id,
 COUNT(*) - COUNT(end_station_name) as end_station_name,
 COUNT(*) - COUNT(end_station_id) as end_station_id,
 COUNT(*) - COUNT(start_lat) as start_lat,
 COUNT(*) - COUNT(start_lng) as start_lng,
 COUNT(*) - COUNT(end_lat) as end_lat,
 COUNT(*) - COUNT(end_lng) as end_lng,
 COUNT(*) - COUNT(member_casual) as  member_casual
FROM "tripdata" ;


-- Deleting all rows with null values
DELETE FROM "tripdata"

WHERE 
start_station_name IS NULL OR
start_station_id IS NULL OR
end_station_id IS NULL OR
end_station_name IS NULL OR
end_lat IS NULL OR
end_lng IS NULL OR
start_lat IS NULL OR
start_lng IS NULL


--Creating new field named 'ride_duration' 
ALTER TABLE "main"."tripdata" ADD COLUMN 	"ride_duration"	INTEGER


-- Subtracting'started_at' and 'ended_at' to find ride duration and adding it into the new column after rounding it 
UPDATE "tripdata" 
SET ride_duration = ROUND((julianday(ended_at) - julianday(started_at)) * 1440


--Deleting rows with values less than 1 to simplify data analysis
DELETE FROM "tripdata"
WHERE ride_duration < 1


--exported table in csv format
