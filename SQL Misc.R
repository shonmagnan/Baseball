#Innings Pitched as IPOuts, equal to IP*3
#*FIP FORMULA
#SELECT playerID, SO AS Strikeouts, BB as Walks, HR as HomeRuns, (3*BB + 3*HBP + 13*HR - 2*SO)/(IPOuts/3) + 3.132 AS FIP
##FROM Pitching

#Fielding
# a + PO = apluspo
#a + PO + E = total chances
#(a = po) /(a + PO + e) = fpct

#three true outcome percentage
SELECT playerID, (BB + SO + HR)/(AB + BB + SF + SH + HBP) AS TTOPercentage
FROM Batting
WHERE yearID = 2000 AND AB >= 500
ORDER BY TTOPercentage desc

#woba for 1998
SELECT playerID, 
yearID, 
teamID, 
(0.713*(BB-IBB) + 0.742*HBP + 0.898*(H-2B-3B-HR) + 1.257*2B + 1.580*3B + 2.007*HR) / (AB + BB - IBB + SF + HBP) AS wOBA
FROM Batting
WHERE yearID = 1998 AND AB > 300
ORDER BY wOBA DESC

#get the unique categories for a field
select distinct POS from fielding
SELECT DISTINCT teamID FROM Teams

#aggregate functions in SQL
select
teamID,
avg(a) as avg_assists,
count(a) as count_assists,
count(distinct a) as count_distinct_assists,
sum(a) as sum_assists,
min(a) as min_assists,
max(a) as max_assists,
stddev(a) as std_assists,
variance(a) as var_assists
from fielding
where pos = '2B'
group by teamID, yearID
order by sum_assists desc

#having works with aggregate functions
select
teamID,
avg(a) as avg_assists,
count(a) as count_assists,
count(distinct a) as count_distinct_assists,
sum(a) as sum_assists,
min(a) as min_assists,
max(a) as max_assists,
stddev(a) as std_assists,
variance(a) as var_assists
from fielding
where yearID > 1960
group by teamID, yearID
having avg(a) > 27
order by sum_assists desc

SELECT playerID, 
sum(SO) as total_SOs
from Batting
group by playerID
order by total_SOs desc, playerID
limit 100;