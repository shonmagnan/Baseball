
*FIP FORMULA
SELECT playerID, SO AS Strikeouts, BB as Walks, HR as HomeRuns, (3*BB + 3*HBP + 13*HR - 2*SO)/(IPOuts/3) + 3.132 AS FIP
FROM Pitching
WHERE yearID = 2014;