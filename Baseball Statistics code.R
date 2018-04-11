#fielding idependent pitching
fip <- (13*HR + 3*BB - 2*SO) / IP
fip2 <- (3*BB + 3*HBP + 13*HR - 2*SO)/(IPOuts/3) + 3.132

#three true outcomes
TTOPercentage <- (BB + SO + HR)/(AB + BB + SF + SH + HBP)

#1998 WOBA
WOBA <- (0.713*(BB - IBB) + 0.742*HBP + 0.898*(H - 2B - 3B - HR) + 1.257*2B + 1.580*3B + 2.007*HR) / (AB + BB - IBB + SF + HBP)

