data = """Layer42 & 135 & 0.00 & 0.04 & 0.00 & 0.04 & 0.01 & 0.07 & 13\\
Compuserv & 539 & 0.01 & 0.36 & 0.01 & 0.38 & 0.01 & 0.85 & 62\\
Airtel & 785 & 0.01 & 0.83 & 0.01 & 0.84 & 0.02 & 2.08 & 92\\
Belnet & 1388 & 0.01 & 3.17 & 0.01 & 3.16 & 0.04 & 7.99 & 224\\
Shentel & 1865 & 0.02 & 4.01 & 0.02 & 4.00 & 0.04 & 9.80 & 259\\
Arpa & 1964 & 0.01 & 4.32 & 0.02 & 4.32 & 0.05 & 10.99 & 317\\
Sanet & 4100 & 0.04 & 23.46 & 0.03 & 25.23 & 0.12 & 62.70 & 762\\
Uunet & 5456 & 0.04 & 81.54 & 0.04 & 81.92 & 0.15 & 204.85 & 1982\\
Missouri & 9680 & .011 & 161.28 & 0.10 & 165.85 & 0.27 & 519.46 & 1934 \\
Telcove & 10720 & 0.09 & 464.15 & 0.08 & 465.27 & 0.28 & 1274.24 & 5953 \\
Deltacom & 27092 & 0.31 & 2392.56 & 0.30 & 2523.03 & 0.75 & 7069.54 & 9384\\
Cogentco & 79682 & 0.97 & 22581.39 & 0.88 & 23300.87 & - & - & 26334\\
FatTree-4 & 2777 & 0.02 & 2.27 & 0.03 & 5.53 & 0.02 & 10.51 & 486\\
FatTree-6 & 22856 & 0.25 & 1033.57 & - & - & 0.35 & 793.83 & 2248\\
FatTree-8 & 58483 & 0.67 & 14984.44 & 0.77 & 16452.51 & - & - & 22505\\"""

def tofloat(x):
  try:
    return float(x)
  except:
    return 1.0

for row in data.split("\n"):
  # compute min of 4th/3rd, 6th/5th, 8th/7th
  vals = row.split(" & ")
  # print(vals)
  if len(vals) < 9:
    continue
  vals = vals[2:][:-1]
  # print(len(vals))
  # print(vals)
  vals = [tofloat(x) for x in vals]
  m = 23823489234
  for i in range(3):
    a = vals[2*i]
    b = vals[2*i+1]
    speedup = b/a if a > 0 else 34234234234
    if speedup != 1.0: m = min(m, speedup)
  print(round(m))



