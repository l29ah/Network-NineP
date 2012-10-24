import HighNineP

cfg = Config $ boringDir "/" [("boring", boringFile "boring"),("nyak", boringFile "nyak")]

main = run9PServer cfg
