import HighNineP

cfg = Config $ boringDir "/" [("boring", boringFile "boring")]

main = run9PServer cfg
