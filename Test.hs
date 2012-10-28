import Network.NineP

--cfg = Config $ boringDir "/" [("boring", boringFile "boring"),("nyak", boringFile "nyak")]
cfg = Config $ boringDir "/" [("lol", boringFile "lol")]

main = run9PServer cfg
