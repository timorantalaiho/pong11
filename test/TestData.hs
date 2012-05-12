-- Test data
start_v2 = Coordinates 1 1
start_p = Coordinates 15.0 15.0
board = Board 123456 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 100 100)) (Conf 640 480  50 10 5 15)
board' = Board 123471 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 2 2)) (Conf 640 480  50 10 5 15)
losingHistory = [board',board]

winningBoard = Board 123456 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 500 400)) (Conf 640 480  50 10 5 15)
winningBoard' = Board 123471 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 639 479)) (Conf 640 480  50 10 5 15)
winningHistory = [winningBoard',winningBoard]
