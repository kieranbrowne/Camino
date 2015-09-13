import Camino

main = do 
    --output $ rect 70 80 90 100
    output $ rect 20 20 200 200 
    --output $ addPaths (rect 0 0 10 10) (rect 50 50 50 50)
    --output $ close $ circle 100 103 40
    output $ addPaths (circle 100 103 40) (rect 90 90 30 30)
