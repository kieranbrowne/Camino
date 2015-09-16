import Camino

main = do 
    --output $ rect 70 80 90 100
    --output $ joinAtEnds [take 2 $ rect 20 20 200 200 , rect 20 20 200 200]
    --output $ addPaths (rect 0 0 10 10) (rect 50 50 50 50)
    --output $ close $ circle 100 103 40
    --output $ joinAtEnds [addPaths (circle 0 (-30) 10) (addPaths (circle 100 103 40) (intersperse (90,90) (30,30) 89)), take 2 $ rect 20 20 20 20, take 45 $ circle 30 20 30]
    output $ rotatePath (rect 0 0 100 100) (-60)
    output $ flowPaths $ replicate 4 (take 80 $ (addPaths (circle 12 12 4) $ intersperse (0,0) (10,0) 89))  
