import Graphics.Gloss
import Graphics.Gloss.Interface.Simulate

main = simulateInWindow
    "Lunar Lander"
    (truncate screenSize, truncate screenSize)
    (10, 10)
    black
    30
    worldInit
    drawWorld
    advanceWorld

screenSize = 600

startAltitude = Altitude 5500

pixelsPerMetre = 10

altitudeToPixels :: Altitude -> Float
altitudeToPixels (Altitude metres) = metres / pixelsPerMetre 

--hmm we could get some nice types in here
gravity = -9.81

--keeping it 1 d so far
data Altitude = Altitude Float deriving (Show)
type Velocity = Float
data Lander = Lander Altitude Velocity 

worldInit :: Lander
worldInit = Lander startAltitude 0

drawWorld :: Lander -> Picture
drawWorld (Lander h v) = Color white $ Pictures [lander, instruments]
	where 	lander = Translate 0 (altitudeToPixels h) $ fromGround $ Circle 5 
		instruments = atTopLeftCorner $ Text ("alt:" ++ show h ++ " vel:" ++ show v)

advanceWorld :: ViewPort -> Float -> Lander -> Lander
advanceWorld _ t lander = checkNotHitGround (applyGravity lander t) t

fromGround :: Picture -> Picture
fromGround p = Translate 0 (negate $ screenSize / 2) p

atTopLeftCorner :: Picture -> Picture
atTopLeftCorner p = Translate (-295) (250) (Scale 0.1 0.1 p)

--here comes the science bit

applyGravity :: Lander -> Float -> Lander
applyGravity (Lander (Altitude h) v) t = Lander h' v' where 
	h' = Altitude (h + (v * t) + (0.5 * gravity * t * t))
	v' = v + (gravity * t)

-- sort this out
checkNotHitGround :: Lander -> Float -> Lander
checkNotHitGround (Lander (Altitude h) v) _ = Lander h' v where h' = Altitude (max 10 h)

