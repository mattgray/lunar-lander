import Graphics.Gloss
import Graphics.Gloss.Interface.Simulate
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Quantities(Velocity)
import qualified Prelude as P

main = simulateInWindow
    "Lunar Lander"
    (P.truncate screenSize, P.truncate screenSize)
    (10, 10)
    black
    30
    worldInit
    drawWorld
    advanceWorld

screenSize = 600
pixelsPerMetre = 10 :: Int

startAltitude = 5500 *~ metre

altitudeToPixels :: (Length Float) -> Float
altitudeToPixels altitude = (altitude /~ metre) P./ 10

gravity = negate $ 9.81 *~ (metre / (second * second))

data Lander = Lander {altitude :: (Length Float), velocity :: (Velocity Float)}

data World = World {lander :: Lander}

worldInit :: World
worldInit = World $ Lander startAltitude (0.0 *~ (metre/second))

drawWorld :: World -> Picture
drawWorld (World lander) = Color white $ Pictures [drawLander lander, drawInstruments lander]

drawLander :: Lander -> Picture
drawLander lander = Translate 0 (altitudeToPixels $ altitude lander) $ fromGround $ Circle 5

drawInstruments :: Lander -> Picture
drawInstruments lander = atTopLeftCorner $ smallText landerText
    where landerText = (show $ altitude lander) ++ " " ++ (show $ velocity lander)

smallText :: String -> Picture
smallText = Scale 0.1 0.1 . Text

advanceWorld :: ViewPort -> Float -> World -> World
advanceWorld _ t (World lander) = World {lander = applyGravity lander t'}
    where t' = t *~ second

fromGround :: Picture -> Picture
fromGround = Translate 0 (P.negate $ screenSize P./ 2)

atTopLeftCorner :: Picture -> Picture
atTopLeftCorner = Translate (-295) (250)

applyGravity :: Lander -> (Time Float) -> Lander
applyGravity (Lander h v) t = Lander h' v' where
	h' = h + v * t + ((_1/_2) * gravity * t * t)
	v' = v + gravity * t
