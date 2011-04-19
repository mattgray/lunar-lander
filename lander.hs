import Graphics.Gloss
import Graphics.Gloss.Interface.Simulate(simulateInWindow, ViewPort)
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

startAltitude = 3500 *~ metre

altitudeToPixels :: (Length Float) -> Float
altitudeToPixels altitude = (altitude /~ metre) P./ 10

gravity = negate $ 9.81 *~ (metre / (second * second))

data Lander = Lander {altitude :: (Length Float), velocity :: (Velocity Float)}

data Surface = Surface Path

data World = World {
    lander :: Lander,
    surface :: Surface}

worldInit :: World
worldInit = World {lander =  Lander startAltitude (0.0 *~ (metre/second)), surface = Surface [(-300, 10), (300, 10)]}

drawWorld :: World -> Picture
drawWorld (World lander surface) = Color white $ Pictures [drawLander lander, drawInstruments lander, drawSurface surface]

drawLander :: Lander -> Picture
drawLander lander = Translate 0 (altitudeToPixels $ altitude lander) $ fromGround $ Circle 5

drawInstruments :: Lander -> Picture
drawInstruments lander = atTopLeftCorner $ smallText landerText
    where landerText = (show $ altitude lander) ++ " " ++ (show $ velocity lander)

drawSurface :: Surface -> Picture
drawSurface (Surface points) = fromGround $ Line points

smallText :: String -> Picture
smallText = Scale 0.1 0.1 . Text

advanceWorld :: ViewPort -> Float -> World -> World
advanceWorld _ t (World lander surface) = World {lander = applyGravity lander t', surface = surface}
    where t' = t *~ second

fromGround :: Picture -> Picture
fromGround = Translate 0 (P.negate $ screenSize P./ 2)

atTopLeftCorner :: Picture -> Picture
atTopLeftCorner = Translate (-295) (250)

applyGravity :: Lander -> (Time Float) -> Lander
applyGravity (Lander h v) t = Lander h' v' where
	h' = h + v * t + ((_1/_2) * gravity * t * t)
	v' = v + gravity * t
