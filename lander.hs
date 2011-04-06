import Graphics.Gloss
import Graphics.Gloss.Interface.Simulate
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Quantities(Velocity)
import qualified Prelude

main = simulateInWindow
    "Lunar Lander"
    (Prelude.truncate screenSize, Prelude.truncate screenSize)
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
altitudeToPixels altitude = (altitude /~ metre) Prelude./ 10

gravity = negate $ 9.81 *~ (metre / (second * second))

type Altitude = Length
data Lander = Lander (Altitude Float) (Velocity Float)

worldInit :: Lander
worldInit = Lander startAltitude (0.0 *~ (metre/second))

drawWorld :: Lander -> Picture
drawWorld (Lander h v) = Color white $ Pictures [(Translate 0 (altitudeToPixels h) $ fromGround $ Circle 5), (atTopLeftCorner $ Text (show h ++ " " ++ show v))]

advanceWorld :: ViewPort -> Float -> Lander -> Lander
advanceWorld _ t lander = applyGravity lander t'
    where t' = t *~ second

fromGround :: Picture -> Picture
fromGround p = Translate 0 (Prelude.negate $ screenSize Prelude./ 2) p

atTopLeftCorner :: Picture -> Picture
atTopLeftCorner p = Translate (-295) (250) (Scale 0.1 0.1 p)

applyGravity :: Lander -> (Time Float) -> Lander
applyGravity (Lander h v) t = Lander h' v' where
	h' = h + (v * t) + ((_1/_2) * gravity * t * t)
	v' = v + (gravity * t)
