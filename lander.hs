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

type Altitude = Length Float

type LanderVelocity = Velocity Float 

data Lander =   Falling Altitude LanderVelocity
                | Crashed Altitude

altitude :: Lander -> Altitude
altitude (Falling a _) = a
altitude (Crashed a) = a

velocity :: Lander -> LanderVelocity
velocity (Falling _ v) = v
velocity (Crashed _) = 10 *~ (metre/second)

data Surface = Surface Path

data World = World {
    lander :: Lander,
    surface :: Surface}

worldInit :: World
worldInit = World {lander = Falling startAltitude (0.0 *~ (metre/second)), surface = Surface [(-300, 10), (300, 10)]}

itemsToDraw :: [World -> Picture]
itemsToDraw = [
    drawLander . lander,
    drawInstruments . lander,
    drawSurface . surface
    ]

drawWorld :: World -> Picture
drawWorld world = Color white $ Pictures $ map ($ world) itemsToDraw

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
advanceWorld _ t (World lander surface) = World {lander = advanceLander lander t', surface = surface}
    where t' = t *~ second


fromGround :: Picture -> Picture
fromGround = Translate 0 (P.negate $ screenSize P./ 2)

atTopLeftCorner :: Picture -> Picture
atTopLeftCorner = Translate (-295) (250)

advanceLander :: Lander -> (Time Float) -> Lander
advanceLander lander@(Falling h v) t = hasCrashed $ applyGravity lander t
advanceLander crashed@(Crashed _) _ = crashed

applyGravity :: Lander -> (Time Float) -> Lander
applyGravity (Falling h v) t = Falling h' v' where
	h' = h + v * t + ((_1/_2) * gravity * t * t)
	v' = v + gravity * t


hasCrashed :: Lander -> Lander
hasCrashed crashed@(Crashed a) = crashed
hasCrashed lander@(Falling h v) = let crashed = altitude lander < (10 *~ metre) in 
                                    if crashed then Crashed (altitude lander) else lander
