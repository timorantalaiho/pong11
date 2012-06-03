module Vector where

import Coordinate

instance Eq Coordinates where
  (==) (Coordinates x1 y1) (Coordinates x2 y2) = (x1 == x2) && (y1 == y2)

instance Num Coordinates where
  (+) (Coordinates x1 y1) (Coordinates x2 y2) = Coordinates (x1 + x2) (y1 + y2)
  (-) (Coordinates x1 y1) (Coordinates x2 y2) = Coordinates (x1 - x2) (y1 - y2)
  (*) (Coordinates x1 y1) (Coordinates x2 y2) = error "Multiplication between two coordinates undefined"
  abs (Coordinates x y) = (Coordinates (abs x) (abs y))
  signum (Coordinates x1 y1) = error "signum of coordinates undefined"
  fromInteger i = (Coordinates (fromInteger i) (fromInteger i))

vscale :: Coordinates -> Float -> Coordinates
vscale (Coordinates vx vy) s = Coordinates (vx * s) (vy * s)

vectorTo :: Coordinates -> Coordinates -> Coordinates
vectorTo (Coordinates x1 y1) (Coordinates x2 y2) =
    Coordinates (x1 - x2) (y1 - y2)

vectorFrom :: Coordinates -> Coordinates -> Coordinates
vectorFrom (Coordinates x1 y1) (Coordinates x2 y2) =
    Coordinates (x1 + x2) (y1 + y2)

normalizedVector :: Coordinates -> Coordinates
normalizedVector vector = vscale vector (1 / length)
  where length = vectorLength vector

dotProduct :: Coordinates -> Coordinates -> Float
dotProduct (Coordinates x1 y1) (Coordinates x2 y2) = x1 * x2 + y1 * y2

perpendicular :: Coordinates -> Coordinates
perpendicular (Coordinates vx vy) = Coordinates (negate vy) vx

vectorAngle (Coordinates vx vy) = atan (vy / vx)

rotateVector (Coordinates vx vy) angle = Coordinates (vx * cos angle - vy * sin angle) (vx * sin angle + vy * cos angle)

vectorLength (Coordinates vx vy) = sqrt (vx * vx + vy * vy)

setVectorLength vector length = vscale (normalizedVector vector) length
