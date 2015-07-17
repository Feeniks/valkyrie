
module Valkyrie.Math where 

import qualified Graphics.Rendering.OpenGL.Raw as GL

type NReal = Double

class Vector v where 
    dot :: v -> v -> NReal
    len :: v -> NReal
    normalise :: v -> v

data Angle = Degrees NReal | Radians NReal

data Vector2 = V2 NReal NReal deriving Show

dot2 :: Vector2 -> Vector2 -> NReal
dot2 (V2 x1 y1) (V2 x2 y2) = (x1 * x2) + (y1 * y2)

instance Vector Vector2 where 
    dot = dot2
    len v = sqrt $ dot2 v v
    normalise (V2 x y) 
        | l == 0.0 = V2 1.0 1.0
        | otherwise = V2 (x / l) (y / l)
        where l = sqrt $ dot2 (V2 x y) (V2 x y)

instance Num Vector2 where 
    (+) (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)
    (*) (V2 x1 y1) (V2 x2 y2) = V2 (x1 * x2) (y1 * y2)
    (-) (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)
    negate (V2 x y) = V2 (-x) (-y)
    abs (V2 x y) = V2 (abs x) (abs y)
    signum = normalise
    fromInteger x = V2 v v where v = fromIntegral x

data Vector3 = V3 NReal NReal NReal deriving Show

dot3 :: Vector3 -> Vector3 -> NReal
dot3 (V3 x1 y1 z1) (V3 x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

instance Vector Vector3 where 
    dot = dot3
    len v = sqrt $ dot3 v v
    normalise (V3 x y z) 
        | l == 0.0 = V3 1.0 1.0 1.0
        | otherwise = V3 (x / l) (y / l) (z / l)
        where l = sqrt $ dot3 (V3 x y z) (V3 x y z)

instance Num Vector3 where 
    (+) (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 + x2) (y1 + y2) (z1 + z2)
    (*) (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 * x2) (y1 * y2) (z1 * z2)
    (-) (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 - x2) (y1 - y2) (z1 - z2)
    negate (V3 x y z) = V3 (-x) (-y) (-z)
    abs (V3 x y z) = V3 (abs x) (abs y) (abs z)
    signum = normalise
    fromInteger x = V3 v v v where v = fromIntegral x

data Vector4 = V4 NReal NReal NReal NReal deriving Show

dot4 :: Vector4 -> Vector4 -> NReal
dot4 (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) = (x1 * x2) + (y1 * y2) + (z1 * z2) + (w1 * w2)

instance Vector Vector4 where 
    dot = dot4
    len v = sqrt $ dot4 v v
    normalise (V4 x y z w) 
        | l == 0.0 = V4 1.0 1.0 1.0 1.0
        | otherwise = V4 (x / l) (y / l) (z / l) (w / l)
        where l = sqrt $ dot4 (V4 x y z w) (V4 x y z w)

instance Num Vector4 where 
    (+) (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) = V4 (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
    (*) (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) = V4 (x1 * x2) (y1 * y2) (z1 * z2) (w1 * w2)
    (-) (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) = V4 (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
    negate (V4 x y z w) = V4 (-x) (-y) (-z) (-w)
    abs (V4 x y z w) = V4 (abs x) (abs y) (abs z) (abs w)
    signum = normalise
    fromInteger x = V4 v v v v where v = fromIntegral x

data Matrix44 = M44 { 
    m11 :: NReal, m21 :: NReal, m31 :: NReal, m41 :: NReal, 
    m12 :: NReal, m22 :: NReal, m32 :: NReal, m42 :: NReal, 
    m13 :: NReal, m23 :: NReal, m33 :: NReal, m43 :: NReal, 
    m14 :: NReal, m24 :: NReal, m34 :: NReal, m44 :: NReal 
} deriving Show

data Plane = Plane Vector3

toRadians :: Angle -> NReal
toRadians (Degrees x) = x * (pi / 180.0)
toRadians (Radians x) = x

cross :: Vector3 -> Vector3 -> Vector3
cross (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 cx cy cz 
    where 
    cx = y1 * z2 - z1 * y2
    cy = z1 * x2 - x1 * z2
    cz = x1 * y2 - y1 * x2

zero44 :: Matrix44
zero44 = M44 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

identity44 :: Matrix44
identity44 = M44 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1

toRows :: Matrix44 -> [Vector4]
toRows m = [vx, vy, vz, vw] 
    where 
    vx = V4 (m11 m) (m12 m) (m13 m) (m14 m)
    vy = V4 (m21 m) (m22 m) (m23 m) (m24 m)
    vz = V4 (m31 m) (m32 m) (m33 m) (m34 m)
    vw = V4 (m41 m) (m42 m) (m43 m) (m44 m)

fromRows :: [Vector4] -> Matrix44 
fromRows vx 
    | length vx /= 4 = error "Inconsistent dimensions"
    | otherwise = M44 {
        m11 = x1, m21 = y1, m31 = z1, m41 = w1, 
        m12 = x2, m22 = y2, m32 = z2, m42 = w2, 
        m13 = x3, m23 = y3, m33 = z3, m43 = w3, 
        m14 = x4, m24 = y4, m34 = z4, m44 = w4 
    } 
    where 
    (V4 x1 x2 x3 x4) = vx !! 0
    (V4 y1 y2 y3 y4) = vx !! 1
    (V4 z1 z2 z3 z4) = vx !! 2
    (V4 w1 w2 w3 w4) = vx !! 3

transpose :: Matrix44 -> Matrix44
transpose m = m { m12 = m21 m, m13 = m31 m, m14 = m41 m, m21 = m12 m, m23 = m32 m, m24 = m42 m, m31 = m13 m, m32 = m23 m, m34 = m43 m, m41 = m14 m, m42 = m24 m, m43 = m34 m }

(<:>) :: Matrix44 -> Vector4 -> Vector4
(<:>) m (V4 x y z w) = V4 x' y' z' w'
    where 
    x' = m11 m * x + m21 m * y + m31 m * z + m41 m * w
    y' = m12 m * x + m22 m * y + m32 m * z + m42 m * w
    z' = m13 m * x + m23 m * y + m33 m * z + m43 m * w
    w' = m14 m * x + m24 m * y + m34 m * z + m44 m * w

(<::>) :: Matrix44 -> Matrix44 -> Matrix44
(<::>) m1 m2 = fromRows $ fmap ((<:>) m1) (toRows m2)

toGL :: Matrix44 -> [GL.GLfloat]
toGL m = fmap realToFrac $ 
    [
        m11 m, m21 m, m31 m, m41 m, 
        m12 m, m22 m, m32 m, m42 m, 
        m13 m, m23 m, m33 m, m43 m,
        m14 m, m24 m, m34 m, m44 m
    ]

rotationX :: Angle -> Matrix44
rotationX a = zero44 { m11 = 1, m22 = cos r, m23 = -(sin r), m32 = sin r, m33 = cos r, m44 = 1 } where r = toRadians a

rotationY :: Angle -> Matrix44
rotationY a = zero44 { m11 = cos r, m13 = -(sin r), m22 = 1, m31 = sin r, m33 = cos r, m44 = 1 } where r = toRadians a

rotationZ :: Angle -> Matrix44
rotationZ a = zero44 { m11 = cos r, m12 = sin r, m21 = -(sin r), m22 = cos r, m33 = 1, m44 = 1 } where r = toRadians a

scale :: NReal -> NReal -> NReal -> Matrix44
scale x y z = identity44 { m11 = x, m22 = y, m33 = z }

translate :: NReal -> NReal -> NReal -> Matrix44
translate x y z = identity44 { m14 = x, m24 = y, m34 = z }

perspective :: Angle -> NReal -> NReal -> NReal -> Matrix44
perspective fov aspect near far = zero44 { m11 = f / aspect, m22 = f, m33 = (far + near) / (near - far), m43 = (2 * far * near) / (near - far), m34 = -1 } where f = 1.0 / (tan $ (toRadians fov) * 0.5)

lookAt :: Vector3 -> Vector3 -> Vector3 -> Matrix44
lookAt eye target up = identity44 { 
            m11 = x1, m12 = x2, m13 = x3, 
            m21 = y1, m22 = y2, m23 = y3, 
            m31 = z1, m32 = z2, m33 = z3, 
            m14 = -(dot (V3 x1 x2 x3) eye), m24 = -(dot (V3 y1 y2 y3) eye), m34 = -(dot (V3 z1 z2 z3) eye) 
        }
    where 
    (V3 ex ey ez) = eye
    (V3 z1 z2 z3) = normalise $ eye - target
    (V3 x1 x2 x3) = normalise $ cross up (V3 z1 z2 z3) 
    (V3 y1 y2 y3) = normalise $ cross (V3 z1 z2 z3) (V3 x1 x2 x3)