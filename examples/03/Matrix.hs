module Matrix(
    modelMatrix
  , cameraMatrix
  , projMatrix
  ) where

import Linear
import qualified LambdaCube.Linear as LC

-- | Convert from linear matrix format to LambdaCube format
convLC :: M44 Float -> LC.M44F
convLC (V4 !a !b !c !d) =  LC.V4 (cv a) (cv b) (cv c) (cv d)
  where
    cv (V4 !x !y !z !w) = LC.V4 x y z w

-- | Model matrix, maps from local model coords to world coords
modelMatrix :: Float -> LC.M44F
modelMatrix t = convLC . quatMatrix $ axisAngle (normalize $ V3 1 1 3) t

-- | Camera matrix, maps from world coords to camera coords
cameraMatrix :: Float -> LC.M44F
cameraMatrix _ = convLC $ lookAt eye (V3 0 0 0) (V3 0 1 0)
  where eye = V3 5 2 5 -- rotate (axisAngle (V3 0 1 0) t) (V3 5 2 5)

-- | Projection matrix, maps from camera coords to device normalized coords
projMatrix :: Float -> LC.M44F
projMatrix !aspect = convLC $ perspective (pi/3) aspect 0.1 100

-- | Transform quaternion to rotation matrix
quatMatrix :: Quaternion Float -> M44 Float
quatMatrix q@(Quaternion !w (V3 !x !y !z)) = V4
  (V4 m00 m01 m02 0)
  (V4 m10 m11 m12 0)
  (V4 m20 m21 m22 0)
  (V4 0 0 0 1)
  where
    s = 2 / norm q
    x2 = x * s
    y2 = y * s
    z2 = z * s
    xx = x * x2
    xy = x * y2
    xz = x * z2
    yy = y * y2
    yz = y * z2
    zz = z * z2
    wx = w * x2
    wy = w * y2
    wz = w * z2

    m00 = 1 - (yy + zz)
    m10 = xy - wz
    m20 = xz + wy

    m01 = xy + wz
    m11 = 1 - (xx + zz)
    m21 = yz - wx

    m02 = xz - wy
    m12 = yz + wx
    m22 = 1 - (xx + yy)
