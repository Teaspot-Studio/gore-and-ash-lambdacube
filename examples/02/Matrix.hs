module Matrix(
    mvp
  ) where

import Linear
import qualified LambdaCube.Linear as LC 

-- | Calculating Model-View-Projection matrix, matrix in LambdaCube format
mvp :: Float -> Float -> LC.M44F
mvp aspect t = convLC $ mvp' aspect t

convLC :: M44 Float -> LC.M44F 
convLC (V4 !a !b !c !d) =  LC.V4 (cv a) (cv b) (cv c) (cv d)
  where
    cv (V4 !x !y !z !w) = LC.V4 x y z w

-- | Calculating Model-View-Projection matrix
mvp' :: Float -> Float -> M44 Float
mvp' aspect t = modelMatrix t !*! cameraMatrix !*! projMatrix aspect

modelMatrix :: Float -> M44 Float 
modelMatrix t = quatMatrix $ axisAngle (V3 1 0 1) t 

quatMatrix :: Quaternion Float -> M44 Float 
quatMatrix q@(Quaternion w (V3 x y z)) = V4
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

cameraMatrix :: M44 Float 
cameraMatrix = lookAt (V3 0 0 (-1)) (V3 0 0 0) (V3 0 1 0)

projMatrix :: Float -> M44 Float 
projMatrix aspect = ortho (-2.5) 2.5 (-2.5) 2.5 (-0.01) (-5)
  --perspective (pi/6) aspect 0.1 5
  --infinitePerspective (pi/3) aspect 0.01
 