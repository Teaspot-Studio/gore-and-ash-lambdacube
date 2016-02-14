module Matrix(
    mvp
  ) where

import Linear
import qualified LambdaCube.Linear as LC 

-- | Calculating Model-View-Projection matrix, matrix in LambdaCube format
mvp :: Float -> LC.M44F
mvp = convLC . mvp'

convLC :: M44 Float -> LC.M44F 
convLC (V4 !a !b !c !d) =  LC.V4 (cv a) (cv b) (cv c) (cv d)
  where
    cv (V4 !x !y !z !w) = LC.V4 x y z w

-- | Calculating Model-View-Projection matrix
mvp' :: Float -> M44 Float
mvp' !t = rotMatrixY t !*! rotMatrixX (0.5*t)

rotMatrixY :: Float -> M44 Float
rotMatrixY !a = V4 (V4 c 0 (-s) 0) (V4 0 1 0 0) (V4 s 0 c 0) (V4 0 0 0 1)
  where
    c = cos a
    s = sin a

rotMatrixX :: Float -> M44 Float
rotMatrixX !a = V4 (V4 1 0 0 0) (V4 0 c s 0) (V4 0 (-s) c 0) (V4 0 0 0 1)
  where
    c = cos a
    s = sin a