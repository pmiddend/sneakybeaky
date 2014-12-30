module SneakyBeaky.Matrix where

type Matrix2D a = (a, a, a, a)

mMultiply :: RealFrac a => Matrix2D a -> (Int,  Int) -> (Int, Int)
mMultiply (a00,a10,a01,a11) (x, y) = let xf = fromIntegral x
                                         yf = fromIntegral y
                                     in (floor (a00 * xf + a01 * yf), floor (a10 * xf + a11 * yf))

mRotationMatrix :: Floating a => a -> Matrix2D a
mRotationMatrix a = (cos a, -(sin a), sin a, cos a)
