{-# LANGUAGE RankNTypes #-}

import Linear.Matrix
import Linear.Vector
import Linear.V3
import Linear.V4
import Data.Maybe
import Data.List (sortOn)
import Codec.Picture
import Debug.Trace
import Text.Printf (printf)

type PointInt = (Int, Int)
type PointFloat = (Float, Float)

type Position     = V3 Float
type Direction    = V3 Float
type Color        = V3 Float

type Matrix44 = M44 Float

data Axis = X | Y | Z
data Ray = Ray {origin :: Position, direction :: Direction} deriving (Show)
data Hit = Hit { hitPoint :: Position
               , hitNorm :: Direction
               , hitObj :: ShadedObject
               } deriving (Eq, Show)

data Object = Sphere {center :: Position, radius :: Float}
              | Plane {planeP :: Position, planeNorm :: Direction, doubleSide :: Bool}
              | Triangle {p0 :: Position, p1 :: Position, p2 :: Position, doubleSide :: Bool}
              | CompositeTriangle {vertex :: [Position], vertexIndex :: [Int]}
              | CheckerBoard { boardP :: Position
                             , boardNorm :: Direction
                             , doubleSide :: Bool
                             , material1 :: Material
                             , material2 :: Material
                             , cellSize :: Float}
              deriving (Eq, Show)

data Material = Material { color        :: Color
                         , smoothness   :: Float
                         , reflectivity :: Float
                         , ior          :: Float
                         , transmission :: Float
                         } deriving (Eq, Show)

data Transformation = Rotate { axis :: Axis, degree :: Float }
                    | Translate { movement :: (V3 Float) }
                    | Scale { scaleFactor :: (V3 Float) }

data Light = Light { lightPos :: Position, brightness :: Float, lightColor :: Color}

data Camera = Camera { cameraPos :: Position
                     , focalLength :: Float
                     , cameraAperture :: PointFloat
                     , imageSize :: PointInt
                     , aaFactor :: Int
                     }


data Scene = Scene { camera :: Transformed Camera
                   , objectList :: [Transformed ShadedObject]
                   , lightList :: [Light]
                   , defaultColor :: Color
                   , ambientLight :: Light
                   }

data ShadedObject = ShadedObject { shadeObj :: Object, material :: Material } deriving (Eq, Show)
data Transformed a = Transformed { transformObj :: a, matrix :: Matrix44} deriving (Eq)


intersectComposite :: Ray -> Material -> [Position] -> [Int] -> Maybe Hit
intersectComposite ray material points links
    | distanceList == [] = Nothing
    | otherwise = snd $ head (sortOn fst distanceList)
    where hitList = map (intersect ray material) objects
          objects = generateTriangles points links
          distanceList = calcDistance hitList
          calcDistance [] = []
          calcDistance (Nothing:xs) = calcDistance xs
          calcDistance (Just x:xs) = (lengthVec (origin ray - hitPoint x), Just x):calcDistance xs


generateTriangles :: [Position] -> [Int] -> [Object]
generateTriangles xs [] = []
generateTriangles xs (a:b:c:ys) = (Triangle (xs !! a) (xs !! b) (xs !! c) False):generateTriangles xs ys


intersectTriangle :: Ray -> Material -> Object -> Maybe Hit
intersectTriangle (Ray rayorig raydir) material (Triangle a b c double)
    | det > (-eplison) && det < eplison = Nothing
    | u < 0.0 || u > 1.0 = Nothing
    | v < 0.0 || v + u > 1.0 = Nothing
    | t <= 0.0 = Nothing
    | otherwise = Just $ Hit pHit pNorm (ShadedObject (Triangle a b c double) material)
    where eplison = 0.00001
          ab = b - a
          ac = c - a
          h = cross raydir ac
          det = dot ab h
          f = 1.0 / det
          s = rayorig - a
          u = f * (dot s h)
          q = cross s ab
          v = f * (dot raydir q)
          t = f * (dot ac q)
          pHit = rayorig + (raydir ^* t)
          pNorm = normalize $ cross ab ac


intersectSphere :: Ray -> Material -> Object -> Maybe Hit
intersectSphere (Ray rayorig raydir) material (Sphere center radius)
    | tca < 0 = Nothing
    | d2 > radius2 = Nothing
    | t0 < 0 && t1 < 0 = Nothing
    | otherwise = Just $ Hit pHit pNorm (ShadedObject (Sphere center radius) material)
    where l = center - rayorig
          tca = dot l raydir
          d2 = (dot l l) - tca * tca
          radius2 = radius * radius
          thc = sqrt (radius2 - d2)
          t0 = tca - thc
          t1 = tca + thc
          pHit = if t0 < 0 then rayorig + (raydir ^* t1) else rayorig + (raydir ^* t0)
          pNorm = normalize (pHit - center)


intersectPlane :: Ray -> Material -> Object -> Maybe Hit
intersectPlane (Ray rayorig raydir) material (Plane point norm doubleSide)
    | nDotRay > 0 && (not doubleSide) = Nothing
    | abs nDotRay < 0.001 = Nothing
    | t < 0 = Nothing
    | otherwise = Just $ Hit p normal (ShadedObject (Plane point normal doubleSide) material)
    where normal = normalize norm
          nDotRay = dot normal raydir
          t = (dot (point - rayorig) normal) / nDotRay
          p = rayorig ^+^ t *^ raydir


intersectChecker :: Ray -> Object -> Maybe Hit
intersectChecker (Ray rayorig raydir) board@(CheckerBoard point norm doubleSide material1 material2 cellSize)
    | nDotRay > 0 && (not doubleSide) = Nothing
    | abs nDotRay < 0.001 = Nothing
    | t < 0 = Nothing
    | d /= e = Just $ Hit p normal (ShadedObject board material1)
    | otherwise = Just $ Hit p normal (ShadedObject board material2)
    where normal = normalize norm
          nDotRay = dot normal raydir
          t = (dot (point - rayorig) normal) / nDotRay
          p@(V3 a b c) = rayorig ^+^ t *^ raydir
          point2dx = dot (p - point) x
          point2dy = dot (p - point) y
          d = even (floor $ point2dx / cellSize)
          e = even (floor $ point2dy / cellSize)
          (x, y) = arbitaryXY board


arbitaryXY :: Object -> (Position, Position)
arbitaryXY (CheckerBoard point norm _ _ _ _) = (x, y)
    where n = normalize norm
          u = if n == V3 1 0 0 then V3 0 1 0 else V3 1 0 0
          un = dot u n
          x = normalize (u - un *^ n)
          y = normalize (cross n x)


intersect :: Ray -> Material -> Object -> Maybe Hit
intersect ray material a@(Sphere _ _) = intersectSphere ray material a
intersect ray material a@(Plane _ _ _) = intersectPlane ray material a
intersect ray material a@(Triangle _ _ _ _) = intersectTriangle ray material a
intersect ray material (CompositeTriangle points indexes) = intersectComposite ray material points indexes
intersect ray _ a@(CheckerBoard _ _ _ _ _ _) = intersectChecker ray a


generateEyeRay :: Transformed Camera -> PointInt -> Ray
generateEyeRay (Transformed camera matrix) (x, y) = Ray transformedOrigin (normalize transformedEyeray)
    where (x', y') = pointTo3Dpoint camera (x, y)
          eyeray = V3 x' y' (-focalLength camera)
          transformedEyeray = transformDir matrix eyeray
          transformedOrigin = transformPos matrix (cameraPos camera)


generateEyeRays :: Transformed Camera -> PointInt -> [Ray]
generateEyeRays c@(Transformed camera matrix) (x, y) = map (generateEyeRay c) points
    where points = generatePoints (x, y) (aaFactor camera)


generatePoints :: PointInt -> Int -> [PointInt]
generatePoints (x, y) n = [(a, b) | a <- xs, b <- ys]
    where xs = map (+(n*x)) [0..(n-1)]
          ys = map (+(n*y)) [0..(n-1)]


pointTo3Dpoint :: Camera -> PointInt -> PointFloat
pointTo3Dpoint camera (x, y) = (pixelScreenx * filmWidth / 2.0, pixelScreeny * filmHeight / 2.0)
    where (pixelNDCx, pixelNDCy) = calcNDCpoints (x, y) (imageSize camera) (aaFactor camera)
          pixelScreenx = 2 * pixelNDCx - 1
          pixelScreeny = 2 * pixelNDCy - 1
          (filmWidth, filmHeight) = cameraAperture camera


calcNDCpoints :: PointInt -> PointInt -> Int -> PointFloat
calcNDCpoints (x, y) (w, h) a = (pixelNDCx, pixelNDCy)
    where pixelNDCx = (fromIntegral x + 0.5) / (fromIntegral virtualw)
          pixelNDCy = (fromIntegral (virtualh-y) + 0.5) / (fromIntegral virtualh)
          virtualw = a * w
          virtualh = a * h



closestHit :: Scene -> Ray -> Maybe Hit
closestHit scene ray
    | distanceList == [] = Nothing
    | otherwise = snd $ head (sortOn fst distanceList)
    where hitList = map (calcHit ray) $ (objectList scene)
          distanceList = calcDistance hitList
          calcDistance [] = []
          calcDistance (Nothing:xs) = calcDistance xs
          calcDistance (Just x:xs) = (lengthVec (origin ray - hitPoint x), Just x):calcDistance xs


calcHit :: Ray -> Transformed ShadedObject -> Maybe Hit
calcHit ray (Transformed (ShadedObject obj material) o2wmat) =
    fmap (transformHit o2wmat (transpose w2omat)) $ hit
    where transformedRay = transformRay w2omat ray
          w2omat = inv44 o2wmat
          hit = intersect transformedRay material obj


renderPixel :: Scene -> Int -> Int -> PixelRGB8
renderPixel scene x y = colorToPixel (averageColors colors)
    where eyerays = generateEyeRays (camera scene) (x, y)
          colors = map (renderRay scene 0) eyerays


averageColors :: [Color] -> Color
averageColors colors = (V3 (a/l) (b/l) (c/l))
    where (V3 a b c) = sum colors
          l = fromIntegral $ length colors


renderRay :: Scene -> Int -> Ray -> Color
renderRay scene depth ray
    | depth > 5 = V3 0 0 0
    | hit == Nothing = defaultColor scene
    | otherwise = renderRay' scene ray (fromJust hit) depth
    where hit = closestHit scene ray


renderRay' :: Scene -> Ray -> Hit -> Int -> Color
renderRay' scene ray hit depth = clip (diffuse) (0, 1)
    where diffuse = renderDiffuse scene hit
          specular = renderSpecular scene hit ray
          refraction = if (kr < 1 && transmission objMaterial > 0)
                       then (renderRefraction scene hit ray (depth + 1)) ^* (1 - kr)
                       else V3 0 0 0
          reflection = if (transmission objMaterial > 0)
                       then (renderReflection scene hit ray (depth + 1)) ^* kr
                       else V3 0 0 0
          kr = fresnel (direction ray) (hitNorm hit) (ior objMaterial)
          objMaterial = material $ hitObj hit


renderDiffuse :: Scene -> Hit -> Color
renderDiffuse scene hit = color material' * intensity' ^* (1 - transmission material')
    where intensity = (brightness $ ambientLight scene) *^ (lightColor $ ambientLight scene)
          intensities = sum $ map (renderDiffuseOneLight scene hit) (lightList scene)
          intensity' = clip (intensity + intensities) (0, 1)
          material' = material $ hitObj hit


renderDiffuseOneLight :: Scene -> Hit -> Light -> Color
renderDiffuseOneLight scene hit light
    | shadowHit == Nothing = (max 0 angle) * brightness light *^ lightColor light
    | otherwise = V3 0 0 0
    where shadowRay = generateShadowray hit light
          shadowHit = closestHit scene shadowRay
          angle = dot (direction shadowRay) (hitNorm hit)


renderSpecular :: Scene -> Hit -> Ray -> Color
renderSpecular scene hit ray = sum intensities
    where intensities = map (renderSpecularOneLight scene hit ray) (lightList scene)


renderSpecularOneLight :: Scene -> Hit -> Ray -> Light -> Color
renderSpecularOneLight scene hit ray light
    | shadowHit == Nothing = lightColor light ^* (brightness light * (max 0 glossyAngle) ** (smoothness material'))
    | otherwise = V3 0 0 0
    where shadowRay = generateShadowray hit light
          shadowHit = closestHit scene shadowRay
          reflectionRay = Ray (hitPoint hit) (reflection (direction shadowRay) (hitNorm hit))
          glossyAngle = dot (direction ray) (direction reflectionRay)
          material' = material $ hitObj hit


renderReflection :: Scene -> Hit -> Ray -> Int -> Color
renderReflection scene hit ray depth = (renderRay scene (depth+1) reflectionRay) ^* (reflectivity material')
    where reflectionRay = generateReflectionray hit ray
          material' = material $ hitObj hit


renderRefraction :: Scene -> Hit -> Ray -> Int -> Color
renderRefraction scene hit ray depth
    | dir == Nothing = V3 0 0 0
    | otherwise = renderRay scene (depth+1) (bias $ Ray (hitPoint hit) (fromJust dir))
    where dir = refraction (direction ray) (hitNorm hit) (ior $ material $ hitObj hit)
          refractionRay = Ray (hitPoint hit)


generateShadowray :: Hit -> Light -> Ray
generateShadowray hit light = bias $ Ray (hitPoint hit) (normalize (lightPos light - hitPoint hit))


generateReflectionray :: Hit -> Ray -> Ray
generateReflectionray hit ray = bias $ Ray (hitPoint hit) reflectDir
    where reflectDir = reflection (direction ray) (hitNorm hit)


bias :: Ray -> Ray
bias (Ray point dir) = Ray newPoint dir
    where newPoint = point + (0.001 *^ dir)


dot :: Direction -> Direction -> Float
dot (V3 a b c) (V3 d e f) = a * d + b * e + c * f


normalize :: Direction -> Direction
normalize (V3 a b c) = V3 (a/l) (b/l) (c/l)
    where l = lengthVec (V3 a b c)


lengthVec :: Direction -> Float
lengthVec (V3 a b c) = sqrt (a*a + b*b + c*c)


reflection :: Direction -> Direction -> Direction
reflection incident norm = incident - (2 * dot incident norm *^ norm)


refraction :: Direction -> Direction -> Float -> Maybe Direction
refraction incident normal ior
    | k < 0 = Nothing
    | otherwise = Just $ (eta *^ incident) ^+^ ((eta * cosi - (k ** 0.5)) *^ n)
    where cosi' = clipSingle (dot incident normal) (-1, 1)
          (cosi, n, etai, etat) = refraction' cosi' normal 1 ior
          eta = etai / etat
          k = 1 - eta * eta * (1 - cosi * cosi)


refraction' :: Float -> V3 Float -> Float -> Float -> (Float, V3 Float, Float, Float)
refraction' cosi n etai etat
    | cosi < 0 = ((-cosi), n, etai, etat)
    | otherwise = (cosi, (-n), etat, etai)


fresnel :: Direction -> Direction -> Float -> Float
fresnel indident normal ior
    | sint >= 1 = 1
    | otherwise = (rs * rs + rp * rp) / 2
    where cosi = clipSingle (dot indident normal) (-1, 1)
          etai = if cosi > 0 then ior else 1
          etat = if cosi > 0 then 1 else ior
          sint = etai / etat * ((max 0 (1 - cosi * cosi)) ** 0.5)
          cost = (max 0 (1 - sint * sint)) ** 0.5
          cosi' = abs cosi
          rs = ((etat * cosi') - (etai * cost)) / ((etat * cosi') + (etai * cost))
          rp = ((etai * cosi') - (etat * cost)) / ((etai * cosi') + (etat * cost))


clip :: Direction -> (Float, Float) -> Direction
clip (V3 a b c) (x, y) = V3 (clip' a) (clip' b) (clip' c)
    where maxmin (a, b) n = min b $ max a n
          clip' = maxmin (x, y)

clipSingle :: Float -> (Float, Float) -> Float
clipSingle a (x, y) = min y $ max x a


colorToPixel :: Color -> PixelRGB8
colorToPixel (V3 r g b) = PixelRGB8 (to256 r) (to256 g) (to256 b)
    where to256 a = round (255 * a)


transformPos :: Matrix44 -> Position -> Position
transformPos matrix (V3 x y z) = V3 a b c
    where position1 = (V4 x y z 1)
          (V4 a b c d) = matrix !* position1


transformDir :: Matrix44 -> Direction -> Direction
transformDir matrix (V3 x y z) = normalize $ V3 a b c
    where direction0 = (V4 x y z 0)
          (V4 a b c d) = matrix !* direction0


transformRay :: Matrix44 -> Ray -> Ray
transformRay matrix ray = Ray newOrig newDir
    where newOrig = transformPos matrix (origin ray)
          newDir = transformDir matrix (direction ray)


transformHit :: Matrix44 -> Matrix44 -> Hit -> Hit
transformHit matrix transNorm hit = Hit newp newnorm (hitObj hit)
    where newp = transformPos matrix (hitPoint hit)
          newnorm = transformDir transNorm (hitNorm hit)


getImageX :: Transformed Camera -> Int
getImageX (Transformed camera matrix) = fst $ imageSize camera


getImageY :: Transformed Camera -> Int
getImageY (Transformed camera matrix) = snd $ imageSize camera


imageGenerator :: String -> Scene -> IO ()
imageGenerator path scene = writePng path $ generateImage (renderPixel scene) x y
    where x = getImageX $ camera scene
          y = getImageY $ camera scene


getTransformMatrix :: [Transformation] -> Matrix44
getTransformMatrix transList = foldl (!*!) identity matrixList
    where matrixList = map getTransformMatrix' transList


getTransformMatrix' :: Transformation -> Matrix44
getTransformMatrix' (Rotate X d) = V4 (V4 1 0 0 0)
                                        (V4 0 c (-s) 0)
                                        (V4 0 s c 0)
                                        (V4 0 0 0 1)
    where c = cos (degreeToRadian d)
          s = sin (degreeToRadian d)

getTransformMatrix' (Rotate Y d) = V4 (V4 c 0 s 0)
                                        (V4 0 1 0 0)
                                        (V4 (-s) 0 c 0)
                                        (V4 0 0 0 1)
    where c = cos (degreeToRadian d)
          s = sin (degreeToRadian d)

getTransformMatrix' (Rotate Z d) = V4 (V4 c (-s) 0 0)
                                        (V4 s c 0 0)
                                        (V4 0 0 1 0)
                                        (V4 0 0 0 1)
    where c = cos (degreeToRadian d)
          s = sin (degreeToRadian d)

getTransformMatrix' (Translate (V3 a b c)) = V4 (V4 1 0 0 a)
                                                (V4 0 1 0 b)
                                                (V4 0 0 1 c)
                                                (V4 0 0 0 1)

getTransformMatrix' (Scale (V3 a b c)) = V4 (V4 a 0 0 0)
                                            (V4 0 b 0 0)
                                            (V4 0 0 c 0)
                                            (V4 0 0 0 1)

degreeToRadian :: Float -> Float
degreeToRadian x = x * pi / 180


generateScene :: Int -> Scene
generateScene n = Scene { camera = transformedCamera
                        , objectList = [transformedPlane0]
                        , lightList = [light]
                        , defaultColor = V3 0 0 0
                        , ambientLight = ambient
                        }
    where material0 = Material { color = V3 0.2 0.2 0.2
                             , smoothness = 80
                             , reflectivity = 1
                             , ior = 2.0
                             , transmission = 0
                             }

          material1 = Material { color = V3 1 0 0
                             , smoothness = 80
                             , reflectivity = 0.8
                             , ior = 1.5
                             , transmission = 1
                             }

          material2 = Material { color = V3 0.7 0.95 0.95
                             , smoothness = 80
                             , reflectivity = 0.5
                             , ior = 1.5
                             , transmission = 0
                             }

          material3 = Material { color = V3 0.93 0.73 0.73
                             , smoothness = 80
                             , reflectivity = 0.5
                             , ior = 1.5
                             , transmission = 0
                             }
          p0 = V3 0 100 0
          p1 = V3 (-80) (-50) (-30)
          p2 = V3 10 (-10) 20
          p3 = V3 90 (-40) (-40)
          plane0 = Plane (V3 0 (-100) 0) (V3 0 1 0) False
          board = CheckerBoard (V3 0 (-100) 0) (V3 0 1 0) False material0 material2 80
          tetrahedron = CompositeTriangle [p0, p1, p2, p3] [0, 1, 2, 0, 2, 3, 0, 3, 1, 2, 1, 3]
          identityMatrix = identity :: M44 Float
          stretchMatrix = getTransformMatrix [(Translate (V3 0 0 (-200))), (Rotate Y ((fromIntegral n) * 12))]
          shadedPlane0 = ShadedObject { shadeObj = board, material = material2 }
          transformedPlane0 = Transformed shadedPlane0 identityMatrix
          shadedTetra = ShadedObject { shadeObj = tetrahedron, material = material3 }
          transformedTetra = Transformed shadedTetra stretchMatrix
          light = Light { lightPos = V3 (-100) 400 0
                        , brightness = 1
                        , lightColor = V3 1 1 1
                        }
          ambient = Light { lightPos = V3 0 0 0
                        , brightness = 0.1
                        , lightColor = V3 1 1 1
                        }
          mycamera = Camera { cameraPos = V3 0 0 0
                        , focalLength = 15
                        , cameraAperture = (36, 24)
                        , imageSize = (600, 400)
                        , aaFactor = 2
                        }
          transformedCamera = Transformed mycamera identityMatrix


serialImageGenerator :: Int -> IO()
serialImageGenerator n = imageGenerator path scene
    where scene = generateScene n
          path = "canvas" ++ printf "%02d" n ++ ".png"


main :: IO ()
main = do
    --let sphere0 = Sphere {center = V3 0 50 (-500), radius = 200}
    --let sphere1 = Sphere {center = V3 130 (-30) (-300), radius = 50}
    --let shadedSphere0 = ShadedObject { shadeObj = sphere0, material = material0 }
    --let transformedSphere0 = Transformed shadedSphere0 stretchMatrix
    --let shadedSphere1 = ShadedObject { shadeObj = sphere1, material = material1 }
    --let transformedSphere1 = Transformed shadedSphere1 identityMatrix

    mapM serialImageGenerator [0]
    return ()
