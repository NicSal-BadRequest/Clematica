module ClemCoreV1
    (ClemCore, sumar, producto, division, sucesor, opuesto)
where 

data ClemCore = ExpA Int
              | ExpB Double
              | ExpC Double Double

sucesor :: ClemCore -> Int          -- O(1)
opuesto :: ClemCore -> Double       -- O(1)
sumar :: ClemCore -> Double         -- O(1)
restar :: ClemCore -> Double        -- O(1)
producto :: ClemCore -> Double      -- O(1)
division :: ClemCore -> Double      -- O(1)

-- Definicion de sucesor
sucesor (ExpA x) = x + 1
sucesor _ = error "El sucesor esta definido solo para Enteros"

-- Definicion de opuesto
opuesto (ExpB x) = x * (-1)
opuesto (ExpA x) = fromIntegral x

-- Definicion de sumar
sumar (ExpC x y) = x + y
sumar (ExpB x) = x
sumar (ExpA x) = fromIntegral x

-- Definicion de restar
restar (ExpC x y) = x - y 
restar (ExpB x) = x
restar (ExpA x) = fromIntegral x

-- Definicion de producto
producto (ExpC x y) = x * y
producto (ExpB x) = x
producto (ExpA x) = fromIntegral x

-- Definicion de division
division (ExpC _ 0) = error "División por cero"
division (ExpC x y) = x / y 
division (ExpB x) = x
division (ExpA x) = fromIntegral x