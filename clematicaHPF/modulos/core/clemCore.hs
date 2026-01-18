module ClemCoreV1
    (ClemCore, 
    Complejo(..),
    sumar,
    producto, 
    division, 
    sucesor, 
    opuesto)
where 

-- Para manejar valores complejos
data Complejo = Lateral Double Double -- Tipo algebraico para los complejos, sería a + bi
    deriving (Eq) 

data ClemCore = ExpA Int --Defino un Tipo Algebraico que maneje los valores de la calculadora por Pattern Matching
            | ExpB Double
            | ExpC Double Double
            | ExpD Complejo 
            | ExpE Complejo Complejo
            deriving (Eq)

-- 1. Instancias SHOW personalizadas
-- Para Complejo
instance Show Complejo where
    show (Lateral a b) = show a ++ " + " ++ show b ++ "i"

-- Para ClemCore
instance Show ClemCore where
    show (ExpA x)   = "Valor Entero: " ++ show x
    show (ExpB x)   = "Valor Real: " ++ show x
    show (ExpC x y) = "Operación Real: (" ++ show x ++ ", " ++ show y ++ ")"
    show (ExpD c)   = "Valor Complejo: " ++ show c
    show (ExpE c1 c2) = "Operación Compleja: (" ++ show c1 ++ ") y (" ++ show c2 ++ ")"

--2. Operaciones de la interfaz
sucesor :: ClemCore -> ClemCore       -- O(1)
opuesto :: ClemCore -> ClemCore       -- O(1)
sumar :: ClemCore -> ClemCore         -- O(1)
restar :: ClemCore -> ClemCore        -- O(1)
producto :: ClemCore -> ClemCore      -- O(1)
division :: ClemCore -> ClemCore      -- O(1)

-- Definicion de sucesor
sucesor (ExpA x) = ExpA (x + 1)
sucesor _ = error "Error: El sucesor esta definido solo para Enteros."

-- Definicion de opuesto
opuesto (ExpD (Lateral x y)) = ExpD (Lateral (-x) (-y))
opuesto (ExpB x) = ExpB (-x)
opuesto (ExpA x) = ExpA (-x)
opuesto _ = error "Error: Opuesto solo no está definido para este constructor."

-- Definicion de sumar
sumar (ExpE (Lateral x y) (Lateral z w)) = ExpD (Lateral (x + z) (y + w))
sumar (ExpC x y) = ExpB (x + y)
sumar _ = error "Error: Constructor incorrecto o falta segundo número."

-- Definicion de restar
restar (ExpE (Lateral x y) (Lateral z w)) = ExpD (Lateral (x - z) (y - w))
restar (ExpC x y) = ExpB (x - y) 
restar _ = error "Error: Constructor incorrecto o falta segundo número."

-- Definicion de producto
-- (x + yi)(z + wi) = (xz - yw) + (xw + yz)i
producto (ExpE (Lateral x y) (Lateral z w)) = ExpD (Lateral (x*z - y*w) (x*w + y*z))
producto (ExpC x y) = ExpB (x * y)
producto _ = error "Error: Constructor incorrecto o falta segundo número."

-- Definicion de division
division (ExpC _ 0) = error "Error: No se puede dividir por cero."
division (ExpC x y) = ExpB (x / y) 
-- Division de complejos multiplicando por el conjugado
division (ExpE (Lateral x y) (Lateral z w)) 
    | z == 0 && w == 0 = error "Error: No se puede dividir por cero."
    | otherwise = let den = z*z + w*w
                in ExpD (Lateral ((x*z + y*w)/den) ((y*z - x*w)/den))
division _ = error "Error: Constructor incorrecto o falta segundo número."
