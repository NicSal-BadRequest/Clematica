module ClemCoreV1
    (ClemCore(...), 
    sumar,
    producto, 
    division, 
    ucesor, 
    opuesto)
where 

-- Para manejar valores complejos
data Complejo = Lateral Double Double -- Tipo algebraico para los complejos, sería a + bi
    deriving (Show, Eq) 

data ClemCore = ExpA Int --Defino un Tipo Algebraico que maneje los valores de la calculadora por Pattern Matching
            | ExpB Double
            | ExpC Double Double
            | ExpD Complejo 
            | ExpE Complejo Complejo
            deriving (Show, Eq)

-- Operaciones de la interfaz
sucesor :: ClemCore -> ClemCore       -- O(1)
opuesto :: ClemCore -> ClemCore       -- O(1)
sumar :: ClemCore -> ClemCore         -- O(1)
restar :: ClemCore -> ClemCore        -- O(1)
producto :: ClemCore -> ClemCore      -- O(1)
division :: ClemCore -> ClemCore      -- O(1)

-- Definicion de sucesor
sucesor (ExpA x) = ExpA (x + 1)
sucesor _ = error "El sucesor esta definido solo para Enteros"

-- Definicion de opuesto
opuesto (ExpD (Lateral x y)) = ExpD (Lateral (-1 * x) (-1 * y))
opuesto (ExpB x) = ExpB (-1 * x)
opuesto (ExpA x) = ExpA (-1 * x)

-- Definicion de sumar
sumar (ExpE (Lateral x y) (Lateral z w)) = ExpE (Lateral (x + z) (y + w))
sumar (ExpD (Lateral x y)) = ExpD (Lateral x y)
sumar (ExpC x y) = ExpC (x + y)
sumar (ExpB x) = ExpB x
sumar (ExpA x) = ExpA x

-- Definicion de restar
restar (ExpC x y) = ExpC (x - y) 
restar (ExpB x) = ExpB x
restar (ExpA x) = ExpA x

-- Definicion de producto
producto (ExpC x y) = ExpC (x * y)
producto (ExpB x) = ExpB x
producto (ExpA x) = ExpA x

-- Definicion de division
division (ExpD _ (Lateral 0 0)) = error "Error: No se puede divir por cero."
division (ExpC _ 0) = error "Error: No se puede dividir por cero."
division (ExpC x y) = ExpC (x / y) 
division (ExpB x) = ExpB x
division (ExpA x) = ExpB x

-- Personalizar la visualización:
instance Show ClemCore where
    show (Lateral x y) = "Valor complejo: (" ++ show x ++ ", " ++ show y ++ "i )."

instance Show ClemCore where
    show (ExpA x)   = "Valor Entero: " ++ show x ++ "."
    show (ExpB x)   = "Valor Real: " ++ show x ++ "."
    show (ExpC x y) = "(" ++ show x ++ " , " ++ show y ++ ")."