module Library where
import PdePreludat

----------------------- Ejemplo Type classes ------------------------------------
class Precio algoConPrecio where
    precio :: algoConPrecio -> Number
    --cuantoPastoCorta :: algoConPrecio -> Number

data Vaca = Vaca {
    nombre :: String,
    edad :: Number,
    peso :: Number,
    raza :: String
} deriving Show

instance Precio Vaca where
    precio vaca = (peso vaca * 10) - (edad vaca / 3)
    --cuantoPastoCorta

data Podadora = Podadora {
    marca :: String,
    superficie :: Number,
    potencia :: Number
} deriving Show

instance Precio Podadora where
    precio podadora = superficie podadora * potencia podadora

comioPasto kgPasto = modificarPeso kgPasto
correr km = modificarPeso (negate km)
modificarPeso delta vaca = vaca{ peso = peso vaca + delta }

aurora = Vaca "Aurora" 5 100 "Holando-argentina"
poda1500 = Podadora "Kärcher" 50 200
{-
Precio de vaca peso * 10 - (edad / 3)
Precio de podadora es superficie * potencia
-}
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type Desgaste = Number
type Patente = String
type Fecha = (Number, Number, Number)
 
-- Definiciones base
anio :: Fecha -> Number
anio (_, _, year) = year
 
data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Number,
 temperaturaAgua :: Number,
 ultimoArreglo :: Fecha
} deriving Show


{-
si la patente tiene 7 dígitos, es $ 12.500
si no, si la patente está entre las letras "DJ" y "NB", se aplica el calculoPatental
que es $ 3.000 multiplicado por la longitud para las patentes que terminen en 4
o $ 20.000 para el resto de las patentes
de lo contrario, se le cobra $ 15000
-}
-- 1
costoReparacion auto
    | (==7).length.patente $ auto = 12500
    | patente auto >= "DJ" && patente auto <= "NB" = calculoPatental.patente $ auto
    | otherwise = 15000

calculoPatental unaPatente
    | (=='4').last $ unaPatente = length unaPatente * 3000
    | otherwise = 20000
-- Maximizar el CFD => Coeficiente de Felicidad del Docente


-- 2a
esPeligroso = (>0.5).head.desgasteLlantas

-- 2b
necesitaRevision = (<= 2015) . anio . ultimoArreglo

-- 3
type Tecnico = Auto -> Auto

alfa,bravo,charly,tango,zulu,lima :: Tecnico
alfa auto = auto{ rpm = min (rpm auto) 2000 }
bravo auto = auto{ desgasteLlantas = [0,0,0,0] }
charly = bravo.alfa
tango = id
--tango auto = auto
zulu auto = lima auto{ temperaturaAgua = 90 }
lima auto = auto{ desgasteLlantas = [0,0] ++ (drop 2.desgasteLlantas $ auto) }



-- 4
estaOrdenadoTOC [] = True
estaOrdenadoTOC [unicoAuto] = odd.desgaste $ unicoAuto
estaOrdenadoTOC (a1:a2:autos) = (odd.desgaste $ a1) && 
        (even.desgaste $ a2) && 
        estaOrdenadoTOC autos

desgaste = round.(*10).sum.desgasteLlantas

-- 5
data OrdenReparacion = OrdenReparacion {
    fecha :: Fecha,
    tecnicos :: [Tecnico]
}

aplicarOrdenReparacion orden auto = autoReparado{ ultimoArreglo = fecha orden }
    where autoReparado = foldl (flip ($)) auto $ tecnicos orden

aplicarOrdenReparacion'' orden auto = autoReparado{ ultimoArreglo = fecha orden }
    where autoReparado = foldr ($) auto . reverse . tecnicos $ orden

aplicarOrdenReparacion' orden auto = foldl (flip ($)) autoConFecha . tecnicos $ orden
    where autoConFecha = auto{ ultimoArreglo = fecha orden }

-- 6a
quienesDejanEnCondiciones auto tecnicos = filter (not.esPeligroso.($auto)) tecnicos 

-- 6b
costoDeAutosRotos  = sum . map costoReparacion . filter esPeligroso

-- 7a
{- En base al punto “dada una lista de técnicos determinar qué técnicos dejarían el 
auto en condiciones” y considerando una lista de técnicos  infinita, ¿podríamos obtener 
el primer técnico que deja el auto en condiciones? Muestre un ejemplo y justifique. -}

-- > head (quienesDejanEnCondiciones autito tecnicosInfinitos)
-- repeat tango
-- tecnicosInfinitos debe tener al menos un técnico que deje el auto en condiciones.

-- 7b
{-
En base al punto “Dada una lista de autos, saber cuál es el costo de reparación de los 
autos que necesitan revisión.”,  ¿podríamos tener una lista infinita de autos? 
Muestre un ejemplo y justifique. 
-}
-- No hay forma, se debe procesar toda la lista para poder obtener el total del costo

{-
Y si tomáramos en cuenta los tres primeros autos que necesitan revisión, ¿cómo debería 
cambiar la función?
-}
costoDeAutosRotos'  = sum . map costoReparacion . take 3 . filter esPeligroso
-- Funciona, siempre y cuando haya al menos 3 autos que necesitan revisión

{-
Por otra parte, ¿está versión aceptaría una lista infinita de autos? Modifique la 
función 6.b con otro nombre y justifique su respuesta.
-}

costoDeAutosRotos'' cantidad  = sum . map costoReparacion . take cantidad . filter esPeligroso

