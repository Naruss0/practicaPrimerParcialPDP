module Library where
import PdePreludat



type Reparacion = Atraccion -> Atraccion

data Atraccion = Atraccion{
    nombre :: String,
    alturaMinima :: Number,
    duracion :: Number,
    opiniones :: [String],
    estaEnMantenimiento :: Bool,
    cantidadDeReparaciones :: [Reparacion],
    tiempoMantenimiento :: Number
}

-- Punto 1 --

esBuenaLaAtraccion :: Atraccion -> Number
esBuenaLaAtraccion atraccion
    | duracion atraccion > 10 = 100
    | (length . cantidadDeReparaciones) atraccion < 3 = 10 * (length . nombre) atraccion + 2 * (length . opiniones) atraccion
    -- | length (cantidadDeReparaciones atraccion) < 3 = 10 * length (nombre atraccion) + 2 * length (opiniones atraccion)
    | otherwise = alturaMinima atraccion * 10


-- Punto 2 --

eliminarUltimaReparacion :: Reparacion
--eliminarUltimaReparacion atraccion = atraccion {cantidadDeReparaciones = init (cantidadDeReparaciones atraccion)}
eliminarUltimaReparacion atraccion = atraccion {
cantidadDeReparaciones = (take ((length . cantidadDeReparaciones) atraccion - 1) . cantidadDeReparaciones) atraccion}
   -- where tamanio = (length . cantidadDeReparaciones ) atraccion

verificarMantenimiento :: Reparacion
verificarMantenimiento atraccion = atraccion {
    estaEnMantenimiento = not ((null . cantidadDeReparaciones) atraccion)}

-- Versión dinámica que acepta cualquier función de reparación
aplicarReparacion :: Reparacion -> Atraccion -> Atraccion
aplicarReparacion reparacion = verificarMantenimiento . eliminarUltimaReparacion . reparacion

-- Ejemplos de uso con lambdas:

--ajusteDeTornilleria 
-- > aplicarReparacion (\atraccion cantidadDeTornillos -> atraccion {duracion = min 10 (duracion atraccion + cantidadDeTornillos)}) unaAtraccion 3


--engrase
-- > aplicarReparacion (\atraccion cantidadDeEngrase -> atraccion {
--    alturaMinima = alturaMinima atraccion + cantidadDeEngrase * 0.1,
--    opiniones = opiniones atraccion ++ ["para valientes"]
-- }) unaAtraccion 5


--mantenimientoElectrico
 --   aplicarReparacion (\atraccion -> atraccion {opiniones = take 2 (opiniones atraccion)}) unaAtraccion 

--mantenimiento Basico
--
 --   aplicarReparacion (\atraccion -> atraccion {
  --      duracion = min 10 (duracion atraccion + 8),
 --       alturaMinima = alturaMinima atraccion + 10 * 0.1,
 --       opiniones = opiniones atraccion ++ ["para valientes"]
 --   }) atraccion

-- Punto 3 ---

--- ¿Qué oooooonda este parque? --

meDaMiedito :: Atraccion -> Bool
meDaMiedito atraccion = tiempoMantenimiento atraccion > 4

cerramos :: Atraccion -> Bool
cerramos atraccion = tiempoMantenimiento atraccion >= 7


disneyNoEsistis :: [Atraccion] -> Bool
disneyNoEsistis atracciones = all (null . cantidadDeReparaciones) (filter ((> 5) . length . nombre) atracciones)

--- Punto 4 ---

reparacionPeola :: Atraccion -> Bool
reparacionPeola atraccion = verificarMejoras atraccion (cantidadDeReparaciones atraccion)
  where verificarMejoras _ [] = True
        verificarMejoras _ [_] = True
        verificarMejoras atraccionActual (reparacion1:reparacion2:resto) 
          | esBuenaLaAtraccion (reparacion2 (reparacion1 atraccionActual)) > esBuenaLaAtraccion (reparacion1 atraccionActual) = 
              verificarMejoras (reparacion1 atraccionActual) (reparacion2:resto)
          | otherwise = False

