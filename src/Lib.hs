module Lib () where

-------------
-- Punto 1 --
-------------

data Auto = Auto {
    color :: Color,
    velocidad :: Int,
    distancia :: Int
} deriving (Eq)

type Carrera = [Auto]

data Color = Negro | Rojo | Azul | Blanco deriving (Eq)

estaCercaDe :: Auto -> Auto -> Bool
estaCercaDe unAuto otroAuto = 
    (not . sonIguales unAuto) otroAuto && ((< 10) . abs . subtract (distancia unAuto))(distancia otroAuto)

sonIguales :: Auto -> Auto -> Bool
sonIguales unAuto otroAuto = color unAuto == color otroAuto

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo unAuto unaCarrera = all (not . estaCercaDe unAuto) unaCarrera && lesVaGanandoATodos unAuto unaCarrera

lesVaGanandoATodos :: Auto -> Carrera -> Bool
lesVaGanandoATodos unAuto  = all (leVaGanando unAuto) 

puestoDeUnAuto :: Auto -> Carrera -> Int
puestoDeUnAuto unAuto unaCarrera = (+1) . length . filter (not . leVaGanando unAuto) $ unaCarrera -- usé count = (length . filter condicion)

leVaGanando :: Auto -> Auto -> Bool
leVaGanando unAuto = (< distancia unAuto) . distancia

-------------
-- Punto 2 --
-------------

queCorra :: Int -> Auto -> Auto
queCorra unTiempo unAuto = unAuto { distancia = (+) (velocidad unAuto * unTiempo) . distancia $ unAuto } 

mapVelocidad :: (Int -> Int) -> Auto -> Auto
mapVelocidad accion unAuto = unAuto { velocidad = accion . velocidad $ unAuto }     

bajarLaVelocidad :: Int -> Auto -> Auto
bajarLaVelocidad unaVelocidad = mapVelocidad (restarVelocidad unaVelocidad)

restarVelocidad :: Int -> Int -> Int
restarVelocidad unaVelocidad otraVelocidad 
    | subtract unaVelocidad otraVelocidad < 0 = 0
    | otherwise                               = subtract unaVelocidad otraVelocidad   

-------------
-- Punto 3 --
-------------

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista


type PowerUp = (Carrera -> Carrera)

terremoto :: Auto -> PowerUp
terremoto unAuto = afectarALosQueCumplen (estaCercaDe unAuto) (bajarLaVelocidad 50)

miguelitos :: Int -> Auto -> PowerUp
miguelitos unaVelocidad unAuto = afectarALosQueCumplen (leVaGanando unAuto) (bajarLaVelocidad unaVelocidad)

jetPack :: Int -> Auto -> PowerUp
jetPack unTiempo unAuto = 
    afectarALosQueCumplen (sonIguales unAuto) (mapVelocidad (const (velocidad unAuto)) . queCorra unTiempo . mapVelocidad (*2))

{- misilTeledirigido :: Auto -> Color -> PowerUp
misilTeledirigido unAuto colorDeUnaVictima = 
    afectarALosQueCumplen ((==) colorDeUnaVictima . color) (accionMisil) -}
-------------
-- Punto 4 --
-------------
type Evento = (Carrera -> Carrera)
type Puesto = (Int, Color)

simularCarrera :: Carrera -> [Evento] -> [Puesto]
simularCarrera unaCarrera unosEventos = 
    zipWith (\unAuto numero -> (numero, color unAuto)) (foldl (flip($)) unaCarrera unosEventos) [1..length unaCarrera]
-- considerando que el auto cabeza de la lista sería el primero

correnTodos :: Int -> Evento
correnTodos unTiempo = map (queCorra unTiempo)

usaPowerUp :: Color -> PowerUp
usaPowerUp unColor unPowerUp = unPowerUp unColor . filter ((==) unColor . color)

carrera :: Carrera
carrera = [Auto Blanco 120 0, Auto Azul 120 0, Auto Negro 120 0, Auto Rojo 120 0]

serieDeEventos :: [Evento]
serieDeEventos = [correnTodos 30, 
        usaPowerUp Azul jetPack 3, 
        usaPowerUp Blanco terremoto, 
        correnTodos 40,
        usaPowerUp Blanco miguelitos 20,
        usaPowerUp Negro jetPack 6,
        correnTodos 10]

carreraPedida :: [Puesto]
carreraPedida = simularCarrera carrera serieDeEventos

-------------
-- Punto 5 --
-------------

-- a) fue dificil pero se me ocurrio algo despues de 20 minutos
{- 
misilTeledirigido :: Auto -> Color -> PowerUp
misilTeledirigido unAuto colorDeUnaVictima = 
    afectarALosQueCumplen ((==) colorDeUnaVictima . color) (accionMisil)
 -} -- el primer parametro me queda sin usar pero serviria para saber quien le manda el misil

-- b) la 1b depende, si hay un auto que no cumpla. Y la 1c no, porque tiene que recorrer toda la lista
