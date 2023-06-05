module Lib () where

-------------
-- Punto 1 --
-------------

data Auto = Auto {
    color :: String,
    velocidad :: Int
    distancia :: Int,
}

type Carrera = [Auto]

estaCercaDe :: Auto -> Auto -> Bool
estaCercaDe unAuto otroAuto = 
    sonDistintos unAuto otroAuto && ((< 10) . abs . substract (distancia unAuto ))(distancia otroAuto)

sonDistintos :: Auto -> Auto -> Bool
sonDistintos unAuto otroAuto = color unAuto /= color otroAuto

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo unAuto unaCarrera = all (not . estaCercaDe $ unAuto) unaCarrera && lesVaGanandoATodos unAuto unaCarrera

lesVaGanandoATodos :: Auto -> Carrera -> Bool
lesVaGanandoATodos unAuto unaCarrera = all (leVaGanando unAuto) unaCarrera

puestoDeUnAuto :: Auto -> Carrera -> Int
puestoDeUnAuto unAuto unaCarrera = (+1) . count (flip leVaGanando unAuto) $ unaCarrera

leVaGanando :: Auto -> Auto -> Bool
leVaGanando unAuto = (< (distancia unAuto)) . distancia

-------------
-- Punto 2 --
-------------

queCorra :: Int -> Auto -> Auto
queCorra unTiempo unAuto = unAuto { distancia = (+) (velocidad unAuto * unTiempo) . distancia $ unAuto } 

mapVelocidad :: (Int -> Int) -> Auto -> Auto
mapVelocidad accion unAuto = unAuto { velocidad = accion . velocidad $ unAuto }     

bajarLaVelocidad :: Int -> Auto -> Auto
bajarLaVelocidad unaVelocidad unAuto = mapVelocidad (restarVelocidad unaVelocidad)

restarVelocidad :: Int -> Int
restarVelocidad unaVelocidad otraVelocidad 
    | substract unaVelocidad otraVelocidad < 0 = 0
    | otherwise                                = substract unaVelocidad otraVelocidad   

-------------
-- Punto 3 --
-------------

type PowerUp = (Auto -> Carrera -> Carrera)

terremoto :: PowerUp
terremoto unAuto = afectarALosQueCumplen (estaCercaDe unAuto) (bajarLaVelocidad 50)

miguelitos :: Int -> PowerUp
miguelitos unaVelocidad unAuto = afectarALosQueCumplen (leVaGanando unAuto) (bajarLaVelocidad unaVelocidad)

jetPack :: Int -> PowerUp
jetPack unTiempo unAuto = 
    afectarALosQueCumplen ((==) color unAuto) (mapVelocidad (const velocidad unAuto) . queCorra unTiempo . mapVelocidad (*2))

