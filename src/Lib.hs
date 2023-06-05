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