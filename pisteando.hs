import Text.Show.Functions
import Data.List(genericLength)
import GHC.Conc (numCapabilities)
import Distribution.Simple.Program.HcPkg (list)

data Auto = Auto {
    marca :: String,
    modelo :: String,
    desgaste :: Desgaste,
    velMax :: Float,
    tiempoCarrera :: Float
} deriving (Show)

data Desgaste = Desgaste {
    ruedas :: Float,
    chasis :: Float
}  deriving (Show)

ferrari :: Auto
ferrari = Auto {
    marca = "Ferrari",
    modelo = "F50",
    desgaste = Desgaste 0 0,
    velMax = 65,
    tiempoCarrera = 0
}

lamborghini :: Auto
lamborghini = Auto {
    marca = "lamborghini",
    modelo = "Diablo",
    desgaste = Desgaste 7 4,
    velMax = 73,
    tiempoCarrera = 0
}

fiat :: Auto
fiat = Auto {
    marca = "fiat",
    modelo = "600",
    desgaste = Desgaste 33 27,
    velMax = 44,
    tiempoCarrera = 0
}

buenEstado :: Auto -> Bool
buenEstado auto = ((<40) . chasis $ desgaste auto) && ((<60) . ruedas $ desgaste auto)

noDaMas :: Auto -> Bool
noDaMas auto = ((>80) . chasis $ desgaste auto) || ((>80) . ruedas $ desgaste auto)

repararUnAuto :: Auto -> Auto
repararUnAuto = cambiarDesgasteChasis (*0.15) . cambiarDesgasteRuedas (const 0)

type Tramo = Auto -> Auto

curva :: Float -> Float -> Tramo
curva longitud angulo unAuto = cambiarDesgasteRuedas (+ 3 * longitud / angulo) . cambiarTiempoCarrera (+ longitud / velMax unAuto / 2 ) $ unAuto

curvaPeligrosa :: Tramo
curvaPeligrosa = curva 300 60

curvaTranca :: Tramo
curvaTranca = curva 550 110


cambiarDesgasteChasis :: (Float -> Float) -> Auto -> Auto
cambiarDesgasteChasis f unAuto = unAuto{
    desgaste = Desgaste (ruedas (desgaste unAuto)) (f . chasis . desgaste $ unAuto)
}

cambiarDesgasteRuedas :: (Float -> Float) -> Auto -> Auto
cambiarDesgasteRuedas f unAuto = unAuto{
    desgaste = Desgaste (f . ruedas . desgaste $ unAuto) (chasis (desgaste unAuto))
}

cambiarTiempoCarrera :: (Float -> Float) -> Auto -> Auto
cambiarTiempoCarrera f unAuto = unAuto{
    tiempoCarrera = f . tiempoCarrera $ unAuto
}

recto :: Float -> Tramo
recto longitud unAuto = cambiarDesgasteChasis (+ longitud/100) . cambiarTiempoCarrera (+ longitud/velMax unAuto) $ unAuto

tramoRectoClassic :: Tramo
tramoRectoClassic = recto 750

tramito :: Tramo
tramito = recto 280

boxes :: Tramo -> Tramo
boxes unTramo unAuto 
    | buenEstado unAuto = unTramo unAuto
    | otherwise = repararUnAuto . cambiarTiempoCarrera (+10) $ unAuto

ripio :: Tramo -> Tramo
ripio unTramo unAuto = unTramo . unTramo $ unAuto

obstruccion :: Tramo -> Float -> Tramo
obstruccion unTramo longitud unAuto = cambiarDesgasteRuedas (+2*longitud). unTramo $ unAuto

pasarPorTramo :: Auto -> Tramo -> Auto
pasarPorTramo unAuto unTramo
    | noDaMas unAuto = unAuto
    | otherwise = unTramo unAuto

type Pista = [Tramo]

superpista :: Pista
superpista = [tramoRectoClassic, curvaTranca, tramito, obstruccion (curva 400 80) 2, curva 650 115, recto 970, curvaPeligrosa, ripio tramito, boxes (recto 800)]

peganLaVuelta :: Pista -> [Auto] -> [Auto]
peganLaVuelta unaPista listaAutos = filter (not . noDaMas) . map (pegaUnaVuelta unaPista) $ listaAutos

pegaUnaVuelta:: Pista -> Auto -> Auto
pegaUnaVuelta unaPista unAuto = foldl pasarPorTramo unAuto unaPista

data Carrera = Carrera {
    pista :: Pista,
    vueltas :: Int    
}

tourDeBuenosAires :: Carrera
tourDeBuenosAires = Carrera superpista 20

correr :: [Auto] -> Carrera -> [[Auto]]
correr unosAutos unaCarrera = take (vueltas unaCarrera) . iterate (peganLaVuelta (pista unaCarrera)) $ unosAutos