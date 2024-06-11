import Text.Show.Functions
import Data.List(genericLength)
import GHC.Conc (numCapabilities)
import Distribution.Simple.Program.HcPkg (list)

type Tareas = Heroe -> Heroe

data Heroe = Heroe {
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefactos],
    tareas :: [Tareas]
} deriving(Show)

data Artefactos = Artefactos {
    nombre :: String,
    rareza :: Int
} deriving(Show)

heracles :: Heroe
heracles = Heroe {
    epiteto = "Guardian del Olimpo",
    reconocimiento = 700,
    artefactos = [pistolaRara, relampagoDeZeus],
    tareas = [matarUnaBestia leonDeNemea]
}

pistolaRara :: Artefactos
pistolaRara = Artefactos "Fierro de la antigua Grecia" 1000

xiphos :: Artefactos
xiphos = Artefactos "Xiphos" 50

lanzaOlimpo :: Artefactos
lanzaOlimpo = Artefactos "Lanza de Olimpo" 100

paseHistoria :: Tareas
paseHistoria heroe
    | reconocimiento heroe > 1000 = cambiarEpiteto "El mitico" heroe
    | reconocimiento heroe >= 500 = cambiarEpiteto "El magnifico" . guardarArtefacto lanzaOlimpo $ heroe
    | reconocimiento heroe > 100 = cambiarEpiteto "Hoplita" . guardarArtefacto xiphos $ heroe
    | otherwise = heroe

cambiarEpiteto :: String -> Tareas
cambiarEpiteto nombre heroe = heroe {epiteto = nombre} 

encontrarArtefacto :: Artefactos -> Tareas
encontrarArtefacto artefacto heroe = subirReconocimiento (rareza artefacto) . guardarArtefacto artefacto $ heroe

subirReconocimiento :: Int -> Tareas
subirReconocimiento rareza heroe = heroe {reconocimiento = reconocimiento heroe + rareza}

guardarArtefacto :: Artefactos -> Tareas
guardarArtefacto artefacto heroe = heroe {artefactos = artefacto: artefactos heroe}

escalarOlimpo :: Tareas
escalarOlimpo heroe = agregarArtefacto relampagoDeZeus . subirReconocimiento 500 . filtrarMenores . triplicarRareza $ heroe

triplicarRareza :: Tareas
triplicarRareza heroe = cambiarArtefactos (map unArtefacto) heroe

unArtefacto :: Artefactos -> Artefactos
unArtefacto artefa = artefa {rareza = rareza artefa *3}

cambiarArtefactos :: ([Artefactos] -> [Artefactos]) -> Heroe -> Heroe 
cambiarArtefactos modificador unHeroe = unHeroe {artefactos = modificador (artefactos unHeroe)}

filtrarMenores :: Tareas
filtrarMenores heroe = cambiarArtefactos (filter (not . esComun)) heroe

esComun :: Artefactos -> Bool
esComun artefacto = rareza artefacto < 1000

relampagoDeZeus :: Artefactos
relampagoDeZeus = Artefactos "Relampago de Zeus" 1000

agregarArtefacto :: Artefactos -> Heroe -> Heroe
agregarArtefacto artefacto heroe = heroe {artefactos = artefacto : artefactos heroe}

-- ayudarACruzarLaCalle :: Int -> Heroe -> Heroe
-- ayudarACruzarLaCalle cuadras unHeroe 
--     | cuadras == 1 = unHeroe {epiteto = "groso" ++ epiteto unHeroe}
--     | otherwise = ayudarACruzarLaCalle (cuadras -1) (unHeroe {epiteto = "o" ++ epiteto unHeroe})

ayudarACruzarLaCalle :: Int -> Heroe -> Heroe
ayudarACruzarLaCalle cuadras unHeroe = unHeroe {epiteto = "gros" ++ replicate cuadras 'o'}

matarUnaBestia :: Bestia -> Tareas
matarUnaBestia unaBestia unHeroe 
    | debilidad unaBestia unHeroe = unHeroe {epiteto = "El asesino de" ++ monstruo unaBestia}
    | otherwise = perderPrimerArtefacto . cambiarEpiteto "El cobarde" $ unHeroe

perderPrimerArtefacto :: Tareas
perderPrimerArtefacto unHeroe = unHeroe{artefactos = drop 1 (artefactos unHeroe)}

data Bestia = Bestia {
    monstruo :: String,
    debilidad :: Debilidad
}

type Debilidad = Heroe -> Bool

leonDeNemea :: Bestia
leonDeNemea = Bestia {
    monstruo = "León de Nemea",
    debilidad = (> 20) . length . epiteto
}

hacerUnaTarea :: Tareas -> Heroe -> Heroe
hacerUnaTarea tarea unHeroe = agregarTarea tarea (tarea unHeroe)

agregarTarea :: Tareas -> Tareas
agregarTarea tarea unHeroe = unHeroe{tareas = tarea : tareas unHeroe}

presumir :: Heroe -> Heroe -> (Heroe, Heroe)
presumir heroe1 heroe2 
    | reconocimiento heroe1 > reconocimiento heroe2 = (heroe1,heroe2)
    | reconocimiento heroe1 < reconocimiento heroe2 = (heroe2,heroe1)
    | sumatoriaRarezas heroe1 > sumatoriaRarezas heroe2 = (heroe1,heroe2)
    | sumatoriaRarezas heroe1 < sumatoriaRarezas heroe2 = (heroe2,heroe1)
    | otherwise = presumir (realizarLabor (tareas heroe1) heroe2) (realizarLabor (tareas heroe2) heroe1)


sumatoriaRarezas :: Heroe -> Int
sumatoriaRarezas heroe = sum . map rareza . artefactos $ heroe


realizarLabor :: [Tareas] -> Heroe -> Heroe
realizarLabor listaTareas unHeroe = foldl (flip hacerUnaTarea) unHeroe listaTareas
