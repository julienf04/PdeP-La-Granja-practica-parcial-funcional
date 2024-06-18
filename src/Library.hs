module Library where
import PdePreludat



--------------------------- DATAS y TYPES ---------------------------


data Animal = Animal {
    nombre :: String,
    tipo :: String, -- En ningun punto del enunciado es necesario usar el tipo del animal, pero en el primer parrafo se pide registrarlo igualmente. Probablemente venga mejor que sea un Class en vez de un string, pero como no lo tengo que usar, lo modelé con el tipo de dato según mi conveniencia y que a la vez tenga algo de sentido.
    peso :: Number, -- El peso en kilos
    edad :: Number, -- Edad en años
    estaEnfermo :: Bool,
    visitasMedicas :: [VisitaMedica]
} deriving (Show)


data VisitaMedica = VisitaMedica {
    diasDeRecuperacion :: Number,
    costoPorAtencion :: Number
} deriving (Show)


type Actividad = Animal -> Animal


--------------------------- PRUEBAS ---------------------------


vacaLoca = Animal {
    nombre = "La vaca locai",
    tipo = "Vaca",
    peso = 1000,
    edad = 10,
    estaEnfermo = True,
    visitasMedicas = [VisitaMedica {
        diasDeRecuperacion = 20,
        costoPorAtencion = 100
    }]
}

visitaLoca = VisitaMedica {
    diasDeRecuperacion = 50,
    costoPorAtencion = 200
}



--------------------------- AUXILIARES ---------------------------


aumentarKilos :: Number -> Actividad
aumentarKilos kilos animal = animal {
    peso = peso animal + kilos
}

agregarVisitaMedica :: VisitaMedica -> Actividad
agregarVisitaMedica visitaMedica animal = animal {
    visitasMedicas =  visitasMedicas animal ++ [visitaMedica]
}

aumentarAnios :: Number -> Actividad
aumentarAnios anios animal = animal {
    edad = edad animal + anios
}

cambiarEstado :: Bool -> Actividad
cambiarEstado estado animal = animal {
    estaEnfermo = estado
}

dentroDelRango :: Number -> Number -> Number -> Bool
dentroDelRango minimo maximo numero = numero >= minimo && numero <= maximo


--------------------------- PUNTO 1 ---------------------------

laPasoMal :: Animal -> Bool
laPasoMal animal = any ((> 30) . diasDeRecuperacion) (visitasMedicas animal)


tieneNombreFalopa :: Animal -> Bool
tieneNombreFalopa animal = (last . nombre) animal == 'i'



--------------------------- PUNTO 2 ---------------------------


engorde :: Number -> Actividad
engorde kilos = aumentarKilos (if kilos > 5 then 5 else kilos / 2)


revisacion :: VisitaMedica -> Actividad
revisacion visitaMedica animal = if estaEnfermo animal then (agregarVisitaMedica visitaMedica . aumentarKilos 2) animal else animal


festejoCumple :: Actividad
festejoCumple = aumentarAnios 1 . aumentarKilos (-1)


chequeoDePeso :: Number -> Actividad
chequeoDePeso kilos animal = cambiarEstado (peso animal > kilos) animal



--------------------------- PUNTO 3 ---------------------------


proceso :: Animal -> [Actividad] -> Animal
proceso = foldr ($)


---- Cómo se podía evaluar por consola el proceso para cada una de las actividades resueltas en el punto anterior:

-- proceso vacaLoca [engorde 4, revisacion visitaLoca, festejoCumple, chequeoDePeso 700]



--------------------------- PUNTO 4 ---------------------------


-- Para que sea true, el peso del animal luego de la actividad debe estar entre el rango numerico:
-- [ Peso animal antes de la actividad ; Peso animal antes de la actividad + 3 ]
mejoraSustentablementeElPeso :: [Actividad] -> Animal -> Bool
mejoraSustentablementeElPeso [] _ = True
mejoraSustentablementeElPeso (actividad:actividades) animal = dentroDelRango (peso animal) (peso animal + 3) ((peso . actividad) animal) && mejoraSustentablementeElPeso actividades animal



--------------------------- PUNTO 5 ---------------------------

------ a)

primeros3AnimalesConNombreFalopa :: [Animal] -> [Animal]
primeros3AnimalesConNombreFalopa = take 3 . filter tieneNombreFalopa


------ b)

-- Si en esa lista infinita hay al menos 3 animales que tienen nombre falopa, entonces sí, ya que toma esos 3 animales y retorna
-- una nueva lista con esos 3 animales solamente.
-- Si hay menos de 3 animales con nombre falopa en esa lista infinita, entonces no sería posible obtener un valor, ya que
-- se quedaría infinitamente buscando a los 3 animales con nombre falopa, pero nunca los va a terminar de encontrar.