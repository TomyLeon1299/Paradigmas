import Text.Show.Functions()

-- PRIMERA PARTE
-- PUNTO 1

data Persona = Persona{
    edad :: Int,
    experiencia :: Int,
    items :: [String]
}deriving(Show)

data Criatura = Criatura{
    peligrosidad :: Int,
    debilidad :: Debilidad
}deriving(Show)

type Debilidad = Persona -> Bool

siempredetras :: Criatura
siempredetras = Criatura {peligrosidad = 0, debilidad = undefined}

gnomos :: Int -> Criatura
gnomos cantidad = Criatura {peligrosidad = 2 ^ cantidad, debilidad = tieneItem "Soplador de hojas"}

tieneItem :: String -> Debilidad
tieneItem item persona = any (== item) (items persona)

fantasmas :: Int -> Debilidad -> Criatura
fantasmas nivel debilidadDada = Criatura {peligrosidad = nivel * 20, debilidad = debilidadDada}

-- Mapeos

mapExperiencia :: (Int -> Int) -> Persona -> Persona
mapExperiencia unaFuncion persona = persona {experiencia = unaFuncion (experiencia persona) }

-- PUNTO 2

enfrentarCriatura :: Persona -> Criatura -> Persona
enfrentarCriatura persona criatura
    | (debilidad criatura) persona = mapExperiencia ((+) (peligrosidad criatura)) persona
    | otherwise = mapExperiencia ((+) 1) persona

--puedeDeshacerse :: Criatura -> Persona -> Bool
--puedeDeshacerse criatura persona = 

-- PUNTO 3

determinarExperiencia :: Persona -> [Criatura] -> Int
determinarExperiencia persona criaturas = experiencia (foldl (enfrentarCriatura) persona criaturas)

grupoCriaturasEjemplo :: [Criatura]
grupoCriaturasEjemplo = [siempredetras, gnomos 10, fantasmas 3 debilidad3, fantasmas 1 debilidad1]

debilidad3 :: Debilidad
debilidad3 persona = edad persona < 13 && (tieneItem "disfraz de oveja" persona)

debilidad1 :: Debilidad
debilidad1 persona = experiencia persona > 10

-- determinarExperiencia persona grupoCriaturasEjemplo

-- SEGUNDA PARTE
-- PUNTO 1

zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf f [a] [b] = [f a b]
zipWithIf f (x:xs) (y:ys) = zipWithIf f [x] [y] ++ zipWithIf f xs ys

-- MUY EXTRAÑA ESTA PARTE

-- PUNTO 2

abecedarioDesde :: Char -> [Char]
abecedarioDesde letra = [letra..'z'] ++ ['a'..letra]

desencriptarLetra :: Char -> Char -> Char
desencriptarLetra letraInicio letraEncriptada = undefined