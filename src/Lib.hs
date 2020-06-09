module Lib where
import Text.Show.Functions
laVerdad = True
--Matias Rizzato Busta 167.782-2

------------------------------------- Punto 1 -------------------------------------
data Guante = UnGuante{
    material :: String,
    gemas :: [Gema]
}deriving (Show)

type Gema = Personaje->Personaje
type Habilidad = String

data Personaje = UnPersonaje{
    edad :: Int,
    energia :: Int,
    habilidades :: [Habilidad],
    nombre :: String,
    planeta :: String
}deriving (Show,Eq)

data Universo = UnUniverso{
    habitantes :: [Personaje]
}deriving (Show,Eq)

chasquido :: Guante->Universo->Universo
chasquido guante universo |((== 6).length.gemas) guante = universo{habitantes= reduzco (habitantes universo)}
                          |otherwise = universo

reduzco :: [Personaje]->[Personaje]
reduzco habitantes = take (laMitad habitantes) habitantes

laMitad :: [Personaje]->Int
laMitad habitantes = (length habitantes) `div `2