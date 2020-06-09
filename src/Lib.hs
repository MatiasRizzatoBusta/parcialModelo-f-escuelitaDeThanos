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

thor = UnPersonaje 30 1000 ["usar Mjolir","tirar martillo"] "Thor" "Asgard"
spiderMan = UnPersonaje 20 400 ["tirar telarania","hacer piruetas","combate cuerpo a cuerpo"] "Peter Parker" "Tierra"
cptAmerica = UnPersonaje 90 990 ["ser fuerte","usar escudo","correr rapido","combate cuerpo a cuerpo"] "steve Rogers" "Tierra"

guanteThanos = UnGuante "uru" [elTiempo,laMente 200,elEspacio]

universoMarvel = UnUniverso [thor,spiderMan,cptAmerica]

chasquido :: Guante->Universo->Universo
chasquido guante universo |((== 6).length.gemas) guante = universo{habitantes= reduzcoUniverso (habitantes universo)}
                          |otherwise = universo

reduzcoUniverso :: [Personaje]->[Personaje]
reduzcoUniverso habitantes = take (laMitad habitantes) habitantes

laMitad :: [Personaje]->Int
laMitad habitantes = (length habitantes) `div `2

------------------------------------- Punto 2 -------------------------------------
aptoParaPendex :: Universo->Bool
aptoParaPendex universo = (cumpleCondicion (<45).map edad) (habitantes universo)

cumpleCondicion :: Eq a=>(a->Bool)->[a]->Bool
cumpleCondicion condicion parametro = all condicion parametro --a la lista de personajes solo la dejo con las edades y me fijo que todos 
 --cumplan la condicion que le paso.
 --podria haber hecho all ((<45).edad)

energiaUniverso :: Universo->Int
energiaUniverso  = sum.map energia.losQueTienenMasDeUnaHabilidad.habitantes

losQueTienenMasDeUnaHabilidad :: [Personaje]->[Personaje]
losQueTienenMasDeUnaHabilidad  =  filter ((>1).length.habilidades) --agarro un pj,voy a sus habilidades les hago length y comparo

------------------------------------- Punto 3 -------------------------------------
laMente :: Int->Gema
laMente debilitar personaje = personaje{energia= (energia personaje) - debilitar}

elAlma :: Habilidad->Gema
elAlma habilidadASacar personaje = personaje{habilidades= sacoHabilidad habilidadASacar (habilidades personaje)}

sacoHabilidad :: Habilidad->[String]->[String]
sacoHabilidad habilidadASacar = filter (not.(== habilidadASacar))

elEspacio :: Gema
elEspacio personaje = personaje{energia = (energia personaje) - 20,planeta = "Endor"}

elPoder :: Gema
elPoder personaje = personaje{energia=0, habilidades = dejoSinHabilidades (habilidades personaje)}

dejoSinHabilidades :: [Habilidad]->[Habilidad]
dejoSinHabilidades listaHabilidades |((<=2).length) listaHabilidades =  [] --si tiene 2 o menos devuelvo lista vacia
                                    |otherwise = listaHabilidades--si tiene mas no lo cambio
elTiempo :: Gema
elTiempo personaje = personaje{edad = reducirEdad (edad personaje)}

reducirEdad :: Int->Int
reducirEdad edad = max 18 (edad `div` 2)

laGemaLoca :: Gema->Gema --la gema que quiero usar se la paso como parametro
laGemaLoca gema = gema.gema

------------------------------------- Punto 4 -------------------------------------
guanteDeGoma = UnGuante "goma" [elTiempo,elAlma "usar Mjolir",laGemaLoca (elAlma "programacion en haskell")]

------------------------------------- Punto 5 -------------------------------------
utilizar :: [Gema]->Personaje->Personaje
utilizar listaGemas enemigo = foldr ($) enemigo listaGemas
--al enemigo se le aplica la primera gema y lo devuelve cambiado.A este resultado se le aplica la segunda gema y asi hasta que 
--el enemigo paso por todas las gemas de la lista,devolviendo entonces al enemigo afectado por cada gema.

------------------------------------- Punto 6 -------------------------------------
gemaMasPoderosa ::Personaje->Guante->Gema
gemaMasPoderosa persona guante = aplicoGemas (gemas guante) persona

aplicoGemas :: [Gema]->Personaje->Gema
aplicoGemas [gema] _ = gema --cuando la lista quede con un solo elemento me quedo con ese.similar a cuando hacia []
aplicoGemas (x:y:xs) persona | (energia.x) persona > (energia.y) persona = aplicoGemas (x:xs) persona
                             |otherwise = aplicoGemas (y:xs) persona

------------------------------------- Punto 7 -------------------------------------
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)
guanteleteDeLocos :: Guante
guanteleteDeLocos = UnGuante "vesconite" (infinitasGemas elTiempo)
usoLasTresPrimerasGemas :: Guante -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete


{-La funcion gemaMasPoderosa punisher guanteleteDeLocos no va a funcionar ya que la funcion va a tener que analizar una lista infinita
de la misma gema y nunca pararia de analizar porque nunca llegaria a una lista con un solo elemento.

La funcion usoLasTresPrimerasGemas guanteleteDeLocos punisher si funcionaria ya que como haskell utiliza el lazy-evaluation,
cuando llegue a la lista infinita de gemas ya sabe que solo tiene que tomar las tres primeras y aplicarselas al personaje que le 
pase como parametro.
-}