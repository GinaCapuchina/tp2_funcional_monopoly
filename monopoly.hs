import Text.Show.Functions()
import Data.List ()
{-Carolina y Manuel son participantes del juego y tienen las siguientes características:

            -Su nombre.
            -Una cantidad de dinero.
            -Su táctica de juego.
            -Sus propiedades compradas, de las cuales sabemos su nombre y su precio.
            -Sus acciones a lo largo del juego.
-}

type Propiedad = (String,Float)
type Dinero = (Num a)

type Tactica= String

data Jugador = Jugador {
            nombre::String,
            dinero::Dinero,
            tactica::Tactica,
            propiedades::[Propiedad],
            acciones:: [Acciones]
} deriving (Show)

--Modelar Acciones
--recibe un jugador y devuelve un jugador modificado
type Acciones = Jugador -> Jugador

pasarPorElBanco:: Acciones
pasarPorElBanco jugador = jugador{ 
                dinero = dinero jugador + 40,
                tactica= "Comprador compulsivo"


 }

enojarse:: Acciones
enojarse jugador =  jugador{
            dinero = dinero jugador + 50,
            acciones= gritar : acciones jugador 


  }

gritar:: Acciones 
gritar jugador = jugador{
                nombre = "AHHHH" ++ nombre jugador 
}
-------------------------------------------------------------------------------

subastar:: Propiedad -> Acciones
subastar  unaPropiedad jugador
                            | esTacticaSubastadora (tactica jugador)  = ganaPropiedad unaPropiedad jugador
                            | otherwise= jugador

--utilizando pattern matching--
esTacticaSubastadora:: Tactica -> Bool
esTacticaSubastadora "Oferente singular"= True
esTacticaSubastadora "Accionista" = True
esTacticaSubastadora _ = False

--Accessors--

precioPropiedad::Propiedad -> Float
precioPropiedad(_,precio)=precio

ganaPropiedad:: Propiedad ->Acciones
ganaPropiedad nuevaPropiedad jugador = jugador {
                                dinero = dinero jugador - precioPropiedad nuevaPropiedad,
                                propiedades= nuevaPropiedad : propiedades jugador

}

-----------------------------------------------------------------------------------
cobraAlquileres:: Acciones
cobraAlquileres jugador 
                  | map esPropiedadBarata (propiedades jugador)= sumarBarata jugador
                  | otherwise =   sumarCaras jugador
                     
                     

sumarBarata:: Acciones
sumarBarata jugador= jugador {

              dinero = dinero jugador + (cantPropiedadesBaratas (propiedades jugador)* 10)

}

sumarCaras:: Acciones
sumarCaras  jugador = jugador{
            dinero = dinero jugador + (cantPropiedadesCaras (propiedades jugador) *20)


}

cantPropiedadesCaras::[Propiedad] -> Int
cantPropiedadesCaras lstpropiedades = (length.filter esPropiedadCaras )lstpropiedades
esPropiedadCaras::Propiedad -> Bool
esPropiedadCaras propiedad = ((150<).precioPropiedad)propiedad


cantPropiedadesBaratas::[Propiedad] -> Int
cantPropiedadesBaratas lstpropiedades = (length.filter esPropiedadBarata )lstpropiedades
esPropiedadBarata::Propiedad -> Bool
esPropiedadBarata propiedad = ((150<).precioPropiedad)propiedad
 --pagarAAccionistas -> No pude hacerlo :(

 -------------------------------------------

-- Modelando a Carolina y a Manuel----------------------
carolina = Jugador {
            nombre= "Carolina",
            dinero=500,
            tactica = "Accionista",
            propiedades =[],
            acciones = [pagarAAccionistas]



}

manuel= Jugador{
            nombre= "Manuel",
            dinero=500,
            tactica = "Oferente singular",
            propiedades =[],
            acciones = [enojarse]

         
}

--Me falto modela Acciones y Propiedades, como se hace??


