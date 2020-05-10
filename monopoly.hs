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

              dinero = dinero jugador + (cantPropiedadesBaratas * 10)

}

sumarCaras:: Acciones
sumarCaras jugador = jugador{
            dinero = dinero jugador + (cantPropiedadesCaras *20)


}

cantPropiedadesCaras::[Propiedad] -> Int
cantPropiedadesCaras propiedades = (length.filter esPropiedadCaras )propiedades
esPropiedadCaras::[Propiedad] -> Bool
esPropiedadCaras propiedades = ((150<).precioPropiedad)propiedades


cantPropiedadesBaratas::[Propiedad] -> Int
cantPropiedadesBaratas propiedades = (length.filter esPropiedadBarata )propiedades
esPropiedadBarata::[Propiedad] -> Bool
esPropiedadBarata propiedades = ((150<).precioPropiedad)propiedades


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


