import Text.Show.Functions()
import Data.List ()

-- Modelando a Carolina y a Manuel----------------------
type Propiedad = (String,Int)

type Tactica= String

data Jugador = Jugador {
            nombre::String,
            dinero::Int,
            tactica::Tactica,
            propiedades::[Propiedad],
            acciones:: [Accion]
} deriving (Show)

carolina::Jugador
carolina = Jugador {
            nombre= "Carolina",
            dinero=500,
            tactica = "Accionista",
            propiedades =[],
            acciones = [pasarPorElBanco, pagarAccionistas] 


}

manuel::Jugador
manuel= Jugador{
            nombre= "Manuel",
            dinero=500,
            tactica = "Oferente singular",
            propiedades =[],
            acciones = [enojarse, pasarPorElBanco]

         
}


--Modelar Acciones
--recibe un jugador y devuelve un jugador modificado
type Accion = Jugador -> Jugador

pasarPorElBanco:: Accion
pasarPorElBanco jugador = jugador{ 
                dinero = agregarDinero (+40) jugador,
                tactica= "Comprador compulsivo"


 }

enojarse:: Accion
enojarse jugador =  jugador{
            dinero = agregarDinero (+50) jugador,
            acciones= gritar : acciones jugador 


  }
---Funcion Aux--------------
agregarDinero:: (Int -> Int) -> Jugador->Int
agregarDinero  funcion jugador = (funcion.dinero)jugador   
-----------------------------------------------------------------
gritar:: Accion 
gritar jugador = jugador{
                nombre = "AHHHH" ++ nombre jugador 
}
-------------------------------------------------------------------------------

subastar:: Propiedad -> Accion
subastar  unaPropiedad jugador
                            | esTacticaSubastadora (tactica jugador)  = ganaPropiedad unaPropiedad jugador
                            | otherwise= jugador

--utilizando pattern matching--
esTacticaSubastadora:: Tactica -> Bool
esTacticaSubastadora "Oferente singular"= True
esTacticaSubastadora "Accionista" = True
esTacticaSubastadora _ = False

--Accessors--

precioPropiedad::Propiedad -> Int
precioPropiedad(_,precio)= precio

ganaPropiedad:: Propiedad ->Accion
ganaPropiedad nuevaPropiedad jugador = jugador {
                                dinero = dinero jugador - precioPropiedad nuevaPropiedad,
                                propiedades= nuevaPropiedad : propiedades jugador

}

-----------------------------------------------------------------------------------
cobraAlquileres:: Accion
cobraAlquileres jugador = jugador{
                          dinero = dinero jugador + sumaDeAlquileres (propiedades jugador)


}

sumaDeAlquileres:: [Propiedad] -> Int
sumaDeAlquileres propiedades' = (sum.map precioDeAlquiler) propiedades'

precioDeAlquiler::Propiedad->Int
precioDeAlquiler propiedad
                          |precioPropiedad propiedad< 150 = 10
                          |otherwise = 20

--------------------------------------------------------------------------
pagarAccionistas::Accion
pagarAccionistas jugador 
            |(esAccionista.tactica)jugador= jugador{dinero= dinero jugador + 200} 
 
            |otherwise = jugador{dinero= dinero jugador - 100} 

esAccionista :: Tactica -> Bool
esAccionista tactica' = (== "Accionista") tactica' 

-----------------------------------------------------------------------------
hacerBerrinchePor::Propiedad -> Accion
hacerBerrinchePor propiedad jugador
              |puedoComprarPropiedad propiedad jugador= comprarPropiedad propiedad jugador
              |otherwise= hacerBerrinchePor propiedad ((gritar.modificarDinero)jugador)
               
                         

modificarDinero::Accion
modificarDinero jugador = jugador{
                          dinero = dinero jugador + 10

}

puedoComprarPropiedad::Propiedad->Jugador->Bool
puedoComprarPropiedad propiedad jugador =  precioPropiedad  propiedad < dinero jugador 

comprarPropiedad:: Propiedad -> Accion
comprarPropiedad propiedad jugador = jugador{

                      propiedades = propiedad : propiedades jugador  

}

 -------------------------------------------
--ultimaRonda:: Jugador -> Accion
--ultimaRonda 

--El ganador es:-----------------------------------------
juegoFinal:: Jugador -> Jugador -> Jugador
juegoFinal j1 j2 
          | dinero j1 > dinero j2 = j1
          | otherwise = j2