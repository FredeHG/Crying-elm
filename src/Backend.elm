module Backend exposing(..)
import Models exposing(Movie, Preferences)

completaAca = identity

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- **************
hacerLower : String -> String
hacerLower = String.toLower
filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = List.filter (peliculaTienePalabrasClave palabras)

-- esta función la dejamos casi lista, pero tiene un pequeño bug. ¡Corregilo!
--
-- Además tiene dos problemas, que también deberías corregir:

--  La pelicula tiene que tener todas las palabras claves. recibo peliculas y las analizo de a una

-- * distingue mayúsculas de minúsculas, pero debería encontrar a "Lion King" aunque escriba "kINg"
-- * busca una coincidencia exacta, pero si escribís "Avengers Ultron" debería encontrar a "Avengers: Age Of Ultron"
--
peliculaTienePalabrasClave : String -> Movie -> Bool
peliculaTienePalabrasClave palabras pelicula = List.all (estaEnTitulo pelicula) ((String.words<<hacerLower) palabras)
estaEnTitulo : Movie -> String -> Bool
estaEnTitulo pelicula palabra = String.contains palabra (hacerLower pelicula.title)
-- **************
-- Requerimiento: visualizar las películas según el género elegido en un selector;
-- **************

filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero = List.filter (peliculaDelGenero genero)

peliculaDelGenero : String -> Movie -> Bool
peliculaDelGenero genero pelicula = if genero == "All" then True 
  else List.any ((==)  (hacerLower genero)) (List.map hacerLower (pelicula.genre))

-- **************
-- Requerimiento: filtrar las películas que sean aptas para menores de edad,
--                usando un checkbox;
-- **************

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores peliculas = if mostrarSoloMenores then peliculasParaMenores peliculas else peliculas

peliculasParaMenores : List Movie -> List Movie
peliculasParaMenores peliculas = List.filter (.forKids) peliculas


-- **************
-- Requerimiento: ordenar las películas por su rating;
-- **************

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating = List.foldl (::) []<<List.sortBy .rating

-- **************
-- Requerimiento: dar like a una película
-- **************

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula identificador = List.map (darLike identificador)

darLike : Int -> Movie -> Movie
darLike identificador pelicula = if identificador == pelicula.id
    then {pelicula | likes = pelicula.likes + 1}
    else pelicula

-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias = List.map (modificarPorcentaje preferencias)

modificarPorcentaje : Preferences -> Movie -> Movie
modificarPorcentaje preferencias pelicula = {pelicula | matchPercentage = min 100 (newPorcentage preferencias pelicula)}

newPorcentage : Preferences -> Movie -> Int
newPorcentage preferencias pelicula = (keyWord preferencias.keywords pelicula) + (generoPredilecto preferencias.genre pelicula)+ (actorFavorito preferencias.favoriteActor pelicula)

keyWord : String->Movie->Int
keyWord palabras pelicula= (((*)20)<<List.length<<List.filter (estaEnTitulo pelicula)) ((String.words<<hacerLower) palabras)

generoPredilecto : String->Movie->Int
generoPredilecto genero pelicula = if peliculaDelGenero genero pelicula then 60 else 0

actorFavorito : String -> Movie -> Int
actorFavorito actor pelicula = 0
