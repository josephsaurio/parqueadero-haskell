import Data.Time
import System.Directory (doesFileExist)

data Vehiculo = Vehiculo
  { placa :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime
  }
  deriving (Show, Read)

cargarVehiculos :: FilePath -> IO [Vehiculo]
cargarVehiculos path = do
  fileExists <- doesFileExist path
  if fileExists
    then do
      contenido <- readFile path
      if null contenido
        then return []
        else return (read contenido)
    else do
      putStrLn "Archivo de parqueadero no encontrado. Creando nuevo archivo."
      return []

guardarVehiculos :: FilePath -> [Vehiculo] -> IO ()
guardarVehiculos path vehiculos = writeFile path (show vehiculos)

registrarEntrada :: [Vehiculo] -> String -> IO [Vehiculo]
registrarEntrada vehiculos placaVehiculo = do
  tiempoActual <- getCurrentTime
  let nuevoVehiculo = Vehiculo placaVehiculo tiempoActual Nothing
  let nuevosVehiculos = nuevoVehiculo : vehiculos
  return nuevosVehiculos

registrarSalida :: [Vehiculo] -> String -> IO [Vehiculo]
registrarSalida vehiculos placaVehiculo = do
  tiempoActual <- getCurrentTime
  let actualizar v = if placa v == placaVehiculo then v {salida = Just tiempoActual} else v
  let nuevosVehiculos = map actualizar vehiculos
  return nuevosVehiculos

buscarPorPlaca :: [Vehiculo] -> String -> IO ()
buscarPorPlaca vehiculos placaVehiculo = do
  let vehiculo = filter (\v -> placa v == placaVehiculo) vehiculos
  if null vehiculo
    then putStrLn "El vehículo no se encuentra en el parqueadero."
    else do
      let v = head vehiculo
      putStrLn $ "Vehículo encontrado: " ++ show v
      case salida v of
        Nothing -> do
          tiempoActual <- getCurrentTime
          let tiempoEnParqueadero = diffUTCTime tiempoActual (entrada v)
          putStrLn $ "Tiempo en parqueadero: " ++ show tiempoEnParqueadero ++ " segundos."
        Just _ -> putStrLn "El vehículo ya ha salido del parqueadero."

listarVehiculos :: [Vehiculo] -> IO ()
listarVehiculos vehiculos = mapM_ print vehiculos

main :: IO ()
main = do
  let archivo = "parqueadero.txt"
  vehiculos <- cargarVehiculos archivo
  menu vehiculos archivo

menu :: [Vehiculo] -> FilePath -> IO ()
menu vehiculos archivo = do
  putStrLn "                      "
  putStrLn "Seleccione una opción:"
  putStrLn "1. Registrar entrada de vehículo"
  putStrLn "2. Registrar salida de vehículo"
  putStrLn "3. Buscar vehículo por placa"
  putStrLn "4. Listar vehículos"
  putStrLn "5. Salir"
  putStrLn "                      "
  opcion <- getLine
  case opcion of
    "1" -> do
      putStrLn "Ingrese la placa del vehículo:"
      placaVehiculo <- getLine
      nuevosVehiculos <- registrarEntrada vehiculos placaVehiculo
      guardarVehiculos archivo nuevosVehiculos
      menu nuevosVehiculos archivo
    "2" -> do
      putStrLn "Ingrese la placa del vehículo:"
      placaVehiculo <- getLine
      nuevosVehiculos <- registrarSalida vehiculos placaVehiculo
      guardarVehiculos archivo nuevosVehiculos
      menu nuevosVehiculos archivo
    "3" -> do
      putStrLn "Ingrese la placa del vehículo a buscar:"
      placaVehiculo <- getLine
      buscarPorPlaca vehiculos placaVehiculo
      menu vehiculos archivo
    "4" -> do
      listarVehiculos vehiculos
      menu vehiculos archivo
    "5" -> do
          putStrLn "Gracias por su visita."
          return ()
    _ -> do
      putStrLn "Opción no válida. Por favor, intente de nuevo."
      menu vehiculos archivo