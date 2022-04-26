type Nombre = String
type Precio = Float
type Producto = (Nombre, Precio)

precioTotal :: Producto -> Float -> Float -> Float -> Float
precioTotal (_, precio) cantidad descuento costoDeEnvio = aplicarCostoDeEnvio (aplicarDescuento precio descuento * cantidad) costoDeEnvio

productoDeElite :: String -> Bool
productoDeElite = productoDeLujo nombreProducto && productoCodiciado nombreProducto && (not . productoCorriente) nombreProducto

aplicarDescuento :: Producto -> Float -> Float
aplicarDescuento (_, precio) descuento = precio * (descuento / 100)

entregaSencilla :: String -> Bool
entregaSencilla fechaDeEntrega = (even.length) fechaDeEntrega

descodiciarProducto :: Producto -> String
descodiciarProducto (nombreProducto, _) = take 10 nombreProducto

productoDeLujo :: Producto -> Bool
productoDeLujo (nombreProducto, _) = (elem 'x' nombreProducto) || (elem 'z' nombreProducto)

aplicarCostoDeEnvio :: Producto -> Float -> Float
aplicarCostoDeEnvio (_, precio) costoEnvio = precio + costoEnvio

productoCodiciado :: Producto -> Bool
productoCodiciado (nombreProducto, _) = (length nombreProducto) > 10

productoCorriente :: Producto -> Bool
productoCorriente (nombreProducto, _) = esVocal . head $ nombreProducto

esVocal :: Char -> Bool
esVocal unaLetra = elem unaLetra "aeiouAEIOU"

productoXL :: Producto -> String
productoXL (nombreProducto, _) = nombreProducto ++ "XL"

versionBarata :: Producto -> String
versionBarata (nombreProducto, _) = reverse . descodiciarProducto $ nombreProducto

--------------------------------

-- take :: Int -> String -> String
-- drop :: Int -> String -> String
-- head :: String -> Char
-- elem :: Char -> String -> Bool
-- reverse :: String -> String