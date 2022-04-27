type Nombre = String
type Precio = Float
type Producto = (Nombre, Precio)

precioTotal :: Producto -> Float -> Float -> Float -> Float
precioTotal producto cantidad descuento costoDeEnvio = aplicarCostoDeEnvio ((aplicarDescuento producto descuento) * cantidad) costoDeEnvio

productoDeElite :: Producto -> Bool
productoDeElite producto = productoDeLujo producto && productoCodiciado producto && (not . productoCorriente) producto

aplicarDescuento :: Producto -> Float -> Float
aplicarDescuento (_, precio) descuento = precio * (descuento / 100)

entregaSencilla :: String -> Bool
entregaSencilla fechaDeEntrega = (even.length) fechaDeEntrega

descodiciarProducto :: Producto -> Producto
descodiciarProducto producto = (take 10 (fst producto), snd producto)

productoDeLujo :: Producto -> Bool
productoDeLujo (nombreProducto, _) = (elem 'x' nombreProducto) || (elem 'z' nombreProducto)

aplicarCostoDeEnvio :: Float -> Float -> Float
aplicarCostoDeEnvio precio costoEnvio = precio + costoEnvio

productoCodiciado :: Producto -> Bool
productoCodiciado (nombreProducto, _) = (length nombreProducto) > 10

productoCorriente :: Producto -> Bool
productoCorriente (nombreProducto, _) = esVocal . head $ nombreProducto

esVocal :: Char -> Bool
esVocal unaLetra = elem unaLetra "aeiouAEIOU"

productoXL :: Producto -> String
productoXL (nombreProducto, _) = nombreProducto ++ "XL"

versionBarata :: Producto -> Producto
versionBarata producto = (reverse . fst . descodiciarProducto $ producto, snd producto)

--------------------------------

-- take :: Int -> String -> String
-- drop :: Int -> String -> String
-- head :: String -> Char
-- elem :: Char -> String -> Bool
-- reverse :: String -> String