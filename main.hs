-- 1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada.

soma1 :: Int -> Int
soma1 x = x + 1

-- 2. Escreva uma função chamada sempre que, não importando o valor de entrada, devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo

sempre :: a -> Int
sempre x = 0

-- 3. Escreva uma função chamada treco que receba três valores em ponto flutuantes com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro.

treco :: Double -> Double -> Double -> Double
treco x y z = (x + y) * z

-- 4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros.

resto :: Int -> Int -> Int
resto x y = x `mod` y

-- 5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores monetários.

maior :: Double -> Double -> Double
maior x y
    | x < y = y
    | otherwise = x

precoMaior :: Double -> Double -> Double -> Double -> Double
precoMaior x y z w = maior (maior x y) (maior z w)

-- 6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar. 

impar :: Int -> Int -> Bool
impar x y = (x * y) `mod` 2 == 1

-- 7. Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟 ∷ (𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros.

par :: (Int, Int) -> Int
par (x, y) = x + y

-- 8. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação 𝑥**2 + (𝑦 / 2) + 𝑧.

equacao :: Double -> Double -> Double -> Double
equacao x y z = x**2 + (y / 2) + z

-- Escreva uma função em Haskell chamada diagnostico que receba o peso e a altura do aluno e imprima um diagnóstico de obesidade, segundo a tabela que pode ser encontrada no link: Sobrepeso,  obesidade  e  obesidade  mórbida:  entenda  a  diferença  entre  os  três  termos (cuidadospelavida.com.br).  Observe  que  este  diagnóstico  é  meramente  estatístico  e  não tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. Todo e qualquer diagnóstico deve ser feito por um profissional médico.

diagnostico :: Double -> Double -> String
diagnostico x y
    | x / (y**2) < 17 = "Muito abaixo do peso"
    | x / (y**2) < 18.49 = "Abaixo do peso"
    | x / (y**2) < 24.99 = "Peso normal"
    | x / (y**2) < 29.99 = "Sobrepeso"
    | x / (y**2) < 34.99 = "Obesidade Ieve"
    | x / (y**2) < 39.99 = "Obesidade severa"
    | otherwise = "Obesidade morbida"

-- 10. Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o ano for bisexto sabendo que anos bissextos obedecem a seguinte regra: 𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4 𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100 𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400 1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto.

bissexto :: Int -> Bool
bissexto x
    | x `mod` 400 == 0 = True
    | x `mod` 100 == 0 = False
    | x `mod` 4 == 0 = True
    | otherwise = False

main = do
  putStr "soma1: entrada: 1; resultado: "
  print(soma1 1)
  putStr "sempre: entrada: 'a'; resultado: "
  print(sempre 'a')
  putStr "treco: entrada: 1 2 3; resultado: "
  print(treco 1 2 3)
  putStr "resto: entrada: 5 3; resultado: "
  print(resto 5 3)
  putStr "precoMaior: entrada: 76 34 79 23; resultado: "
  print(precoMaior 76 34 79 23)
  putStr "impar: entrada: 3 2; resultado: "
  print(impar 3 2)
  putStr "par: entrada: (12,2); resultado: "
  print(par (12,2))
  putStr "equacao: entrada: 1 2 3; resultado: "
  print(equacao 1 2 3)
  putStr "diagnostico: entrada: 46; resultado: "
  print(diagnostico 46 1.70)
  putStr "bissexto: entrada: 2024; resultado: "
  print(bissexto 2024)