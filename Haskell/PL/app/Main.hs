--Prelude> 1+2
--main = putStrLn "Hello, World!"
main = print (fac 20)

fac 0 = 1
fac n = n * fac (n-1)