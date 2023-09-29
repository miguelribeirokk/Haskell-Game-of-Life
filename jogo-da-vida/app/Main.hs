module Main where

import           Data.Matrix (Matrix, fromLists, prettyMatrix)
import qualified Data.Matrix as M
import           Data.Maybe (fromMaybe)

lerInicial :: String -> IO (Matrix Bool)
lerInicial fp = do
        texto <- readFile fp 
        let linhas = lines texto 
        let booleanos = map (map(=='#')) linhas
        return (fromLists booleanos)

ataulizar :: MatrixBool -> Matrix Bool
atualizar m = M.mapPos () m

atualizarCelula :: MatrixBool -> (Int, Int) -> Bool -> Bool
atualizarCelula m (i,j) v =(v && (vivos ==2 || vivos ==3)) || vivos == 3

atualizarCelula m(i,j) v=v
        where
                deslocamentos =[(-1,-1), (-1,0), (-1,1),
                                (0,-1)           (0, 1),
                                (1,-1), (1,0),   (1, 1)]
                vizinhos = map (\(di,dj)-> M.safeGet (i+di) (j+dj) m) deslocamentos 
                vizinhos' = map (fromMaybe False) vizinhos
                vivos = lenght (filter (\x -> x) vizinhos')
 main :: IO() 
 main = do  
    let fp = "../teste.txt"
    m <- lerInicial fp
    putStrLn ""
    where 
        run m = do
                putStrl (prettyMatrix m)
                run (atualizar m)

