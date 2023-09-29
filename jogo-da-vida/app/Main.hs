module Main where

import           Data.Matrix (matrix, fromLists)
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
    let m = fromLists [[1,2], [3,4]]
    putStrLn (prettymatrix m)

       