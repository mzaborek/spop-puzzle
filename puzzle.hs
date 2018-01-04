--install cabal https://www.haskell.org/cabal/download.html
--cabal install matrix
import System.IO
import Data.Matrix
import qualified Data.Vector as V

--zapytanie o nazwę pliku
getFileName = do
    putStrLn ("Type file name")
    input <- getLine
    return input

--wczytanie pliku (dowolny rozmiar planszy)
{-loadFile = do
    fileName <- getFileName
-}

sampleRows = [1, 0, 2, 1, 2, 1]
sampleColumns = [1, 1, 2, 1, 1, 1]
sampleHouses = [(0, 1), (3, 2), (3, 4), (4, 0), (4, 4), (5, 2), (5, 5)]--row, col

sampleData = initPuzzleData sampleRows sampleColumns sampleHouses

tankData = placeElement (0, 0) 'U' sampleData

getMtx (PuzzleData _ _ pdata) = pdata

{-
Reprezentacja danych na planszy
H - domek
X - puste pole
L - zbiornik przyłączony do domku po lewej
R, U, D - analogicznie
-}


{-
wykreśl pola, które nie sąsiadują z żadnym domkiem

domek styka się bokiem z jednym pustym polem -> postaw tam zbiornik z gazem i powykreślaj pola dookoła zbiornika
jeśli w rzędzie jest tyle zbiorników ile w opisie rzędu - poskreślaj puste pola
-}
main =
    displaySolution (findSolution(crossOutNoNeighbour (placeHouses sampleHouses sampleData)))



--znalezienie rozwiązania
findSolution :: PuzzleData -> PuzzleData
findSolution (PuzzleData rows cols pdata)
    | solutionFound (PuzzleData rows cols pdata) = PuzzleData rows cols pdata
    | otherwise = findSolution (placeTanks(crossOutRowsAndCols(PuzzleData rows cols pdata)))

--sprawdza, czy wszystkie zbiorniki zostaly juz umieszczone
solutionFound :: PuzzleData -> Bool
solutionFound (PuzzleData rows cols pdata)
    | rows == [tanksInRow x (PuzzleData rows cols pdata) | x <- [0..(length rows-1)]]= True
    | otherwise = False


--TODO
--przechodzi po macierzy i probuje umiescic zbiorniki
placeTanks :: PuzzleData -> PuzzleData
placeTanks (PuzzleData rows cols pdata) = PuzzleData rows cols pdata

--umieszcza na zadanym wspolrzednych dany zbiornik i wykresla wolne pola dookola
placeTank :: (Int, Int) -> Char -> PuzzleData -> PuzzleData
placeTank position element pdata =
    (placeElement position element (crossOutAround position pdata))

crossOutAround :: (Int, Int) -> PuzzleData -> PuzzleData
crossOutAround (row, col) pdata =
    (crossOutIfEmpty(row+1, col+1)
    (crossOutIfEmpty(row+1, col)
    (crossOutIfEmpty(row+1, col-1)
    (crossOutIfEmpty(row, col+1)
    (crossOutIfEmpty(row, col-1)
    (crossOutIfEmpty(row-1, col-1)
    (crossOutIfEmpty(row-1, col)
    (crossOutIfEmpty (row-1, col+1) pdata
    ))))))))

crossOutIfEmpty :: (Int, Int) -> PuzzleData -> PuzzleData
crossOutIfEmpty position pdata
    | getElement position pdata == Just ' ' = placeElement position 'X' pdata
    | otherwise = pdata

--dla kazdej kolumny/rzedu - jesli liczba zbiornikow jest rowna docelowej liczbie - wykreslone zostaja pozostale puste miejsca
crossOutRowsAndCols :: PuzzleData -> PuzzleData
crossOutRowsAndCols (PuzzleData rows cols pdata) = crossOutRowsAndCols' (length rows - 1) (length cols - 1) (PuzzleData rows cols pdata)
crossOutRowsAndCols' :: Int -> Int -> PuzzleData -> PuzzleData
crossOutRowsAndCols' row col (PuzzleData rows cols pdata)
    | row < 0 = (PuzzleData rows cols pdata)
    | col < 0 && rows!!row == (tanksInRow row (PuzzleData rows cols pdata)) = crossOutRowsAndCols' (row-1) col (crossOutRow row (PuzzleData rows cols pdata))
    | col < 0 = crossOutRowsAndCols' (row-1) col (PuzzleData rows cols pdata)
    | cols!!col == (tanksInCol col (PuzzleData rows cols pdata)) = crossOutRowsAndCols' row (col-1) (crossOutColumn col  (PuzzleData rows cols pdata))
    | otherwise = crossOutRowsAndCols' row (col-1)  (PuzzleData rows cols pdata)

tanksInRow :: Int -> PuzzleData -> Int
tanksInRow row (PuzzleData rows cols pdata) =
    length (V.filter isTank (getRow (row+1) pdata))

tanksInCol :: Int -> PuzzleData -> Int
tanksInCol col (PuzzleData rows cols pdata) =
    length (V.filter isTank (getCol (col+1) pdata))

isTank :: Char -> Bool
isTank elem
    | elem == 'D' = True
    | elem == 'U' = True
    | elem == 'L' = True
    | elem == 'R' = True
    |otherwise = False

crossOutColumn :: Int -> PuzzleData -> PuzzleData
crossOutColumn col (PuzzleData rows cols pdata) = crossOutColumn' (length rows -1) col (PuzzleData rows cols pdata)--Length od dobrej wartosci?

crossOutColumn' :: Int -> Int -> PuzzleData -> PuzzleData
crossOutColumn' row col pdata
    | row == -1 = pdata
    | getElement (row, col) pdata == Just ' ' = placeElement (row, col) 'X' (crossOutColumn' (row-1) col pdata)
    | otherwise = crossOutColumn' (row-1) col pdata

crossOutRow :: Int -> PuzzleData -> PuzzleData
crossOutRow row (PuzzleData rows cols pdata) = crossOutRow' row (length cols -1) (PuzzleData rows cols pdata)--Length od dobrej wartosci?

crossOutRow' :: Int -> Int -> PuzzleData -> PuzzleData
crossOutRow' row col pdata
    | col == -1 = pdata
    | getElement (row, col) pdata == Just ' ' = placeElement (row, col) 'X' (crossOutRow' row (col-1) pdata)
    | otherwise = crossOutRow' row (col-1) pdata

crossOutNoNeighbour :: PuzzleData -> PuzzleData
crossOutNoNeighbour (PuzzleData rows cols pdata) = crossOutNoNeighbour' (length rows-1) (length cols-1) (PuzzleData rows cols pdata)--Length od dobrej wartosci?

crossOutNoNeighbour' :: Int -> Int -> PuzzleData -> PuzzleData
crossOutNoNeighbour' row col (PuzzleData rows cols pdata)
    | col == -1 = PuzzleData rows cols pdata
    | row == -1 = crossOutNoNeighbour' (length rows-1) (col-1) (PuzzleData rows cols pdata)--Length od dobrej wartosci?
    | checkCrossOut row col (PuzzleData rows cols pdata) = placeElement (row, col) 'X' (crossOutNoNeighbour' (row -1) col (PuzzleData rows cols pdata))
    | otherwise = crossOutNoNeighbour' (row - 1) col (PuzzleData rows cols pdata)

checkCrossOut  :: Int -> Int -> PuzzleData -> Bool
checkCrossOut row col pdata
    | Just ' ' /= getElement (row, col) pdata = False
    | Just 'H' == getElement (row, (col - 1)) pdata = False
    | Just 'H' == getElement (row, (col + 1)) pdata = False
    | Just 'H' == getElement ((row - 1), col) pdata = False
    | Just 'H' == getElement ((row + 1), col) pdata = False
    | otherwise = True

--wyświetlenie rozwiązania
displaySolution :: PuzzleData -> IO()
displaySolution (PuzzleData _ _ pdata) = do putStrLn $ prettyMatrix pdata

--możliwość zapisania rozwiązania do pliku o podanej nazwie
--saveToFile

data PuzzleData = PuzzleData [Int][Int](Matrix Char)
initPuzzleData :: [Int] -> [Int] -> [(Int, Int)] -> PuzzleData
initPuzzleData a b h =
    PuzzleData a b (fromList (length b) (length a) [' ', ' '..])--Length od dobrej wartosci?

placeHouses :: [(Int, Int)] -> PuzzleData -> PuzzleData
placeHouses [] pdata = pdata
placeHouses (x:xs) pdata =
    placeHouses xs (placeElement x 'H' pdata)

placeElement :: (Int, Int) -> Char -> PuzzleData -> PuzzleData
placeElement (row, col) elem (PuzzleData rows cols pdata)
--    PuzzleData a b (setElem elem (row+1, col+1) pdata)
    | row >= 0 && col >= 0 && row < (nrows pdata) && col < (ncols pdata) = PuzzleData rows cols (setElem elem (row+1, col+1) pdata)
    | otherwise = (PuzzleData rows cols pdata)

getElement :: (Int, Int) -> PuzzleData -> Maybe Char
getElement (row, col) (PuzzleData _ _ pdata) = safeGet (row+1) (col+1) pdata