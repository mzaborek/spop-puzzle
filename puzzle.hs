--install cabal https://www.haskell.org/cabal/download.html
--cabal install matrix
import System.IO
import System.Directory
import Data.Matrix
import qualified Data.Vector as V

getMtx (PuzzleData _ _ pdata) = pdata

{-
Reprezentacja danych na planszy
H - domek
X - puste pole
D - zbiornik przyłączony do domku po lewej
-}

main = do
    putStrLn "Type input file name:"
    inputFileName <- getLine
    fileExists <- doesFileExist inputFileName --IO Bool - dlaczego w ten sposób jest ok?
    if (fileExists)
        then do
            inputData <- readFile inputFileName
            putStrLn "File loaded successfully:"
            putStrLn inputData

            let dataLines = lines inputData

            let inputRows = read (dataLines!!0) :: [Int]
            let inputCols = read (dataLines!!1) :: [Int]
            let inputHouses = read (dataLines!!2) :: [(Int,Int)]
            let inputData = initPuzzleData inputRows inputCols inputHouses
            displaySolution inputData

            putStrLn "Finding a solution"
            let solutionData = (findSolution (crossOutNoNeighbour inputData))
            displaySolution solutionData
            
            putStrLn "Save solution to file:"
            outputFileName <- getLine
            saveFile outputFileName solutionData

        else do
            putStrLn ("File \"" ++ inputFileName ++ "\" not found")
            
    
saveFile :: FilePath -> PuzzleData -> IO() 
saveFile fileName puzzle = do writeFile fileName (puzzleAsString puzzle)

puzzleAsString :: PuzzleData -> [Char]
puzzleAsString (PuzzleData rows cols pdata) =
    concat (map show cols) ++ "\n" ++ puzzleAsString' rows (toLists pdata)

puzzleAsString' :: [Int] -> [[Char]] -> [Char]
puzzleAsString' [] _ = []
puzzleAsString' (r:rx) (row:x) = row ++ show r ++ "\n" ++ puzzleAsString' rx x


--znalezienie rozwiązania
findSolution :: PuzzleData -> PuzzleData
findSolution (PuzzleData rows cols pdata)
    | solutionFound (PuzzleData rows cols pdata) || not (emptyFieldsLeft (PuzzleData rows cols pdata)) = crossOutRowsAndCols (PuzzleData rows cols pdata)
    | (PuzzleData rows cols pdata) == crossOutRowsAndCols(tryEmptyFields(placeTanks(crossOutRowsAndCols(PuzzleData rows cols pdata)))) && solutionFound (findSolution(placeFirstEmptyField 'D' (PuzzleData rows cols pdata))) = findSolution(placeFirstEmptyField 'D' (PuzzleData rows cols pdata))
    | (PuzzleData rows cols pdata) == crossOutRowsAndCols(tryEmptyFields(placeTanks(crossOutRowsAndCols(PuzzleData rows cols pdata)))) && solutionFound (findSolution(placeFirstEmptyField 'x' (PuzzleData rows cols pdata))) = findSolution(placeFirstEmptyField 'x' (PuzzleData rows cols pdata))                                                                                                                                                              
    | (PuzzleData rows cols pdata) == crossOutRowsAndCols(tryEmptyFields(placeTanks(crossOutRowsAndCols(PuzzleData rows cols pdata)))) = (PuzzleData rows cols pdata)
    | otherwise = findSolution (crossOutRowsAndCols(tryEmptyFields (placeTanks (crossOutRowsAndCols (PuzzleData rows cols pdata)))))
--jak brak zmian, to wstaw w puste miejsce, jak się nie udało, to się cofnij i szukaj rozwiązania dalej

--sprawdza, czy wszystkie zbiorniki zostaly juz umieszczone
solutionFound :: PuzzleData -> Bool
solutionFound (PuzzleData rows cols pdata)
    | rows == [tanksInRow x (PuzzleData rows cols pdata) | x <- [0..(length rows-1)]] = True
    | otherwise = False

--sprawdź, czy są puste pola
emptyFieldsLeft :: PuzzleData -> Bool
emptyFieldsLeft puzzle = 
    length (filter (== ' ') (toList (getMtx puzzle))) /= 0

--umieszcza znak w pierwszym pustym polu
placeFirstEmptyField :: Char -> PuzzleData -> PuzzleData
placeFirstEmptyField char (PuzzleData rows cols pdata) =
    placeFirstEmptyField' ((length rows)-1) ((length cols)-1) char (PuzzleData rows cols pdata)

placeFirstEmptyField' :: Int -> Int -> Char -> PuzzleData -> PuzzleData
placeFirstEmptyField' row col char (PuzzleData rows cols pdata)
    | row < 0 = (PuzzleData rows cols pdata)
    | col < 0 = placeFirstEmptyField' (row-1) ((length cols)-1) char (PuzzleData rows cols pdata)
    | (getElement (row, col) (PuzzleData rows cols pdata)) == Just ' ' && isTank char = placeTank (row, col) char (PuzzleData rows cols pdata)
    | (getElement (row, col) (PuzzleData rows cols pdata)) == Just ' ' = placeElement (row, col) char (PuzzleData rows cols pdata)
    | otherwise = placeFirstEmptyField' row (col-1) char (PuzzleData rows cols pdata)

--przechodzi po macierzy i probuje umiescic zbiorniki
placeTanks :: PuzzleData -> PuzzleData
placeTanks (PuzzleData rows cols pdata)
    = placeTanks' ((length rows)-1) ((length cols)-1) (PuzzleData rows cols pdata)

tryEmptyFields :: PuzzleData -> PuzzleData
tryEmptyFields (PuzzleData rows cols pdata) =
    tryEmptyFields' ((length rows)-1) ((length cols)-1) (PuzzleData rows cols pdata)

tryEmptyFields' :: Int -> Int -> PuzzleData -> PuzzleData
tryEmptyFields' row col (PuzzleData rows cols pdata)
    | row < 0 = (PuzzleData rows cols pdata)
    | col < 0 = tryEmptyFields' (row-1) ((length cols) - 1) (tryToPlace row 0 (PuzzleData rows cols pdata)) 
    | otherwise = tryEmptyFields' row (col-1) (tryToPlace row col (PuzzleData rows cols pdata)) 

--jeśli puste pole, nie sąsiaduje ze zbiornikiem, ma domek obok i można wstawić (kolumny i wiersze), to wstawia zbiornik
tryToPlace :: Int -> Int -> PuzzleData -> PuzzleData
tryToPlace row col (PuzzleData rows cols pdata)
    | ((getElement (row, col) (PuzzleData rows cols pdata)) == Just ' ') && noGasAround row col (PuzzleData rows cols pdata) && oneHouseToConnect row col (PuzzleData rows cols pdata) && cols!!col > tanksInCol col (PuzzleData rows cols pdata) && rows!!row > tanksInRow row (PuzzleData rows cols pdata) = (placeTank (row, col) 'D' (PuzzleData rows cols pdata)) 
    | otherwise = (PuzzleData rows cols pdata)

noGasAround row col pdata
    =not (isMaybeTank(getElement (row+1, col-1) pdata)) &&  not (isMaybeTank(getElement (row+1, col) pdata)) && not (isMaybeTank(getElement (row+1, col+1) pdata)) && not (isMaybeTank(getElement (row, col-1) pdata)) &&  not (isMaybeTank(getElement (row, col+1) pdata)) && not (isMaybeTank(getElement (row-1, col-1) pdata)) &&  not (isMaybeTank(getElement (row-1, col) pdata)) && not (isMaybeTank(getElement (row-1, col+1) pdata))

oneHouseToConnect :: Int -> Int -> PuzzleData -> Bool    
oneHouseToConnect row col pdata
        = (length (filter (== Just 'H')[getElement (row+1, col) pdata, getElement(row-1, col) pdata, getElement(row, col+1) pdata, getElement(row, col-1) pdata])) == 1 && (emptyAround (houseRow, houseCol) pdata) == 1 
        where
            houseRow =  if((getElement (row, col+1) pdata) == Just 'H' || (getElement (row, col-1) pdata) == Just 'H')
                            then row
                            else if ((getElement (row-1, col) pdata) == Just 'H')
                                then row-1
                                else row + 1
            houseCol =  if((getElement (row+1, col) pdata) == Just 'H' || (getElement (row-1, col) pdata) == Just 'H')
                            then col
                            else if ((getElement (row, col-1) pdata) == Just 'H')
                                then col-1
                                else col + 1


isMaybeTank :: Maybe Char -> Bool
isMaybeTank elem 
    | elem == Just 'D' = True
    | elem == Just 'U' = True
    | elem == Just 'L' = True
    | elem == Just 'R' = True
    |otherwise = False

placeTanks' :: Int-> Int -> PuzzleData -> PuzzleData
placeTanks' row col (PuzzleData rows cols pdata)
    | col < 0 = (PuzzleData rows cols pdata)
    | row < 0 = placeTanks' (row) (col-1) (placeTanksInCol col (PuzzleData rows cols pdata))
    |otherwise = placeTanks' (row-1) (col) (placeTanksInRow row (PuzzleData rows cols pdata))

--jesli tyle samo wolnych miejs, co powinno byc zbiornikow - umiesc zbiornik na kazdym miejscu wolnym
--jesli sasiaduja one tylko z jednym domkiem - podepnij go do niego, jesli z wiecej niz jednym - podepnij do takiego, ktory moze miec zbiornik tylko w jednym miejscu lub zostaw niepodpiety
placeTanksInRow :: Int -> PuzzleData -> PuzzleData
placeTanksInRow row (PuzzleData rows cols pdata)
    | rows!!(row) == length (V.filter (==' ') (getRow (row+1) pdata)) = placeTanksInRow' 0 row (PuzzleData rows cols pdata)
    | otherwise = PuzzleData rows cols pdata

--dziala okej
placeTanksInRow' :: Int -> Int -> PuzzleData -> PuzzleData
placeTanksInRow' elem row (PuzzleData rows cols pdata)
    | elem == (length cols) = (PuzzleData rows cols pdata)
    | (getElement (row, elem) (PuzzleData rows cols pdata)) == Just ' ' = placeTanksInRow' (elem+1) row (placeTank (row, elem) 'R' (PuzzleData rows cols pdata))
    | otherwise = placeTanksInRow' (elem+1) row (PuzzleData rows cols pdata)

--czy dziala? chyba
placeTanksInCol :: Int -> PuzzleData -> PuzzleData
placeTanksInCol col (PuzzleData rows cols pdata)
    | cols!!(col) == length (V.filter (==' ') (getCol (col+1) pdata)) = placeTanksInCol' 0 col (PuzzleData rows cols pdata)
    | otherwise = PuzzleData rows cols pdata

--dziala okej
placeTanksInCol' :: Int -> Int -> PuzzleData -> PuzzleData
placeTanksInCol' elem col (PuzzleData rows cols pdata)
    | elem == (length rows) = (PuzzleData rows cols pdata)
    | (getElement (elem, col) (PuzzleData rows cols pdata)) == Just ' ' = placeTanksInCol' (elem+1) col (placeTank (elem, col) 'L' (PuzzleData rows cols pdata))
    | otherwise = placeTanksInCol' (elem+1) col (PuzzleData rows cols pdata)

--ile pustych pol jest dookola (po bokach? - jesli 1 i domek nie jest podloczony, to tylko tam moze miec podpiety zbiornik)
emptyAround :: (Int, Int) -> PuzzleData -> Int
emptyAround (row, col) pdata =
    length (filter (== Just ' ') [getElement (row+x, col+y) pdata| x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)])

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
    | getElement position pdata == Just ' ' = placeElement position 'x' pdata
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
crossOutColumn col (PuzzleData rows cols pdata) = crossOutColumn' (length rows -1) col (PuzzleData rows cols pdata)

crossOutColumn' :: Int -> Int -> PuzzleData -> PuzzleData
crossOutColumn' row col pdata
    | row == -1 = pdata
    | getElement (row, col) pdata == Just ' ' = placeElement (row, col) 'x' (crossOutColumn' (row-1) col pdata)
    | otherwise = crossOutColumn' (row-1) col pdata

crossOutRow :: Int -> PuzzleData -> PuzzleData
crossOutRow row (PuzzleData rows cols pdata) = crossOutRow' row (length cols -1) (PuzzleData rows cols pdata)

crossOutRow' :: Int -> Int -> PuzzleData -> PuzzleData
crossOutRow' row col pdata
    | col == -1 = pdata
    | getElement (row, col) pdata == Just ' ' = placeElement (row, col) 'x' (crossOutRow' row (col-1) pdata)
    | otherwise = crossOutRow' row (col-1) pdata

crossOutNoNeighbour :: PuzzleData -> PuzzleData
crossOutNoNeighbour (PuzzleData rows cols pdata) = crossOutNoNeighbour' (length rows-1) (length cols-1) (PuzzleData rows cols pdata)

crossOutNoNeighbour' :: Int -> Int -> PuzzleData -> PuzzleData
crossOutNoNeighbour' row col (PuzzleData rows cols pdata)
    | col == -1 = PuzzleData rows cols pdata
    | row == -1 = crossOutNoNeighbour' (length rows-1) (col-1) (PuzzleData rows cols pdata)
    | checkCrossOut row col (PuzzleData rows cols pdata) = placeElement (row, col) 'x' (crossOutNoNeighbour' (row -1) col (PuzzleData rows cols pdata))
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
displaySolution (PuzzleData rows cols pdata) = 
    do 
        putStrLn $ show (concat (map show cols))        
        displaySolution' rows (toLists pdata)

displaySolution' [] _ = return ()
displaySolution' (r:rx) (b:bx) = do
    putStr (show r)
    putStrLn b
    displaySolution' rx bx

data PuzzleData = PuzzleData [Int][Int](Matrix Char) deriving (Eq)
initPuzzleData :: [Int] -> [Int] -> [(Int, Int)] -> PuzzleData
initPuzzleData a b h =
    placeHouses h (PuzzleData a b (fromList (length a) (length b) [' ', ' '..]))

placeHouses :: [(Int, Int)] -> PuzzleData -> PuzzleData
placeHouses [] pdata = pdata
placeHouses (x:xs) pdata =
    placeHouses xs (placeElement x 'H' pdata)

placeElement :: (Int, Int) -> Char -> PuzzleData -> PuzzleData
placeElement (row, col) elem (PuzzleData rows cols pdata)
    | row >= 0 && col >= 0 && row < (nrows pdata) && col < (ncols pdata) = PuzzleData rows cols (setElem elem (row+1, col+1) pdata)
    | otherwise = (PuzzleData rows cols pdata)

getElement :: (Int, Int) -> PuzzleData -> Maybe Char
getElement (row, col) (PuzzleData _ _ pdata) = safeGet (row+1) (col+1) pdata