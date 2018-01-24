--install cabal https://www.haskell.org/cabal/download.html
--cabal install matrix
import System.IO
import System.Directory
import Data.Matrix
import qualified Data.Vector as V



--wczytanie pliku (dowolny rozmiar planszy)
{-loadFile = do
    fileName <- getFileName
-}
{-
sampleRows = [1, 0, 2, 1, 2, 1]
sampleColumns = [1, 1, 2, 1, 1, 1]
sampleHouses = [(0, 1), (3, 2), (3, 4), (4, 0), (4, 4), (5, 2), (5, 5)]--row, col
-}
{-
sampleRows = [3, 0, 3, 0, 1, 2]
sampleColumns = [2, 1, 1, 2, 1, 2]
sampleHouses = [(0,0), (0,4), (1,5), (2,1), (2, 4), (3,0), (3, 3), (4,4), (5,1)]
-}
sampleRows = [5, 1, 4, 1, 4, 2, 4, 2, 1, 4]
sampleColumns = [3, 2, 0, 5, 0, 5, 0, 2, 2, 3, 0, 2, 2, 2]
sampleHouses = [(0, 4), (0, 6), (0, 7), (0, 10), (0, 13), (1, 0), (2, 4), (2, 8), (2, 11), (3, 1), (3, 4), (4, 4), (5, 0), (5, 2), (5, 5), (5, 8), (5, 9), (5, 11), (5, 12), (6, 0), (6, 7), (6, 12), (7, 4), (8, 8), (8, 12), (9, 1), (9, 2), (9, 6)]
    

sampleData = initPuzzleData sampleRows sampleColumns sampleHouses
sampleData2 = placeHouses sampleHouses sampleData

tankData = placeElement (0, 0) 'U' sampleData

getMtx (PuzzleData _ _ pdata) = pdata

{-
Reprezentacja danych na planszy
H - domek
X - puste pole
L - zbiornik przyłączony do domku po lewej
R, U, D - analogicznie
T- niepodlaczony zbiornik ??
-}


{-
wykreśl pola, które nie sąsiadują z żadnym domkiem

domek styka się bokiem z jednym pustym polem -> postaw tam zbiornik z gazem i powykreślaj pola dookoła zbiornika
jeśli w rzędzie jest tyle zbiorników ile w opisie rzędu - poskreślaj puste pola
-}

--zapytanie o nazwę pliku
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
            let solutionData = (findSolution inputData)
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
  --  | (PuzzleData rows cols pdata) == (tryEmptyFields(placeTanks(crossOutRowsAndCols(PuzzleData rows cols pdata)))) && solutionFound (findSolution(placeFirstEmptyField 'X' (PuzzleData rows cols pdata))) = findSolution(placeFirstEmptyField 'X' (PuzzleData rows cols pdata))
  --  | (PuzzleData rows cols pdata) == (tryEmptyFields(placeTanks(crossOutRowsAndCols(PuzzleData rows cols pdata))))  = findSolution(placeFirstEmptyField 'U' (PuzzleData rows cols pdata))                                                                                                                                                              

    | (PuzzleData rows cols pdata) == (tryEmptyFields(placeTanks(crossOutRowsAndCols(PuzzleData rows cols pdata)))) && solutionFound (findSolution(placeFirstEmptyField 'D' (PuzzleData rows cols pdata))) = findSolution(placeFirstEmptyField 'U' (PuzzleData rows cols pdata))
    | (PuzzleData rows cols pdata) == (tryEmptyFields(placeTanks(crossOutRowsAndCols(PuzzleData rows cols pdata))))  = findSolution(placeFirstEmptyField 'X' (PuzzleData rows cols pdata))                                                                                                                                                              
    | otherwise = findSolution (tryEmptyFields (placeTanks (crossOutRowsAndCols (PuzzleData rows cols pdata))))
--jak brak zmian, to wstaw w puste miejsce, jak się nie udało, to się cofnij i szukaj rozwiązania dalej


--sprawdza, czy wszystkie zbiorniki zostaly juz umieszczone
solutionFound :: PuzzleData -> Bool
solutionFound (PuzzleData rows cols pdata)
    | rows == [tanksInRow x (PuzzleData rows cols pdata) | x <- [0..(length rows-1)]]= True--Length od dobrej wartosci?
    | otherwise = False

    --sprawdź, czy są puste pola
emptyFieldsLeft :: PuzzleData -> Bool
emptyFieldsLeft puzzle = 
    length (filter (== ' ') (toList (getMtx puzzle))) /= 0

--umieszcza znak w pierwszym pustym polu
placeFirstEmptyField :: Char -> PuzzleData -> PuzzleData
placeFirstEmptyField char (PuzzleData rows cols pdata) =
    placeFirstEmptyField' ((length rows)-1) ((length cols)-1) char (PuzzleData rows cols pdata) --Length od dobrej wartosci?

placeFirstEmptyField' :: Int -> Int -> Char -> PuzzleData -> PuzzleData
placeFirstEmptyField' row col char (PuzzleData rows cols pdata)
    | row < 0 = (PuzzleData rows cols pdata)
    | col < 0 = placeFirstEmptyField' (row-1) ((length cols)-1) char (PuzzleData rows cols pdata)
    | (getElement (row, col) (PuzzleData rows cols pdata)) == Just ' ' && isTank char = placeTank (row, col) char (PuzzleData rows cols pdata)
    | (getElement (row, col) (PuzzleData rows cols pdata)) == Just ' ' = placeElement (row, col) char (PuzzleData rows cols pdata)
    | otherwise = placeFirstEmptyField' row (col-1) char (PuzzleData rows cols pdata)

--przechodzi po macierzy i probuje umiescic zbiorniki
placeTanks :: PuzzleData -> PuzzleData
placeTanks (PuzzleData rows cols pdata) -- = PuzzleData rows cols pdata
    = placeTanks' ((length rows)-1) ((length cols)-1) (PuzzleData rows cols pdata) --Length od dobrej wartosci?

tryEmptyFields :: PuzzleData -> PuzzleData
tryEmptyFields (PuzzleData rows cols pdata) =
    tryEmptyFields' ((length rows)-1) ((length cols)-1) (PuzzleData rows cols pdata) --Length od dobrej wartosci?

tryEmptyFields' :: Int -> Int -> PuzzleData -> PuzzleData
tryEmptyFields' row col (PuzzleData rows cols pdata)
    | row < 0 = (PuzzleData rows cols pdata)
    | col < 0 = tryEmptyFields' (row-1) ((length cols) - 1) (tryToPlace row 0 (PuzzleData rows cols pdata)) 
    | otherwise = tryEmptyFields' row (col-1) (tryToPlace row col (PuzzleData rows cols pdata)) 

--jeśli puste pole, nie sąsiaduje ze zbiornikiem, ma domek obok i można wstawić (kolumny i wiersze), to wstawia zbiornik
tryToPlace :: Int -> Int -> PuzzleData -> PuzzleData
tryToPlace row col (PuzzleData rows cols pdata)
    | ((getElement (row, col) (PuzzleData rows cols pdata)) == Just ' ') && noGasAround row col (PuzzleData rows cols pdata) && oneHouseToConnect row col (PuzzleData rows cols pdata) && cols!!col > tanksInCol col (PuzzleData rows cols pdata) && rows!!row > tanksInRow row (PuzzleData rows cols pdata) = (placeTank (row, col) 'R' (PuzzleData rows cols pdata)) 
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
    | row < 0 = (PuzzleData rows cols pdata)
    | row < 0 = placeTanks' (row) (col-1) (placeTanksInCol col (PuzzleData rows cols pdata))
   -- | col < 0 = placeTanks' (row-1) col (placeTanksInRow row (PuzzleData rows cols pdata))
   -- | otherwise = placeTanks' row (col-1) (placeTanksInCol col (PuzzleData rows cols pdata))
    |otherwise = placeTanks' (row-1) (col) (placeTanksInRow row (PuzzleData rows cols pdata))
   -- | otherwise = placeTanks' (row-1) (col) (PuzzleData rows cols pdata)

--jesli tyle samo wolnych miejs, co powinno byc zbiornikow - umiesc zbiornik na kazdym miejscu wolnym
--jesli sasiaduja one tylko z jednym domkiem - podepnij go do niego, jesli z wiecej niz jednym - podepnij do takiego, ktory moze miec zbiornik tylko w jednym miejscu lub zostaw niepodpiety
placeTanksInRow :: Int -> PuzzleData -> PuzzleData
placeTanksInRow row (PuzzleData rows cols pdata)
    | rows!!(row) == length (V.filter (==' ') (getRow (row+1) pdata)) = placeTanksInRow' 0 row (PuzzleData rows cols pdata)
    | otherwise = PuzzleData rows cols pdata

--dziala okej
placeTanksInRow' :: Int -> Int -> PuzzleData -> PuzzleData
placeTanksInRow' elem row (PuzzleData rows cols pdata)
--    | rows$row == length (V.filter (==' ') (getRow (row+1) pdata)) =
    | elem == (length rows) = (PuzzleData rows cols pdata)--Length od dobrej wartosci?
    | (getElement (row, elem) (PuzzleData rows cols pdata)) == Just ' ' = placeTanksInRow' (elem+1) row (placeTank (row, elem) 'D' (PuzzleData rows cols pdata))
    | otherwise = placeTanksInRow' (elem+1) row (PuzzleData rows cols pdata)

--jesli domek moze miec zbiornik tylko w jednym miejscu, to go tam postaw
--TODO

--czy dziala? chyba
placeTanksInCol :: Int -> PuzzleData -> PuzzleData
placeTanksInCol col (PuzzleData rows cols pdata)
    | cols!!(col) == length (V.filter (==' ') (getCol (col+1) pdata)) = placeTanksInCol' 0 col (PuzzleData rows cols pdata)
    | otherwise = PuzzleData rows cols pdata

--dziala okej
placeTanksInCol' :: Int -> Int -> PuzzleData -> PuzzleData
placeTanksInCol' elem col (PuzzleData rows cols pdata)
--    | rows$row == length (V.filter (==' ') (getRow (row+1) pdata)) =
    | elem == (length cols) = (PuzzleData rows cols pdata)--Length od dobrej wartosci?
    | (getElement (elem, col) (PuzzleData rows cols pdata)) == Just ' ' = placeTanksInCol' (elem+1) col (placeTank (elem, col) 'D' (PuzzleData rows cols pdata))
    | otherwise = placeTanksInCol' (elem+1) col (PuzzleData rows cols pdata)


--ile pustych pol jest dookola (po bokach? - jesli 1 i domek nie jest podloczony, to tylko tam moze miec podpiety zbiornik)
emptyAround :: (Int, Int) -> PuzzleData -> Int
emptyAround (row, col) pdata =
    length (filter (== Just ' ') [getElement (row+x, col+y) pdata| x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)])
--    length (filter (== Just ' ') [getElement position pdata | position <- [(-1, -1), (1, -1), (-1, 1), (1, 1)]])


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
crossOutRowsAndCols (PuzzleData rows cols pdata) = crossOutRowsAndCols' (length rows - 1) (length cols - 1) (PuzzleData rows cols pdata)--Length od dobrej wartosci?
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
displaySolution (PuzzleData rows cols pdata) = 
    do 
        --putStrLn $ prettyMatrix (pdata)
        --mapM_ putStrLn (toLists pdata)
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
    placeHouses h (PuzzleData a b (fromList (length a) (length b) [' ', ' '..]))--Length od dobrej wartosci?

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